{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.MacKext
Description : The attached drive through the BrightPlaza TPer kext: a 'ReadOnly transport, Level 0 IF-RECV only
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The drive's transport on macOS (design 7.2), through a C shim over IOKit
(@cbits/hsed_kext.{h,c}@) compiled only under the package's manual flag
@kext@ (@stack build hsed --flag hsed:kext@, which defines @HSED_KEXT@): it
locates the TPer under a BSD device in the IORegistry, opens the kext's
user client, issues the one command M3 needs, IF-RECV of Level 0 Discovery,
and closes it. A @'ReadOnly@ transport: no IF-SEND exists on it, in the
types or in the shim; and the process must run as root, hsed's own gate
(the loaded kext, build 625, checks no privilege).

'openKext', in order, opens nothing unless each holds:

1. the process runs as root (effective user id 0);
2. the target names a whole disk (@/dev/disk6@; "Hsed.Transport.Kext");
3. @hsed_kext_locate@ finds the kext's TPer directly under it, and reads
   its interface (SAT or SCSI), its IDENTIFY serial, its registry entry id,
   the kext's build and the kext's user clients already on it;
4. there are none of those, and the serial (trimmed) is the one expected
   (@--expect-serial@, which a drive target must give);
5. @hsed_kext_open@ opens the user client, exclusively (another opener gets
   @kIOReturnExclusiveAccess@).

The transport then answers IF-RECV of Level 0 Discovery (protocol 1, ComID
1) of 1 to 255 whole 512-byte blocks through @hsed_kext_level0@, its
payload exactly the length asked; any other request is 'Unsupported'
@M3: Level 0 only@ without the shim being called (this refusal and the
shim's fixed templates are the barriers below the Guard, design 9.3). A
@kern_return_t@ other than success, a CDB other than hsed's, or another
count of bytes moved than asked is a 'TransportFailure' naming it, and
after one the transport issues nothing more: a command the kext fails
leaves its SCSI task unreleased (TPerDriver.cpp's PerformSCSICommand
returns before ReleaseSCSITask), which a retry would leak again, and a
drive that answered other than hsed asked is not asked again. A residue on
a good transfer keeps the bytes the buffer held beside the failure, as
evidence for design 13.29 (d), never as a capture
("Hsed.Transport.Kext".'judgeLevel0'). Each command it issues it prints on
stderr: the CDB, the @kern_return_t@, the bytes transferred. Closing it
closes the user client, once.

Built without the flag (the default, and CI's), 'openKext' refuses, and the
shim is neither compiled nor linked.
-}
module Hsed.Transport.MacKext
  ( KextOpts (..)
  , openKext
  ) where

import           RIO
#ifdef HSED_KEXT
import           Foreign.C.String         (CString, withCString)
import           Foreign.C.Types          (CInt (..))
import           Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc    (alloca, allocaBytes)
import           Foreign.Marshal.Utils    (fillBytes)
import           Foreign.Ptr              (Ptr, castPtr, plusPtr)
import           Foreign.Storable         (peek, peekByteOff, poke)
import           GHC.ForeignPtr           (mallocForeignPtrAlignedBytes)
import           Numeric                  (showHex)
import qualified RIO.ByteString           as B
import qualified RIO.Text                 as T
import           System.Posix.User        (getEffectiveUserID)

import           Hsed.Transport.Cdb       (level0Blocks)
import           Hsed.Transport.Drive     (DriveInfo (..), InterfaceType (..))
import           Hsed.Transport.Kext      (InfoLayout (..), Located (..), admit, diskName, ifRecvBuffer, infoLayout,
                                           judgeLevel0, kernReturn, level0Line)
import           Hsed.Wire.Internal       (Cap (..), Describe (..), IfStatus (..), Request (lenBytes),
                                           Response (..), Transport (..), Unit (..))
#else
import           Hsed.Transport.Drive     (DriveInfo)
import           Hsed.Wire                (Cap (..), Transport)
#endif


-- | The drive to open: its BSD name (@disk6@, or the device @/dev/disk6@),
--   and the serial it must have (required: a drive is opened only as the
--   drive named).
data KextOpts = KextOpts
  { koBsd          :: !Text
  , koExpectSerial :: !Text
  }
  deriving (Eq, Show)

-- | The drive's transport and what the IORegistry says of it, or why not.
openKext :: KextOpts -> IO (Either Text (Transport 'ReadOnly, DriveInfo))
#ifdef HSED_KEXT
openKext opts = do
  euid <- getEffectiveUserID
  if euid /= 0
    then pure (Left (koBsd opts <> ": the kext transport needs root (sudo), hsed's own gate: the loaded kext "
                     <> "checks no privilege (design 9.3)"))
    else either (pure . Left) (openDisk opts) (diskName (koBsd opts))

-- | Locate, admit and open a whole disk's TPer.
openDisk :: KextOpts -> Text -> IO (Either Text (Transport 'ReadOnly, DriveInfo))
openDisk opts disk = do
  info <- mallocForeignPtrAlignedBytes (ilBytes infoLayout) (ilAlignment infoLayout)
  located <- withForeignPtr info $ \p -> do
    fillBytes p 0 (ilBytes infoLayout)
    kr <- withCString (T.unpack disk) (`hsedKextLocate` p)
    if kr /= 0
      then pure (Left (disk <> ": hsed_kext_locate: " <> kernReturn (fromIntegral kr)))
      else Right <$> peekLocated p
  case located >>= admit (koExpectSerial opts) disk of
    Left why -> pure (Left why)
    Right di -> do
      kr <- withForeignPtr info hsedKextOpen
      if kr /= 0
        then pure (Left (disk <> ": hsed_kext_open: " <> kernReturn (fromIntegral kr)))
        else do
          complain ("kext " <> disk <> ": opened the TPer's user client (" <> interfaceName (diInterface di)
                    <> ", registry id 0x" <> T.pack (showHex (diRegistryId di) "")
                    <> ", kext build " <> diKextBuild di <> ")")
          state <- newMVar Ready
          pure (Right (kextTransport disk di info state, di))

-- | Whether the transport may still issue a command.
data KextState
  = Ready
  | Failed !Text  -- ^ a command failed; nothing more is issued
  | Closed

-- | The transport over an opened user client.
kextTransport :: Text -> DriveInfo -> ForeignPtr () -> MVar KextState -> Transport 'ReadOnly
kextTransport disk di info state = MkTransport
  { tExchange = exchange
  , tInject   = \_ -> pure (Unsupported "the kext transport causes no events")
  , tUnit     = UnitBlocks512
  , tDescribe = Describe
      { dRole      = "drive"
      , dImpl      = "BrightPlazaTPer kext, build " <> diKextBuild di
      , dUnits     = UnitBlocks512
      , dSsc       = Nothing
      , dBaseComId = Nothing
      , dNComIds   = Nothing
      , dDevice    = Just ("/dev/" <> disk)
      }
  , tClose    = close
  }
  where
    exchange r = case level0Blocks r of
      Left why    -> pure (Response (Unsupported why) mempty)
      Right blocks -> modifyMVar state $ \case
        Closed     -> pure (Closed, failure "the kext transport is closed")
        Failed why -> pure (Failed why, failure ("a command failed before (" <> why <> "); nothing is retried"))
        Ready      -> do
          answer <- level0 (diInterface di) blocks (lenBytes r)
          pure (either (Failed . fst) (const Ready) answer, either (\(why, kept) -> Response (TransportFailure why) kept) (Response Good) answer)
    failure why = Response (TransportFailure why) mempty

    -- one IF-RECV through the shim, into a zeroed page-aligned buffer
    level0 iface blocks len = do
      (buf, size) <- ifRecvBuffer len
      withForeignPtr info $ \pInfo -> withForeignPtr buf $ \pBuf -> allocaBytes 16 $ \pCdb -> alloca $ \pMoved -> do
        fillBytes pCdb 0 16
        poke pMoved 0
        kr      <- fromIntegral <$> hsedKextLevel0 pInfo blocks pBuf (fromIntegral size) pCdb pMoved
        cdb     <- B.packCStringLen (castPtr pCdb, 16)
        moved   <- peek pMoved
        payload <- B.packCStringLen (castPtr pBuf, fromIntegral len)
        complain (level0Line disk cdb kr moved)
        pure (judgeLevel0 iface blocks kr cdb moved payload)

    close = modifyMVar_ state $ \case
      Closed -> pure Closed
      _      -> do
        kr <- withForeignPtr info hsedKextClose
        complain ("kext " <> disk <> ": closed the TPer's user client"
                  <> (if kr == 0 then "" else ": hsed_kext_close: " <> kernReturn (fromIntegral kr)))
        pure Closed

-- | What @hsed_kext_locate@ wrote into an @hsed_kext_info@.
peekLocated :: Ptr () -> IO Located
peekLocated p = do
  registryId <- peekByteOff p (ilRegistryId infoLayout)
  sat        <- peekByteOff p (ilSat infoLayout) :: IO CInt
  serial     <- cString (ilSerial infoLayout) (ilSerialBytes infoLayout)
  build      <- cString (ilKextBuild infoLayout) (ilKextBuildBytes infoLayout)
  others     <- peekByteOff p (ilOtherClients infoLayout) :: IO CInt
  pure Located
    { lRegistryId   = registryId
    , lInterface    = if sat /= 0 then IfSAT else IfSCSI
    , lSerial       = serial
    , lKextBuild    = build
    , lOtherClients = fromIntegral others
    }
  where
    -- a char array of the struct, to its first NUL or its end
    cString off n = decodeUtf8Lenient . B.takeWhile (/= 0) <$> B.packCStringLen (castPtr (p `plusPtr` off), n)

interfaceName :: InterfaceType -> Text
interfaceName = \case
  IfSAT  -> "SAT"
  IfSCSI -> "SCSI"

complain :: Text -> IO ()
complain t = hPutBuilder stderr (getUtf8Builder (display t <> "\n"))

-- | @hsed_kext_locate@: the TPer under a BSD name, its registry entry read
--   into an @hsed_kext_info@; opens nothing.
foreign import ccall safe "hsed_kext_locate"
  hsedKextLocate :: CString -> Ptr () -> IO CInt

-- | @hsed_kext_open@: the user client opened, exclusively.
foreign import ccall safe "hsed_kext_open"
  hsedKextOpen :: Ptr () -> IO CInt

-- | @hsed_kext_level0@: IF-RECV of Level 0 Discovery, a number of 512-byte
--   blocks into a page-aligned buffer of a size; the CDB used and the bytes
--   transferred written back.
foreign import ccall safe "hsed_kext_level0"
  hsedKextLevel0 :: Ptr () -> Word8 -> Ptr Word8 -> Word64 -> Ptr Word8 -> Ptr Word64 -> IO CInt

-- | @hsed_kext_close@: the user client closed.
foreign import ccall safe "hsed_kext_close"
  hsedKextClose :: Ptr () -> IO CInt
#else
openKext _ = pure (Left "built without -fkext")
#endif
