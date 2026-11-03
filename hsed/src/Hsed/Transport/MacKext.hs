{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.MacKext
Description : The attached drive through the BrightPlaza TPer kext: a 'ReadOnly transport of five commands, and the bind of its base ComID
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The drive's transport on macOS (design 7.2), through a C shim over IOKit
(@cbits/hsed_kext.{h,c}@) compiled only under the package's manual flag
@kext@ (@stack build hsed --flag hsed:kext@, which defines @HSED_KEXT@): it
locates the TPer under a BSD device in the IORegistry, opens the kext's
user client, binds the drive's base ComID, issues design 9.3's five
commands, and closes it. This module calls the shim and does nothing
else: which command a request is, its buffer, the judging of the shim's
answer and the latch are "Hsed.Transport.Kext"'s, which every build
compiles and the tests run over shims of their own; and every CDB,
direction and length the shim issues is built by the portable C of
@cbits/hsed_cdb.c@, which every build compiles and the tests run through
FFI. The process must run as root, hsed's own gate, checked here and again
in the shim (the loaded kext, build 625, checks no privilege).

'openKext', in order, opens nothing unless each holds:

1. the process runs as root (effective user id 0);
2. the target names a whole disk (@/dev/disk22@; "Hsed.Transport.Kext");
3. @hsed_kext_locate@ finds the kext's TPer directly under it, and reads
   its interface (SAT or SCSI), its IDENTIFY serial, its registry entry id,
   the kext's build and the kext's user clients already on it;
4. there are none of those, and the serial (trimmed) is the one expected
   (@--expect-serial@, which a drive target must give);
5. @hsed_kext_open@, root again, opens the user client, exclusively
   (another opener gets @kIOReturnExclusiveAccess@).

It hands the drive over as a 'RawDrive' beside the DriveInfo, and the jig's
target puts it behind the Guard at once ("Hsed.Jig.Guard"'s
'Hsed.Jig.Guard.GuardedDrive', design 13.29 (l)):

* the transport ("Hsed.Transport.Kext".'Hsed.Transport.Kext.kextExchange'
  over @hsed_kext_command@) carries Level 0 Discovery's IF-RECV alone until
  the base is bound, then, on the base, a ComPacket's IF-SEND and IF-RECV
  and STACK_RESET's HANDLE_COMID_REQUEST and GET_COMID_RESPONSE, each in
  whole 512-byte blocks, and refuses anything else before the shim is
  called ('Unsupported'). The shim takes each command's six scalars, its
  CDB among them, from the portable C, built from the shape and the base
  it bound, and checks an IF-SEND's payload in the very
  buffer it passes to the kernel, and echoes the scalars' CDB, the bytes
  the kernel is given. A @kern_return_t@ other than success, a CDB other
  than hsed's, or another count of bytes moved than asked (design 13.29
  (d)) is a 'TransportFailure' naming it, and after one the transport
  issues nothing more. Each command it issues it prints on stderr, after
  the judgement and apart from it: the command, the CDB, the
  @kern_return_t@, the bytes transferred, never the payload.
* the bind ("Hsed.Transport.Kext".'Hsed.Transport.Kext.kextBind' over
  @hsed_kext_bind@) checks again, in C (@hsed_cdb_bind_check@ on what the
  shim reads of the TPer), that the base is a ComID Core Table 26 does not
  reserve, bound once, that the TPer is still the one opened, with this
  user client alone on it, and that its serial is unchanged; a bind
  refused issues nothing more.

Closing the transport closes the user client, once.

Built without the flag (the default, and CI's tests'), 'openKext' refuses,
and the shim is neither compiled nor linked.
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
import           Foreign.Marshal.Utils    (fillBytes)
import           Foreign.Ptr              (Ptr, castPtr, plusPtr)
import           Foreign.Storable         (peekByteOff)
import           GHC.ForeignPtr           (mallocForeignPtrAlignedBytes)
import           Numeric                  (showHex)
import qualified RIO.ByteString           as B
import qualified RIO.Text                 as T
import           System.Posix.User        (getEffectiveUserID)

import           Hsed.Transport.Cdb       (shapeCode)
import           Hsed.Transport.Drive     (DriveInfo (..), InterfaceType (..), RawDrive (..))
import           Hsed.Transport.Kext      (InfoLayout (..), KextHandle, Located (..), ShimCommand, admit, diskName,
                                           infoLayout, kernReturn, kextBind, kextClose, kextExchange, newKextHandle, quietly)
import           Hsed.Wire.Internal       (Cap (..), Describe (..), IfStatus (..), Transport (..), Unit (..))
#else
import           Hsed.Transport.Drive     (DriveInfo, RawDrive)
#endif


-- | The drive to open: its BSD name (@disk22@, or the device @/dev/disk22@),
--   and the serial it must have (required: a drive is opened only as the
--   drive named).
data KextOpts = KextOpts
  { koBsd          :: !Text
  , koExpectSerial :: !Text
  }
  deriving (Eq, Show)

-- | The drive, its transport and the bind of its base ComID, and what the
--   IORegistry says of it, or why not.
openKext :: KextOpts -> IO (Either Text (RawDrive, DriveInfo))
#ifdef HSED_KEXT
openKext opts = do
  euid <- getEffectiveUserID
  if euid /= 0
    then pure (Left (koBsd opts <> ": the kext transport needs root (sudo), hsed's own gate: the loaded kext "
                     <> "checks no privilege (design 9.3)"))
    else either (pure . Left) (openDisk opts) (diskName (koBsd opts))

-- | Locate, admit and open a whole disk's TPer.
openDisk :: KextOpts -> Text -> IO (Either Text (RawDrive, DriveInfo))
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
          -- the drive handed over before anything is printed, so that a printer that fails (a closed stderr) never
          -- leaves the user client open with no transport to close it
          h <- newKextHandle disk (diInterface di)
          let raw = RawDrive (kextTransport info h di) (kextBind (shimBind info) h)
          quietly complain ("kext " <> disk <> ": opened the TPer's user client (" <> interfaceName (diInterface di)
                            <> ", registry id 0x" <> T.pack (showHex (diRegistryId di) "")
                            <> ", kext build " <> diKextBuild di <> ")")
          pure (Right (raw, di))

-- | The transport over an opened user client: each command through
--   @hsed_kext_command@ ("Hsed.Transport.Kext".'kextExchange'), no events,
--   512-byte blocks, the drive as the registry names it; closing it closes
--   the user client, once.
kextTransport :: ForeignPtr () -> KextHandle -> DriveInfo -> Transport 'ReadOnly
kextTransport info h di = MkTransport
  { tExchange = kextExchange complain (shimCommand info) h
  , tInject   = \_ -> pure (Unsupported "the kext transport causes no events")
  , tUnit     = UnitBlocks512
  , tDescribe = Describe
      { dRole      = "drive"
      , dImpl      = "BrightPlazaTPer kext, build " <> diKextBuild di
      , dUnits     = UnitBlocks512
      , dSsc       = Nothing
      , dBaseComId = Nothing
      , dNComIds   = Nothing
      , dDevice    = Just ("/dev/" <> diBsd di)
      }
  , tClose    = kextClose complain (fromIntegral <$> withForeignPtr info hsedKextClose) h
  }

-- | @hsed_kext_command@ on the opened user client: the shape by its number
--   in cbits/hsed_cdb.h ("Hsed.Transport.Cdb".'shapeCode'), the
--   @kern_return_t@ as its bits.
shimCommand :: ForeignPtr () -> ShimCommand
shimCommand info shape blocks buf size cdb moved =
  fromIntegral <$> withForeignPtr info (\p -> hsedKextCommand p (fromIntegral (shapeCode shape)) blocks buf size cdb moved)

-- | @hsed_kext_bind@ on the opened user client.
shimBind :: ForeignPtr () -> Word16 -> IO Word32
shimBind info base = fromIntegral <$> withForeignPtr info (`hsedKextBind` base)

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

-- | @hsed_kext_open@: the user client opened, exclusively, as root only.
foreign import ccall safe "hsed_kext_open"
  hsedKextOpen :: Ptr () -> IO CInt

-- | @hsed_kext_bind@: the drive's base ComID bound, once, after the shim's
--   checks.
foreign import ccall safe "hsed_kext_bind"
  hsedKextBind :: Ptr () -> Word16 -> IO CInt

-- | @hsed_kext_command@: one of the five commands, by its shape's number,
--   of a number of 512-byte blocks, through a page-aligned buffer of a size;
--   the CDB issued and the bytes transferred written back.
foreign import ccall safe "hsed_kext_command"
  hsedKextCommand :: Ptr () -> CInt -> Word8 -> Ptr Word8 -> Word64 -> Ptr Word8 -> Ptr Word64 -> IO CInt

-- | @hsed_kext_close@: the user client closed.
foreign import ccall safe "hsed_kext_close"
  hsedKextClose :: Ptr () -> IO CInt
#else
openKext _ = pure (Left "built without -fkext")
#endif
