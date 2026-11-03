{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Kext
Description : The kext transport without the kext: the disk, the drive admitted or refused, each command through the shim judged, and the latch
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The kext transport ("Hsed.Transport.MacKext") but for its calls into the C
shim, here so that every build compiles it and the tests run it, the shim
given as a function (the module that calls the shim is compiled only under
the package's flag @kext@, and passes the shim's functions in):

* the whole disk a target names ('diskName');
* what @hsed_kext_locate@ read of the TPer ('Located', laid out as
  'infoLayout' says @hsed_kext_info@ is) admitted as the drive to open, or
  refused: other user clients already on it, or a serial other than the
  one expected, which a drive target must name ('admit');
* each command through the shim ('kextExchange'): which of the five a
  drive transport carries it is ("Hsed.Transport.Cdb".'shapeOf', on the
  base ComID bound by 'kextBind'), refused before the shim if none; its
  buffer ('commandBuffer'); the shim's answer judged ('judgeCommand':
  success, hsed's CDB, every byte moved) and printed ('commandLine'); and
  the latch: once a command fails, or its answer is lost, nothing more is
  issued ('KextState'), the whole of it uninterruptible;
* a @kern_return_t@ named ('kernReturn').

A drive's base ComID is bound once, from the drive's own Level 0
Discovery ("Hsed.Jig.Guard".'Hsed.Jig.Guard.driveBind', through the
'Hsed.Transport.Drive.RawDrive''s bind, which is 'kextBind'): the shim
binds it first, checking it again in C (@hsed_kext_bind@: a non-reserved
ComID, the TPer's user clients exactly this one, its serial unchanged),
then the transport. Until then the transport carries Level 0 Discovery's
IF-RECV alone, as M3's did.
-}
module Hsed.Transport.Kext
  ( -- * hsed_kext_info
    InfoLayout (..)
  , infoLayout
  , Located (..)
    -- * Before anything is opened
  , diskName
  , admit
    -- * The transport over the shim
  , KextState (..)
  , KextHandle (..)
  , newKextHandle
  , ShimCommand
  , kextExchange
  , kextBind
  , kextClose
    -- * One command through the shim
  , pageBytes
  , bufferBytes
  , ifRecvBuffer
  , commandBuffer
  , judgeCommand
  , commandLine
  , shapeName
    -- * kern_return_t
  , kernReturn
  ) where

import           Foreign.ForeignPtr    (ForeignPtr, mallocForeignPtrBytes, plusForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Marshal.Utils (copyBytes, fillBytes)
import           Foreign.Ptr           (Ptr, alignPtr, castPtr, minusPtr)
import           Foreign.Storable      (peek, poke)
import           Numeric               (showHex)
import qualified RIO.ByteString        as B
import           RIO
import qualified RIO.Char              as C
import qualified RIO.List              as L
import qualified RIO.Text              as T

import           Hsed.Transport.Cdb    (Shape (..), blockBytes, cdbFor, cdbHex, shapeDir, shapeOf, shapeProto)
import           Hsed.Transport.Drive  (DriveInfo (..), InterfaceType (..), maskSerial)
import           Hsed.Wire             (Dir (..), IfStatus (..), Request (..), Response (..), hex)


-- $setup
-- >>> :set -XOverloadedStrings

-- * hsed_kext_info

-- | Where @hsed_kext_info@'s fields lie (cbits/hsed_kext.h), in bytes: its
--   size and alignment, the offset of each field, the sizes of its two
--   strings. The C file asserts the same numbers of the struct the compiler
--   lays out, and "Hsed.Transport.KextSourceSpec" compares its assertions with these.
data InfoLayout = InfoLayout
  { ilBytes          :: !Int
  , ilAlignment      :: !Int
  , ilConn           :: !Int
  , ilRegistryId     :: !Int
  , ilSat            :: !Int
  , ilSerial         :: !Int
  , ilSerialBytes    :: !Int
  , ilKextBuild      :: !Int
  , ilKextBuildBytes :: !Int
  , ilOtherClients   :: !Int
  , ilBaseComId      :: !Int
  }
  deriving (Eq, Show)

-- | @hsed_kext_info@: 128 bytes aligned to 8; @conn@ at 0, @registry_id@ at
--   8, @sat@ at 16, @serial[64]@ at 20, @kext_build[32]@ at 84,
--   @other_clients@ at 116, @base_comid@ at 120 (0 until the shim binds
--   one), then padding to the alignment.
infoLayout :: InfoLayout
infoLayout = InfoLayout
  { ilBytes          = 128
  , ilAlignment      = 8
  , ilConn           = 0
  , ilRegistryId     = 8
  , ilSat            = 16
  , ilSerial         = 20
  , ilSerialBytes    = 64
  , ilKextBuild      = 84
  , ilKextBuildBytes = 32
  , ilOtherClients   = 116
  , ilBaseComId      = 120
  }

-- | What @hsed_kext_locate@ read of the TPer under a BSD name.
data Located = Located
  { lRegistryId   :: !Word64
  , lInterface    :: !InterfaceType
  , lSerial       :: !Text   -- ^ the IDENTIFY serial as the registry holds it, its padding kept; empty if none
  , lKextBuild    :: !Text   -- ^ the loaded kext's CFBundleVersion; empty if it could not be read
  , lOtherClients :: !Int    -- ^ the kext's user clients already on the TPer; negative if they could not be counted
  }
  deriving (Eq, Show)


-- * Before anything is opened

-- | The BSD name of the whole disk a target names, @/dev/disk22@ or
--   @disk22@: @disk22@. Anything else is refused: a partition (@disk22s1@), a
--   raw device (@rdisk22@), another path. The shim refuses a disk that is
--   not directly on the kext's TPer besides.
--
-- >>> diskName "/dev/disk22"
-- Right "disk22"
diskName :: Text -> Either Text Text
diskName target
  | Just n <- T.stripPrefix "disk" name, not (T.null n), T.all C.isDigit n = Right name
  | otherwise = Left (target <> ": not a whole disk, /dev/diskN")
  where name = fromMaybe target (T.stripPrefix "/dev/" target)

-- | The drive a located TPer is, given the serial it must have (a drive
--   target names one, @--expect-serial@), or why the kext transport opens
--   nothing:
--
-- * no serial expected (empty once trimmed): a drive is opened only as the
--   drive named;
-- * user clients of the kext already on the TPer (sedutil, the ToolBox's
--   daemon), or none counted: the kext allows one opener, and one already
--   open may be in the middle of its own session;
-- * a serial other than the one expected, both trimmed (the registry keeps
--   IDENTIFY's padding), the two printed masked ('maskSerial').
--
-- The serial kept is the trimmed one; a kext build that could not be read
-- is @unknown@.
admit :: Text -> Text -> Located -> Either Text DriveInfo
admit expected disk l
  | T.null e =
      Left (disk <> ": no serial expected: a drive is opened only as the one --expect-serial names")
  | lOtherClients l < 0 =
      Left (disk <> ": the kext's user clients on its TPer could not be counted")
  | lOtherClients l > 0 =
      Left (disk <> ": " <> tshow (lOtherClients l) <> " of the kext's user clients already open on its TPer "
            <> "(sedutil-cli, the ToolBox's sedd?): the kext allows one")
  | e /= serial =
      Left (disk <> ": its serial is " <> maskSerial serial <> ", not the " <> maskSerial e <> " expected")
  | otherwise = Right DriveInfo
      { diBsd        = disk
      , diSerial     = serial
      , diKextBuild  = if T.null (lKextBuild l) then "unknown" else lKextBuild l
      , diInterface  = lInterface l
      , diRegistryId = lRegistryId l
      }
  where
    serial = T.strip (lSerial l)
    e      = T.strip expected


-- * The transport over the shim

-- | Whether the transport may still issue a command.
data KextState
  = Ready
  | Failed !Text  -- ^ a command failed, or its answer was lost; nothing more is issued
  | Closed
  deriving (Eq, Show)

-- | An opened user client as the transport keeps it: the disk's BSD name
--   (for the lines it prints), the drive's interface (whose CDBs the shim
--   builds), the base ComID bound (none until 'kextBind'), and whether it
--   may still issue a command.
data KextHandle = KextHandle
  { khDisk      :: !Text
  , khInterface :: !InterfaceType
  , khBase      :: !(IORef (Maybe Word16))
  , khState     :: !(MVar KextState)
  }

-- | A handle on a user client just opened: no base ComID bound, ready.
newKextHandle :: Text -> InterfaceType -> IO KextHandle
newKextHandle disk iface = KextHandle disk iface <$> newIORef Nothing <*> newMVar Ready

-- | One command through the shim (@hsed_kext_command@): the shape, the
--   512-byte blocks, the page-aligned buffer and its size, where to write
--   the 16 bytes of CDB issued and the bytes the kext says it moved; the
--   @kern_return_t@. The shim builds the CDB, the direction and the length
--   itself, from the shape, the blocks and the base it has bound
--   (cbits/hsed_cdb.c), and checks an IF-SEND's payload in the buffer.
type ShimCommand = Shape -> Word8 -> Ptr Word8 -> Word64 -> Ptr Word8 -> Ptr Word64 -> IO Word32

-- | A command through the shim, the lines it prints given to the first
--   argument: one of the five a drive transport carries, on the base ComID
--   bound ("Hsed.Transport.Cdb".'shapeOf'; Level 0 Discovery's IF-RECV
--   alone while none is), or 'Unsupported' with why, the shim not called.
--   Otherwise, while the transport is ready, the shim is called once over
--   'commandBuffer', the line 'commandLine' printed and the answer judged
--   ('judgeCommand'): 'Good' with an IF-RECV's payload (none for an
--   IF-SEND), or a 'TransportFailure' saying why, which latches the
--   transport: every command after is a 'TransportFailure' and the shim is
--   not called again (a command the kext failed leaves its SCSI task
--   unreleased, which a retry would leak again, and a drive that answered
--   other than hsed asked is not asked again); a shim that throws latches it
--   too.
--
--   From the latch's check to the latch set again, the command is
--   uninterruptible: an asynchronous exception (a 'timeout', a signal
--   thrown as one) waits for the shim and for the command's answer to be
--   judged and kept, the kext's own limit, 10 s a command, bounding the
--   wait. One that arrived meanwhile is then raised, the answer lost to
--   the caller, and so the transport latches before it is: what the drive
--   did with the command is no longer known.
kextExchange :: (Text -> IO ()) -> ShimCommand -> KextHandle -> Request -> IO Response
kextExchange complain shim h q = do
  base <- readIORef (khBase h)
  case shapeOf base q of
    Left why              -> pure (Response (Unsupported why) mempty)
    Right (shape, blocks) -> uninterruptibleMask $ \restore -> do
      answer <- modifyMVar (khState h) (command shape blocks)
      restore (pure answer) `onException` lost shape
  where
    command shape blocks = \case
      Closed     -> pure (Closed, failure "the kext transport is closed")
      Failed why -> pure (Failed why, failure ("a command failed before (" <> why <> "); nothing more is issued"))
      Ready      -> tryAny (issue shape blocks) <&> \case
        Left e                   -> let why = threw shape e in (Failed why, failure why)
        Right (Left (why, kept)) -> (Failed why, Response (TransportFailure why) kept)
        Right (Right received)   -> (Ready, Response Good received)

    failure why = Response (TransportFailure why) mempty

    -- the command through the shim, judged
    issue shape blocks = do
      (buf, size) <- commandBuffer q
      withForeignPtr buf $ \pBuf -> allocaBytes 16 $ \pCdb -> alloca $ \pMoved -> do
        fillBytes pCdb 0 16
        poke pMoved 0
        kr       <- shim shape blocks pBuf (fromIntegral size) pCdb pMoved
        cdb      <- B.packCStringLen (castPtr pCdb, 16)
        moved    <- peek pMoved
        received <- if shapeDir shape == IfRecv then B.packCStringLen (castPtr pBuf, fromIntegral (lenBytes q)) else pure mempty
        complain (commandLine (khDisk h) shape (comId q) blocks cdb kr moved)
        pure (judgeCommand (khInterface h) shape (comId q) blocks kr cdb moved received)

    threw shape e =
      "the shim threw on " <> shapeName shape <> ": " <> tshow e <> "; whether it reached the drive is not known, so "
      <> "nothing more is issued on this drive in this run"

    -- the answer lost to an asynchronous exception: the latch set, unless a failure set it already
    lost shape = modifyMVar_ (khState h) $ \case
      Ready -> pure (Failed ("the answer to " <> shapeName shape <> " was lost to an interruption, after the shim "
                             <> "returned: what the drive did with it is not known, so nothing more is issued on this "
                             <> "drive in this run"))
      st    -> pure st

-- | Bind the drive's base ComID, given the shim's bind (@hsed_kext_bind@,
--   which answers a @kern_return_t@): while the transport is ready, the
--   shim's first (in C: a ComID Core Table 26 does not reserve, bound once;
--   the TPer found again by its registry id; its user clients exactly
--   this one; its serial unchanged), then the transport's, after which it
--   carries the commands on that ComID ("Hsed.Transport.Cdb".'shapeOf').
--   The same base again is bound already. A bind the shim refuses latches
--   the transport, as a failed command does: the drive is not the one
--   opened, or not this process's alone, or not as the run expects it, and
--   nothing more is issued on it. The whole is uninterruptible, so that
--   the shim's base and the transport's never differ.
kextBind :: (Word16 -> IO Word32) -> KextHandle -> Word16 -> IO (Either Text ())
kextBind bind h base = uninterruptibleMask_ $ modifyMVar (khState h) $ \case
  Closed     -> pure (Closed, Left (named <> ": the kext transport is closed"))
  Failed why -> pure (Failed why, Left (named <> ": a command failed before (" <> why <> "); nothing more is bound or issued"))
  Ready      -> do
    kr <- bind base
    if kr == 0
      then (Ready, Right ()) <$ writeIORef (khBase h) (Just base)
      else let why = named <> ": " <> kernReturn kr <> refused kr in pure (Failed why, Left why)
  where
    named = "hsed_kext_bind of " <> hex base
    refused = \case
      0xe00002c5 -> ", the TPer's user clients not exactly this one (none counted is a count that is not what it seems, "
                    <> "design 13.29 (d)): another could reach the drive between hsed's commands, so nothing more is issued"
      0xe00002c0 -> ", the TPer not the one opened, or its serial changed: nothing more is issued"
      0xe00002c2 -> ", not a base ComID the shim takes (0x1000 or above, Core Table 26, PDF 55-56), or another bound "
                    <> "already: nothing more is issued"
      0xe00002cd -> ", the user client not open: nothing more is issued"
      _          -> ": nothing more is issued"

-- | Close the transport, once, given the shim's close (@hsed_kext_close@):
--   the user client closed, failed or not, and a @kern_return_t@ other than
--   success printed; a closed transport issues nothing more.
kextClose :: (Text -> IO ()) -> IO Word32 -> KextHandle -> IO ()
kextClose complain close h = uninterruptibleMask_ $ modifyMVar_ (khState h) $ \case
  Closed -> pure Closed
  _      -> do
    kr <- close
    complain ("kext " <> khDisk h <> ": closed the TPer's user client"
              <> (if kr == 0 then "" else ": hsed_kext_close: " <> kernReturn kr))
    pure Closed


-- * One command through the shim

-- | A command's buffer's alignment and the unit of its size: 16 KiB, the
--   arm64 page (sedutil's @IO_BUFFER_ALIGNMENT@, DtaConstants.h:39), a
--   multiple of x86_64's 4 KiB; the shim refuses a buffer that is not
--   page-aligned, as the kext's own user library does
--   (TPerKernelInterface.c).
pageBytes :: Int
pageBytes = 16384

-- | The buffer for a transfer: its length rounded up to whole pages, one at
--   least.
--
-- >>> map bufferBytes [2048, 16384, 130560]
-- [16384,16384,131072]
bufferBytes :: Word32 -> Int
bufferBytes len = max 1 ((fromIntegral len + pageBytes - 1) `div` pageBytes) * pageBytes

-- | A buffer for a transfer and its size: 'bufferBytes' of them, zeroed,
--   aligned to 'pageBytes', and pinned, so that it stays where the shim and
--   the kernel were told it is: the shim's call is a safe one, during which
--   the collector may run (an idle, a minor or a major collection), and the
--   kext reads the buffer into the kernel or writes it from there.
--
--   Its bytes are an array 'mallocForeignPtrBytes' pins, a page less a byte
--   longer than the buffer, which the pointer returned starts inside at the
--   first page boundary; 'plusForeignPtr' keeps the whole array alive with
--   it. Not "GHC.ForeignPtr"'s @mallocForeignPtrAlignedBytes size 16384@: GHC
--   9.4's RTS lays an array aligned past its 4 KiB block out with its header
--   in a later block of the group than the first, whose descriptor says
--   nothing of pinning, and then moves it at a collection though its
--   payload is aligned (design 7.2; the test reads the RTS's pinned flag and
--   the array's address across collections).
ifRecvBuffer :: Word32 -> IO (ForeignPtr Word8, Int)
ifRecvBuffer len = do
  let size = bufferBytes len
  raw <- mallocForeignPtrBytes (size + pageBytes - 1)
  offset <- withForeignPtr raw $ \p -> pure (alignPtr p pageBytes `minusPtr` p)
  let buf = raw `plusForeignPtr` offset
  withForeignPtr buf $ \p -> fillBytes p 0 size
  pure (buf, size)

-- | A command's buffer and its size: 'ifRecvBuffer' of its transfer
--   length, pinned, page-aligned and zeroed, an IF-SEND's payload copied
--   into its first bytes (the payload is the transfer length's, which
--   "Hsed.Transport.Cdb".'shapeOf' requires); the rest stays zero, which the
--   shim requires of an IF-SEND's bytes after the payload's own end.
commandBuffer :: Request -> IO (ForeignPtr Word8, Int)
commandBuffer q = do
  (buf, size) <- ifRecvBuffer (lenBytes q)
  when (dir q == IfSend) $ withForeignPtr buf $ \p ->
    B.useAsCStringLen (B.take size (payload q)) (\(src, n) -> copyBytes p (castPtr src) n)
  pure (buf, size)

-- | One @hsed_kext_command@ call judged, from the drive's interface, the
--   shape, its ComID (the base bound, or Level 0 Discovery's), the blocks
--   asked, the @kern_return_t@, the 16 bytes of CDB the shim wrote back,
--   the bytes the kext says it moved and the payload (an IF-RECV's: the
--   buffer's first bytes, as many as asked): an IF-RECV's payload, or none
--   for an IF-SEND; or why the transport fails the command, with the bytes
--   kept of it, and with it every command after:
--
-- * a @kern_return_t@ other than success: the shim refused the command or
--   the kext failed it, returning before it released the command's SCSI
--   task (TPerDriver.cpp's PerformSCSICommand), which a retry would leak
--   again; nothing is kept, and of an IF-SEND whether it reached the drive
--   is not known;
-- * a CDB other than hsed's ("Hsed.Transport.Cdb".'cdbFor', its last four
--   bytes zero): the drive was asked something other than hsed meant;
--   nothing is kept;
-- * success with another count of bytes moved than asked (a residue), on
--   any of the five (design 13.29 (d), decided for every command at M4c):
--   of an IF-RECV the rest of the buffer would be its own zeros, not the
--   TPer's, so nothing is taken, and the payload is kept beside the failure
--   as evidence alone (the run's JSON report prints it with the command);
--   an IF-SEND reached the drive, but not as hsed asked it. The message
--   says what to do.
judgeCommand
  :: InterfaceType -> Shape -> Word16 -> Word8 -> Word32 -> ByteString -> Word64 -> ByteString
  -> Either (Text, ByteString) ByteString
judgeCommand iface shape base blocks kr cdb moved received
  | kr /= 0 =
      Left ("hsed_kext_command answered " <> kernReturn kr <> " for " <> shapeName shape <> unknown
            <> ", so nothing more is issued on this drive in this run", mempty)
  | Right cdb /= expected =
      Left ("hsed_kext_command issued the CDB " <> cdbHex cdb <> " for " <> shapeName shape <> ", not hsed's "
            <> either id cdbHex expected <> ": the drive was asked something other than hsed meant, so nothing more "
            <> "is issued on this drive in this run", mempty)
  | moved /= asked = Left (residue, if sending then mempty else received)
  | otherwise = Right (if sending then mempty else received)
  where
    expected = (<> B.replicate 4 0) <$> cdbFor iface shape base blocks
    asked    = fromIntegral blocks * fromIntegral blockBytes :: Word64
    sending  = shapeDir shape == IfSend
    unknown  = if sending then ": whether it reached the drive is not known" else ""
    residue
      | sending =
          "hsed_kext_command: the kext moved " <> tshow moved <> " of the " <> tshow asked <> " bytes of "
          <> shapeName shape <> ", its kern_return success: the command reached the drive, but not as hsed asked it, "
          <> "and hsed takes no transfer of another length (design 13.29 (d)), so nothing more is issued on this "
          <> "drive in this run. Stop here: keep the run's reports, and settle design 13.29 (d) before another run"
      | otherwise =
          "hsed_kext_command: the kext moved " <> tshow moved <> " of the " <> tshow asked <> " bytes asked of "
          <> shapeName shape <> ", its kern_return success: hsed takes no transfer of another length (design 13.29 "
          <> "(d)), so nothing is taken of it and nothing more is issued on this drive in this run; the "
          <> tshow asked <> " bytes the buffer held are kept, as evidence only, with this command in the run's JSON "
          <> "report. Stop here: keep that report, and settle design 13.29 (d) before another run"

-- | The line the kext transport prints of each command it issues: the
--   disk, the command, its protocol, ComID and length, the CDB, the
--   @kern_return_t@ and the bytes transferred. It is given no payload, so
--   that none is printed (an IF-RECV's may be the MSID).
--
-- >>> commandLine "disk22" ResetSend 0x1004 1 (either (const mempty) id (cdbFor IfSAT ResetSend 0x1004 1)) 0 512
-- "kext disk22: STACK_RESET's HANDLE_COMID_REQUEST, protocol 0x02 ComID 0x1004, 512 bytes, CDB a1 0a 06 02 01 00 04 10 00 5e 00 00, kern_return kIOReturnSuccess (0x00000000), 512 bytes transferred"
commandLine :: Text -> Shape -> Word16 -> Word8 -> ByteString -> Word32 -> Word64 -> Text
commandLine disk shape c blocks cdb kr moved =
  "kext " <> disk <> ": " <> shapeName shape <> ", protocol " <> hex (shapeProto shape) <> " ComID " <> hex c <> ", "
  <> tshow (fromIntegral blocks * blockBytes) <> " bytes, CDB " <> cdbHex (B.take 12 cdb) <> ", kern_return "
  <> kernReturn kr <> ", " <> tshow moved <> " bytes transferred"

-- | A shape as the lines and the failures name it (Core 3.3.6.2, PDF 67;
--   Tables 29-30, PDF 62-63).
shapeName :: Shape -> Text
shapeName = \case
  Level0Recv -> "Level 0 Discovery's IF-RECV"
  PacketSend -> "a ComPacket's IF-SEND"
  PacketRecv -> "a ComPacket's IF-RECV"
  ResetSend  -> "STACK_RESET's HANDLE_COMID_REQUEST"
  ResetRecv  -> "STACK_RESET's GET_COMID_RESPONSE"


-- * kern_return_t

-- | A @kern_return_t@ by its name and its hex: IOKit's common codes
--   (IOReturn.h of the macOS SDK) and the few of Mach's an IOKit call
--   returns.
--
-- >>> kernReturn 0xe00002c5
-- "kIOReturnExclusiveAccess (0xe00002c5)"
kernReturn :: Word32 -> Text
kernReturn kr = fromMaybe "kern_return" (L.lookup kr names) <> " (0x" <> T.justifyRight 8 '0' (T.pack (showHex kr "")) <> ")"
  where
    names =
      [ (0x00000000, "kIOReturnSuccess")
      , (0x00000004, "KERN_INVALID_ARGUMENT")
      , (0x00000005, "KERN_FAILURE")
      , (0x10000003, "MACH_SEND_INVALID_DEST")
      , (0xe0000001, "kIOReturnInvalid")
      ] <> zip [0xe00002bc ..] common
    -- iokit_common_err 0x2bc to 0x2f0, in order; 0x2cb and 0x2dc are unassigned
    common =
      [ "kIOReturnError", "kIOReturnNoMemory", "kIOReturnNoResources", "kIOReturnIPCError", "kIOReturnNoDevice"
      , "kIOReturnNotPrivileged", "kIOReturnBadArgument", "kIOReturnLockedRead", "kIOReturnLockedWrite"
      , "kIOReturnExclusiveAccess", "kIOReturnBadMessageID", "kIOReturnUnsupported", "kIOReturnVMError"
      , "kIOReturnInternalError", "kIOReturnIOError", "kern_return", "kIOReturnCannotLock", "kIOReturnNotOpen"
      , "kIOReturnNotReadable", "kIOReturnNotWritable", "kIOReturnNotAligned", "kIOReturnBadMedia"
      , "kIOReturnStillOpen", "kIOReturnRLDError", "kIOReturnDMAError", "kIOReturnBusy", "kIOReturnTimeout"
      , "kIOReturnOffline", "kIOReturnNotReady", "kIOReturnNotAttached", "kIOReturnNoChannels", "kIOReturnNoSpace"
      , "kern_return", "kIOReturnPortExists", "kIOReturnCannotWire", "kIOReturnNoInterrupt", "kIOReturnNoFrames"
      , "kIOReturnMessageTooLarge", "kIOReturnNotPermitted", "kIOReturnNoPower", "kIOReturnNoMedia"
      , "kIOReturnUnformattedMedia", "kIOReturnUnsupportedMode", "kIOReturnUnderrun", "kIOReturnOverrun"
      , "kIOReturnDeviceError", "kIOReturnNoCompletion", "kIOReturnAborted", "kIOReturnNoBandwidth"
      , "kIOReturnNotResponding", "kIOReturnIsoTooOld", "kIOReturnIsoTooNew", "kIOReturnNotFound"
      ]
