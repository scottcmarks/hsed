{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Kext
Description : What the kext transport decides without the kext: the disk, the drive admitted or refused, the shim's answer judged
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The decisions of "Hsed.Transport.MacKext" that need neither IOKit nor the
kext, here so that every build compiles them and the tests run them (the
module that calls the C shim is compiled only under the package's flag
@kext@):

* the whole disk a target names ('diskName');
* what @hsed_kext_locate@ read of the TPer ('Located', laid out as
  'infoLayout' says @hsed_kext_info@ is) admitted as the drive to open, or
  refused: other user clients already on it, or a serial other than the
  one expected, which a drive target must name ('admit');
* the buffer an IF-RECV moves into ('bufferBytes', 'ifRecvBuffer'), and
  what one @hsed_kext_level0@ call answered, judged ('judgeLevel0') and
  printed ('level0Line');
* a @kern_return_t@ named ('kernReturn').
-}
module Hsed.Transport.Kext
  ( -- * hsed_kext_info
    InfoLayout (..)
  , infoLayout
  , Located (..)
    -- * Before anything is opened
  , diskName
  , admit
    -- * One Level 0 IF-RECV through the shim
  , pageBytes
  , bufferBytes
  , ifRecvBuffer
  , judgeLevel0
  , level0Line
    -- * kern_return_t
  , kernReturn
  ) where

import           Foreign.ForeignPtr    (ForeignPtr, mallocForeignPtrBytes, plusForeignPtr, withForeignPtr)
import           Foreign.Marshal.Utils (fillBytes)
import           Foreign.Ptr           (alignPtr, minusPtr)
import           Numeric               (showHex)
import qualified RIO.ByteString        as B
import           RIO
import qualified RIO.Char              as C
import qualified RIO.List              as L
import qualified RIO.Text              as T

import           Hsed.Transport.Cdb    (cdbHex, level0Cdb)
import           Hsed.Transport.Drive  (DriveInfo (..), InterfaceType (..), maskSerial)


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
  }
  deriving (Eq, Show)

-- | @hsed_kext_info@: 120 bytes aligned to 8; @conn@ at 0, @registry_id@ at
--   8, @sat@ at 16, @serial[64]@ at 20, @kext_build[32]@ at 84,
--   @other_clients@ at 116.
infoLayout :: InfoLayout
infoLayout = InfoLayout
  { ilBytes          = 120
  , ilAlignment      = 8
  , ilConn           = 0
  , ilRegistryId     = 8
  , ilSat            = 16
  , ilSerial         = 20
  , ilSerialBytes    = 64
  , ilKextBuild      = 84
  , ilKextBuildBytes = 32
  , ilOtherClients   = 116
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

-- | The BSD name of the whole disk a target names, @/dev/disk6@ or @disk6@:
--   @disk6@. Anything else is refused: a partition (@disk6s1@), a raw device
--   (@rdisk6@), another path. The shim refuses a disk that is not directly
--   on the kext's TPer besides.
--
-- >>> diskName "/dev/disk6"
-- Right "disk6"
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


-- * One Level 0 IF-RECV through the shim

-- | The IF-RECV buffer's alignment and the unit of its size: 16 KiB, the
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
--   kext writes the buffer from the kernel.
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

-- | One @hsed_kext_level0@ call judged, from the drive's interface, the
--   blocks asked, the @kern_return_t@, the 16 bytes of CDB the shim wrote
--   back, the bytes the kext says it moved and the payload (the buffer's
--   first bytes, as many as asked): the payload, or why the transport fails
--   the command, with the bytes kept of it, and with it every command after:
--
-- * a @kern_return_t@ other than success: the kext returned before releasing
--   the command's SCSI task (TPerDriver.cpp's PerformSCSICommand), which a
--   retry would leak again; nothing is kept;
-- * a CDB other than hsed's ("Hsed.Transport.Cdb"), its last four bytes
--   zero: the drive was asked something other than hsed meant; nothing is
--   kept;
-- * success with another count of bytes moved than asked (a residue): the
--   rest of the buffer would be its own zeros, not the TPer's, so nothing is
--   captured, and the payload is kept beside the failure as evidence alone
--   (the run's JSON report prints it with the command), the message saying
--   what to do. Whether a residue on a good transfer may be taken, the
--   kext's count written beside the capture, is design 13.29 (d), Scott's.
judgeLevel0 :: InterfaceType -> Word8 -> Word32 -> ByteString -> Word64 -> ByteString -> Either (Text, ByteString) ByteString
judgeLevel0 iface blocks kr cdb moved payload
  | kr /= 0 = Left ("hsed_kext_level0: " <> kernReturn kr, mempty)
  | Right cdb /= expected =
      Left ("hsed_kext_level0 issued the CDB " <> cdbHex cdb <> ", not hsed's " <> either id cdbHex expected, mempty)
  | moved /= fromIntegral (B.length payload) = Left (residue, payload)
  | otherwise = Right payload
  where
    expected = (<> B.replicate 4 0) <$> level0Cdb iface (fromIntegral blocks)
    asked    = tshow (B.length payload)
    residue  =
      "hsed_kext_level0: the kext moved " <> tshow moved <> " of the " <> asked <> " bytes asked, its kern_return success: "
      <> "hsed takes no transfer of another length (design 13.29 (d)), so nothing is captured and nothing more is issued "
      <> "on this drive in this run; the " <> asked <> " bytes the buffer held are kept, as evidence only, with this "
      <> "command in the run's JSON report. Stop here: keep that report, and settle design 13.29 (d) before another run"

-- | The line the kext transport prints of each command it issues: the disk,
--   the CDB, the @kern_return_t@ and the bytes transferred.
level0Line :: Text -> ByteString -> Word32 -> Word64 -> Text
level0Line disk cdb kr moved =
  "kext " <> disk <> ": Level 0 IF-RECV, CDB " <> cdbHex (B.take 12 cdb) <> ", kern_return " <> kernReturn kr
  <> ", " <> tshow moved <> " bytes transferred"


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
