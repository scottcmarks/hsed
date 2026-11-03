{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.FakeKext
Description : The kext transport over the simulator: the real kextExchange and the real C of the CDBs, the simulator where the kernel and the drive would be
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The drive path below the Guard without the kext or the drive, for the
tests: a 'RawDrive' whose transport is "Hsed.Transport.Kext".'kextExchange'
and whose bind is 'kextBind', as "Hsed.Transport.MacKext" builds them, over
a shim that does in Haskell what @hsed_kext_command@ and @hsed_kext_bind@
do in C around their IOKit calls, calling the real portable C for the rest
(@hsed_cdb_build@, @hsed_cdb_check_payload@, @hsed_cdb_base_ok@, through
"Hsed.Transport.CdbFFI"), and puts the simulator in the place of the kernel
and the drive.

The fake shim, in @hsed_kext_command@'s order: the user client open; the
CDB, the direction and the length from one @hsed_cdb_build@ on the base it
has bound (a command on the base before the bind is
@kIOReturnNotReady@, any other refusal @kIOReturnBadArgument@); the buffer
as long as the length asked (@kIOReturnNoSpace@) and page-aligned
(@kIOReturnNotAligned@); the payload, the buffer's first bytes, checked by
@hsed_cdb_check_payload@ (@kIOReturnBadArgument@); then the CDB written
back and the command issued. Its bind, in @hsed_kext_bind@'s: the user
client open (@kIOReturnNotOpen@), a base @hsed_cdb_base_ok@ takes and none
other bound (@kIOReturnBadArgument@); the TPer, its one user client and its
serial are the simulator's, and hold by construction.

The kernel and the drive ('kernelRead'): the command the CDB says, read as
the kext reads it (TPerDriver.cpp's PerformSCSICommand: an A1h's direction
from its PROTOCOL field, ATAProtocolDirections.cpp, and its size from its
COUNT, CDBAccess.cpp:46-50; A2h In and B5h Out) and as the drive reads it
(the security protocol, the ComID, the blocks), which must agree with the
direction and the length the shim passed beside it; an IF-SEND's bytes, as
many as asked, given to the simulator, or an IF-RECV of that length whose
answer is written into the buffer; a status other than Good failing the
command as the kext fails a CHECK CONDITION, @kIOReturnError@; the bytes
moved those asked. Every CDB issued is recorded, its 12 bytes (what the
kext sends of an A1h, an A2h or a B5h), in order.

'FakeFaults' fail the nth command of a shape that reaches the kext with a
@kern_return_t@ (the simulator not asked), or say it moved no bytes (the
simulator asked).

'fakeKextDrive' is shaped like "Hsed.Jig.Support".'Hsed.Jig.Support.fakeDrive',
so that its drive goes to "Hsed.Jig.Support".'Hsed.Jig.Support.onFakeDrive'
as the run opens a drive, behind the Guard; 'fakeKextOn' opens the fake
kext again over the same simulator, as each run of the jig opens the kext's
user client anew (closing the transport closes the fake user client: a
later command is a 'Hsed.Wire.TransportFailure').
-}
module Hsed.Transport.FakeKext
  ( -- * Faults
    FakeFaults (..)
  , noFaults
    -- * The fake drive
  , fakeKextDrive
  , fakeKextOn
  , fakeDriveInfo
    -- * The kernel and the drive
  , kernelRead
  ) where

import           Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import           Foreign.Marshal.Utils (copyBytes)
import           Foreign.Ptr           (Ptr, castPtr, nullPtr, ptrToWordPtr)
import           Foreign.Storable      (poke)
import qualified RIO.ByteString        as B
import qualified RIO.Map               as Map
import           RIO

import           Hsed.Sim.Types        (SimParams (..))
import           Hsed.Transport.Cdb    (Shape (..), shapeCode)
import           Hsed.Transport.CdbFFI (Built (..), cdbBaseOk, cdbBuild, cdbCheckPayloadAt, cdbEComId, cdbOk, satFlag)
import           Hsed.Transport.Drive  (DriveInfo (..), InterfaceType (..), RawDrive (..))
import           Hsed.Transport.InProc (InProcOpts (..), SimControl, newInProc)
import           Hsed.Transport.Kext   (ShimCommand, kextBind, kextClose, kextExchange, newKextHandle, pageBytes)
import           Hsed.Wire.Internal    (Cap (..), Describe (..), Dir (..), IfStatus (..), Request (..), Response (..),
                                        Transport (..), Unit (..))


-- * Faults

-- | Failures the fake kext injects, each on the nth command of a shape (1,
--   the first) that reaches the kext past the shim's own checks, counted
--   from the fake user client's open.
data FakeFaults = FakeFaults
  { ffKernReturn :: !(Maybe (Shape, Int, Word32))  -- ^ answered with this @kern_return_t@, the simulator not asked
  , ffMoveNone   :: !(Maybe (Shape, Int))          -- ^ the simulator asked, and no bytes said to be moved
  }
  deriving (Eq, Show)

-- | No failure injected.
noFaults :: FakeFaults
noFaults = FakeFaults Nothing Nothing


-- * The fake drive

-- | What the IORegistry would say of the fake drive: disk6, serial
--   TESTSERIAL0001, kext build 625, SAT, registry id 0 (as
--   "Hsed.Jig.Support".'Hsed.Jig.Support.fakeDrive' says it).
fakeDriveInfo :: DriveInfo
fakeDriveInfo = DriveInfo "disk6" "TESTSERIAL0001" "625" IfSAT 0

-- | The simulator of the parameters, in 512-byte blocks as the kext's
--   transport counts (its @spUnits@ and the in-process transport's), behind
--   the fake kext on SAT ('fakeKextOn'), with 'fakeDriveInfo', the
--   simulator's control, and the CDBs the fake kext issues, in order.
fakeKextDrive :: SimParams -> FakeFaults -> IO (RawDrive, DriveInfo, SimControl, IORef [ByteString])
fakeKextDrive params faults = do
  (sim, control) <- newInProc params { spUnits = UnitBlocks512 } (InProcOpts UnitBlocks512 0)
  cdbs <- newIORef []
  raw <- fakeKextOn IfSAT sim faults cdbs
  pure (raw, fakeDriveInfo, control, cdbs)

-- | The fake kext's user client opened over a simulator, its CDBs built for
--   an interface and appended to the list given: a 'RawDrive' whose
--   transport is 'kextExchange' over the fake shim (role @drive@, units
--   blocks512, the device @/dev/disk6@, no events; its close the fake user
--   client's, once) and whose bind is 'kextBind' over the fake bind. Each
--   call opens it anew, nothing bound, the faults counted from none.
fakeKextOn :: InterfaceType -> Transport 'ReadWrite -> FakeFaults -> IORef [ByteString] -> IO RawDrive
fakeKextOn iface sim faults cdbs = do
  h <- newKextHandle (diBsd fakeDriveInfo) iface
  open <- newIORef True
  base <- newIORef 0
  counts <- newIORef Map.empty
  let quiet = const (pure ())
      transport = MkTransport
        { tExchange = kextExchange quiet (fakeShim iface sim faults cdbs open base counts) h
        , tInject   = \_ -> pure (Unsupported "the fake kext causes no events")
        , tUnit     = UnitBlocks512
        , tDescribe = Describe "drive" "fake kext, build 625" UnitBlocks512 Nothing Nothing Nothing (Just "/dev/disk6")
        , tClose    = kextClose quiet (fakeClose open base) h
        }
  pure (RawDrive transport (kextBind (fakeBind open base) h))

-- | What @hsed_kext_bind@ checks that the fake can fail: the user client
--   open, a base the C takes and none other bound.
fakeBind :: IORef Bool -> IORef Word16 -> Word16 -> IO Word32
fakeBind open base b = do
  isOpen <- readIORef open
  taken <- cdbBaseOk b
  bound <- readIORef base
  if | not isOpen                               -> pure kIOReturnNotOpen
     | not taken || (bound /= 0 && bound /= b)  -> pure kIOReturnBadArgument
     | otherwise                                -> 0 <$ writeIORef base b

-- | @hsed_kext_close@: the user client closed, nothing bound.
fakeClose :: IORef Bool -> IORef Word16 -> IO Word32
fakeClose open base = do
  was <- atomicModifyIORef' open (\o -> (False, o))
  writeIORef base 0
  pure (if was then 0 else kIOReturnNotOpen)

-- | @hsed_kext_command@ with the simulator for the kernel and the drive.
fakeShim
  :: InterfaceType -> Transport 'ReadWrite -> FakeFaults -> IORef [ByteString] -> IORef Bool -> IORef Word16
  -> IORef (Map Shape Int) -> ShimCommand
fakeShim iface sim faults cdbs open base counts shape blocks buf size pCdb pMoved = do
  isOpen <- readIORef open
  if not isOpen || buf == nullPtr || pCdb == nullPtr || pMoved == nullPtr
    then pure kIOReturnBadArgument
    else do
      poke pMoved 0
      b <- readIORef base
      built <- cdbBuild (satFlag iface) (shapeCode shape) b blocks
      let asked = bRequested built
      checked <- if bCode built == cdbOk && size >= asked && aligned buf
                   then cdbCheckPayloadAt (shapeCode shape) b buf asked
                   else pure cdbOk
      if | bCode built == cdbEComId && b == 0 -> pure kIOReturnNotReady
         | bCode built /= cdbOk                -> pure kIOReturnBadArgument
         | size < asked                        -> pure kIOReturnNoSpace
         | not (aligned buf)                   -> pure kIOReturnNotAligned
         | checked /= cdbOk                    -> pure kIOReturnBadArgument
         | otherwise                           -> do
             pokeBytes pCdb (bCdb built)
             issued shape (bCdb built) (bDirection built) asked buf pMoved
  where
    aligned p = ptrToWordPtr p `mod` fromIntegral pageBytes == 0

    -- the selector-2 call: the CDB recorded, the faults, the kernel and the drive
    issued s cdb direction asked p pMoved' = do
      n <- atomicModifyIORef' counts (\m -> let k = Map.findWithDefault 0 s m + 1 in (Map.insert s k m, k))
      modifyIORef' cdbs (<> [B.take 12 cdb])
      case ffKernReturn faults of
        Just (fs, fn, kr) | fs == s, fn == n -> kr <$ poke pMoved' asked
        _ -> case kernelRead cdb direction asked of
          Left _  -> pure kIOReturnUnsupported
          Right q -> do
            r <- case dir q of
              IfSend -> do
                sent <- B.packCStringLen (castPtr p, fromIntegral asked)
                tExchange sim q { payload = sent }
              IfRecv -> do
                answer <- tExchange sim q
                when (status answer == Good) $
                  B.useAsCStringLen (B.take (fromIntegral asked) (rpayload answer)) (\(src, len) -> copyBytes p (castPtr src) len)
                pure answer
            poke pMoved' (if ffMoveNone faults == Just (s, n) then 0 else asked)
            pure (if status r == Good then 0 else kIOReturnError)

-- | Sixteen bytes written at a pointer.
pokeBytes :: Ptr Word8 -> ByteString -> IO ()
pokeBytes p bytes = B.useAsCStringLen (B.take 16 (bytes <> B.replicate 16 0)) (\(src, n) -> copyBytes p (castPtr src) n)


-- * The kernel and the drive

-- | The command a CDB carries to the drive, given the direction scalar and
--   the length the shim passed beside it, as the kext and the drive read
--   it; or why the kext would not carry it as asked:
--
-- * A1h, SAT's ATA PASS-THROUGH (12): its direction from the PROTOCOL field
--   of byte 1, 4 PIO Data-In and 5 PIO Data-Out, no other moving data
--   (ATAProtocolDirections.cpp); its size COUNT's blocks (CDBAccess.cpp:46-50);
--   TRUSTED RECEIVE (5Ch) with Data-In and T_DIR from the device, TRUSTED
--   SEND (5Eh) with Data-Out and T_DIR to it, alone, BYTE_BLOCK set and
--   T_LENGTH in COUNT; FEATURES the protocol, LBA 23 to 8 the ComID, LBA 7
--   to 0 zero (SIIS 1.20 Tables 16-17, PDF 40-41);
-- * A2h and B5h, SCSI's SECURITY PROTOCOL IN and OUT: In and Out; INC_512
--   set, the length in blocks; the protocol and the ComID (Tables 12-13,
--   PDF 30-31);
--
-- and the direction scalar the CDB's (the kext copies the buffer in for
-- Out and out for In, UserClient.cpp:165-171 and 188-192), the length
-- asked the CDB's. The request's payload is left empty, for the caller to
-- fill with the bytes copied in.
kernelRead :: ByteString -> Word32 -> Word64 -> Either Text Request
kernelRead cdb direction asked = do
  (d, p, c, n) <- case B.unpack (B.take 12 cdb) of
    [0xA1, b1, b2, p, n, 0x00, lo, hi, _, command, _, _] -> do
      d <- case ((b1 `shiftR` 1) .&. 0x0F, b2, command) of
        (4, 0x0E, 0x5C) -> Right IfRecv
        (5, 0x06, 0x5E) -> Right IfSend
        other           -> Left ("an A1h of PROTOCOL, T_DIR to T_LENGTH and COMMAND " <> tshow other
                                 <> ": no TRUSTED RECEIVE or SEND of PIO counted in blocks in COUNT")
      Right (d, p, word hi lo, n)
    [0xA2, p, hi, lo, 0x80, _, 0, 0, 0, n, _, _] -> Right (IfRecv, p, word hi lo, n)
    [0xB5, p, hi, lo, 0x80, _, 0, 0, 0, n, _, _] -> Right (IfSend, p, word hi lo, n)
    _ -> Left "no CDB the kext carries as a TCG command"
  let bytes = fromIntegral n * 512 :: Word64
  unless (direction == (if d == IfSend then 2 else 1)) $
    Left ("the direction scalar " <> tshow direction <> " is not the CDB's")
  unless (bytes == asked && n /= 0) $
    Left ("the length asked, " <> tshow asked <> ", is not the CDB's " <> tshow bytes)
  pure (Request d p c (fromIntegral bytes) mempty)
  where
    word hi lo = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo


-- * kern_return_t

kIOReturnError, kIOReturnBadArgument, kIOReturnUnsupported, kIOReturnNotOpen, kIOReturnNotAligned, kIOReturnNotReady,
  kIOReturnNoSpace :: Word32
kIOReturnError       = 0xe00002bc
kIOReturnBadArgument = 0xe00002c2
kIOReturnUnsupported = 0xe00002c7
kIOReturnNotOpen     = 0xe00002cd
kIOReturnNotAligned  = 0xe00002d0
kIOReturnNotReady    = 0xe00002d8
kIOReturnNoSpace     = 0xe00002db
