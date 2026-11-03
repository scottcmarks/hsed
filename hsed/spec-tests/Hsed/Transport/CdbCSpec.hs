{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.CdbCSpec
Description : Test specifications for cbits/hsed_cdb.c, the CDBs of the five commands in C, run through FFI against Hsed.Transport.Cdb
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The portable C the kext shim takes every scalar it passes the kernel
from, every CDB, direction and length among them, every IF-SEND's payload
check and every bind's checks, run here through FFI
("Hsed.Transport.CdbFFI") on whatever builds the suite, so that the code
that writes the bytes a drive is sent, and decides what may be sent, runs
before any drive does (design 9.3, M4c):

* @hsed_cdb_build@ against "Hsed.Transport.Cdb".'cdbFor', written apart
  from it in Haskell, over 5 shapes, 2 interfaces, 256 counts of blocks and
  10 bases (25,600 cases): the same 12 bytes then 4 zeros, the direction
  the shape's, the length its blocks', or a refusal by both, and then
  nothing written but the CDB's zeros; every CDB it builds holding the
  invariants a drive's command must (one of three opcodes, SAT's
  direction bits and command agreeing with the direction, protocol 1 or 2,
  ComID 1 for Level 0 alone, protocol 2 in one block, bytes 12 to 15 zero),
  its direction and length the ones the kext reads from the CDB itself;
  NULLs and a shape outside the five refused;
* @hsed_cdb_base_ok@ as Core Table 26 reads in the generated code
  ("Hsed.Sim.Params".'reservedComId'), over all 65,536 ComIDs;
* @hsed_cdb_check_payload@: STACK_RESET's request for the base taken, and
  every change of one byte to it refused (130,560), another length, another
  base; the ComPackets the jig sends a drive taken on their ComID and
  refused on another; a ComPacket with a reserved byte, an extension,
  OutstandingData, MinTransfer, a Length past the transfer or a byte after
  it refused; a receive's buffer anything; NULL refused;
* @hsed_cdb_scalars@, what @hsed_kext_command@ passes the kernel, over the
  25,600 cases on a page-aligned buffer holding what each send carries:
  scalars 0 and 1 cdbFor's CDB as the kernel reads them (their 16 bytes in
  memory, UserClient.cpp:83), the buffer's address, its size, the
  direction the shape's and the length its blocks', the direction and the
  length the ones the kext reads from that CDB, and the CDB echoed those 16
  bytes; or the build's refusal; a buffer short of the length, off the
  page, a payload its shape does not carry, NULL and no page refused; each
  refusal writing nothing but zeros;
* @hsed_cdb_bind_check@, what @hsed_kext_bind@ decides, against a model
  over 972 binds: a base @hsed_cdb_base_ok@ takes, none bound or the
  same, exactly one user client, the serial unchanged and not empty;
* the return codes, as cbits/hsed_cdb.h defines them.
-}
module Hsed.Transport.CdbCSpec (spec) where

import           Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import           Foreign.ForeignPtr    (withForeignPtr)
import           Foreign.Marshal.Utils (copyBytes, fillBytes)
import           Foreign.Ptr           (Ptr, castPtr, nullPtr, plusPtr, ptrToWordPtr)
import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import qualified RIO.Text              as T
import           Test.Hspec

import           Hsed.Core.Record      (encodeRecord)
import           Hsed.Core.StreamItem  (StreamItem (..))
import           Hsed.Core.Token        (IsToken (..))
import           Hsed.Gen.Core.Layouts (STACK_RESETCommandRequest (..))
import           Hsed.Gen.Core.UIDs    (uMethodID_Get, uProperties, uStartSession)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uC_PIN_MSID)
import           Hsed.Jig.Options      (RunOptions (..), defaultRunOptions)
import           Hsed.Jig.Run          (driveCases, runOpened)
import           Hsed.Jig.Script       (controlComPacket, propertiesCall)
import           Hsed.Jig.Support      (silently)
import           Hsed.Jig.Target       (Target (..))
import           Hsed.Jig.Transcript   (TEntry (..), entries, newTranscript)
import           Hsed.Sim.Params       (defaultSimParams, reservedComId)
import           Hsed.Sim.Types        (SimParams (..))
import           Hsed.Transport.Cdb    (Shape (..), cdbFor, shapeCode, shapeDir, shapeProto)
import           Hsed.Transport.CdbFFI
import           Hsed.Transport.Drive  (InterfaceType (..))
import           Hsed.Transport.InProc (InProcOpts (..), newInProc)
import           Hsed.Transport.Kext   (ifRecvBuffer, pageBytes)
import           Hsed.Wire             (Dir (..), Request (..), Unit (..), comIdManagementProtocol, comPacketProtocol)


-- | The bases of the comparison: the reserved range's edges and Level 0's
--   and TPER_RESET's ComIDs, the first non-reserved ComID, the drive's base,
--   and others above (Core Table 26, PDF 55-56).
bases :: [Word16]
bases = [0x0000, 0x0001, 0x0004, 0x07FF, 0x0800, 0x0FFF, 0x1000, 0x1004, 0x8000, 0xFFFF]

-- | Every case: shape, interface, blocks, base.
cases :: [(Shape, InterfaceType, Word8, Word16)]
cases = [ (s, i, n, b) | s <- [minBound .. maxBound], i <- [IfSAT, IfSCSI], n <- [minBound .. maxBound], b <- bases ]

-- | The direction scalar of a direction: IOKit's kIODirectionIn, 1, and
--   kIODirectionOut, 2 (IOMemoryDescriptor.h; the kext copies the buffer
--   out for the one, UserClient.cpp:188-192, and in for the other,
--   165-171).
directionOf :: Dir -> Word32
directionOf = \d -> if d == IfSend then 2 else 1

-- | What the C built of a case, beside cdbFor's.
built :: (Shape, InterfaceType, Word8, Word16) -> IO ((Shape, InterfaceType, Word8, Word16), Built)
built c@(s, i, n, b) = (,) c <$> cdbBuild (satFlag i) (shapeCode s) b n

-- | Whether the C's answer is cdbFor's: the same bytes, then four zeros,
--   the shape's direction and the blocks' length; or both refuse, the C's
--   code the blocks' where they are wrong (none, or ComID management's other
--   than one) and the ComID's otherwise, and nothing written but the CDB's
--   zeros.
agrees :: ((Shape, InterfaceType, Word8, Word16), Built) -> Bool
agrees ((s, i, n, b), c) = case cdbFor i s b n of
  Right bytes ->
    bCode c == cdbOk && bCdb c == bytes <> B.replicate 4 0 && bDirection c == directionOf (shapeDir s)
      && bRequested c == fromIntegral n * 512
  Left _ ->
    bCode c == (if n == 0 || (shapeProto s == comIdManagementProtocol && n /= 1) then cdbEBlocks else cdbEComId)
      && bCdb c == B.replicate 16 0 && bDirection c == unwritten32 && bRequested c == unwritten64

-- | What a CDB the C built breaks of the invariants of a drive's command,
--   given the interface and the base it was built on: none, for a CDB the
--   shim may issue.
violations :: InterfaceType -> Word16 -> Built -> [Text]
violations i base c = map snd (filter (not . fst)
  [ (op `elem` [0xA1, 0xA2, 0xB5], "byte 0 is A1h, A2h or B5h")
  , (if i == IfSAT then op == 0xA1 else op `elem` [0xA2, 0xB5], "SAT's opcode is A1h, SCSI's A2h or B5h")
  , (i /= IfSAT || at 9 `elem` [0x5C, 0x5E], "SAT's byte 9 is 5Ch or 5Eh")
  , (i /= IfSAT || L.all (== out) [at 9 == 0x5E, at 1 == 0x0A, at 2 == 0x06],
       "on SAT, 5Eh, the direction 2, byte 1 0Ah and byte 2 06h go together")
  , (i /= IfSAT || out || (at 1, at 2) == (0x08, 0x0E), "SAT's receive is PIO Data-In from the device")
  , (i /= IfSCSI || (op == 0xB5) == out, "on SCSI, B5h and the direction 2 go together")
  , (i /= IfSCSI || at 4 == 0x80, "SCSI's byte 4 is INC_512")
  , (p `elem` [comPacketProtocol, comIdManagementProtocol], "the protocol is 1 or 2")
  , (cid /= 0x0001 || (p == comPacketProtocol && not out), "ComID 0x0001 is Level 0's IF-RECV of protocol 1 alone")
  , (cid == 0x0001 || (cid == base && cid >= 0x1000), "any other ComID is the base, 0x1000 or above")
  , (p /= comIdManagementProtocol || n == 1, "protocol 2 moves one block")
  , (B.drop 12 (bCdb c) == B.replicate 4 0, "bytes 12 to 15 are zero")
  , (bDirection c `elem` [1, 2], "the direction is 1 or 2")
  ])
  where
    at k     = fromMaybe 0 (listToMaybe (drop k (B.unpack (bCdb c))))
    op       = at 0
    out      = bDirection c == 2
    word h l = (fromIntegral h `shiftL` 8) .|. fromIntegral l :: Word16
    (p, cid, n)
      | i == IfSAT = (at 3, word (at 7) (at 6), at 4)
      | otherwise  = (at 1, word (at 2) (at 3), at 9)

-- | The direction and the length the kext itself reads from a CDB: an
--   A1h's direction from its PROTOCOL field, 4 In and 5 Out
--   (ATAProtocolDirections.cpp), and its size from its COUNT
--   (CDBAccess.cpp:46-50); A2h In and B5h Out, of the length asked, which
--   INC_512's blocks are (TPerDriver.cpp's PerformSCSICommand).
kextReads :: ByteString -> Maybe (Word32, Word64)
kextReads cdb = case B.unpack (B.take 12 cdb) of
  0xA1 : b1 : _ : _ : n : _ -> (\d -> (d, fromIntegral n * 512)) <$> L.lookup ((b1 `shiftR` 1) .&. 0x0F) [(4, 1), (5, 2)]
  [0xA2, _, _, _, 0x80, _, l3, l2, l1, l0, _, _] -> Just (1, blocks l3 l2 l1 l0 * 512)
  [0xB5, _, _, _, 0x80, _, l3, l2, l1, l0, _, _] -> Just (2, blocks l3 l2 l1 l0 * 512)
  _ -> Nothing
  where blocks a b c d = L.foldl' (\acc x -> acc * 256 + fromIntegral x) 0 [a, b, c, d]

-- | STACK_RESET's request for a ComID, as the jig's stackReset encodes it
--   (Core Table 35, PDF 65: the Extended ComID, the ComID and the static
--   extension 0x0000; the Request Code), zero-padded to its one block.
stackResetRequest :: Word16 -> ByteString
stackResetRequest c = either (const "") (\r -> r <> B.replicate (512 - B.length r) 0)
                             (encodeRecord (STACK_RESETCommandRequest (fromIntegral c `shiftL` 16)))

-- | A ComPacket on 0x1004 of a byte past its 20-byte header, Length 1,
--   zero-padded to 512, with a byte changed.
packetWith :: Int -> Word8 -> ByteString
packetWith k v = B.take k bytes <> B.singleton v <> B.drop (k + 1) bytes
  where bytes = B.pack ([0, 0, 0, 0, 0x10, 0x04, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0xF8]) <> B.replicate 491 0

-- | The IF-SENDs the jig sends a drive, as it sends the simulator them: the
--   requests of design 9.3's drive cases (UCT-01, UCT-02, UCT-03:1a-1c,
--   SPF-02:1) run on the simulator in 512-byte blocks, its base 0x1004 a
--   drive's.
jigSends :: IO [Request]
jigSends = withSystemTempDirectory "hsed-cdbc" $ \reports -> do
  let params = defaultSimParams { spUnits = UnitBlocks512 }
  (sim, control) <- newInProc params (InProcOpts UnitBlocks512 0)
  t <- newTranscript
  _ <- silently (runOpened defaultRunOptions { roTests = driveCases, roSim = params, roReportDir = reports } t
                           (SimTarget sim control))
  es <- entries t
  pure [ q | Issued _ _ q _ <- es, dir q == IfSend ]

-- | The ComPackets of design 9.3's drive cases, each told by what it
--   carries: SMUID.Properties with UCT-02's host properties, StartSession,
--   Get on C_PIN_MSID, and End of Session, a data subpacket of the one
--   token FAh (Core Table 17's 20-byte ComPacket header, Table 18's 24-byte
--   packet header and Table 19's 12-byte subpacket header before it, its
--   Length 1 at bytes 52 to 55; PDF 40-42).
jigKinds :: [(Text, ByteString -> Bool)]
jigKinds =
  [ ("Properties with the host's", \p -> holds (uidToken uProperties) p && holds "MaxComPacketSize" p)
  , ("StartSession", holds (uidToken uStartSession))
  , ("Get on C_PIN_MSID", \p -> holds (uidToken uMethodID_Get) p && holds (uidToken uC_PIN_MSID) p)
  , ("End of Session", \p -> B.take 4 (B.drop 52 p) == B.pack [0, 0, 0, 1] && B.take 1 (B.drop 56 p) == B.singleton 0xFA)
  ]
  where uidToken = generate . token

-- | Whether some bytes are among a payload's.
holds :: ByteString -> ByteString -> Bool
holds = B.isInfixOf

-- | What a send of a shape carries on a base, that the C takes:
--   STACK_RESET's request for it, or a ComPacket on it of no payload (its
--   header, then zeros); nothing for a receive.
carried :: Shape -> Word16 -> ByteString
carried s b = case s of
  ResetSend  -> stackResetRequest b
  PacketSend -> B.pack [0, 0, 0, 0, fromIntegral (b `shiftR` 8), fromIntegral b] <> B.replicate 14 0
  _          -> ""

-- | Bytes written at a pointer, after 512 zeros there.
place :: Ptr Word8 -> ByteString -> IO ()
place p bytes = do
  fillBytes p 0 512
  B.useAsCStringLen bytes (\(src, n) -> copyBytes p (castPtr src) n)

-- | Whether the C's scalars agree with cdbFor's CDB of a case for a buffer
--   at an address of a size: the CDB in scalars 0 and 1 as the kernel reads
--   them, the address, the size, the shape's direction and the blocks'
--   length, the echo those 16 bytes; or both refuse, the C's code as for
--   'agrees', and nothing written but zeros.
scalarsAgree :: Word64 -> Word64 -> ((Shape, InterfaceType, Word8, Word16), Scalars, ByteString) -> Bool
scalarsAgree address size ((s, i, n, b), r, inScalars) = case cdbFor i s b n of
  Right bytes ->
    sCode r == cdbOk && inScalars == bytes <> B.replicate 4 0 && sEcho r == inScalars
      && drop 2 (sScalars r) == [address, size, fromIntegral (directionOf (shapeDir s)), fromIntegral n * 512]
  Left _ ->
    sCode r == (if n == 0 || (shapeProto s == comIdManagementProtocol && n /= 1) then cdbEBlocks else cdbEComId)
      && sScalars r == replicate 6 0 && sEcho r == B.replicate 16 0

-- | What hsed_cdb_bind_check should answer, given the base, the base bound
--   (0 for none), the user clients counted, the serial now and the one
--   located (each as a C string, to its first NUL, of at most 63 bytes).
bindModel :: Word16 -> Word16 -> Int -> ByteString -> ByteString -> Int
bindModel base bound clients serial located
  | base < 0x1000 || (bound /= 0 && bound /= base) = cdbEComId
  | clients /= 1                                   = cdbEClients
  | B.null now || now /= B.takeWhile (/= 0) (B.take 63 located) = cdbESerial
  | otherwise                                      = cdbOk
  where now = B.takeWhile (/= 0) (B.take 63 serial)

spec :: Spec
spec = describe "cbits/hsed_cdb.c, through FFI" $ do
  describe "hsed_cdb_build" $ do
    it "builds cdbFor's CDB, the shape's direction and the blocks' length, or refuses where cdbFor does: 25,600 cases" $ do
      results <- traverse built cases
      length results `shouldBe` 25600
      [ c | (c, b) <- results, not (agrees (c, b)) ] `shouldBe` []
      -- Level 0 of 1 to 255 blocks on every base; a ComPacket's two on the four non-reserved bases; ComID management's
      -- two of one block on them
      length [ () | (_, b) <- results, bCode b == cdbOk ] `shouldBe` 2 * 255 * 10 + 2 * 2 * 255 * 4 + 2 * 2 * 1 * 4
    it "holds every CDB it builds to the invariants of a drive's command" $ do
      results <- traverse built cases
      [ (c, v) | (c@(_, i, _, b), r) <- results, bCode r == cdbOk, v <- violations i b r ] `shouldBe` []
    it "gives the kext the direction and the length the kext reads from the CDB itself (TPerDriver.cpp, CDBAccess.cpp)" $ do
      results <- traverse built cases
      [ c | (c, r) <- results, bCode r == cdbOk, kextReads (bCdb r) /= Just (bDirection r, bRequested r) ] `shouldBe` []
    it "builds Level 0 Discovery's IF-RECV on ComID 0x0001 whatever base it is given, never refused for it" $ do
      results <- traverse built [ (Level0Recv, i, 4, b) | i <- [IfSAT, IfSCSI], b <- bases ]
      [ c | (c@(_, i, _, _), r) <- results, Right (bCdb r) /= ((<> B.replicate 4 0) <$> cdbFor i Level0Recv 0x0001 4) ]
        `shouldBe` []
    it "refuses a shape outside the five, writing nothing but the CDB's zeros" $ do
      -- the C's int is 32 bits: its least and greatest among them
      results <- traverse (\s -> cdbBuild 1 s 0x1004 1) [-1, 5, 6, 255, fromIntegral (minBound :: Int32), fromIntegral (maxBound :: Int32)]
      [ r | r <- results, r /= Built cdbEShape (B.replicate 16 0) unwritten32 unwritten64 ] `shouldBe` []
    it "refuses a NULL CDB, direction or length, zeroing a CDB it was given" $ do
      answers <- traverse (\(a, b, c) -> cdbBuildNulls a b c) [(True, False, False), (False, True, False), (False, False, True)]
      answers `shouldBe` [(cdbEArgument, Nothing), (cdbEArgument, Just (B.replicate 16 0)), (cdbEArgument, Just (B.replicate 16 0))]

  describe "hsed_cdb_base_ok" $
    it "takes exactly the ComIDs Core Table 26 does not reserve, as the generated table reads (PDF 55-56): all 65,536" $ do
      oks <- traverse cdbBaseOk [minBound .. maxBound]
      [ c | (c, ok) <- zip [minBound .. maxBound :: Word16] oks, ok == reservedComId c ] `shouldBe` []
      length (filter id oks) `shouldBe` 0x10000 - 0x1000

  describe "hsed_cdb_check_payload" $ do
    it "takes STACK_RESET's request for the base, 512 bytes (Core Table 35, PDF 65)" $ do
      B.take 8 (stackResetRequest 0x1004) `shouldBe` B.pack [0x10, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02]
      cdbCheckPayload (shapeCode ResetSend) 0x1004 (stackResetRequest 0x1004) `shouldReturn` cdbOk
    it "refuses every change of one byte to it: 512 bytes, each of 255 other values, 130,560 requests" $
      cdbCheckEachByte (shapeCode ResetSend) 0x1004 (stackResetRequest 0x1004) `shouldReturn` []
    it "refuses it of 511 or 513 bytes, against another base, or as a ComPacket" $ do
      let request = stackResetRequest 0x1004
      answers <- sequence
        [ cdbCheckPayload (shapeCode ResetSend) 0x1004 (B.take 511 request)
        , cdbCheckPayload (shapeCode ResetSend) 0x1004 (request <> B.singleton 0)
        , cdbCheckPayload (shapeCode ResetSend) 0x1005 request
        , cdbCheckPayload (shapeCode PacketSend) 0x1004 request
        ]
      answers `shouldBe` [cdbEPayload, cdbEPayload, cdbEPayload, cdbEPayload]
      cdbCheckPayload (shapeCode ResetSend) 0x1005 (stackResetRequest 0x1005) `shouldReturn` cdbOk
    it "takes the ComPackets the jig sends a drive on their base, 0x1004, and refuses them on 0x1005" $ do
      sends <- jigSends
      let packets = [ q | q <- sends, proto q == comPacketProtocol ]
      [ q | q <- packets, comId q /= 0x1004 || lenBytes q /= 512 ] `shouldBe` []
      -- Properties with UCT-02's host properties, StartSession as Anybody, Get on C_PIN_MSID's PIN, End of Session
      [ name | (name, sent) <- jigKinds, not (any (sent . payload) packets) ] `shouldBe` []
      taken <- traverse (\q -> cdbCheckPayload (shapeCode PacketSend) 0x1004 (payload q)) packets
      elsewhere <- traverse (\q -> cdbCheckPayload (shapeCode PacketSend) 0x1005 (payload q)) packets
      (taken, elsewhere) `shouldBe` (map (const cdbOk) packets, map (const cdbEPayload) packets)
    it "takes Properties as the error cases send it (Script.controlComPacket), zero-padded to 512, on 0x1004 alone" $ do
      let bytes = either (const "") id (controlComPacket 0x1004 propertiesCall)
          padded = bytes <> B.replicate (512 - B.length bytes) 0
      answers <- traverse (\b -> cdbCheckPayload (shapeCode PacketSend) b padded) [0x1004, 0x1005, 0x0004]
      answers `shouldBe` [cdbOk, cdbEPayload, cdbEComId]
    it "refuses a ComPacket with a reserved byte set, an extension, OutstandingData, MinTransfer, a Length past the transfer, or a byte after it" $ do
      cdbCheckPayload (shapeCode PacketSend) 0x1004 (packetWith 20 0xF8) `shouldReturn` cdbOk
      answers <- traverse (cdbCheckPayload (shapeCode PacketSend) 0x1004)
        [ packetWith 0 1, packetWith 3 1, packetWith 7 1, packetWith 11 1, packetWith 15 1
        , packetWith 18 0x02, packetWith 21 1, packetWith 511 1, B.take 19 (packetWith 20 0xF8) ]
      answers `shouldBe` replicate 9 cdbEPayload
    it "takes any buffer of a receive, which sends nothing, and refuses a NULL buffer and a shape outside the five" $ do
      receives <- traverse (\s -> cdbCheckPayload (shapeCode s) 0x0000 (B.replicate 512 0xFF)) [Level0Recv, PacketRecv, ResetRecv]
      receives `shouldBe` [cdbOk, cdbOk, cdbOk]
      nulls <- traverse (\s -> cdbCheckPayloadNull (shapeCode s) 0x1004 512) [minBound .. maxBound]
      nulls `shouldBe` replicate 5 cdbEArgument
      outside <- traverse (\s -> cdbCheckPayload s 0x1004 (stackResetRequest 0x1004)) [-1, 5, 255]
      outside `shouldBe` replicate 3 cdbEShape
    it "refuses a send on a base Core Table 26 reserves" $ do
      answers <- traverse (\b -> cdbCheckPayload (shapeCode ResetSend) b (stackResetRequest b)) [0x0000, 0x0004, 0x0FFF]
      answers `shouldBe` replicate 3 cdbEComId

  describe "hsed_cdb_scalars" $ do
    it "gives the kext the six scalars and the echo from one build, the CDB cdbFor's as the kernel reads it, or refuses where cdbFor does: 25,600 cases" $ do
      (buf, size) <- ifRecvBuffer (255 * 512)
      results <- withForeignPtr buf $ \p -> for cases $ \c@(s, i, n, b) -> do
        place p (carried s b)
        r <- cdbScalarsAt (satFlag i) (shapeCode s) b n p (fromIntegral size) (fromIntegral pageBytes)
        inScalars <- scalarCdb (sScalars r)
        pure (c, r, inScalars, fromIntegral (ptrToWordPtr p))
      length results `shouldBe` 25600
      [ c | (c, r, inScalars, address) <- results, not (scalarsAgree address (fromIntegral size) (c, r, inScalars)) ] `shouldBe` []
      length [ () | (_, r, _, _) <- results, sCode r == cdbOk ] `shouldBe` 2 * 255 * 10 + 2 * 2 * 255 * 4 + 2 * 2 * 1 * 4
    it "gives the kext the direction and the length it reads from the CDB in scalars 0 and 1 (TPerDriver.cpp, CDBAccess.cpp)" $ do
      (buf, size) <- ifRecvBuffer (255 * 512)
      results <- withForeignPtr buf $ \p -> for cases $ \(s, i, n, b) -> do
        place p (carried s b)
        r <- cdbScalarsAt (satFlag i) (shapeCode s) b n p (fromIntegral size) (fromIntegral pageBytes)
        (,) r <$> scalarCdb (sScalars r)
      [ r | (r, inScalars) <- results, sCode r == cdbOk
          , fmap (\(d, l) -> [fromIntegral d, l]) (kextReads inScalars) /= Just (drop 4 (sScalars r)) ] `shouldBe` []
    it "refuses a buffer shorter than the length asked, one off the page, a payload its shape does not carry, NULL and no page, writing nothing but zeros" $ do
      (buf, size) <- ifRecvBuffer 4096
      answers <- withForeignPtr buf $ \p -> do
        place p (stackResetRequest 0x1004)
        sequence
          [ cdbScalarsAt 1 (shapeCode ResetSend) 0x1004 1 p 511 (fromIntegral pageBytes)
          , cdbScalarsAt 1 (shapeCode ResetRecv) 0x1004 1 (p `plusPtr` 512) 1024 (fromIntegral pageBytes)
          , cdbScalarsAt 1 (shapeCode PacketSend) 0x1004 1 p (fromIntegral size) (fromIntegral pageBytes)
          , cdbScalarsAt 1 (shapeCode ResetSend) 0x1005 1 p (fromIntegral size) (fromIntegral pageBytes)
          , cdbScalarsAt 1 (shapeCode ResetSend) 0x1004 1 nullPtr (fromIntegral size) (fromIntegral pageBytes)
          , cdbScalarsAt 1 (shapeCode ResetSend) 0x1004 1 p (fromIntegral size) 0 ]
      map sCode answers `shouldBe` [cdbESpace, cdbEAlign, cdbEPayload, cdbEPayload, cdbEArgument, cdbEArgument]
      [ a | a <- answers, sScalars a /= replicate 6 0 || sEcho a /= B.replicate 16 0 ] `shouldBe` []
    it "checks the payload in the very buffer passed: a STACK_RESET request one page on is not the buffer's" $ do
      (buf, size) <- ifRecvBuffer 32768
      answers <- withForeignPtr buf $ \p -> do
        place p ""
        place (p `plusPtr` pageBytes) (stackResetRequest 0x1004)
        sequence [ cdbScalarsAt 1 (shapeCode ResetSend) 0x1004 1 p (fromIntegral size) (fromIntegral pageBytes)
                 , cdbScalarsAt 1 (shapeCode ResetSend) 0x1004 1 (p `plusPtr` pageBytes) (fromIntegral size - fromIntegral pageBytes)
                                (fromIntegral pageBytes) ]
      map sCode answers `shouldBe` [cdbEPayload, cdbOk]

  describe "hsed_cdb_bind_check" $ do
    it "takes a base hsed_cdb_base_ok takes, none bound or the same, exactly one user client, the serial unchanged; refuses each other, as a model says: 972 binds" $ do
      let serials = [ ("S59BNM0RA15802W     ", "S59BNM0RA15802W     "), ("S59BNM0RA15802W", "S59BNM0RA15802X"), ("", "")
                    , ("S59BNM0RA15802W", "S59BNM0RA15802W     "), ("S59BNM0RA1580", "S59BNM0RA15802W"), ("", "S59BNM0RA15802W") ]
          binds   = [ (b, bound, n, sn) | b <- [0x0000, 0x0004, 0x0FFF, 0x1000, 0x1004, 0xFFFF], bound <- [0, 0x1004, 0x1005, 0x0FFF, b]
                                        , n <- [-1, 0, 1, 2, 3, 100], sn <- serials ]
          bindsUnique = L.nub binds
      answers <- for bindsUnique $ \(b, bound, n, (now, located)) -> (,) (b, bound, n, now, located) <$> cdbBindCheck b bound n now located 64
      length answers `shouldBe` 972
      [ (c, got) | (c@(b, bound, n, now, located), got) <- answers, got /= bindModel b bound n now located ] `shouldBe` []
      length [ () | (_, got) <- answers, got == cdbOk ] `shouldSatisfy` (> 0)
    it "refuses no size to compare in (an argument), and compares no further than the size" $ do
      cdbBindCheck 0x1004 0 1 "S59BNM0RA15802W" "S59BNM0RA15802W" 0 `shouldReturn` cdbEArgument
      cdbBindCheck 0x1004 0 1 "S59BNM0RA15802W" "S59BNM0RA15802X" 14 `shouldReturn` cdbOk
      cdbBindCheck 0x1004 0 1 "S59BNM0RA15802W" "S59BNM0RA15802X" 15 `shouldReturn` cdbESerial

  describe "the return codes" $
    it "are cbits/hsed_cdb.h's" $ do
      header <- decodeUtf8Lenient <$> readFileBinary "cbits/hsed_cdb.h"
      let defines = [ (n, v) | l <- T.lines header, "#define" : n : v : _ <- [T.words l], "HSED_CDB_" `T.isPrefixOf` n ]
      defines `shouldBe` zip [ "HSED_CDB_OK", "HSED_CDB_E_ARGUMENT", "HSED_CDB_E_SHAPE", "HSED_CDB_E_BLOCKS", "HSED_CDB_E_COMID", "HSED_CDB_E_PAYLOAD"
                             , "HSED_CDB_E_SPACE", "HSED_CDB_E_ALIGN", "HSED_CDB_E_CLIENTS", "HSED_CDB_E_SERIAL" ]
                             (map tshow [cdbOk, cdbEArgument, cdbEShape, cdbEBlocks, cdbEComId, cdbEPayload, cdbESpace, cdbEAlign, cdbEClients, cdbESerial])
