{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples     #-}

{-|
Module      : Hsed.Transport.KextSpec
Description : Test specifications for Hsed.Transport.Kext, the kext transport over a shim of the test's, and FakeKext's round trip
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The kext transport as it is but for its calls into the C shim, which nothing
here makes (no IOKit, no device): the whole disk a target names; the TPer
the registry describes admitted, or refused for other user clients, another
serial or none expected, the serial printed only masked; each command's
buffer, its size, its alignment, its payload, and that the RTS has it
pinned where it was put across collections.

'kextExchange' over a shim of the test's that builds each CDB with the real
@hsed_cdb_build@ on the base its bind was given, as @hsed_kext_command@
does, checks a send's payload with the real @hsed_cdb_check_payload@, and
counts its calls: Level 0 Discovery's IF-RECV alone before the bind, the
five after it, each CDB cdbFor's; a command none of the five refused before
the shim; the latch on a @kern_return_t@, a CDB not hsed's, a residue (an
IF-RECV's bytes kept as evidence, an IF-SEND said to have reached the
drive), a shim that throws, a lost answer; the lines printed, never the
payload; the command uninterruptible, a 'timeout' returning only after the
shim, the transport latched. 'kextBind' and 'kextClose' with it.

'judgeCommand' and 'commandLine' on values; @kern_return_t@ codes named as
IOReturn.h names them; @hsed_kext_info@ laid out as cbits/hsed_kext.h
declares it.

"Hsed.Transport.FakeKext", the drive path the integration's runs take: a
round trip through it on SAT and on SCSI (Level 0, the bind, Properties
sent and its answer received, the CDBs recorded cdbFor's), and its faults.
-}
module Hsed.Transport.KextSpec (spec) where

import           Foreign.ForeignPtr     (touchForeignPtr, withForeignPtr)
import           Foreign.Marshal.Utils  (copyBytes)
import           Foreign.Ptr            (castPtr, ptrToWordPtr)
import           Foreign.Storable       (poke)
import qualified GHC.Clock              as Clock
import           GHC.Exts               (Int (..), addr2Int#, byteArrayContents#, isMutableByteArrayPinned#, isTrue#,
                                         unsafeCoerce#)
import           GHC.ForeignPtr         (ForeignPtr (..), ForeignPtrContents (..))
import           GHC.IO                 (IO (..))
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO
import qualified RIO.Text               as T
import           System.Mem             (performMajorGC, performMinorGC)
import           Test.Hspec
import           Test.Hspec.QuickCheck  (prop)

import           Hsed.Core.Record       (encodeRecord)
import           Hsed.Gen.Core.Layouts  (STACK_RESETCommandRequest (..))
import           Hsed.Jig.Script        (controlComPacket, propertiesCall)
import           Hsed.Sim.Params        (defaultSimParams)
import           Hsed.Sim.Types         (SimParams (..))
import           Hsed.Transport.Cdb     (Shape (..), cdbFor, cdbHex, shapeCode, shapeDir)
import           Hsed.Transport.CdbFFI  (Built (..), cdbBuild, cdbCheckPayloadAt, cdbOk)
import           Hsed.Transport.Drive   (DriveInfo (..), InterfaceType (..), RawDrive (..))
import           Hsed.Transport.FakeKext (FakeFaults (..), fakeKextDrive, fakeKextOn, noFaults)
import           Hsed.Transport.InProc  (InProcOpts (..), newInProc)
import           Hsed.Transport.Kext
import           Hsed.Wire              (Dir (..), IfStatus (..), Request (..), Response (..), Unit (..), defaultExchangeOpts,
                                         exchangeComPacket, newChannel)
import           Hsed.Wire.Internal     (Transport (..))


-- | disk6 as the registry described its TPer (an unprivileged ioreg, 2026-10-30; the drive is disk22 since).
disk6 :: Located
disk6 = Located
  { lRegistryId   = 0x100000a0d
  , lInterface    = IfSAT
  , lSerial       = "S59BNM0RA15802W     "
  , lKextBuild    = "625"
  , lOtherClients = 0
  }

-- | What hsed admits disk6 as.
disk6Info :: DriveInfo
disk6Info = DriveInfo
  { diBsd        = "disk6"
  , diSerial     = "S59BNM0RA15802W"
  , diKextBuild  = "625"
  , diInterface  = IfSAT
  , diRegistryId = 0x100000a0d
  }

-- | Where the RTS has the array a heap 'ForeignPtr' holds, and whether it
--   says the array is pinned; 'Nothing' for a pointer not of GHC's heap.
heapArray :: ForeignPtr a -> IO (Maybe (Int, Bool))
heapArray (ForeignPtr _ contents) = case contents of
  MallocPtr mba _ -> Just <$> probe mba
  PlainPtr mba    -> Just <$> probe mba
  _               -> pure Nothing
  where
    probe mba = IO $ \s ->
      (# s, (I# (addr2Int# (byteArrayContents# (unsafeCoerce# mba))), isTrue# (isMutableByteArrayPinned# mba)) #)

-- | A shape's CDB on SAT, as the shim writes it back: cdbFor's 12 bytes, then four zeros.
satCdb :: Shape -> Word16 -> Word8 -> ByteString
satCdb s c n = either (const "") (<> B.replicate 4 0) (cdbFor IfSAT s c n)


-- * Requests

-- | An IF-RECV.
recv :: Word8 -> Word16 -> Word32 -> Request
recv p c n = Request { dir = IfRecv, proto = p, comId = c, lenBytes = n, payload = "" }

-- | Level 0 Discovery's IF-RECV of 2048 bytes.
level0 :: Request
level0 = recv 0x01 0x0001 2048

-- | Properties on 0x1004 as the error cases send it, in one block.
propertiesSend :: Request
propertiesSend = Request IfSend 0x01 0x1004 512 (padded (either (const "") id (controlComPacket 0x1004 propertiesCall)))

-- | STACK_RESET's request for 0x1004 (Core Table 35, PDF 65), in one block.
resetSend :: Request
resetSend = Request IfSend 0x02 0x1004 512
  (padded (either (const "") id (encodeRecord (STACK_RESETCommandRequest 0x10040000))))

-- | The five commands on 0x1004, each with its shape.
fiveOnBase :: [(Shape, Request)]
fiveOnBase =
  [ (Level0Recv, level0), (PacketSend, propertiesSend), (PacketRecv, recv 0x01 0x1004 2048)
  , (ResetSend, resetSend), (ResetRecv, recv 0x02 0x1004 512) ]

-- | Bytes zero-padded to a block.
padded :: ByteString -> ByteString
padded b = b <> B.replicate (512 - B.length b) 0


-- * The test's shim

-- | How the test's shim answers.
data Knobs = Knobs
  { kKernReturn :: !Word32
  , kCdb        :: ByteString -> ByteString  -- ^ what becomes of the 16 bytes of CDB it writes back
  , kMoved      :: Word64 -> Word64          -- ^ the bytes it says moved, of those asked
  , kAnswer     :: !ByteString               -- ^ what an IF-RECV moves into the buffer
  , kDelay      :: !Int                      -- ^ the microseconds the call takes
  , kThrows     :: !Bool
  }

-- | A shim that does as asked.
straight :: Knobs
straight = Knobs 0 id id "" 0 False

-- | The kext transport over the test's shim, with its calls counted, the
--   base its bind was given, its lines and its knobs.
data Rig = Rig
  { rHandle :: !KextHandle
  , rCalls  :: !(IORef Int)
  , rBase   :: !(IORef Word16)
  , rLines  :: !(IORef [Text])
  , rKnobs  :: !(IORef Knobs)
  }

newRig :: IO Rig
newRig = Rig <$> newKextHandle "disk22" IfSAT <*> newIORef 0 <*> newIORef 0 <*> newIORef [] <*> newIORef straight

-- | A command through the rig: the test's shim, as @hsed_kext_command@
--   does it, builds the CDB with the real C on the base its bind was given
--   (a refusal is kIOReturnBadArgument), checks a send's payload with the
--   real C, then answers as the knobs say.
exchange :: Rig -> Request -> IO Response
exchange r = kextExchange (\l -> modifyIORef' (rLines r) (<> [l])) shim (rHandle r)
  where
    shim shape blocks buf size pCdb pMoved = do
      modifyIORef' (rCalls r) (+ 1)
      k <- readIORef (rKnobs r)
      base <- readIORef (rBase r)
      built <- cdbBuild 1 (shapeCode shape) base blocks
      checked <- if bCode built == cdbOk && size >= bRequested built
                   then cdbCheckPayloadAt (shapeCode shape) base buf (bRequested built)
                   else pure (bCode built)
      if checked /= cdbOk || bCode built /= cdbOk
        then pure 0xe00002c2
        else do
          when (kThrows k) (throwString "the test's shim throws")
          B.useAsCStringLen (B.take 16 (kCdb k (bCdb built) <> B.replicate 16 0)) (\(src, n) -> copyBytes pCdb (castPtr src) n)
          when (shapeDir shape == IfRecv) $
            B.useAsCStringLen (B.take (fromIntegral (bRequested built)) (kAnswer k)) (\(src, n) -> copyBytes buf (castPtr src) n)
          poke pMoved (kMoved k (bRequested built))
          threadDelay (kDelay k)
          pure (kKernReturn k)

-- | The rig's bind: the shim's takes any base, as the test's.
bindRig :: Rig -> Word16 -> IO (Either Text ())
bindRig r = kextBind (\b -> 0 <$ writeIORef (rBase r) b) (rHandle r)

-- | Whether a response is a transport failure saying something.
failsWith :: Text -> Response -> Bool
failsWith t r = case status r of
  TransportFailure why -> t `T.isInfixOf` why
  _                    -> False

-- | Whether a response is a refusal to carry the command, before the shim.
refused :: Response -> Bool
refused r = case status r of
  Unsupported _ -> B.null (rpayload r)
  _             -> False


spec :: Spec
spec = describe "Hsed.Transport.Kext" $ do
  describe "diskName" $ do
    it "takes a whole disk, as a device or a BSD name" $
      map diskName ["/dev/disk22", "disk22", "/dev/disk6"] `shouldBe` [Right "disk22", Right "disk22", Right "disk6"]
    it "refuses a partition, a raw device, a volume, no number" $
      filter (isRight . diskName) ["/dev/disk22s1", "/dev/rdisk22", "rdisk22", "/Volumes/X", "disk", "/dev/", "", "disk22 ", "disk22\0"]
        `shouldBe` []

  describe "admit" $ do
    it "admits a TPer no one has open, with the serial expected, the serial trimmed" $ do
      admit "S59BNM0RA15802W" "disk6" disk6 `shouldBe` Right disk6Info
      admit "  S59BNM0RA15802W " "disk6" disk6 `shouldBe` Right disk6Info
    it "refuses to open a drive no serial names: one is required" $
      map (\e -> admit e "disk6" disk6) ["", "   "] `shouldSatisfy` all isLeft
    it "refuses a TPer whose user clients are already open, or could not be counted" $ do
      admit "S59BNM0RA15802W" "disk6" disk6 { lOtherClients = 1 } `shouldSatisfy` isLeft
      admit "S59BNM0RA15802W" "disk6" disk6 { lOtherClients = -1 } `shouldSatisfy` isLeft
    it "refuses another serial, or none, printing each only masked" $ do
      let refusals = lefts
            [ admit "S59BNM0RA15802X" "disk6" disk6
            , admit "S59BNM0RA15802W" "disk6" disk6 { lSerial = "" }
            ]
      length refusals `shouldBe` 2
      filter (\t -> any (`T.isInfixOf` t) ["S59BNM0RA15802W", "S59BNM0RA15802X"]) refusals `shouldBe` []
      refusals `shouldSatisfy` all ("***********802X" `T.isInfixOf`) . take 1
    it "keeps the interface and the registry entry id, and names a kext build it could not read unknown" $ do
      diInterface <$> admit "S59BNM0RA15802W" "disk6" disk6 { lInterface = IfSCSI } `shouldBe` Right IfSCSI
      diKextBuild <$> admit "S59BNM0RA15802W" "disk6" disk6 { lKextBuild = "" } `shouldBe` Right "unknown"

  describe "a command's buffer" $ do
    it "is whole 16 KiB pages, one at least" $
      map bufferBytes [0, 512, 2048, 16384, 16385, 130560] `shouldBe` [16384, 16384, 16384, 16384, 32768, 131072]
    prop "holds the transfer, in whole pages" $ \n ->
      let b = bufferBytes n in b `mod` pageBytes == 0 && b >= fromIntegral n && b - fromIntegral n < pageBytes + 1
    it "is allocated zeroed and aligned to the page, whatever its size" $ do
      buffers <- traverse ifRecvBuffer [512, 2048, 16384, 16385, 130560]
      checks <- traverse (\(buf, size) -> withForeignPtr buf $ \p -> do
                            bytes <- B.packCStringLen (castPtr p, size)
                            pure (ptrToWordPtr p `mod` fromIntegral pageBytes, size, B.all (== 0) bytes)) buffers
      checks `shouldBe` [(0, 16384, True), (0, 16384, True), (0, 16384, True), (0, 32768, True), (0, 131072, True)]
    it "is an array the RTS has pinned, which stays where it was across minor and major collections while held" $ do
      -- many of them, so that an array the RTS took for an unpinned one would be found: GHC 9.4's aligned
      -- allocation at 16 KiB gave arrays whose header lay past their block group's first block, unpinned and
      -- moved by about three collections in four (design 7.2)
      buffers <- traverse (ifRecvBuffer . fromIntegral) (concat (replicate 4 [512 :: Int, 2048, 16384, 16385, 130560]))
      allocated <- traverse (heapArray . fst) buffers
      performMinorGC >> performMajorGC >> performMinorGC >> performMajorGC
      collected <- traverse (heapArray . fst) buffers
      traverse_ (touchForeignPtr . fst) buffers
      map (fmap snd) allocated `shouldBe` map (const (Just True)) buffers
      collected `shouldBe` allocated
    it "is commandBuffer's for every command: pinned, page-aligned, an IF-SEND's payload first and zeros after, an IF-RECV's all zeros" $ do
      let sent = B.pack (take 1024 (L.cycle [1 .. 255]))
      buffers <- traverse commandBuffer [Request IfSend 0x01 0x1004 1024 sent, recv 0x01 0x1004 2048, resetSend]
      contents <- traverse (\(buf, size) -> withForeignPtr buf $ \p ->
                              (,) (ptrToWordPtr p `mod` fromIntegral pageBytes) <$> B.packCStringLen (castPtr p, size)) buffers
      pinned <- traverse (heapArray . fst) buffers
      map (fmap snd) pinned `shouldBe` replicate 3 (Just True)
      contents `shouldBe`
        [ (0, sent <> B.replicate (16384 - 1024) 0), (0, B.replicate 16384 0)
        , (0, payload resetSend <> B.replicate (16384 - 512) 0) ]

  describe "kextExchange, over a shim that builds its CDB in the C" $ do
    it "carries Level 0 Discovery's IF-RECV alone before the bind: a ComPacket's IF-SEND is refused, the shim not called" $ do
      r <- newRig
      unbound <- exchange r propertiesSend
      callsBefore <- readIORef (rCalls r)
      l0 <- exchange r level0
      (refused unbound, callsBefore, status l0, B.length (rpayload l0)) `shouldBe` (True, 0, Good, 2048)
      readIORef (rCalls r) `shouldReturn` 1
    it "carries each of the five on the base once kextBind has bound it, the CDB each issued cdbFor's" $ do
      r <- newRig
      bindRig r 0x1004 `shouldReturn` Right ()
      answers <- traverse (exchange r . snd) fiveOnBase
      map status answers `shouldBe` replicate 5 Good
      map (B.length . rpayload) answers `shouldBe` [2048, 0, 2048, 0, 512]
      readIORef (rCalls r) `shouldReturn` 5
      ls <- readIORef (rLines r)
      [ l | ((s, q), l) <- zip fiveOnBase ls
          , not (("CDB " <> cdbHex (B.take 12 (satCdb s (comId q) (fromIntegral (lenBytes q `div` 512))))) `T.isInfixOf` l) ]
        `shouldBe` []
    it "refuses before the shim a command none of the five is: another ComID, protocol, length, or a payload not its length" $ do
      r <- newRig
      _ <- bindRig r 0x1004
      answers <- traverse (exchange r)
        [ recv 0x01 0x1005 2048, recv 0x01 0x0004 512, recv 0x00 0x1004 512, recv 0x01 0x1004 1000, recv 0x02 0x1004 1024
        , propertiesSend { payload = B.take 511 (payload propertiesSend) }
        , propertiesSend { payload = payload propertiesSend <> B.singleton 0 } ]
      filter (not . refused) answers `shouldBe` []
      readIORef (rCalls r) `shouldReturn` 0
    it "latches on a kern_return other than success: that command and every later one fail, the shim not called again" $ do
      r <- newRig
      _ <- bindRig r 0x1004
      writeIORef (rKnobs r) straight { kKernReturn = 0xe00002bc }
      first' <- exchange r (recv 0x01 0x1004 2048)
      writeIORef (rKnobs r) straight
      later <- traverse (exchange r) [level0, propertiesSend, resetSend]
      first' `shouldSatisfy` failsWith "hsed_kext_command answered kIOReturnError (0xe00002bc) for a ComPacket's IF-RECV"
      rpayload first' `shouldBe` ""
      filter (not . failsWith "a command failed before (hsed_kext_command answered kIOReturnError") later `shouldBe` []
      readIORef (rCalls r) `shouldReturn` 1
    it "says of a send the kext failed that whether it reached the drive is not known" $ do
      r <- newRig
      _ <- bindRig r 0x1004
      writeIORef (rKnobs r) straight { kKernReturn = 0xe00002bc }
      exchange r resetSend >>= (`shouldSatisfy` failsWith "STACK_RESET's HANDLE_COMID_REQUEST: whether it reached the drive is not known")
    it "latches on a CDB echoed other than cdbFor's: another opcode, a byte past the twelfth, none" $
      forM_ [\c -> B.take 9 c <> B.singleton 0x5E <> B.drop 10 c, \c -> B.take 15 c <> B.singleton 1, const ""] $ \corrupt -> do
        r <- newRig
        writeIORef (rKnobs r) straight { kCdb = corrupt }
        answer <- exchange r level0
        answer `shouldSatisfy` failsWith "not hsed's a1 08 0e 01 04 00 01 00 00 5c 00 00 00 00 00 00"
        rpayload answer `shouldBe` ""
        exchange r level0 >>= (`shouldSatisfy` failsWith "a command failed before")
        readIORef (rCalls r) `shouldReturn` 1
    it "latches on a residue of an IF-RECV, its bytes kept as evidence, saying what to do" $
      forM_ [1536, 2560, 0] $ \moved -> do
        r <- newRig
        let answer = B.pack (take 2048 (L.cycle [0x41 .. 0x5A]))
        writeIORef (rKnobs r) straight { kMoved = const moved, kAnswer = answer }
        a <- exchange r level0
        rpayload a `shouldBe` answer
        a `shouldSatisfy` failsWith ("moved " <> tshow moved <> " of the 2048 bytes asked of Level 0 Discovery's IF-RECV")
        a `shouldSatisfy` failsWith "kept, as evidence only"
        a `shouldSatisfy` failsWith "settle design 13.29 (d) before another run"
        exchange r level0 >>= (`shouldSatisfy` failsWith "a command failed before")
        readIORef (rCalls r) `shouldReturn` 1
    it "latches on a residue of an IF-SEND, saying the command reached the drive (design 13.29 (d))" $
      forM_ [propertiesSend, resetSend] $ \q -> do
        r <- newRig
        _ <- bindRig r 0x1004
        writeIORef (rKnobs r) straight { kMoved = const 0 }
        a <- exchange r q
        rpayload a `shouldBe` ""
        a `shouldSatisfy` failsWith "moved 0 of the 512 bytes of "
        a `shouldSatisfy` failsWith "the command reached the drive, but not as hsed asked it"
        a `shouldSatisfy` failsWith "settle design 13.29 (d) before another run"
        exchange r level0 >>= (`shouldSatisfy` failsWith "a command failed before")
        readIORef (rCalls r) `shouldReturn` 1
    it "latches when the shim throws" $ do
      r <- newRig
      writeIORef (rKnobs r) straight { kThrows = True }
      exchange r level0 >>= (`shouldSatisfy` failsWith "the shim threw on Level 0 Discovery's IF-RECV")
      writeIORef (rKnobs r) straight
      exchange r level0 >>= (`shouldSatisfy` failsWith "a command failed before")
      readIORef (rCalls r) `shouldReturn` 1
    it "prints a line of each command, never its payload: an answer carrying MSIDMSIDMSID leaves the lines without it" $ do
      r <- newRig
      _ <- bindRig r 0x1004
      let msid = "MSIDMSIDMSID"
      writeIORef (rKnobs r) straight { kAnswer = B.concat (replicate 100 msid) }
      answers <- traverse (exchange r) [recv 0x01 0x1004 2048, level0]
      map (B.isInfixOf msid . rpayload) answers `shouldBe` [True, True]
      ls <- readIORef (rLines r)
      length ls `shouldBe` 2
      filter (\l -> any (`T.isInfixOf` T.toLower l) ["msid", "4d 53 49 44", "4d534944"]) ls `shouldBe` []
    it "waits, uninterruptible, for a slow shim: a timeout shorter returns after the shim, and the answer lost latches it" $ do
      -- the shim half a second, the timeout a fiftieth: a loaded runner's timer has the rest to fire in
      r <- newRig
      writeIORef (rKnobs r) straight { kDelay = 500000 }
      started <- Clock.getMonotonicTime
      outcome <- timeout 20000 (exchange r level0)
      ended <- Clock.getMonotonicTime
      (isNothing outcome, ended - started >= 0.5) `shouldBe` (True, True)
      readIORef (rCalls r) `shouldReturn` 1
      writeIORef (rKnobs r) straight
      exchange r level0 >>= (`shouldSatisfy` failsWith "was lost to an interruption, after the shim returned")
      readIORef (rCalls r) `shouldReturn` 1
    it "answers within a timeout longer than the shim, and stays ready" $ do
      r <- newRig
      writeIORef (rKnobs r) straight { kDelay = 1000 }
      outcome <- timeout 5000000 (exchange r level0)
      fmap status outcome `shouldBe` Just Good
      exchange r level0 >>= (`shouldSatisfy` (== Good) . status)

  describe "kextBind" $ do
    it "binds through the shim first; a base the shim refuses binds nothing, latches, and says why" $ do
      r <- newRig
      kextBind (\_ -> pure 0xe00002c5) (rHandle r) 0x1004 >>= (`shouldSatisfy` either ("design 13.29 (d)" `T.isInfixOf`) (const False))
      readIORef (khBase (rHandle r)) `shouldReturn` Nothing
      exchange r level0 >>= (`shouldSatisfy` failsWith "hsed_kext_bind of 0x1004: kIOReturnExclusiveAccess (0xe00002c5)")
      readIORef (rCalls r) `shouldReturn` 0
    it "binds nothing on a closed or latched transport, its shim not called" $ do
      closed <- newRig
      kextClose (const (pure ())) (pure 0) (rHandle closed)
      latched <- newRig
      writeIORef (rKnobs latched) straight { kKernReturn = 0xe00002bc }
      _ <- exchange latched level0
      bound <- newIORef (0 :: Int)
      answers <- traverse (\g -> kextBind (\_ -> 0 <$ modifyIORef' bound (+ 1)) (rHandle g) 0x1004) [closed, latched]
      answers `shouldSatisfy` all isLeft
      readIORef bound `shouldReturn` 0
    it "binds the same base again" $ do
      r <- newRig
      traverse (bindRig r) [0x1004, 0x1004] `shouldReturn` [Right (), Right ()]
      readIORef (khBase (rHandle r)) `shouldReturn` Just 0x1004

  describe "kextClose" $
    it "closes once, the shim's close called once; a closed transport issues nothing, the shim not called" $ do
      r <- newRig
      closes <- newIORef (0 :: Int)
      replicateM_ 2 (kextClose (const (pure ())) (0 <$ modifyIORef' closes (+ 1)) (rHandle r))
      readIORef closes `shouldReturn` 1
      exchange r level0 >>= (`shouldSatisfy` failsWith "the kext transport is closed")
      readIORef (rCalls r) `shouldReturn` 0

  describe "judgeCommand" $ do
    let received = B.replicate 2048 0x5a
    it "passes an IF-RECV's payload, and none of an IF-SEND's, when it succeeded with hsed's CDB and every byte moved" $ do
      judgeCommand IfSAT Level0Recv 0x0001 4 0 (satCdb Level0Recv 0x0001 4) 2048 received `shouldBe` Right received
      judgeCommand IfSAT ResetSend 0x1004 1 0 (satCdb ResetSend 0x1004 1) 512 "" `shouldBe` Right ""
    it "fails a kern_return other than success, by name, keeping nothing" $
      first snd (judgeCommand IfSAT Level0Recv 0x0001 4 0xe00002bc (satCdb Level0Recv 0x0001 4) 2048 received)
        `shouldBe` Left ""
    it "fails a CDB other than hsed's: another command, another interface's, another count or ComID, a byte past the twelfth" $
      filter (\cdb -> isRight (judgeCommand IfSAT PacketRecv 0x1004 4 0 cdb 2048 received))
        [ satCdb PacketSend 0x1004 4, either (const "") (<> B.replicate 4 0) (cdbFor IfSCSI PacketRecv 0x1004 4)
        , satCdb PacketRecv 0x1004 5, satCdb PacketRecv 0x1005 4, satCdb Level0Recv 0x0001 4
        , B.take 15 (satCdb PacketRecv 0x1004 4) <> B.singleton 1, B.take 12 (satCdb PacketRecv 0x1004 4) ]
        `shouldBe` []
    it "fails another count moved than asked, of an IF-RECV keeping the bytes as evidence, of an IF-SEND none" $ do
      first snd (judgeCommand IfSAT PacketRecv 0x1004 4 0 (satCdb PacketRecv 0x1004 4) 1536 received) `shouldBe` Left received
      first snd (judgeCommand IfSAT PacketSend 0x1004 1 0 (satCdb PacketSend 0x1004 1) 0 "") `shouldBe` Left ""

  describe "commandLine" $
    it "prints the disk, the command, its protocol, ComID and length, the CDB, the kern_return and the bytes moved" $ do
      commandLine "disk22" ResetSend 0x1004 1 (satCdb ResetSend 0x1004 1) 0 512
        `shouldBe` ("kext disk22: STACK_RESET's HANDLE_COMID_REQUEST, protocol 0x02 ComID 0x1004, 512 bytes, "
                    <> "CDB a1 0a 06 02 01 00 04 10 00 5e 00 00, kern_return kIOReturnSuccess (0x00000000), 512 bytes transferred")
      commandLine "disk22" Level0Recv 0x0001 4 (satCdb Level0Recv 0x0001 4) 0 2048
        `shouldBe` ("kext disk22: Level 0 Discovery's IF-RECV, protocol 0x01 ComID 0x0001, 2048 bytes, "
                    <> "CDB a1 08 0e 01 04 00 01 00 00 5c 00 00, kern_return kIOReturnSuccess (0x00000000), 2048 bytes transferred")

  describe "kernReturn" $ do
    it "names IOKit's codes as IOReturn.h does, with their hex" $
      map kernReturn [0, 0xe00002bc, 0xe00002c0, 0xe00002c2, 0xe00002c5, 0xe00002c7, 0xe00002cd, 0xe00002d0, 0xe00002d6, 0xe00002d8, 0xe00002db, 0xe00002f0]
        `shouldBe`
          [ "kIOReturnSuccess (0x00000000)", "kIOReturnError (0xe00002bc)", "kIOReturnNoDevice (0xe00002c0)"
          , "kIOReturnBadArgument (0xe00002c2)", "kIOReturnExclusiveAccess (0xe00002c5)", "kIOReturnUnsupported (0xe00002c7)"
          , "kIOReturnNotOpen (0xe00002cd)", "kIOReturnNotAligned (0xe00002d0)", "kIOReturnTimeout (0xe00002d6)"
          , "kIOReturnNotReady (0xe00002d8)", "kIOReturnNoSpace (0xe00002db)", "kIOReturnNotFound (0xe00002f0)" ]
    it "gives any other code its hex alone" $
      map kernReturn [0xe00002cb, 0xe00002f1, 0x12345678]
        `shouldBe` ["kern_return (0xe00002cb)", "kern_return (0xe00002f1)", "kern_return (0x12345678)"]

  describe "hsed_kext_info" $
    it "is laid out as cbits/hsed_kext.h declares it: 128 bytes, each field after the last, base_comid at 120" $ do
      let l = infoLayout
      (ilBytes l, ilAlignment l) `shouldBe` (128, 8)
      [ilConn l, ilRegistryId l, ilSat l, ilSerial l, ilKextBuild l, ilOtherClients l, ilBaseComId l] `shouldBe` [0, 8, 16, 20, 84, 116, 120]
      (ilSerial l + ilSerialBytes l, ilKextBuild l + ilKextBuildBytes l) `shouldBe` (ilKextBuild l, ilOtherClients l)

  describe "the fake kext (Hsed.Transport.FakeKext)" $ do
    it "round-trips on SAT: Level 0, kextBind 0x1004, Properties sent and its answer received, the CDBs recorded cdbFor's" $ do
      (raw, di, _, cdbs) <- fakeKextDrive defaultSimParams noFaults
      roundTrip raw cdbs (diInterface di)
    it "round-trips on SCSI's CDBs the same" $ do
      (sim, _) <- newInProc defaultSimParams { spUnits = UnitBlocks512 } (InProcOpts UnitBlocks512 0)
      cdbs <- newIORef []
      raw <- fakeKextOn IfSCSI sim noFaults cdbs
      roundTrip raw cdbs IfSCSI
    it "fails the nth command of a shape with a kern_return, the simulator not asked, and latches" $ do
      (raw, _, _, cdbs) <- fakeKextDrive defaultSimParams noFaults { ffKernReturn = Just (ResetSend, 1, 0xe00002bc) }
      let t = rdTransport raw
      rdBind raw 0x1004 `shouldReturn` Right ()
      answers <- traverse (tExchange t) [resetSend, level0]
      answers `shouldSatisfy` all (failsWith "kIOReturnError (0xe00002bc)")
      readIORef cdbs `shouldReturn` [B.take 12 (satCdb ResetSend 0x1004 1)]
    it "says of the nth command of a shape that it moved no bytes, the simulator asked: a residue, and the latch" $ do
      (raw, _, _, cdbs) <- fakeKextDrive defaultSimParams noFaults { ffMoveNone = Just (PacketSend, 1) }
      let t = rdTransport raw
      rdBind raw 0x1004 `shouldReturn` Right ()
      a <- tExchange t propertiesSend
      a `shouldSatisfy` failsWith "the command reached the drive, but not as hsed asked it"
      tExchange t level0 >>= (`shouldSatisfy` failsWith "a command failed before")
      readIORef cdbs `shouldReturn` [B.take 12 (satCdb PacketSend 0x1004 1)]
    it "refuses a command on the base before the bind, and a base the C does not take, as the shim does" $ do
      (raw, _, _, cdbs) <- fakeKextDrive defaultSimParams noFaults
      tExchange (rdTransport raw) resetSend >>= (`shouldSatisfy` refused)
      rdBind raw 0x0004 >>= (`shouldSatisfy` either ("kIOReturnBadArgument" `T.isInfixOf`) (const False))
      readIORef cdbs `shouldReturn` []
    it "closes its user client once; opened again over the same simulator, it carries Level 0 again" $ do
      (sim, _) <- newInProc defaultSimParams { spUnits = UnitBlocks512 } (InProcOpts UnitBlocks512 0)
      cdbs <- newIORef []
      first' <- fakeKextOn IfSAT sim noFaults cdbs
      tClose (rdTransport first')
      tExchange (rdTransport first') level0 >>= (`shouldSatisfy` failsWith "the kext transport is closed")
      again <- fakeKextOn IfSAT sim noFaults cdbs
      tExchange (rdTransport again) level0 >>= (`shouldSatisfy` (== Good) . status)
      readIORef cdbs `shouldReturn` [B.take 12 (satCdb Level0Recv 0x0001 4)]
  where
    roundTrip raw cdbs iface = do
      let t = rdTransport raw
          cdbOf s c n = either (const "") id (cdbFor iface s c n)
      l0 <- tExchange t level0
      (status l0, B.length (rpayload l0)) `shouldBe` (Good, 2048)
      rdBind raw 0x1004 `shouldReturn` Right ()
      chan <- newChannel
      answer <- exchangeComPacket defaultExchangeOpts (\q -> Right <$> tExchange t q) chan t 0x1004
                                  (either (const "") id (controlComPacket 0x1004 propertiesCall))
      fmap (not . null) answer `shouldBe` Right True
      recorded <- readIORef cdbs
      take 3 recorded `shouldBe` [cdbOf Level0Recv 0x0001 4, cdbOf PacketSend 0x1004 1, cdbOf PacketRecv 0x1004 4]
      drop 3 recorded `shouldSatisfy` all (== cdbOf PacketRecv 0x1004 4)
