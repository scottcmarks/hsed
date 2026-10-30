{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.WireSpec
Description : Test specifications for Hsed.Wire: units, one command per ComID, the host's ComPacket exchange
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The host's side of the wire contract (design 6.1-6.2): transfer lengths in a
transport's unit (SIIS 1.20 4.3, PDF 30-31: a count of 512-byte blocks, or
bytes); one exchange in flight per ComID; and the exchange of a ComPacket
against a scripted transport (built with the constructor of
"Hsed.Wire.Internal", which the test suite may see), answering each IF-RECV
with a header of Core Table 45 (PDF 83): it asks again at the larger of the
MinTransfer and its length, with or without packets in that ComPacket
(rows 2 and 3), polls past Response(s) to come (row 1), and gives up after
its timeout, or past the bounds on the whole exchange (its time, the bytes it
collects) against a TPer that never stops answering; an answer of another
extension than the one sent is malformed. And the wire's constants, each
defined once.
-}
module Hsed.WireSpec (spec) where

import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Positive (..), (===), (==>))

import           Hsed.Gen.Core.Layouts (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..))
import           Hsed.Packet           (ComPacketFrame (..), PacketFrame (..), encodeComPacket, encodePacket)
import           Hsed.Wire
import           Hsed.Wire.Internal    (Transport (..))


-- * A scripted transport

-- | A transport whose IF-SENDs are answered Good and whose IF-RECVs are
--   answered from a script, each answer exactly the length asked; every
--   request it is given, in order. Past the script, Response(s) to come.
scripted :: Unit -> [ComPacketHeader -> ByteString] -> IO (Transport 'ReadWrite, IO [Request])
scripted u answers = do
  script <- newIORef answers
  seen   <- newIORef []
  let exchange r = do
        modifyIORef' seen (r :)
        case dir r of
          IfSend -> pure (Response Good mempty)
          IfRecv -> do
            next <- atomicModifyIORef' script (\as -> (drop 1 as, listToMaybe as))
            pure (Response Good (exactly (lenBytes r) (fromMaybe toCome next (ComPacketHeader (comId r) 0 0 0))))
      t = MkTransport exchange (const (pure Good)) u (Describe "tper" "scripted" u Nothing Nothing Nothing Nothing) (pure ())
  pure (t, reverse <$> readIORef seen)
  where
    toCome h = comPacket h { comPacketOutstandingData = 1 } []

-- | Bytes a ComPacket of a header and packets encodes to.
comPacket :: ComPacketHeader -> [PacketFrame] -> ByteString
comPacket h ps = either (const mempty) id (encodeComPacket (ComPacketFrame h ps))

-- | A control session packet of one data subpacket of these bytes.
packet :: ByteString -> PacketFrame
packet = PacketFrame (PacketHeader 0 0 0 0 0) . pure . DataSubPacket (DataSubPacketHeader 0)

-- | Exchange a ComPacket on 0x1004 through the transport's own IF-SEND.
exchanged :: ExchangeOpts -> Transport 'ReadWrite -> IO (Either ExchangeError [PacketFrame])
exchanged opts t = do
  ch <- newChannel
  exchangeComPacket opts (\r -> Right <$> ifSend t (proto r) (comId r) (lenBytes r) (payload r)) ch t 0x1004 sent
  where sent = comPacket (ComPacketHeader 0x1004 0 0 0) [packet "\xf8"]

-- | Fast polls, a generous timeout.
quick :: ExchangeOpts
quick = defaultExchangeOpts { xPollMicros = 1000 }

-- | The lengths of a run's IF-RECVs.
recvLengths :: [Request] -> [Word32]
recvLengths rs = [ lenBytes r | r <- rs, dir r == IfRecv ]


spec :: Spec
spec = do
  describe "units" $ do
    it "counts blocks 1 as 512 bytes, or one byte (SPF-02 case 2's)" $ do
      transferLength UnitBlocks512 1 `shouldBe` 512
      transferLength UnitBytes 1 `shouldBe` 1
      transferLength UnitBlocks512 4 `shouldBe` 2048
      transferLength UnitBytes (-1) `shouldBe` 0
    it "saturates a count past what 32 bits hold at the last whole unit, never wrapping" $ do
      map (transferLength UnitBlocks512) [8388607, 8388608, 8388609, maxBound] `shouldBe` replicate 4 0xFFFFFE00
      transferLength UnitBytes 4294967297 `shouldBe` maxBound
    prop "is n units' bytes, or the most a length holds" $ \n ->
      toInteger (transferLength UnitBlocks512 n) === min 0xFFFFFE00 (toInteger (max 0 n) * 512)
    it "rounds a length up to whole blocks, and leaves bytes as they are" $ do
      map (roundUp UnitBlocks512) [0, 1, 511, 512, 513, 2048, 2049] `shouldBe` [0, 512, 512, 512, 1024, 2048, 2560]
      map (roundUp UnitBytes) [0, 1, 513] `shouldBe` [0, 1, 513]
      roundUp UnitBlocks512 maxBound `shouldBe` 0xFFFFFE00
    prop "rounds up to the least whole number of blocks at or above the length" $ \len ->
      let r = roundUp UnitBlocks512 len
      in len <= 0xFFFFFE00 ==> (r >= len, r `mod` 512, r - len < 512) === (True, 0, True)
    it "counts the units a length asks for, rounded up" $ do
      unitsFor UnitBlocks512 (255 * 512) 1 `shouldBe` Right 1
      unitsFor UnitBlocks512 (255 * 512) 512 `shouldBe` Right 1
      unitsFor UnitBlocks512 (255 * 512) 513 `shouldBe` Right 2
      unitsFor UnitBlocks512 (255 * 512) (255 * 512) `shouldBe` Right 255
      unitsFor UnitBytes 2048 2048 `shouldBe` Right 2048
    it "refuses no bytes, and more than the transport carries, with Other Invalid Command Parameter (SIIS 1.20 4.3.1.2, 4.3.2.2)" $ do
      unitsFor UnitBlocks512 (255 * 512) 0 `shouldBe` Left OtherInvalidCommandParameter
      unitsFor UnitBytes 2048 0 `shouldBe` Left OtherInvalidCommandParameter
      unitsFor UnitBlocks512 (255 * 512) (255 * 512 + 1) `shouldBe` Left OtherInvalidCommandParameter
      unitsFor UnitBytes 2048 2049 `shouldBe` Left OtherInvalidCommandParameter
      unitsFor UnitBlocks512 1000 600 `shouldBe` Left OtherInvalidCommandParameter
    prop "counts what a transfer of n units carries as n units" $ \(Positive n) ->
      n <= 255 ==> unitsFor UnitBlocks512 (255 * 512) (transferLength UnitBlocks512 n) === Right (fromIntegral n)

  describe "one command in flight per ComID" $ do
    it "holds a second exchange on a ComID until the first ends, and not one on another ComID" $ do
      ch       <- newChannel
      order    <- newIORef ([] :: [Text])
      started  <- newEmptyMVar
      release  <- newEmptyMVar
      first'   <- async (withComId ch 0x1004 (putMVar started () *> takeMVar release *> modifyIORef' order (<> ["first"])))
      takeMVar started
      second'  <- async (withComId ch 0x1004 (modifyIORef' order (<> ["second"])))
      withComId ch 0x1005 (modifyIORef' order (<> ["other ComID"]))
      threadDelay 20000
      readIORef order `shouldReturn` ["other ComID"]
      putMVar release ()
      wait first' *> wait second'
      readIORef order `shouldReturn` ["other ComID", "first", "second"]
    it "releases a ComID whose exchange throws" $ do
      ch <- newChannel
      withComId ch 0x1004 (throwString "the transport failed" :: IO ()) `shouldThrow` anyException
      timeout 1000000 (withComId ch 0x1004 (pure ())) `shouldReturn` Just ()

  describe "the host's exchange of a ComPacket" $ do
    it "sends the ComPacket on its ComID, rounded up to the unit and zero-padded, then asks at the starting length" $ do
      (t, seen) <- scripted UnitBlocks512 [\h -> comPacket h [packet "x"]]
      exchanged quick t `shouldReturn` Right [packet "x"]
      rs <- seen
      map (\r -> (dir r, proto r, comId r, lenBytes r)) rs `shouldBe` [(IfSend, 1, 0x1004, 512), (IfRecv, 1, 0x1004, 2048)]
      [ B.length (payload r) | r <- rs, dir r == IfSend ] `shouldBe` [512]
    it "is done at once when nothing is to come, (0, 0, 0): no packets" $ do
      (t, _) <- scripted UnitBytes [\h -> comPacket h []]
      exchanged quick t `shouldReturn` Right []
    it "asks again at the MinTransfer when the length was too short, no packets in the answer (row 2)" $ do
      (t, seen) <- scripted UnitBytes [ \h -> comPacket h { comPacketOutstandingData = 3100, comPacketMinTransfer = 3000 } []
                                      , \h -> comPacket h [packet "big"] ]
      exchanged quick t `shouldReturn` Right [packet "big"]
      recvLengths <$> seen `shouldReturn` [2048, 3000]
    it "asks again at the MinTransfer, rounded up to blocks" $ do
      (t, seen) <- scripted UnitBlocks512 [ \h -> comPacket h { comPacketOutstandingData = 3100, comPacketMinTransfer = 3000 } []
                                          , \h -> comPacket h [packet "big"] ]
      exchanged quick t `shouldReturn` Right [packet "big"]
      recvLengths <$> seen `shouldReturn` [2048, 3072]
    it "collects the packets of an answer that has more available, and asks again at the larger length (row 3)" $ do
      (t, seen) <- scripted UnitBytes [ \h -> comPacket h { comPacketOutstandingData = 2600, comPacketMinTransfer = 2500 } [packet "one"]
                                      , \h -> comPacket h { comPacketOutstandingData = 60, comPacketMinTransfer = 50 } [packet "two"]
                                      , \h -> comPacket h [packet "three"] ]
      exchanged quick t `shouldReturn` Right [packet "one", packet "two", packet "three"]
      recvLengths <$> seen `shouldReturn` [2048, 2500, 2500]
    it "polls past Response(s) to come, (0, 1, 0), and collects what comes after more to come (row 4)" $ do
      (t, seen) <- scripted UnitBytes [ \h -> comPacket h { comPacketOutstandingData = 1 } []
                                      , \h -> comPacket h { comPacketOutstandingData = 1 } []
                                      , \h -> comPacket h { comPacketOutstandingData = 1 } [packet "one"]
                                      , \h -> comPacket h { comPacketOutstandingData = 1 } []
                                      , \h -> comPacket h [packet "two"] ]
      exchanged quick t `shouldReturn` Right [packet "one", packet "two"]
      recvLengths <$> seen `shouldReturn` replicate 5 2048
    it "gives up on a TPer that keeps answering Response(s) to come, after its timeout" $ do
      (t, _) <- scripted UnitBytes []
      exchanged quick { xTimeoutMicros = 50000 } t `shouldReturn` Left TimedOut
    it "times out a command not answered within its timeout (50 ms)" $ do
      (t, _) <- scripted UnitBytes []
      let slow = t { tExchange = \r -> case dir r of
                       IfSend -> pure (Response Good mempty)
                       IfRecv -> threadDelay 2000000 *> tExchange t r }
      timeout 1000000 (exchanged quick { xTimeoutMicros = 50000 } slow) `shouldReturn` Just (Left TimedOut)
    it "stops at a status other than Good" $ do
      (t, _) <- scripted UnitBytes []
      let failing = t { tExchange = \r -> case dir r of
                          IfSend -> pure (Response SynchronousProtocolViolation mempty)
                          IfRecv -> tExchange t r }
      exchanged quick failing `shouldReturn` Left (IfError SynchronousProtocolViolation)
    it "passes on a refusal of the IF-SEND path, issuing nothing" $ do
      (t, seen) <- scripted UnitBytes []
      ch <- newChannel
      exchangeComPacket quick (const (pure (Left "the Guard refuses it"))) ch t 0x1004 "\x00" `shouldReturn` Left (Refused "the Guard refuses it")
      seen `shouldReturn` []
    it "refuses a MinTransfer more than the transport carries" $ do
      (t, _) <- scripted UnitBlocks512 [\h -> comPacket h { comPacketOutstandingData = 0x200000, comPacketMinTransfer = 0x200000 } []]
      exchanged quick t >>= (`shouldSatisfy` isRefused)
    it "calls an answer no ComPacket of the ComID, or of an inactive one, malformed" $ do
      (other, _) <- scripted UnitBytes [\_ -> comPacket (ComPacketHeader 0x1005 0 0 0) []]
      exchanged quick other >>= (`shouldSatisfy` isMalformed)
      (inactive, _) <- scripted UnitBytes [\h -> comPacket h { comPacketComIDExtension = 0xFFFF } []]
      exchanged quick inactive >>= (`shouldSatisfy` isMalformed)
      (row2, _) <- scripted UnitBytes [\h -> comPacket h { comPacketOutstandingData = 100, comPacketMinTransfer = 100 } []]
      exchanged quick row2 >>= (`shouldSatisfy` isMalformed)
    it "calls an answer of another extension than the ComPacket sent malformed: the ComID is in use elsewhere (Core 3.3.3.1)" $ do
      (other, _) <- scripted UnitBytes [\h -> comPacket h { comPacketComIDExtension = 0x1234 } []]
      exchanged quick other >>= (`shouldSatisfy` isMalformed)
    it "gives up on a TPer that keeps returning a packet with more to come (row 4), past the exchange's time" $ do
      (t, _) <- scripted UnitBytes (L.repeat (\h -> comPacket h { comPacketOutstandingData = 1 } [packet "x"]))
      timeout 5000000 (exchanged quick { xTotalMicros = 200000 } t) `shouldReturn` Just (Left TimedOut)
    it "gives up on a TPer that keeps returning packets at once (row 3), past the bytes it collects" $ do
      (t, seen) <- scripted UnitBytes (L.repeat (\h -> comPacket h { comPacketOutstandingData = 100, comPacketMinTransfer = 80 } [packet "x"]))
      r <- timeout 5000000 (exchanged quick { xResponseBytes = 4096 } t)
      r `shouldSatisfy` \case Just (Left (Malformed _)) -> True; _ -> False
      -- every answer one packet more, until they hold more than 4096 bytes
      let each = either (const 0) B.length (encodePacket (packet "x"))
      length . recvLengths <$> seen `shouldReturn` (4096 `div` each + 1)

  describe "the wire's constants" $ do
    it "are Core's: protocol 0x01, a 20-byte ComPacket header, extensions 0x0000 static and 0xFFFF inactive" $
      (comPacketProtocol, comPacketHeaderSize, staticExtension, inactiveExtension) `shouldBe` (0x01, 20, 0x0000, 0xFFFF)
    it "carry at most 255 blocks, or 1 MiB" $
      (mostCarried UnitBlocks512, mostCarried UnitBytes) `shouldBe` (130560, 1048576)
    it "cut or pad a transfer to exactly its length" $
      map (`exactly` "ab") [0, 1, 2, 4] `shouldBe` ["", "a", "ab", "ab\0\0"]
    it "print a number in hex, as many digits as its type has" $
      (hex (0x1004 :: Word16), hex (1 :: Word8), hex (0x5c :: Word32)) `shouldBe` ("0x1004", "0x01", "0x0000005c")
  where
    isRefused   = \case Left (Refused _) -> True; _ -> False
    isMalformed = \case Left (Malformed _) -> True; _ -> False
