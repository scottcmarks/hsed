{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.InProcSpec
Description : Test specifications for Hsed.Transport.InProc: the simulated TPer as a transport, in both units, holding its responses
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The in-process transport (design 7.1) over the simulator, in bytes and in
512-byte blocks: Level 0 answered with exactly the bytes asked; IF-RECV in
Awaiting IF-SEND answered with Core Table 45's last row (0, 0, 0); a second
IF-SEND while a response is held a Synchronous Protocol Violation (3.3.10.2.1
item 3 a, PDF 82); a response held for N IF-RECVs answered (0, 1, 0) N times
(Table 45 row 1, PDF 83); an IF-SEND of Empty atoms or on a session the TPer
does not have discarded, the ComID still awaiting IF-SEND (3.3.10.6 S2:S1 d,
3.3.10.7 a, PDF 85-86); the lengths, protocols and ComIDs the TPer refuses
(SIIS 1.20 4.3, PDF 30-31; Core 3.3.3.2-3, PDF 58-59; 3.3.10.7 c, PDF 86).

The examples that need the Session Manager to answer Properties pend until
it does ("Hsed.Sim.SessionManager".@answer@, group G2's): until then a
Properties IF-SEND is accepted and discarded.
-}
module Hsed.Transport.InProcSpec (spec) where

import qualified RIO.ByteString          as B
import qualified RIO.List                as L
import           RIO
import           Test.Hspec

import           Hsed.Core.Layout        (Content (..), Decoded (..), Layout (..), decode)
import           Hsed.Core.Method        (invocation)
import           Hsed.Core.Record        (DecodedRecord (..), LayoutRecord (..), decodeRecordStrict)
import           Hsed.Core.StreamItem    (StreamItem (generate))
import           Hsed.Gen.Core.Layouts   (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), Level0DiscoveryHeader (..),
                                          PacketHeader (..), comPacketLayout)
import qualified Hsed.Gen.Core.Methods   as Methods
import           Hsed.Gen.Core.UIDs      (uSMUID)
import           Hsed.Packet             (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, encodeComPacket)
import           Hsed.Sim                (syncOf)
import           Hsed.Sim.SessionManager (answer)
import           Hsed.Sim.Types          (SimParams (..), SyncState (..), defaultSimParams)
import           Hsed.Transport.InProc
import           Hsed.Version            (hsedVersion)
import           Hsed.Wire


-- * Helpers

-- | The static ComID of the default parameters.
base :: Word16
base = spBaseComId defaultSimParams

-- | A ComPacket on the base ComID of these packets.
comPacketOf :: [PacketFrame] -> ByteString
comPacketOf ps = either (const mempty) id (encodeComPacket (ComPacketFrame (ComPacketHeader base 0 0 0) ps))

-- | A packet of a session (its TSN and HSN) of one data subpacket.
packetOf :: (Word32, Word32) -> ByteString -> PacketFrame
packetOf (tsn, hsn) = PacketFrame (PacketHeader tsn hsn 0 0 0) . pure . DataSubPacket (DataSubPacketHeader 0)

-- | The host's Properties invocation, on the control session.
properties :: ByteString
properties = comPacketOf [packetOf (0, 0) (generate (invocation uSMUID (Methods.Properties Nothing)))]

-- | IF-SEND of a ComPacket on the base ComID, its length rounded up to the unit.
send :: Transport 'ReadWrite -> ByteString -> IO Response
send t bytes = ifSend t 0x01 base (roundUp (unitOf t) (fromIntegral (B.length bytes))) bytes

-- | IF-RECV on the base ComID of 2048 bytes, the TPer's MaxComPacketSize.
recv :: Transport c -> IO Response
recv t = ifRecv t 0x01 base 2048

-- | The ComPacket header and packets an IF-RECV answered with, after
--   checking it is Good and exactly the length asked.
answered :: Word32 -> Response -> IO (ComPacketHeader, [PacketFrame])
answered len r = do
  status r `shouldBe` Good
  B.length (rpayload r) `shouldBe` fromIntegral len
  case decodeComPacket (rpayload r) of
    Left e -> expectationFailure (show e) *> throwString "no ComPacket"
    Right (DecodedFrame (ComPacketFrame h ps) _ rest) -> do
      rest `shouldSatisfy` B.all (== 0)
      pure (h, ps)

-- | Table 45's fields of the answer to an IF-RECV of 2048 bytes: its Length,
--   OutstandingData and MinTransfer, read with Core Table 17's layout.
t45 :: Response -> IO (Integer, Integer, Integer)
t45 r = do
  _ <- answered 2048 r
  case decode comPacketLayout (rpayload r) of
    Left e  -> expectationFailure (show e) *> throwString "no ComPacket"
    Right d -> pure (field d "Length", field d "OutstandingData", field d "MinTransfer")
  where
    field d name = case L.lookup name (decodedFields d) of
      Just (Number v) -> v
      _               -> -1

-- | Table 45's fifth row: a response, all of it returned, nothing further.
allReturnedWithData :: (Integer, Integer, Integer) -> Bool
allReturnedWithData (l, od, mt) = l > 0 && od == 0 && mt == 0

-- | An example that needs the Session Manager to answer Properties: pending
--   while it does not (G2's answer, a working stub that answers nothing).
needsProperties :: Expectation -> Expectation
needsProperties e = case answer defaultSimParams (invocation uSMUID (Methods.Properties Nothing)) of
  Nothing -> pendingWith "needs Hsed.Sim.SessionManager.answer (group G2) to answer Properties: until it does, a Properties IF-SEND is discarded"
  Just _  -> e

-- | A simulator in a unit, holding responses for so many IF-RECVs.
simulator :: Unit -> Int -> IO (Transport 'ReadWrite, SimControl)
simulator u hold = newInProc defaultSimParams (InProcOpts u hold)

isS2, isS3 :: Maybe SyncState -> Bool
isS2 = \s -> case s of Just (S2 _) -> True; _ -> False
isS3 = \s -> case s of Just (S3 _) -> True; _ -> False


spec :: Spec
spec = do
  describe "the transport" $
    it "says it is the simulator of this build, of Opal 2.30, with the parameters' ComIDs, in the unit asked" $
      forM_ [UnitBytes, UnitBlocks512] $ \u -> do
        (t, _) <- simulator u 0
        unitOf t `shouldBe` u
        describeTransport t `shouldBe`
          Describe "tper" ("hsed-sim " <> hsedVersion) u (Just "opal-2.30") (Just 0x1004) (Just 1) Nothing

  describe "Level 0 Discovery" $ do
    it "answers IF-RECV with exactly the bytes asked, its header first, in bytes (100 truncated, 4096 padded)" $ do
      (t, _) <- simulator UnitBytes 0
      forM_ [100, 4096] (level0Answer t)
    it "answers IF-RECV with exactly the bytes asked in blocks (512 and 4096), and refuses 100 bytes" $ do
      (t, _) <- simulator UnitBlocks512 0
      forM_ [512, 4096] (level0Answer t)
      ifRecv t 0x01 0x0001 100 `shouldReturn` Response OtherInvalidCommandParameter mempty
    it "accepts IF-SEND and discards it, outside the synchronous protocol (Core 3.3.6.1, 3.3.10.3 b)" $ do
      (t, ctl) <- simulator UnitBytes 0
      st <- scPeek ctl
      ifSend t 0x01 0x0001 512 "anything" `shouldReturn` Response Good mempty
      scPeek ctl `shouldReturn` st

  describe "IF-RECV awaiting IF-SEND (S1)" $
    it "answers a 20-byte ComPacket header of the ComID, extension 0, Length, OutstandingData and MinTransfer 0, then zeros" $
      forM_ [(UnitBytes, 2048), (UnitBlocks512, 512)] $ \(u, len) -> do
        (t, _) <- simulator u 0
        r <- ifRecv t 0x01 base len
        answered len r `shouldReturn` (ComPacketHeader base 0 0 0, [])
        B.take 20 (rpayload r) `shouldBe` (B.replicate 4 0 <> "\x10\x04" <> B.replicate 14 0)
        B.drop 20 (rpayload r) `shouldSatisfy` B.all (== 0)

  describe "Properties" $ do
    it "is accepted, Good" $
      forM_ [UnitBytes, UnitBlocks512] $ \u -> do
        (t, _) <- simulator u 0
        send t properties `shouldReturn` Response Good mempty
    it "then a second IF-SEND is a Synchronous Protocol Violation, the TPer unchanged: S3 without a hold, S2 with hold 1" $
      needsProperties $ forM_ [UnitBytes, UnitBlocks512] $ \u -> forM_ [(0, isS3), (1, isS2)] $ \(hold, inState) -> do
        (t, ctl) <- simulator u hold
        send t properties `shouldReturn` Response Good mempty
        st <- scPeek ctl
        syncOf base st `shouldSatisfy` inState
        send t properties `shouldReturn` Response SynchronousProtocolViolation mempty
        scPeek ctl `shouldReturn` st
    it "is answered at once without a hold: the response, then (0, 0, 0)" $
      needsProperties $ do
        (t, _) <- simulator UnitBytes 0
        _ <- send t properties
        recv t >>= t45 >>= (`shouldSatisfy` allReturnedWithData)
        recv t >>= t45 >>= (`shouldBe` (0, 0, 0))
    it "held for N IF-RECVs (N = 1, 2, 3) is answered (0, 1, 0) N times, then the response, then (0, 0, 0)" $
      needsProperties $ forM_ [1, 2, 3] $ \n -> do
        (t, _) <- simulator UnitBytes n
        _ <- send t properties
        replicateM_ n (recv t >>= t45 >>= (`shouldBe` (0, 1, 0)))
        recv t >>= t45 >>= (`shouldSatisfy` allReturnedWithData)
        recv t >>= t45 >>= (`shouldBe` (0, 0, 0))
    it "held, is released by the control before the hold is over" $
      needsProperties $ do
        (t, ctl) <- simulator UnitBytes 3
        _ <- send t properties
        recv t >>= t45 >>= (`shouldBe` (0, 1, 0))
        scRelease ctl base
        syncOf base <$> scPeek ctl >>= (`shouldSatisfy` isS3)
        recv t >>= t45 >>= (`shouldSatisfy` allReturnedWithData)
    it "held, is dropped by an event, every ComID awaiting IF-SEND" $
      needsProperties $ do
        (t, ctl) <- simulator UnitBytes 1
        _ <- send t properties
        inject t PowerCycle `shouldReturn` Good
        syncOf base <$> scPeek ctl `shouldReturn` Just S1
        recv t >>= t45 >>= (`shouldBe` (0, 0, 0))
    it "is exchanged by the host's loop, which polls past (0, 1, 0)" $
      needsProperties $ forM_ [(UnitBytes, 0), (UnitBytes, 2), (UnitBlocks512, 1)] $ \(u, hold) -> do
        (t, _) <- simulator u hold
        ch <- newChannel
        r <- exchangeComPacket defaultExchangeOpts { xPollMicros = 1000 } (\q -> Right <$> ifSend t (proto q) (comId q) (lenBytes q) (payload q))
               ch t base properties
        either (const 0) length r `shouldBe` 1

  describe "an IF-SEND that asks nothing (Core 3.3.10.6 S2:S1 d)" $
    it "of one data subpacket of four Empty atoms is Good, the ComID kept awaiting IF-SEND, and the next IF-RECV is (0, 0, 0), held or not" $
      forM_ [UnitBytes, UnitBlocks512] $ \u -> forM_ [0, 1] $ \hold -> do
        (t, ctl) <- simulator u hold
        send t (comPacketOf [packetOf (0, 0) "\xff\xff\xff\xff"]) `shouldReturn` Response Good mempty
        syncOf base <$> scPeek ctl `shouldReturn` Just S1
        recv t >>= t45 >>= (`shouldBe` (0, 0, 0))

  describe "an IF-SEND on a session the TPer does not have (Core 3.3.10.7 a)" $
    it "is ignored: Good, the ComID awaiting IF-SEND, the next IF-RECV (0, 0, 0)" $
      forM_ [UnitBytes, UnitBlocks512] $ \u -> do
        (t, ctl) <- simulator u 0
        send t (comPacketOf [packetOf (1, 0x69) (generate (invocation uSMUID (Methods.Properties Nothing)))]) `shouldReturn` Response Good mempty
        syncOf base <$> scPeek ctl `shouldReturn` Just S1
        recv t >>= t45 >>= (`shouldBe` (0, 0, 0))

  describe "what the TPer refuses" $ do
    it "an IF-SEND longer than its MaxComPacketSize, 2048: Invalid Transfer Length on IF-SEND (Core 3.3.10.7 c)" $ do
      (bytes, _) <- simulator UnitBytes 0
      ifSend bytes 0x01 base 2049 properties `shouldReturn` Response InvalidTransferLengthOnIfSend mempty
      (blocks, ctl) <- simulator UnitBlocks512 0
      ifSend blocks 0x01 base 2560 properties `shouldReturn` Response InvalidTransferLengthOnIfSend mempty
      syncOf base <$> scPeek ctl `shouldReturn` Just S1
    it "in blocks, a length of part of a block or of none: Other Invalid Command Parameter (SIIS 1.20 4.3)" $ do
      (t, _) <- simulator UnitBlocks512 0
      ifSend t 0x01 base 100 properties `shouldReturn` Response OtherInvalidCommandParameter mempty
      ifRecv t 0x01 base 100 `shouldReturn` Response OtherInvalidCommandParameter mempty
      ifRecv t 0x01 base 0 `shouldReturn` Response OtherInvalidCommandParameter mempty
    it "protocol 3: Invalid Security Protocol ID" $ do
      (t, _) <- simulator UnitBytes 0
      ifRecv t 0x03 base 2048 `shouldReturn` Response InvalidSecurityProtocolId mempty
      ifSend t 0x03 base 2048 properties `shouldReturn` Response InvalidSecurityProtocolId mempty
    it "a reserved ComID it does not support, 0x0002: Other Invalid Command Parameter both ways (Core 3.3.3.2-3)" $ do
      (t, _) <- simulator UnitBytes 0
      ifRecv t 0x01 0x0002 2048 `shouldReturn` Response OtherInvalidCommandParameter mempty
      ifSend t 0x01 0x0002 2048 properties `shouldReturn` Response OtherInvalidCommandParameter mempty
    it "an inactive ComID, 0x2000: Other Invalid Command Parameter both ways, or discarding, Good and a header of extension 0xFFFF" $ do
      (t, _) <- simulator UnitBytes 0
      ifRecv t 0x01 0x2000 2048 `shouldReturn` Response OtherInvalidCommandParameter mempty
      ifSend t 0x01 0x2000 2048 properties `shouldReturn` Response OtherInvalidCommandParameter mempty
      (d, _) <- newInProc defaultSimParams { spInactiveComIdDiscard = True } (InProcOpts UnitBytes 0)
      ifSend d 0x01 0x2000 2048 properties `shouldReturn` Response Good mempty
      r <- ifRecv d 0x01 0x2000 2048
      status r `shouldBe` Good
      B.length (rpayload r) `shouldBe` 2048
      dfFrame <$> decodeComPacket (rpayload r) `shouldBe` Right (ComPacketFrame (ComPacketHeader 0x2000 0xFFFF 0 0) [])
  where
    -- exactly the bytes asked; Core Table 39's header first, its pins as printed; zeros after the Length of
    -- Parameter Data and the field itself (Core 3.3.6.2, PDF 67-68)
    level0Answer t len = do
      r <- ifRecv t 0x01 0x0001 len
      status r `shouldBe` Good
      B.length (rpayload r) `shouldBe` fromIntegral len
      let headerSize = layoutSize (recordLayout (Proxy :: Proxy Level0DiscoveryHeader))
      case decodeRecordStrict (B.take headerSize (rpayload r)) of
        Left e -> expectationFailure ("no Level 0 header: " <> show e)
        Right (DecodedRecord h _ _) -> do
          let lopd = fromIntegral (level0DiscoveryHeaderLengthOfParameterData h)
          lopd `shouldSatisfy` (>= headerSize - 4)
          B.drop (lopd + 4) (rpayload r) `shouldSatisfy` B.all (== 0)
