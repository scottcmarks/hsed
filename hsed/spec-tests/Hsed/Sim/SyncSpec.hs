{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.SyncSpec
Description : Test specifications for Hsed.Sim.Sync: Table 45, the outcomes of an IF-SEND, the answers to IF-RECV
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The synchronous protocol of a static ComID (Core 3.3.10, PDF 82-87): the
generated Table 45 (PDF 83), its six rows by position and its five
quantities in words; what an IF-SEND accepted in S1 comes to, with a Session
Manager of the test's own that answers Properties (the simulator's own,
group G2's, answers it from M3's integration on); and the answer to IF-RECV
in each state, exactly the length asked.
-}
module Hsed.Sim.SyncSpec (spec) where

import qualified RIO.ByteString            as B
import qualified RIO.List                  as L
import           RIO
import           Test.Hspec

import           Hsed.Core.Call            (MethodCall (..), MethodHeader (..), statusList)
import           Hsed.Core.Method          (invocation)
import           Hsed.Core.StreamItem      (StreamItem (generate))
import           Hsed.Core.Sync            (Setting (..), StateRow (..), StateTable (..))
import           Hsed.Core.Token           (Token (..))
import           Hsed.Gen.Core.Layouts     (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..))
import qualified Hsed.Gen.Core.Methods     as Methods
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Gen.Core.UIDs        (uSMUID, uStartSession)
import           Hsed.Packet               (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, decodePacket,
                                            encodeComPacket, encodePacket)
import           Hsed.Sim.Sync
import           Hsed.Sim.Types            (ComIdLife (..), ComIdState (..), Outcome (..), SyncState (..))
import           Hsed.Wire                 (IfStatus (..), Response (..))


-- * Frames

base :: Word16
base = 0x1004

-- | A ComPacket on a ComID and extension of these packets.
comPacketOn :: Word16 -> Word16 -> [PacketFrame] -> ByteString
comPacketOn c ext ps = either (const mempty) id (encodeComPacket (ComPacketFrame (ComPacketHeader c ext 0 0) ps))

-- | A ComPacket on the base ComID.
comPacketOf :: [PacketFrame] -> ByteString
comPacketOf = comPacketOn base 0

-- | A packet of a session of data subpackets of these payloads.
packetIn :: (Word32, Word32) -> [ByteString] -> PacketFrame
packetIn (tsn, hsn) = PacketFrame (PacketHeader tsn hsn 0 0 0) . map (DataSubPacket (DataSubPacketHeader 0))

-- | A control session packet of data subpackets of these payloads.
control :: [ByteString] -> PacketFrame
control = packetIn (0, 0)

-- | The host's Properties invocation.
propertiesCall :: MethodCall
propertiesCall = invocation uSMUID (Methods.Properties Nothing)

-- | The test's Session Manager: it answers Properties with Properties, and
--   nothing else.
respond :: MethodCall -> Maybe MethodCall
respond mc
  | mc == propertiesCall = Just reply
  | otherwise            = Nothing

-- | Its answer, told from the call by its status.
reply :: MethodCall
reply = propertiesCall { _statusCode = 0x3F }

-- | The response packet the answers make: one control session packet, one
--   data subpacket of their tokens.
responseOf :: [MethodCall] -> Either Text [ByteString]
responseOf mcs = either (Left . tshow) (Right . pure) (encodePacket (control [foldMap generate mcs]))

-- | A packet of so many data bytes, encoded: 24 + 12 + n bytes, n a multiple of 4.
packetOfSize :: Int -> ByteString
packetOfSize n = either (const mempty) id (encodePacket (control [B.replicate n 7]))

discarded :: Outcome -> Bool
discarded (Discarded _) = True
discarded _             = False

-- | The ComPacket an IF-RECV answer holds: its header and packets.
frameOf :: Response -> Maybe (ComPacketHeader, [PacketFrame])
frameOf r = either (const Nothing) (\(DecodedFrame (ComPacketFrame h ps) _ _) -> Just (h, ps)) (decodeComPacket (rpayload r))


spec :: Spec
spec = do
  describe "Core Table 45" $ do
    it "has six rows, matched by position to T45Row's, their states as printed (two with an en dash)" $ do
      length (states table45) `shouldBe` 6
      t45Rows `shouldBe` [ResponsesToCome, InsufficientLength, AdditionalAvailable, AdditionalToCome, AllReturnedWithData, AllReturnedNoData]
      map state (states table45) `shouldBe`
        [ "Response(s) to come, no Response(s) available"
        , "Response ready, insufficient transfer length request"
        , "Response, additional Response(s) available"
        , "Response, additional Response(s) to come, no Response(s) available"
        , "Response, all Response(s) returned \8211 no further data"
        , "All Response(s) returned \8211 no further data"
        ]
      fields table45 `shouldBe` ["Length", "OutstandingData", "MinTransfer"]
    it "names five quantities in words, each computed; words it does not use are not" $ do
      let quantities = L.nub [ q | r <- states table45, Quantity q <- settings r ]
      length quantities `shouldBe` 5
      filter (isNothing . t45Quantity) quantities `shouldBe` []
      isNothing (t45Quantity "Total bytes, more or less") `shouldBe` True
    it "evaluates each row over what the ComID has staged and what the answer returns" $ do
      let p1 = B.replicate 30 1
          p2 = B.replicate 40 2
      t45Fields ResponsesToCome (Delivery [] []) `shouldBe` Right (0, 1, 0)
      t45Fields InsufficientLength (Delivery [p1, p2] []) `shouldBe` Right (0, 20 + 70, 20 + 30)
      t45Fields AdditionalAvailable (Delivery [p1, p2] [p1]) `shouldBe` Right (30, 20 + 40, 20 + 40)
      t45Fields AdditionalToCome (Delivery [p1] [p1]) `shouldBe` Right (30, 1, 0)
      t45Fields AllReturnedWithData (Delivery [p1, p2] [p1, p2]) `shouldBe` Right (70, 0, 0)
      t45Fields AllReturnedNoData (Delivery [] []) `shouldBe` Right (0, 0, 0)

  describe "what an IF-SEND accepted in S1 comes to" $ do
    it "is a response packet on the control session of the Session Manager's answer" $
      classifyIfSend respond base (comPacketOf [control [generate propertiesCall]]) `shouldBe` either Discarded Prepared (responseOf [reply])
    it "frames the answer as Hsed.Packet decodes it: Session 0, one data subpacket of its tokens" $
      case classifyIfSend respond base (comPacketOf [control [generate propertiesCall]]) of
        Prepared [p] -> dfFrame <$> decodePacket p `shouldBe` Right (control [generate reply])
        other        -> expectationFailure (show other)
    it "drops Empty atoms wherever they are among the tokens (Core 3.2.2.3.1.5)" $ do
      let pieces = [ generate Call, generate (_header propertiesCall), generate (_parameters propertiesCall), generate EndOfData
                   , generate (statusList (_statusCode propertiesCall)) ]
          spaced = "\xff" <> mconcat (L.intersperse "\xff\xff" pieces) <> "\xff"
      classifyIfSend respond base (comPacketOf [control [spaced]]) `shouldBe` either Discarded Prepared (responseOf [reply])
    it "answers each call the Session Manager answers, in order, and ignores the rest (Core 3.2.4.2)" $ do
      let unanswered = propertiesCall { _header = (_header propertiesCall) { _methodID = uStartSession } }
      classifyIfSend respond base (comPacketOf [control [generate propertiesCall <> generate propertiesCall]])
        `shouldBe` either Discarded Prepared (responseOf [reply, reply])
      classifyIfSend respond base (comPacketOf [control [generate unanswered], control [generate propertiesCall, "\xf8\x00"]])
        `shouldBe` either Discarded Prepared (responseOf [reply])
    it "discards a call the Session Manager does not answer, and tokens that are no call" $ do
      classifyIfSend (const Nothing) base (comPacketOf [control [generate propertiesCall]]) `shouldSatisfy` discarded
      classifyIfSend respond base (comPacketOf [control ["\xf0\xf1"]]) `shouldSatisfy` discarded
      classifyIfSend respond base (comPacketOf [control ["\x80"]]) `shouldSatisfy` discarded
      classifyIfSend respond base (comPacketOf [control [B.take 10 (generate propertiesCall)]]) `shouldSatisfy` discarded
    it "discards a ComPacket that asks nothing: no packets, packets without payload, subpackets empty or of Empty atoms (3.3.10.6 S2:S1 d)" $ do
      classifyIfSend respond base (comPacketOf []) `shouldSatisfy` discarded
      classifyIfSend respond base (comPacketOf [control [], control []]) `shouldSatisfy` discarded
      classifyIfSend respond base (comPacketOf [control ["", "\xff\xff\xff\xff"], control ["\xff"]]) `shouldSatisfy` discarded
    it "discards a ComPacket whose packet names a session the TPer does not have (3.3.10.7 a)" $
      forM_ [(1, 0), (0, 1), (0x1000, 0x69)] $ \session ->
        classifyIfSend respond base (comPacketOf [packetIn session [generate propertiesCall]]) `shouldSatisfy` discarded
    it "discards what is no ComPacket of the ComID, or names an extension other than a static ComID's" $ do
      classifyIfSend respond base "\x00\x01" `shouldSatisfy` discarded
      classifyIfSend respond base (comPacketOn 0x1005 0 [control [generate propertiesCall]]) `shouldSatisfy` discarded
      classifyIfSend respond base (comPacketOn base 1 [control [generate propertiesCall]]) `shouldSatisfy` discarded
    it "reads no subpacket of another Kind than data (Core Table 19)" $ do
      let credit = PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader 0x8001) (generate propertiesCall)]
      classifyIfSend respond base (comPacketOf [credit]) `shouldSatisfy` discarded

  describe "the answer to IF-RECV on a static ComID" $ do
    let p1 = packetOfSize 4    -- 40 bytes
        p2 = packetOfSize 16   -- 52 bytes
        cs s = ComIdState 0 Issued s
    it "has the packets the tests stage" $ map B.length [p1, p2] `shouldBe` [40, 52]
    it "in S1 is (0, 0, 0), exactly the length asked, S1 kept" $ do
      let (r, cs') = ifRecvStatic base 2048 (cs S1)
      (status r, B.length (rpayload r), frameOf r, cSync cs') `shouldBe` (Good, 2048, Just (ComPacketHeader base 0 0 0, []), S1)
      fst (ifRecvStatic base 4 (cs S1)) `shouldBe` Response Good (B.replicate 4 0)
    it "names the ComID's extension" $
      frameOf (fst (ifRecvStatic base 2048 (ComIdState 7 Issued S1))) `shouldBe` Just (ComPacketHeader base 7 0 0, [])
    it "in S2 is (0, 1, 0), S2 kept" $ do
      let (r, cs') = ifRecvStatic base 2048 (cs (S2 [p1]))
      (frameOf r, cSync cs') `shouldBe` (Just (ComPacketHeader base 0 1 0, []), S2 [p1])
    it "in S3, short of the header and the first packet, is (0, all staged, the least for the first), S3 kept" $ do
      let (r, cs') = ifRecvStatic base 59 (cs (S3 [p1, p2]))
      (B.length (rpayload r), frameOf r, cSync cs') `shouldBe` (59, Just (ComPacketHeader base 0 (20 + 92) (20 + 40), []), S3 [p1, p2])
    it "in S3, room for some packets, returns them, (their length, the rest, the least for the next), S3 with the rest" $
      forM_ [60, 100, 111] $ \len -> do
        let (r, cs') = ifRecvStatic base len (cs (S3 [p1, p2]))
        (frameOf r, cSync cs') `shouldBe` (Just (ComPacketHeader base 0 (20 + 52) (20 + 52), [packetFrame p1]), S3 [p2])
    it "in S3, room for all, returns them all, (their length, 0, 0), then S1" $ do
      let (r, cs') = ifRecvStatic base 112 (cs (S3 [p1, p2]))
      (B.length (rpayload r), frameOf r, cSync cs') `shouldBe` (112, Just (ComPacketHeader base 0 0 0, map packetFrame [p1, p2]), S1)
  where
    packetFrame p = either (const (PacketFrame (PacketHeader 1 1 1 1 1) [])) dfFrame (decodePacket p)
