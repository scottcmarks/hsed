{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.SyncSpec
Description : Test specifications for Hsed.Sim.Sync and the laws of Hsed.Law.Sync: Table 45, the outcomes of an IF-SEND, the answers to IF-RECV
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The synchronous protocol of a static ComID (Core 3.3.10, PDF 82-87): the
generated Table 45 (PDF 83), its six rows by position and its five
quantities in words; what an IF-SEND accepted in S1 comes to, over the TPer's
state, the simulator's own Session Manager answering Properties, within what
the TPer and the host each take (Core 5.2.2.4.1: a packet of more subpackets
than the TPer's MaxSubpackets discarded, as ETC-15's is; invocations past
its MaxMethods and packets past its MaxPackets ignored; an answer too large
for the host RESPONSE_OVERFLOW, the host's limits the properties the ComID
keeps of it), the control session's tokens read in order (a stray token
discarding the payload, as ETC-07's does; End of Session and Transaction
Control tokens ignored; a status list's reserved values ignored), a packet
on a session the TPer has not open discarded with the TPer unchanged; the
answer to IF-RECV in each state, exactly the length asked, as many packets
as the host takes; and the four laws of "Hsed.Law.Sync", each run as a
QuickCheck property of 100 cases.
-}
module Hsed.Sim.SyncSpec (spec) where

import qualified RIO.ByteString            as B
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck     (modifyMaxSuccess)
import           Test.QuickCheck           (counterexample, property)

import           Hsed.Core.Call            (MethodCall (..), MethodHeader (..), Parameters (..), statusList)
import           Hsed.Core.Method          (invocation)
import           Hsed.Core.StreamItem      (StreamItem (generate))
import           Hsed.Core.Sync            (Setting (..), StateRow (..), StateTable (..))
import           Hsed.Core.Token           (Token (..))
import           Hsed.Core.Value           (List (..), Value (..))
import           Hsed.Gen.Core.Layouts     (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..))
import qualified Hsed.Gen.Core.Methods     as Methods
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Gen.Core.UIDs        (uSMUID, uStartSession)
import           Hsed.Law                  (Law (..))
import qualified Hsed.Law.Sync             as Law
import           Hsed.Packet               (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, decodePacket,
                                            encodeComPacket, encodePacket)
import           Hsed.Sim.SessionManager   (answer)
import           Hsed.Sim.Sync
import           Hsed.Sim.Types            (ComIdLife (..), ComIdState (..), MgmtBuffer (..), Outcome (..), SimParams (..),
                                            SyncState (..), TPerState (..), defaultSimParams, initialHostProperties, initialState,
                                            maxComPacketSizeName, maxMethodsName, maxPacketSizeName, maxPacketsName,
                                            maxSubpacketsName)
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

-- | The default TPer, awaiting IF-SEND on its base ComID.
st0 :: TPerState
st0 = initialState defaultSimParams

-- | The TPer with one of its properties set, in its place among them (the
--   answer to Properties reports them in that order).
withTPer :: Text -> Integer -> TPerState -> TPerState
withTPer name v st = st { tsParams = (tsParams st) { spTPerProperties = map replaced (spTPerProperties (tsParams st)) } }
  where replaced (n, v') = (n, if n == name then v else v')

-- | The TPer with one of the host's properties set on its base ComID.
withHost :: Text -> Natural -> TPerState -> TPerState
withHost name v st = st { tsComIds = Map.adjust (\cs -> cs { cHostProps = Map.insert name v (cHostProps cs) }) base (tsComIds st) }

-- | An IF-SEND on the base ComID classified by a TPer: its outcome alone.
classifyIn :: TPerState -> ByteString -> Outcome
classifyIn st bytes = fst (classifyIfSend base bytes st)

-- | An IF-SEND on the base ComID classified by the default TPer.
classify :: ByteString -> Outcome
classify = classifyIn st0

-- | The Session Manager's answer to Properties on a TPer, which the outcomes
--   are checked against: it reports that TPer's properties.
replyIn :: TPerState -> MethodCall
replyIn st = fromMaybe (error "the Session Manager does not answer Properties") (fst (answer base propertiesCall st))

-- | Its answer on the default TPer.
reply :: MethodCall
reply = replyIn st0

-- | Properties' tokens to its End of Data, then a status list of these bytes
--   (each a tiny atom: 0x41 a signed one).
propertiesWith :: [Word8] -> ByteString
propertiesWith statuses =
  generate Call <> generate (_header propertiesCall) <> generate (_parameters propertiesCall) <> generate EndOfData
  <> B.pack ([0xF0] <> statuses <> [0xF1])

-- | The response packet the answers make: one control session packet, one
--   data subpacket of their tokens.
responseOf :: [MethodCall] -> Either Text [ByteString]
responseOf mcs = either (Left . tshow) (Right . pure) (encodePacket (control [foldMap generate mcs]))

-- | What the TPer prepares for these answers.
prepared :: [MethodCall] -> Outcome
prepared = either Discarded Prepared . responseOf

-- | A packet of so many data bytes, encoded: 24 + 12 + n bytes, n a multiple of 4.
packetOfSize :: Int -> ByteString
packetOfSize n = either (const mempty) id (encodePacket (control [B.replicate n 7]))

discarded :: Outcome -> Bool
discarded (Discarded _) = True
discarded _             = False

-- | The ComPacket an IF-RECV answer holds: its header and packets.
frameOf :: Response -> Maybe (ComPacketHeader, [PacketFrame])
frameOf r = either (const Nothing) (\(DecodedFrame (ComPacketFrame h ps) _ _) -> Just (h, ps)) (decodeComPacket (rpayload r))

-- | A static ComID's state in a synchronous state, the host's properties
--   its initial ones.
comIdIn :: SyncState -> ComIdState
comIdIn s = ComIdState 0 Issued s initialHostProperties (MgmtBuffer False Nothing)

-- | The same with one of the host's properties set.
hostOf :: Text -> Natural -> ComIdState -> ComIdState
hostOf name v cs = cs { cHostProps = Map.insert name v (cHostProps cs) }


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
    it "is a response packet on the control session of the Session Manager's answer, the TPer otherwise as it was" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [control [generate propertiesCall]]) st0
      outcome `shouldBe` prepared [reply]
      st' `shouldBe` st0
    it "frames the answer as Hsed.Packet decodes it: Session 0, one data subpacket of its tokens" $
      case classify (comPacketOf [control [generate propertiesCall]]) of
        Prepared [p] -> dfFrame <$> decodePacket p `shouldBe` Right (control [generate reply])
        other        -> expectationFailure (show other)
    it "drops Empty atoms wherever they are among the tokens (Core 3.2.2.3.1.5)" $ do
      let pieces = [ generate Call, generate (_header propertiesCall), generate (_parameters propertiesCall), generate EndOfData
                   , generate (statusList (_statusCode propertiesCall)) ]
          spaced = "\xff" <> mconcat (L.intersperse "\xff\xff" pieces) <> "\xff"
      classify (comPacketOf [control [spaced]]) `shouldBe` prepared [reply]
    it "answers the invocations of a subpacket as far as the TPer's MaxMethods, ignoring the rest (Core 5.2.2.4.1.7)" $
      classify (comPacketOf [control [generate propertiesCall <> generate propertiesCall]]) `shouldBe` prepared [reply]
    it "answers each call the Session Manager answers, in order, each in a packet of its own, and ignores the rest (Core 3.2.4.2)" $ do
      let unanswered = propertiesCall { _header = (_header propertiesCall) { _methodID = uStartSession } }
          two        = withTPer maxMethodsName 2 st0
          packets    = traverse (responseOf . pure) [replyIn two, replyIn two] <&> concat
      classifyIn two (comPacketOf [control [generate unanswered <> generate propertiesCall]]) `shouldBe` prepared [replyIn two]
      classifyIn two (comPacketOf [control [generate propertiesCall <> generate propertiesCall]]) `shouldBe` either Discarded Prepared packets
    it "discards a control session packet of more subpackets than the TPer's MaxSubpackets, ETC-15's (Core 5.2.2.4.1.1)" $ do
      classify (comPacketOf [control [generate propertiesCall, generate propertiesCall]]) `shouldSatisfy` discarded
      classify (comPacketOf [control [generate propertiesCall, ""]]) `shouldSatisfy` discarded
      let roomier = withTPer maxSubpacketsName 2 st0
      classifyIn roomier (comPacketOf [control [generate propertiesCall, ""]]) `shouldBe` prepared [replyIn roomier]
    it "reads the packets as far as the TPer's MaxPackets, ignoring the rest (Core 5.2.2.4.1.3)" $ do
      let unanswered = propertiesCall { _header = (_header propertiesCall) { _methodID = uStartSession } }
      classify (comPacketOf [control [generate propertiesCall], control [generate propertiesCall]]) `shouldBe` prepared [reply]
      classify (comPacketOf [control [generate unanswered], control [generate propertiesCall]]) `shouldSatisfy` discarded
    it "discards the payload at a token out of place: before a call, a Call inside one, one after a call (Core 3.2.2.4.2; Opal 3.3.4.1.3)" $
      forM_ [ generate EndList <> generate propertiesCall                                    -- ETC-07's shape
            , generate Call <> generate propertiesCall
            , generate propertiesCall <> generate EndList <> generate propertiesCall
            , generate propertiesCall <> generate EndList
            , generate propertiesCall <> generate StartName ] $ \tokens ->
        classifyIn (withTPer maxMethodsName 2 st0) (comPacketOf [control [tokens]]) `shouldSatisfy` discarded
    it "ignores End of Session and Transaction Control tokens, each with its status, on the control session (Core 3.3.7.1.3)" $
      forM_ [ generate propertiesCall <> generate EndOfSession
            , generate propertiesCall <> generate EndTransaction <> "\x00"
            , generate StartTransaction <> "\x00" <> generate propertiesCall
            , generate EndOfSession <> generate propertiesCall
            , generate StartTransaction <> generate propertiesCall ] $ \tokens ->
        classify (comPacketOf [control [tokens]]) `shouldBe` prepared [reply]
    it "answers RESPONSE_OVERFLOW, an empty list, for an answer the host's packet cannot hold, and nothing after it (Opal 3.3.1)" $ do
      let overflowed = reply { _parameters = Parameters [] [], _statusCode = 0x11 }
          small      = withHost maxPacketSizeName 100 (withTPer maxMethodsName 2 st0)
      classifyIn small (comPacketOf [control [generate propertiesCall <> generate propertiesCall]]) `shouldBe` prepared [overflowed]
    it "discards the payload where even RESPONSE_OVERFLOW's answer would not fit the host's ComPacket, staging nothing (no host within Table 17's minima)" $ do
      let outcome = classifyIn (withHost maxComPacketSizeName 40 st0) (comPacketOf [control [generate propertiesCall]])
      outcome `shouldSatisfy` discarded
      outcome `shouldSatisfy` \o -> case o of
        Discarded why -> "RESPONSE_OVERFLOW" `T.isInfixOf` why
        _             -> False
    it "discards a call the Session Manager does not answer, and tokens that are no call" $ do
      let unanswered = propertiesCall { _header = (_header propertiesCall) { _methodID = uStartSession } }
      classify (comPacketOf [control [generate unanswered]]) `shouldSatisfy` discarded
      classify (comPacketOf [control ["\xf0\xf1"]]) `shouldSatisfy` discarded
      classify (comPacketOf [control ["\x80"]]) `shouldSatisfy` discarded
      classify (comPacketOf [control [B.take 10 (generate propertiesCall)]]) `shouldSatisfy` discarded
    it "discards a ComPacket that asks nothing: no packets, packets without payload, subpackets empty or of Empty atoms (3.3.10.6 S2:S1 d)" $ do
      classify (comPacketOf []) `shouldSatisfy` discarded
      classify (comPacketOf [control [], control []]) `shouldSatisfy` discarded
      classify (comPacketOf [control ["\xff\xff\xff\xff"], control ["\xff"]]) `shouldSatisfy` discarded
    it "discards a ComPacket whose packet names a session the TPer does not have, the TPer unchanged (3.3.10.7 a, ETC-04)" $
      forM_ [(1, 0), (0, 1), (0x1000, 0x69), (4096, 1)] $ \session -> do
        let (outcome, st') = classifyIfSend base (comPacketOf [packetIn session [generate propertiesCall]]) st0
        outcome `shouldSatisfy` discarded
        st' `shouldBe` st0
    it "discards what is no ComPacket of the ComID, or names an extension other than a static ComID's" $ do
      classify "\x00\x01" `shouldSatisfy` discarded
      classify (comPacketOn 0x1005 0 [control [generate propertiesCall]]) `shouldSatisfy` discarded
      classify (comPacketOn base 1 [control [generate propertiesCall]]) `shouldSatisfy` discarded
    it "reads no subpacket of another Kind than data (Core Table 19)" $ do
      let credit = PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader 0x8001) (generate propertiesCall)]
      classify (comPacketOf [credit]) `shouldSatisfy` discarded
    it "discards on a ComID the TPer does not have" $
      fst (classifyIfSend 0x1005 (comPacketOn 0x1005 0 [control [generate propertiesCall]]) st0) `shouldSatisfy` discarded

  describe "a control session's data subpacket, read" $ do
    it "is its invocations in order, a status list's reserved values and any after them ignored (Core 3.2.4.2 item 2)" $
      map (controlInvocations . propertiesWith) [[0x00, 0x00, 0x00], [0x00, 0x01, 0x00], [0x00, 0x00, 0x07, 0x05], [0x10, 0x00, 0x00]]
        `shouldBe` map (Right . pure . Just) [propertiesCall, propertiesCall, propertiesCall, propertiesCall { _statusCode = 0x10 }]
    it "is no invocations at a status list of fewer than three values, or one of a signed one (3.2.2.4.2 item 5)" $
      map (controlInvocations . propertiesWith) [[0x00, 0x00], [], [0x00, 0x00, 0x41]] `shouldSatisfy` all isLeft
    it "ignores an invocation whose header does not read: a uinteger where its InvokingID goes (3.2.2.4.2 item 3)" $ do
      let unread = generate Call <> "\x01" <> generate (_methodID (_header propertiesCall)) <> generate (_parameters propertiesCall)
                   <> generate EndOfData <> generate (statusList 0)
      controlInvocations unread `shouldBe` Right [Nothing]
      classify (comPacketOf [control [unread]]) `shouldSatisfy` discarded
    it "reads ETC-08's StartList after StartList as a call with a list for a parameter, which the Session Manager does not answer" $ do
      let etc08 = generate Call <> generate (_header propertiesCall) <> B.pack [0xF0, 0xF0, 0xF1, 0xF1] <> generate EndOfData
                  <> generate (statusList 0)
      controlInvocations etc08 `shouldBe` Right [Just propertiesCall { _parameters = Parameters [L (List [])] [] }]
      classify (comPacketOf [control [etc08]]) `shouldSatisfy` discarded

  describe "the answer to IF-RECV on a static ComID" $ do
    let p1 = packetOfSize 4    -- 40 bytes
        p2 = packetOfSize 16   -- 52 bytes
        recv = ifRecvStatic defaultSimParams base
    it "has the packets the tests stage" $ map B.length [p1, p2] `shouldBe` [40, 52]
    it "in S1 is (0, 0, 0), exactly the length asked, S1 kept" $ do
      let (r, cs') = recv 2048 (comIdIn S1)
      (status r, B.length (rpayload r), frameOf r, cSync cs') `shouldBe` (Good, 2048, Just (ComPacketHeader base 0 0 0, []), S1)
      fst (recv 4 (comIdIn S1)) `shouldBe` Response Good (B.replicate 4 0)
    it "names the ComID's extension" $
      frameOf (fst (recv 2048 (comIdIn S1) { cExt = 7 })) `shouldBe` Just (ComPacketHeader base 7 0 0, [])
    it "in S2 is (0, 1, 0), S2 kept" $ do
      let (r, cs') = recv 2048 (comIdIn (S2 [p1]))
      (frameOf r, cSync cs') `shouldBe` (Just (ComPacketHeader base 0 1 0, []), S2 [p1])
    it "in S3, short of the header and the first packet, is (0, all staged, the least for the first), S3 kept" $ do
      let (r, cs') = recv 59 (comIdIn (S3 [p1, p2]))
      (B.length (rpayload r), frameOf r, cSync cs') `shouldBe` (59, Just (ComPacketHeader base 0 (20 + 92) (20 + 40), []), S3 [p1, p2])
    it "in S3, short of them for the host's ComPacket (the property the ComID keeps), the transfer however long, is the same" $ do
      let (r, cs') = recv 2048 (hostOf maxComPacketSizeName 59 (comIdIn (S3 [p1, p2])))
      (frameOf r, cSync cs') `shouldBe` (Just (ComPacketHeader base 0 (20 + 92) (20 + 40), []), S3 [p1, p2])
    it "in S3, room for packets, returns as many as the host's MaxPackets, one: (its length, the rest, the least for the next), S3 with the rest (Core 5.2.2.4.1.3)" $
      forM_ [60, 100, 111, 112, 2048] $ \len -> do
        let (r, cs') = recv len (comIdIn (S3 [p1, p2]))
        (frameOf r, cSync cs') `shouldBe` (Just (ComPacketHeader base 0 (20 + 52) (20 + 52), [packetFrame p1]), S3 [p2])
    it "in S3, room for all and a host that takes them (MaxPackets 2 kept on the ComID), returns them all, (their length, 0, 0), then S1" $ do
      let (r, cs') = recv 112 (hostOf maxPacketsName 2 (comIdIn (S3 [p1, p2])))
      (B.length (rpayload r), frameOf r, cSync cs') `shouldBe` (112, Just (ComPacketHeader base 0 0 0, map packetFrame [p1, p2]), S1)
    it "in S3, a host MaxPackets of 0, no limit (Core Table 167), returns them all" $ do
      let (r, cs') = recv 112 (hostOf maxPacketsName 0 (comIdIn (S3 [p1, p2])))
      (frameOf r, cSync cs') `shouldBe` (Just (ComPacketHeader base 0 0 0, map packetFrame [p1, p2]), S1)
    it "in S3, the last packet, returns it, (its length, 0, 0), then S1" $ do
      let (r, cs') = recv 2048 (comIdIn (S3 [p2]))
      (frameOf r, cSync cs') `shouldBe` (Just (ComPacketHeader base 0 0 0, [packetFrame p2]), S1)

  describe "the laws of Hsed.Law.Sync" $
    modifyMaxSuccess (const 100) $
      forM_ Law.laws $ \l ->
        it (T.unpack (lawName l) <> ", over the simulator") $
          fromMaybe (counterexample "the law has no property" (property False)) (lawProp l)
  where
    packetFrame p = either (const (PacketFrame (PacketHeader 1 1 1 1 1) [])) dfFrame (decodePacket p)
