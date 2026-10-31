{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

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
than the TPer's MaxSubpackets discarded with the whole payload, as ETC-15's
is, wherever it comes among the packets considered, 3.3.10.7 a; invocations
past its MaxMethods and packets past its MaxPackets ignored; an answer too
large for the host RESPONSE_OVERFLOW, which ends the payload's processing,
the host's limits the properties the ComID keeps of it, and every packet
staged one its ComPacket can carry), the control session's tokens read in order by "Hsed.Core.Stream" (a
stray token discarding the payload, as ETC-07's does; End of Session and
Transaction Control tokens ignored; a status list's reserved values ignored;
an S-bit atom, ETC-08's bytes, a call on ThisSP and a host's CloseSession
each discarded), a packet on a session the TPer has not open discarded with
the TPer unchanged; a regular session's packet, opened directly
("Hsed.Sim.State".@openSession@), answered in a packet carrying its TSN and
HSN, [Get, EOS] closing it, a bare EOS echoed, (TSN, HSN + 1) discarded
with the session kept (ETC-04), the error cases aborting it with the TPer's
CloseSession or answering NOT_AUTHORIZED (ETC-05, ETC-06, ETC-14's shape);
the answer to IF-RECV in each state, exactly the length asked, as many
packets as the host takes; and the four laws of "Hsed.Law.Sync", each run
as a QuickCheck property of 100 cases.
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

import           Hsed.Core.Call            (MethodCall (..), MethodHeader (..), MethodResponse (..), Parameters (..), statusList)
import           Hsed.Core.CellBlock       (cellBlockValue, columnRange)
import           Hsed.Core.Method          (invocation)
import           Hsed.Core.StreamItem      (StreamItem (generate))
import           Hsed.Core.Sync            (Setting (..), StateRow (..), StateTable (..))
import           Hsed.Core.Token           (Token (..))
import           Hsed.Core.Value           (List (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes       (StatusCode (..), pattern NOT_AUTHORIZED, pattern RESPONSE_OVERFLOW)
import           Hsed.Gen.Core.Layouts     (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..))
import qualified Hsed.Gen.Core.Methods     as Methods
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Core.Token           (IsToken (..))
import           Hsed.Core.Value           (IsValue (..))
import           Hsed.Gen.Core.UIDs        (uAuthority_Anybody, uCloseSession, uProperties, uSMUID, uSP_Admin, uStartSession,
                                            uThisSP)
import           Hsed.Law                  (Law (..))
import qualified Hsed.Law.Sync             as Law
import           Hsed.OpalUIDs             (uC_PIN_MSID)
import           Hsed.Packet               (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, decodePacket,
                                            encodeComPacket, encodePacket)
import           Hsed.Sim                  (step)
import           Hsed.Sim.SessionManager   (answer, closeSessionCall)
import           Hsed.Sim.State            (lifeOf, openSession, resolveSession)
import           Hsed.Sim.Sync
import           Hsed.Sim.Types            (ComIdLife (..), ComIdState (..), Input (..), MgmtBuffer (..), Outcome (..), Output (..),
                                            Session (..), SimParams (..), SyncState (..), TPerState (..), defaultSimParams,
                                            initialHostProperties, initialState, maxComPacketSizeName, maxMethodsName,
                                            maxPacketSizeName, maxPacketsName, maxSubpacketsName)
import           Hsed.Wire                 (Dir (..), IfStatus (..), Request (..), Response (..), Unit (..), comPacketProtocol,
                                            roundUp)


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

-- | The host's StartSession to the Admin SP as Anybody, HSN 1, read-write.
startSessionCall :: MethodCall
startSessionCall = invocation uSMUID Methods.StartSession
  { Methods.startSessionHostSessionID         = value (1 :: Natural)
  , Methods.startSessionSPID                  = value (token uSP_Admin)
  , Methods.startSessionWrite                 = value (1 :: Natural)
  , Methods.startSessionHostChallenge         = Nothing
  , Methods.startSessionHostExchangeAuthority = Nothing
  , Methods.startSessionHostExchangeCert      = Nothing
  , Methods.startSessionHostSigningAuthority  = Nothing
  , Methods.startSessionHostSigningCert       = Nothing
  , Methods.startSessionSessionTimeout        = Nothing
  , Methods.startSessionTransTimeout          = Nothing
  , Methods.startSessionInitialCredit         = Nothing
  , Methods.startSessionSignedHash            = Nothing
  }

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

-- | The TPer with a regular session open on its base ComID: an Admin SP
--   session as Anybody, HSN 0x51, TSN 4096.
ses :: Session
st1 :: TPerState
(ses, st1) = openSession base 0x51 uSP_Admin True [uAuthority_Anybody] st0

-- | A packet of the session.
inSession :: [ByteString] -> PacketFrame
inSession = packetIn (sesTSN ses, sesHSN ses)

-- | UCT-03 step 1b's Get, C_PIN_MSID's PIN column, and its result.
getCall :: MethodCall
getCall = invocation uC_PIN_MSID (Methods.Get (cellBlockValue (columnRange 3 3)))

getResult :: MethodResponse
getResult = MethodResponse [L (List [N ((3 :: Natural) .: spMsid defaultSimParams)])] 0

-- | A response packet of the session of these payloads, encoded.
sessionPacket :: [ByteString] -> ByteString
sessionPacket payloads = either (const mempty) id (encodePacket (inSession payloads))

-- | The TPer's CloseSession for the session, a control session packet.
closing :: ByteString
closing = either (const mempty) id (encodePacket (control [generate (closeSessionCall ses)]))

-- | Whether the session is still open in a TPer.
open :: TPerState -> Bool
open = isJust . resolveSession base (sesTSN ses) (sesHSN ses)

-- | An IF-SEND of a payload on the base ComID through the simulator's step,
--   in bytes, then a Tick and an IF-RECV of 2048 bytes: the IF-SEND's answer
--   and the TPer after it, and the IF-RECV's.
sent :: ByteString -> TPerState -> (Output, TPerState)
sent bytes = step (Cmd (Request IfSend comPacketProtocol base (roundUp UnitBytes (fromIntegral (B.length bytes))) bytes))

received :: TPerState -> (Output, TPerState)
received st = step (Cmd (Request IfRecv comPacketProtocol base 2048 mempty)) (snd (step (Tick base) st))

-- | The ComPacket an IF-RECV's output holds.
frameOut :: Output -> Maybe (ComPacketHeader, [PacketFrame])
frameOut = \case
  Out r -> frameOf r
  _     -> Nothing


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
    it "ends the payload at an overflow on the control session: the calls after the overflowed one are not run, a StartSession among them opening nothing (Opal 3.3.1 item 1)" $ do
      let overflowed = reply { _parameters = Parameters [] [], _statusCode = statusValue RESPONSE_OVERFLOW }
          small      = withHost maxPacketSizeName 100 (withTPer maxMethodsName 2 (withTPer maxPacketsName 2 st0))
          (outcome, st') = classifyIfSend base (comPacketOf [control [generate propertiesCall <> generate startSessionCall], control [generate startSessionCall]]) small
      outcome `shouldBe` prepared [overflowed]
      tsSessions st' `shouldBe` mempty
    it "discards the whole payload at a control packet over the TPer's limits, wherever it comes among the packets considered, the TPer unchanged (Core 3.3.10.7 a; 5.2.2.4.1.1-2; TPer MaxPackets 2)" $ do
      let two = withTPer maxPacketsName 2 st0
      forM_ [ [control [generate propertiesCall, generate propertiesCall], control [generate propertiesCall]]
            , [control [generate propertiesCall], control [generate propertiesCall, generate propertiesCall]] ] $ \ps -> do
        let (outcome, st') = classifyIfSend base (comPacketOf ps) two
        outcome `shouldSatisfy` discarded
        st' `shouldBe` two
      classifyIn two (comPacketOf [control [generate propertiesCall], control [generate propertiesCall]])
        `shouldBe` either Discarded Prepared (concat <$> traverse (responseOf . pure) [replyIn two, replyIn two])
    it "through the step, stages packets an IF-RECV can carry for a host taking eight subpackets a packet of any size, within its ComPacket (Core 5.2.2.4.1.4): the first IF-RECV returns a packet" $ do
      let wide = withHost maxSubpacketsName 8 (withHost maxPacketSizeName 0 (withTPer maxMethodsName 8 st0))
          (out, st') = sent (comPacketOf [control [mconcat (replicate 8 (generate propertiesCall))]]) wide
          staged = case Map.lookup base (tsComIds st') of
            Just ComIdState { cSync = S2 ps } -> ps
            _                                 -> []
      out `shouldBe` Out (Response Good mempty)
      length staged `shouldSatisfy` (>= 2)
      sum (map B.length staged) `shouldSatisfy` (> 2028)
      staged `shouldSatisfy` all ((<= 2028) . B.length)
      fmap (not . null . snd) (frameOut (fst (received st'))) `shouldBe` Just True
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

  describe "a control session's tokens, read by Hsed.Core.Stream" $ do
    it "answers an invocation whatever its status list's reserved values and any after them, a host status echoed (Core 3.2.4.2 item 2)" $ do
      forM_ [[0x00, 0x00, 0x00], [0x00, 0x01, 0x00], [0x00, 0x00, 0x07, 0x05]] $ \statuses ->
        classify (comPacketOf [control [propertiesWith statuses]]) `shouldBe` prepared [reply]
      classify (comPacketOf [control [propertiesWith [0x10, 0x00, 0x00]]])
        `shouldBe` prepared [MethodCall (MethodHeader uSMUID uProperties) (Parameters [L (List [])] []) 0x10]
    it "discards the payload at a status list of fewer than three values, or one holding an S-bit atom (3.2.2.4.2 item 5; Opal Table 15)" $
      forM_ [[0x00, 0x00], [], [0x00, 0x00, 0x41]] $ \statuses ->
        classify (comPacketOf [control [propertiesWith statuses]]) `shouldSatisfy` discarded
    it "discards the payload at an invocation whose header does not read: a uinteger where its InvokingID goes (3.2.2.4.2 item 3; Opal 3.3.4.1.3)" $ do
      let unread = generate Call <> "\x01" <> generate (_methodID (_header propertiesCall)) <> generate (_parameters propertiesCall)
                   <> generate EndOfData <> generate (statusList 0)
      classify (comPacketOf [control [unread]]) `shouldSatisfy` discarded
    it "discards ETC-08's bytes, a Properties call with a list for a required parameter, which the Session Manager does not answer (Test Cases PDF 81)" $ do
      let etc08 = B.pack [ 0xF8, 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x01
                         , 0xF0, 0xF0, 0xF1, 0xF1, 0xF9, 0xF0, 0x00, 0x00, 0x00, 0xF1 ]
      etc08 `shouldBe` generate Call <> generate (_header propertiesCall) <> B.pack [0xF0, 0xF0, 0xF1, 0xF1] <> generate EndOfData <> generate (statusList 0)
      let (outcome, st') = classifyIfSend base (comPacketOf [control [etc08]]) st0
      outcome `shouldSatisfy` discarded
      st' `shouldBe` st0
    it "answers [Properties, EOS] with one Properties answer and no End of Session (3.3.7.1.3)" $
      case classify (comPacketOf [control [generate propertiesCall <> generate EndOfSession]]) of
        Prepared [p] -> dfFrame <$> decodePacket p `shouldBe` Right (control [generate reply])
        other        -> expectationFailure (show other)
    it "discards a call on ThisSP, and a host's CloseSession, which only the TPer transmits (3.3.7.1.3; 5.2.3.5)" $ do
      classify (comPacketOf [control [generate propertiesCall { _header = MethodHeader uThisSP uProperties }]]) `shouldSatisfy` discarded
      classify (comPacketOf [control [generate (closeSessionCall ses)]]) `shouldSatisfy` discarded
      classify (comPacketOf [control [generate propertiesCall { _header = MethodHeader uSMUID uCloseSession }]]) `shouldSatisfy` discarded
    it "discards a control session packet of more bytes than the TPer's MaxPacketSize, the TPer unchanged (Core 5.2.2.4.1.2)" $ do
      let roomy = withTPer maxComPacketSizeName 4096 st0
          large = generate propertiesCall <> B.replicate 2000 0xff   -- a packet of 36 + 2036 bytes, past MaxPacketSize 2028
      let (outcome, st') = classifyIfSend base (comPacketOf [control [large]]) roomy
      outcome `shouldSatisfy` discarded
      st' `shouldBe` roomy
      classifyIn (withTPer maxPacketSizeName 2100 roomy) (comPacketOf [control [large]]) `shouldBe` prepared [replyIn (withTPer maxPacketSizeName 2100 roomy)]
    it "through the step, ETC-15: a packet of two Properties subpackets is Good, the ComID in S1, the TPer unchanged, and the next IF-RECV (0, 0, 0)" $ do
      let (out, st') = sent (comPacketOf [control [generate propertiesCall, generate propertiesCall]]) st0
      (out, st') `shouldBe` (Out (Response Good mempty), st0)
      frameOut (fst (received st')) `shouldBe` Just (ComPacketHeader base 0 0 0, [])
    it "through the step, a packet over the TPer's MaxPacketSize does the same" $ do
      let roomy = withTPer maxComPacketSizeName 4096 st0
          (out, st') = sent (comPacketOf [control [generate propertiesCall <> B.replicate 2000 0xff]]) roomy
      (out, st') `shouldBe` (Out (Response Good mempty), roomy)
      frameOut (fst (received st')) `shouldBe` Just (ComPacketHeader base 0 0 0, [])

  describe "a regular session's packet" $ do
    it "answers a Get in a packet carrying the session's TSN and HSN, one data subpacket of the result, the TPer otherwise as it was" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate getCall]]) st1
      outcome `shouldBe` Prepared [sessionPacket [generate getResult]]
      st' `shouldBe` st1
    it "answers [Get, EOS] with the result then the TPer's End of Session, the session closed, the ComID Issued (3.2.2.3.3.3; 3.3.7.1.5)" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate getCall <> generate EndOfSession]]) st1
      outcome `shouldBe` Prepared [sessionPacket [generate getResult <> generate EndOfSession]]
      open st' `shouldBe` False
      lifeOf base st' `shouldBe` Just Issued
    it "echoes a bare End of Session and closes the session; the tokens after it are ignored" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate EndOfSession <> generate getCall]]) st1
      outcome `shouldBe` Prepared [sessionPacket [generate EndOfSession]]
      open st' `shouldBe` False
    it "discards a packet of the session's TSN and another HSN, or another TSN, the session kept (3.3.10.7 a; ETC-04)" $
      forM_ [(sesTSN ses, sesHSN ses + 1), (sesTSN ses + 1, sesHSN ses), (sesTSN ses, 0), (0, sesHSN ses)] $ \session -> do
        let (outcome, st') = classifyIfSend base (comPacketOf [packetIn session [generate getCall]]) st1
        outcome `shouldSatisfy` discarded
        st' `shouldBe` st1
    it "ends the payload at an overflow in a regular session: the subpackets after the overflowed one are not processed (Opal 3.3.1 item 1; TPer MaxSubpackets 2)" $ do
      let (narrow, stN) = openSession base 0x52 uSP_Admin True [uAuthority_Anybody] (withHost maxPacketSizeName 60 (withTPer maxSubpacketsName 2 st0))
          (outcome, st') = classifyIfSend base (comPacketOf [packetIn (sesTSN narrow, sesHSN narrow) [generate getCall, generate getCall]]) stN
      outcome `shouldBe` Prepared [either (const mempty) id (encodePacket (packetIn (sesTSN narrow, sesHSN narrow) [generate (MethodResponse [] (statusValue RESPONSE_OVERFLOW))]))]
      isJust (resolveSession base (sesTSN narrow) (sesHSN narrow) st') `shouldBe` True
    it "discards a session packet that asks nothing: no data subpacket, or Empty atoms alone, the session kept" $ do
      forM_ [inSession [], inSession ["\xff\xff\xff\xff"]] $ \p -> do
        let (outcome, st') = classifyIfSend base (comPacketOf [p]) st1
        outcome `shouldSatisfy` discarded
        st' `shouldBe` st1
    it "aborts the session at EndList before Call, preparing the TPer's CloseSession [HSN, TSN] on the control session (3.2.2.4.2 item 2; 3.3.10.7 b; ETC-05)" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate EndList <> generate getCall]]) st1
      outcome `shouldBe` SessionAborted [] (Just closing)
      open st' `shouldBe` False
      lifeOf base st' `shouldBe` Just Issued
    it "aborts without a CloseSession where the run prepares none (spCloseSessionOnAbort False)" $ do
      let (quiet, stQ) = openSession base 0x51 uSP_Admin True [uAuthority_Anybody] (initialState defaultSimParams { spCloseSessionOnAbort = False })
          (outcome, st') = classifyIfSend base (comPacketOf [packetIn (sesTSN quiet, sesHSN quiet) [generate EndList <> generate getCall]]) stQ
      outcome `shouldBe` SessionAborted [] Nothing
      isJust (resolveSession base (sesTSN quiet) (sesHSN quiet) st') `shouldBe` False
    it "answers Call EndList, a header that does not read, with NOT_AUTHORIZED and an empty result, the session kept (3.2.2.4.2 item 3; ETC-06)" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate Call <> generate EndList <> B.drop 1 (generate getCall)]]) st1
      outcome `shouldBe` Prepared [sessionPacket [generate (MethodResponse [] (statusValue NOT_AUTHORIZED))]]
      open st' `shouldBe` True
    it "aborts at a Start Transaction, the TPer starting no transaction (3.2.2.3.3.4)" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate StartTransaction <> "\x00" <> generate getCall]]) st1
      outcome `shouldBe` SessionAborted [] (Just closing)
      open st' `shouldBe` False
    it "aborts at a packet of two subpackets, more than the TPer's MaxSubpackets (5.2.2.4.1.1; ETC-14's shape)" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate getCall, generate getCall]]) st1
      outcome `shouldBe` SessionAborted [] (Just closing)
      open st' `shouldBe` False
    it "aborts at a packet of more bytes than the TPer's MaxPacketSize (5.2.2.4.1.2)" $ do
      let roomy = withTPer maxComPacketSizeName 4096 st1
          (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate getCall <> B.replicate 2000 0xff]]) roomy
      outcome `shouldBe` SessionAborted [] (Just closing)
      open st' `shouldBe` False
    it "answers the first of two invocations in a subpacket, then aborts, past the TPer's MaxMethods (5.2.2.4.1.7)" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate getCall <> generate getCall]]) st1
      outcome `shouldBe` SessionAborted [sessionPacket [generate getResult]] (Just closing)
      open st' `shouldBe` False
    it "aborts at an S-bit atom, a token the TPer does not read (Opal Table 15), with the results completed before it" $ do
      let (outcome, st') = classifyIfSend base (comPacketOf [inSession [generate getCall <> "\x41"]]) st1
      outcome `shouldBe` SessionAborted [] (Just closing)
      open st' `shouldBe` False
    it "processes a control packet and a session packet of one ComPacket in order, within the TPer's MaxPackets" $ do
      let two = withTPer maxPacketsName 2 st1
      classifyIn two (comPacketOf [control [generate propertiesCall], inSession [generate getCall]])
        `shouldBe` either Discarded Prepared ((<>) <$> responseOf [replyIn two] <*> pure [sessionPacket [generate getResult]])
      classifyIn st1 (comPacketOf [control [generate propertiesCall], inSession [generate getCall]]) `shouldBe` prepared [replyIn st1]
      classifyIn two (comPacketOf [control [generate propertiesCall { _header = MethodHeader uSMUID uStartSession }], inSession [generate getCall]])
        `shouldBe` Prepared [sessionPacket [generate getResult]]
    it "through the step: the Get's answer at the next IF-RECV, in the session's packet" $ do
      let (out, st') = sent (comPacketOf [inSession [generate getCall]]) st1
      out `shouldBe` Out (Response Good mempty)
      fmap (map (\p -> (pfHeader p, map dataSubPacketPayload (pfSubPackets p))) . snd) (frameOut (fst (received st')))
        `shouldBe` Just [(PacketHeader (sesTSN ses) (sesHSN ses) 0 0 0, [generate getResult])]

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
