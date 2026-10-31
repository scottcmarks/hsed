{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Jig.ScriptSpec
Description : Test specifications for Hsed.Jig.Script
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A script's verdict ('attempt'): its own, a failure where an expectation did
not hold or it threw, unobservable where a drive's transport cannot show a
status; a step for the simulator alone; on a drive, an IF-SEND stopped by
the Guard before the transport, recorded. The ComPackets the jig sends are
the Test Cases' (Table 6, PDF 27): the control session's with its
Properties call in design 11's M0 encoding, a session's with its TSN and
HSN, and ETC-15's control packet of several subpackets; ETC-08's payload
is the Test Cases' bytes (PDF 81). An IF-RECV's header is read for Table
45; the matcher's field names are the generated table's, and
MaxComPacketSize a property Core Table 167 names; each answer is named by
its row of Table 45, through "Hsed.Sim.Sync".@t45Rows@, which reads the
generated table's rows in order. An answer expected of a static ComID is a
ComPacket of it and a static extension; the drain classifies answers as
the host's loop does, so that those Core allows an SSC drain too; ETC-02's
drain is outside its verdict.

The session verbs (M4a), over a transport that answers from lists and keeps
what it was sent: an answer is read as the host reads a TPer's stream
('returnedOf': a SyncSession, a Get result, the TPer's End of Session);
StartSession goes out with HSN 1 and Write 1 (Test Cases 4.1 e and h, PDF
26), its authority and challenge where given, and comes back as a session,
a failed status, or no SyncSession (nothing, or the TPer's CloseSession, an
abort); Get goes in a packet of the session and comes back as its results
and status; End of Session is answered or not; Properties with the host's
properties reads both tables back; a status is named by Core Table 166; an
abort is accepted where an error status is expected (Test Cases 7.1, PDF
76); STACK_RESET is Core Tables 35 to 37 on protocol 2, stopped by the
Guard on a drive; the simulator's Opal SSC V2 descriptor and SID's
credential are read from its state; a session is opened and ended around
a step, STACK_RESET following a failure on the simulator.
-}
module Hsed.Jig.ScriptSpec (spec) where

import qualified RIO.ByteString            as B
import qualified RIO.List                  as L
import           RIO
import qualified RIO.Text                  as T
import           Test.Hspec

import           Hsed.Core.Call            (MethodCall (..), MethodHeader (..), MethodResponse (..), Parameters (..),
                                            optionalParameter)
import           Hsed.Core.CellBlock       (cellBlockValue, columnRange)
import           Hsed.Core.Method          (fromCall, invocationWith)
import           Hsed.Core.Properties      (Property (..))
import           Hsed.Core.Record          (DecodedRecord (..), decodeRecordStrict, encodeRecord)
import           Hsed.Core.Stream          (Returned (..))
import           Hsed.Core.StreamItem      (StreamItem (..))
import           Hsed.Core.Sync            (StateTable (..))
import           Hsed.Core.Token           (IsToken (..), Token (..))
import           Hsed.Core.Value           (IsValue (..), List (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes       (StatusCode (..), pattern INVALID_PARAMETER, pattern NOT_AUTHORIZED, pattern SP_BUSY,
                                            pattern SUCCESS, statusValue)
import           Hsed.Gen.Core.Layouts     (ComPacket (..), ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..),
                                            NoResponseAvailable (..), PacketHeader (..), STACK_RESETCommandRequest (..),
                                            STACK_RESETCommandResponse (..), STACK_RESETPending (..))
import qualified Hsed.Gen.Core.Methods     as Methods
import qualified Hsed.Gen.Core.Properties  as P
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Gen.Core.UIDs        (uAuthority_Anybody, uAuthority_SID, uProperties, uSMUID, uSP_Admin)
import           Hsed.Gen.Opal.Features    (Level0DiscoveryOpalSSCV2Feature (..))
import           Hsed.Jig.Case             (Case (..))
import           Hsed.Jig.Cases.ETC02      (etc02)
import           Hsed.Jig.Cases.ETC08      (etc08Payload)
import           Hsed.Jig.Cases.ETC15      (etc15)
import           Hsed.Jig.Cases.UCT02      (hostProperties, tperExpected, uct02)
import           Hsed.Jig.Options          (defaultRunOptions)
import           Hsed.Jig.Script
import           Hsed.Jig.Target           (Target (..))
import           Hsed.Jig.Transcript       (TEntry (..), entries, newTranscript)
import           Hsed.Level0               (splitLevel0)
import           Hsed.OpalUIDs             (uC_PIN_MSID)
import           Hsed.Packet               (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket)
import           Hsed.Sim.Level0           (level0Response)
import           Hsed.Sim.SessionManager   (closeSessionCall)
import           Hsed.Sim.Sync             (T45Row (..))
import           Hsed.Sim.Types            (Session (..), SimParams (..), defaultSimParams, initialState)
import           Hsed.Transport.InProc     (SimControl (..))
import           Hsed.Wire                 (ExchangeError (..), newChannel)
import           Hsed.Wire.Internal        (Describe (..), Dir (..), IfStatus (..), Request (..), Response (..), Transport (..),
                                            Unit (..))


-- | A transport that counts what reaches it and answers Good.
counted :: IORef Int -> Transport c
counted n = MkTransport
  { tExchange = \_ -> Response Good mempty <$ modifyIORef' n (+ 1)
  , tInject   = \_ -> pure Good
  , tUnit     = UnitBytes
  , tDescribe = Describe "test" "a test" UnitBytes Nothing Nothing Nothing Nothing
  , tClose    = pure ()
  }

-- | A script on the simulator's end or a drive's, over counting transports:
--   what it came to, how many commands reached the transport, the
--   transcript.
scripted :: Bool -> Script a -> IO (a, Int, [TEntry])
scripted sim s = do
  n <- newIORef 0
  t <- newTranscript
  channel <- newChannel
  ctx <- newIORef emptyRunCtx
  let target | sim       = SimTarget (counted n) (SimControl (\_ -> pure ()) (pure (initialState defaultSimParams)))
             | otherwise = DriveTarget (counted n) Nothing
  a <- runRIO (RunEnv target t channel ctx defaultRunOptions (mkLogFunc (\_ _ _ _ -> pure ()))) s
  (,,) a <$> readIORef n <*> entries t

-- | A response holding a ComPacket header: Length bytes of payload,
--   OutstandingData, MinTransfer.
answerOf :: Int -> Word32 -> Word32 -> Response
answerOf = answerOn 0x1004 0

-- | The same of a ComID and extension.
answerOn :: Word16 -> Word16 -> Int -> Word32 -> Word32 -> Response
answerOn c ext l o m = Response Good (either (error . show) id (encodeRecord (ComPacket (ComPacketHeader c ext o m) (B.replicate l 0))) <> B.replicate 64 0)

-- | A simulator's end whose IF-SENDs are answered from one list of statuses
--   and IF-RECVs from one of responses (past either, Good and nothing), the
--   base ComID 0x1004 recorded: what a script came to, every request the
--   transport saw, and the transcript.
harness :: [IfStatus] -> [Response] -> Script a -> IO (a, [Request], [TEntry])
harness sends recvs s = do
  sent <- newIORef sends
  rcvd <- newIORef recvs
  seen <- newIORef []
  t <- newTranscript
  channel <- newChannel
  ctx <- newIORef emptyRunCtx { rcBaseComId = Just 0x1004 }
  let next ref end = atomicModifyIORef' ref (\xs -> (drop 1 xs, fromMaybe end (listToMaybe xs)))
      tr = MkTransport
        { tExchange = \q -> do
            modifyIORef' seen (<> [q])
            case dir q of
              IfSend -> (`Response` mempty) <$> next sent Good
              IfRecv -> next rcvd (Response Good mempty)
        , tInject   = \_ -> pure Good
        , tUnit     = UnitBytes
        , tDescribe = Describe "tper" "a test" UnitBytes Nothing Nothing Nothing Nothing
        , tClose    = pure ()
        }
      target = SimTarget tr (SimControl (\_ -> pure ()) (pure (initialState defaultSimParams)))
  a <- runRIO (RunEnv target t channel ctx defaultRunOptions (mkLogFunc (\_ _ _ _ -> pure ()))) s
  (,,) a <$> readIORef seen <*> entries t

-- | 'harness' counting the IF-RECVs alone.
replying :: [IfStatus] -> [Response] -> Script a -> IO (a, Int, [TEntry])
replying sends recvs s = harness sends recvs s <&> \(a, qs, es) -> (a, length [ () | q <- qs, dir q == IfRecv ], es)

-- | An IF-RECV's answer on 0x1004 holding tokens in one packet of the
--   control session, or of a session, everything returned (OutstandingData
--   0), padded to the 2048 bytes asked.
controlAnswer :: ByteString -> Response
controlAnswer tokens = Response Good (padded (either (error . show) id (controlComPacket 0x1004 tokens)))

sessionAnswer :: SessionHandle -> ByteString -> Response
sessionAnswer h tokens = Response Good (padded (either (error . show) id (sessionComPacket h tokens)))

padded :: ByteString -> ByteString
padded b = b <> B.replicate (2048 - B.length b) 0

-- | The method invocations the IF-SENDs of protocol 1 among requests carry.
sentCalls :: [Request] -> [MethodCall]
sentCalls qs = [ mc | q <- qs, dir q == IfSend, proto q == 1, Right (DecodedFrame (ComPacketFrame _ ps) _ _) <- [decodeComPacket (payload q)]
                    , p <- ps, sp <- pfSubPackets p, Right mc <- [parseByteString (dataSubPacketPayload sp)] ]

-- | The Sessions (TSN, HSN) of the packets the IF-SENDs carry.
sentSessions :: [Request] -> [(Word32, Word32)]
sentSessions qs = [ (packetTSN (pfHeader p), packetHSN (pfHeader p))
                  | q <- qs, dir q == IfSend, Right (DecodedFrame (ComPacketFrame _ ps) _ _) <- [decodeComPacket (payload q)], p <- ps ]

-- | SyncSession as the TPer answers a StartSession: the HSN, the TSN, a
--   status.
syncSession :: Natural -> Natural -> StatusCode -> ByteString
syncSession hsn tsn st = generate $ invocationWith (statusValue st) uSMUID Methods.SyncSession
  { Methods.syncSessionHostSessionID = value hsn
  , Methods.syncSessionSPSessionID   = value tsn
  , Methods.syncSessionSPChallenge   = Nothing
  , Methods.syncSessionSPExchangeCert = Nothing
  , Methods.syncSessionSPSigningCert = Nothing
  , Methods.syncSessionTransTimeout  = Nothing
  , Methods.syncSessionInitialCredit = Nothing
  , Methods.syncSessionSignedHash    = Nothing
  }

-- | The session the simulator opens first: TSN 4096, HSN 1.
first' :: SessionHandle
first' = SessionHandle 0x1004 4096 1

-- | A Get result of one row's PIN column.
getResult :: ByteString -> MethodResponse
getResult pin = MethodResponse [L (List [N ((3 :: Natural) .: pin)])] 0

-- | Tokens in one packet of the control session.
framesOf :: ByteString -> [PacketFrame]
framesOf tokens = [PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader 0) tokens]]

-- | A STACK_RESET response for 0x1004, success; its pending reply; No
--   Response Available (Core Tables 36, 37 and 31).
stackResponse, stackPending, noResponseAvailable :: Response
stackResponse       = Response Good (either (error . show) id (encodeRecord (STACK_RESETCommandResponse 0x10040000 0)))
stackPending        = Response Good (either (error . show) id (encodeRecord (STACK_RESETPending 0x10040000)) <> B.replicate 4 0)
noResponseAvailable = Response Good (either (error . show) id (encodeRecord (NoResponseAvailable 0)) <> B.replicate 4 0)

isFail :: Verdict -> Bool
isFail = \case
  Fail _ -> True
  _      -> False

spec :: Spec
spec = do
  describe "attempt" $ do
    it "gives a script's own verdict" $ do
      (v, _, _) <- scripted True (attempt (pure (Skipped "why")))
      v `shouldBe` Skipped "why"
    it "fails a script whose expectation did not hold, and one that threw, naming it" $ do
      (v1, _, _) <- scripted True (attempt (unmet "not so"))
      (v2, _, _) <- scripted True (attempt (throwString "boom"))
      v1 `shouldBe` Fail ["not so"]
      v2 `shouldSatisfy` \v -> case v of
        Fail [r] -> "exception: " `T.isPrefixOf` r
        _        -> False

  describe "observe" $
    it "adds a name and a value to the case's observations, in order" $ do
      (obs, _, _) <- scripted True (observe "a" "1" >> observe "b" "2" >> (rcObservations <$> (asks reCtx >>= readIORef)))
      obs `shouldBe` [("a", "1"), ("b", "2")]

  describe "expectStatus" $ do
    it "passes the status expected, fails another, naming both" $ do
      (v1, _, _) <- scripted True (attempt (Pass <$ expectStatus "step 1" Good (Response Good mempty)))
      (v2, _, _) <- scripted True (attempt (Pass <$ expectStatus "step 2" SynchronousProtocolViolation (Response Good mempty)))
      v1 `shouldBe` Pass
      v2 `shouldBe` Fail ["step 2: good, not synchronous_protocol_violation"]
    it "on a drive, takes a transport failure for an error expected as unobservable (design 6.2)" $ do
      (v, _, _) <- scripted False (attempt (Pass <$ expectStatus "step 2" SynchronousProtocolViolation (Response (TransportFailure "0xff") mempty)))
      v `shouldSatisfy` \x -> case x of
        Unobservable _ -> True
        _              -> False

  describe "simOnly" $
    it "runs a step on the simulator alone" $ do
      (a, _, _) <- scripted True (simOnly (pure ()))
      (b, _, _) <- scripted False (simOnly (pure ()))
      (a, b) `shouldBe` (Just (), Nothing)

  describe "rawIfSend on a drive" $
    it "is stopped by the Guard (Level0Only), recorded, answered unsupported, the transport never called" $ do
      (r, reached, es) <- scripted False (rawIfSend 0x1004 512 (either (error . show) id (controlComPacket 0x1004 propertiesCall)))
      reached `shouldBe` 0
      status r `shouldSatisfy` \s -> case s of
        Unsupported _ -> True
        _             -> False
      length [ () | Blocked {} <- es ] `shouldBe` 1

  describe "the ComPackets the jig sends (Test Cases Table 6, PDF 27)" $ do
    it "call SMUID.Properties as design 11's M0 encodes it" $
      B.unpack propertiesCall `shouldBe`
        [0xF8, 0xA8, 0, 0, 0, 0, 0, 0, 0, 0xFF, 0xA8, 0, 0, 0, 0, 0, 0, 0xFF, 0x01, 0xF0, 0xF1, 0xF9, 0xF0, 0, 0, 0, 0xF1]
    it "controlComPacket: one packet of the control session, one Data subpacket, the rest zeros" $ do
      let bytes = either (error . show) id (controlComPacket 0x1004 propertiesCall)
      B.length bytes `mod` 4 `shouldBe` 0
      decodeComPacket bytes `shouldBe` Right (DecodedFrame
        (ComPacketFrame (ComPacketHeader 0x1004 0 0 0) [PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader 0) propertiesCall]])
        [] mempty)
    it "sessionComPacket: one packet of the session, its TSN then its HSN, one Data subpacket, every other field zero" $ do
      let bytes = either (error . show) id (sessionComPacket first' propertiesCall)
      B.length bytes `mod` 4 `shouldBe` 0
      decodeComPacket bytes `shouldBe` Right (DecodedFrame
        (ComPacketFrame (ComPacketHeader 0x1004 0 0 0) [PacketFrame (PacketHeader 4096 1 0 0 0) [DataSubPacket (DataSubPacketHeader 0) propertiesCall]])
        [] mempty)
    it "controlSubpackets: one control session packet of as many Data subpackets as payloads (ETC-15's n + 1)" $ do
      let bytes = either (error . show) id (controlSubpackets 0x1004 [propertiesCall, propertiesCall])
      decodeComPacket bytes `shouldBe` Right (DecodedFrame
        (ComPacketFrame (ComPacketHeader 0x1004 0 0 0)
                        [PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader 0) propertiesCall, DataSubPacket (DataSubPacketHeader 0) propertiesCall]])
        [] mempty)
    it "ETC-08's payload is the Test Cases' bytes (PDF 81): a Start List where Properties' parameter values go" $
      B.unpack etc08Payload `shouldBe`
        [0xF8, 0xA8, 0, 0, 0, 0, 0, 0, 0, 0xFF, 0xA8, 0, 0, 0, 0, 0, 0, 0xFF, 0x01, 0xF0, 0xF0, 0xF1, 0xF1, 0xF9, 0xF0, 0, 0, 0, 0xF1]

  describe "returnedOf, the host's reading of an answer" $ do
    it "decodes a SyncSession (a Session Manager response, an invocation), a Get result (a method response) and the TPer's End of Session" $ do
      let sync = syncSession 1 4096 SUCCESS
      returnedOf (framesOf sync) `shouldBe` Right [ReturnedCall (either error id (parseByteString sync))]
      returnedOf (framesOf (generate (getResult "MSID"))) `shouldBe` Right [ReturnedResults (getResult "MSID")]
      returnedOf (framesOf (generate EndOfSession)) `shouldBe` Right [ReturnedEndOfSession]
      returnedOf [] `shouldBe` Right []
    it "reads the subpackets of every packet in order" $
      returnedOf (framesOf (generate (getResult "a")) <> framesOf (generate EndOfSession))
        `shouldBe` Right [ReturnedResults (getResult "a"), ReturnedEndOfSession]
    it "refuses tokens that do not read (a TCG-reserved tag) and a response cut short" $ do
      returnedOf (framesOf "\xF4") `shouldSatisfy` isLeft
      returnedOf (framesOf (generate Call)) `shouldSatisfy` isLeft

  describe "startSession" $ do
    it "sends StartSession with HSN 1 and Write 1 (Test Cases 4.1 e and h), no authority for Anybody, and reads SyncSession back as the session" $ do
      (answer, qs, _) <- harness [Good] [controlAnswer (syncSession 1 4096 SUCCESS)] (startSession adminSPAsAnybody)
      answer `shouldBe` Right (Synced first')
      map (fromCall @Methods.StartSession) (sentCalls qs) `shouldBe`
        [Just (Right Methods.StartSession
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
          })]
      sentSessions qs `shouldBe` [(0, 0)]
      hostSessionNumber `shouldBe` 1
    it "keeps the session in the run's context" $ do
      (ctx, _, _) <- harness [Good] [controlAnswer (syncSession 1 4096 SUCCESS)] (startSession adminSPAsAnybody >> (asks reCtx >>= readIORef))
      rcSession ctx `shouldBe` Just first'
    it "sends the HostSigningAuthority and the HostChallenge where given (ETC-21/adminsp) and reads a failed SyncSession's status" $ do
      (answer, qs, _) <- harness [Good] [controlAnswer (syncSession 1 0 NOT_AUTHORIZED)]
                                 (startSession (StartArgs uSP_Admin (Just uAuthority_SID) (Just "wrong") True))
      answer `shouldBe` Right (SyncFailed NOT_AUTHORIZED)
      [ (Methods.startSessionHostSigningAuthority s, Methods.startSessionHostChallenge s) | Just (Right s) <- map (fromCall @Methods.StartSession) (sentCalls qs) ]
        `shouldBe` [(Just (value (token uAuthority_SID)), Just (value ("wrong" :: ByteString)))]
    it "reads no response as no SyncSession, and the TPer's CloseSession as an abort" $ do
      (a1, _, _) <- harness [Good] [answerOf 0 0 0] (startSession adminSPAsAnybody)
      a1 `shouldSatisfy` \case
        Right (NoSync why) -> "no response" `T.isInfixOf` why
        _                  -> False
      let close = generate (closeSessionCall (Session 0x1004 4096 1 uSP_Admin True [uAuthority_Anybody] mempty))
      (a2, _, _) <- harness [Good] [controlAnswer close] (startSession adminSPAsAnybody)
      a2 `shouldSatisfy` \case
        Right (NoSync why) -> "CloseSession" `T.isInfixOf` why
        _                  -> False
    it "gives an IF-SEND's error status as the exchange's" $ do
      (a, _, _) <- harness [SynchronousProtocolViolation] [] (startSession adminSPAsAnybody)
      a `shouldBe` Left (IfError SynchronousProtocolViolation)

  describe "getCells and endSession" $ do
    it "sends Get with the cell_block in a packet of the session, and reads the result and status" $ do
      (outcome, qs, _) <- harness [Good] [sessionAnswer first' (generate (getResult "MSID"))] (getCells first' uC_PIN_MSID (columnRange 3 3))
      outcome `shouldBe` Right (MethodOutcome [L (List [N ((3 :: Natural) .: ("MSID" :: ByteString))])] SUCCESS)
      sentSessions qs `shouldBe` [(4096, 1)]
      map (fromCall @Methods.Get) (sentCalls qs) `shouldBe` [Just (Right (Methods.Get (cellBlockValue (columnRange 3 3))))]
      map (_invokingID . _header) (sentCalls qs) `shouldBe` [uC_PIN_MSID]
    it "reads a status other than SUCCESS with an empty result, and no response or the TPer's CloseSession as no outcome" $ do
      (refused, _, _) <- harness [Good] [sessionAnswer first' (generate (MethodResponse [] (statusValue NOT_AUTHORIZED)))]
                                 (getCells first' uC_PIN_MSID (columnRange 3 3))
      refused `shouldBe` Right (MethodOutcome [] NOT_AUTHORIZED)
      (none, _, _) <- harness [Good] [answerOf 0 0 0] (getCells first' uC_PIN_MSID (columnRange 3 3))
      none `shouldSatisfy` isLeft
      let close = generate (closeSessionCall (Session 0x1004 4096 1 uSP_Admin True [] mempty))
      (aborted, _, _) <- harness [Good] [controlAnswer close] (getCells first' uC_PIN_MSID (columnRange 3 3))
      aborted `shouldSatisfy` \case
        Left (Malformed why) -> "CloseSession" `T.isInfixOf` why
        _                    -> False
    it "endSession sends a bare End of Session in a packet of the session and reads the TPer's as True, nothing as False" $ do
      (closed, qs, _) <- harness [Good] [sessionAnswer first' (generate EndOfSession)] (endSession first')
      closed `shouldBe` Right True
      [ dataSubPacketPayload sp | q <- qs, dir q == IfSend, Right (DecodedFrame (ComPacketFrame _ ps) _ _) <- [decodeComPacket (payload q)]
                                , p <- ps, sp <- pfSubPackets p ] `shouldBe` [generate EndOfSession]
      sentSessions qs `shouldBe` [(4096, 1)]
      (none, _, _) <- harness [Good] [answerOf 0 0 0] (endSession first')
      none `shouldBe` Right False
    it "endSession answered drops the session from the run's context" $ do
      (ctx, _, _) <- harness [Good, Good] [controlAnswer (syncSession 1 4096 SUCCESS), sessionAnswer first' (generate EndOfSession)]
                             (startSession adminSPAsAnybody >> endSession first' >> (asks reCtx >>= readIORef))
      rcSession ctx `shouldBe` Nothing

  describe "propertiesWith" $ do
    let reply :: [(Text, Natural)] -> Maybe [(Text, Natural)] -> ByteString
        reply tper host = generate (MethodCall (MethodHeader uSMUID uProperties)
                                               (Parameters [L (List [ N (encodeUtf8 n .: v) | (n, v) <- tper ])]
                                                           [ optionalParameter 0 (List [ N (encodeUtf8 n .: v) | (n, v) <- hs ]) | Just hs <- [host] ])
                                               0)
    it "sends HostProperties as named numbers and reads both tables back, keeping them and MaxComPacketSize in the context" $ do
      (answer, qs, _) <- harness [Good] [controlAnswer (reply [("MaxComPacketSize", 2048 :: Natural), ("MaxSessions", 1)] (Just [("MaxComPacketSize", 4096)]))]
                                 (propertiesWith (Just [("MaxComPacketSize", 4096)]))
      answer `shouldBe` Right (PropertiesAnswer [("MaxComPacketSize", 2048), ("MaxSessions", 1)] (Just [("MaxComPacketSize", 4096)]))
      map (fromCall @Methods.Properties) (sentCalls qs)
        `shouldBe` [Just (Right (Methods.Properties (Just (L (List [N (("MaxComPacketSize" :: ByteString) .: (4096 :: Natural))])))))]
      (ctx, _, _) <- harness [Good] [controlAnswer (reply [("MaxComPacketSize", 2048 :: Natural)] (Just [("MaxComPacketSize", 4096)]))]
                             (propertiesWith (Just [("MaxComPacketSize", 4096)]) >> (asks reCtx >>= readIORef))
      (rcTPerProperties ctx, rcHostProperties ctx, rcMaxComPacketSize ctx)
        `shouldBe` (Just [("MaxComPacketSize", 2048)], Just [("MaxComPacketSize", 4096)], Just 2048)
    it "properties is its TPer half, with no HostProperties sent nor answered" $ do
      (answer, qs, _) <- harness [Good] [controlAnswer (reply [("MaxComPacketSize", 2048 :: Natural)] Nothing)] properties
      answer `shouldBe` Right [("MaxComPacketSize", 2048)]
      map (fromCall @Methods.Properties) (sentCalls qs) `shouldBe` [Just (Right (Methods.Properties Nothing))]
    it "refuses an answer whose status is not SUCCESS, naming it, and one that is no Properties" $ do
      let failed = generate (MethodCall (MethodHeader uSMUID uProperties) (Parameters [L (List [])] []) (statusValue INVALID_PARAMETER))
      (a1, _, _) <- harness [Good] [controlAnswer failed] properties
      a1 `shouldBe` Left (Malformed "Properties answered INVALID_PARAMETER (12)")
      (a2, _, _) <- harness [Good] [controlAnswer (syncSession 1 4096 SUCCESS)] properties
      a2 `shouldSatisfy` isLeft

  describe "UCT-02's verdict (Test Cases PDF 32), over a TPer the simulator's parameters could not be" $ do
    let tperReply :: [(Text, Natural)] -> [(Text, Natural)] -> ByteString
        tperReply below host = generate (MethodCall (MethodHeader uSMUID uProperties)
                                                    (Parameters [L (List [ N (encodeUtf8 n .: v) | (_, n, least) <- tperExpected, let v = fromMaybe least (L.lookup n below) ])]
                                                                [optionalParameter 0 (List [ N (encodeUtf8 n .: v) | (n, v) <- host ])])
                                                    0)
    it "fails a TPer property below the Test Cases' minimum, and a host property out of its range, each by its step" $ do
      (v, _, _) <- harness [Good] [controlAnswer (tperReply [("MaxAuthentications", 1)] [(n, 8192) | (n, _) <- hostProperties])] (attempt (cRun uct02))
      v `shouldBe` Fail [ "2i: MaxAuthentications = 1, less than 2"
                        , "3a: MaxComPacketSize = 8192, more than the 4096 sent", "3b: MaxPacketSize = 8192, more than the 4076 sent"
                        , "3c: MaxIndTokenSize = 8192, more than the 4040 sent" ]
    it "passes a TPer property of 0 as no limit, with the lead's observation, and a host answer within its range; fails one not answered" $ do
      (v, _, es) <- harness [Good] [controlAnswer (tperReply [("MaxSessions", 0)] hostProperties)] (attempt (cRun uct02) >>= \v -> (,) v . rcObservations <$> (asks reCtx >>= readIORef))
      fst v `shouldBe` Pass
      snd v `shouldSatisfy` elem ("2h", "MaxSessions = 0 (no limit, Core Table 167); the Test Cases' minimum is met")
      length [ () | Note _ _ <- es ] `shouldBe` 0
      (v2, _, _) <- harness [Good] [controlAnswer (tperReply [] (take 2 hostProperties))] (attempt (cRun uct02))
      v2 `shouldBe` Fail ["3c: MaxIndTokenSize not answered"]
    it "lists step 2's eleven properties as Opal 2.30 Table 17 orders them, 2a MaxComPacketSize to 2k DefSessionTimeout at least 0" $ do
      map (\(s, _, _) -> s) tperExpected `shouldBe` [ "2" <> T.singleton c | c <- ['a' .. 'k'] ]
      [ (n, least) | (_, n, least) <- tperExpected, n `elem` ["MaxComPacketSize", "MaxTransactionLimit", "DefSessionTimeout"] ]
        `shouldBe` [("MaxComPacketSize", 2048), ("MaxTransactionLimit", 1), ("DefSessionTimeout", 0)]

  describe "ETC-15's not-applicable skips (design 13.29 (b)), over a TPer the simulator's parameters could not be" $ do
    let tperWith :: [(Text, Natural)] -> Response
        tperWith props = controlAnswer (generate (MethodCall (MethodHeader uSMUID uProperties)
                                                             (Parameters [L (List [ N (encodeUtf8 n .: v) | (n, v) <- props ])] []) 0))
        skippedSaying what v = case v of
          Skipped r -> all (`T.isInfixOf` r) what
          _         -> False
    it "a MaxSubpackets of 0, no limit, which step 2 cannot exceed: Skipped (NA), after step 1's Properties alone" $ do
      (v, qs, _) <- harness [Good] [tperWith [("MaxSubpackets", 0), ("MaxComPacketSize", 2048)]] (attempt (cRun etc15))
      v `shouldSatisfy` skippedSaying ["MaxSubpackets = 0", "no limit", "not applicable"]
      length [ () | q <- qs, dir q == IfSend ] `shouldBe` 1
    it "a MaxSubpackets so large that n + 1 subpackets exceed MaxComPacketSize: Skipped, the IF-SEND would fail for its length (Core 3.3.10.7 c), nothing sent for step 2" $ do
      (v, qs, _) <- harness [Good] [tperWith [("MaxSubpackets", 100), ("MaxComPacketSize", 2048)]] (attempt (cRun etc15))
      v `shouldSatisfy` skippedSaying ["101 Properties subpackets take", "more than MaxComPacketSize 2048", "would fail for its length"]
      length [ () | q <- qs, dir q == IfSend ] `shouldBe` 1

  describe "statuses and expectations" $ do
    it "names a status by Core Table 166's row, with its value" $ do
      statusName NOT_AUTHORIZED `shouldBe` "NOT_AUTHORIZED (1)"
      statusName SUCCESS `shouldBe` "SUCCESS (0)"
      statusName (StatusCode 0x0b) `shouldBe` "status 11"
    it "expectSyncStatus: a session open where SUCCESS is expected, the status expected where it failed, else a failure naming both" $ do
      (v1, _, _) <- scripted True (attempt (Pass <$ expectSyncStatus "1a" SUCCESS (Synced first')))
      (v2, _, _) <- scripted True (attempt (Pass <$ expectSyncStatus "step 1" NOT_AUTHORIZED (SyncFailed NOT_AUTHORIZED)))
      (v3, _, _) <- scripted True (attempt (Pass <$ expectSyncStatus "1a" NOT_AUTHORIZED (SyncFailed SP_BUSY)))
      (v4, _, _) <- scripted True (attempt (Pass <$ expectSyncStatus "1a" NOT_AUTHORIZED (Synced first')))
      (v1, v2) `shouldBe` (Pass, Pass)
      v3 `shouldBe` Fail ["1a: SyncSession answered SP_BUSY (3), not NOT_AUTHORIZED (1)"]
      v4 `shouldSatisfy` isFail
    it "expectSyncStatus accepts a session abort (no SyncSession) where an error status is expected, noted (Test Cases 7.1, PDF 76), not where SUCCESS is" $ do
      (v1, _, es) <- scripted True (attempt (Pass <$ expectSyncStatus "step 1" NOT_AUTHORIZED (NoSync "no response")))
      v1 `shouldBe` Pass
      [ t | Note _ t <- es ] `shouldSatisfy` any ("accepted as a session abort" `T.isInfixOf`)
      (v2, _, _) <- scripted True (attempt (Pass <$ expectSyncStatus "1a" SUCCESS (NoSync "no response")))
      v2 `shouldBe` Fail ["1a: no SyncSession: no response"]
    it "expectMethodStatus by name" $ do
      (v1, _, _) <- scripted True (attempt (Pass <$ expectMethodStatus "1b" SUCCESS (MethodOutcome [] SUCCESS)))
      (v2, _, _) <- scripted True (attempt (Pass <$ expectMethodStatus "1b" SUCCESS (MethodOutcome [] NOT_AUTHORIZED)))
      v1 `shouldBe` Pass
      v2 `shouldBe` Fail ["1b: the method answered NOT_AUTHORIZED (1), not SUCCESS (0)"]
    it "expectNoResponse passes no packets alone" $ do
      (v1, _, _) <- scripted True (attempt (Pass <$ expectNoResponse "step 2" (Right [])))
      (v2, _, _) <- scripted True (attempt (Pass <$ expectNoResponse "step 2" (Right (framesOf propertiesCall))))
      (v3, _, _) <- scripted True (attempt (Pass <$ expectNoResponse "step 2" (Left TimedOut)))
      v1 `shouldBe` Pass
      v2 `shouldSatisfy` isFail
      v3 `shouldBe` Fail ["step 2: no answer within the host's timeout"]
    it "opened gives the session SUCCESS opened and stops on anything else" $ do
      (h, _, _) <- scripted True (opened "1a" (Synced first'))
      h `shouldBe` first'
      (v, _, _) <- scripted True (attempt (Pass <$ opened "1a" (SyncFailed SP_BUSY)))
      v `shouldSatisfy` isFail

  describe "stackReset (Core 3.3.4.7.5, Tables 35-37)" $ do
    it "sends Table 35's request on protocol 2 for the ComID's Extended ComID and reads Table 36's response: Success/Failure" $ do
      (r, qs, _) <- harness [Good] [stackResponse] (stackReset 0x1004)
      r `shouldBe` Right 0
      map (\q -> (dir q, proto q, comId q)) qs `shouldBe` [(IfSend, 2, 0x1004), (IfRecv, 2, 0x1004)]
      [ fmap drRecord (decodeRecordStrict (payload q)) | q <- qs, dir q == IfSend ] `shouldBe` [Right (STACK_RESETCommandRequest 0x10040000)]
    it "asks again while the reset is pending, and reads No Response Available, another ComID's response, and an error status as none" $ do
      (r1, qs1, _) <- harness [Good] [stackPending, stackResponse] (stackReset 0x1004)
      r1 `shouldBe` Right 0
      length [ () | q <- qs1, dir q == IfRecv ] `shouldBe` 2
      (r2, _, _) <- harness [Good] [noResponseAvailable] (stackReset 0x1004)
      r2 `shouldSatisfy` either ("No Response Available" `T.isInfixOf`) (const False)
      (r3, _, _) <- harness [Good] [Response Good (either (error . show) id (encodeRecord (STACK_RESETCommandResponse 0x10050000 0)))] (stackReset 0x1004)
      r3 `shouldSatisfy` isLeft
      (r4, _, _) <- harness [OtherInvalidCommandParameter] [] (stackReset 0x1004)
      r4 `shouldSatisfy` either ("other_invalid_command_parameter" `T.isInfixOf`) (const False)
    it "on a drive, is stopped by the Guard (Level0Only) before the transport, and says so" $ do
      (r, reached, es) <- scripted False (stackReset 0x1004)
      reached `shouldBe` 0
      r `shouldSatisfy` either ("blocked by the Guard" `T.isInfixOf`) (const False)
      length [ () | Blocked {} <- es ] `shouldBe` 1

  describe "what is read from the simulator" $ do
    it "opalV2Of reads its Opal SSC V2 descriptor: base ComID 0x1004, the Initial C_PIN_SID PIN Indicator 0" $ do
      let l0 = either (error . show) id (splitLevel0 (level0Response (initialState defaultSimParams) 2048))
      fmap level0DiscoveryOpalSSCV2FeatureBaseComID (opalV2Of l0) `shouldBe` Just 0x1004
      fmap level0DiscoveryOpalSSCV2FeatureInitialC_PIN_SIDPINIndicator (opalV2Of l0) `shouldBe` Just 0
    it "simCredential reads SID's: the MSID's PIN by default, the parameters' when given, with TryLimit and Tries; none for Anybody" $ do
      simCredential uAuthority_SID (initialState defaultSimParams) `shouldBe` Just (Credential (spMsid defaultSimParams) 0 0)
      simCredential uAuthority_SID (initialState defaultSimParams { spSidPin = Just "SID!", spSidTryLimit = 3 }) `shouldBe` Just (Credential "SID!" 3 0)
      simCredential uAuthority_Anybody (initialState defaultSimParams) `shouldBe` Nothing
    it "peekSim gives the state on the simulator alone" $ do
      (a, _, _) <- scripted True peekSim
      (b, _, _) <- scripted False peekSim
      (isJust a, isJust b) `shouldBe` (True, False)

  describe "withSession" $ do
    it "opens the session, runs the step, ends the session: two IF-SENDs and a pass" $ do
      (v, qs, _) <- harness [Good, Good] [controlAnswer (syncSession 1 4096 SUCCESS), sessionAnswer first' (generate EndOfSession)]
                            (withSession adminSPAsAnybody (\h -> pure (if h == first' then Pass else Fail ["another session"])))
      v `shouldBe` Pass
      sentSessions qs `shouldBe` [(0, 0), (4096, 1)]
    it "on a step's failure on the simulator, still ends the session, then STACK_RESET on its ComID, noted" $ do
      (v, qs, es) <- harness [Good, Good, Good]
                             [controlAnswer (syncSession 1 4096 SUCCESS), sessionAnswer first' (generate EndOfSession), stackResponse]
                             (withSession adminSPAsAnybody (\_ -> unmet "not so"))
      v `shouldBe` Fail ["not so"]
      map (\q -> (dir q, proto q)) qs `shouldBe` [(IfSend, 1), (IfRecv, 1), (IfSend, 1), (IfRecv, 1), (IfSend, 2), (IfRecv, 2)]
      [ t | Note _ t <- es ] `shouldSatisfy` any ("STACK_RESET on 0x1004: Success/Failure 0" `T.isInfixOf`)
    it "fails where the TPer does not answer the End of Session, and STACK_RESETs" $ do
      (v, qs, _) <- harness [Good, Good, Good] [controlAnswer (syncSession 1 4096 SUCCESS), answerOf 0 0 0, stackResponse]
                            (withSession adminSPAsAnybody (\_ -> pure Pass))
      v `shouldBe` Fail ["End of Session: the TPer did not answer the End of Session with its own (Core 3.2.2.3.3.3, PDF 37)"]
      length [ () | q <- qs, proto q == 2 ] `shouldBe` 2
    it "stops where the startup fails or goes unanswered" $ do
      (v1, _, _) <- harness [Good] [controlAnswer (syncSession 1 0 SP_BUSY)] (attempt (withSession adminSPAsAnybody (\_ -> pure Pass)))
      v1 `shouldBe` Fail ["StartSession: SyncSession answered SP_BUSY (3), not SUCCESS (0)"]
      (v2, _, _) <- harness [Good] [answerOf 0 0 0] (attempt (withSession adminSPAsAnybody (\_ -> pure Pass)))
      v2 `shouldSatisfy` isFail

  describe "Table 45" $ do
    it "reads an IF-RECV's header as its Length, OutstandingData and MinTransfer" $
      map t45Of [answerOf 0 1 0, answerOf 40 0 0, answerOf 0 100 128] `shouldBe` map Right [(0, 1, 0), (40, 0, 0), (0, 100, 128)]
    it "matches the generated table's fields by their names, and MaxComPacketSize is Core Table 167's" $ do
      fields table45 `shouldBe` ["Length", "OutstandingData", "MinTransfer"]
      maxComPacketSizeName `shouldSatisfy` (`elem` map propertyName P.properties)
    it "reads an answer of a static ComID as such, and refuses another ComID's or extension's (Core 3.3.10.5)" $ do
      t45On 0x1004 (answerOf 40 0 0) `shouldBe` Right (40, 0, 0)
      t45On 0x1005 (answerOf 40 0 0) `shouldSatisfy` isLeft
      t45On 0x1004 (answerOn 0x1004 0x1234 0 0 0) `shouldSatisfy` isLeft
      (v, _, _) <- scripted True (attempt (Pass <$ expectT45 "an IF-RECV" 0x1005 AllReturnedNoData (answerOf 0 0 0)))
      v `shouldSatisfy` \x -> case x of
        Fail [r] -> "ComID 0x1004, not 0x1005" `T.isInfixOf` r
        _        -> False
    it "names each answer's row, in the order the simulator reads the generated rows" $
      map t45RowOf [(0, 1, 0), (0, 100, 128), (40, 100, 128), (40, 1, 0), (40, 0, 0), (0, 0, 0), (0, 0, 7)]
        `shouldBe` [ Just ResponsesToCome, Just InsufficientLength, Just AdditionalAvailable, Just AdditionalToCome
                   , Just AllReturnedWithData, Just AllReturnedNoData, Nothing ]

  describe "drain" $ do
    it "takes what Core 3.3.10.2.1 3.d.ii allows an SSC, no row of Table 45 though they are: (100, 500, 0), (100, 0, 50)" $
      forM_ [answerOf 100 500 0, answerOf 100 0 50] $ \first'' -> do
        (v, recvs, _) <- replying [] [first'', answerOf 0 0 0] (attempt (Pass <$ drain 0x1004))
        (v, recvs) `shouldBe` (Pass, 2)
    it "asks again at the MinTransfer, and refuses one no larger with no packets" $ do
      (v1, recvs, _) <- replying [] [answerOf 0 300 4096, answerOf 300 0 0] (attempt (Pass <$ drain 0x1004))
      (v1, recvs) `shouldBe` (Pass, 2)
      (v2, _, _) <- replying [] [answerOf 0 100 100] (attempt (Pass <$ drain 0x1004))
      v2 `shouldSatisfy` \x -> case x of
        Fail [r] -> "no row of Table 45" `T.isInfixOf` r
        _        -> False
    it "refuses an answer of another ComID" $ do
      (v, _, _) <- replying [] [answerOn 0x1005 0 0 0 0] (attempt (Pass <$ drain 0x1004))
      v `shouldSatisfy` isFail

  describe "ETC-02" $
    it "passes on its two steps alone, its drain after them outside its verdict, a drain that fails said in a note" $ do
      (v, _, es) <- replying [Good, SynchronousProtocolViolation] [Response Good "not a ComPacket"] (attempt (cRun etc02))
      v `shouldBe` Pass
      [ t | Note _ t <- es, "ETC-02's drain of 0x1004 after its steps, outside its verdict" `T.isPrefixOf` t ] `shouldSatisfy` (not . null)
