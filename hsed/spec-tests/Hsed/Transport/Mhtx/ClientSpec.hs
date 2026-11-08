{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-|
Module      : Hsed.Transport.Mhtx.ClientSpec
Description : Test specifications for Hsed.Transport.Mhtx.Client over memory, against a scripted hub
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The client over "Hsed.Transport.Mhtx.Conn".'memoryPair' against a scripted hub double, then
the real hub over memory (design 11 M5c, group G3's acceptance): open refuses no hello in time,
@v@ other than 1, an @app.sed@ that does not read, a role other than @tper@, unknown units, a
close before hello and a dial refused; ids 1, 2, 3 ..., none reused after a reconnect, each
registered before its write (answers given at once never lost); an unknown id, a @sed.rsp@ that
carries @seq@ and a @sed.evt@ never taken for an answer; a timeout a transport failure, the
entry gone however the wait ends (an asynchronous exception inside @mask_@ among them), a late
answer ignored; a dropped socket fails what it carried, none re-sent; a request issued while
disconnected sent after a reconnect judged none, refused naming a restart or broadcasts missed
otherwise, each noticed; a changed @app.sed@ latches the client; pings every interval, any
message proof of life, silence past the interval and the grace a reconnect; 'backoffDelay' and
'judgeMissed' as mh_transport's Dart (backoff.dart:27-35, client.dart:397-406, SPEC.md
L81-85); the transport as the in-process one (a length past what the unit carries refused with
nothing sent, a protocol error a transport failure, the reset op's answer, unsupported
included, its description and unit from @app.sed@); the view and the control as the hub's
peek and release. Every wait is the test's (the options' microseconds), none over about 10 ms
in an example that passes, so that hsed-test keeps its budget (design 13.34). No socket is
opened: memory alone.

The examples over memory need group G1's frames and connections, and those against the real
hub group G2's hub: until G3 rebases onto them (M5c's merge order G1, G4, G2, G3) each is
pending, naming the group ('awaiting').
-}
module Hsed.Transport.Mhtx.ClientSpec (spec) where

import           Control.Monad              (replicateM)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.KeyMap          as KM
import qualified RIO.ByteString             as B
import           RIO
import           RIO.List                   (sort)
import qualified RIO.Text                   as T
import           Test.Hspec

import           Hsed.Gen.Core.UIDs         (uLocking_GlobalRange, uSP_Admin)
import           Hsed.Sim.Types             (defaultSimParams)
import           Hsed.Sim.View              (ComIdView (..), Control (..), SimQuery (..), SimView (..), SomeQuery (..), setupOf)
import           Hsed.Transport.InProc      (InProcOpts (..), inProcView)
import           Hsed.Transport.Mhtx.Client
import           Hsed.Transport.Mhtx.Conn   (Conn (..), Incoming (..), codeBadToken, codeGoingAway, codeHeartbeat, memoryPair)
import           Hsed.Transport.Mhtx.Frame  (Allow (..), FromClient (..), Hello (..), Op (..), Reply (..), SedApp (..), SedEvt (..),
                                             SedReq (..), SedRsp (..), answerValue, evtText, fromClient, helloText, rspText,
                                             sedAppValue)
import           Hsed.Transport.Mhtx.Hub    (Backend (..), HubOpts (..), backendOf, defaultHubOpts, hubServe, newHub)
import           Hsed.Wire                  (Describe (..), Event (..), IfStatus (..), Response (..), Unit (..), describeTransport,
                                             ifRecv, ifSend, inject, mostCarried, unitOf)


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Client" $ do
  describe "backoffDelay (mh_transport's backoff.dart:27-35)" $ do
    it "is backoff.dart's over attempts 0-8 at draws 0, 0.5 and 0.999: 1 s doubling to a 30 s cap, half of it the draw's" $
      [ [ backoffDelay defaultBackoff n r | n <- [0 .. 8] ] | r <- [0, 0.5, 0.999] ] `shouldBe` dartDelays
    it "rounds as Dart's round does, a half away from zero" $
      map (\n -> backoffDelay (Backoff 1 2 30 0.5) n 0) [0 .. 5] `shouldBe` [1, 1, 2, 4, 8, 15]

  describe "judgeMissed (client.dart:397-406)" $
    it "is SPEC.md L81-85's table: the same server_id and seq none, a greater seq some; another server_id, a first connection or no hello unknown" $ do
      -- hellos whose app object is empty: the verdict reads the server id and the seq alone
      let seen = Just ("k3v9x2m8p1qz", 42)
          at s sid = Just (Hello 1 sid s 20000 60000 KM.empty)
      [ judgeMissed True seen (at 42 "k3v9x2m8p1qz"), judgeMissed True seen (at 43 "k3v9x2m8p1qz")
        , judgeMissed True seen (at 42 "a0b1c2d3e4f5"), judgeMissed True seen (at 41 "k3v9x2m8p1qz")
        , judgeMissed False seen (at 42 "k3v9x2m8p1qz"), judgeMissed True seen Nothing, judgeMissed True Nothing (at 42 "k3v9x2m8p1qz") ]
        `shouldBe` [MissedNone, MissedSome, MissedUnknown, MissedUnknown, MissedUnknown, MissedUnknown, MissedUnknown]

  describe "openMhtx, one attempt (SPEC.md L37, L45, L99-104)" $ do
    it "opens on a hello of v 1 whose app.sed says a TPer answers, its app.sed the client's" $ awaiting "G1" >> within (
      withClient $ \_ c _ -> clientApp c `shouldBe` tperApp)
    it "refuses no hello within the wait, v other than 1, an app.sed that does not read, role drive, unknown units, a close before hello and a refused dial, each saying why" $ awaiting "G1" >> within (do
      refusedBy (const (pure ())) "no hello within 20 ms"
      refusedBy (`sendHello` (helloOf "server-a" 0 tperApp) { helloV = 2 }) "hello's v is 2, not 1"
      refusedBy (`sendHello` (helloOf "server-a" 0 tperApp) { helloApp = KM.fromList [("sed", A.String "a TPer")] }) "hello's app.sed does not read"
      refusedBy (`sendHello` helloOf "server-a" 0 tperApp { saRole = "drive" }) "hello's app.sed.role is drive, not tper"
      refusedBy (`sendHello` (helloOf "server-a" 0 tperApp) { helloApp = KM.fromList [("sed", unitsSaid "blocks4096")] }) "hello's app.sed does not read"
      refusedBy (\hub -> connClose hub codeBadToken "token") "closed before hello with 4001"
      openMhtx (testOpts' (\_ -> pure (Left "connection refused"))) >>= \case
        Left why -> why `shouldSatisfy` T.isInfixOf "connection refused"
        Right c  -> closeMhtx c >> expectationFailure "opened on a refused dial")

  describe "requests" $ do
    it "numbers them 1, 2, 3 ..., none reused after a reconnect, each registered before its write (answers given at once are never lost)" $ awaiting "G1" >> within (
      withClient $ \hubs c hub -> do
        ids <- replicateM 20 (answeredAtOnce hub (ifRecv (clientTransport c) 1 0x1004 16))
        ids `shouldBe` [1 .. 20]
        connClose hub codeGoingAway "the double's drop"
        hub2 <- atomically (readTQueue hubs)
        sendHello hub2 (helloOf "server-a" 0 tperApp)
        answeredAtOnce hub2 (ifRecv (clientTransport c) 1 0x1004 16) `shouldReturn` 21
        clientInFlight c `shouldReturn` [])
    it "ignores an unknown id, a sed.rsp that carries seq and a sed.evt: none is taken for the answer" $ awaiting "G1" >> within (
      withClient $ \_ c hub -> do
        r <- async (ifRecv (clientTransport c) 1 0x1004 16)
        req <- requested hub
        answer hub (reqId req + 99) (Answered Good (B.replicate 16 9))
        connSendText hub ("{\"type\":\"sed.rsp\",\"id\":" <> encodeUtf8 (tshow (reqId req)) <> ",\"seq\":5,\"status\":\"good\",\"len\":0}")
        connSendText hub (evtText (SedEvt 1 PowerCycle [0x1004] []))
        answer hub (reqId req) (Answered Good (B.replicate 16 7))
        wait r `shouldReturn` Response Good (B.replicate 16 7)
        clientNotices c `shouldReturn` ["a reset by another client: power_cycle, seq 1"])
    it "fails one not answered in its wait with a transport failure, its entry gone; a late answer is ignored" $ awaiting "G1" >> within (
      withClientOpts (\o -> o { coRequestMicros = 10000 }) $ \_ c hub -> do
        r <- ifRecv (clientTransport c) 1 0x1004 16
        status r `shouldSatisfy` failedWith "no answer from memory within 10 ms: the request was sent, and is never sent again"
        clientInFlight c `shouldReturn` []
        late <- requested hub
        answer hub (reqId late) (Answered Good (B.replicate 16 1))
        next <- async (ifRecv (clientTransport c) 1 0x1004 16)
        req <- requested hub
        answer hub (reqId req) (Answered Good (B.replicate 16 2))
        wait next `shouldReturn` Response Good (B.replicate 16 2))
    it "leaves no entry when its wait is cancelled by an asynchronous exception inside mask_ (as the transcript records a command under mask_, the host's exchange its timeout)" $ awaiting "G1" >> within (
      withClient $ \_ c hub -> do
        timeout 5000 (mask_ (ifRecv (clientTransport c) 1 0x1004 16)) `shouldReturn` Nothing
        _ <- requested hub
        clientInFlight c `shouldReturn` [])

  describe "a connection lost (nothing sent is ever sent again: an IF-RECV in S3 delivers once, Hsed.Sim.Sync)" $ do
    it "fails every request it carried, IF-RECV included, with a transport failure; none is sent again after the reconnect" $ awaiting "G1" >> within (
      withClient $ \hubs c hub -> do
        let tr = clientTransport c
        carried <- traverse async [ifSend tr 1 0x1004 16 "a ComPacket", ifRecv tr 1 0x1004 16, ifRecv tr 1 0x1004 2048]
        replicateM_ 3 (requested hub)
        connClose hub codeGoingAway "the double's drop"
        for_ carried $ \r -> wait r >>= (`shouldSatisfy` failedWith "the request was sent, and is never sent again") . status
        hub2 <- atomically (readTQueue hubs)
        sendHello hub2 (helloOf "server-a" 0 tperApp)
        afterwards <- async (ifRecv tr 1 0x1004 16)
        req <- requested hub2
        (reqId req, reqOp req) `shouldBe` (4, OpIfRecv 1 0x1004 16)
        answer hub2 (reqId req) (Answered Good (B.replicate 16 0))
        wait afterwards `shouldReturn` Response Good (B.replicate 16 0))
    it "sends a request issued while disconnected once a reconnect's hello says nothing was missed (same server_id and seq)" $ awaiting "G1" >> within (
      reissued (helloOf "server-a" 0 tperApp) $ \c hub2 r -> do
        req <- requested hub2
        answer hub2 (reqId req) (Answered Good (B.replicate 16 3))
        wait r `shouldReturn` Response Good (B.replicate 16 3)
        clientNotices c `shouldReturn` []
        clientVerdicts c `shouldReturn` [MissedUnknown, MissedNone])
    it "fails a request issued while disconnected after a new server_id, naming the restart, which it notices" $ awaiting "G1" >> within (
      reissued (helloOf "server-b" 0 tperApp) $ \c _ r -> do
        let restart = "the hub restarted, server_id server-a -> server-b: a new TPer at its OFS"
        status <$> wait r `shouldReturn` TransportFailure ("not sent: " <> restart)
        clientNotices c `shouldReturn` [restart]
        clientVerdicts c `shouldReturn` [MissedUnknown, MissedUnknown])
    it "fails a request issued while disconnected after broadcasts were missed (the same server_id, a greater seq), which it notices" $ awaiting "G1" >> within (
      reissued (helloOf "server-a" 1 tperApp) $ \c _ r -> do
        let missed = "broadcasts missed, seq 0 -> 1: a reset may have happened"
        status <$> wait r `shouldReturn` TransportFailure ("not sent: " <> missed)
        clientNotices c `shouldReturn` [missed]
        clientVerdicts c `shouldReturn` [MissedUnknown, MissedSome])
    it "is latched by a reconnect whose app.sed is not the first's: every request then fails, the latch noticed, no reconnect tried" $ awaiting "G1" >> within (
      withClient $ \hubs c hub -> do
        connClose hub codeGoingAway "the double's drop"
        hub2 <- atomically (readTQueue hubs)
        sendHello hub2 (helloOf "server-a" 0 tperApp { saUnits = UnitBlocks512 })
        let latched = "the hub's app.sed changed on a reconnect (units): the client is latched, every request failing"
        connReceive hub2 >>= (`shouldSatisfy` closedWith 1008)
        status <$> ifRecv (clientTransport c) 1 0x1004 16 `shouldReturn` TransportFailure latched
        clientNotices c `shouldReturn` [latched]
        threadDelay 5000
        isNothing <$> atomically (tryReadTQueue hubs) `shouldReturn` True)

  describe "the heartbeat (SPEC.md L57-62)" $ do
    it "pings every interval after hello" $ awaiting "G1" >> within (
      withHello (helloOf "server-a" 0 tperApp) { helloIntervalMs = 2 } id $ \_ _ hub ->
        replicateM_ 3 (pinged hub))
    it "takes any message, a binary one among them, for proof of life, and drops and redials after the interval and the grace in silence" $ awaiting "G1" >> within (
      withHello (helloOf "server-a" 0 tperApp) { helloIntervalMs = 2 } (\o -> o { coGraceMicros = 10000 }) $ \hubs _ hub -> do
        -- 24 ms of messages every 3 ms, twice the 12 ms of silence that would drop it
        replicateM_ 8 (connSendBinary hub "\0" >> threadDelay 3000)
        isNothing <$> atomically (tryReadTQueue hubs) `shouldReturn` True
        untilClosed hub >>= (`shouldBe` Just (codeHeartbeat, "heartbeat timeout"))
        void (atomically (readTQueue hubs)))

  describe "the transport (as the in-process one, Hsed.Transport.InProc)" $ do
    it "refuses a length past what its unit carries Other Invalid Command Parameter, nothing sent" $ awaiting "G1" >> within (
      withClient $ \_ c hub -> do
        ifRecv (clientTransport c) 1 0x1004 (mostCarried UnitBytes + 1) `shouldReturn` Response OtherInvalidCommandParameter mempty
        answeredAtOnce hub (ifRecv (clientTransport c) 1 0x1004 16) `shouldReturn` 1)
    it "gives a protocol_error as a transport failure naming it" $ awaiting "G1" >> within (
      withClient $ \_ c hub -> do
        r <- async (ifRecv (clientTransport c) 1 0x1004 16)
        req <- requested hub
        answer hub (reqId req) (ProtocolError "len over 1 MiB")
        status <$> wait r `shouldReturn` TransportFailure "the hub's protocol_error: len over 1 MiB")
    it "causes an event as the reset op, its own sed.evt no notice, the answer's status given, unsupported with its detail" $ awaiting "G1" >> within (
      withClient $ \_ c hub -> do
        let tr = clientTransport c
        good <- async (inject tr PowerCycle)
        req <- requested hub
        reqOp req `shouldBe` OpReset PowerCycle
        connSendText hub (evtText (SedEvt 1 PowerCycle [0x1004] []))
        answer hub (reqId req) (Answered Good mempty)
        wait good `shouldReturn` Good
        refused <- async (inject tr HardwareReset)
        req2 <- requested hub
        answer hub (reqId req2) (Answered (Unsupported "hsed-sim answers reset with --allow-remote-reset alone") mempty)
        wait refused `shouldReturn` Unsupported "hsed-sim answers reset with --allow-remote-reset alone"
        clientNotices c `shouldReturn` [])
    it "describes itself and counts its unit as app.sed says, the device none" $ awaiting "G1" >> within (
      withClient $ \_ c _ -> do
        describeTransport (clientTransport c)
          `shouldBe` Describe "tper" "hsed-sim test" UnitBytes (Just "opal-2.30") (Just 0x1004) (Just 1) Nothing
        unitOf (clientTransport c) `shouldBe` UnitBytes)
    it "views the simulator as the hub's peek op and releases a ComID as its release op" $ awaiting "G1" >> within (
      withClient $ \_ c hub -> do
        let ranges = [uLocking_GlobalRange, uSP_Admin]
        viewed <- async (peekView (clientView c) LockingRanges)
        req <- requested hub
        reqOp req `shouldBe` OpPeek (SomeQuery LockingRanges)
        answer hub (reqId req) (Viewed (answerValue LockingRanges ranges))
        wait viewed `shouldReturn` Right ranges
        released <- async (controlRelease (clientControl c) 0x1004)
        req2 <- requested hub
        reqOp req2 `shouldBe` OpRelease 0x1004
        answer hub (reqId req2) (Answered Good mempty)
        wait released `shouldReturn` Right ()
        refused <- async (peekView (clientView c) Msid)
        req3 <- requested hub
        answer hub (reqId req3) (Answered (Unsupported "--allow-remote-control") mempty)
        wait refused `shouldReturn` Left "peek: unsupported: --allow-remote-control")

  describe "against the real hub (Hsed.Transport.Mhtx.Hub.hubServe over memory)" $
    it "answers as its backend answers in process: its description, what it allows, Level 0 Discovery, the ComIDs viewed" $ awaiting "G1 and G2" >> slowly (do
      let hubOpts = defaultHubOpts { hoAllowControl = True, hoAllowReset = True }
      backend <- backendOf hubOpts defaultSimParams (InProcOpts UnitBytes 0) >>= either (throwString . T.unpack) pure
      hub <- newHub hubOpts backend
      let served :: Dialer
          served act = memoryPair >>= \(client, server) -> withAsync (hubServe hub Nothing server) (\_ -> Right <$> act client)
      bracket (openMhtx (testOpts' served) >>= either (throwString . T.unpack) pure) closeMhtx $ \c -> do
        let tr = clientTransport c
        describeTransport tr `shouldBe` describeTransport (beTransport backend)
        sort (saAllows (clientApp c)) `shouldBe` [AllowPeek, AllowRelease, AllowReset]
        -- reads alone, so that the backend in process answers as it answered the hub
        ifRecv tr 1 1 2048 `shouldReturnSame` ifRecv (beTransport backend) 1 1 2048
        (fmap (map cvComId) <$> peekView (clientView c) ComIds)
          `shouldReturnSame` (fmap (map cvComId) <$> peekView (inProcView (beControl backend)) ComIds))

-- | Pending until the groups named merge (M5c's merge order G1, G4, G2, G3): the example is written against their
--   signatures and runs once G3 rebases onto them.
awaiting :: String -> Expectation
awaiting groups = pendingWith ("awaiting group " <> groups <> "'s merge (M5c): written against its signatures, run once G3 rebases onto it")

-- | backoff.dart:27-35's delays over attempts 0-8, at draws 0, 0.5 and 0.999 (1 s doubling to 30 s, jitter 0.5),
--   reckoned in IEEE doubles and rounded as Dart rounds, a half away from zero.
dartDelays :: [[Int]]
dartDelays =
  [ [500000, 1000000, 2000000, 4000000, 8000000, 15000000, 15000000, 15000000, 15000000]
  , [750000, 1500000, 3000000, 6000000, 12000000, 22500000, 22500000, 22500000, 22500000]
  , [999500, 1999000, 3998000, 7996000, 15992000, 29985000, 29985000, 29985000, 29985000]
  ]

-- | A hub's app.sed for the double: a TPer in bytes, everything allowed, the default setup.
tperApp :: SedApp
tperApp = SedApp
  { saRole      = "tper"
  , saImpl      = "hsed-sim test"
  , saUnits     = UnitBytes
  , saSsc       = Just "opal-2.30"
  , saErrata    = Just "scott"
  , saBaseComId = Just 0x1004
  , saNComIds   = Just 1
  , saAllows    = [minBound .. maxBound]
  , saSetup     = Just (setupOf defaultSimParams 0)
  }

-- | The double's hello: its server id, its seq, an app.sed; a heartbeat of 10 s, which no example that does not set
--   its own reaches.
helloOf :: Text -> Integer -> SedApp -> Hello
helloOf serverId s app = Hello 1 serverId s 10000 60000 (KM.fromList [("sed", sedAppValue app)])

-- | tperApp's app.sed with its units said otherwise.
unitsSaid :: Text -> A.Value
unitsSaid word = case sedAppValue tperApp of
  A.Object o -> A.Object (KM.insert "units" (A.String word) o)
  other      -> other

-- | The client's options over a dialer: waits of milliseconds, a backoff of 1 ms doubling to 4 ms, no jitter.
testOpts' :: Dialer -> ClientOpts
testOpts' dialer = ClientOpts
  { coDial                = dialer
  , coName                = "memory"
  , coConnectMicros       = 200000
  , coHelloMicros         = 20000
  , coRequestMicros       = 200000
  , coGraceMicros         = 10000000
  , coReconnectWaitMicros = 200000
  , coBackoff             = Backoff 1000 2 4000 0
  }

-- | Connections over memory: each dial a 'memoryPair', its hub's side given to the test in order, the client's
--   action run on the other.
memoryDialer :: TQueue Conn -> Dialer
memoryDialer hubs act = do
  (client, hub) <- memoryPair
  atomically (writeTQueue hubs hub)
  Right <$> act client

-- | A client open on the double's hello, the test given the connections to come, the client and the first
--   connection's hub side; closed after.
withClient :: (TQueue Conn -> MhtxClient -> Conn -> IO a) -> IO a
withClient = withHello (helloOf "server-a" 0 tperApp) id

-- | 'withClient' with its options changed.
withClientOpts :: (ClientOpts -> ClientOpts) -> (TQueue Conn -> MhtxClient -> Conn -> IO a) -> IO a
withClientOpts = withHello (helloOf "server-a" 0 tperApp)

-- | 'withClient' on a hello and options given.
withHello :: Hello -> (ClientOpts -> ClientOpts) -> (TQueue Conn -> MhtxClient -> Conn -> IO a) -> IO a
withHello hello changed act = do
  hubs <- newTQueueIO
  opening <- async (openMhtx (changed (testOpts' (memoryDialer hubs))))
  hub <- atomically (readTQueue hubs)
  sendHello hub hello
  wait opening >>= either (throwString . T.unpack) (\c -> act hubs c hub `finally` closeMhtx c)

-- | An open refused: the double's first connection given to an action (a hello, a close, nothing), the refusal saying
--   what is given.
refusedBy :: (Conn -> IO ()) -> Text -> Expectation
refusedBy onHub reason = do
  hubs <- newTQueueIO
  opening <- async (openMhtx (testOpts' (memoryDialer hubs)))
  atomically (readTQueue hubs) >>= onHub
  wait opening >>= \case
    Left why -> why `shouldSatisfy` T.isInfixOf reason
    Right c  -> closeMhtx c >> expectationFailure ("opened, where " <> T.unpack reason <> " was to refuse it")

-- | The double's first connection dropped, a request issued while the client redials (its wait, 200 ms), then the
--   hello given on the next connection; the test given the client, that connection and the request.
reissued :: Hello -> (MhtxClient -> Conn -> Async Response -> IO a) -> IO a
reissued hello act = withClient $ \hubs c hub -> do
  connClose hub codeGoingAway "the double's drop"
  hub2 <- atomically (readTQueue hubs)
  r <- async (ifRecv (clientTransport c) 1 0x1004 16)
  -- the request waiting for the reconnect before the hello is given (a request issued once the reconnect is taken
  -- is sent on it, whatever its hello said)
  threadDelay 5000
  sendHello hub2 hello
  act c hub2 r

sendHello :: Conn -> Hello -> IO ()
sendHello hub = connSendText hub . helloText

-- | An answer to a request's id.
answer :: Conn -> Integer -> Reply -> IO ()
answer hub rid = connSendText hub . rspText . SedRsp (Just rid)

-- | The next request the client sent, pings and what a hub ignores passed over.
requested :: Conn -> IO SedReq
requested hub = connReceive hub >>= \case
  InClosed c -> throwString ("the client closed the connection: " <> show c)
  incoming   -> case fromClient incoming of
    ClientReq r -> pure r
    _           -> requested hub

-- | A command issued and answered as soon as the double reads it, sixteen zeros; its id.
answeredAtOnce :: Conn -> IO Response -> IO Integer
answeredAtOnce hub command = do
  r <- async command
  req <- requested hub
  answer hub (reqId req) (Answered Good (B.replicate 16 0))
  wait r `shouldReturn` Response Good (B.replicate 16 0)
  pure (reqId req)

-- | The next message, which must be a ping.
pinged :: Conn -> IO ()
pinged hub = connReceive hub >>= \incoming -> fromClient incoming `shouldBe` ClientPing

-- | The connection's close, what came before it passed over: its code and reason.
untilClosed :: Conn -> IO (Maybe (Word16, Text))
untilClosed hub = connReceive hub >>= \case
  InClosed c -> pure c
  _          -> untilClosed hub

-- | Whether a message is the close of a code.
closedWith :: Word16 -> Incoming -> Bool
closedWith code = \case
  InClosed (Just (c, _)) -> c == code
  _                      -> False

-- | Whether a status is a transport failure whose text says what is given.
failedWith :: Text -> IfStatus -> Bool
failedWith reason = \case
  TransportFailure why -> reason `T.isInfixOf` why
  _                    -> False

-- | Two actions' results equal.
shouldReturnSame :: (Eq a, Show a) => IO a -> IO a -> Expectation
shouldReturnSame a b = join (shouldBe <$> a <*> b)

-- | An example's body within a second: a client that hangs fails its example, not the suite.
within :: IO a -> IO a
within act = timeout 1000000 act >>= maybe (throwString "not done within 1 s") pure

-- | 'within' 10 s, for an example that folds a simulator's state first.
slowly :: IO a -> IO a
slowly act = timeout 10000000 act >>= maybe (throwString "not done within 10 s") pure
