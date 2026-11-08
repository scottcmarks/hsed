{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.HubSpec
Description : Test specifications for Hsed.Transport.Mhtx.Hub over memory, its clock injected
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The hub's rules held without a socket (design 11 M5c, group G2's
acceptance). hsed-sim's command line: 'parseListen' takes 127.0.0.1:PORT
alone and refuses the wildcard address, IPv6 and every name (Scott's answer
Q1; written here, which CapabilitySpec does not scan, since no module of src,
app or lan-tests may write the wildcard address); the heartbeat it can run;
the files it reads, regular files outside /dev, the token a file's bytes,
one line ending left out. The backend's @app.sed@ and what each flag allows;
a hub with no peer, its server id, its @seq@ advancing with none connected,
the broadcasts it refuses.

Over memory ("Hsed.Transport.Mhtx.Conn".'memoryPair', group G1's), its clock
a variable the test sets ('Clock') and its TPer the real in-process
simulator: hello first; seq shared and never skipped or repeated, however
peers attach while broadcasts run; pong for every ping, even while a request
is held; liveness from any message, junk and binary included, and the
sweep's 4000; requests in arrival order on their own socket with no seq,
protocol_error, ids; two sockets on one ComID; the reset op and its
broadcast before its response; the control ops; the token's 4001; a full
queue's 1008; hubClose's 1001, the hub serving on; and IF-SEND and IF-RECV
answered as the in-process transport answers them.
-}
module Hsed.Transport.Mhtx.HubSpec (spec) where

import qualified Data.Aeson                as A
import qualified Data.Aeson.KeyMap         as KeyMap
import qualified RIO.ByteString            as B
import qualified RIO.Char                  as C
import qualified RIO.List                  as L
import           RIO
import           RIO.Directory             (createDirectory, createFileLink)
import           RIO.FilePath              ((</>))
import qualified RIO.Text                  as T
import           Test.Hspec
import           Test.QuickCheck           (forAll, ioProperty, listOf1, withMaxSuccess, (===))

import           Hsed.Core.StreamItem      (StreamItem (generate))
import           Hsed.Core.Table           (TableDesc (..))
import           Hsed.Gen.Core.TableDescs  (authorityTableDesc)
import           Hsed.Gen.Core.UIDs        (uAuthority_SID, uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Script           (StartArgs (..), controlComPacket, dataStoreTable, propertiesCall, startSessionCall)
import           Hsed.Sim                  (syncOf)
import           Hsed.Sim.Gen              (foldedAt, genStaticRequest)
import           Hsed.Sim.Types            (LifeCycle (..), SimParams (..), SyncState (..), defaultSimParams)
import           Hsed.Sim.View             (SimQuery (..), SimSetup (..), SomeQuery (..), answer, setupOf)
import           Hsed.Transport.InProc     (InProcOpts (..), SimControl (..))
import           Hsed.Transport.Mhtx.Conn  (Conn (..), Incoming (..), memoryPair)
import           Hsed.Transport.Mhtx.Frame (Allow (..), FromServer (..), Hello (..), Op (..), Reply (..), SedApp (..), SedEvt (..),
                                            SedReq (..), SedRsp (..), fromServer, parseAnswer, pingText, reqText, sedAppOf)
import           Hsed.Transport.Mhtx.Hub
import           Hsed.Version              (hsedVersion)
import           Hsed.Wire                 (Cap (..), Dir (..), Event (..), IfStatus (..), Request (..), Response (..), Unit (..), ifRecv,
                                            ifSend)
import           Hsed.Wire.Internal        (Transport (..))


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Hub" $ do
  describe "hsed-sim's command line (Scott's answer Q1: 127.0.0.1 alone)" $ do
    it "listens on 127.0.0.1:PORT, PORT 0 to 65535 in decimal, 0 an ephemeral port" $
      map parseListen ["127.0.0.1:0", "127.0.0.1:1", "127.0.0.1:443", "127.0.0.1:65535"] `shouldBe` map Right [0, 1, 443, 65535]
    it "refuses the wildcard address, IPv6, a name, another address, no port, a port out of range or not in decimal" $
      [ s | s <- [ "0.0.0.0:0", "0.0.0.0:7000", "[::]:0", ":::0", "::1:80", "[::1]:80", "localhost:80", "127.0.0.2:80"
                 , "127.1:80", "192.0.2.1:80", "127.0.0.1", "127.0.0.1:", ":80", "", "127.0.0.1:65536", "127.0.0.1:123456"
                 , "127.0.0.1:-1", "127.0.0.1:+80", "127.0.0.1:0x50", " 127.0.0.1:80", "127.0.0.1:80 " ]
          , isRight (parseListen s) ] `shouldBe` []
    it "names the host it refuses and the port it cannot read" $ do
      parseListen "0.0.0.0:80" `shouldSatisfy` either (\e -> "host \"0.0.0.0\"" `L.isPrefixOf` e && "127.0.0.1 alone" `L.isInfixOf` e) (const False)
      parseListen "127.0.0.1:99999" `shouldSatisfy` either ("port \"99999\"" `L.isPrefixOf`) (const False)
    it "runs a heartbeat whose interval is positive and whose timeout is above it" $ do
      map (uncurry heartbeatProblems) [(200, 600), (20000, 60000), (1, 2)] `shouldBe` [[], [], []]
      map (length . uncurry heartbeatProblems) [(0, 600), (-200, 600), (600, 600), (600, 200), (0, 0)] `shouldBe` [1, 1, 1, 1, 2]

  describe "the files hsed-sim reads (--profile, --token-file)" $ do
    it "reads a regular file, or a link to one, outside /dev" $
      withSystemTempDirectory "hsed-hub-files" $ \tmp -> do
        writeFileBinary (tmp </> "token") "s3cret\n"
        createFileLink (tmp </> "token") (tmp </> "link")
        traverse (plainFileProblem "--token-file") [tmp </> "token", tmp </> "link"] `shouldReturn` [Nothing, Nothing]
    it "refuses a directory, a missing file, /dev itself and what lies under it, and a link into it, opening none" $
      withSystemTempDirectory "hsed-hub-files" $ \tmp -> do
        createDirectory (tmp </> "d")
        createFileLink "/dev/null" (tmp </> "null")
        problems <- traverse (plainFileProblem "--profile") [tmp </> "d", tmp </> "missing", "/dev", "/dev/null", tmp </> "null"]
        map (fmap ("--profile " `T.isPrefixOf`)) problems `shouldBe` replicate 5 (Just True)
        [ p | Just p <- drop 2 problems, not ("under /dev" `T.isInfixOf` p) ] `shouldBe` []
    it "takes a token file's bytes, one LF or CRLF at their end left out, and refuses an empty or unreadable one" $
      withSystemTempDirectory "hsed-hub-token" $ \tmp -> do
        let tokens = [ ("lf", "s3cret\n"), ("crlf", "s3cret\r\n"), ("none", "s3cret"), ("two", "s3cret\n\n"), ("spaces", " s3 cret \n")
                     , ("empty", ""), ("newline", "\n"), ("crlf-alone", "\r\n") ]
        for_ tokens $ \(name, contents) -> writeFileBinary (tmp </> name) contents
        read' <- traverse (readTokenFile . (tmp </>) . fst) tokens
        map (either (const Nothing) Just) read' `shouldBe` [Just "s3cret", Just "s3cret", Just "s3cret", Just "s3cret\n", Just " s3 cret "
                                                             , Nothing, Nothing, Nothing]
        readTokenFile (tmp </> "missing") >>= (`shouldSatisfy` isLeft)

  describe "the backend" $ do
    it "says in app.sed what answers: a TPer, hsed-sim and the version, bytes, Opal 2.30, scott, ComID 0x1004, one of them" $ do
      be <- backend defaultHubOpts bytes
      beApp be `shouldBe` SedApp "tper" ("hsed-sim " <> hsedVersion) UnitBytes (Just "opal-2.30") (Just "scott") (Just 0x1004) (Just 1) []
                                 (Just (setupOf defaultSimParams 0) { ssSidPin = Nothing, ssMsid = Nothing })
    it "allows peek and release with control and reset with reset, and publishes the MSID's digest with control alone" $
      for_ [(False, False), (True, False), (False, True), (True, True)] $ \(control, reset) -> do
        be <- backend defaultHubOpts { hoAllowControl = control, hoAllowReset = reset } bytes
        saAllows (beApp be) `shouldBe` [ a | (a, granted) <- [(AllowPeek, control), (AllowRelease, control), (AllowReset, reset)], granted ]
        (ssMsid <$> saSetup (beApp be)) `shouldBe` Just (if control then ssMsid (setupOf defaultSimParams 0) else Nothing)
    it "takes its unit and hold from the transport's options" $ do
      be <- backend defaultHubOpts (InProcOpts UnitBlocks512 2)
      (saUnits (beApp be), saSetup (beApp be)) `shouldBe` (UnitBlocks512, Just (setupOf defaultSimParams { spUnits = UnitBlocks512 } 2)
                                                                                { ssSidPin = Nothing, ssMsid = Nothing })
    it "refuses parameters out of range" $
      (either Just (const Nothing) <$> backendOf defaultHubOpts defaultSimParams { spNComIds = 0 } bytes)
        >>= (`shouldSatisfy` maybe False ("no static ComID" `T.isInfixOf`))

  beforeAll (backend defaultHubOpts bytes) $ do
    describe "a hub with no peer" $ do
      it "draws a server id of 12 characters of [a-z0-9], another for another hub, and starts at seq 0 with no peer" $ \be -> do
        hubs <- sequenceA (replicate 2 (newHub defaultHubOpts be))
        map hubServerId hubs `shouldSatisfy` all (\t -> T.length t == 12 && T.all (\c -> C.isDigit c || C.isAsciiLower c) t)
        map hubServerId hubs `shouldSatisfy` (\ids -> length (L.nub ids) == 2)
        traverse hubSeq hubs `shouldReturn` [0, 0]
        traverse hubClientCount hubs `shouldReturn` [0, 0]
      it "advances seq with no peer connected, and refuses mh_transport's types, sed.* and a frame with no string type, seq kept" $ \be -> do
        hub <- newHub defaultHubOpts be
        traverse (hubBroadcast hub . typed) ["conformance_event", "state_changed"] `shouldReturn` [Right 1, Right 2]
        refused <- traverse (hubBroadcast hub) (map typed ["hello", "ping", "pong", "sed.req", "sed.rsp", "sed.evt", "sed.other"]
                                                <> [KeyMap.fromList [("n", A.Number 1)], KeyMap.fromList [("type", A.Number 1)]])
        refused `shouldSatisfy` all isLeft
        hubSeq hub `shouldReturn` 2
      it "sweeps and closes nothing when it has no peer" $ \be -> do
        hub <- newHub defaultHubOpts be
        hubSweep hub >> hubClose hub >> hubClose hub
        hubClientCount hub `shouldReturn` 0

    describe "over memory (group G1's frames and connections)" $ before_ needsG1 $ do
      it "says hello first: v 1, its server id, seq 0, its heartbeat, and the backend's app.sed" $ \be -> do
        hub <- newHub defaultHubOpts { hoIntervalMs = 20, hoTimeoutMs = 60 } be
        withPeer hub Nothing $ \c -> do
          h <- helloOn c
          (helloV h, helloServerId h, helloSeq h, helloIntervalMs h, helloTimeoutMs h) `shouldBe` (1, hubServerId hub, 0, 20, 60)
          sedAppOf h `shouldBe` Right (beApp be)

      it "shares seq among its peers, advancing it with none connected" $ \be -> do
        hub <- newHub defaultHubOpts be
        _ <- traverse (hubBroadcast hub . typed) ["one", "two"]
        withPeer hub Nothing $ \a -> withPeer hub Nothing $ \b -> do
          traverse (fmap helloSeq . helloOn) [a, b] `shouldReturn` [2, 2]
          hubBroadcast hub (typed "three") `shouldReturn` Right 3
          traverse nextFrame [a, b] `shouldReturn` [ServerBroadcast 3, ServerBroadcast 3]

      it "never skips or repeats a seq for peers that attach while broadcasts run: hello's seq and every broadcast after it" $ \be -> do
        hub <- newHub defaultHubOpts be
        pairs <- sequenceA (replicate 8 memoryPair)
        servers <- withAsync (replicateM_ 200 (hubBroadcast hub (typed "tick") >> yieldThread)) $ \broadcasts -> do
          servers <- traverse (\(_, server) -> async (hubServe hub Nothing server) <* yieldThread) pairs
          wait broadcasts $> servers
        flip finally (traverse_ cancel servers) $ do
          final <- hubSeq hub
          final `shouldBe` 200
          for_ pairs $ \(client, _) -> do
            h <- helloOn client
            connSendText client pingText
            untilPong client `shouldReturn` [ServerBroadcast n | n <- [helloSeq h + 1 .. final]]

      it "answers every ping with pong, whatever the peer's age, even while a request is held in the worker" $ \be -> do
        gate <- newEmptyMVar
        clock <- newClock
        let tr = beTransport be
        hub <- newHub (clockOpts clock defaultHubOpts) be { beTransport = tr { tExchange = \r -> readMVar gate >> tExchange tr r } }
        withPeer hub Nothing $ \c -> do
          _ <- helloOn c
          pingPong c
          connSendText c (reqText (SedReq 1 (OpIfRecv 1 0x1004 512)))
          setClock clock 1000
          pingPong c
          putMVar gate ()
          nextFrame c >>= (`shouldSatisfy` \case ServerRsp (SedRsp (Just 1) (Answered Good _)) -> True; _ -> False)

      it "ignores an unknown type, non-JSON, a JSON array or scalar, a frame with no type and binary, the socket kept" $ \be -> do
        hub <- newHub defaultHubOpts be
        withPeer hub Nothing $ \c -> do
          _ <- helloOn c
          traverse_ (connSendText c) junk
          connSendBinary c "\x00\x01binary"
          pingPong c
          hubClientCount hub `shouldReturn` 1

      it "takes any message as liveness, binary and junk included, and sweeps a peer silent at or past the timeout with 4000" $ \be -> do
        clock <- newClock
        hub <- newHub (clockOpts clock defaultHubOpts { hoTimeoutMs = 1000 }) be
        withPeer hub Nothing $ \silent -> withPeer hub Nothing $ \lively -> do
          _ <- helloOn silent
          _ <- helloOn lively
          -- a message read by the hub at a time; then a ping read at 0, which stamps nothing later (a stamp never goes
          -- back), its pong the proof that the message before it was read and stamped
          let sentAt :: Double -> IO () -> IO ()
              sentAt t send = do
                setClock clock t
                n <- readsOf clock
                send
                readPast clock n
                setClock clock 0
                pingPong lively
          sentAt 0.5 (connSendBinary lively "\x00")
          setClock clock 1.0 >> hubSweep hub
          hubClientCount hub `shouldReturn` 1
          next silent `shouldReturn` InClosed (Just (4000, "heartbeat timeout"))
          for_ (zip [1.2, 1.9, 2.5, 3.2] junk) $ \(t, frame) -> do
            sentAt t (connSendText lively frame)
            setClock clock (t + 0.99) >> hubSweep hub
            hubClientCount hub `shouldReturn` 1
          setClock clock 4.5 >> hubSweep hub
          hubClientCount hub `shouldReturn` 0
          next lively `shouldReturn` InClosed (Just (4000, "heartbeat timeout"))

      it "answers each request in arrival order, on its own socket alone, with no seq" $ \be -> do
        hub <- newHub defaultHubOpts be
        withPeer hub Nothing $ \a -> withPeer hub Nothing $ \b -> do
          traverse_ helloOn [a, b]
          traverse_ (connSendText a . reqText) [SedReq 1 OpDescribe, SedReq 2 (OpIfRecv 1 0x1004 64), SedReq 3 OpDescribe]
          connSendText b (reqText (SedReq 7 OpDescribe))
          replies <- sequenceA (replicate 3 (next a))
          mapMaybe responseId replies `shouldBe` [1, 2, 3]
          filter carriesSeq replies `shouldBe` []
          (responseId <$> next b) `shouldReturn` Just 7
          traverse_ pingPong [a, b]

      it "answers protocol_error to a bad field, len over 1 MiB, bad base64, a payload not len long and an unknown op, its id echoed" $ \be -> do
        hub <- newHub defaultHubOpts be
        withPeer hub Nothing $ \c -> do
          _ <- helloOn c
          for_ badRequests $ \(frame, rid) -> do
            connSendText c frame
            nextFrame c >>= (`shouldSatisfy` \case ServerRsp (SedRsp i (ProtocolError _)) -> i == rid; _ -> False)
          -- none of those ids was accepted: 1 still is
          request c 1 OpDescribe `shouldReturn` SedRsp (Just 1) (Described (beApp be))

      it "answers protocol_error to an id not above the last accepted, which the refusal leaves where it was" $ \be -> do
        hub <- newHub defaultHubOpts be
        withPeer hub Nothing $ \c -> do
          _ <- helloOn c
          request c 5 OpDescribe `shouldReturn` SedRsp (Just 5) (Described (beApp be))
          for_ [5, 4, 1] $ \i -> request c i OpDescribe >>= (`shouldSatisfy` \case SedRsp (Just i') (ProtocolError _) -> i' == i; _ -> False)
          request c 6 OpDescribe `shouldReturn` SedRsp (Just 6) (Described (beApp be))

      it "answers a second socket's IF-SEND on 0x1004 synchronous_protocol_violation while the first's response waits" $ \_ -> do
        be <- backend defaultHubOpts bytes
        hub <- newHub defaultHubOpts be
        withPeer hub Nothing $ \a -> withPeer hub Nothing $ \b -> do
          traverse_ helloOn [a, b]
          request a 1 (OpIfSend 1 0x1004 properties) `shouldReturn` SedRsp (Just 1) (Answered Good mempty)
          request b 1 (OpIfSend 1 0x1004 properties) `shouldReturn` SedRsp (Just 1) (Answered SynchronousProtocolViolation mempty)

      it "answers the reset op unsupported without --allow-remote-reset, naming it, the TPer untouched" $ \_ -> do
        be <- backend defaultHubOpts bytes
        hub <- newHub defaultHubOpts be
        withPeer hub Nothing $ \c -> do
          _ <- helloOn c
          _ <- request c 1 (OpIfSend 1 0x1004 startSession)
          untouched <- scPeek (beControl be)
          request c 2 (OpReset PowerCycle) >>= (`shouldSatisfy` unsupported "--allow-remote-reset")
          scPeek (beControl be) `shouldReturn` untouched
          hubSeq hub `shouldReturn` 0

      it "with --allow-remote-reset, broadcasts one sed.evt to every peer, the requester's before its response" $ \_ -> do
        let opts = defaultHubOpts { hoAllowReset = True }
        be <- backend opts bytes
        hub <- newHub opts be
        withPeer hub Nothing $ \a -> withPeer hub Nothing $ \b -> do
          traverse_ helloOn [a, b]
          _ <- request a 1 (OpIfSend 1 0x1004 startSession)
          st <- scPeek (beControl be)
          answer Sessions st `shouldSatisfy` (not . null)
          let evt = ServerEvt (SedEvt 1 PowerCycle [0x1004] (answer Sessions st))
          connSendText a (reqText (SedReq 2 (OpReset PowerCycle)))
          traverse nextFrame [a, a, b] `shouldReturn` [evt, ServerRsp (SedRsp (Just 2) (Answered Good mempty)), evt]
          traverse_ pingPong [a, b]
          answer Sessions <$> scPeek (beControl be) `shouldReturn` []

      it "answers peek and release unsupported without --allow-remote-control; describe always" $ \be -> do
        hub <- newHub defaultHubOpts be
        withPeer hub Nothing $ \c -> do
          _ <- helloOn c
          request c 1 (OpPeek (SomeQuery Msid)) >>= (`shouldSatisfy` unsupported "--allow-remote-control")
          request c 2 (OpRelease 0x1004) >>= (`shouldSatisfy` unsupported "--allow-remote-control")
          request c 3 OpDescribe `shouldReturn` SedRsp (Just 3) (Described (beApp be))

      it "with --allow-remote-control, answers each of the eight queries as the state does, and release ticks the ComID" $ \_ -> do
        let opts = defaultHubOpts { hoAllowControl = True }
        be <- backendWith opts defaultSimParams { spLockingOFS = Manufactured } (InProcOpts UnitBytes 1)
        hub <- newHub opts be
        withPeer hub Nothing $ \c -> do
          _ <- helloOn c
          request c 1 (OpIfSend 1 0x1004 properties) `shouldReturn` SedRsp (Just 1) (Answered Good mempty)
          let peeks :: (Eq a, Show a) => Integer -> SimQuery a -> IO ()
              peeks i q = do
                st <- scPeek (beControl be)
                request c i (OpPeek (SomeQuery q)) >>= \case
                  SedRsp (Just i') (Viewed v) | i' == i -> parseAnswer q v `shouldBe` Right (answer q st)
                  other                                 -> expectationFailure (show other)
          peeks 2 ComIds
          peeks 3 Sessions
          peeks 4 (LifeCycleOf uSP_Locking)
          peeks 5 (CredentialOf uSP_Admin uAuthority_SID)
          peeks 6 (CellOf uSP_Locking (tdTableUID authorityTableDesc) LockingSP.uAuthority_User1 "Enabled")
          peeks 7 (ByteTableOf uSP_Locking dataStoreTable)
          peeks 8 Msid
          peeks 9 LockingRanges
          (syncOf 0x1004 <$> scPeek (beControl be)) >>= (`shouldSatisfy` \case Just (S2 _) -> True; _ -> False)
          request c 10 (OpRelease 0x1004) `shouldReturn` SedRsp (Just 10) (Answered Good mempty)
          (syncOf 0x1004 <$> scPeek (beControl be)) >>= (`shouldSatisfy` \case Just (S3 _) -> True; _ -> False)

      it "with --token-file, closes a peer whose token is missing or wrong with 4001 before hello, and greets the right one" $ \be -> do
        hub <- newHub defaultHubOpts { hoToken = Just "s3cret" } be
        for_ [Nothing, Just "wrong", Just "s3cret\n", Just ""] $ \token ->
          withPeer hub token $ \c -> next c >>= (`shouldSatisfy` \case InClosed (Just (4001, _)) -> True; _ -> False)
        withPeer hub (Just "s3cret") $ \c -> helloOn c >> (hubClientCount hub `shouldReturn` 1)

      it "closes a peer whose send queue is full with 1008 and removes it at once" $ \be -> do
        hub <- newHub defaultHubOpts { hoQueue = 4 } be
        (client, server) <- memoryPair
        gate <- newEmptyMVar
        sending <- newEmptyMVar
        let held = server { connSendText = \bytes' -> void (tryPutMVar sending ()) >> readMVar gate >> connSendText server bytes' }
        withAsync (hubServe hub Nothing held) $ \_ -> do
          takeMVar sending
          replicateM_ 5 (hubBroadcast hub (typed "flood"))
          hubClientCount hub `shouldReturn` 0
          putMVar gate ()
          _ <- helloOn client
          next client `shouldReturn` InClosed (Just (1008, "send queue full"))

      it "closes every peer with 1001 on hubClose, removed at once, and serves on: a peer reconnects to the same server id and seq" $ \be -> do
        hub <- newHub defaultHubOpts be
        _ <- hubBroadcast hub (typed "before")
        withPeer hub Nothing $ \a -> withPeer hub Nothing $ \b -> do
          traverse_ helloOn [a, b]
          hubClose hub >> hubClose hub
          hubClientCount hub `shouldReturn` 0
          traverse next [a, b] `shouldReturn` replicate 2 (InClosed (Just (1001, "going away")))
        withPeer hub Nothing $ \c -> helloOn c >>= \h -> (helloServerId h, helloSeq h) `shouldBe` (hubServerId hub, 1)

      it "answers IF-SEND and IF-RECV as the in-process transport does, request for request (QuickCheck over genStaticRequest)" $ \_ ->
        withMaxSuccess 4 . forAll (listOf1 (genStaticRequest (foldedAt ManufacturedInactive))) $ \requests -> ioProperty $ do
          direct <- backend defaultHubOpts bytes
          answers <- traverse (\r -> exchanged (beTransport direct) r) requests
          viaHub <- backend defaultHubOpts bytes
          hub <- newHub defaultHubOpts viaHub
          replies <- withPeer hub Nothing $ \c -> helloOn c >> traverse (\(i, r) -> rspReply <$> request c i (opOf r)) (zip [1 ..] requests)
          pure (replies === map (\r -> Answered (status r) (rpayload r)) answers)


-- * The backend

-- | The default simulator's backend for these options and transport
--   options; a failure where there is none.
backend :: HubOpts -> InProcOpts -> IO Backend
backend opts = backendWith opts defaultSimParams

-- | A backend of these parameters; a failure where there is none.
backendWith :: HubOpts -> SimParams -> InProcOpts -> IO Backend
backendWith opts params inProc = backendOf opts params inProc >>= either (throwString . T.unpack) pure

-- | Bytes, no hold: the in-process run's own options.
bytes :: InProcOpts
bytes = InProcOpts UnitBytes 0


-- * Group G1's

-- | Pending, naming group G1, while group G1's frames and connections are
--   the scaffold's stubs (M5c): every example over memory needs memoryPair
--   and the codecs. Part 2 of group G2 deletes this once G1 is merged.
needsG1 :: IO ()
needsG1 = do
  stubbed <- or <$> sequence [stub (evaluate (B.length pingText)), stub (void memoryPair)]
  when stubbed $ pendingWith "group G1's Hsed.Transport.Mhtx.Frame and Conn (memoryPair, the codecs): pending until G1 is merged"
  where
    stub act = try act <&> either (\e -> "group G1 (" `T.isInfixOf` tshow (e :: SomeException)) (const False)


-- * A clock the test sets

-- | The hub's clock as the test sets it, in seconds, counting its reads:
--   the hub reads it once as a peer attaches, once for each message a peer
--   sends and once for each sweep, so that the test knows when a message has
--   been read.
data Clock = Clock
  { cNow   :: !(TVar Double)
  , cReads :: !(TVar Int)
  }

newClock :: IO Clock
newClock = Clock <$> newTVarIO 0 <*> newTVarIO 0

-- | The options with the clock as the hub's.
clockOpts :: Clock -> HubOpts -> HubOpts
clockOpts c o = o { hoNow = atomically (modifyTVar' (cReads c) (+ 1) >> readTVar (cNow c)) }

setClock :: Clock -> Double -> IO ()
setClock c = atomically . writeTVar (cNow c)

readsOf :: Clock -> IO Int
readsOf = readTVarIO . cReads

-- | Once the clock has been read more than so many times; a failure after
--   2 s.
readPast :: Clock -> Int -> IO ()
readPast c n = timeout 2000000 (atomically (readTVar (cReads c) >>= checkSTM . (> n)))
  >>= maybe (expectationFailure ("the hub's clock was not read past " <> show n <> " times within 2 s")) pure


-- * Peers over memory

-- | A peer of the hub over memory: the client's end given to the action, the
--   hub serving the other with the token given in a thread of its own,
--   stopped when the action returns.
withPeer :: Hub -> Maybe ByteString -> (Conn -> IO a) -> IO a
withPeer hub token act = do
  (client, server) <- memoryPair
  withAsync (hubServe hub token server) (const (act client))

-- | What arrives next on a connection: within 2 s, a bound on a failure,
--   never a wait the test counts on.
next :: Conn -> IO Incoming
next c = timeout 2000000 (connReceive c) >>= maybe (throwString "nothing arrived within 2 s") pure

-- | The next frame, sorted.
nextFrame :: Conn -> IO FromServer
nextFrame c = fromServer <$> next c

-- | The hello a connection begins with.
helloOn :: Conn -> IO Hello
helloOn c = nextFrame c >>= \case
  ServerHello h -> pure h
  other         -> throwString ("the first frame is not a hello: " <> show other)

-- | A ping, and its pong the next frame: the socket open and everything sent
--   before it read.
pingPong :: Conn -> IO ()
pingPong c = connSendText c pingText >> nextFrame c >>= (`shouldBe` ServerPong)

-- | The frames before the next pong.
untilPong :: Conn -> IO [FromServer]
untilPong c = nextFrame c >>= \case
  ServerPong -> pure []
  frame      -> (frame :) <$> untilPong c

-- | A request and its response, the next frame.
request :: Conn -> Integer -> Op -> IO SedRsp
request c i op = connSendText c (reqText (SedReq i op)) >> nextFrame c >>= \case
  ServerRsp r -> pure r
  other       -> throwString ("not a response: " <> show other)

-- | A response's id, where the frame is one.
responseId :: Incoming -> Maybe Integer
responseId incoming = case fromServer incoming of
  ServerRsp r -> rspId r
  _           -> Nothing

-- | Whether a frame carries seq.
carriesSeq :: Incoming -> Bool
carriesSeq = \case
  InText t -> maybe False (KeyMap.member "seq") (A.decodeStrict t :: Maybe A.Object)
  _        -> False

-- | Whether a response is unsupported, its detail naming the flag.
unsupported :: Text -> SedRsp -> Bool
unsupported flag = \case
  SedRsp _ (Answered (Unsupported why) b) -> flag `T.isInfixOf` why && B.null b
  _                                       -> False

-- | An object of this type alone.
typed :: Text -> A.Object
typed t = KeyMap.fromList [("type", A.String t)]

-- | Text frames a hub ignores (SPEC.md L19-21; conformance.dart:142-143).
junk :: [ByteString]
junk = ["{\"type\":\"conformance_bogus\",\"x\":1}", "this is not json", "[1,2]", "{\"x\":1}", "42"]

-- | Requests refused as they are read, each with the id its refusal echoes:
--   a field missing, a field out of its type, len past 1 MiB, base64 not
--   canonical or not base64, a payload not len long, an op not known, ids not
--   positive or not integral.
badRequests :: [(ByteString, Maybe Integer)]
badRequests =
  [ ("{\"type\":\"sed.req\",\"id\":1,\"op\":\"if_recv\",\"proto\":1,\"comid\":4100}", Just 1)
  , ("{\"type\":\"sed.req\",\"id\":2,\"op\":\"if_recv\",\"proto\":256,\"comid\":4100,\"len\":64}", Just 2)
  , ("{\"type\":\"sed.req\",\"id\":3,\"op\":\"if_recv\",\"proto\":1,\"comid\":4100,\"len\":1048577}", Just 3)
  , ("{\"type\":\"sed.req\",\"id\":4,\"op\":\"if_send\",\"proto\":1,\"comid\":4100,\"len\":1,\"payload\":\"AB==\"}", Just 4)
  , ("{\"type\":\"sed.req\",\"id\":5,\"op\":\"if_send\",\"proto\":1,\"comid\":4100,\"len\":3,\"payload\":\"!!!!\"}", Just 5)
  , ("{\"type\":\"sed.req\",\"id\":6,\"op\":\"if_send\",\"proto\":1,\"comid\":4100,\"len\":4,\"payload\":\"AAAA\"}", Just 6)
  , ("{\"type\":\"sed.req\",\"id\":7,\"op\":\"format\"}", Just 7)
  , ("{\"type\":\"sed.req\",\"id\":0,\"op\":\"describe\"}", Just 0)
  , ("{\"type\":\"sed.req\",\"id\":\"x\",\"op\":\"describe\"}", Nothing)
  , ("{\"type\":\"sed.req\",\"id\":2.5,\"op\":\"describe\"}", Nothing)
  ]

-- | SMUID.Properties on 0x1004, a control session's ComPacket.
properties :: ByteString
properties = either (error . show) id (controlComPacket 0x1004 propertiesCall)

-- | StartSession on 0x1004 to the Admin SP as Anybody, Write True, which
--   the simulator requires (Core 5.2.3.1.3; "Hsed.Sim.SessionManager").
startSession :: ByteString
startSession = either (error . show) id (controlComPacket 0x1004 (generate (startSessionCall (StartArgs uSP_Admin Nothing Nothing True))))

-- | A request as an op of the hub's.
opOf :: Request -> Op
opOf r = case dir r of
  IfSend -> OpIfSend (proto r) (comId r) (payload r)
  IfRecv -> OpIfRecv (proto r) (comId r) (lenBytes r)

-- | A request exchanged on a transport as hsed's host issues it.
exchanged :: Transport 'ReadWrite -> Request -> IO Response
exchanged t r = case dir r of
  IfSend -> ifSend t (proto r) (comId r) (lenBytes r) (payload r)
  IfRecv -> ifRecv t (proto r) (comId r) (lenBytes r)
