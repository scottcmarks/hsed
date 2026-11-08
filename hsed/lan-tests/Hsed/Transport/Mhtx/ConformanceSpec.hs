{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.ConformanceSpec
Description : The eight checks of mh_transport's SPEC.md re-expressed against the hub on 127.0.0.1, at 200/600 ms
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

mh_transport SPEC.md's checks 1-7 (L113-129) as conformance.dart runs them
(:112-235), with its waits: 3 s for a frame (:88), 2 s for a pong (:135,
:145, :228), 3 s for a count to drop, polled every 100 ms (:199-203), and the
silent client closed within the timeout and the interval and a second
(:208, :220); each against a hub of its own on bindLoopback 0, at a heartbeat
of 200 ms and a timeout of 600 ms, its hooks on, emit and count going
through 'hubBroadcast' and 'hubClientCount' as conformance_hub_test.dart:14-20
does; and check 8 (conformance.dart:237-275) with hsed's client, which
has no stop and start that keep what it last saw: the hub closes every peer
with 1001 and serves on ('hubClose', its server id and @seq@ kept), the
client reconnects by itself after a fixed 250 ms, a broadcast made in that
gap the second time, and each connection's verdict is read as
'clientVerdicts' gives it (design 13.34). Besides them: the hooks over raw HTTP (@POST \/emit@'s 200
@{"seq": n}@ and its 400s, @GET \/clients@, a 426 and a 404); the
binary-frame check (SPEC.md L21: a binary message ignored, and counted as
liveness, hub.dart:83-86, which the Python hub breaks, design D5); a
non-increasing id and bogus frames; and the token's 4001. The latencies the
checks measure (a pong, the silent client's close, the live client's
longest gap between pongs) are written to stderr (Scott's answer Q3: CI
runs this at 200/600 ms, and a flake is a bug, never a reason to widen the
heartbeat).
-}
module Hsed.Transport.Mhtx.ConformanceSpec (spec) where

import qualified Data.Aeson                as A
import qualified Data.Aeson.KeyMap         as KeyMap
import qualified GHC.Clock                 as Clock
import qualified RIO.ByteString            as B
import qualified RIO.List                  as L
import           RIO
import qualified RIO.Text                  as T
import           Test.Hspec

import           Hsed.Lan.Support           (lanBackend, rawHttp, withHub)
import           Hsed.Transport.Mhtx.Client (Backoff (..), ClientOpts (..), Missed (..), MhtxClient, clientOpts, clientVerdicts,
                                             closeMhtx, openMhtx)
import           Hsed.Transport.Mhtx.Conn   (Conn (..), Incoming (..), codeNormal, dialLoopback)
import           Hsed.Transport.Mhtx.Frame  (FromServer (..), Hello (..), Op (..), Reply (..), SedReq (..), SedRsp (..), fromServer,
                                             pingText, reqText)
import           Hsed.Transport.Mhtx.Hub    (Hub, HubOpts (..), defaultHubOpts, hubBroadcast, hubClientCount, hubClose, hubServerId)


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Hub on 127.0.0.1 at 200/600 ms (mh_transport SPEC.md L113-129)" $ do
  checks
  it "8. has the client judge missed broadcasts: unknown, then some, then none, each connection within 5 s (conformance.dart:237-275)" $
    conformant $ \port hub -> bracket (openMhtx (reconnecting port) >>= either (throwString . T.unpack) pure) closeMhtx $ \client -> do
      opened  <- verdictsOf client
      started <- Clock.getMonotonicTime
      hubClose hub
      hubBroadcast hub (event 3) `shouldReturn` Right 1
      missed  <- verdictsOf client
      again   <- Clock.getMonotonicTime
      hubClose hub
      kept    <- verdictsOf client
      done    <- Clock.getMonotonicTime
      [opened, missed, kept] `shouldBe` [[MissedUnknown], [MissedSome], [MissedNone]]
      note ("check 8: unknown, then some " <> ms (again - started) <> " after the first close, then none " <> ms (done - again)
            <> " after the second")
  beyond

-- | SPEC.md's checks 1 to 7, as conformance.dart runs them.
checks :: Spec
checks = do
  it "1. sends a well-formed hello with v 1 first, within 3 s (conformance.dart:117-129)" $ conformant $ \port hub -> withProbe port "/ws" $ \a -> do
    h <- helloOf a
    (helloV h, helloServerId h, helloIntervalMs h, helloTimeoutMs h) `shouldBe` (1, hubServerId hub, 200, 600)

  it "2. answers ping with pong within 2 s (conformance.dart:133-139)" $ conformant $ \port _ -> withProbe port "/ws" $ \a -> do
    _ <- helloOf a
    sent <- Clock.getMonotonicTime
    send a pingText
    nextRaw a twoSeconds >>= \case
      Just (at, t) -> (fromServer (InText t) `shouldBe` ServerPong) >> note ("check 2: a pong " <> ms (at - sent) <> " after its ping")
      Nothing      -> expectationFailure "no pong within 2 s"

  it "3. ignores an unknown type and a non-JSON message and still answers pings (conformance.dart:141-151)" $ conformant $ \port _ ->
    withProbe port "/ws" $ \a -> do
      _ <- helloOf a
      traverse_ (send a) ["{\"type\":\"conformance_bogus\",\"x\":1}", "this is not json", pingText]
      nextFrame a twoSeconds `shouldReturn` Just ServerPong
      isOpen a `shouldReturn` True

  it "4. broadcasts to every client with the next seq, then the one after (conformance.dart:154-179)" $ conformant $ \port hub ->
    withProbe port "/ws" $ \a -> do
      _ <- helloOf a
      withProbe port "/ws" $ \b -> do
        helloB <- helloOf b
        helloServerId helloB `shouldBe` hubServerId hub
        let expected = helloSeq helloB + 1
        hubBroadcast hub (event 1) `shouldReturn` Right expected
        traverse (`nextObject` threeSeconds) [a, b] `shouldReturn` replicate 2 (Just (eventFrame 1 expected))
        hubBroadcast hub (event 2) `shouldReturn` Right (expected + 1)
        nextObject a threeSeconds `shouldReturn` Just (eventFrame 2 (expected + 1))

  it "5. greets a fresh connection with the last seq broadcast and the same server id (conformance.dart:181-190)" $ conformant $ \port hub -> do
    traverse_ (hubBroadcast hub . event) [1, 2]
    withProbe port "/ws" $ \c -> do
      h <- helloOf c
      (helloSeq h, helloServerId h) `shouldBe` (2, hubServerId hub)

  it "6. counts a closed client out within 3 s, polled every 100 ms (conformance.dart:193-205)" $ conformant $ \port hub ->
    withProbe port "/ws" $ \d -> do
      _ <- helloOf d
      counted <- hubClientCount hub
      counted `shouldBe` 1
      connClose (prConn d) codeNormal "bye"
      dropped <- polled 30 (100 * millis) ((< counted) <$> hubClientCount hub)
      dropped `shouldBe` True

  it "7. closes a client that never pings with 4000 within 1.8 s and keeps one pinging every 200 ms (conformance.dart:207-235)" $ conformant $ \port _ ->
    withProbe port "/ws" $ \silent -> withProbe port "/ws" $ \live -> do
      attached <- helloOf silent >> Clock.getMonotonicTime
      _ <- helloOf live
      stop <- newTVarIO False
      closed <- withAsync (pinging live stop) $ \pinger ->
        waitClosed silent (1800 * millis) <* atomically (writeTVar stop True) <* wait pinger
      closedAfter <- subtract attached <$> Clock.getMonotonicTime
      fmap fst closed `shouldBe` Just (Just 4000)
      isOpen live `shouldReturn` True
      gaps <- pongGaps live
      send live pingText
      nextFrame live twoSeconds `shouldReturn` Just ServerPong
      note ("check 7: the silent client closed " <> ms closedAfter <> " after its hello; the live client's longest gap between pongs "
            <> maybe "(none)" ms (L.maximumMaybe gaps))

-- | Beyond SPEC.md's checks: the hooks over raw HTTP, binary messages,
--   requests and the token.
beyond :: Spec
beyond = do
  describe "the hooks, over raw HTTP (--conformance-hooks)" $ do
    it "answers POST /emit 200 {\"seq\": n} and GET /clients {\"count\": n}" $ conformant $ \port _ -> do
      emitted <- rawHttp port (post "/emit" "{\"type\":\"conformance_event\",\"n\":1}")
      (statusOf emitted, bodyOf emitted) `shouldBe` (200, Just (A.object ["seq" A..= (1 :: Int)]))
      counted <- rawHttp port (get "/clients")
      (statusOf counted, bodyOf counted) `shouldBe` (200, Just (A.object ["count" A..= (0 :: Int)]))
      withProbe port "/ws" $ \a -> do
        _ <- helloOf a
        counted' <- rawHttp port (get "/clients")
        bodyOf counted' `shouldBe` Just (A.object ["count" A..= (1 :: Int)])
    it "answers 400 to a body that is not a JSON object, to mh_transport's own types and to sed.*, the seq kept" $ conformant $ \port hub -> do
      statuses <- traverse (fmap statusOf . rawHttp port . post "/emit")
                    ["[1,2]", "\"conformance_event\"", "this is not json", "{\"n\":1}", "{\"type\":\"hello\"}", "{\"type\":\"ping\"}"
                    , "{\"type\":\"pong\"}", "{\"type\":\"sed.evt\",\"seq\":1}", "{\"type\":\"sed.rsp\",\"id\":1}"]
      statuses `shouldBe` replicate 9 400
      hubBroadcast hub (event 1) `shouldReturn` Right 1
    it "answers 426 to /ws without an upgrade, 404 elsewhere, and 404 to the hooks of a hub without them" $ do
      conformant $ \port _ -> traverse (fmap statusOf . rawHttp port) [get "/ws", get "/", get "/emit", post "/clients" "{}", get "/ws/"]
                                `shouldReturn` [426, 404, 404, 404, 404]
      be <- lanBackend opts
      withHub opts { hoHooks = False } be $ \port _ ->
        traverse (fmap statusOf . rawHttp port) [post "/emit" "{\"type\":\"conformance_event\"}", get "/clients"] `shouldReturn` [404, 404]

  describe "binary messages (SPEC.md L21; hub.dart:83-86)" $ do
    it "ignores a binary message: a ping after it is answered pong next, the socket open" $ conformant $ \port _ ->
      withProbe port "/ws" $ \a -> do
        _ <- helloOf a
        connSendBinary (prConn a) "\x00\x01\x02 not text"
        send a pingText
        nextFrame a twoSeconds `shouldReturn` Just ServerPong
        isOpen a `shouldReturn` True
    it "keeps a client that sends binary messages alone, every 200 ms, past the timeout" $ conformant $ \port hub ->
      withProbe port "/ws" $ \a -> do
        _ <- helloOf a
        replicateM_ 8 (connSendBinary (prConn a) "\x00" >> threadDelay (200 * millis))
        (isOpen a, hubClientCount hub) `bothReturn` (True, 1)
        send a pingText
        nextFrame a twoSeconds `shouldReturn` Just ServerPong

  describe "requests (design 6.3)" $ do
    it "answers protocol_error to an id not above the last accepted, and nothing to bogus frames" $ conformant $ \port _ ->
      withProbe port "/ws" $ \a -> do
        _ <- helloOf a
        send a (reqText (SedReq 2 OpDescribe))
        nextFrame a twoSeconds >>= (`shouldSatisfy` \case Just (ServerRsp (SedRsp (Just 2) (Described _))) -> True; _ -> False)
        send a (reqText (SedReq 2 OpDescribe))
        nextFrame a twoSeconds >>= (`shouldSatisfy` \case Just (ServerRsp (SedRsp (Just 2) (ProtocolError _))) -> True; _ -> False)
        traverse_ (send a) ["{\"type\":\"conformance_bogus\"}", "{]", pingText]
        nextFrame a twoSeconds `shouldReturn` Just ServerPong

  describe "--token-file (SPEC.md L92-95)" $
    it "closes a client whose token query parameter is missing or wrong with 4001 before hello, and greets the right one" $ do
      be <- lanBackend opts
      withHub opts { hoToken = Just "s3cret" } be $ \port hub -> do
        for_ ["/ws", "/ws?token=wrong", "/ws?token="] $ \path -> withProbe port path $ \p -> do
          nextFrame p threeSeconds `shouldReturn` Nothing
          closeCode p `shouldReturn` Just 4001
        withProbe port "/ws?token=s3cret" $ \p -> helloOf p >>= \h -> helloServerId h `shouldBe` hubServerId hub


-- | The conformance run's hub options: a heartbeat of 200 ms, a timeout of
--   600 ms (the plan's, Scott's answer Q3: never wider), the hooks on.
opts :: HubOpts
opts = defaultHubOpts { hoIntervalMs = 200, hoTimeoutMs = 600, hoHooks = True }

-- | An action against a hub of the conformance run's options, its own, on
--   127.0.0.1 and an ephemeral port.
conformant :: (Word16 -> Hub -> IO a) -> IO a
conformant act = lanBackend opts >>= \be -> withHub opts be act

-- | A ping every 200 ms (conformance.dart:216-218) until told to stop, the
--   stop seen between two pings, never in a send.
pinging :: Probe -> TVar Bool -> IO ()
pinging p stop = readTVarIO stop >>= \done -> unless done $ do
  send p pingText
  void (timeout (200 * millis) (atomically (readTVar stop >>= checkSTM)))
  pinging p stop


-- * Check 8's client

-- | hsed's client of the hub, which requires a hello (conformance.dart:239-243's
--   @requireHello@), reconnecting 250 ms after a drop, no jitter: long
--   enough for a broadcast made at once to be missed, well within the
--   check's 5 s for a connection (conformance.dart:245-249).
reconnecting :: Word16 -> ClientOpts
reconnecting port = (clientOpts port "/ws") { coBackoff = Backoff 250000 2 250000 0 }

-- | The verdicts on the connections the client has taken since last asked,
--   polled every 10 ms until there is one, for 5 s at most
--   (conformance.dart:249's timeout); none when it took none.
verdictsOf :: MhtxClient -> IO [Missed]
verdictsOf client = go (500 :: Int)
  where
    go n = clientVerdicts client >>= \case
      [] | n > 0 -> threadDelay (10 * millis) >> go (n - 1)
      vs         -> pure vs


-- * A probe (conformance.dart:284-350's _Probe)

-- | A websocket to the hub, its one reader queueing what arrives, as
--   conformance.dart's probe does: each text frame with the time it arrived,
--   and the close with its code and reason.
data Probe = Probe
  { prConn   :: !Conn
  , prFrames :: !(TQueue (Double, ByteString))
  , prClosed :: !(TMVar (Maybe (Word16, Text)))
  }

-- | An action over a probe on the hub's path, connected within 5 s
--   (conformance.dart:91); a failure where it is not.
withProbe :: Word16 -> Text -> (Probe -> IO a) -> IO a
withProbe port path act = dialLoopback port path (5000 * millis) opened >>= either (throwString . T.unpack) pure
  where
    opened c = do
      p <- Probe c <$> newTQueueIO <*> newEmptyTMVarIO
      withAsync (reading p) (const (act p))
    reading p = connReceive (prConn p) >>= \case
      InText t     -> Clock.getMonotonicTime >>= \at -> atomically (writeTQueue (prFrames p) (at, t)) >> reading p
      InBinary _   -> reading p
      InClosed why -> atomically (putTMVar (prClosed p) why)

-- | A text frame sent.
send :: Probe -> ByteString -> IO ()
send p = connSendText (prConn p)

-- | The next text frame and when it arrived, within the wait; none at its
--   end, or once the probe is closed.
nextRaw :: Probe -> Int -> IO (Maybe (Double, ByteString))
nextRaw p wait' = join <$> timeout wait' (atomically ((Just <$> readTQueue (prFrames p)) `orElse` (Nothing <$ readTMVar (prClosed p))))

-- | The next frame, sorted.
nextFrame :: Probe -> Int -> IO (Maybe FromServer)
nextFrame p wait' = fmap (fromServer . InText . snd) <$> nextRaw p wait'

-- | The next frame as a JSON object.
nextObject :: Probe -> Int -> IO (Maybe A.Object)
nextObject p wait' = (>>= A.decodeStrict . snd) <$> nextRaw p wait'

-- | The hello a probe begins with, within 3 s (conformance.dart:119-122).
helloOf :: Probe -> IO Hello
helloOf p = nextFrame p threeSeconds >>= \case
  Just (ServerHello h) -> pure h
  other                -> throwString ("the first frame is not a hello within 3 s: " <> show other)

-- | Whether the probe is still open.
isOpen :: Probe -> IO Bool
isOpen p = isNothing <$> atomically (tryReadTMVar (prClosed p))

-- | The code the probe was closed with, waiting 3 s at most for its close.
closeCode :: Probe -> IO (Maybe Word16)
closeCode p = timeout (3000 * millis) (atomically (readTMVar (prClosed p))) <&> \r -> fst <$> join r

-- | The probe's close, with its code and reason, within the wait.
waitClosed :: Probe -> Int -> IO (Maybe (Maybe Word16, Text))
waitClosed p wait' = timeout wait' (atomically (readTMVar (prClosed p))) <&> fmap (maybe (Nothing, "") (first Just))

-- | The gaps between the pongs the probe has received, its queue drained
--   (conformance.dart:226's drain).
pongGaps :: Probe -> IO [Double]
pongGaps p = do
  frames <- atomically drained
  let pongs = [ at | (at, t) <- frames, fromServer (InText t) == ServerPong ]
  pure (zipWith (-) (L.drop 1 pongs) pongs)
  where
    drained = tryReadTQueue (prFrames p) >>= maybe (pure []) (\frame -> (frame :) <$> drained)


-- * Waits, frames, HTTP

millis :: Int
millis = 1000

twoSeconds, threeSeconds :: Int
twoSeconds   = 2000 * millis
threeSeconds = 3000 * millis

-- | Whether a condition held within so many polls, so far apart.
polled :: Int -> Int -> IO Bool -> IO Bool
polled n gap cond
  | n <= 0    = cond
  | otherwise = cond >>= \held -> if held then pure True else threadDelay gap >> polled (n - 1) gap cond

-- | Two results as expected.
bothReturn :: (HasCallStack, Eq a, Eq b, Show a, Show b) => (IO a, IO b) -> (a, b) -> Expectation
bothReturn (x, y) want = ((,) <$> x <*> y) `shouldReturn` want

-- | conformance.dart's test broadcast (:161, :171).
event :: Int -> A.Object
event n = KeyMap.fromList [("type", A.String "conformance_event"), ("n", A.toJSON n)]

-- | The broadcast as a client receives it: the event with the hub's seq.
eventFrame :: Int -> Integer -> A.Object
eventFrame n s = KeyMap.insert "seq" (A.toJSON s) (event n)

-- | A duration in seconds as milliseconds, to a tenth.
ms :: Double -> Text
ms s = tshow (fromIntegral (round (s * 10000) :: Int) / 10 :: Double) <> " ms"

-- | A line the suite's run prints, on stderr.
note :: Text -> IO ()
note t = hPutBuilder stderr (getUtf8Builder ("ConformanceSpec: " <> display t <> "\n"))

-- | An HTTP request to the hub, closing the connection after its response.
get :: ByteString -> ByteString
get path = "GET " <> path <> " HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"

-- | A POST of a JSON body.
post :: ByteString -> ByteString -> ByteString
post path body = "POST " <> path <> " HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Type: application/json\r\nContent-Length: "
                 <> encodeUtf8 (tshow (B.length body)) <> "\r\nConnection: close\r\n\r\n" <> body

-- | A response's status code (0 where its status line does not read).
statusOf :: ByteString -> Int
statusOf response = case B.split 32 (B.takeWhile (/= 13) response) of
  _ : code : _ -> fromMaybe 0 (readMaybe (T.unpack (decodeUtf8Lenient code)))
  _            -> 0

-- | A response's body as JSON.
bodyOf :: ByteString -> Maybe A.Value
bodyOf response = case B.breakSubstring "\r\n\r\n" response of
  (_, rest) | not (B.null rest) -> A.decodeStrict (B.drop 4 rest)
  _                             -> Nothing
