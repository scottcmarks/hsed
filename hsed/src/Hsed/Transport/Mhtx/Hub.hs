{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.Hub
Description : The simulator's mh_transport hub: hello, heartbeat, the ops on a shared in-process TPer, broadcasts, the hooks
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The hub hsed-sim serves (design 7 item 5, 11 M5c): one in-process simulator
('Backend', "Hsed.Transport.InProc") shared by every socket behind one lock;
a hello first on every socket, registered with the broadcasts' @seq@ at once
(mh_transport SPEC.md L37, L70-77); pong for every ping, whatever the
socket's age and whatever request is in progress (L58, L64); the sweep that
closes a socket silent past the timeout with 4000 (L58-60); each socket's
requests in arrival order, answered on that socket with no @seq@ (design
6.3); the reset op's seq-stamped broadcast before its response; the control
ops, peek and release, only with @--allow-remote-control@, and reset only
with @--allow-remote-reset@, otherwise answered unsupported; a token checked
before hello (4001, SPEC.md L92-95), and asked of the hooks too; the hooks
@POST \/emit@ and @GET \/clients@ only with @--conformance-hooks@; a request
from a web page (an @Origin@ header, which a browser sends and hsed's
client, the Dart conformance client and the Python fixture's do not) or
for a host other than 127.0.0.1 or localhost (a @Host@ a DNS name rebound
to 127.0.0.1 would carry) refused with 403 before anything else; served by
warp on the socket
"Hsed.Transport.Mhtx.Conn".'Hsed.Transport.Mhtx.Conn.bindLoopback' bound,
127.0.0.1 alone ('parseListen'; Scott's answer Q1).

Each peer has three threads, none outliving it: a reader, which stamps the
peer's liveness on every message, binary and junk included (hub.dart:83-86),
answers a ping with pong straight into the peer's send queue and hands
requests to the worker; a writer, which sends the queue in order; and a
worker, which answers the requests in order. Registering a peer (reading
@seq@, queueing its hello) and broadcasting (@seq@ + 1, the frame queued to
every peer) are each one STM transaction, so that a peer's hello and the
broadcasts it receives count every broadcast once (the guarantee hub.dart:66-77
has from running those steps with no await between them). A queue is
bounded in frames ('hoQueue') and in bytes ('hoBacklog'): the worker takes
a peer's next request only while the bytes queued to it and not yet
written are under the backlog, so that a client that sends and does not
read is held back by its own socket (TCP's flow control) rather than held
in the hub's memory; a peer whose queue is full in frames, or past the
backlog by more than a message ('Hsed.Transport.Mhtx.Conn.messageLimit',
which the broadcasts and pongs no worker holds back could reach), is
closed (1008) and removed. When the hub closes a peer (the sweep,
'hubClose', a full queue) it removes the peer at once, sends the close from
the peer's writer, and waits a second at most for the peer's echo
(conformance.dart:346 gives a close one second) before it ends the peer.

The command line hsed-sim reads is checked here too, so that its rules are
held by the suites: 'parseListen', 'heartbeatProblems', 'usageProblems',
'plainFileProblem' and 'readTokenFile'.
-}
module Hsed.Transport.Mhtx.Hub
  ( -- * Options
    HubOpts (..)
  , defaultHubOpts
  , parseListen
  , heartbeatProblems
  , usageProblems
    -- * The files hsed-sim reads
  , plainFileProblem
  , readTokenFile
    -- * The TPer behind it
  , Backend (..)
  , backendOf
    -- * The hub
  , Hub
  , newHub
  , hubServerId
  , hubSeq
  , hubClientCount
  , hubBacklog
  , hubServe
  , hubBroadcast
  , hubSweep
  , hubClose
  , runHub
  ) where

import qualified Crypto.Random                  as Random
import qualified Data.Aeson                     as A
import qualified Data.Aeson.KeyMap              as KeyMap
import qualified Data.ByteArray                 as BA
import qualified GHC.Clock                      as Clock
import qualified Network.HTTP.Types             as H
import           Network.Socket                 (Socket)
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import qualified RIO.ByteString                 as B
import qualified RIO.ByteString.Lazy            as BL
import qualified RIO.Char                       as C
import qualified RIO.List                       as L
import qualified RIO.Map                        as Map
import           RIO
import           RIO.Directory                  (canonicalizePath)
import qualified RIO.Text                       as T
#if defined(mingw32_HOST_OS)
import           RIO.Directory                  (doesFileExist)
#else
import           System.Posix.Files             (getFileStatus, isRegularFile)
#endif

import           Hsed.Sha256                    (sha256)
import           Hsed.Sim.Types                 (SimParams (..), foldProblemText, simParamsProblems)
import           Hsed.Sim.View                  (ComIdView (..), SimQuery (..), SimSetup (..), SomeQuery (..), answer, setupOf)
import           Hsed.Transport.InProc          (InProcOpts (..), SimControl (..), newInProc)
import           Hsed.Transport.Mhtx.Conn       (Conn (..), Incoming (..), codeBadToken, codeGoingAway, codeHeartbeat, codePolicy,
                                                 connectionOptions, fromWebSocket, messageLimit)
import           Hsed.Transport.Mhtx.Frame      (Allow (..), FromClient (..), Hello (..), Op (..), Reply (..), SedApp (..), SedEvt (..),
                                                 SedReq (..), SedRsp (..), answerValue, broadcastText, evtText, fromClient, helloText,
                                                 pongText, rspText, sedAppValue)
import           Hsed.Wire                      (Cap (..), Describe (..), IfStatus (..), Response (..), Transport, describeTransport,
                                                 ifRecv, ifSend, inject)


-- $setup
-- >>> :set -XOverloadedStrings

-- * Options

-- | How a hub runs: its heartbeat's interval and timeout (hello's
--   @heartbeat@, milliseconds), whether it allows the control ops (peek,
--   release), the reset op and the conformance hooks, the token a client
--   must give (none: none asked), its clock (seconds, monotonic), how
--   many frames a socket's send queue holds before the socket is closed,
--   and how many bytes queued to a socket and still unsent hold back its
--   requests (the backlog).
data HubOpts = HubOpts
  { hoIntervalMs   :: !Int
  , hoTimeoutMs    :: !Int
  , hoAllowControl :: !Bool
  , hoAllowReset   :: !Bool
  , hoHooks        :: !Bool
  , hoToken        :: !(Maybe ByteString)
  , hoNow          :: IO Double
  , hoQueue        :: !Int
  , hoBacklog      :: !Int
  }

-- | A heartbeat of 20 s and a timeout of 60 s (design 6.3's hello,
--   docs/spec-as-source.md:502), nothing allowed beyond commands and
--   describe, no token, GHC's monotonic clock in seconds (not RIO's, which
--   counts nanoseconds on macOS on Apple silicon: "Hsed.Wire"'s exchange
--   says so), a send queue of 1024 frames (the scaffold's bound,
--   reversible), and a backlog of 8 MiB, four messages at the frames' limit
--   (the M5c review's bound, reversible): what one socket can hold of the
--   hub's memory is about the backlog and a message more, not 1024 answers
--   of 1 MiB each.
defaultHubOpts :: HubOpts
defaultHubOpts = HubOpts
  { hoIntervalMs   = 20000
  , hoTimeoutMs    = 60000
  , hoAllowControl = False
  , hoAllowReset   = False
  , hoHooks        = False
  , hoToken        = Nothing
  , hoNow          = Clock.getMonotonicTime
  , hoQueue        = 1024
  , hoBacklog      = 4 * fromIntegral messageLimit
  }

-- | @--listen 127.0.0.1:PORT@: the port, 0 to 65535 in one to five decimal
--   digits (0 asks for an ephemeral one); or why not. Any other host is
--   refused, a name (@localhost@, which macOS resolves to ::1 first), an
--   IPv6 address, another IPv4 address, the wildcard address: hsed-sim
--   listens on 127.0.0.1 alone (Scott's answer Q1; until M6, its TLS and a
--   token required). No port is refused by its number: a port another
--   process holds fails the bind loudly (exit 1).
--
-- >>> parseListen "127.0.0.1:0"
-- Right 0
-- >>> parseListen "127.0.0.1:65535"
-- Right 65535
-- >>> parseListen "localhost:8080"
-- Left "host \"localhost\": 127.0.0.1 alone, the loopback address (Scott's answer Q1: no other until M6)"
-- >>> parseListen "192.0.2.1:8080"
-- Left "host \"192.0.2.1\": 127.0.0.1 alone, the loopback address (Scott's answer Q1: no other until M6)"
-- >>> parseListen "127.0.0.1:65536"
-- Left "port \"65536\": 0 to 65535, in decimal (0 an ephemeral port)"
-- >>> parseListen "127.0.0.1"
-- Left "\"127.0.0.1\": no port: 127.0.0.1:PORT"
parseListen :: String -> Either String Word16
parseListen s = case L.break (== ':') (reverse s) of
  (_, [])            -> Left (show s <> ": no port: 127.0.0.1:PORT")
  (rport, _ : rhost) -> do
    let host = reverse rhost
        port = reverse rport
    unless (host == "127.0.0.1") $
      Left ("host " <> show host <> ": 127.0.0.1 alone, the loopback address (Scott's answer Q1: no other until M6)")
    maybe (Left ("port " <> show port <> ": 0 to 65535, in decimal (0 an ephemeral port)")) Right (portNumber port)
  where
    portNumber digits
      | not (null digits), length digits <= 5, all C.isDigit digits
      , n <- L.foldl' (\acc d -> acc * 10 + toInteger (fromEnum d - fromEnum '0')) 0 digits, n <= 65535 = Just (fromInteger n)
      | otherwise = Nothing

-- | What is wrong with a heartbeat, its interval and its timeout in
--   milliseconds (@--heartbeat-interval-ms@, @--dead-timeout-ms@): none where
--   the hub can run it. The interval must be positive, a client pinging that
--   often (SPEC.md L57); the timeout above the interval, or a client pinging
--   every interval would be closed between two of its pings (L58-60).
--
-- >>> heartbeatProblems 200 600
-- []
-- >>> heartbeatProblems 0 600
-- ["--heartbeat-interval-ms 0: not positive (a client pings this often, SPEC.md L57)"]
-- >>> heartbeatProblems 600 600
-- ["--dead-timeout-ms 600: not above --heartbeat-interval-ms 600 (a client pinging every interval would be closed between its pings, SPEC.md L57-60)"]
heartbeatProblems :: Int -> Int -> [Text]
heartbeatProblems interval dead =
  [ "--heartbeat-interval-ms " <> tshow interval <> ": not positive (a client pings this often, SPEC.md L57)" | interval <= 0 ]
  <> [ "--dead-timeout-ms " <> tshow dead <> ": not above --heartbeat-interval-ms " <> tshow interval
       <> " (a client pinging every interval would be closed between its pings, SPEC.md L57-60)" | dead <= interval ]


-- | What is wrong with the command line hsed-sim was given, before it reads
--   a file or binds a socket (each a usage error, exit 2): the simulator's
--   parameters out of range ("Hsed.Sim.Types".'simParamsProblems'), a
--   heartbeat it cannot run ('heartbeatProblems'), and running as root
--   (euid 0, whether it does given; design 13.34 (28)(a): hsed-sim runs as
--   its user, never as root, whatever it was asked for).
--
-- >>> import Hsed.Sim.Types (defaultSimParams)
-- >>> usageProblems False defaultSimParams defaultHubOpts
-- []
-- >>> usageProblems True defaultSimParams defaultHubOpts
-- ["running as root (euid 0): hsed-sim runs as yourself, never as root"]
usageProblems :: Bool -> SimParams -> HubOpts -> [Text]
usageProblems root params opts =
  simParamsProblems params <> heartbeatProblems (hoIntervalMs opts) (hoTimeoutMs opts)
  <> [ "running as root (euid 0): hsed-sim runs as yourself, never as root" | root ]


-- * The files hsed-sim reads

-- | Why hsed-sim may not read a file a flag names (@--profile@,
--   @--token-file@), none where it may: it must be a regular file outside
--   @\/dev@, the path as given and the path its links resolve to both, which
--   is checked by its name and its status (neither opens it) before anything
--   reads it, so that nothing hsed-sim opens is a device (design 9.3: nothing
--   of the LAN leg reaches a drive).
plainFileProblem :: Text -> FilePath -> IO (Maybe Text)
plainFileProblem flag path = tryIO (canonicalizePath path) >>= \case
  Left e -> pure (Just (named <> ": " <> tshow e))
  Right resolved
    | any underDev [path, resolved] -> pure (Just (named <> ": under /dev, a device's directory: hsed-sim opens no device"))
    | otherwise                     -> regular resolved <&> \case
        Left e      -> Just (named <> ": " <> tshow e)
        Right False -> Just (named <> ": not a regular file")
        Right True  -> Nothing
  where
    named = flag <> " " <> T.pack path
    underDev p = p == "/dev" || "/dev/" `L.isPrefixOf` p
#if defined(mingw32_HOST_OS)
    regular = tryIO . doesFileExist
#else
    regular = tryIO . fmap isRegularFile . getFileStatus
#endif

-- | The token a file holds (@--token-file@): its bytes, a line ending at
--   their end (LF or CRLF, as an editor or @echo@ leaves one) left out; or
--   why there is none: the file unreadable, or no byte left. Read as bytes,
--   since mh_transport's hubs read no token themselves (the caller checks
--   it before the hub serves, mh_transport.py:97-98), and compared with the
--   @token@ query parameter as its percent-decoding gives it (a request
--   carrying the parameter more than once taken as carrying none). Never logged:
--   a reason names the file, never its contents.
readTokenFile :: FilePath -> IO (Either Text ByteString)
readTokenFile path = tryIO (readFileBinary path) <&> \case
  Left e -> Left ("--token-file " <> T.pack path <> ": " <> tshow e)
  Right bytes
    | B.null (endless bytes) -> Left ("--token-file " <> T.pack path <> ": holds no token")
    | otherwise              -> Right (endless bytes)
  where
    endless b = fromMaybe b (B.stripSuffix "\r\n" b <|> B.stripSuffix "\n" b)


-- * The TPer behind it

-- | The one TPer a hub serves: the in-process simulator's transport and
--   control ("Hsed.Transport.InProc".'Hsed.Transport.InProc.newInProc'),
--   and the @app.sed@ its hello carries.
data Backend = Backend
  { beTransport :: !(Transport 'ReadWrite)
  , beControl   :: !SimControl
  , beApp       :: !SedApp
  }

-- | The backend of a simulator of these parameters and transport options,
--   its @app.sed@ saying what the options allow; or why there is none: the
--   parameters out of range ("Hsed.Sim.Types".'simParamsProblems'), or
--   folding with no SP ('newInProc'). The @app.sed@ is the transport's own
--   description (a TPer, @hsed-sim@ and the version, the unit, Opal 2.30,
--   the base ComID and their count), the errata the simulator was compiled
--   under, @scott@ (hsedc's @--errata scott@, design E3: hsed-sim takes no
--   other), what is allowed (peek and release with @--allow-remote-control@,
--   reset with @--allow-remote-reset@), and the setup
--   ("Hsed.Sim.View".'setupOf' of the parameters at the transport's unit and
--   hold), the SID's PIN and the MSID left out of it unless control is
--   allowed, whose peeks read them anyway.
backendOf :: HubOpts -> SimParams -> InProcOpts -> IO (Either Text Backend)
backendOf opts params inProc = case simParamsProblems p of
  problems@(_ : _) -> pure (Left (T.intercalate "; " problems))
  []               -> newInProc p inProc <&> \case
    Left problems         -> Left (T.intercalate "; " (map foldProblemText problems))
    Right (tr, control) -> Right (Backend tr control (appOf (describeTransport tr)))
  where
    p = params { spUnits = ioUnits inProc }
    appOf d = SedApp
      { saRole      = dRole d
      , saImpl      = dImpl d
      , saUnits     = dUnits d
      , saSsc       = dSsc d
      , saErrata    = Just "scott"
      , saBaseComId = dBaseComId d
      , saNComIds   = dNComIds d
      , saAllows    = filter allowed [minBound .. maxBound]
      , saSetup     = Just (published (setupOf p (ioHoldIfRecvs inProc)))
      }
    allowed = \case
      AllowPeek    -> hoAllowControl opts
      AllowRelease -> hoAllowControl opts
      AllowReset   -> hoAllowReset opts
    published s
      | hoAllowControl opts = s
      | otherwise           = s { ssSidPin = Nothing, ssMsid = Nothing }


-- * The hub

-- | A hub: its options, its TPer, its server id, the @app@ its hellos
--   carry, its broadcasts so far, its peers by number, the next number, and
--   the lock around every operation on the TPer.
data Hub = Hub
  { hOpts    :: !HubOpts
  , hBackend :: !Backend
  , hId      :: !Text
  , hApp     :: A.Object
  , hSeq     :: !(TVar Integer)
  , hPeers   :: !(TVar (Map Int Peer))
  , hNext    :: !(TVar Int)
  , hLock    :: !(MVar ())
  }

-- | A peer: its number; the frames to send it, in order, and their bytes;
--   its requests for the worker, in order; the hub's clock when it last
--   sent anything; the close the hub asked for it (none yet); whether that
--   close was sent; and whether its serving is over.
data Peer = Peer
  { pKey   :: !Int
  , pOut   :: !(TBQueue ByteString)
  , pBytes :: !(TVar Int)
  , pIn    :: !(TBQueue FromClient)
  , pSeen  :: !(TVar Double)
  , pClose :: !(TVar (Maybe (Word16, Text)))
  , pSent  :: !(TVar Bool)
  , pDone  :: !(TVar Bool)
  }

-- | A hub over a backend, no peer yet, its server id drawn ('serverId').
newHub :: HubOpts -> Backend -> IO Hub
newHub opts backend = do
  sid <- serverId
  Hub opts backend sid (KeyMap.singleton "sed" (sedAppValue (beApp backend)))
    <$> newTVarIO 0 <*> newTVarIO mempty <*> newTVarIO 0 <*> newMVar ()

-- | Its server id: 12 characters of [a-z0-9], one for the process.
hubServerId :: Hub -> Text
hubServerId = hId

-- | Its broadcasts so far.
hubSeq :: Hub -> IO Integer
hubSeq = readTVarIO . hSeq

-- | Its peers now.
hubClientCount :: Hub -> IO Int
hubClientCount = fmap Map.size . readTVarIO . hPeers

-- | The bytes queued to its peers now and not yet taken by their writers.
hubBacklog :: Hub -> IO Int
hubBacklog hub = atomically (readTVar (hPeers hub) >>= fmap sum . traverse (readTVar . pBytes) . Map.elems)

-- | Serve one connection until it closes, given the token its request
--   carried (the @token@ query parameter; none, none given): where the hub
--   asks for one (@--token-file@), a token missing or not equal to it
--   ('tokenAccepted') has the connection closed with 4001 before
--   hello (SPEC.md L92-95), and is never logged. 'runHub' reads the token
--   from the upgrade's request; a test over memory gives it here.
hubServe :: Hub -> Maybe ByteString -> Conn -> IO ()
hubServe hub given conn
  | not (tokenAccepted (hoToken (hOpts hub)) given) = do
      connClose conn codeBadToken "token missing or wrong"
      void (timeout closeWaitMicros (untilClosed conn))
  | otherwise = bracket (attach hub) (detach hub) $ \peer ->
      withAsync (writer conn peer) $ \w ->
      withAsync (worker hub peer) $ \k ->
        link k >> race_ (reader hub conn peer) (race_ (writerOver w) (closedByHub peer))
  where
    -- the writer's end: a send that failed ends the peer, the close sent waits for the echo
    writerOver w = waitCatch w >>= either (const (pure ())) (const (threadDelay closeWaitMicros))
    closedByHub peer = atomically (readTVar (pClose peer) >>= maybe retrySTM (const (pure ()))) >> threadDelay closeWaitMicros

-- | Broadcast an object with the next @seq@, which it returns; or why not:
--   a type of mh_transport's own (hello, ping, pong: SPEC.md L23-29), one of
--   hsed's application layer (@sed.*@, which the hub alone sends, design
--   6.3), no string type at all (every frame has one, SPEC.md L18-19), or a
--   frame, the object with its @seq@, past the frames' limit, which no peer
--   of hsed's would read ("Hsed.Transport.Mhtx.Conn".'messageLimit'; @seq@
--   not advanced). The object's own @seq@, if it has one, is the hub's.
hubBroadcast :: Hub -> A.Object -> IO (Either Text Integer)
hubBroadcast hub = fmap (first snd) . broadcastObject hub

-- | Close every peer silent at or past the timeout, with 4000 and the reason
--   @heartbeat timeout@ (SPEC.md L58-60), each removed at once.
hubSweep :: Hub -> IO ()
hubSweep hub = do
  now <- hoNow (hOpts hub)
  atomically $ do
    peers  <- Map.elems <$> readTVar (hPeers hub)
    silent <- filterM (fmap (\seen -> now - seen >= timeoutSeconds) . readTVar . pSeen) peers
    traverse_ (\p -> closePeer hub p codeHeartbeat "heartbeat timeout") silent
  where
    timeoutSeconds = fromIntegral (hoTimeoutMs (hOpts hub)) / 1000 :: Double

-- | Close every peer, with 1001 (the hub going away, RFC 6455 7.4.1), each
--   removed at once and its close sent before it returns (a second at most);
--   and keep serving: a peer may connect again, to the same server id and
--   @seq@ (so that a test can make clients reconnect with nothing missed).
--   hsed-sim calls it before it exits; a second call finds no peer.
hubClose :: Hub -> IO ()
hubClose hub = do
  kicked <- atomically $ do
    peers <- Map.elems <$> readTVar (hPeers hub)
    traverse_ (\p -> closePeer hub p codeGoingAway "going away") peers
    pure peers
  void . timeout closeWaitMicros . atomically $
    traverse_ (\p -> (||) <$> readTVar (pSent p) <*> readTVar (pDone p) >>= checkSTM) kicked

-- | Serve the hub on a listening socket, the one
--   "Hsed.Transport.Mhtx.Conn".'Hsed.Transport.Mhtx.Conn.bindLoopback'
--   bound (warp's runSettingsSocket, which binds nothing), and sweep its
--   peers every heartbeat interval ('hubSweep'), until killed: what
--   hsed-sim runs and the LAN suite's hubs run, nothing else beside it.
--   @\/ws@ upgraded to a websocket, the token read from its query; @\/ws@
--   without an upgrade 426; with @--conformance-hooks@ @POST \/emit@ and
--   @GET \/clients@; anything else 404. warp's connection timeout is set
--   above the heartbeat's window ('warpTimeout'), so that warp never cuts a
--   websocket before the hub's sweep would: warp tickles its timeout at every
--   byte a websocket sends or receives (its Response.hs:254-262), which undoes
--   the pause warp offers (Warp.hs:490-491: a tickle sets the handle active
--   again), and kills a connection idle through two of its periods, so that
--   at warp's 30 s a client silent for 30 to 60 s, which the hub's default
--   heartbeat keeps until 60 s and then closes with 4000, would be cut first
--   with no close code (SoakSpec shows it). An exception warp would show is
--   written to stderr, but a handshake's, which holds the request's path,
--   its token with it.
runHub :: Hub -> Socket -> IO ()
runHub hub sock = race_ sweeping (Warp.runSettingsSocket settings sock (application hub))
  where
    sweeping = forever (threadDelay intervalMicros >> hubSweep hub)
    intervalMicros = fromInteger (min (toInteger (maxBound :: Int)) (toInteger (hoIntervalMs (hOpts hub)) * 1000))
    settings = Warp.setTimeout (warpTimeout (hOpts hub)) (Warp.setOnException shown Warp.defaultSettings)
    shown _ e
      | handshake e                          = pure ()
      | Warp.defaultShouldDisplayException e = hPutBuilder stderr (getUtf8Builder ("hsed hub: " <> displayShow e <> "\n"))
      | otherwise                            = pure ()
    handshake = isJust . (fromException :: SomeException -> Maybe WS.HandshakeException)


-- * Internals

-- | warp's connection timeout for a hub, in seconds: past the most a
--   websocket's silence lasts before the hub's sweep closes it (the timeout
--   and an interval, 'hubSweep' running every interval), a second more,
--   and never below warp's own 30 s, which an HTTP request to the hooks
--   keeps. warp kills a connection only after more than one such period of
--   silence, so that the hub's sweep is always the first to close a silent
--   websocket, with its 4000.
--
-- >>> warpTimeout defaultHubOpts
-- 81
-- >>> warpTimeout defaultHubOpts { hoIntervalMs = 200, hoTimeoutMs = 600 }
-- 30
warpTimeout :: HubOpts -> Int
warpTimeout opts = fromInteger (max 30 (min most (seconds + 1)))
  where
    seconds = (toInteger (hoTimeoutMs opts) + toInteger (hoIntervalMs opts) + 999) `div` 1000
    most    = toInteger (maxBound :: Int) `div` 1000000   -- warp counts it in microseconds, an Int

-- | How long a peer the hub closed is kept for its echo of the close: a
--   second, the wait conformance.dart:346 gives a close.
closeWaitMicros :: Int
closeWaitMicros = 1000000

-- | The most an @/emit@ body may hold: 2 MiB, the frames' own limit
--   ("Hsed.Transport.Mhtx.Conn".'messageLimit'), read no further; its
--   broadcast, the object as aeson writes it with @seq@ inserted, is held to
--   the same limit by 'broadcastWith', so that a body just under it whose
--   broadcast is not refused 413 rather than sent to peers that would
--   close on it.
emitMost :: Int
emitMost = fromIntegral messageLimit

-- | A server id: 12 characters of [a-z0-9] (hub.dart:170-173), each drawn
--   uniformly from the system's random bytes (cryptonite's 'Random.getRandomBytes'),
--   a byte at or past 252 (7 times 36) drawn again, so that no character is
--   likelier than another.
serverId :: IO Text
serverId = drawn ""
  where
    drawn acc
      | T.length acc >= 12 = pure (T.take 12 acc)
      | otherwise          = Random.getRandomBytes 24 >>= \bytes ->
          drawn (acc <> T.concat [ T.take 1 (T.drop (fromIntegral b `mod` 36) alphabet) | b <- B.unpack (bytes :: ByteString), b < 252 ])
    alphabet = "abcdefghijklmnopqrstuvwxyz0123456789"

-- | Whether a token a client gave is the one the hub asks for (none asked:
--   any): their SHA-256 digests compared in constant time (memory's
--   'BA.constEq', which returns at once on two lengths that differ, so that
--   comparing the tokens themselves would tell a client the token's length).
tokenAccepted :: Maybe ByteString -> Maybe ByteString -> Bool
tokenAccepted want given = case want of
  Nothing -> True
  Just w  -> maybe False (BA.constEq (sha256 w) . sha256) given

-- | A connection read until it closes.
untilClosed :: Conn -> IO ()
untilClosed conn = connReceive conn >>= \case
  InClosed _ -> pure ()
  _          -> untilClosed conn

-- | A new peer registered: in one transaction its number taken, @seq@ read,
--   its hello at that @seq@ queued first, and the peer added.
attach :: Hub -> IO Peer
attach hub = do
  now <- hoNow opts
  atomically $ do
    key <- readTVar (hNext hub)
    writeTVar (hNext hub) (key + 1)
    out <- newTBQueue depth
    seq' <- readTVar (hSeq hub)
    let hello = helloText (Hello 1 (hId hub) seq' (hoIntervalMs opts) (hoTimeoutMs opts) (hApp hub))
    writeTBQueue out hello
    peer <- Peer key out <$> newTVar (B.length hello) <*> newTBQueue depth <*> newTVar now <*> newTVar Nothing <*> newTVar False
                         <*> newTVar False
    modifyTVar' (hPeers hub) (Map.insert key peer)
    pure peer
  where
    opts = hOpts hub
    depth = fromIntegral (max 1 (hoQueue opts))

-- | A peer's serving over: removed, if the hub has not removed it already.
detach :: Hub -> Peer -> IO ()
detach hub peer = atomically $ do
  modifyTVar' (hPeers hub) (Map.delete (pKey peer))
  writeTVar (pDone peer) True

-- | A peer closed by the hub: removed at once, the close asked of its
--   writer (the first close asked stands).
closePeer :: Hub -> Peer -> Word16 -> Text -> STM ()
closePeer hub peer code why = do
  modifyTVar' (hPeers hub) (Map.delete (pKey peer))
  readTVar (pClose peer) >>= \case
    Nothing -> writeTVar (pClose peer) (Just (code, why))
    Just _  -> pure ()

-- | A frame queued for a peer, its bytes counted; a peer whose queue is
--   full closed instead (1008), and nothing queued: full in frames
--   ('hoQueue'), or past the backlog by more than a message
--   ('hoBacklog', 'messageLimit'), which only frames no worker holds back
--   reach (broadcasts, pongs), the worker queueing an answer only under the
--   backlog.
enqueue :: Hub -> Peer -> ByteString -> STM ()
enqueue hub peer frame = do
  full   <- isFullTBQueue (pOut peer)
  queued <- readTVar (pBytes peer)
  if full || queued + B.length frame > hoBacklog (hOpts hub) + fromIntegral messageLimit
    then closePeer hub peer codePolicy "send queue full"
    else writeTBQueue (pOut peer) frame >> writeTVar (pBytes peer) (queued + B.length frame)

-- | A frame stamped with the next @seq@ broadcast: @seq@ + 1 and the frame
--   queued to every peer in one transaction (hub.dart:103-111); the @seq@,
--   or the frame's length where it is past the frames' limit
--   ('messageLimit'), nothing broadcast and @seq@ not advanced.
broadcastWith :: Hub -> (Integer -> ByteString) -> IO (Either Int Integer)
broadcastWith hub frameAt = atomically $ do
  n <- (+ 1) <$> readTVar (hSeq hub)
  let frame = frameAt n
  if fromIntegral (B.length frame) > messageLimit
    then pure (Left (B.length frame))
    else do
      writeTVar (hSeq hub) n
      peers <- Map.elems <$> readTVar (hPeers hub)
      traverse_ (\p -> enqueue hub p frame) peers
      pure (Right n)

-- | 'hubBroadcast' with the status @\/emit@ answers a refusal with: 400 for
--   a type or none, 413 for a frame past the frames' limit.
broadcastObject :: Hub -> A.Object -> IO (Either (H.Status, Text) Integer)
broadcastObject hub o = case KeyMap.lookup "type" o of
  Just (A.String t)
    | t `elem` ["hello", "ping", "pong"] -> refused ("type " <> t <> ": mh_transport's own, never broadcast (SPEC.md L23-29)")
    | "sed." `T.isPrefixOf` t           -> refused ("type " <> t <> ": hsed's own layer, which the hub alone sends (design 6.3)")
    | otherwise                         -> first tooBig <$> broadcastWith hub (`broadcastText` o)
  _ -> refused "no string type: every frame is a JSON object with a string type (SPEC.md L18-19)"
  where
    refused why = pure (Left (H.status400, why))
    tooBig n = (H.status413, "a broadcast of " <> tshow n <> " bytes with its seq, past the frames' limit of " <> tshow messageLimit
                             <> " (no peer would read it)")

-- | A peer's reader: every message stamps the peer's liveness (hub.dart:83-86,
--   binary and junk included), a stamp never going back (a monotonic clock's
--   own order, kept whatever clock the options give); a ping is answered with
--   pong at once, a request handed to the worker (a peer with too many
--   waiting closed, 1008), anything else ignored (SPEC.md L19-21); once the
--   hub has asked for the peer's close, nothing is answered. It ends when the
--   connection closes.
reader :: Hub -> Conn -> Peer -> IO ()
reader hub conn peer = connReceive conn >>= \case
  InClosed _ -> pure ()
  incoming   -> do
    sorted <- evaluate (fromClient incoming)
    now <- hoNow (hOpts hub)
    atomically $ do
      modifyTVar' (pSeen peer) (max now)
      closing <- isJust <$> readTVar (pClose peer)
      unless closing $ case sorted of
        ClientPing    -> enqueue hub peer pongText
        ClientIgnored -> pure ()
        request       -> isFullTBQueue (pIn peer) >>= \case
          True  -> closePeer hub peer codePolicy "too many requests waiting"
          False -> writeTBQueue (pIn peer) request
    reader hub conn peer

-- | A peer's writer: its queue sent in order, each frame's bytes no longer
--   counted once it is taken, until the hub asks for its close, which it
--   sends instead of anything still queued.
writer :: Conn -> Peer -> IO ()
writer conn peer = atomically (closing `orElse` queued) >>= \case
  Left (code, why) -> connClose conn code why >> atomically (writeTVar (pSent peer) True)
  Right frame      -> connSendText conn frame >> writer conn peer
  where
    closing = Left <$> (readTVar (pClose peer) >>= maybe retrySTM pure)
    queued  = do
      frame <- readTBQueue (pOut peer)
      modifyTVar' (pBytes peer) (subtract (B.length frame))
      pure (Right frame)

-- | A peer's worker: its requests answered in arrival order, each on the
--   peer's own socket, the next taken only while the bytes queued to the
--   peer are under the backlog ('hoBacklog'), so that a peer that does not
--   read holds back its own requests. A request whose id is not above the last one
--   accepted on the socket, or one refused as it was read, is answered
--   @protocol_error@ (its id where it had an integer one) and leaves the last
--   accepted id as it was (design 6.3 as M5c decides it).
worker :: Hub -> Peer -> IO ()
worker hub peer = go 0
  where
    go lastId = atomically (readTVar (pBytes peer) >>= checkSTM . (< hoBacklog (hOpts hub)) >> readTBQueue (pIn peer)) >>= \case
      ClientBadReq rid why -> reply (SedRsp rid (ProtocolError why)) >> go lastId
      ClientReq (SedReq rid op)
        | rid <= lastId -> reply (SedRsp (Just rid) (ProtocolError ("id " <> tshow rid <> " is not above " <> tshow lastId
                                                                    <> ", the last accepted on this socket"))) >> go lastId
        | otherwise     -> (opReply hub op >>= reply . SedRsp (Just rid)) >> go rid
      _ -> go lastId
    reply rsp = evaluate (rspText rsp) >>= atomically . enqueue hub peer

-- | An op's reply: IF-SEND and IF-RECV on the TPer ("Hsed.Wire"'s, whose
--   length past what a transport carries the in-process transport answers
--   itself); the reset, with @--allow-remote-reset@, its event caused and,
--   good, broadcast as @sed.evt@ (every ComID, the sessions open before it)
--   before the reply is queued; describe, always; peek and release, with
--   @--allow-remote-control@. An op not allowed is answered unsupported,
--   naming the flag that allows it. Each op on the TPer holds the hub's
--   lock, so that a reset's view, its event and its broadcast are one step
--   to every other socket.
opReply :: Hub -> Op -> IO Reply
opReply hub = \case
  OpIfSend p c bytes -> locked (answered <$> ifSend tr p c (fromIntegral (B.length bytes)) bytes)
  OpIfRecv p c len   -> locked (answered <$> ifRecv tr p c len)
  OpDescribe         -> pure (Described (beApp backend))
  OpReset e
    | hoAllowReset opts   -> locked (reset e)
    | otherwise           -> pure (unsupported "the reset op" "--allow-remote-reset")
  OpPeek (SomeQuery q)
    | hoAllowControl opts -> locked (Viewed . answerValue q . answer q <$> scPeek ctl)
    | otherwise           -> pure (unsupported "the peek op" "--allow-remote-control")
  OpRelease c
    | hoAllowControl opts -> locked (scRelease ctl c $> Answered Good mempty)
    | otherwise           -> pure (unsupported "the release op" "--allow-remote-control")
  where
    opts    = hOpts hub
    backend = hBackend hub
    tr      = beTransport backend
    ctl     = beControl backend
    locked  = withMVar (hLock hub) . const
    answered r = Answered (status r) (rpayload r)
    unsupported what flag = Answered (Unsupported (what <> " is not allowed: hsed-sim allows it with " <> flag)) mempty
    -- the event and its broadcast one step even to a cancel (the requester gone), so that no peer misses a reset that
    -- happened; neither blocks, the hub's lock held
    reset e = do
      st <- scPeek ctl
      let comIds   = map cvComId (answer ComIds st)
          sessions = answer Sessions st
      uninterruptibleMask_ $ do
        s <- inject tr e
        when (s == Good) . void $ broadcastWith hub (\n -> evtText (SedEvt n e comIds sessions))
        pure (Answered s mempty)

-- | The hub's HTTP application: a request from a web page or for another
--   host refused ('foreignRequest', 403); then @\/ws@, the hooks (the token
--   asked of them as of a websocket, 401 where it is missing or wrong), 404.
application :: Hub -> Wai.Application
application hub req respond = case (Wai.requestMethod req, Wai.pathInfo req) of
  _ | Just why <- foreignRequest req -> respond (plain H.status403 [] why)
  (_, ["ws"]) -> case WaiWS.websocketsApp connectionOptions upgraded req of
    Just upgrade -> respond upgrade
    Nothing      -> respond (plain (H.mkStatus 426 "Upgrade Required") [("Upgrade", "websocket")] "ws: a websocket's path: ask for an upgrade (RFC 6455 4.1)")
  (m, ["emit"])    | hooks, m == H.methodPost -> withToken (emitted hub req)
  (m, ["clients"]) | hooks, m == H.methodGet  -> withToken (json H.status200 . A.object . pure . ("count" A..=) <$> hubClientCount hub)
  _ -> respond (plain H.status404 [] "not found")
  where
    hooks = hoHooks (hOpts hub)
    upgraded pending = WS.acceptRequest pending >>= hubServe hub token . fromWebSocket (tshow (Wai.remoteHost req))
    -- the token parameter given once (a request carrying it twice carries none), its value or empty
    token = case [ fromMaybe "" v | (k, v) <- Wai.queryString req, k == "token" ] of
      [t] -> Just t
      _   -> Nothing
    withToken answered
      | tokenAccepted (hoToken (hOpts hub)) token = answered >>= respond
      | otherwise = respond (json H.status401 (A.object ["error" A..= ("token missing or wrong" :: Text)]))

-- | Why a request is not a local client's, if it is not: one carrying an
--   @Origin@ header, which a browser sends with a page's websocket or
--   cross-origin POST (RFC 6455 4.1 item 8; the Fetch standard) and none
--   of the hub's clients does (hsed's, conformance.dart's dart:io socket,
--   the Python fixture's tests); or one whose @Host@ names a host other than
--   127.0.0.1 or localhost, as a page's would through a DNS name rebound to
--   127.0.0.1. A request with no @Host@ (HTTP/1.0) is taken as local, the
--   socket being 127.0.0.1's alone.
--
-- >>> let req hs = Wai.defaultRequest { Wai.requestHeaders = hs, Wai.requestHeaderHost = L.lookup "Host" hs }
-- >>> map (isJust . foreignRequest . req) [[("Host", "127.0.0.1:8080")], [("Host", "localhost")], [], [("Host", "evil.example:8080")], [("Host", "127.0.0.1:80"), ("Origin", "http://127.0.0.1:80")]]
-- [False,False,False,True,True]
foreignRequest :: Wai.Request -> Maybe Text
foreignRequest req
  | isJust (L.lookup "Origin" (Wai.requestHeaders req)) =
      Just "a request from a web page (an Origin header): the hub serves local clients alone, never a browser's page"
  | Just host <- Wai.requestHeaderHost req, B.takeWhile (/= 58) host `notElem` ["127.0.0.1", "localhost"] =
      Just "a request for another host (its Host header): the hub serves 127.0.0.1 alone"
  | otherwise = Nothing

-- | @POST \/emit@: a JSON object broadcast ('hubBroadcast'), answered 200
--   @{"seq": n}@ (conformance_fixture.py:34-36); a body that is not a JSON
--   object, or an object 'hubBroadcast' refuses for its type, 400 with the
--   reason (SPEC.md and the fixture give no status: the Python fixture's
--   own is an exception's 500, conformance_fixture.py:35); a body past
--   'emitMost', or one whose broadcast, the object with its @seq@, is past
--   the frames' limit, 413.
emitted :: Hub -> Wai.Request -> IO Wai.Response
emitted hub req = bounded 0 [] >>= \case
  Nothing   -> pure (json H.status413 (failure ("a body past " <> tshow emitMost <> " bytes, the frames' limit")))
  Just body -> case A.eitherDecodeStrict body of
    Right (A.Object o) -> broadcastObject hub o <&> either (\(s, why) -> json s (failure why)) (json H.status200 . A.object . pure . ("seq" A..=))
    Right _            -> pure (json H.status400 (failure "not a JSON object: a broadcast is one (SPEC.md L18-19)"))
    Left e             -> pure (json H.status400 (failure ("not JSON: " <> T.pack e)))
  where
    bounded n chunks = Wai.getRequestBodyChunk req >>= more n chunks
    more n chunks chunk
      | B.null chunk                  = pure (Just (mconcat (reverse chunks)))
      | n + B.length chunk > emitMost = pure Nothing
      | otherwise                     = bounded (n + B.length chunk) (chunk : chunks)
    failure :: Text -> A.Value
    failure why = A.object ["error" A..= why]

-- | A response of JSON.
json :: H.Status -> A.Value -> Wai.Response
json s = sized s [(H.hContentType, "application/json")] . A.encode

-- | A response of plain text, a line.
plain :: H.Status -> H.ResponseHeaders -> Text -> Wai.Response
plain s headers = sized s ((H.hContentType, "text/plain; charset=utf-8") : headers) . BL.fromStrict . encodeUtf8 . (<> "\n")

-- | A response whose body's length its headers give, so that warp sends it
--   as it is, not in HTTP/1.1's chunks (warp's Response.hs:378-381), which a
--   raw reader would have to undo.
sized :: H.Status -> H.ResponseHeaders -> BL.ByteString -> Wai.Response
sized s headers body = Wai.responseLBS s ((H.hContentLength, encodeUtf8 (tshow (BL.length body))) : headers) body
