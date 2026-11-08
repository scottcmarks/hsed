{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-|
Module      : Hsed.Transport.Mhtx.Client
Description : The mh_transport client of a simulator's hub: a 'ReadWrite transport, its view and its control over the LAN leg
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The jig's side of the LAN leg (design 7 item 4, 11 M5c; docs/wire.md, a
client's rules). One connection manager per client, opened by one attempt:
connected within the dialer's wait, then a hello within its own, required
(mh_transport SPEC.md L99-104, @requireHello@), whose @v@ is 1 (L45) and
whose @app.sed@ reads and says a TPer answers, in units it knows. Once open
the manager keeps the client connected: a ping every @interval_ms@ after
hello (L57), a pong for a ping (L65), any message proof of life and silence
past the interval and a grace dropping the connection (L60-62: any
message, binary and undecodable ones too, wider than the Dart client's
decoded frames), and after any drop a reconnect with equal-jitter backoff
('backoffDelay', L108-111), its count reset by each connection taken, one
attempt at a time. Each reconnect's hello is judged for broadcasts missed
('judgeMissed', L78-85): the verdicts are kept ('clientVerdicts'), and one
other than none is noticed ('clientNotices'): a new server_id, the hub
restarted, a new TPer at its Original Factory State; the same server_id
and a greater seq, broadcasts missed, a reset perhaps. A reconnect whose
@app.sed@ is not the first's latches the client: every request fails from
then on.

Requests carry ids from 1, increasing for the client's life, none reused
across connections (design 6.3 as M5c decides it); each is registered
before its frame is written and answered by its id alone (an unknown id
ignored; a @sed.rsp@ that carries @seq@, a broadcast's, never taken as an
answer: "Hsed.Transport.Mhtx.Frame".'fromServer' ignores it). The wait for
an answer blocks interruptibly ("Hsed.Jig.Transcript".@recording@ runs the
exchange masked, and the host's exchange cuts it with a timeout), is
bounded past the host's own (15 s to the host's 10 s, so that a slow hub
is cut by the host as the in-process simulator would be), and leaves no
entry behind however it ends, a late answer then ignored. A connection lost
fails every request sent on it with a transport failure, and none is ever
sent again: an IF-RECV in S3 delivers its packets once ("Hsed.Sim.Sync"),
so that a retry would read "all returned" and lose them. A request issued
while the client is not connected waits for the reconnect (at most its
wait), then is sent if the new hello says nothing was missed, and fails
naming what happened otherwise.

It gives the run a @'ReadWrite@ transport ('clientTransport') that
answers as the in-process one does ("Hsed.Transport.InProc"): a transfer
past what its unit carries refused Other Invalid Command Parameter with
nothing sent; IF-SEND and IF-RECV as the hub's @if_send@ and @if_recv@, the
status and bytes as the hub's simulator gave them; a @protocol_error@ or a
connection lost as a transport failure naming it; an event as the hub's
@reset@ op (its @sed.evt@, which the hub broadcasts before the answer,
taken as this client's own; another's noticed), an unsupported answer
mapped with its detail; its unit and description from @app.sed@, the
device none, as in process; closing it closes the client. Its view
('clientView') asks the hub's @peek@ op, its control ('clientControl') the
@release@ op, each refused by a hub without @--allow-remote-control@.
-}
module Hsed.Transport.Mhtx.Client
  ( -- * Reconnecting
    Backoff (..)
  , defaultBackoff
  , backoffDelay
  , Missed (..)
  , judgeMissed
    -- * Options
  , Dialer
  , ClientOpts (..)
  , clientOpts
    -- * The client
  , MhtxClient
  , openMhtx
  , clientApp
  , clientTransport
  , clientView
  , clientControl
  , clientNotices
  , clientVerdicts
  , clientInFlight
  , clientWaiting
  , clientSettled
  , closeMhtx
  ) where

import qualified GHC.Clock                 as Clock
import qualified RIO.ByteString            as B
import qualified RIO.Map                   as Map
import           RIO
import qualified RIO.Set                   as Set
import qualified RIO.Text                  as T
import           System.Random             (randomRIO)

import           Hsed.Sim.View             (Control (..), SimView (..), SomeQuery (..))
import           Hsed.Transport.Mhtx.Conn  (Conn (..), Incoming (..), codeHeartbeat, codeNormal, codePolicy, dialLoopback)
import           Hsed.Transport.Mhtx.Frame (FromServer (..), Hello (..), Op (..), Reply (..), SedApp (..), SedEvt (..), SedReq (..),
                                            SedRsp (..), fromServer, mostId, parseAnswer, pingText, pongText, reqText, sedAppOf)
import           Hsed.Wire                 (exactly, ifStatusDetail, ifStatusName, mostCarried)
import           Hsed.Wire.Internal        (Cap (..), Describe (..), Dir (..), Event (..), IfStatus (..), Request (..),
                                            Response (..), Transport (..))


-- $setup
-- >>> :set -XOverloadedStrings

-- * Reconnecting

-- | Exponential backoff with equal jitter: the delay before retry @n@ is
--   the initial delay times the factor to the @n@, capped, of which the
--   jitter's fraction is uniformly random (mh_transport's backoff.dart:5-35).
data Backoff = Backoff
  { boInitialMicros :: !Int
  , boFactor        :: !Double
  , boMaxMicros     :: !Int
  , boJitter        :: !Double
  }
  deriving (Eq, Show)

-- | 1 s doubling to a 30 s cap, half the delay random (SPEC.md L108-110;
--   backoff.dart:5-12).
defaultBackoff :: Backoff
defaultBackoff = Backoff
  { boInitialMicros = 1000000
  , boFactor        = 2
  , boMaxMicros     = 30000000
  , boJitter        = 0.5
  }

-- | The delay in microseconds before retry @n@ (0 the first), given a
--   uniform draw in [0, 1): backoff.dart:27-35's, in its arithmetic (IEEE
--   doubles, the cap taken before the jitter, the sum rounded as Dart's
--   @round@ does, a half away from zero).
--
-- >>> map (\n -> backoffDelay defaultBackoff n 0.5) [0, 1, 4, 5, 9]
-- [750000,1500000,12000000,22500000,22500000]
backoffDelay :: Backoff -> Int -> Double -> Int
backoffDelay b attempt draw = roundAway (fixed + spread)
  where
    capped = min (fromIntegral (boMaxMicros b)) (fromIntegral (boInitialMicros b) * boFactor b ** fromIntegral attempt)
    fixed  = capped * (1 - boJitter b)
    spread = capped * boJitter b * draw

-- | A double rounded as Dart's @round@ rounds it: to the nearest integer, a
--   half away from zero.
roundAway :: Double -> Int
roundAway x
  | frac >= 0.5  = whole + 1
  | frac <= -0.5 = whole - 1
  | otherwise    = whole
  where
    whole = truncate x
    frac  = x - fromIntegral whole

-- | What a reconnect's hello says of the broadcasts missed while away
--   (SPEC.md L78-85).
data Missed
  = MissedNone
  | MissedSome
  | MissedUnknown
  deriving (Eq, Show, Enum, Bounded)

-- | The verdict on a hello: whether it is a reconnect, the server id and
--   @seq@ last seen, the hello (none, the server sent none): client.dart:397-406's.
--   Not a reconnect, no hello, nothing seen or another server id: unknown;
--   the same server id and the same @seq@: none; a greater one: some; a
--   smaller one (a counter gone back): unknown.
--
-- >>> import qualified Data.Aeson.KeyMap as KM
-- >>> let hello s = Hello 1 "k3v9x2m8p1qz" s 20000 60000 KM.empty
-- >>> map (judgeMissed True (Just ("k3v9x2m8p1qz", 42)) . Just . hello) [42, 43, 41]
-- [MissedNone,MissedSome,MissedUnknown]
judgeMissed :: Bool -> Maybe (Text, Integer) -> Maybe Hello -> Missed
judgeMissed isReconnect lastSeen hello = case (lastSeen, hello) of
  (Just (serverId, lastSeq), Just h)
    | isReconnect, helloServerId h == serverId, helloSeq h == lastSeq -> MissedNone
    | isReconnect, helloServerId h == serverId, helloSeq h > lastSeq  -> MissedSome
  _                                                                  -> MissedUnknown


-- * Options

-- | How a client reaches its hub: an action over a new connection, or why
--   there is none.
type Dialer = forall a. (Conn -> IO a) -> IO (Either Text a)

-- | A client's options: its dialer and its name (the URL, for notices),
--   then its waits in microseconds: the connection's, hello's, a request's,
--   the heartbeat's grace past the interval, a request's for a reconnect
--   while disconnected; and its backoff.
data ClientOpts = ClientOpts
  { coDial                :: Dialer
  , coName                :: Text
  , coConnectMicros       :: !Int
  , coHelloMicros         :: !Int
  , coRequestMicros       :: !Int
  , coGraceMicros         :: !Int
  , coReconnectWaitMicros :: !Int
  , coBackoff             :: !Backoff
  }

-- | The client of @ws:\/\/127.0.0.1:PORT\/PATH@: dialled there alone
--   ("Hsed.Transport.Mhtx.Conn".'dialLoopback'); 5 s to connect and 3 s
--   for hello (mh_transport's client.dart:114-115); 15 s for a request,
--   past the host's own 10 s ("Hsed.Wire".@defaultExchangeOpts@), so that a
--   slow hub is cut by the host as the in-process simulator would be; the
--   heartbeat's grace 10 s (SPEC.md L61-62; client.dart:117); 3 s for a
--   reconnect while disconnected (the scaffold's, reversible); and
--   'defaultBackoff'. The connection's wait is the dialer's own as well:
--   options that change 'coConnectMicros' change 'coDial' with it.
clientOpts :: Word16 -> Text -> ClientOpts
clientOpts port path = ClientOpts
  { coDial                = dialLoopback port path connect
  , coName                = "ws://127.0.0.1:" <> tshow port <> path
  , coConnectMicros       = connect
  , coHelloMicros         = 3000000
  , coRequestMicros       = 15000000
  , coGraceMicros         = 10000000
  , coReconnectWaitMicros = 3000000
  , coBackoff             = defaultBackoff
  }
  where connect = 5000000


-- * The client

-- | An open client: what its hello said, its transport, view and control,
--   its connection manager and the state they share.
data MhtxClient = MhtxClient
  { mcApp       :: !SedApp
  , mcTransport :: !(Transport 'ReadWrite)
  , mcView      :: !SimView
  , mcControl   :: !Control
  , mcState     :: !ClientState
  , mcManager   :: !(Async ())
  }

-- | What a client's connection manager and its requests share.
data ClientState = ClientState
  { stName          :: !Text
  , stRequestMicros :: !Int
  , stWaitMicros    :: !Int                         -- ^ a request's wait for a reconnect
  , stLink          :: !(TVar Link)
  , stGen           :: !(TVar Int)                  -- ^ the connections taken so far
  , stNextId        :: !(TVar Integer)
  , stPending       :: !(TVar (Map Integer Pending))
  , stLastSeen      :: !(TVar (Maybe (Text, Integer)))  -- ^ the server id and the last @seq@ seen
  , stBreak         :: !(TVar (Int, Text))          -- ^ the last connection judged other than none, and its notice
  , stNotices       :: !(TVar [Text])               -- ^ newest first
  , stVerdicts      :: !(TVar [Missed])             -- ^ newest first
  , stResets        :: !(TVar (Set Integer))        -- ^ this client's reset requests whose @sed.evt@ has not come
  , stWaiting       :: !(TVar Int)                  -- ^ requests waiting for a reconnect now
  , stSending       :: !(MVar ())                   -- ^ held from a request's id to its frame written, so that ids go
                                                    --   out in order (a hub refuses one not above the last, design 6.3)
  }

-- | Where a client stands with its hub.
data Link
  = Linked !Live          -- ^ connected, its hello taken
  | Unlinked !Int !Text   -- ^ not connected: the last connection taken (0, none yet), why there is none now
  | Latched !Text         -- ^ refused for good (a reconnect's @app.sed@ not the first's): every request fails
  | Shut                  -- ^ closed

-- | A connection taken: its socket, its place among the client's
--   connections (from 1), its hub's server id.
data Live = Live
  { lvConn     :: !Conn
  , lvGen      :: !Int
  , lvServerId :: !Text
  }

-- | A request registered: the connection it is sent on, where its answer
--   goes (or why there is none).
data Pending = Pending
  { pdGen   :: !Int
  , pdReply :: !(TMVar (Either Text Reply))
  }

-- | How one connection ended, as its manager sees it.
data Ending
  = NoHello !Text  -- ^ connected, its hello not taken (none in time, unreadable, or not a TPer's)
  | Lost !Text     -- ^ its hello taken, then the connection lost
  | Stopped        -- ^ the client closed or latched: no attempt more

-- | A client after one attempt: connected within the wait, a hello within
--   its wait whose @v@ is 1 and whose @app.sed@ reads, says role @tper@,
--   units known; or why not. Once open it reconnects by itself.
openMhtx :: ClientOpts -> IO (Either Text MhtxClient)
openMhtx opts = do
  st <- newState opts
  opened <- newEmptyMVar
  manager <- asyncWithUnmask (\unmask -> unmask (manage opts st opened))
  -- the manager stopped if the opener is interrupted, so that no client the caller never had dials on
  outcome <- timeout (coConnectMicros opts + 2 * coHelloMicros opts) (readMVar opened) `onException` cancel manager
  case fromMaybe (Left ("no connection and hello within " <> duration (coConnectMicros opts + coHelloMicros opts))) outcome of
    Left why  -> Left why <$ cancel manager
    Right app -> pure (Right MhtxClient
      { mcApp       = app
      , mcTransport = transportOf st manager app
      , mcView      = viewOf st
      , mcControl   = controlOf st
      , mcState     = st
      , mcManager   = manager
      })

-- | A client's state before its first connection.
newState :: ClientOpts -> IO ClientState
newState opts = ClientState (coName opts) (coRequestMicros opts) (coReconnectWaitMicros opts)
  <$> newTVarIO (Unlinked 0 "not connected yet")
  <*> newTVarIO 0
  <*> newTVarIO 1
  <*> newTVarIO mempty
  <*> newTVarIO Nothing
  <*> newTVarIO (0, "")
  <*> newTVarIO []
  <*> newTVarIO []
  <*> newTVarIO mempty
  <*> newTVarIO 0
  <*> newMVar ()

-- | The @app.sed@ of the hello it opened on.
clientApp :: MhtxClient -> SedApp
clientApp = mcApp

-- | Its transport: IF-SEND, IF-RECV and the reset events over the hub.
clientTransport :: MhtxClient -> Transport 'ReadWrite
clientTransport = mcTransport

-- | Its view: queries as the hub's peek op.
clientView :: MhtxClient -> SimView
clientView = mcView

-- | Its control: a release as the hub's release op.
clientControl :: MhtxClient -> Control
clientControl = mcControl

-- | What it has noticed since last asked (a restart, broadcasts missed,
--   another client's reset, a latch), each once, in order.
clientNotices :: MhtxClient -> IO [Text]
clientNotices = atomically . drain . stNotices . mcState

-- | The verdict on each connection it has taken since last asked, in order:
--   the first connection's unknown among them, and a reconnect's none,
--   which notices nothing (what the conformance suite's check 8 judges,
--   SPEC.md L128-129).
clientVerdicts :: MhtxClient -> IO [Missed]
clientVerdicts = atomically . drain . stVerdicts . mcState

-- | The ids of its requests registered and not yet ended, in order.
clientInFlight :: MhtxClient -> IO [Integer]
clientInFlight = fmap Map.keys . readTVarIO . stPending . mcState

-- | How many of its requests are waiting for a reconnect now (issued while
--   it was not connected): what a test waits for before it lets the
--   reconnect happen, rather than a delay.
clientWaiting :: MhtxClient -> IO Int
clientWaiting = readTVarIO . stWaiting . mcState

-- | Once it is connected again, or will never be (closed or latched), or
--   its wait for a reconnect has passed (a request's own: 3 s by
--   'clientOpts'): what the run does before it hears the client's notices
--   ("Hsed.Jig.Script".@heedNotices@), so that a reconnect under way when a
--   case ends is heard before the next case, not refused in its
--   prerequisites (the M5c review). Connected, it returns at once.
clientSettled :: MhtxClient -> IO ()
clientSettled c = void . timeout (stWaitMicros st) . atomically $ readTVar (stLink st) >>= \case
  Unlinked _ _ -> retrySTM
  _            -> pure ()
  where st = mcState c

-- | Close it: requests in flight failed, the socket closed with 1000.
closeMhtx :: MhtxClient -> IO ()
closeMhtx c = shut (mcState c) (mcManager c)

-- | A list kept newest first, taken in order and emptied.
drain :: TVar [a] -> STM [a]
drain v = reverse <$> readTVar v <* writeTVar v []


-- * The connection manager

-- | The connection manager: the first attempt, its outcome put where
--   'openMhtx' waits for it; then, once a connection has been taken, a
--   reconnect after each drop, the backoff's count reset by each connection
--   taken, until the client is closed or latched. A manager that fails
--   latches the client, naming why.
manage :: ClientOpts -> ClientState -> MVar (Either Text SedApp) -> IO ()
manage opts st opened = firstAttempt `catchAny` failed
  where
    firstAttempt = coDial opts (session opts st Nothing (void . tryPutMVar opened . Right)) >>= \case
      Left why            -> void (tryPutMVar opened (Left why))
      Right (NoHello why) -> void (tryPutMVar opened (Left why))
      Right (Lost why)    -> readMVar opened >>= traverse_ (\app -> again app 0 why)
      Right Stopped       -> pure ()
    again app attempt why = do
      going <- atomically (unlinked st why)
      when going $ do
        draw <- randomRIO (0, 1)
        threadDelay (backoffDelay (coBackoff opts) attempt draw)
        coDial opts (session opts st (Just app) (const (pure ()))) >>= \case
          Left why'            -> again app (attempt + 1) why'
          Right (NoHello why') -> again app (attempt + 1) why'
          Right (Lost why')    -> again app 0 why'
          Right Stopped        -> pure ()
    failed e = do
      let why = "the client of " <> coName opts <> " stopped: its connection manager failed: " <> tshow e
      atomically (latch st why)
      void (tryPutMVar opened (Left why))

-- | Why the client is not connected now, if it is not closed or latched;
--   whether it goes on.
unlinked :: ClientState -> Text -> STM Bool
unlinked st why = readTVar (stLink st) >>= \case
  Unlinked gen _ -> True <$ writeTVar (stLink st) (Unlinked gen why)
  Linked _       -> pure True
  Latched _      -> pure False
  Shut           -> pure False

-- | One connection, given the first's @app.sed@ if this is a reconnect
--   (none, the first) and what to do once the first is taken: its hello
--   awaited and judged, then kept alive until it is lost.
session :: ClientOpts -> ClientState -> Maybe SedApp -> (SedApp -> IO ()) -> Conn -> IO Ending
session opts st firstApp taken conn = awaitHello >>= \case
  Left why          -> NoHello why <$ refuse why
  Right (hello, app) -> case firstApp of
    Just app0 | app /= app0 -> do
      let why = "the hub's app.sed changed on a reconnect (" <> T.intercalate ", " (appChanges app0 app)
                <> "): the client is latched, every request failing"
      atomically (latch st why)
      Stopped <$ refuse why
    _ -> connected hello app
  where
    refuse why = connClose conn codePolicy (T.take 100 why) `catchAny` const (pure ())
    -- the first message that is not ignored must be the hello (SPEC.md L37), within the wait
    awaitHello = fromMaybe (Left ("no hello within " <> duration (coHelloMicros opts) <> ": hello is required (SPEC.md L99-104)"))
      <$> timeout (coHelloMicros opts) firstMessage
    firstMessage = tryAny (connReceive conn) >>= \case
      Left e             -> pure (Left ("the connection failed before hello: " <> tshow e))
      Right (InClosed c) -> pure (Left ("closed before hello" <> closeText c))
      Right incoming     -> case fromServer incoming of
        ServerIgnored -> firstMessage
        ServerHello h -> pure (helloTaken h)
        other         -> pure (Left ("the hub sent " <> frameWord other <> " before hello (SPEC.md L37)"))
    connected hello app = do
      now <- Clock.getMonotonicTime
      heard <- newTVarIO now
      atomically (takeConnection st hello conn) >>= \case
        Nothing   -> pure Stopped
        Just live -> do
          when (isNothing firstApp) (taken app)
          why <- keepAlive opts st live heard (helloIntervalMs hello)
          going <- atomically (lost st live why)
          pure (if going then Lost why else Stopped)

-- | A hello taken, its @app.sed@ read, or why it is refused: @v@ 1 (SPEC.md
--   L45), a heartbeat interval to keep, an @app.sed@ that reads (its units
--   known among it), role @tper@.
helloTaken :: Hello -> Either Text (Hello, SedApp)
helloTaken h
  | helloV h /= 1           = Left ("hello's v is " <> tshow (helloV h) <> ", not 1 (SPEC.md L45)")
  | helloIntervalMs h <= 0  = Left ("hello's heartbeat.interval_ms is " <> tshow (helloIntervalMs h) <> ": no heartbeat to keep (SPEC.md L49)")
  | otherwise               = case sedAppOf h of
      Left why -> Left ("hello's app.sed does not read: " <> why)
      Right app
        | saRole app /= "tper" -> Left ("hello's app.sed.role is " <> saRole app <> ", not tper: a hub serves the simulated TPer alone")
        | otherwise            -> Right (h, app)

-- | The fields of @app.sed@ that differ between two hellos'.
appChanges :: SedApp -> SedApp -> [Text]
appChanges a b = [ name | (name, same) <- fields, not same ]
  where
    fields =
      [ ("role", saRole a == saRole b), ("impl", saImpl a == saImpl b), ("units", saUnits a == saUnits b)
      , ("ssc", saSsc a == saSsc b), ("errata", saErrata a == saErrata b), ("base_comid", saBaseComId a == saBaseComId b)
      , ("n_comids", saNComIds a == saNComIds b), ("allows", saAllows a == saAllows b), ("setup", saSetup a == saSetup b) ]

-- | A connection's hello taken: the connection linked as the client's next,
--   its verdict kept and, a reconnect's other than none, noticed as what
--   requests waiting for it fail with; none if the client was closed or
--   latched meanwhile.
takeConnection :: ClientState -> Hello -> Conn -> STM (Maybe Live)
takeConnection st hello conn = readTVar (stLink st) >>= \case
  Latched _          -> pure Nothing
  Shut               -> pure Nothing
  Linked _           -> Just <$> taken True
  Unlinked lastGen _ -> Just <$> taken (lastGen > 0)
  where
    taken reconnect = do
      seen <- readTVar (stLastSeen st)
      gen <- (+ 1) <$> readTVar (stGen st)
      let verdict = judgeMissed reconnect seen (Just hello)
          live    = Live conn gen (helloServerId hello)
      writeTVar (stGen st) gen
      writeTVar (stLink st) (Linked live)
      writeTVar (stLastSeen st) (Just (helloServerId hello, helloSeq hello))
      modifyTVar' (stVerdicts st) (verdict :)
      for_ (if reconnect then noticeOf seen hello verdict else Nothing) $ \notice -> do
        modifyTVar' (stNotices st) (notice :)
        writeTVar (stBreak st) (gen, notice)
      pure live

-- | What a reconnect's verdict tells the run, if anything: a new server id
--   is a restarted hub, a new TPer at its Original Factory State; a greater
--   @seq@, broadcasts missed, a reset perhaps; a smaller one, taken as a
--   restart.
noticeOf :: Maybe (Text, Integer) -> Hello -> Missed -> Maybe Text
noticeOf seen h = \case
  MissedNone    -> Nothing
  MissedSome    -> Just ("broadcasts missed, seq " <> maybe "?" (tshow . snd) seen <> " -> " <> tshow (helloSeq h)
                         <> ": a reset may have happened")
  MissedUnknown -> Just $ case seen of
    Just (serverId, lastSeq) | serverId == helloServerId h ->
      "broadcasts unknown, seq " <> tshow lastSeq <> " -> " <> tshow (helloSeq h) <> " on server_id " <> serverId <> ": taken as a restart"
    _ -> "the hub restarted, server_id " <> maybe "unknown" fst seen <> " -> " <> helloServerId h <> ": a new TPer at its OFS"

-- | A connection lost: the client unlinked from it, unless closed or
--   latched meanwhile, and every request sent on it failed, none ever sent
--   again (an IF-RECV in S3 delivers its packets once); whether the client
--   goes on.
lost :: ClientState -> Live -> Text -> STM Bool
lost st live why = do
  going <- readTVar (stLink st) >>= \case
    Linked l | lvGen l == lvGen live -> True <$ writeTVar (stLink st) (Unlinked (lvGen live) why)
    Linked _                         -> pure True
    Unlinked _ _                     -> pure True
    Latched _                        -> pure False
    Shut                             -> pure False
  failPending st ((== lvGen live) . pdGen)
    ("the connection to " <> stName st <> " was lost (" <> why <> "): the request was sent, and is never sent again")
  pure going

-- | A connection kept alive until it is lost, and why it was: its messages
--   read (each proof of life; a ping answered, an answer given to its
--   request, a broadcast's @seq@ kept), a ping every interval, and the
--   connection dropped after the interval and the grace with nothing heard.
keepAlive :: ClientOpts -> ClientState -> Live -> TVar Double -> Int -> IO Text
keepAlive opts st live heard intervalMs = either id (either id id) <$> race reader (race pinger watchdog)
  where
    conn = lvConn live
    reader = tryAny (connReceive conn) >>= \case
      Left e             -> pure ("receiving failed: " <> tshow e)
      Right incoming     -> do
        Clock.getMonotonicTime >>= atomically . writeTVar heard
        case incoming of
          InClosed c -> pure ("closed" <> closeText c)
          _          -> received (fromServer incoming) >> reader
    received = \case
      ServerPing        -> void (tryAny (connSendText conn pongText))
      ServerRsp rsp     -> atomically (answered st live rsp)
      ServerEvt evt     -> atomically (evented st live evt)
      ServerBroadcast s -> atomically (writeTVar (stLastSeen st) (Just (lvServerId live, s)))
      _                 -> pure ()
    pinger = do
      threadDelay (intervalMs * 1000)
      tryAny (connSendText conn pingText) >>= either (pure . ("sending a ping failed: " <>) . tshow) (const pinger)
    deadMicros = intervalMs * 1000 + coGraceMicros opts
    watchdog = do
      remaining <- (\h now -> h + fromIntegral deadMicros / 1e6 - now) <$> readTVarIO heard <*> Clock.getMonotonicTime
      if remaining > 0
        then threadDelay (max 1 (ceiling (remaining * 1e6))) >> watchdog
        else ("nothing from the hub in " <> duration deadMicros <> ", its interval and the grace (SPEC.md L60-62)")
               <$ (connClose conn codeHeartbeat "heartbeat timeout" `catchAny` const (pure ()))

-- | An answer given to the request of its id, if one on this connection
--   waits for it; ignored otherwise.
answered :: ClientState -> Live -> SedRsp -> STM ()
answered st live rsp = for_ (rspId rsp) $ \rid ->
  Map.lookup rid <$> readTVar (stPending st) >>= traverse_ (\p ->
    when (pdGen p == lvGen live) (void (tryPutTMVar (pdReply p) (Right (rspReply rsp)))))

-- | A reset's broadcast: its @seq@ kept; this client's own if one of its
--   resets waits for its answer (the hub broadcasts before it answers),
--   another client's noticed otherwise. A @sed.evt@ names no requester
--   (docs/wire.md), so that while this client's reset waits, another
--   client's reset that the hub runs first is taken for this client's, and
--   this client's own then noticed as another's: the count of resets noticed
--   is right, their attribution not (the M5c review; M5c runs one client per
--   hub).
evented :: ClientState -> Live -> SedEvt -> STM ()
evented st live evt = do
  writeTVar (stLastSeen st) (Just (lvServerId live, evtSeq evt))
  Set.minView <$> readTVar (stResets st) >>= \case
    Just (_, others) -> writeTVar (stResets st) others
    Nothing          -> modifyTVar' (stNotices st)
      (("a reset by another client: " <> eventWord (evtEvent evt) <> ", seq " <> tshow (evtSeq evt)) :)

-- | The client latched: every request failing for the reason, from now
--   on, noticed.
latch :: ClientState -> Text -> STM ()
latch st why = do
  writeTVar (stLink st) (Latched why)
  modifyTVar' (stNotices st) (why :)
  failPending st (const True) why

-- | The requests registered that a predicate picks failed for a reason
--   (those answered already keep their answer).
failPending :: ClientState -> (Pending -> Bool) -> Text -> STM ()
failPending st picked why =
  readTVar (stPending st) >>= traverse_ (\p -> tryPutTMVar (pdReply p) (Left why)) . filter picked . Map.elems

-- | Close the client: every request failed, no reconnect, the connection
--   closed with 1000, the manager stopped.
shut :: ClientState -> Async () -> IO ()
shut st manager = do
  live <- atomically $ do
    was <- readTVar (stLink st)
    writeTVar (stLink st) Shut
    failPending st (const True) (closedText st)
    pure (case was of
      Linked l -> Just l
      _        -> Nothing)
  for_ live (\l -> connClose (lvConn l) codeNormal "the client closed" `catchAny` const (pure ()))
  cancel manager

closedText :: ClientState -> Text
closedText st = "the client of " <> stName st <> " is closed"


-- * Requests

-- | Where a request stands once issued: registered on a connection with its
--   id and where its answer goes, its frame written; refused; or waiting for
--   a connection after the one of a generation was lost.
data Issue
  = Registered !Live !Integer !(TMVar (Either Text Reply))
  | Refused !Text
  | Waiting !Int

-- | A request and its answer, or why there is none. Its id taken and its
--   entry registered before its frame is written, the two under the client's
--   send lock, so that its ids go out in order; on the connection now or,
--   while there is none, on the next one within the wait if its hello says
--   nothing was missed since the request was issued; then its answer
--   awaited, interruptibly, within the request's wait; its entry removed
--   however it ends (masked between, so that no exception leaves one behind).
request :: ClientState -> Op -> IO (Either Text Reply)
request st op = mask_ $ issue Nothing >>= \case
  Registered _ rid slot -> answer slot `finally` atomically (forget st rid)
  Refused why           -> pure (Left why)
  Waiting _             -> pure (Left ("not connected to " <> stName st))
  where
    issue since = withMVar (stSending st) (const (atomically (tryRegister st op since) >>= written)) >>= \case
      Waiting gen -> bracket_ (atomically (modifyTVar' (stWaiting st) (+ 1))) (atomically (modifyTVar' (stWaiting st) (subtract 1)))
                       (timeout (stWaitMicros st) (atomically (relinked st gen)))
                       >>= maybe (Refused <$> notConnected) (const (issue (Just gen)))
      other       -> pure other
    -- the frame written on the connection the request was registered on, never on a later one
    written = \case
      Registered live rid slot -> tryAny (connSendText (lvConn live) (reqText (SedReq rid op))) `onException` atomically (forget st rid)
        >>= \case
          Left e   -> Refused ("not sent to " <> stName st <> ": " <> tshow e) <$ atomically (forget st rid)
          Right () -> pure (Registered live rid slot)
      other -> pure other
    answer slot = fromMaybe (Left ("no answer from " <> stName st <> " within " <> duration (stRequestMicros st)
                                   <> ": the request was sent, and is never sent again"))
                    <$> timeout (stRequestMicros st) (atomically (takeTMVar slot))
    notConnected = readTVarIO (stLink st) <&> \case
      Unlinked _ why -> "not connected to " <> stName st <> " within " <> duration (stWaitMicros st) <> ": " <> why
      Latched why    -> why
      Shut           -> closedText st
      Linked _       -> "not connected to " <> stName st <> " within " <> duration (stWaitMicros st)

-- | A request registered on the connection now, if there is one and, for a
--   request issued while the connection of a generation was lost, unless a
--   connection taken since was judged other than none (then refused with
--   what it noticed); waiting if there is none.
tryRegister :: ClientState -> Op -> Maybe Int -> STM Issue
tryRegister st op since = readTVar (stLink st) >>= \case
  Linked live -> case since of
    Nothing  -> register st op live
    Just gen -> readTVar (stBreak st) >>= \(broken, notice) ->
      if broken > gen then pure (Refused ("not sent: " <> notice)) else register st op live
  Unlinked gen _ -> pure (Waiting (fromMaybe gen since))
  Latched why    -> pure (Refused why)
  Shut           -> pure (Refused (closedText st))

-- | Waits until there is a connection taken after the one of a generation,
--   or none ever will be (the client latched or closed).
relinked :: ClientState -> Int -> STM ()
relinked st gen = readTVar (stLink st) >>= \case
  Linked live | lvGen live > gen -> pure ()
  Latched _                      -> pure ()
  Shut                           -> pure ()
  _                              -> retrySTM

-- | A request registered on a connection: the client's next id, where its
--   answer goes, a reset among the resets whose broadcast is to come.
register :: ClientState -> Op -> Live -> STM Issue
register st op live = do
  rid <- readTVar (stNextId st)
  if rid > mostId
    then pure (Refused ("no id left: " <> tshow mostId <> " requests made (docs/wire.md)"))
    else do
      writeTVar (stNextId st) (rid + 1)
      slot <- newEmptyTMVar
      modifyTVar' (stPending st) (Map.insert rid (Pending (lvGen live) slot))
      case op of
        OpReset _ -> modifyTVar' (stResets st) (Set.insert rid)
        _         -> pure ()
      pure (Registered live rid slot)

-- | A request's entry removed, its reset's too.
forget :: ClientState -> Integer -> STM ()
forget st rid = do
  modifyTVar' (stPending st) (Map.delete rid)
  modifyTVar' (stResets st) (Set.delete rid)


-- * The transport, the view, the control

-- | The client as a @'ReadWrite@ transport: as the in-process one answers
--   ("Hsed.Transport.InProc"), a transfer past what its unit carries
--   refused locally; commands and events as the hub's ops; its unit and
--   description from @app.sed@; closing it closes the client.
transportOf :: ClientState -> Async () -> SedApp -> Transport 'ReadWrite
transportOf st manager app = MkTransport
  { tExchange = \r -> if lenBytes r > mostCarried (saUnits app)
                        then pure (Response OtherInvalidCommandParameter mempty)
                        else responseTo r <$> request st (commandOp r)
  , tInject   = \e -> injected <$> request st (OpReset e)
  , tUnit     = saUnits app
  , tDescribe = Describe
      { dRole      = saRole app
      , dImpl      = saImpl app
      , dUnits     = saUnits app
      , dSsc       = saSsc app
      , dBaseComId = saBaseComId app
      , dNComIds   = saNComIds app
      , dDevice    = Nothing
      }
  , tClose    = shut st manager
  }

-- | A command as the hub's op: IF-SEND's payload exactly its transfer
--   length's bytes (as "Hsed.Wire".'Hsed.Wire.ifSend' gives it).
commandOp :: Request -> Op
commandOp r = case dir r of
  IfSend -> OpIfSend (proto r) (comId r) (exactly (lenBytes r) (payload r))
  IfRecv -> OpIfRecv (proto r) (comId r) (lenBytes r)

-- | A command's answer: the status and bytes the hub's simulator gave (a
--   good IF-RECV's exactly the length asked); a protocol error, another
--   reply or no answer a transport failure naming it.
responseTo :: Request -> Either Text Reply -> Response
responseTo r = \case
  Left why -> failure why
  Right (Answered s bytes)
    | s == Good, dir r == IfRecv, B.length bytes /= fromIntegral (lenBytes r) ->
        failure ("the hub answered " <> tshow (B.length bytes) <> " bytes to an IF-RECV of " <> tshow (lenBytes r))
    | otherwise -> Response s bytes
  Right (ProtocolError why) -> failure ("the hub's protocol_error: " <> why)
  Right other -> failure ("the hub answered " <> replyWord other <> " to a command")
  where failure why = Response (TransportFailure why) mempty

-- | An event's status: the hub's answer to the reset op (unsupported, its
--   detail kept, where the hub does not allow it); a protocol error,
--   another reply or no answer a transport failure naming it.
injected :: Either Text Reply -> IfStatus
injected = \case
  Left why                  -> TransportFailure why
  Right (Answered s _)      -> s
  Right (ProtocolError why) -> TransportFailure ("the hub's protocol_error: " <> why)
  Right other               -> TransportFailure ("the hub answered " <> replyWord other <> " to a reset")

-- | The client's view: each query the hub's peek op, its answer read for
--   the query's type; a hub's refusal (without @--allow-remote-control@) or
--   no answer, why.
viewOf :: ClientState -> SimView
viewOf st = SimView (\q -> request st (OpPeek (SomeQuery q)) <&> \case
  Left why                  -> Left why
  Right (Viewed v)          -> first ("the hub's answer to peek does not read: " <>) (parseAnswer q v)
  Right (Answered s _)      -> Left ("peek: " <> statusText s)
  Right (ProtocolError why) -> Left ("peek: the hub's protocol_error: " <> why)
  Right other               -> Left ("peek: the hub answered " <> replyWord other))

-- | The client's control: a release the hub's release op, answered good; a
--   hub's refusal or no answer, why.
controlOf :: ClientState -> Control
controlOf st = Control (\comid -> request st (OpRelease comid) <&> \case
  Left why                  -> Left why
  Right (Answered Good _)   -> Right ()
  Right (Answered s _)      -> Left ("release: " <> statusText s)
  Right (ProtocolError why) -> Left ("release: the hub's protocol_error: " <> why)
  Right other               -> Left ("release: the hub answered " <> replyWord other))


-- * Words

-- | A status as the hub sent it: its name, and its detail if it has one.
statusText :: IfStatus -> Text
statusText s = ifStatusName s <> maybe "" (": " <>) (ifStatusDetail s)

-- | What a reply is, in a word or two.
replyWord :: Reply -> Text
replyWord = \case
  Answered s _    -> "status " <> ifStatusName s
  Described _     -> "a description"
  Viewed _        -> "a view"
  ProtocolError _ -> "protocol_error"

-- | What a server's message is, in a word or two.
frameWord :: FromServer -> Text
frameWord = \case
  ServerHello _     -> "hello"
  ServerPing        -> "ping"
  ServerPong        -> "pong"
  ServerRsp _       -> "sed.rsp"
  ServerEvt _       -> "sed.evt"
  ServerBroadcast _ -> "a broadcast"
  ServerIgnored     -> "a message to ignore"

-- | An event as the frames name it (the reports' words, "Hsed.Wire.Internal").
eventWord :: Event -> Text
eventWord = \case
  PowerCycle    -> "power_cycle"
  HardwareReset -> "hardware_reset"
  HotPlug       -> "hot_plug"

-- | A close as received: its code and reason, if the peer gave them.
closeText :: Maybe (Word16, Text) -> Text
closeText = maybe " with no close frame" (\(code, reason) -> " with " <> tshow code <> (if T.null reason then "" else " (" <> reason <> ")"))

-- | Microseconds as the texts give them: whole seconds, whole
--   milliseconds, or microseconds.
--
-- >>> map duration [15000000, 20000, 1500]
-- ["15 s","20 ms","1500 us"]
duration :: Int -> Text
duration us
  | us >= 1000000, us `mod` 1000000 == 0 = tshow (us `div` 1000000) <> " s"
  | us >= 1000, us `mod` 1000 == 0       = tshow (us `div` 1000) <> " ms"
  | otherwise                            = tshow us <> " us"
