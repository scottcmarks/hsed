{-# LANGUAGE DataKinds         #-}
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

The jig's side of the LAN leg (design 7 item 4, 11 M5c): one connection
manager, opened by one attempt that requires a hello whose @app.sed@ says a
TPer answers (mh_transport SPEC.md L99-104, @requireHello@), then a ping
every @interval_ms@ (L57), any frame proof of life, silence past the
interval and a grace dropping the connection (L60-62), and reconnects with
equal-jitter backoff (L108-111, 'backoffDelay'), each hello judged for
missed broadcasts (L78-85, 'judgeMissed'). Requests carry increasing ids,
each registered before it is written and answered by its id; a dropped
socket fails every request sent, none ever sent again (an IF-RECV in S3
delivers its packets once, Hsed.Sim.Sync), and a new server_id or missed
broadcasts are noticed ('clientNotices'). It gives the run a @'ReadWrite@
transport, a 'SimView' and a 'Control'.

Not yet written: group G3 writes the bodies; the types, the defaults and
the signatures are the scaffold's (M5c's S1.2).
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
  , closeMhtx
  ) where

import           RIO

import           Hsed.Sim.View             (Control, SimView)
import           Hsed.Transport.Mhtx.Conn  (Conn, dialLoopback)
import           Hsed.Transport.Mhtx.Frame (Hello, SedApp)
import           Hsed.Wire                 (Cap (..), Transport)


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
--   uniform draw in [0, 1): backoff.dart:27-35's.
backoffDelay :: Backoff -> Int -> Double -> Int
backoffDelay _backoff _attempt _draw = error "not yet written: group G3 (Hsed.Transport.Mhtx.Client.backoffDelay)"

-- | What a reconnect's hello says of the broadcasts missed while away
--   (SPEC.md L78-85).
data Missed
  = MissedNone
  | MissedSome
  | MissedUnknown
  deriving (Eq, Show, Enum, Bounded)

-- | The verdict on a hello: whether it is a reconnect, the server id and
--   @seq@ last seen, the hello (none, the server sent none): client.dart:397-406's.
judgeMissed :: Bool -> Maybe (Text, Integer) -> Maybe Hello -> Missed
judgeMissed _isReconnect _lastSeen _hello = error "not yet written: group G3 (Hsed.Transport.Mhtx.Client.judgeMissed)"


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
--   'defaultBackoff'.
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

-- | An open client: its connection manager, its requests in flight, what
--   it has noticed.
data MhtxClient

-- | A client after one attempt: connected within the wait, a hello within
--   its wait whose @app.sed@ reads, says role @tper@, units known; or why
--   not. Once open it reconnects by itself.
openMhtx :: ClientOpts -> IO (Either Text MhtxClient)
openMhtx _opts = pure (Left "not yet written: group G3 (Hsed.Transport.Mhtx.Client.openMhtx)")

-- | The @app.sed@ of the hello it opened on.
clientApp :: MhtxClient -> SedApp
clientApp _client = error "not yet written: group G3 (Hsed.Transport.Mhtx.Client.clientApp)"

-- | Its transport: IF-SEND, IF-RECV and the reset events over the hub.
clientTransport :: MhtxClient -> Transport 'ReadWrite
clientTransport _client = error "not yet written: group G3 (Hsed.Transport.Mhtx.Client.clientTransport)"

-- | Its view: queries as the hub's peek op.
clientView :: MhtxClient -> SimView
clientView _client = error "not yet written: group G3 (Hsed.Transport.Mhtx.Client.clientView)"

-- | Its control: a release as the hub's release op.
clientControl :: MhtxClient -> Control
clientControl _client = error "not yet written: group G3 (Hsed.Transport.Mhtx.Client.clientControl)"

-- | What it has noticed since last asked (a restart, broadcasts missed,
--   another client's reset), each once.
clientNotices :: MhtxClient -> IO [Text]
clientNotices _client = throwString "not yet written: group G3 (Hsed.Transport.Mhtx.Client.clientNotices)"

-- | Close it: requests in flight failed, the socket closed with 1000.
closeMhtx :: MhtxClient -> IO ()
closeMhtx _client = throwString "not yet written: group G3 (Hsed.Transport.Mhtx.Client.closeMhtx)"
