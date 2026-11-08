{-# LANGUAGE DataKinds         #-}
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
before hello (4001, SPEC.md L92-95); the hooks @POST \/emit@ and @GET
\/clients@ only with @--conformance-hooks@; served by warp on the socket
"Hsed.Transport.Mhtx.Conn".'Hsed.Transport.Mhtx.Conn.bindLoopback' bound,
127.0.0.1 alone ('parseListen'; Scott's answer Q1).

Not yet written: group G2 writes the bodies; the types, the defaults and
the signatures are the scaffold's (M5c's S1.2).
-}
module Hsed.Transport.Mhtx.Hub
  ( -- * Options
    HubOpts (..)
  , defaultHubOpts
  , parseListen
    -- * The TPer behind it
  , Backend (..)
  , backendOf
    -- * The hub
  , Hub
  , newHub
  , hubServerId
  , hubSeq
  , hubClientCount
  , hubServe
  , hubBroadcast
  , hubSweep
  , hubClose
  , runHub
  ) where

import qualified Data.Aeson                as A
import qualified GHC.Clock                 as Clock
import           Network.Socket            (Socket)
import           RIO

import           Hsed.Sim.Types            (SimParams)
import           Hsed.Transport.InProc     (InProcOpts, SimControl)
import           Hsed.Transport.Mhtx.Conn  (Conn)
import           Hsed.Transport.Mhtx.Frame (SedApp)
import           Hsed.Wire                 (Cap (..), Transport)


-- * Options

-- | How a hub runs: its heartbeat's interval and timeout (hello's
--   @heartbeat@, milliseconds), whether it allows the control ops (peek,
--   release), the reset op and the conformance hooks, the token a client
--   must give (none: none asked), its clock (seconds, monotonic), and how
--   many frames a socket's send queue holds before the socket is closed.
data HubOpts = HubOpts
  { hoIntervalMs   :: !Int
  , hoTimeoutMs    :: !Int
  , hoAllowControl :: !Bool
  , hoAllowReset   :: !Bool
  , hoHooks        :: !Bool
  , hoToken        :: !(Maybe ByteString)
  , hoNow          :: IO Double
  , hoQueue        :: !Int
  }

-- | A heartbeat of 20 s and a timeout of 60 s (design 6.3's hello,
--   docs/spec-as-source.md:502), nothing allowed beyond commands and
--   describe, no token, GHC's monotonic clock in seconds (not RIO's, which
--   counts nanoseconds on macOS on Apple silicon: "Hsed.Wire"'s exchange
--   says so), and a send queue of 1024 frames (the scaffold's bound,
--   reversible).
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
  }

-- | @--listen 127.0.0.1:PORT@: the port, or why not; any other host is
--   refused (Scott's answer Q1: 127.0.0.1 alone until M6).
parseListen :: String -> Either String Word16
parseListen _listen = Left "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.parseListen)"


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
--   its @app.sed@ saying what the options allow; or why there is none.
backendOf :: HubOpts -> SimParams -> InProcOpts -> IO (Either Text Backend)
backendOf _opts _params _inProc = pure (Left "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.backendOf)")


-- * The hub

-- | A hub: its server id, its @seq@, its peers.
data Hub

-- | A hub over a backend, no peer yet.
newHub :: HubOpts -> Backend -> IO Hub
newHub _opts _backend = throwString "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.newHub)"

-- | Its server id: 12 characters of [a-z0-9], one for the process.
hubServerId :: Hub -> Text
hubServerId _hub = error "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.hubServerId)"

-- | Its broadcasts so far.
hubSeq :: Hub -> IO Integer
hubSeq _hub = throwString "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.hubSeq)"

-- | Its peers now.
hubClientCount :: Hub -> IO Int
hubClientCount _hub = throwString "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.hubClientCount)"

-- | Serve one connection until it closes, given the token its request
--   carried (the @token@ query parameter; none, none given): where the hub
--   asks for one (@--token-file@), a token missing or not equal to it
--   (compared in constant time) has the connection closed with 4001 before
--   hello (SPEC.md L92-95), and is never logged. 'runHub' reads the token
--   from the upgrade's request; a test over memory gives it here.
hubServe :: Hub -> Maybe ByteString -> Conn -> IO ()
hubServe _hub _token _conn = throwString "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.hubServe)"

-- | Broadcast an object with the next @seq@, which it returns; or why not
--   (a reserved type, a @sed.*@ type).
hubBroadcast :: Hub -> A.Object -> IO (Either Text Integer)
hubBroadcast _hub _object = pure (Left "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.hubBroadcast)")

-- | Close every peer silent past the timeout, with 4000.
hubSweep :: Hub -> IO ()
hubSweep _hub = throwString "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.hubSweep)"

-- | Close every peer, with 1001.
hubClose :: Hub -> IO ()
hubClose _hub = throwString "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.hubClose)"

-- | Serve the hub on a listening socket, the one
--   "Hsed.Transport.Mhtx.Conn".'Hsed.Transport.Mhtx.Conn.bindLoopback'
--   bound (warp's runSettingsSocket, which binds nothing), and sweep its
--   peers every heartbeat interval ('hubSweep'), until killed: what
--   hsed-sim runs and the LAN suite's hubs run, nothing else beside it.
runHub :: Hub -> Socket -> IO ()
runHub _hub _socket = throwString "not yet written: group G2 (Hsed.Transport.Mhtx.Hub.runHub)"
