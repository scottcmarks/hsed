{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Lan.Support
Description : What the LAN suite's specs share: a hub in the suite's own threads on 127.0.0.1, a probe, one raw HTTP exchange
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The LAN suite (hsed-lan-test, design 11 M5c) runs its hubs in its own
threads, each on a socket "Hsed.Transport.Mhtx.Conn".'bindLoopback' binds on
127.0.0.1 and an ephemeral port (D6), kept from the bind to the close so that
no other process can take the port between them, and closed whatever the
spec comes to ('withHub'), or on a port given, a hub started again where one
stopped ('withHubOn'); over the default simulator's backend ('lanBackend');
dials them as a client would ('probe'); speaks HTTP to their hooks with
one raw request, no HTTP client ('rawHttp'); and holds what the specs
share besides: a bound on an example's wait ('withinMicros') and a scripted
hub's @app.sed@ ('tperApp').
-}
module Hsed.Lan.Support
  ( withHub
  , withHubOn
  , lanBackend
  , probe
  , rawHttp
  , withinMicros
  , tperApp
  ) where

import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as NB
import qualified RIO.ByteString            as B
import           RIO
import qualified RIO.Text                  as T

import           Hsed.Sim.Types            (defaultSimParams)
import           Hsed.Sim.View             (setupOf)
import           Hsed.Transport.InProc     (InProcOpts (..))
import           Hsed.Transport.Mhtx.Conn  (Conn, bindLoopback, dialLoopback)
import           Hsed.Transport.Mhtx.Frame (SedApp (..))
import           Hsed.Transport.Mhtx.Hub   (Backend, Hub, HubOpts, backendOf, hubClose, newHub, runHub)
import           Hsed.Wire                 (Unit (..))


-- | A hub over a backend, served in the suite's own threads on 127.0.0.1
--   and an ephemeral port for the action's length, then closed, its socket
--   with it: the action given the port and the hub.
withHub :: HubOpts -> Backend -> (Word16 -> Hub -> IO a) -> IO a
withHub = withHubOn 0

-- | 'withHub' on 127.0.0.1's port given (0, an ephemeral one): a hub
--   started where another was stopped, on its port again (ReuseAddr), as
--   RestartSpec's hub B takes hub A's. The hub's serving is linked to the
--   action, so that a hub that fails fails the spec at once, not as a probe's
--   timeout.
withHubOn :: Word16 -> HubOpts -> Backend -> (Word16 -> Hub -> IO a) -> IO a
withHubOn port opts backend act = bracket (bindLoopback port) (N.close . fst) $ \(sock, bound) ->
  bracket (newHub opts backend) hubClose $ \hub ->
    withAsync (runHub hub sock) $ \serving -> link serving >> act bound hub

-- | The backend of the default simulator (bytes, no hold) for these
--   options, as hsed-sim serves it with no simulator flag; a failure where
--   there is none.
lanBackend :: HubOpts -> IO Backend
lanBackend opts = backendOf opts defaultSimParams (InProcOpts UnitBytes 0) >>= either (throwString . T.unpack) pure

-- | An action over a websocket to the hub's @\/ws@ on 127.0.0.1's port,
--   connected within 5 s (conformance.dart:88's wait for a frame), or why
--   there is none.
probe :: Word16 -> (Conn -> IO a) -> IO (Either Text a)
probe port = dialLoopback port "/ws" 5000000

-- | One HTTP exchange with the hub on 127.0.0.1's port: the request's bytes
--   sent whole, the response read until the hub closes the connection (the
--   request says @Connection: close@).
rawHttp :: Word16 -> ByteString -> IO ByteString
rawHttp port request = bracket open N.close $ \sock -> do
  NB.sendAll sock request
  mconcat <$> received sock
  where
    open = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.connect sock (N.SockAddrInet (fromIntegral port) (N.tupleToHostAddress (127, 0, 0, 1))) `onException` N.close sock
      pure sock
    received sock = NB.recv sock 4096 >>= \chunk -> if B.null chunk then pure [] else (chunk :) <$> received sock

-- | An action within the microseconds given, named: one that hangs fails its example, saying what was not done in
--   time, not the suite.
withinMicros :: Int -> String -> IO a -> IO a
withinMicros micros what act = timeout micros act >>= maybe (throwString (what <> ": not in time")) pure

-- | A scripted hub's app.sed: a TPer in bytes, everything allowed, the default setup.
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
