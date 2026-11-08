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
dials them as a client would ('probe'); and speaks HTTP to their hooks with
one raw request, no HTTP client ('rawHttp').
-}
module Hsed.Lan.Support
  ( withHub
  , withHubOn
  , lanBackend
  , probe
  , rawHttp
  , needsG1
  ) where

import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as NB
import qualified RIO.ByteString            as B
import           RIO
import qualified RIO.Text                  as T
import           Test.Hspec                (pendingWith)

import           Hsed.Sim.Types            (defaultSimParams)
import           Hsed.Transport.InProc     (InProcOpts (..))
import           Hsed.Transport.Mhtx.Conn  (Conn, bindLoopback, connectionOptions, dialLoopback)
import           Hsed.Transport.Mhtx.Frame (pingText)
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

-- | Pending, naming group G1, while group G1's frames and connections are
--   the scaffold's stubs (M5c): every example of the suite binds, dials and
--   codes frames through them. Part 2 of group G2 deletes this once G1 is
--   merged.
needsG1 :: IO ()
needsG1 = do
  stubbed <- or <$> sequence [stub (void (evaluate connectionOptions)), stub (void (evaluate (B.length pingText)))]
  when stubbed $ pendingWith "group G1's Hsed.Transport.Mhtx.Frame and Conn (bindLoopback, dialLoopback, the codecs): pending until G1 is merged"
  where
    stub act = try act <&> either (\e -> "group G1 (" `T.isInfixOf` tshow (e :: SomeException)) (const False)
