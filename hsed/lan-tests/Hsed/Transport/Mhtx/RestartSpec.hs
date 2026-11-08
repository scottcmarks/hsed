{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.RestartSpec
Description : A hub restarted under the client: what was sent fails, the client reconnects, the restart noticed
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A hub restarted under a client (design 11 M5c, group G3's acceptance): the client opened on
hub A ("Hsed.Lan.Support".'withHubOn', 127.0.0.1 and an ephemeral port) and used after A's
bracket ends; a request A holds (its backend answering Level 0 Discovery's IF-RECV of 1536
bytes only once a gate opens) fails with a transport failure when A stops, and hub B, started
on A's port (ReuseAddr), never sees it; the client reconnects to B with a new server_id, which
it notices and judges unknown; a request issued while no hub listens fails naming the
restart; one issued after it is answered. Nothing sent is ever sent again: an IF-RECV in S3
delivers its packets once ("Hsed.Sim.Sync"). A run of the jig whose hub restarts under it
is "Hsed.Jig.WsTargetSpec"'s.
-}
module Hsed.Transport.Mhtx.RestartSpec (spec) where

import           RIO
import qualified RIO.Text                   as T
import           Test.Hspec

import           Hsed.Lan.Support           (withHubOn, withinMicros)
import           Hsed.Sim.Types             (defaultSimParams)
import           Hsed.Transport.InProc      (InProcOpts (..))
import           Hsed.Transport.Mhtx.Client
import           Hsed.Transport.Mhtx.Hub    (Backend (..), HubOpts (..), backendOf, defaultHubOpts)
import           Hsed.Wire                  (IfStatus (..), Request (..), Response (..), Unit (..), ifRecv)
import           Hsed.Wire.Internal         (Transport (..))


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Client, its hub restarted" $
  it "fails what hub A held when A stops, hub B on A's port never seeing it; reconnects to B, the restart noticed; fails a request issued in the outage naming the restart, answers one after" $
    withinMicros 20000000 "the restart" (do
      gate <- newEmptyMVar
      arrived <- newEmptyMVar
      seenByB <- newTVarIO []
      backendA <- holding arrived gate <$> backend
      backendB <- logging seenByB <$> backend
      (port, c, held) <- withHubOn 0 hubOpts backendA $ \port _ -> do
        c <- openMhtx (restartOpts port) >>= either (throwString . T.unpack) pure
        held <- async (ifRecv (clientTransport c) 1 1 heldLength)
        takeMVar arrived
        pure (port, c, held)
      -- hub A has stopped: its peers closed, its socket closed
      (status <$> wait held) >>= (`shouldSatisfy` failedWith "the request was sent, and is never sent again")
      during <- async (ifRecv (clientTransport c) 1 1 2048)
      threadDelay 50000
      withHubOn port hubOpts backendB $ \portB _ -> do
        portB `shouldBe` port
        (status <$> wait during) >>= (`shouldSatisfy` failedWith "not sent: the hub restarted, server_id ")
        notices <- clientNotices c
        notices `shouldSatisfy` any ("the hub restarted, server_id " `T.isPrefixOf`)
        clientVerdicts c >>= (`shouldSatisfy` (== Just MissedUnknown) . lastMaybe)
        (status <$> ifRecv (clientTransport c) 1 1 2048) `shouldReturn` Good
        readTVarIO seenByB >>= (`shouldSatisfy` notElem heldLength . map lenBytes)
        closeMhtx c
      putMVar gate ())

-- | The hubs' options: the control ops and the reset op allowed, a heartbeat the example never reaches.
hubOpts :: HubOpts
hubOpts = defaultHubOpts { hoAllowControl = True, hoAllowReset = True }

-- | A backend of the default simulator, in bytes, no hold.
backend :: IO Backend
backend = backendOf hubOpts defaultSimParams (InProcOpts UnitBytes 0) >>= either (throwString . T.unpack) pure

-- | The client of hub A's port: reconnects from 20 ms doubling to 200 ms, no jitter; 3 s for a reconnect while
--   disconnected, the default.
restartOpts :: Word16 -> ClientOpts
restartOpts port = (clientOpts port "/ws") { coBackoff = Backoff 20000 2 200000 0 }

-- | The length of the request hub A holds: Level 0 Discovery's IF-RECV of 1536 bytes, which nothing else asks for.
heldLength :: Word32
heldLength = 1536

-- | A backend whose IF-RECVs of 'heldLength' are held: their arrival signalled, their answer given once the gate
--   opens.
holding :: MVar () -> MVar () -> Backend -> Backend
holding arrived gate b = b { beTransport = (beTransport b) { tExchange = \r -> do
  when (lenBytes r == heldLength) (putMVar arrived () >> readMVar gate)
  tExchange (beTransport b) r } }

-- | A backend that keeps every request it is given.
logging :: TVar [Request] -> Backend -> Backend
logging seen b = b { beTransport = (beTransport b) { tExchange = \r -> do
  atomically (modifyTVar' seen (<> [r]))
  tExchange (beTransport b) r } }

-- | Whether a status is a transport failure whose text says what is given.
failedWith :: Text -> IfStatus -> Bool
failedWith reason = \s -> case s of
  TransportFailure why -> reason `T.isInfixOf` why
  _                    -> False

-- | The last of a list, if any.
lastMaybe :: [a] -> Maybe a
lastMaybe = foldl' (const Just) Nothing

