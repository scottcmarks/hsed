{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.ClientHeartbeatSpec
Description : The client's heartbeat at 200/600 ms over memory, against a scripted hub
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The client's heartbeat (mh_transport SPEC.md L57-62; design 11 M5c, group G3's acceptance)
at the conformance hub's 200 ms interval and 600 ms timeout, over
"Hsed.Transport.Mhtx.Conn".'memoryPair' against a scripted hub double, as
"Hsed.Transport.Mhtx.ClientSpec" scripts it: a ping every interval after hello; any message,
a binary one among them, proof of life, so that messages every interval keep the connection
past twice the timeout; and silence for the interval and the grace (200 + 400 ms) a close with
4000 and a dial again. Timed, so in hsed-lan-test, not hsed-test (design 13.34, L1); no socket
is opened.
-}
module Hsed.Transport.Mhtx.ClientHeartbeatSpec (spec) where

import           Control.Monad              (replicateM)
import qualified Data.Aeson.KeyMap          as KM
import qualified GHC.Clock                  as Clock
import           RIO
import qualified RIO.Text                   as T
import           Test.Hspec

import           Hsed.Sim.Types             (defaultSimParams)
import           Hsed.Sim.View              (setupOf)
import           Hsed.Transport.Mhtx.Client
import           Hsed.Transport.Mhtx.Conn   (Conn (..), Incoming (..), codeHeartbeat, memoryPair)
import           Hsed.Transport.Mhtx.Frame  (FromClient (..), Hello (..), SedApp (..), fromClient, helloText, pongText, sedAppValue)
import           Hsed.Wire                  (Unit (..))


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Client, its heartbeat at 200/600 ms (SPEC.md L57-62)" $ do
  it "pings every interval after hello" $ within $
    withHeartbeat $ \_ hub -> do
      -- each answered with pong, as a hub answers it, so that the client hears the hub within its 600 ms
      times <- replicateM 4 (pinged hub >> connSendText hub pongText >> Clock.getMonotonicTime)
      let gaps = zipWith (-) (drop 1 times) times
      gaps `shouldSatisfy` all (\g -> g >= 0.19 && g < 0.4)
  it "takes any message, a binary one among them, for proof of life; drops after the interval and the grace in silence, with 4000, and dials again" $ within $
    withHeartbeat $ \hubs hub -> do
      -- 1.4 s of messages every 200 ms, more than twice the 600 ms of silence that would drop it
      replicateM_ 7 (connSendBinary hub "\0" >> threadDelay 200000)
      silent <- Clock.getMonotonicTime
      connSendBinary hub "\0"
      isNothing <$> atomically (tryReadTQueue hubs) `shouldReturn` True
      untilClosed hub `shouldReturn` Just (codeHeartbeat, "heartbeat timeout")
      closedAfter <- subtract silent <$> Clock.getMonotonicTime
      closedAfter `shouldSatisfy` (\t -> t >= 0.6 && t < 1.2)
      void (atomically (readTQueue hubs))

-- | A client open on the double's hello of a 200 ms interval and a 600 ms timeout, its grace 400 ms; the test given
--   the connections to come and the first connection's hub side; closed after.
withHeartbeat :: (TQueue Conn -> Conn -> IO a) -> IO a
withHeartbeat act = do
  hubs <- newTQueueIO
  opening <- async (openMhtx (heartbeatOpts hubs))
  hub <- atomically (readTQueue hubs)
  connSendText hub (helloText (Hello 1 "server-a" 0 200 600 (KM.fromList [("sed", sedAppValue tperApp)])))
  wait opening >>= either (throwString . T.unpack) (\c -> act hubs hub `finally` closeMhtx c)

-- | The client's options over memory: each dial a 'memoryPair', its hub's side given to the test; a grace of 400 ms,
--   so that the interval and the grace are the hello's 600 ms timeout; a backoff of 1 ms, no jitter.
heartbeatOpts :: TQueue Conn -> ClientOpts
heartbeatOpts hubs = ClientOpts
  { coDial                = \use -> memoryPair >>= \(client, hub) -> atomically (writeTQueue hubs hub) >> Right <$> use client
  , coName                = "memory"
  , coConnectMicros       = 200000
  , coHelloMicros         = 200000
  , coRequestMicros       = 200000
  , coGraceMicros         = 400000
  , coReconnectWaitMicros = 200000
  , coBackoff             = Backoff 1000 2 4000 0
  }

-- | The double's app.sed: a TPer in bytes, everything allowed, the default setup.
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

-- | The next message, which must be a ping.
pinged :: Conn -> IO ()
pinged hub = connReceive hub >>= \incoming -> fromClient incoming `shouldBe` ClientPing

-- | The connection's close, what came before it (the client's pings) passed over: its code and reason.
untilClosed :: Conn -> IO (Maybe (Word16, Text))
untilClosed hub = connReceive hub >>= \case
  InClosed c -> pure c
  _          -> untilClosed hub

-- | An example's body within 5 s: a client that hangs fails its example, not the suite.
within :: IO a -> IO a
within act = timeout 5000000 act >>= maybe (throwString "not done within 5 s") pure
