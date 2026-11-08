{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.SoakSpec
Description : The hub's default heartbeat held past warp's 30 s timeout (HSED_LAN_SOAK=1 alone)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

With HSED_LAN_SOAK=1 alone (pending otherwise, a note saying so), at the
hub's default heartbeat of 20000/60000 ms: a client pinging every 20 s kept
connected for 45 s, past warp's 30 s connection timeout, every ping answered
(design 11 M5c, group G2's acceptance); and beside it a client that connects
10 s after the hub starts and never pings, kept until the hub's own sweep
closes it with 4000, 60 to 80 s after its hello (SPEC.md L58-60), never cut
first by warp. The silent client is what shows the hub's warp timeout
("Hsed.Transport.Mhtx.Hub".@warpTimeout@): warp tickles its timeout at every
byte a websocket sends or receives (its Response.hs:254-262), which undoes
the pause it offers (Warp.hs:490-491), and its timeout manager, ticking every
period from the hub's start, kills a connection idle through two ticks; so
that at warp's own 30 s a client pinging every 20 s is never cut, and the
silent one, its hello 10 s in, is cut at the 60 s tick with no close code, 10
s before the hub's sweep would close it. About 80 s in all.
-}
module Hsed.Transport.Mhtx.SoakSpec (spec) where

import qualified GHC.Clock                 as Clock
import           RIO
import qualified RIO.Text                  as T
import           System.Environment        (lookupEnv)
import           Test.Hspec

import           Hsed.Lan.Support          (lanBackend, withHub)
import           Hsed.Transport.Mhtx.Conn  (Conn (..), Incoming (..), dialLoopback)
import           Hsed.Transport.Mhtx.Frame (FromServer (..), Hello (..), fromServer, pingText)
import           Hsed.Transport.Mhtx.Hub   (defaultHubOpts)


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Hub on 127.0.0.1, its default heartbeat" $
  it "keeps a client pinging every 20 s past warp's 30 s timeout, and one silent until the hub's 4000, never cut first by warp (HSED_LAN_SOAK=1)" $ do
    soak <- lookupEnv "HSED_LAN_SOAK"
    unless (soak == Just "1") $ pendingWith "a soak of about 80 s at the default heartbeat of 20000/60000 ms: run with HSED_LAN_SOAK=1"
    be <- lanBackend defaultHubOpts
    withHub defaultHubOpts be $ \port _ -> do
      (pinging, silent) <- concurrently (client port (pings [0, 20, 40, 45]))
                                        (threadDelay 10000000 >> client port silentUntilClosed)
      pinging `shouldBe` Right ()
      case join silent of
        Right (code, elapsed) -> do
          code `shouldBe` Just (4000, "heartbeat timeout")
          elapsed `shouldSatisfy` (\s -> s >= 59.5 && s <= 81)
        Left why              -> expectationFailure (T.unpack why)
  where
    -- a client of the hub, its hello read and checked first
    client port act = dialLoopback port "/ws" 5000000 $ \c -> do
      started <- Clock.getMonotonicTime
      frame 2 c >>= \case
        ServerHello h -> (helloIntervalMs h, helloTimeoutMs h) `shouldBe` (20000, 60000)
        other         -> expectationFailure ("the first frame is not a hello: " <> show other)
      act started c
    -- a ping at each of these seconds after the hello, each answered
    pings :: [Double] -> Double -> Conn -> IO ()
    pings ats started c = for_ ats $ \at -> do
      waitUntil (started + at)
      connSendText c pingText
      frame 2 c `shouldReturn` ServerPong
    -- nothing sent: the close, and how long after the hello it came (the hub's sweep every 20 s past its 60 s)
    silentUntilClosed :: Double -> Conn -> IO (Either Text (Maybe (Word16, Text), Double))
    silentUntilClosed started c = timeout 81000000 (connReceive c) >>= \case
      Just (InClosed why) -> Clock.getMonotonicTime <&> \now -> Right (why, now - started)
      other               -> pure (Left ("the silent client: no close within 81 s, but " <> tshow other))
    waitUntil t = Clock.getMonotonicTime >>= \now -> threadDelay (max 0 (round ((t - now) * 1000000)))
    -- the next text frame, within so many seconds (a pong's wait, conformance.dart:135)
    frame :: Int -> Conn -> IO FromServer
    frame seconds c = timeout (seconds * 1000000) (connReceive c) >>= \case
      Just incoming@(InText _) -> pure (fromServer incoming)
      other                    -> throwString ("no frame within " <> show seconds <> " s: " <> T.unpack (tshow other))
