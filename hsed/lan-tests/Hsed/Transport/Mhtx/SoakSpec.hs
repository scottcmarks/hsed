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

With HSED_LAN_SOAK=1 alone (pending otherwise, a note saying so): the hub's
default heartbeat of 20000/60000 ms, and a client pinging every 20 s kept
connected for 45 s, past warp's 30 s connection timeout (s1-sources: warp's
settingsTimeout, which the hub pauses for its websockets, Warp.hs:490-491),
every ping answered (design 11 M5c, group G2's acceptance).
-}
module Hsed.Transport.Mhtx.SoakSpec (spec) where

import qualified GHC.Clock                 as Clock
import           RIO
import qualified RIO.Text                  as T
import           System.Environment        (lookupEnv)
import           Test.Hspec

import           Hsed.Lan.Support          (lanBackend, needsG1, withHub)
import           Hsed.Transport.Mhtx.Conn  (Conn (..), Incoming (..), dialLoopback)
import           Hsed.Transport.Mhtx.Frame (FromServer (..), Hello (..), fromServer, pingText)
import           Hsed.Transport.Mhtx.Hub   (defaultHubOpts, hubClientCount)


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Hub on 127.0.0.1, its default heartbeat" $
  it "keeps a client pinging every 20 s connected for 45 s, past warp's 30 s timeout, every ping answered (HSED_LAN_SOAK=1)" $ do
    soak <- lookupEnv "HSED_LAN_SOAK"
    unless (soak == Just "1") $ pendingWith "a soak of 45 s at the default heartbeat of 20000/60000 ms: run with HSED_LAN_SOAK=1"
    needsG1
    be <- lanBackend defaultHubOpts
    withHub defaultHubOpts be $ \port hub -> do
      kept <- dialLoopback port "/ws" 5000000 $ \c -> do
        started <- Clock.getMonotonicTime
        first' <- frame c
        case first' of
          ServerHello h -> (helloIntervalMs h, helloTimeoutMs h) `shouldBe` (20000, 60000)
          other         -> expectationFailure ("the first frame is not a hello: " <> show other)
        -- pings at 0, 20 and 40 s, each answered; at 45 s, past warp's 30 s, the socket open and a ping answered
        for_ [0, 20, 40, 45 :: Double] $ \at -> do
          now <- Clock.getMonotonicTime
          threadDelay (max 0 (round ((started + at - now) * 1000000)))
          connSendText c pingText
          frame c `shouldReturn` ServerPong
        hubClientCount hub
      kept `shouldBe` Right 1
  where
    -- the next frame, within 2 s (a pong's wait, conformance.dart:135)
    frame c = timeout 2000000 (connReceive c) >>= \case
      Just incoming@(InText _) -> pure (fromServer incoming)
      other                    -> throwString ("no frame within 2 s: " <> T.unpack (tshow other))
