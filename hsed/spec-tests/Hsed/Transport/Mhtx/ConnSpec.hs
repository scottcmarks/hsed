{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.ConnSpec
Description : Test specifications for Hsed.Transport.Mhtx.Conn over memory: text, binary and close codes both ways
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

memoryPair carries text, binary and a close with its code both ways, in order; a close is
echoed by the end that receives it, and a receive after a close gives the close again; nothing
is sent after a close, and data that crosses a close is dropped, as a websocket's closing
handshake has it (RFC 6455 5.5.1, 7.1.1); a close's reason is cut as a close frame cuts it.
No socket is opened: the loopback examples are hsed-lan-test's ConnLoopbackSpec (design 11
M5c, group G1's acceptance).
-}
module Hsed.Transport.Mhtx.ConnSpec (spec) where

import           Control.Monad            (replicateM)
import qualified RIO.Text                 as T
import           RIO
import           Test.Hspec

import           Hsed.Transport.Mhtx.Conn (Conn (..), Incoming (..), codeBadToken, codeHeartbeat, codeNormal, memoryPair)


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Conn" $ describe "memoryPair" $ do
  it "carries text and binary both ways, in order" $ do
    (a, b) <- memoryPair
    connSendText a "{\"type\":\"ping\"}"
    connSendBinary a "\0\1\2"
    connSendText a "second"
    connSendBinary b "\255"
    connSendText b "{\"type\":\"pong\"}"
    replicateM 3 (connReceive b) `shouldReturn` [InText "{\"type\":\"ping\"}", InBinary "\0\1\2", InText "second"]
    replicateM 2 (connReceive a) `shouldReturn` [InBinary "\255", InText "{\"type\":\"pong\"}"]
    connName a `shouldNotBe` connName b

  it "carries a close with its code and reason, echoed by the end that receives it; a receive after it gives it again" $ do
    (a, b) <- memoryPair
    connClose a codeHeartbeat "heartbeat timeout"
    connReceive b `shouldReturn` InClosed (Just (4000, "heartbeat timeout"))
    connReceive b `shouldReturn` InClosed (Just (4000, "heartbeat timeout"))
    connReceive a `shouldReturn` InClosed (Just (4000, "heartbeat timeout"))
    connReceive a `shouldReturn` InClosed (Just (4000, "heartbeat timeout"))

  it "carries a close the other way, the close sent once" $ do
    (a, b) <- memoryPair
    connClose b codeBadToken "bad token"
    connClose b codeNormal "a second close is not sent"
    connReceive a `shouldReturn` InClosed (Just (4001, "bad token"))
    connReceive b `shouldReturn` InClosed (Just (4001, "bad token"))

  it "sends nothing after a close, and drops data that crosses one" $ do
    (a, b) <- memoryPair
    connSendText b "crosses the close"
    connClose a codeNormal "done"
    connSendText a "after the close"
    connSendBinary a "after the close"
    -- b reads a's close (its text was sent before it) and echoes it; a drops b's text, sent before
    -- b read the close, and reads the echo
    connReceive b `shouldReturn` InClosed (Just (1000, "done"))
    connSendText b "after the echo"
    connReceive a `shouldReturn` InClosed (Just (1000, "done"))
    connReceive b `shouldReturn` InClosed (Just (1000, "done"))

  it "cuts a close's reason to the 123 bytes of UTF-8 a close frame carries" $ do
    (a, b) <- memoryPair
    connClose a codeNormal (T.replicate 100 "\233")
    connReceive b >>= \case
      InClosed (Just (1000, reason)) -> reason `shouldBe` T.replicate 61 "\233" <> "\65533"
      other                          -> expectationFailure ("not the close: " <> show other)
