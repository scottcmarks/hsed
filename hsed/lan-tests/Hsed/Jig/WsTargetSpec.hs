{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.WsTargetSpec
Description : A run over ws://127.0.0.1:<port>/ws against the same run in process: whole transcripts equal
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

An in-test hub on bindLoopback 0 serves a run over ws://127.0.0.1:PORT/ws, its transcript
equal entry for entry to the same run's in process, its reports equal but for the target; the
reset clause over the reset op; the open's refusals (design 11 M5c, group G3's acceptance).

Not yet written: group G3's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Jig.WsTargetSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Jig.Target" $
  it "gives a ws:// run's transcript equal to the in-process run's" $
    pendingWith "not yet written: group G3 (Hsed.Jig.WsTargetSpec)"
