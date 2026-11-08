{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Mhtx.HubSpec
Description : Test specifications for Hsed.Transport.Mhtx.Hub over memory, its clock injected
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The hub over memoryPair, its clock injected and its backend the real in-process simulator:
hello first, seq, liveness and the sweep, requests in order with protocol_error, the reset op
and its broadcast, the control ops, the hooks and the token (design 11 M5c, group G2's
acceptance).

Not yet written: group G2's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Transport.Mhtx.HubSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Hub" $
  it "says hello first, keeps seq, sweeps the silent, and answers requests in order" $
    pendingWith "not yet written: group G2 (Hsed.Transport.Mhtx.HubSpec)"
