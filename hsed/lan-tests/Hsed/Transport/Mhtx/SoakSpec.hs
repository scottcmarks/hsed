{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Mhtx.SoakSpec
Description : The hub's default heartbeat held past warp's 30 s timeout (HSED_LAN_SOAK=1 alone)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

With HSED_LAN_SOAK=1 alone: the default heartbeat of 20000/60000, a client pinging every 20 s
kept connected 45 s, past warp's 30 s connection timeout (design 11 M5c, group G2's
acceptance).

Not yet written: group G2's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Transport.Mhtx.SoakSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Hub" $
  it "keeps a client pinging every 20 s connected past warp's 30 s" $
    pendingWith "not yet written: group G2 (Hsed.Transport.Mhtx.SoakSpec)"
