{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Mhtx.ConformanceSpec
Description : The eight checks of mh_transport's SPEC.md re-expressed against the hub on 127.0.0.1, at 200/600 ms
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

mh_transport SPEC.md's checks 1-7 (L113-129) with conformance.dart's waits, the binary-frame
check, the hooks over raw HTTP, against a hub on bindLoopback 0 at a heartbeat of 200 ms and a
timeout of 600 ms (design 11 M5c, group G2's acceptance; check 8 with group G3's client).

Not yet written: group G2's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Transport.Mhtx.ConformanceSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Hub" $
  it "passes SPEC.md's checks 1 to 7 and the binary-frame check at 200/600 ms" $
    pendingWith "not yet written: group G2 (Hsed.Transport.Mhtx.ConformanceSpec)"
