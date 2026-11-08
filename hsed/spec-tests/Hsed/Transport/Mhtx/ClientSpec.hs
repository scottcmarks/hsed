{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Mhtx.ClientSpec
Description : Test specifications for Hsed.Transport.Mhtx.Client over memory, against a scripted hub
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The client over memoryPair against a scripted hub double, then the real hub: open, ids,
timeouts, a dropped socket's requests failed and never re-sent, reconnects judged, the
heartbeat, backoffDelay and judgeMissed as mh_transport's Dart, the transport (design 11 M5c,
group G3's acceptance).

Not yet written: group G3's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Transport.Mhtx.ClientSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Client" $
  it "opens on a hello of a TPer, fails what a dropped socket held, and never re-sends" $
    pendingWith "not yet written: group G3 (Hsed.Transport.Mhtx.ClientSpec)"
