{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Mhtx.RestartSpec
Description : A hub restarted under the client: what was sent fails, the client reconnects, the restart noticed
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A request held by hub A fails TransportFailure when A stops, B never seeing it; the client
reconnects to B on the same port with a new server_id and notices it; a request issued during
the outage fails naming the restart, one after it succeeds (design 11 M5c, group G3's
acceptance).

Not yet written: group G3's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Transport.Mhtx.RestartSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Client" $
  it "fails what was sent when its hub stops, and reconnects to the next" $
    pendingWith "not yet written: group G3 (Hsed.Transport.Mhtx.RestartSpec)"
