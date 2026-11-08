{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Mhtx.ConnSpec
Description : Test specifications for Hsed.Transport.Mhtx.Conn over memory: text, binary and close codes both ways
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

memoryPair carries text, binary and a close with its code both ways, and a receive after a
close gives the close; no socket is opened (design 11 M5c, group G1's acceptance).

Not yet written: group G1's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Transport.Mhtx.ConnSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Conn" $
  it "carries text, binary and a close with its code both ways in memory" $
    pendingWith "not yet written: group G1 (Hsed.Transport.Mhtx.ConnSpec)"
