{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim.ViewSpec
Description : Test specifications for Hsed.Sim.View: each query's answer the reader it replaces
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Each query's answer equals the be239b5 reader it replaces, restated here with its source
line, over generated states and at both Original Factory States (design 11 M5c, group G4's
acceptance).

Not yet written: group G4's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Sim.ViewSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Sim.View" $
  it "answers each query as the reader it replaces" $
    pendingWith "not yet written: group G4 (Hsed.Sim.ViewSpec)"
