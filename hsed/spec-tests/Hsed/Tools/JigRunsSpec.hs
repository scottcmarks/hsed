{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Tools.JigRunsSpec
Description : Test specifications for tools/jig-runs.sh: it parses, and its usage errors
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

tools/jig-runs.sh parses (bash -n), and -w's usage errors exit 2: runs 7, 8 and 9 with -w
(design 11 M5c, group G3's acceptance).

Not yet written: group G3's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Tools.JigRunsSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "tools/jig-runs.sh" $
  it "parses, and refuses -w with runs 7, 8 or 9" $
    pendingWith "not yet written: group G3 (Hsed.Tools.JigRunsSpec)"
