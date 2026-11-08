{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.OptionsSpec
Description : Test specifications for Hsed.Jig.Options: the ws:// target's accepted and refused forms
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Every accepted and refused form of a ws:// target: ws://127.0.0.1:PORT/PATH alone, wss://,
another host, no port or port 0, user info, a query or fragment, and a pair with a ws:// end
refused with exit 2, nothing opened (design 11 M5c, group G3's acceptance).

Not yet written: group G3's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Jig.OptionsSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Jig.Options" $
  it "takes ws://127.0.0.1:PORT/PATH alone" $
    pendingWith "not yet written: group G3 (Hsed.Jig.OptionsSpec)"
