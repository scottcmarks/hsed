{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Mhtx.FrameSpec
Description : Test specifications for Hsed.Transport.Mhtx.Frame: the LAN leg's frames coded, decoded and sorted
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The frames of design 6.3 round trip, base64 payloads are canonical, what arrives is sorted
(a request, a request refused, a frame ignored), and the view's codec covers every query and
answer (design 11 M5c, group G1's acceptance).

Not yet written: group G1's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Transport.Mhtx.FrameSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Frame" $
  it "codes design 6.3's frames, refuses what a request may not be, and ignores the rest" $
    pendingWith "not yet written: group G1 (Hsed.Transport.Mhtx.FrameSpec)"
