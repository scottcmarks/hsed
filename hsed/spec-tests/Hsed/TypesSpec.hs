{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.TypesSpec
Description : Test specifications for the umbrella Hsed.Types
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The umbrella brings the generated enumerations and, since the M2d review,
"Hsed.Core.Types" with them: an enumeration's value is 'enumValue', the
spec's, which for Core T138 table_kind (range 1..8) is 'fromEnum' plus one.
-}
module Hsed.TypesSpec (spec) where

import           RIO
import           Test.Hspec

import           Hsed.Types


spec :: Spec
spec = describe "the umbrella Hsed.Types" $
  it "encodes an enumeration with enumValue, the spec's value, not with fromEnum (T138 starts at 1)" $ do
    range @Core_table_kind `shouldBe` (1, 8)
    [ enumValue x | x <- [minBound .. maxBound :: Core_table_kind] ] `shouldBe` [1 .. 8]
    [ fromEnum x | x <- [minBound .. maxBound :: Core_table_kind] ] `shouldBe` [0 .. 7]
    enumFromValue 2 `shouldBe` Just Core_table_kind_Byte
