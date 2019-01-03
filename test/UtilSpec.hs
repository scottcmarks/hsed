{-# LANGUAGE NoImplicitPrelude #-}
module UtilSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "plus2" $ do
    it "adds to zero (basic check)" $ plus2 0 `shouldBe` 2
    it "adds to 40 to give the answer to Life, the Universe, and Everything" $ plus2 40 `shouldBe` 42
    it "overflows" $ plus2 maxBound `shouldBe` minBound + 1
    prop "is the inverse of minus 2" $ \i -> plus2 i - 2 `shouldBe` i
