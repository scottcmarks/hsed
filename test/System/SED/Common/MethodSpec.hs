{-|
Module      : System.SED.Common.MethodSpec
Description : Test specifications for System.SED.Common.Types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Test specifications for System.SED.Common.Method
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.SED.Common.MethodSpec (spec) where

import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           System.SED.Common.Method
import           System.SED.Common.StreamItem


-- | test suite
spec :: Spec
spec = do
  describe "Names" $ do
    prop "can be any Final" $
      (\f -> pg (Name f) `shouldBe` Just (Name f))
