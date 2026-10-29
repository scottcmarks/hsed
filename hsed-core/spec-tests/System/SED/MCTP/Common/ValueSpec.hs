{-|
Module      : System.SED.MCTP.Common.ValueSpec
Description : Test specifications for System.SED.MCTP.Common.Value
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Test specifications for System.SED.MCTP.Common.Value
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.SED.MCTP.Common.ValueSpec (spec) where

import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           System.SED.MCTP.Common.StreamItem
import           System.SED.MCTP.Common.Value


-- | test suite
spec :: Spec
spec = do
  describe "Datums" $ do
    prop "can be any Datum" $
       (\n -> pg n `shouldBe` Just (n::Datum))
    it "do not parse a Named value" $
       (parseByteString (generate ((1::Natural) .: (2::Natural))) :: Either String Datum)
         `shouldSatisfy` isLeft
    it "do not parse a List" $
       (parseByteString (generate (List [])) :: Either String Datum)
         `shouldSatisfy` isLeft

  describe "Names" $ do
    prop "can be any Name" $
      (\n -> pg n `shouldBe` Just (n::Name))

  describe "NamedValues" $ do
    prop "can be any Name with any Value" $
      (\nv -> pg nv `shouldBe` Just (nv::NamedValue))

  describe "Lists" $ do
    prop "can be any list of Values" $
      (\l -> pg l `shouldBe` Just (l::List))

  describe "Values" $ do
    prop "can be any Datum, Named value or List" $
      (\v -> pg v `shouldBe` Just (v::Value))
