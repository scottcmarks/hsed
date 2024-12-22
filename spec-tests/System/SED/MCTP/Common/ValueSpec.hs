{-|
Module      : System.SED.MCTP.Common.ValueSpec
Description : Test specifications for System.SED.MCTP.Common.Types
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
    -- prop "can be any Final" $
    --    (\f -> pg (Datum f) `shouldBe` Just (Datum f))

  -- describe "NamedValues" $ do
  --   prop "can be any Name with any Value" $
  --     (\nv -> pg nv `shouldBe` Just (nv::NamedValue))

  -- describe "List" $ do
  --   prop "can be any old list, which is WRONG" $
  --     (\l -> pg l `shouldBe` Just (l::List))
