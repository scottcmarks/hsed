{-|
Module      : System.SED.MCTP.Common.ColumnTypes.TH
Description : Test specifications for System.SED.MCTP.Common.ColumnTypes.THSpec
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Test specifications for System.SED.MCTP.Common.ColumnTypes.TH
-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.SED.MCTP.Common.ColumnTypes.THSpec (spec) where

import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck            ()

import           System.SED.MCTP.Common.ColumnTypes.TH ()

-- | test suite
spec :: Spec
spec = do
    describe "The Column Types Template Haskell test suite" $ do
        it "is completely unimplemented so far." $
          pendingWith "Code something!"
