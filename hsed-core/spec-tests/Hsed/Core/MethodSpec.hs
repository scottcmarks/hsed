{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Core.MethodSpec
Description : Test specifications for Hsed.Core.Method
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A method record as the spec compiler prints one (design example E9, Core
5.3.3.7 Set: two optional parameters, Where and Values): its invocation names
the optional parameters it is given by their 0-based index, whichever are left
out, and carries the method's UID.
-}
module Hsed.Core.MethodSpec (spec) where

import           RIO                           hiding (Set)
import           Test.Hspec

import           Hsed.Core.Method
import           System.SED.MCTP.Common.Call  (MethodCall (..), MethodHeader (..), Parameters (..))
import           System.SED.MCTP.Common.UID   (uid)
import           System.SED.MCTP.Common.Value (IsValue (..), NamedValue (..), Name (..), Datum (..), (.:))
import           System.SED.MCTP.Common.Token (Token (..))


-- | Core 5.3.3.7 as generated.
data Set = Set
  { setWhere  :: Maybe Value
  , setValues :: Maybe Value
  }
  deriving (Eq, Show)

instance MethodSignature Set where
  signature = Signature
    { sigMethod   = "Set"
    , sigInvokers = ["TableUID", "ObjectUID"]
    , sigRequired = []
    , sigOptional = [Param "Where" "row_address", Param "Values" "typeOr { Bytes : bytes, RowValues : list [ ColumnNumber = <type of column> ... ] }"]
    , sigResponse = Results [] []
    , sigAnswers  = Nothing
    , sigSpecRef  = "core-2.01:5.3.3.7"
    }
  requiredValues _ = []
  optionalValues m = [setWhere m, setValues m]

instance Method Set where
  methodUID = uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17

spec :: Spec
spec = describe "a generated method record" $ do
  let values = value (3 :: Natural)
      named i = (i :: Natural) .: values
  it "names an optional parameter by its index among the optional ones, whichever come before it" $
    parameters (Set Nothing (Just values)) `shouldBe` Parameters [] [named 1]
  it "gives both optional parameters in order" $
    parameters (Set (Just values) (Just values)) `shouldBe` Parameters [] [named 0, named 1]
  it "invokes the method's UID on the invoking ID" $ do
    let MethodCall header _ status = invocation (uid 0x00 0x00 0x08 0x02 0x00 0x00 0x00 0x01) (Set Nothing Nothing)
    header `shouldBe` MethodHeader (uid 0x00 0x00 0x08 0x02 0x00 0x00 0x00 0x01) (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)
    status `shouldBe` 0
  it "keeps the signature as printed" $ do
    map paramName (sigOptional (signature @Set)) `shouldBe` ["Where", "Values"]
    sigSpecRef (signature @Set) `shouldBe` "core-2.01:5.3.3.7"
  it "names its first optional parameter 0, an unsigned name" $
    case parameters (Set (Just values) Nothing) of
      Parameters [] [NamedValue (Name (Datum t)) _] -> t `shouldBe` Unsigned 0
      other                                         -> expectationFailure (show other)
