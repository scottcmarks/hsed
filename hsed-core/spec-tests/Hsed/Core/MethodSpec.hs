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
out, carries the method's UID and the status asked; and the reading back, the
parameters of a call placed against a signature (3.2.4.1), each way they can
fail to be the signature's, and an answer's parameters read by the same rule.
-}
module Hsed.Core.MethodSpec (spec) where

import           RIO                           hiding (Set)
import           Test.Hspec

import           Hsed.Core.Call                (MethodCall (..), MethodHeader (..), Parameters (..))
import           Hsed.Core.Method
import           Hsed.Core.Token               (Token (..))
import           Hsed.Core.UID                 (uid)
import           Hsed.Core.Value               (Datum (..), IsValue (..), List (..), Name (..), NamedValue (..), Value (..), (.:))


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
spec = do
  describe "a generated method record" $ do
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
    it "invokes with the status asked, 0 being invocation's" $ do
      _statusCode (invocationWith 12 (uid 0x00 0x00 0x08 0x02 0x00 0x00 0x00 0x01) (Set Nothing Nothing)) `shouldBe` 12
      invocationWith 0 (uid 0x00 0x00 0x08 0x02 0x00 0x00 0x00 0x01) (Set (Just values) Nothing)
        `shouldBe` invocation (uid 0x00 0x00 0x08 0x02 0x00 0x00 0x00 0x01) (Set (Just values) Nothing)
    it "keeps the signature as printed" $ do
      map paramName (sigOptional (signature @Set)) `shouldBe` ["Where", "Values"]
      sigSpecRef (signature @Set) `shouldBe` "core-2.01:5.3.3.7"
    it "names its first optional parameter 0, an unsigned name" $
      case parameters (Set (Just values) Nothing) of
        Parameters [] [NamedValue (Name (Datum t)) _] -> t `shouldBe` Unsigned 0
        other                                         -> expectationFailure (show other)

  describe "positional, a call's parameters read against a signature" $ do
    let a = Param "A" "uinteger"
        w = Param "W" "row_address"
        v = Param "V" "bytes"
        one = value (1 :: Natural)
        two = value (2 :: Natural)
        named i x = (i :: Natural) .: x
    it "places the required values and each optional one by its index, Nothing where none is given" $ do
      positional [a] [w, v] (Parameters [one] [named 1 two]) `shouldBe` Right ([one], [Nothing, Just two])
      positional [a] [w, v] (Parameters [one] [named 0 one, named 1 two]) `shouldBe` Right ([one], [Just one, Just two])
      positional [] [] (Parameters [] []) `shouldBe` Right ([], [])
    it "counts the required values (3.2.4.1)" $ do
      positional [a] [w] (Parameters [] []) `shouldBe` Left (RequiredCount 1 0)
      positional [] [w] (Parameters [one] []) `shouldBe` Left (RequiredCount 0 1)
    it "refuses an optional parameter named by other than a uinteger, or past the signature's" $ do
      positional [] [w, v] (Parameters [] [("Where" :: String) .: one]) `shouldBe` Left (OptionalNotIndexed (Name (Datum (Bytes "Where"))))
      positional [] [w, v] (Parameters [] [named 2 one]) `shouldBe` Left (OptionalIndex 2 2)
      positional [] [] (Parameters [] [named 0 one]) `shouldBe` Left (OptionalIndex 0 0)
    it "refuses optional parameters out of order, or one given twice" $ do
      positional [] [w, v] (Parameters [] [named 1 one, named 0 two]) `shouldBe` Left (OptionalOrder 1 0)
      positional [] [w, v] (Parameters [] [named 0 one, named 0 two]) `shouldBe` Left (OptionalOrder 0 0)
    it "reads the record's own parameters back" $
      positional (sigRequired (signature @Set)) (sigOptional (signature @Set)) (parameters (Set Nothing (Just two)))
        `shouldBe` Right ([], [Nothing, Just two])
    it "says each error in words" $
      map (\e -> paramErrorText e == "") [RequiredCount 1 0, OptionalNotIndexed (Name (Datum (Bytes "x"))), OptionalIndex 2 2, OptionalOrder 1 0, WrongShape "a list"]
        `shouldBe` [False, False, False, False, False]

  describe "namedComponents, a grouped abstract type's components (5.1.4.1)" $ do
    let named i x = N ((i :: Natural) .: x)
        one = value (1 :: Natural)
    it "places each component given, in order, Nothing for the rest" $ do
      namedComponents 3 [named 0 one, named 2 one] `shouldBe` Right [Just one, Nothing, Just one]
      namedComponents 2 [] `shouldBe` Right [Nothing, Nothing]
    it "refuses a component that is no Named value" $
      namedComponents 2 [one] `shouldSatisfy` \r -> case r of
        Left (WrongShape _) -> True
        _                   -> False

  describe "responseParameters, an answer given as an invocation" $ do
    let p = Param "Properties" "list [ name = value ... ]"
        h = Param "HostProperties" "list [ name = value ... ]"
        tper = L (List [N (("MaxMethods" :: String) .: (1 :: Natural))])
        host = L (List [])
    it "gives the required values and each optional one Named by its index, as a record's parameters are" $ do
      responseParameters [p] [h] [tper] [Just host] `shouldBe` Parameters [tper] [(0 :: Natural) .: host]
      responseParameters [p] [h] [tper] [Nothing] `shouldBe` Parameters [tper] []
    it "is read back by positional over the same parameters" $ do
      positional [p] [h] (responseParameters [p] [h] [tper] [Just host]) `shouldBe` Right ([tper], [Just host])
      positional [p] [h] (responseParameters [p] [h] [tper] [Nothing]) `shouldBe` Right ([tper], [Nothing])
    it "carries no more values than the signature has parameters" $
      responseParameters [p] [] [tper, host] [Just host] `shouldBe` Parameters [tper] []
