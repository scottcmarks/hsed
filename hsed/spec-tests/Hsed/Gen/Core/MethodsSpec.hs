{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Gen.Core.MethodsSpec
Description : The generated method records read back: every record through its parameters, the methods list by UID
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The decoding direction of the generated method records (design section 11,
M4a; the M2b review): @methods@ lists every record with a UID, 52, Stir's
having none, each UID once and found by 'methodWithUID'; for every record,
values of its signature's arity make a record ('fromValues') that
'decodeParameters' reads back from 'parameters', whatever the values; and
'fromCall' takes an invocation of the record's method and no other.
-}
module Hsed.Gen.Core.MethodsSpec (spec) where

import           Data.List             (nub)
import           Data.Proxy            (Proxy (..))
import qualified RIO.Text              as T
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Hsed.Core.Method
import           Hsed.Core.Token       (Token (..))
import           Hsed.Core.UID         (uNull)
import           Hsed.Core.Value       (Datum (..), List (..), Value (..))
import qualified Hsed.Gen.Core.Methods as Methods
import           Hsed.Gen.Core.UIDs    (uMethodID_Get, uMethodID_Set, uProperties, uSMUID, uThisSP)


-- | Values for a signature's arity: a required value is a datum or a list
--   (never a Named value, which would be read as an optional parameter), an
--   optional one any value or none.
valuesFor :: Signature -> Gen ([Value], [Maybe Value])
valuesFor sg = (,) <$> vectorOf (length (sigRequired sg)) required <*> vectorOf (length (sigOptional sg)) arbitrary
  where required = frequency [(10, D <$> arbitrary), (5, L <$> arbitrary)]

spec :: Spec
spec = describe "the generated method records" $ do
  it "lists the 52 methods with a UID, every block's but Stir's, each UID once" $ do
    length Methods.methods `shouldBe` 52
    length (nub (map someMethodUID Methods.methods)) `shouldBe` 52
    map (sigMethod . someSignature) (take 3 Methods.methods) `shouldBe` ["Properties", "StartSession", "SyncSession"]
    filter ((== "Stir") . sigMethod . someSignature) Methods.methods `shouldSatisfy` null
  it "finds a method by its UID, and none by a UID no method has" $ do
    fmap (sigMethod . someSignature) (methodWithUID Methods.methods uMethodID_Get) `shouldBe` Just "Get"
    fmap (sigMethod . someSignature) (methodWithUID Methods.methods uProperties) `shouldBe` Just "Properties"
    fmap someMethodUID (methodWithUID Methods.methods uNull) `shouldBe` Nothing
  prop "reads every record back from its parameters: decodeParameters . parameters is Right, whatever the values" $
    forAllShow (elements Methods.methods) (T.unpack . sigMethod . someSignature) $ \case
      SomeMethod (_ :: Proxy m) -> forAll (valuesFor (signature @m)) $ \(rs, os) ->
        case fromValues rs os :: Maybe m of
          Nothing -> counterexample "fromValues refused values of the signature's arity" False
          Just m  -> decodeParameters (parameters m) === Right m
  it "reads Get from its invocation, and refuses another method's" $ do
    let get = Methods.Get (L (List []))
    fromCall @Methods.Get (invocation uThisSP get) `shouldBe` Just (Right get)
    fromCall @Methods.Get (invocation uSMUID (Methods.Properties Nothing)) `shouldBe` Nothing
    fmap (fmap Methods.setWhere) (fromCall @Methods.Set (invocation uThisSP (Methods.Set (Just (D (Datum (Unsigned 1)))) Nothing)))
      `shouldBe` Just (Right (Just (D (Datum (Unsigned 1)))))
    someMethodUID <$> methodWithUID Methods.methods uMethodID_Set `shouldBe` Just uMethodID_Set
  it "refuses parameters that are not the signature's: a required value too many, an optional one past the signature" $ do
    decodeParameters @Methods.Get (parameters (Methods.Properties Nothing)) `shouldBe` Left (RequiredCount 1 0)
    fromCall @Methods.Properties (invocation uSMUID (Methods.Get (D (Datum (Unsigned 0)))))
      `shouldBe` Nothing
