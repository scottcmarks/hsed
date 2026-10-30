{-|
Module      : Hsed.Core.CallSpec
Description : Test specifications for Hsed.Core.Call
Copyright   : (c) Magnolia Heights R&D, 2026
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Method invocation and response encoding (Core Spec 2.01 section 3.2.4.2).
-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hsed.Core.CallSpec (spec) where

import           Data.ByteString                           (pack)
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Hsed.Core.Call
import           Hsed.Core.StreamItem
import           Hsed.Core.UID                             (UID, uid)
import           Hsed.Core.Value

-- | The SMUID and the Properties method UID (Core Spec Tables 239 and 241),
-- spelled out here so the codec's tests do not depend on the spec data package.
uSMUID, uProperties :: UID
uSMUID      = uid 0x00 0x00 0x00 0x00  0x00 0x00 0x00 0xFF
uProperties = uid 0x00 0x00 0x00 0x00  0x00 0x00 0xFF 0x01


-- | test suite
spec :: Spec
spec = do
  describe "MethodHeader" $ do
    prop "round-trips" $
      (\h -> pg h `shouldBe` Just (h::MethodHeader))

  describe "Parameters" $ do
    prop "round-trip" $
      (\p -> pg p `shouldBe` Just (p::Parameters))
    it "are a List: required values first, then optional Named values" $
      generate (Parameters [d 1, L (List [d 2])] [optionalParameter 0 (d 3)])
        `shouldBe` pack [0xF0, 0x01, 0xF0, 0x02, 0xF1, 0xF2, 0x00, 0x03, 0xF3, 0xF1]
    it "keep optional parameters out of the required ones" $
      parseByteString (pack [0xF0, 0x01, 0xF2, 0x00, 0x03, 0xF3, 0xF1])
        `shouldBe` Right (Parameters [d 1] [optionalParameter 0 (d 3)])
    it "reject a required parameter after an optional one" $
      (parseByteString (pack [0xF0, 0xF2, 0x00, 0x03, 0xF3, 0x01, 0xF1]) :: Either String Parameters)
        `shouldSatisfy` isLeft

  describe "MethodCall" $ do
    prop "round-trips" $
      (\c -> pg c `shouldBe` Just (c::MethodCall))
    it "encodes SMUID.Properties[] as the spec's token stream" $
      generate (MethodCall (MethodHeader uSMUID uProperties) (Parameters [] []) 0)
        `shouldBe` pack [ 0xF8                                                    -- Call
                        , 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF    -- SMUID
                        , 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x01    -- Properties
                        , 0xF0, 0xF1                                              -- no parameters
                        , 0xF9                                                    -- EndOfData
                        , 0xF0, 0x00, 0x00, 0x00, 0xF1 ]                          -- status list
    it "rejects a status list whose second value is not zero" $
      (parseByteString (pack [ 0xF8
                             , 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF
                             , 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x01
                             , 0xF0, 0xF1, 0xF9
                             , 0xF0, 0x00, 0x01, 0x00, 0xF1 ]) :: Either String MethodCall)
        `shouldSatisfy` isLeft

  describe "MethodResponse" $ do
    prop "round-trips" $
      (\r -> pg r `shouldBe` Just (r::MethodResponse))
    it "encodes an empty result with status SUCCESS" $
      generate (MethodResponse [] 0)
        `shouldBe` pack [0xF0, 0xF1, 0xF9, 0xF0, 0x00, 0x00, 0x00, 0xF1]
