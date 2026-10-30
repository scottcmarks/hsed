{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Core.FeatureSpec
Description : Test specifications for Hsed.Core.Feature
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The class on a small descriptor of its own (not the spec's: the spec's are
generated, @Hsed.Gen.\<Spec\>.Features@, and tested in hsed): a header pinned
to a code, a version and a length, reserved bits, two flags and a number.
Every record encodes and decodes back to itself, its pins and reserved bits
as the layout has them; a pin read with another value is a difference to
'decodeFeature' and an error to 'decodeFeatureStrict'; a record whose values
are missing or out of range is refused.
-}
module Hsed.Core.FeatureSpec (spec) where

import qualified RIO.ByteString        as B
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)

import           Hsed.Core.Feature
import           Hsed.Core.Layout


-- | A descriptor of three bytes: a 4-bit code pinned to 5, a 2-bit version
--   pinned to 1, a reserved bit, a flag; a length byte pinned to 1; a flag, a
--   3-bit count and four reserved bits.
data Toy = Toy { toyReady :: Bool, toyBusy :: Bool, toyCount :: Natural }
  deriving (Eq, Show)

instance Feature Toy where
  featureCode    = 5
  featureVersion = Just 1
  featureLength  = Just 1
  featureLayout  = Layout "Toy" 3
    [ spanning "Code" (0, 7) (0, 4) (Pinned 5), spanning "Version" (0, 3) (0, 2) (Pinned 1)
    , spanning "Reserved" (0, 1) (0, 1) Reserved, spanning "Ready" (0, 0) (0, 0) Flag
    , spanning "Length" (1, 7) (1, 0) (Pinned 1)
    , spanning "Busy" (2, 7) (2, 7) Flag, spanning "Count" (2, 6) (2, 4) Unsigned, spanning "Reserved" (2, 3) (2, 0) Reserved ]
    Nothing
  featureValues t = [flag "Ready" (toyReady t), flag "Busy" (toyBusy t), number "Count" (toyCount t)]
  featureFromValues vs = Toy <$> flagOf "Ready" vs <*> flagOf "Busy" vs <*> numberOf "Count" vs

spec :: Spec
spec = describe "a feature descriptor's record" $ do
  it "the layout tiles" $
    tiling (featureLayout @Toy) `shouldBe` []
  it "encodes a record with its pins and zero reserved bits" $
    encodeFeature (Toy True False 5) `shouldBe` Right "\x55\x01\x50"
  prop "every record decodes back to itself, with nothing differing and the rest kept" $ \ready busy (n :: Word8) ->
    let toy = Toy ready busy (fromIntegral (n `mod` 8))
    in (encodeFeature toy >>= decodeFeatureStrict . (<> "rest")) `shouldBe` Right (DecodedFeature toy [] "rest")
  it "reports a pin read with another value, and refuses it strictly" $ do
    let bytes = "\x95\x01\x50"   -- code 9
    fmap featureDifferences (decodeFeature @Toy bytes) `shouldBe` Right [PinDiffers "Code" 5 9]
    fmap decodedFeature (decodeFeature @Toy bytes) `shouldBe` Right (Toy True False 5)
    void (decodeFeatureStrict @Toy bytes) `shouldBe` Left (PinViolated "Code" 5 9)
  it "reports reserved bits read other than zero" $
    fmap featureDifferences (decodeFeature @Toy "\x55\x01\x5f") `shouldBe` Right [ReservedNotZero "Reserved" 15]
  it "refuses a count the field cannot hold, and a short input" $ do
    encodeFeature (Toy False False 8) `shouldBe` Left (OutOfRange "Count" 8)
    void (decodeFeature @Toy (B.take 2 "\x55\x01\x50")) `shouldBe` Left (Truncated 3 2)
  it "maps each bit to its field" $
    [ (pos, fieldName f) | (pos@(2, _), Just f) <- bitMap (featureLayout @Toy) ]
      `shouldBe` [((2, 7), "Busy"), ((2, 6), "Count"), ((2, 5), "Count"), ((2, 4), "Count"), ((2, 3), "Reserved"), ((2, 2), "Reserved"), ((2, 1), "Reserved"), ((2, 0), "Reserved")]
