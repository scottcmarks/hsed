{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Core.FeatureSpec
Description : Test specifications for Hsed.Core.Feature
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The class on a small descriptor of its own (not the spec's: the spec's are
generated, @Hsed.Gen.\<Spec\>.Features@, and tested in hsed), written as the
@feature@ rule writes its instances, a "Hsed.Core.Record" record with a
'Feature' instance: a two-byte header of a code pinned to 5, a version pinned
to 1 and a length pinned to 1, reserved bits, two flags and a number. Every
record encodes and decodes back to itself, its pins and reserved bits as the
layout has them; a pin read with another value is a difference to
'decodeFeature' and an error to 'decodeFeatureStrict'; the input after a
descriptor starts where its length field says, whatever its pin says, and a
length field giving fewer bytes than the layout's is refused with where the
descriptor ends; a record whose values are missing or out of range is
refused.
-}
module Hsed.Core.FeatureSpec (spec) where

import qualified RIO.ByteString        as B
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)

import           Hsed.Core.Feature
import           Hsed.Core.Layout
import           Hsed.Core.Record


-- | A descriptor of three bytes: a 4-bit code pinned to 5, a 2-bit version
--   pinned to 1, a reserved bit, a flag; a length byte pinned to 1, the
--   header's end; a flag, a 3-bit count and four reserved bits.
data Toy = Toy { toyReady :: !Bool, toyBusy :: !Bool, toyCount :: !Word8 }
  deriving (Eq, Show)

toyLayout :: Layout
toyLayout = Layout "Toy" 3
  [ spanning "Code" (0, 7) (0, 4) (Pinned 5), spanning "Version" (0, 3) (0, 2) (Pinned 1)
  , spanning "Reserved" (0, 1) (0, 1) Reserved, spanning "Ready" (0, 0) (0, 0) Flag
  , spanning "Length" (1, 7) (1, 0) (Pinned 1)
  , spanning "Busy" (2, 7) (2, 7) Flag, spanning "Count" (2, 6) (2, 4) Unsigned, spanning "Reserved" (2, 3) (2, 0) Reserved ]
  Nothing

instance LayoutRecord Toy where
  recordLayout _ = toyLayout
  recordFields t = [("Ready", flag (toyReady t)), ("Busy", flag (toyBusy t)), ("Count", number (toyCount t))]
  fromFields vs = pure Toy <*> flagField "Ready" vs <*> flagField "Busy" vs <*> numberField "Count" vs

instance Feature Toy where
  featureCode _    = 5
  featureVersion _ = Just 1
  featureLength _  = Just 1
  featureHeader _  = FeatureHeader 2 "Length"

toy :: Proxy Toy
toy = Proxy

-- | A decoded descriptor's record and differences, without the rest.
decoded :: Either LayoutError (DecodedRecord Toy) -> Either LayoutError (Toy, [Difference])
decoded = fmap (\d -> (drRecord d, drDifferences d))

spec :: Spec
spec = describe "a feature descriptor's record" $ do
  it "has its header's constants, and its layout tiles" $ do
    (featureCode toy, featureVersion toy, featureLength toy, featureHeader toy) `shouldBe` (5, Just 1, Just 1, FeatureHeader 2 "Length")
    tiling (recordLayout toy) `shouldBe` []
  it "encodes a record with its pins and zero reserved bits" $
    encodeRecord (Toy True False 5) `shouldBe` Right "\x55\x01\x50"
  prop "every record decodes back to itself, with nothing differing and the rest kept" $ \ready busy (n :: Word8) ->
    let t = Toy ready busy (n `mod` 8)
    in (encodeRecord t >>= decodeFeatureStrict . (<> "rest")) `shouldBe` Right (DecodedRecord t [] "rest")
  it "reports a pin read with another value at its first bit, and refuses it strictly" $ do
    let bytes = "\x95\x01\x50"   -- code 9
    decoded (decodeFeature bytes) `shouldBe` Right (Toy True False 5, [PinDiffers "Code" 0 5 9])
    decoded (decodeFeatureStrict bytes) `shouldBe` Left (PinViolated "Code" 5 9)
  it "reports reserved bits read other than zero" $
    fmap snd (decoded (decodeFeature "\x55\x01\x5f")) `shouldBe` Right [ReservedNotZero "Reserved" 20 15]
  it "starts the rest where its length field says the descriptor ends, a length other than its pin reported" $ do
    let longer = "\x55\x02\x50\xaa" <> "next"   -- Length 2: one byte more than the layout's
    fmap drRest (decodeFeature longer :: Either LayoutError (DecodedRecord Toy)) `shouldBe` Right "next"
    fmap snd (decoded (decodeFeature longer)) `shouldBe` Right [PinDiffers "Length" 8 1 2]
    decoded (decodeFeatureStrict longer) `shouldBe` Left (PinViolated "Length" 1 2)
  it "refuses a length field giving more bytes than the input holds, or fewer than the layout's, saying where that one ends" $ do
    decoded (decodeFeature "\x55\x09\x50") `shouldBe` Left (Truncated 11 3)
    decoded (decodeFeature "\x55\x00\x50") `shouldBe` Left (LengthBelowLayout "Length" 2 3)
    decoded (decodeFeature "\x55\x00") `shouldBe` Left (LengthBelowLayout "Length" 2 3)
    decoded (decodeFeature "\x55") `shouldBe` Left (Truncated 2 1)
  it "refuses a count the field cannot hold, and a short input" $ do
    encodeRecord (Toy False False 8) `shouldBe` Left (OutOfRange "Count" 8)
    decoded (decodeFeature (B.take 2 "\x55\x01\x50")) `shouldBe` Left (Truncated 3 2)
  it "maps each bit to its field" $
    [ (pos, fieldName f) | (pos@(2, _), Just f) <- bitMap toyLayout ]
      `shouldBe` [((2, 7), "Busy"), ((2, 6), "Count"), ((2, 5), "Count"), ((2, 4), "Count"), ((2, 3), "Reserved"), ((2, 2), "Reserved"), ((2, 1), "Reserved"), ((2, 0), "Reserved")]
