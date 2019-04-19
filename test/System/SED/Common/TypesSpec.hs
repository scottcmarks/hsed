{-|
Module      : System.SED.Common.TypesSpec
Description : Test specifications for System.SED.Common.Types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Test specifications for System.SED.Common.Types
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.SED.Common.TypesSpec (spec) where

import           Data.Attoparsec.ByteString           hiding (takeWhile)
import           Data.Bits
import           Data.ByteString
-- import           Data.ByteString.Arbitrary
import           Data.Either.Combinators
import           Data.Ix
import           Data.Word
import           Numeric.Natural
import           RIO

import           System.SED.Common.Import

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                      hiding ((.&.))
import           Test.QuickCheck.Instances.ByteString ()


instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkMapBy abs id shrinkIntegral

-- | fail on 'mempty', succeed on 'singleton' @b0@, test longer with @pred@
validFirstTwo :: ByteString -> (Word8 -> Word8 -> Bool) -> Bool
validFirstTwo bs pred = case uncons bs of
  Nothing -> False
  Just(b0,bs') -> case uncons bs' of
    Nothing    -> True
    Just(b1,_) -> pred b0 (b1 .&. 0x80)

-- | if two or more bytes, the first one should not be a redundant 0x00
validNatural :: ByteString -> Bool
validNatural bs = validFirstTwo bs $ \b0 b1 ->
  case (b0, b1) of
      (0x00,    _) -> False
      (   _,    _) -> True

-- | if two or more bytes, the first one should not be a redundant sign propagation
validInteger :: ByteString -> Bool
validInteger bs = validFirstTwo bs $ \b0 b1 ->
  case (b0, b1) of
      (0x00, 0x00) -> False
      (0xFF, 0x80) -> False
      (   _,    _) -> True


-- | test suite
spec :: Spec
spec = do
  describe "byteStringToNatural" $ do
    prop "is the inverse of naturalToBytestring" $
      (\n -> (byteStringToNatural . naturalToByteString) n `shouldBe` (n :: Natural))
    prop "is inverted by naturalToBytestring when the input is valid" $
      (\bs -> validNatural bs ==>
                (naturalToByteString . byteStringToNatural) bs `shouldBe` (bs :: ByteString))

    -- FIXME: gives up after ~50 tests; 1000 discarded
    -- prop "is not inverted by naturalToBytestring when the input is not valid" $
    --   (\bs -> (not . validNatural) bs ==>
    --             (naturalToByteString . byteStringToNatural) bs `shouldNotBe` (bs :: ByteString))

    it "is not inverted by naturalToBytestring when the input is \"\\x00\\x80\" (not valid)" $
          ( (naturalToByteString . byteStringToNatural) (fromString "\x00\x80")
          `shouldNotBe`
            (fromString "\x00\x80") )
    it "is trimmed by naturalToBytestring when the input is \"\\x00\\x80\" (not valid)" $
          ( (naturalToByteString . byteStringToNatural) (fromString "\x00\x80")
          `shouldBe`
            (fromString "\x80"))

  describe "byteStringToInteger" $ do
    prop "is the inverse of integerToByteString" $
      (\i -> (byteStringToInteger . integerToByteString) i `shouldBe` (i :: Integer))
    prop "is inverted by integerToByteString" $
      (\bs -> validInteger bs ==>
                (integerToByteString . byteStringToInteger) bs `shouldBe` (bs :: ByteString))

  describe "parseToken" $ do
    prop "is the inverse of generateToken for an unsigned byte" $
      (\b -> inRange (0,63) b ==>
          let i = fromIntegral (b::Word8)
           in pg (UnsignedAtom i) `shouldBe` Just (UnsignedAtom i))
    prop "is the inverse of generateToken for a signed byte" $
      (\b -> inRange (-32,31) b ==>
          let i = fromIntegral (b::Int8)
           in pg (SignedAtom i) `shouldBe` Just (SignedAtom i))
    prop "is the inverse of generateToken for a Natural" $
      (\n -> pg (UnsignedAtom n) `shouldBe` Just (UnsignedAtom n))
    prop "is the inverse of generateToken for an Integer" $
      (\i -> pg (SignedAtom i) `shouldBe` Just (SignedAtom i))
    it "is the inverse of generateToken for the Integer 32" $
      pg (SignedAtom 32) `shouldBe` Just (SignedAtom 32)
    it "will fail if not given enough input" $
        parseString "\x91"
      `shouldBe`
        Left "Needed 1 byte for Short Token: not enough input"
    it "does not allow a zero-length Short Unsigned atom" $
        parseString "\x80"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "does not allow a zero-length Short Signed atom" $
        parseString "\x90"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "allows a zero-length Short Bytes atom" $
        parseString "\xA0"
      `shouldBe`
        Right (ByteSequence mempty)
    it "does not allow a zero-length Short Continued Bytes atom" $
        parseString "\xB0"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "does not allow a zero-length Short Continued Bytes atom" $
        parseString "\xB0"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "does not allow a zero-length Medium Unsigned atom" $
        parseString "\xC0\x00"
      `shouldBe`
        Left "Failed reading: Medium Atom with illegal length 0"


  describe "generateToken" $ do
    prop "is the inverse of parseToken for non-negative bytes" $
      (\b -> inRange (0x00,0x7F) b ==>
          let bs = singleton b
           in gp bs `shouldBe` Just bs)


maybeGenerate :: Maybe Token -> Maybe ByteString
maybeGenerate = (maybe Nothing (Just . generateToken))

parseTok :: ByteString -> Either String Token
parseTok = parseOnly parseToken

parseString :: String -> Either String Token
parseString = parseTok . fromString

maybeParse :: ByteString -> Maybe Token
maybeParse = rightToMaybe . parseTok

gp :: ByteString -> Maybe ByteString
gp = maybeGenerate . maybeParse

pg :: Token -> Maybe Token
pg = maybeParse . generateToken
