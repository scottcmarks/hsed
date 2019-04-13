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

validFirstTwo :: ByteString -> (Word8 -> Word8 -> Bool) -> Bool
validFirstTwo bs pred = case uncons bs of
  Nothing -> False
  Just(b0,bs') -> case uncons bs' of
    Nothing    -> True
    Just(b1,_) -> pred b0 (b1 .&. 0x80)

validNatural :: ByteString -> Bool
validNatural bs = validFirstTwo bs $ \b0 b1 ->
  case (b0, b1) of
      (0x00, 0x00) -> False
      (   _,    _) -> True

validInteger :: ByteString -> Bool
validInteger bs = validFirstTwo bs $ \b0 b1 ->
  case (b0, b1) of
      (0x00, 0x00) -> False
      (0xFF, 0x80) -> False
      (   _,    _) -> True



spec :: Spec
spec = do
  describe "byteStringToNatural" $ do
    prop "is the inverse of naturalToBytestring" $
      (\n -> (byteStringToNatural . naturalToByteString) n `shouldBe` (n :: Natural))
    prop "is inverted by naturalToBytestring" $
      (\bs -> validNatural bs ==>
                (naturalToByteString . byteStringToNatural) bs `shouldBe` (bs :: ByteString))

  describe "byteStringToInteger" $ do
    prop "is the inverse of integerToByteString" $
      (\i -> (byteStringToInteger . integerToByteString) i `shouldBe` (i :: Integer))
    prop "is inverted by integerToByteString" $
      (\bs -> validInteger bs ==>
                (integerToByteString . byteStringToInteger) bs `shouldBe` (bs :: ByteString))

  describe "parseToken" $ do
    prop "is the inverse of generateToken for unsigned bytes" $
      (\b -> inRange (0,63) b ==>
          let i = fromIntegral (b::Word8)
           in pg (UnsignedAtom i) `shouldBe` Just (UnsignedAtom i))
    prop "is the inverse of generateToken for signed bytes" $
      (\b -> inRange (-32,31) b ==>
          let i = fromIntegral (b::Int8)
           in pg (SignedAtom i) `shouldBe` Just (SignedAtom i))


  describe "generateToken" $ do
    prop "is the inverse of parseToken for non-negative bytes" $
      (\b -> inRange (0x00,0x7F) b ==>
          let bs = singleton b
           in gp bs `shouldBe` Just bs)


maybeGenerate :: Maybe Token -> Maybe ByteString
maybeGenerate = (maybe Nothing (Just . generateToken))

maybeParse :: ByteString -> Maybe Token
maybeParse = rightToMaybe . (parseOnly parseToken)

gp :: ByteString -> Maybe ByteString
gp = maybeGenerate . maybeParse

pg :: Token -> Maybe Token
pg = maybeParse . generateToken
