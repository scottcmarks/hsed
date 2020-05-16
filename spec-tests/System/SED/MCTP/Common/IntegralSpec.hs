{-|
Module      : System.SED.MCTP.Common.IntegralSpec
Description : Test specifications for System.SED.MCTP.Common.Types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Test specifications for System.SED.MCTP.Common.Integral
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.SED.MCTP.Common.IntegralSpec (spec) where

import           Data.Bits
import           Data.ByteString
import           Data.Word
import           Numeric.Natural
import           RIO                                  hiding (null)

import           System.SED.MCTP.Common.Import             hiding (null)

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                      hiding ((.&.))
import           Test.QuickCheck.Instances.ByteString ()


-- | fail on 'mempty', succeed on 'singleton' @b0@, test longer with @pred@
validFirstTwo :: ByteString -> ((Word8, Word8) -> Bool) -> Bool
validFirstTwo bs pred = case uncons bs of
  Nothing -> False
  Just(b0,bs') -> case uncons bs' of
    Nothing    -> True
    Just(b1,_) -> pred (b0, (b1 .&. 0x80))

-- | if two or more bytes, the first one should not be a redundant 0x00
validNatural :: ByteString -> Bool
validNatural bs = validFirstTwo bs $ \case
      (0x00,    _) -> False
      (   _,    _) -> True

-- | if two or more bytes, the first one should not be a redundant sign propagation
validInteger :: ByteString -> Bool
validInteger bs = validFirstTwo bs $ \case
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
