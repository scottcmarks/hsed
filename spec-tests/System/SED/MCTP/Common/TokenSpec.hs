{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : System.SED.MCTP.Common.TokenSpec
Description : Test specifications for System.SED.MCTP.Common.Types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Test specifications for System.SED.MCTP.Common.Token
-}

module System.SED.MCTP.Common.TokenSpec (spec) where


import           Data.Bits                            ((.&.))
import           Data.ByteString
import           Data.Either
import           Data.Ix
import           Data.Word
import           RIO                                  hiding (null)

import           System.SED.MCTP.Common.Import        hiding (null)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                      hiding (generate, (.&.))
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Instances.Natural    ()
import           Text.Printf

-- | test suite
spec :: Spec
spec = do
  describe "tokenParser" $ do
    prop "is the inverse of generateToken for an unsigned byte" $
      (\b -> inRange (0,63) b ==>
          let i = fromIntegral (b::Word8)
           in pg (Unsigned i) `shouldBe` Just (Unsigned i))

    prop "is the inverse of generateToken for a signed byte" $
      (\b -> inRange (-32,31) b ==>
          let i = fromIntegral (b::Int8)
           in pg (Signed i) `shouldBe` Just (Signed i))

    prop "is the inverse of generateToken for a Natural" $
      (\n -> pg (Unsigned n) `shouldBe` Just (Unsigned n))

    prop "is the inverse of generateToken for an Integer" $
      (\i -> pg (Signed i) `shouldBe` Just (Signed i))

    it "is the inverse of generateToken for the Integer 32" $
      pg (Signed 32) `shouldBe` Just (Signed 32)

    prop "is the inverse of generateToken for a ByteString" $
      (\bs -> pg (Bytes bs) `shouldBe` Just (Bytes bs))

    prop "is the inverse of generateToken for a continued ByteString" $
      (\bs -> (not . null) bs ==>
        pg (ContinuedBytes bs) `shouldBe` Just (ContinuedBytes bs))

    it "is the inverse of generateToken for StartList" $
      pg StartList `shouldBe` Just StartList

    it "is the inverse of generateToken for EndList" $
      pg EndList `shouldBe` Just EndList

    it "is the inverse of generateToken for StartName" $
      pg StartName `shouldBe` Just StartName

    it "is the inverse of generateToken for EndName" $
      pg EndName `shouldBe` Just EndName

    it "is the inverse of generateToken for Call" $
      pg Call `shouldBe` Just Call

    it "is the inverse of generateToken for EndOfData" $
      pg EndOfData `shouldBe` Just EndOfData

    it "is the inverse of generateToken for EndOfSession" $
      pg EndOfSession `shouldBe` Just EndOfSession

    it "is the inverse of generateToken for StartTransaction" $
      pg StartTransaction `shouldBe` Just StartTransaction

    it "is the inverse of generateToken for EndTransaction" $
      pg EndTransaction `shouldBe` Just EndTransaction

    it "is the inverse of generateToken for Empty" $
      pg Empty `shouldBe` Just Empty


    -- * Length checks
    it "will fail if not given enough input" $
        tokenParserString "\x91"
      `shouldBe`
        Left "Needed 1 byte for Short Token: not enough input"

    it "does not allow a zero-length Short Unsigned Token" $
        tokenParserString "\x80"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "does not allow a zero-length Short Signed Token" $
        tokenParserString "\x90"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "allows a zero-length Short Bytes Token" $
        tokenParserString "\xA0"
      `shouldBe`
        (Right $ Bytes mempty)
    it "does not allow a zero-length Short Continued Bytes Token" $
        tokenParserString "\xB0"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "does not allow a zero-length Short Continued Bytes Token" $
        tokenParserString "\xB0"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"

    it "must have one length byte for Medium Unsigned Token" $
        tokenParserString "\xC0"
      `shouldBe`
        Left "Needed 1 byte for Medium Token length: not enough input"
    it "does not allow a zero-length Medium Unsigned Token" $
        tokenParserString "\xC0\x00"
      `shouldBe`
        Left "Failed reading: Medium Atom with illegal length 0"
    it "must have one length byte for Medium Signed Token" $
        tokenParserString "\xC8"
      `shouldBe`
        Left "Needed 1 byte for Medium Token length: not enough input"
    it "does not allow a zero-length Medium Signed Token" $
        tokenParserString "\xC8\x00"
      `shouldBe`
        Left "Failed reading: Medium Atom with illegal length 0"
    it "must have one length byte for Medium Byte Sequence Token" $
        tokenParserString "\xD0"
      `shouldBe`
        Left "Needed 1 byte for Medium Token length: not enough input"
    it "does not allow a zero-length Medium Byte Sequence Token" $
        tokenParserString "\xD0\x00"
      `shouldBe`
        Left "Failed reading: Medium Atom with illegal length 0"
    it "must have one length byte for Medium Continued Byte Sequence Token" $
        tokenParserString "\xD8"
      `shouldBe`
        Left "Needed 1 byte for Medium Token length: not enough input"
    it "does not allow a zero-length Medium Continued Byte Sequence Token" $
        tokenParserString "\xD8\x00"
      `shouldBe`
        Left "Failed reading: Medium Atom with illegal length 0"

    it "must have three length bytes for Long Unsigned Token" $
        tokenParserString "\xE0"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Unsigned Token" $
        tokenParserString "\xE0\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Unsigned Token" $
        tokenParserString "\xE0\x00\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "does not allow a zero-length Long Unsigned Token" $
        tokenParserString "\xE0\x00\x00\x00"
      `shouldBe`
        Left "Failed reading: Long Atom with illegal length 0"

    it "must have three length bytes for Long Signed Token" $
        tokenParserString "\xE1"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Signed Token" $
        tokenParserString "\xE1\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Signed Token" $
        tokenParserString "\xE1\x00\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "does not allow a zero-length Long Signed Token" $
        tokenParserString "\xE1\x00\x00\x00"
      `shouldBe`
        Left "Failed reading: Long Atom with illegal length 0"

    it "must have three length bytes for Long Byte Sequence Token" $
        tokenParserString "\xE2"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Byte Sequence Token" $
        tokenParserString "\xE2\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Byte Sequence Token" $
        tokenParserString "\xE2\x00\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "does not allow a zero-length Long Byte Sequence Token" $
        tokenParserString "\xE2\x00\x00\x00"
      `shouldBe`
        Left "Failed reading: Long Atom with illegal length 0"

    it "must have three length bytes for Long Continued Byte Sequence Token" $
        tokenParserString "\xE3"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Continued Byte Sequence Token" $
        tokenParserString "\xE3\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Continued Byte Sequence Token" $
        tokenParserString "\xE3\x00\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "does not allow a zero-length Long Continued Byte Sequence Token" $
        tokenParserString "\xE3\x00\x00\x00"
      `shouldBe`
        Left "Failed reading: Long Atom with illegal length 0"

    it "fails on TCG Reserved tags E4-EF,FD,FE" $
      forAll tagsReservedForTCG $
          (\b -> tokenParserByteString (singleton b)
        `shouldBe`
          Left (printf "Failed reading: TCG Reserved Token Type 0x%02X" b))

  describe "generateToken" $ do
    prop "is the inverse of tokenParser for non-negative bytes" $
      (\b -> inRange (0x00,0x7F) b ==>
          let bs = singleton b
           in gp bs `shouldBe` Just bs)
    prop "will match the consumed input of successful parsing, or at least parse the same" $
      withMaxSuccess 1000 $  -- some low-frequency strings found bugs
        (\bs -> gp bs `shouldSatisfy` (maybeIsPrefixOf bs <||> maybeParsesEq bs))

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




maybeIsPrefixOf :: ByteString -> Maybe ByteString -> Bool
maybeIsPrefixOf bs = maybe True (`isPrefixOf` bs)

maybeParsesEq :: ByteString -> Maybe ByteString -> Bool
maybeParsesEq bs = maybe True ((p bs ==) . p)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)
infixr 2 <||>

tagsReservedForTCG :: Gen Word8
tagsReservedForTCG = elements . unpack . fromString $
    "\xE4\xE5\xE6\xE7\xE8\xE9\xEA\xEB\xEC\xED\xEE\xEF\xFD\xFE"

tokenParserByteString :: ByteString -> Either String Token
tokenParserByteString = parseByteString

tokenParserString :: String -> Either String Token
tokenParserString = parseString

p :: ByteString -> Maybe Token
p = maybeParse

gp :: ByteString -> Maybe ByteString
gp = (generate <$>) . p

-- | Cute, but ultimately unneeded
_pgp :: ByteString -> Maybe Token
_pgp = p >=> pg


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
