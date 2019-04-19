{-|
Module      : System.SED.Common.TokenSpec
Description : Test specifications for System.SED.Common.Types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Test specifications for System.SED.Common.Token
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.SED.Common.TokenSpec (spec) where


import           Data.ByteString
import           Data.Either
import           Data.Ix
import           Data.Word
import           RIO                                  hiding (null)

import           System.SED.Common.Import             hiding (null)
import           System.SED.Common.StreamItem
import           System.SED.Common.Token              ()

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                      hiding (generate, (.&.))
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Instances.Natural    ()
import           Text.Printf

-- | test suite
spec :: Spec
spec = do
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

    it "is the inverse of generateToken for EmptyAtom" $
      pg EmptyAtom `shouldBe` Just EmptyAtom


    -- * Length checks
    it "will fail if not given enough input" $
        parseTokenString "\x91"
      `shouldBe`
        Left "Needed 1 byte for Short Token: not enough input"

    it "does not allow a zero-length Short Unsigned Token" $
        parseTokenString "\x80"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "does not allow a zero-length Short Signed Token" $
        parseTokenString "\x90"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "allows a zero-length Short Bytes Token" $
        parseTokenString "\xA0"
      `shouldBe`
        (Right $ Datum $ Final $ Bytes mempty)
    it "does not allow a zero-length Short Continued Bytes Token" $
        parseTokenString "\xB0"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"
    it "does not allow a zero-length Short Continued Bytes Token" $
        parseTokenString "\xB0"
      `shouldBe`
        Left "Failed reading: Short Atom with illegal length 0"

    it "must have one length byte for Medium Unsigned Token" $
        parseTokenString "\xC0"
      `shouldBe`
        Left "Needed 1 byte for Medium Token length: not enough input"
    it "does not allow a zero-length Medium Unsigned Token" $
        parseTokenString "\xC0\x00"
      `shouldBe`
        Left "Failed reading: Medium Atom with illegal length 0"
    it "must have one length byte for Medium Signed Token" $
        parseTokenString "\xC8"
      `shouldBe`
        Left "Needed 1 byte for Medium Token length: not enough input"
    it "does not allow a zero-length Medium Signed Token" $
        parseTokenString "\xC8\x00"
      `shouldBe`
        Left "Failed reading: Medium Atom with illegal length 0"
    it "must have one length byte for Medium Byte Sequence Token" $
        parseTokenString "\xD0"
      `shouldBe`
        Left "Needed 1 byte for Medium Token length: not enough input"
    it "does not allow a zero-length Medium Byte Sequence Token" $
        parseTokenString "\xD0\x00"
      `shouldBe`
        Left "Failed reading: Medium Atom with illegal length 0"
    it "must have one length byte for Medium Continued Byte Sequence Token" $
        parseTokenString "\xD8"
      `shouldBe`
        Left "Needed 1 byte for Medium Token length: not enough input"
    it "does not allow a zero-length Medium Continued Byte Sequence Token" $
        parseTokenString "\xD8\x00"
      `shouldBe`
        Left "Failed reading: Medium Atom with illegal length 0"

    it "must have three length bytes for Long Unsigned Token" $
        parseTokenString "\xE0"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Unsigned Token" $
        parseTokenString "\xE0\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Unsigned Token" $
        parseTokenString "\xE0\x00\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "does not allow a zero-length Long Unsigned Token" $
        parseTokenString "\xE0\x00\x00\x00"
      `shouldBe`
        Left "Failed reading: Long Atom with illegal length 0"

    it "must have three length bytes for Long Signed Token" $
        parseTokenString "\xE1"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Signed Token" $
        parseTokenString "\xE1\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Signed Token" $
        parseTokenString "\xE1\x00\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "does not allow a zero-length Long Signed Token" $
        parseTokenString "\xE1\x00\x00\x00"
      `shouldBe`
        Left "Failed reading: Long Atom with illegal length 0"

    it "must have three length bytes for Long Byte Sequence Token" $
        parseTokenString "\xE2"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Byte Sequence Token" $
        parseTokenString "\xE2\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Byte Sequence Token" $
        parseTokenString "\xE2\x00\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "does not allow a zero-length Long Byte Sequence Token" $
        parseTokenString "\xE2\x00\x00\x00"
      `shouldBe`
        Left "Failed reading: Long Atom with illegal length 0"

    it "must have three length bytes for Long Continued Byte Sequence Token" $
        parseTokenString "\xE3"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Continued Byte Sequence Token" $
        parseTokenString "\xE3\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "must have three length bytes for Long Continued Byte Sequence Token" $
        parseTokenString "\xE3\x00\x00"
      `shouldBe`
        Left "Needed 3 bytes for Long Token length: not enough input"
    it "does not allow a zero-length Long Continued Byte Sequence Token" $
        parseTokenString "\xE3\x00\x00\x00"
      `shouldBe`
        Left "Failed reading: Long Atom with illegal length 0"

    it "fails on TCG Reserved tags E4-EF,FD,FE" $
      forAll tagsReservedForTCG $
          (\b -> parseTokenByteString (singleton b)
        `shouldBe`
          Left (printf "Failed reading: TCG Reserved Token Type 0x%02X" b))

  describe "generateToken" $ do
    prop "is the inverse of parseToken for non-negative bytes" $
      (\b -> inRange (0x00,0x7F) b ==>
          let bs = singleton b
           in gp bs `shouldBe` Just bs)


tagsReservedForTCG :: Gen Word8
tagsReservedForTCG = elements . unpack . fromString $
    "\xE4\xE5\xE6\xE7\xE8\xE9\xEA\xEB\xEC\xED\xEE\xEF\xFD\xFE"

parseTokenByteString :: ByteString -> Either String Token
parseTokenByteString = parseByteString

parseTokenString :: String -> Either String Token
parseTokenString = parseString

gp :: ByteString -> Maybe ByteString
gp = maybeGenerate . (maybeParse :: ByteString -> Maybe Token)
