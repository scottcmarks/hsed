{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Core.RecordSpec
Description : Test specifications for Hsed.Core.Record
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A record over a layout with every kind of field and a tail whose length a
field gives, written as the @byteGrid@ rule writes its instances: decoding
what was encoded gives the record back with nothing after it; the length
field is the tail's length and the tail is padded; reserved bits and pins are
the layout's, not the record's. The spec's own records are generated
(@Hsed.Gen.\<Spec\>.Layouts@) and tested in hsed.
-}
module Hsed.Core.RecordSpec (spec) where

import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Hsed.Core.Layout
import           Hsed.Core.Record


-- | The fixed part of a framed message, as a generated header record is.
data FrameHeader = FrameHeader
  { fhKind   :: !Word8
  , fhUrgent :: !Bool
  , fhId     :: !Word32
  , fhTag    :: !ByteString   -- ^ 9 bytes: a byte string, wider than any word
  }
  deriving (Eq, Show)

-- | The whole: the header and its payload.
data Frame = Frame
  { frameHeader  :: !FrameHeader
  , framePayload :: !ByteString
  }
  deriving (Eq, Show)

frameLayout :: Layout
frameLayout = Layout
  { layoutName   = "Frame Format"
  , layoutSize   = 17
  , layoutFields =
      [ spanning "Reserved" (0, 7) (0, 5) Reserved
      , spanning "Kind" (0, 4) (0, 1) Unsigned
      , spanning "Urgent" (0, 0) (0, 0) Flag
      , byteRange "Id" 1 4 Unsigned
      , byteRange "Version" 5 5 (Pinned 1)
      , byteRange "Tag" 6 14 Bytes
      , byteRange "Length" 15 16 Unsigned ]
  , layoutTail   = Just (Tail "Payload" (Just "Length") 4 Bytes) }

instance LayoutRecord Frame where
  recordLayout _ = frameLayout
  recordFields r =
    [ ("Kind", number (fhKind (frameHeader r)))
    , ("Urgent", flag (fhUrgent (frameHeader r)))
    , ("Id", number (fhId (frameHeader r)))
    , ("Tag", octets (fhTag (frameHeader r)))
    , ("Payload", octets (framePayload r)) ]
  fromFields vs = pure Frame
    <*> (pure FrameHeader <*> numberField "Kind" vs <*> flagField "Urgent" vs <*> numberField "Id" vs <*> octetsField "Tag" vs)
    <*> octetsField "Payload" vs

instance Arbitrary Frame where
  arbitrary = do
    header <- FrameHeader <$> chooseEnum (0, 15) <*> arbitrary <*> arbitrary <*> (B.pack <$> vector 9)
    Frame header . B.pack <$> (chooseInt (0, 13) >>= vector)

-- | A frame's bytes; a test failure if it does not encode.
encoding :: Frame -> IO ByteString
encoding = either (throwString . show) pure . encodeRecord

spec :: Spec
spec = describe "Hsed.Core.Record" $ do
  prop "decodes what it encoded, the length computed and the payload padded to 4" $ \frame -> do
    encoded <- encoding frame
    let n = B.length (framePayload frame)
    B.length encoded `shouldBe` 17 + n + (negate n `mod` 4)
    decodeRecord encoded `shouldBe` Right (DecodedRecord frame [] "")
    (L.lookup "Length" . decodedFields <$> decode frameLayout encoded) `shouldBe` Right (Just (Number (toInteger n)))
  prop "keeps what follows the record" $ \frame rest -> do
    encoded <- encoding frame
    drRest <$> (decodeRecord (encoded <> B.pack rest) :: Either LayoutError (DecodedRecord Frame)) `shouldBe` Right (B.pack rest)
  it "writes the pin and the reserved bits, and reads them back as differences or refuses the pin" $ do
    encoded <- encoding (Frame (FrameHeader 3 True 0x01020304 (B.replicate 9 0xAA)) "hi")
    -- reserved bits 7-5 of byte 0 set, the pinned byte 5 changed
    let altered = "\xE7" <> B.take 4 (B.drop 1 encoded) <> "\x02" <> B.drop 6 encoded
    B.take 6 encoded `shouldBe` "\x07\x01\x02\x03\x04\x01"
    (drDifferences <$> (decodeRecord altered :: Either LayoutError (DecodedRecord Frame)))
      `shouldBe` Right [ReservedNotZero "Reserved" 0 7, PinDiffers "Version" 40 1 2]
    (drRecord <$> (decodeRecordStrict altered :: Either LayoutError (DecodedRecord Frame))) `shouldBe` Left (PinViolated "Version" 1 2)
  it "refuses a value the field's width cannot hold, and a byte string of the wrong length" $ do
    encodeRecord (Frame (FrameHeader 16 False 0 (B.replicate 9 0)) "") `shouldBe` Left (OutOfRange "Kind" 16)
    encodeRecord (Frame (FrameHeader 1 False 0 "short") "") `shouldBe` Left (WrongLength "Tag" 9 5)
  it "names the field a decoded layout lacks, or holds as the other kind" $ do
    (fromFields [("Kind", Number 1)] :: Either LayoutError Frame) `shouldBe` Left (MissingField "Urgent")
    (numberField "Tag" [("Tag", Octets "x")] :: Either LayoutError Word8) `shouldBe` Left (WrongContent "Tag")
    octetsField "Kind" [("Kind", Number 1)] `shouldBe` Left (WrongContent "Kind")
