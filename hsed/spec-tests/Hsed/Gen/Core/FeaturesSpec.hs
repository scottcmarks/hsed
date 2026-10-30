{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Gen.Core.FeaturesSpec
Description : The generated feature descriptors against sedutil's bit maps, and their round trips
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design docs/spec-as-source.md example E8 and section 11 M2c's acceptance: the
layouts @hsedc compile@ generated from Core Tables 42 and 43
("Hsed.Gen.Core.Features") give every bit of bytes 0-4 the field sedutil
gives it in @Common/DtaStructures.h@ (@~/preboot/DTA/sedutil@, read only),
@Discovery0TPerFeatures@ (lines 81-108) and @Discovery0LockingFeatures@ (lines
114-136), pinned here as sedutil prints them.

sedutil declares the flags of byte 4 as one-bit bitfields, bit 0 first: on the
little-endian targets it builds for, the first bitfield of a byte is its least
significant bit, so the declaration order (lines 96-103 and 129-136) is bit 0
to bit 7, the reverse of the big-endian order its comments show (lines 86-95
and 119-128) and of the grid, which prints bit 7 leftmost. Byte 2 declares
@reserved_v : 4@ then @version : 4@: the reserved nibble is bits 3-0, the
version bits 7-4. @featureCode@ is a @uint16_t@ that sedutil byte-swaps
(@SWAP16@, ParseDiscovery0Features.cpp:54): big-endian on the wire, bytes 0-1;
@length@ is byte 3. The three @uint32_t@ reserved words after byte 4 lie at
sedutil's struct alignment, not at the wire's bytes 5-15, and are not compared.

One difference is the spec's: Core 2.01 reserves bit 6 of the Locking
descriptor's byte 4 (its grid prints @Reserved@ over bits 7-6), where sedutil's
declaration has @MBRshadowingNotSupported@, a flag Opal 2 defines (its
big-endian comment still has @reserved02@ there, as Core does).

Then every TPer and Locking record (each of the 64 settings of its six flags)
encodes to sixteen bytes with the code, version and length the spec pins
(0x0001 or 0x0002, 1, 12; PDF 70 and 71) and zero reserved bits, and decodes
back to itself; and a descriptor as a drive would send it reads as sedutil
reads it.
-}
module Hsed.Gen.Core.FeaturesSpec (spec) where

import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import           Test.Hspec

import qualified RIO.ByteString         as B
import           RIO                    (Text)

import           Hsed.Core.Feature
import           Hsed.Core.Layout
import           Hsed.Gen.Core.Features (LockingFeature (..), TPerFeature (..))


-- | A struct's bits as sedutil declares them, each with the field of the
--   spec's grid it is: the bitfields of one byte in declaration order, bit 0
--   first, and the multi-byte fields by their bytes.
data Sedutil = Sedutil
  { sedutilWide  :: [(String, [(Int, Int)], Text)]  -- ^ a field of whole bytes or nibbles: its (byte, bit)s, its spec name
  , sedutilByte4 :: [(String, Text)]                 -- ^ byte 4's bitfields, bit 0 first, each with its spec name
  }

-- | DtaStructures.h:81-108.
sedutilTPer :: Sedutil
sedutilTPer = Sedutil
  { sedutilWide =
      [ ("featureCode", [ (b, i) | b <- [0, 1], i <- [7, 6 .. 0] ], "Feature Code")   -- line 82, uint16_t, SWAP16
      , ("reserved_v", [ (2, i) | i <- [3, 2, 1, 0] ], "Reserved")                     -- line 83, the low nibble
      , ("version", [ (2, i) | i <- [7, 6, 5, 4] ], "Version")                         -- line 84, the high nibble
      , ("length", [ (3, i) | i <- [7, 6 .. 0] ], "Length") ]                          -- line 85
  , sedutilByte4 =                                                                     -- lines 96-103
      [ ("sync", "Sync Supported"), ("async", "Async Supported"), ("acknack", "ACK/NAK Supported")
      , ("bufferManagement", "Buffer Mgmt Supported"), ("streaming", "Streaming Supported"), ("reserved02", "Reserved")
      , ("comIDManagement", "ComID Mgmt Supported"), ("reserved01", "Reserved") ] }

-- | DtaStructures.h:114-136, but for bit 6 (see the module header).
sedutilLocking :: Sedutil
sedutilLocking = Sedutil
  { sedutilWide = sedutilWide sedutilTPer                                              -- lines 115-118, as 82-85
  , sedutilByte4 =                                                                     -- lines 129-136
      [ ("lockingSupported", "Locking Supported"), ("lockingEnabled", "Locking Enabled"), ("locked", "Locked")
      , ("mediaEncryption", "Media Encryption"), ("MBREnabled", "MBR Enabled"), ("MBRDone", "MBR Done")
      , ("MBRshadowingNotSupported", "Reserved"), ("reserved02", "Reserved") ] }

-- | sedutil's big-endian comments, bit 7 first: TPer lines 87-94, Locking 120-127.
sedutilComment :: [(String, [String])]
sedutilComment =
  [ ("TPer", ["reserved01", "comIDManagement", "reserved02", "streaming", "bufferManagement", "acknack", "async", "sync"])
  , ("Locking", ["reserved01", "reserved02", "MBRDone", "MBREnabled", "mediaEncryption", "locked", "lockingEnabled", "lockingSupported"]) ]

-- | sedutil's map of bytes 0-4, (byte, bit) to the spec's name of the field there.
sedutilMap :: Sedutil -> Map.Map (Int, Int) Text
sedutilMap s = Map.fromList ([ (pos, name) | (_, ps, name) <- sedutilWide s, pos <- ps ]
                             <> [ ((4, bit), name) | (bit, (_, name)) <- zip [0 ..] (sedutilByte4 s) ])

-- | The generated layout's map of bytes 0-4.
generatedMap :: Layout -> Map.Map (Int, Int) Text
generatedMap l = Map.fromList [ (pos, fieldName f) | (pos@(byte, _), Just f) <- bitMap l, byte <= 4 ]

-- | Every setting of a layout's flags, by name.
settings :: Layout -> [[(Text, Content)]]
settings l = mapM (\n -> [(n, Number 0), (n, Number 1)]) [ fieldName f | f <- layoutFields l, fieldKind f == Flag ]

spec :: Spec
spec = describe "the generated feature descriptors" $ do
  describe "the bit maps of bytes 0-4, as sedutil's" $ do
    it "TPer Feature Descriptor (Table 42) equals Discovery0TPerFeatures (DtaStructures.h:81-108)" $
      generatedMap (featureLayout @TPerFeature) `shouldBe` sedutilMap sedutilTPer
    it "Locking Feature Descriptor (Table 43) equals Discovery0LockingFeatures (DtaStructures.h:114-136), bit 6 reserved in Core" $ do
      generatedMap (featureLayout @LockingFeature) `shouldBe` sedutilMap sedutilLocking
      lookup "MBRshadowingNotSupported" [ (n, bit) | (bit, (n, _)) <- zip [0 :: Int ..] (sedutilByte4 sedutilLocking) ] `shouldBe` Just 6
    it "sedutil's declarations, bit 0 first, are its big-endian comments reversed (but Locking's bit 6)" $ do
      lookup "TPer" sedutilComment `shouldBe` Just (map fst (reverse (sedutilByte4 sedutilTPer)))
      fmap (zipWith (==) (map fst (reverse (sedutilByte4 sedutilLocking)))) (lookup "Locking" sedutilComment)
        `shouldBe` Just [False, False, True, True, True, True, True, True]
    it "the flags are the records' fields, at sedutil's bits" $ do
      [ (fieldName f, bitPosition (fieldStart f)) | f <- layoutFields (featureLayout @TPerFeature), fieldKind f == Flag ]
        `shouldBe` [ (n, (4, b)) | (b, (_, n)) <- reverse (zip [0 ..] (sedutilByte4 sedutilTPer)), n /= "Reserved" ]
      [ (fieldName f, bitPosition (fieldStart f)) | f <- layoutFields (featureLayout @LockingFeature), fieldKind f == Flag ]
        `shouldBe` [ (n, (4, b)) | (b, (_, n)) <- reverse (zip [0 ..] (sedutilByte4 sedutilLocking)), n /= "Reserved" ]

  describe "the header" $
    it "pins the codes sedutil switches on (FC_TPER, FC_LOCKING: DtaStructures.h:49-50), version 1 and length 12 (PDF 70, 71)" $ do
      (featureCode @TPerFeature, featureVersion @TPerFeature, featureLength @TPerFeature) `shouldBe` (0x0001, Just 1, Just 12)
      (featureCode @LockingFeature, featureVersion @LockingFeature, featureLength @LockingFeature) `shouldBe` (0x0002, Just 1, Just 12)
      map tiling [featureLayout @TPerFeature, featureLayout @LockingFeature] `shouldBe` [[], []]

  describe "encoding and decoding" $ do
    it "every TPer record encodes to its sixteen bytes and decodes back to itself" $ do
      let records = mapMaybe (either (const Nothing) Just . featureFromValues @TPerFeature) (settings (featureLayout @TPerFeature))
      length records `shouldBe` 64
      mapM_ (\r -> (encodeFeature r >>= decodeFeatureStrict) `shouldBe` Right (DecodedFeature r [] B.empty)) records
      mapM_ (\r -> fmap (B.take 4) (encodeFeature r) `shouldBe` Right (B.pack [0x00, 0x01, 0x10, 0x0C])) records
    it "every Locking record encodes to its sixteen bytes and decodes back to itself" $ do
      let records = mapMaybe (either (const Nothing) Just . featureFromValues @LockingFeature) (settings (featureLayout @LockingFeature))
      length records `shouldBe` 64
      mapM_ (\r -> (encodeFeature r >>= decodeFeatureStrict) `shouldBe` Right (DecodedFeature r [] B.empty)) records
      mapM_ (\r -> fmap B.length (encodeFeature r) `shouldBe` Right 16) records
    it "reads a TPer descriptor as sedutil does: byte 4 0x11 is Streaming and Sync" $ do
      let bytes = B.pack ([0x00, 0x01, 0x10, 0x0C, 0x11] <> replicate 11 0)
      fmap decodedFeature (decodeFeatureStrict @TPerFeature bytes) `shouldBe`
        Right (TPerFeature { tPerFeatureComIDMgmtSupported = False, tPerFeatureStreamingSupported = True
                           , tPerFeatureBufferMgmtSupported = False, tPerFeatureACKNAKSupported = False
                           , tPerFeatureAsyncSupported = False, tPerFeatureSyncSupported = True })
    it "reads a Locking descriptor as sedutil does: byte 4 0x07 is Locked, Locking Enabled and Locking Supported" $ do
      let bytes = B.pack ([0x00, 0x02, 0x10, 0x0C, 0x07] <> replicate 11 0)
      fmap decodedFeature (decodeFeatureStrict @LockingFeature bytes) `shouldBe`
        Right (LockingFeature { lockingFeatureMBRDone = False, lockingFeatureMBREnabled = False
                              , lockingFeatureMediaEncryption = False, lockingFeatureLocked = True
                              , lockingFeatureLockingEnabled = True, lockingFeatureLockingSupported = True })
    it "reports Opal's MBR Shadowing Not Supported bit as reserved bits set, and refuses another feature's code strictly" $ do
      let opalShadow = B.pack ([0x00, 0x02, 0x10, 0x0C, 0x40] <> replicate 11 0)
          tperAsLocking = B.pack ([0x00, 0x01, 0x10, 0x0C, 0x00] <> replicate 11 0)
      fmap featureDifferences (decodeFeature @LockingFeature opalShadow) `shouldBe` Right [ReservedNotZero "Reserved" 1]
      fmap featureDifferences (decodeFeature @LockingFeature tperAsLocking) `shouldBe` Right [PinDiffers "Feature Code" 2 1]
      fmap (const ()) (decodeFeatureStrict @LockingFeature tperAsLocking) `shouldBe` Left (PinViolated "Feature Code" 2 1)
