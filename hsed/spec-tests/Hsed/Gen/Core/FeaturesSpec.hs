{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
@length@ is byte 3. sedutil's structs are packed (@#pragma pack(1)@,
DtaStructures.h:25-26, popped at 840): after byte 4 it declares three
@uint32_t@ reserved words, 12 bytes (17 in all), where the PDF has 11 (bytes
5-15, 16 in all), so only bytes 0-4 are compared.

One difference is the spec's: Core 2.01 reserves bit 6 of the Locking
descriptor's byte 4 (its grid prints @Reserved@ over bits 7-6), where sedutil's
declaration has @MBRshadowingNotSupported@, a flag Opal 2 defines (its
big-endian comment still has @reserved02@ there, as Core does).

Then every TPer and Locking record (each of the 64 settings of its six flags)
encodes to sixteen bytes with the code, version and length the spec pins
(0x0001 or 0x0002, 1, 12; PDF 70 and 71) and zero reserved bits, and decodes
back to itself; a descriptor as a drive would send it reads as sedutil reads
it; a descriptor longer than its pin says ends where its Length says (the
general format of Table 41: four bytes and Length more), so the next one is
read from its first byte, and one shorter than its layout is refused with
where it ends; and the descriptors are told apart by their pins.
-}
module Hsed.Gen.Core.FeaturesSpec (spec) where

import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import           Data.Proxy             (Proxy (..))
import           Test.Hspec

import qualified RIO.ByteString         as B
import           RIO                    (Text)

import           Hsed.Core.Feature
import           Hsed.Core.Layout
import           Hsed.Core.Record
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

tper :: Proxy TPerFeature
tper = Proxy

locking :: Proxy LockingFeature
locking = Proxy

-- | Every setting of a layout's flags, by name.
settings :: Layout -> [[(Text, Content)]]
settings l = mapM (\n -> [(n, Number 0), (n, Number 1)]) [ fieldName f | f <- layoutFields l, fieldKind f == Flag ]

spec :: Spec
spec = describe "the generated feature descriptors" $ do
  describe "the bit maps of bytes 0-4, as sedutil's" $ do
    it "TPer Feature Descriptor (Table 42) equals Discovery0TPerFeatures (DtaStructures.h:81-108)" $
      generatedMap (recordLayout tper) `shouldBe` sedutilMap sedutilTPer
    it "Locking Feature Descriptor (Table 43) equals Discovery0LockingFeatures (DtaStructures.h:114-136), bit 6 reserved in Core" $ do
      generatedMap (recordLayout locking) `shouldBe` sedutilMap sedutilLocking
      lookup "MBRshadowingNotSupported" [ (n, bit) | (bit, (n, _)) <- zip [0 :: Int ..] (sedutilByte4 sedutilLocking) ] `shouldBe` Just 6
    it "sedutil's declarations, bit 0 first, are its big-endian comments reversed (but Locking's bit 6)" $ do
      lookup "TPer" sedutilComment `shouldBe` Just (map fst (reverse (sedutilByte4 sedutilTPer)))
      fmap (zipWith (==) (map fst (reverse (sedutilByte4 sedutilLocking)))) (lookup "Locking" sedutilComment)
        `shouldBe` Just [False, False, True, True, True, True, True, True]
    it "the flags are the records' fields, at sedutil's bits" $ do
      [ (fieldName f, bitPosition (fieldStart f)) | f <- layoutFields (recordLayout tper), fieldKind f == Flag ]
        `shouldBe` [ (n, (4, b)) | (b, (_, n)) <- reverse (zip [0 ..] (sedutilByte4 sedutilTPer)), n /= "Reserved" ]
      [ (fieldName f, bitPosition (fieldStart f)) | f <- layoutFields (recordLayout locking), fieldKind f == Flag ]
        `shouldBe` [ (n, (4, b)) | (b, (_, n)) <- reverse (zip [0 ..] (sedutilByte4 sedutilLocking)), n /= "Reserved" ]

  describe "the header" $
    it "pins the codes sedutil switches on (FC_TPER, FC_LOCKING: DtaStructures.h:49-50), version 1 and length 12 (PDF 70, 71)" $ do
      (featureCode tper, featureVersion tper, featureLength tper) `shouldBe` (0x0001, Just 1, Just 12)
      (featureCode locking, featureVersion locking, featureLength locking) `shouldBe` (0x0002, Just 1, Just 12)
      (featureHeader tper, featureHeader locking) `shouldBe` (FeatureHeader 4 "Length", FeatureHeader 4 "Length")
      map tiling [recordLayout tper, recordLayout locking] `shouldBe` [[], []]

  describe "encoding and decoding" $ do
    it "every TPer record encodes to its sixteen bytes and decodes back to itself" $ do
      let records = mapMaybe (either (const Nothing) Just . (fromFields :: [(Text, Content)] -> Either LayoutError TPerFeature)) (settings (recordLayout tper))
      length records `shouldBe` 64
      mapM_ (\r -> (encodeRecord r >>= decodeFeatureStrict) `shouldBe` Right (DecodedRecord r [] B.empty)) records
      mapM_ (\r -> fmap (B.take 4) (encodeRecord r) `shouldBe` Right (B.pack [0x00, 0x01, 0x10, 0x0C])) records
    it "every Locking record encodes to its sixteen bytes and decodes back to itself" $ do
      let records = mapMaybe (either (const Nothing) Just . (fromFields :: [(Text, Content)] -> Either LayoutError LockingFeature)) (settings (recordLayout locking))
      length records `shouldBe` 64
      mapM_ (\r -> (encodeRecord r >>= decodeFeatureStrict) `shouldBe` Right (DecodedRecord r [] B.empty)) records
      mapM_ (\r -> fmap B.length (encodeRecord r) `shouldBe` Right 16) records
    it "reads a TPer descriptor as sedutil does: byte 4 0x11 is Streaming and Sync" $ do
      let bytes = B.pack ([0x00, 0x01, 0x10, 0x0C, 0x11] <> replicate 11 0)
      fmap drRecord (decodeFeatureStrict bytes) `shouldBe`
        Right (TPerFeature { tPerFeatureComIDMgmtSupported = False, tPerFeatureStreamingSupported = True
                           , tPerFeatureBufferMgmtSupported = False, tPerFeatureACKNAKSupported = False
                           , tPerFeatureAsyncSupported = False, tPerFeatureSyncSupported = True })
    it "reads a Locking descriptor as sedutil does: byte 4 0x07 is Locked, Locking Enabled and Locking Supported" $ do
      let bytes = B.pack ([0x00, 0x02, 0x10, 0x0C, 0x07] <> replicate 11 0)
      fmap drRecord (decodeFeatureStrict bytes) `shouldBe`
        Right (LockingFeature { lockingFeatureMBRDone = False, lockingFeatureMBREnabled = False
                              , lockingFeatureMediaEncryption = False, lockingFeatureLocked = True
                              , lockingFeatureLockingEnabled = True, lockingFeatureLockingSupported = True })
    it "reports Opal's MBR Shadowing Not Supported bit as reserved bits set, and refuses another feature's code strictly" $ do
      let opalShadow = B.pack ([0x00, 0x02, 0x10, 0x0C, 0x40] <> replicate 11 0)
          tperAsLocking = B.pack ([0x00, 0x01, 0x10, 0x0C, 0x00] <> replicate 11 0)
      fmap drDifferences (decodeFeature opalShadow :: Either LayoutError (DecodedRecord LockingFeature)) `shouldBe` Right [ReservedNotZero "Reserved" 32 1]
      fmap drDifferences (decodeFeature tperAsLocking :: Either LayoutError (DecodedRecord LockingFeature)) `shouldBe` Right [PinDiffers "Feature Code" 0 2 1]
      fmap (const ()) (decodeFeatureStrict tperAsLocking :: Either LayoutError (DecodedRecord LockingFeature)) `shouldBe` Left (PinViolated "Feature Code" 2 1)
    it "ends a descriptor where its Length says (4 + Length), so a longer one than its pin leaves the next one aligned" $ do
      -- a TPer descriptor of Length 0x10 (20 bytes, a later version's) and a Locking descriptor after it
      let longer = B.pack ([0x00, 0x01, 0x20, 0x10, 0x11] <> replicate 15 0)
          next = B.pack ([0x00, 0x02, 0x10, 0x0C, 0x07] <> replicate 11 0)
      fmap drDifferences (decodeFeature (longer <> next) :: Either LayoutError (DecodedRecord TPerFeature))
        `shouldBe` Right [PinDiffers "Version" 16 1 2, PinDiffers "Length" 24 12 16]
      fmap drRest (decodeFeature (longer <> next) :: Either LayoutError (DecodedRecord TPerFeature)) `shouldBe` Right next
      (decodeFeature (longer <> next) >>= \d -> decodeFeature (drRest (d :: DecodedRecord TPerFeature)))
        `shouldBe` Right (DecodedRecord (LockingFeature False False False True True True) [] B.empty)
    it "says where a descriptor shorter than its layout ends (an older version's, Length 8), so a walk steps past it to the next" $ do
      let shorter = B.pack ([0x00, 0x01, 0x10, 0x08, 0x11] <> replicate 7 0)
          next = B.pack ([0x00, 0x02, 0x10, 0x0C, 0x07] <> replicate 11 0)
      fmap drRecord (decodeFeature (shorter <> next) :: Either LayoutError (DecodedRecord TPerFeature)) `shouldBe` Left (LengthBelowLayout "Length" 12 16)
      fmap drRecord (decodeFeatureStrict (B.drop 12 (shorter <> next))) `shouldBe` Right (LockingFeature False False False True True True)
    it "tells the descriptors apart by their pins (a lenient decode's PinDiffers is no match)" $ do
      let bytes = B.pack ([0x00, 0x02, 0x10, 0x0C, 0x07] <> replicate 11 0)
      map (layoutName . fst) (matching [recordLayout tper, recordLayout locking] bytes) `shouldBe` ["Locking Feature Descriptor"]
