{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Sim.FeatureSetsSpec
Description : Test specifications for the hand-written feature descriptors, verified against their sources, and the simulator's descriptors
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Milestone M3's group G2: the four descriptors no compiled spec gives
("Hsed.Sim.FeatureSets") held to their sources, which these examples pin.

* SIIS 1.20 Table 2 (PDF 21-22) and CNL 1.30 Table 6 (PDF 51), untagged
  PDFs: each field's bits as the column rules of the grid bound its cell
  (byte 2 split at bit 4 in both; SIIS byte 5 at bits 3 and 1; CNL byte 4 at
  bits 7, 6 and 5), and the constants their bullets pin (SIIS PDF 22; CNL
  5.2.1.1-5.2.1.4, PDF 51-52).
* The Additional DataStore Table and Block SID Authentication descriptors,
  whose feature sets' PDFs are not in @tcg/@: bit for bit sedutil's packed
  structures (@~/preboot/DTA/sedutil@ at 136389c, read only,
  @Common/DtaStructures.h@ lines 222-231 and 342-355, @#pragma pack(1)@ at
  25-26), transcribed member by member: a multi-byte integer whole bytes,
  big-endian on the wire (sedutil byte-swaps it, ParseDiscovery0Features.cpp
  109-111), a bitfield the next bits of its byte from bit 0 up (the
  little-endian targets sedutil builds for allocate a byte's first bitfield
  to its least significant bit); and the version decided at M3, 0x1 or any.

Then every record of these and of Opal's generated descriptors encodes and
decodes back to itself, strictly; and the simulator's own descriptors are
read strictly by their types, holding what Opal and the parameters say.
-}
module Hsed.Sim.FeatureSetsSpec (spec) where

import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import qualified RIO.Map                as Map
import           RIO
import qualified RIO.Text               as T
import           Test.Hspec
import           Test.Hspec.QuickCheck  (prop)
import           Test.QuickCheck        (Gen, Property, arbitrary, choose, forAll, (===))

import           Hsed.Core.Feature      (Allowance (..), Feature (..), FeatureHeader (..), bitMap,
                                         decodeFeatureStrict)
import           Hsed.Core.Layout       (Difference, Field (..), FieldKind (..), Layout (..), LayoutError,
                                         bitPosition, fieldEnd, tiling)
import           Hsed.Core.Record       (DecodedRecord (..), LayoutRecord (..), encodeRecord)
import           Hsed.Gen.Opal.Features
import           Hsed.Jig.Support       (isAsPrinted)
import           Hsed.Level0            (Descriptor (..))
import           Hsed.Sim.FeatureSets
import           Hsed.Sim.Level0        (simDescriptors)
import           Hsed.Sim.Types         (LifeCycle (..), SimParams (..), defaultSimParams, initialState)


spec :: Spec
spec = describe "the hand-written feature descriptors" $ do
  describe "SIIS (0x0005), SIIS 1.20 Table 2" $ do
    it "has each field at the bits Table 2's column rules give it (PDF 21-22)" $
      spans siisFeatureLayout `shouldBe`
        [ ("Feature Code", (0, 7), (1, 0)), ("Data Structure Version", (2, 7), (2, 4)), ("Reserved", (2, 3), (2, 0))
        , ("Length", (3, 7), (3, 0)), ("SIIS Revision Number", (4, 7), (4, 0)), ("Reserved", (5, 7), (5, 3))
        , ("Identifier Usage Scope", (5, 2), (5, 1)), ("Key Change Zone Behavior", (5, 0), (5, 0))
        , ("Reserved", (6, 7), (15, 0)) ]
    it "pins what its bullets make exact: code 0x0005, version 0x1, Length 0x0C (PDF 22)" $ do
      pins siisFeatureLayout `shouldBe` [("Feature Code", 0x0005), ("Data Structure Version", 0x1), ("Length", 0x0C)]
      featureVersion (Proxy :: Proxy SiisFeature) `shouldBe` Just 0x1

  describe "Configurable Namespace Locking (0x0403), CNL 1.30 Table 6" $ do
    it "has each field at the bits Table 6's column rules give it (PDF 51)" $
      spans namespaceLockingFeatureLayout `shouldBe`
        [ ("Feature Code", (0, 7), (1, 0)), ("Feature Descriptor Version Number", (2, 7), (2, 4))
        , ("Feature Set Minor Version Number", (2, 3), (2, 0)), ("Length", (3, 7), (3, 0)), ("Range_C", (4, 7), (4, 7))
        , ("Range_P", (4, 6), (4, 6)), ("SUM_C", (4, 5), (4, 5)), ("Reserved", (4, 4), (4, 0)), ("Reserved", (5, 7), (7, 0))
        , ("Maximum Key Count", (8, 7), (11, 0)), ("Unused Key Count", (12, 7), (15, 0))
        , ("Maximum Ranges Per Namespace", (16, 7), (19, 0)) ]
    it "pins code 0x0403, version 0x2 and Length 0x10 (5.2.1.1, 5.2.1.2 and 5.2.1.4, PDF 51-52)" $
      pins namespaceLockingFeatureLayout
        `shouldBe` [("Feature Code", 0x0403), ("Feature Descriptor Version Number", 0x2), ("Length", 0x10)]

  describe "Additional DataStore Table (0x0202) and Block SID Authentication (0x0402), as sedutil's structures" $ do
    it "the DataStore descriptor's bits are Discovery0DatastoreTable's (DtaStructures.h:222-231)" $
      generatedMap dataStoreFeatureLayout `shouldBe` structMap sedutilDataStore
    it "the Block SID descriptor's bits are Discovery0BlockSIDFeatures's (DtaStructures.h:342-355)" $
      generatedMap blockSIDFeatureLayout `shouldBe` structMap sedutilBlockSID
    it "both structures are sixteen bytes, so Length is 0x0C" $ do
      map (Map.size . structMap) [sedutilDataStore, sedutilBlockSID] `shouldBe` [16 * 8, 16 * 8]
      pins dataStoreFeatureLayout `shouldBe` [("Feature Code", 0x0202), ("Length", 0x0C)]
      pins blockSIDFeatureLayout `shouldBe` [("Feature Code", 0x0402), ("Length", 0x0C)]
    it "pin no version, and allow any, the simulator sending 0x1 (decided at M3)" $ do
      (featureVersion (Proxy :: Proxy DataStoreFeature), featureVersion (Proxy :: Proxy BlockSIDFeature))
        `shouldBe` (Nothing, Nothing)
      L.lookup "Version" (featureAllowances (Proxy :: Proxy DataStoreFeature)) `shouldBe` Just (AnyVersion 0x1)
      L.lookup "Version" (featureAllowances (Proxy :: Proxy BlockSIDFeature)) `shouldBe` Just (AnyVersion 0x1)
      [ dVersion d | d <- own, dCode d `elem` [0x0202, 0x0402] ] `shouldBe` [0x1, 0x1]
    it "the DataStore descriptor allows the least UCT-01 accepts (step 4b, Test Cases PDF 30)" $
      drop 1 (featureAllowances (Proxy :: Proxy DataStoreFeature)) `shouldBe`
        [ ("Maximum number of DataStore Tables", Minimum 1), ("Maximum total size of DataStore Tables", Minimum 0xA0000)
        , ("DataStore Table size alignment", Minimum 1) ]

  describe "the Block SID descriptor's flags" $
    it "are each the TPer's present state, as printed, compared unchecked" $
      [ (n, isAsPrinted a) | (n, a) <- featureAllowances (Proxy :: Proxy BlockSIDFeature), n /= "Version" ]
        `shouldBe` [("SID Value State", True), ("SID Blocked State", True), ("Hardware Reset", True)]

  describe "Supported Data Removal Mechanism's allowances (Opal 2.30 Tables 9-12)" $ do
    it "name fields of the generated layout: its times and formats any value, its mechanisms and states as printed" $ do
      filter (`notElem` map fieldName (layoutFields level0DiscoverySupportedDataRemovalMechanismFeatureLayout))
             (map fst dataRemovalAllowances) `shouldBe` []
      length [ () | (_, VendorUnique) <- dataRemovalAllowances ] `shouldBe` 8
      [ n | (n, a) <- dataRemovalAllowances, isAsPrinted a ] `shouldBe`
        ["Data Removal Operation Processing", "Data Removal Operation Interrupted", "Supported Data Removal Mechanism"]
    it "allow its Version nothing: Table 9 prints 0x02 alone" $
      L.lookup "Version" dataRemovalAllowances `shouldBe` Nothing

  describe "Geometry Reporting's allowances, until M5" $
    it "name each of Table 5's value fields, as the LockingInfo table has it (3.1.1.4.2-5, PDF 22)" $ do
      map fst geometryAllowances `shouldBe`
        [ fieldName f | f <- layoutFields level0DiscoveryGeometryReportingFeatureLayout, fieldKind f `elem` [Flag, Unsigned] ]
      [ () | (_, AsPrinted t) <- geometryAllowances, "LockingInfo table" `T.isInfixOf` t ] `shouldBe` [(), (), (), ()]

  describe "each descriptor's instance" $ do
    instanceFacts "SIIS" (Proxy :: Proxy SiisFeature)
    instanceFacts "DataStore" (Proxy :: Proxy DataStoreFeature)
    instanceFacts "Block SID" (Proxy :: Proxy BlockSIDFeature)
    instanceFacts "Configurable Namespace Locking" (Proxy :: Proxy NamespaceLockingFeature)
    instanceFacts "Opal TPer" (Proxy :: Proxy Level0DiscoveryTPerFeature)
    instanceFacts "Opal Locking" (Proxy :: Proxy Level0DiscoveryLockingFeature)
    instanceFacts "Opal Geometry Reporting" (Proxy :: Proxy Level0DiscoveryGeometryReportingFeature)
    instanceFacts "Opal SSC V2" (Proxy :: Proxy Level0DiscoveryOpalSSCV2Feature)
    instanceFacts "Opal Supported Data Removal Mechanism" (Proxy :: Proxy Level0DiscoverySupportedDataRemovalMechanismFeature)

  describe "every record encodes and decodes back to itself, strictly" $ do
    prop "SIIS" (roundTrip (SiisFeature <$> arbitrary <*> choose (0, 3) <*> arbitrary))
    prop "DataStore" (roundTrip (DataStoreFeature <$> nibble <*> arbitrary <*> arbitrary <*> arbitrary))
    prop "Block SID" (roundTrip (BlockSIDFeature <$> nibble <*> arbitrary <*> arbitrary <*> arbitrary))
    prop "Configurable Namespace Locking" $ roundTrip $
      NamespaceLockingFeature <$> nibble <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    prop "Opal TPer" (roundTrip (Level0DiscoveryTPerFeature <$> nibble <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary))
    prop "Opal Locking" $ roundTrip $
      Level0DiscoveryLockingFeature <$> nibble <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    prop "Opal Geometry Reporting" $ roundTrip $
      Level0DiscoveryGeometryReportingFeature <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    prop "Opal SSC V2" $ roundTrip $
      Level0DiscoveryOpalSSCV2Feature <$> nibble <*> nibble <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
    prop "Opal Supported Data Removal Mechanism" $ roundTrip $
      Level0DiscoverySupportedDataRemovalMechanismFeature <$> nibble <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  describe "the simulator's own descriptors" $ do
    it "are read strictly by the types of their codes: no pin read otherwise, no reserved bit set, nothing left" $
      map strictly own `shouldBe` map (const (Just (Right ([], B.empty)))) own
    it "hold Opal's versions, minima and states, and the parameters' values" $ do
      decodedAs (Proxy :: Proxy Level0DiscoveryTPerFeature) `shouldBe`
        Just (Level0DiscoveryTPerFeature 0x1 False False False False)
      decodedAs (Proxy :: Proxy Level0DiscoveryLockingFeature) `shouldBe`
        Just (Level0DiscoveryLockingFeature 0x3 False False False False False)
      decodedAs (Proxy :: Proxy Level0DiscoveryGeometryReportingFeature) `shouldBe`
        Just (Level0DiscoveryGeometryReportingFeature True 512 8 0)
      decodedAs (Proxy :: Proxy SiisFeature) `shouldBe` Just (SiisFeature 0x20 0 False)
      decodedAs (Proxy :: Proxy DataStoreFeature) `shouldBe` Just (DataStoreFeature 0x1 1 0xA00000 1)
      decodedAs (Proxy :: Proxy Level0DiscoveryOpalSSCV2Feature) `shouldBe`
        Just (Level0DiscoveryOpalSSCV2Feature 0x2 0x3 0x1004 1 False 4 8 0 0)
      decodedAs (Proxy :: Proxy BlockSIDFeature) `shouldBe` Just (BlockSIDFeature 0x1 False False False)
      decodedAs (Proxy :: Proxy Level0DiscoverySupportedDataRemovalMechanismFeature) `shouldBe`
        Just (Level0DiscoverySupportedDataRemovalMechanismFeature 0x02 False False 0x04 False False False False 0 0 0 0)
    it "report LockingEnabled once the Locking SP is Manufactured (Opal 3.1.1.3.1, PDF 21)" $ do
      let manufactured = simDescriptors (initialState defaultSimParams { spLockingOFS = Manufactured })
      fmap level0DiscoveryLockingFeatureLockingEnabled (decodedIn manufactured (Proxy :: Proxy Level0DiscoveryLockingFeature))
        `shouldBe` Just True
    it "take the base ComID, the ComIDs and the SSC minor version from the parameters" $ do
      let other = simDescriptors (initialState defaultSimParams { spBaseComId = 0x2000, spNComIds = 3, spSscMinor = 0x1 })
      fmap (\r -> ( level0DiscoveryOpalSSCV2FeatureBaseComID r, level0DiscoveryOpalSSCV2FeatureNumberOfComIDs r
                  , level0DiscoveryOpalSSCV2FeatureSSCMinorVersionNumber r ))
           (decodedIn other (Proxy :: Proxy Level0DiscoveryOpalSSCV2Feature))
        `shouldBe` Just (0x2000, 3, 0x1)
  where
    own = simDescriptors (initialState defaultSimParams)
    decodedAs :: Feature f => Proxy f -> Maybe f
    decodedAs = decodedIn own


-- | A layout's fields, each with its first and last (byte, bit).
spans :: Layout -> [(Text, (Int, Int), (Int, Int))]
spans l = [ (fieldName f, bitPosition (fieldStart f), bitPosition (fieldEnd f - 1)) | f <- layoutFields l ]

-- | A layout's pins, by field.
pins :: Layout -> [(Text, Integer)]
pins l = [ (fieldName f, v) | f <- layoutFields l, Pinned v <- [fieldKind f] ]

-- | A layout's map of (byte, bit) to the field there.
generatedMap :: Layout -> Map.Map (Int, Int) Text
generatedMap l = Map.fromList [ (pos, fieldName f) | (pos, Just f) <- bitMap l ]

-- | A member of one of sedutil's packed structures, as declared: its name,
--   then an unsigned integer of whole bytes or a bitfield of a byte, and the
--   field of hsed's layout it is.
data Member
  = Whole String Int Text
  | Bits String Int Text

-- | Each bit of a packed structure, (byte, bit) with bit 7 the most
--   significant, and the field there: whole integers big-endian on the wire,
--   a byte's bitfields from bit 0 up.
structMap :: [Member] -> Map.Map (Int, Int) Text
structMap = Map.fromList . concat . snd . L.mapAccumL place (0, 0)
  where
    place (byte, _) (Whole _ n field)  = ((byte + n, 0), [ ((b, i), field) | b <- [byte .. byte + n - 1], i <- [7, 6 .. 0] ])
    place (byte, used) (Bits _ w field) =
      ( if used + w >= 8 then (byte + 1, 0) else (byte, used + w)
      , [ ((byte, i), field) | i <- [used .. used + w - 1] ] )

-- | DtaStructures.h:222-231.
sedutilDataStore :: [Member]
sedutilDataStore =
  [ Whole "featureCode" 2 "Feature Code"                                   -- line 223
  , Bits "reserved_v" 4 "Reserved"                                         -- line 224
  , Bits "version" 4 "Version"                                             -- line 225
  , Whole "length" 1 "Length"                                              -- line 226
  , Whole "reserved01" 2 "Reserved"                                        -- line 227
  , Whole "maxTables" 2 "Maximum number of DataStore Tables"               -- line 228
  , Whole "maxSizeTables" 4 "Maximum total size of DataStore Tables"       -- line 229
  , Whole "tableSizeAlignment" 4 "DataStore Table size alignment"          -- line 230
  ]

-- | DtaStructures.h:342-355.
sedutilBlockSID :: [Member]
sedutilBlockSID =
  [ Whole "featureCode" 2 "Feature Code"                                   -- line 343
  , Bits "reserved_v" 4 "Reserved"                                         -- line 344
  , Bits "version" 4 "Version"                                             -- line 345
  , Whole "length" 1 "Length"                                              -- line 346
  , Bits "SIDvalueState" 1 "SID Value State"                               -- line 347
  , Bits "BlockSIDState" 1 "SID Blocked State"                             -- line 348
  , Bits "reserved07" 6 "Reserved"                                         -- line 349
  , Bits "HardReset" 1 "Hardware Reset"                                    -- line 350
  , Bits "reserved06" 7 "Reserved"                                         -- line 351
  , Whole "reserved05" 2 "Reserved"                                        -- line 352
  , Whole "reserved04" 4 "Reserved"                                        -- line 353
  , Whole "reserved03" 4 "Reserved"                                        -- line 354
  ]

-- | What every descriptor's instance says of it: its layout tiles; its
--   code, and Length the bytes after the header, are its layout's pins; its
--   header is Core Table 41's general format; every allowance names a field.
instanceFacts :: Feature f => String -> Proxy f -> Spec
instanceFacts name p = it (name <> ": code, Length and header agree with its layout") $ do
  let l = recordLayout p
  tiling l `shouldBe` []
  pins l `shouldContain` [("Feature Code", featureCode p)]
  [ v | ("Length", v) <- pins l ] `shouldBe` maybeToList (featureLength p)
  featureLength p `shouldBe` Just (toInteger (layoutSize l - headerSize (featureHeader p)))
  featureHeader p `shouldBe` FeatureHeader 4 "Length"
  filter (`notElem` map fieldName (layoutFields l)) (map fst (featureAllowances p)) `shouldBe` []

-- | A record's encoding decoded strictly is the record, nothing differing, nothing left.
roundTrip :: (Feature f, Eq f, Show f) => Gen f -> Property
roundTrip g = forAll g $ \r -> (encodeRecord r >>= decodeFeatureStrict) === Right (DecodedRecord r [] B.empty)

-- | A four-bit field's value.
nibble :: Gen Word8
nibble = choose (0, 15)

-- | A descriptor decoded strictly by the type of its code: what differed and
--   what was left.
strictly :: Descriptor -> Maybe (Either LayoutError ([Difference], ByteString))
strictly d = ($ dBytes d) <$> L.lookup (dCode d) decoders
  where
    decoders =
      [ decoder (Proxy :: Proxy Level0DiscoveryTPerFeature), decoder (Proxy :: Proxy Level0DiscoveryLockingFeature)
      , decoder (Proxy :: Proxy Level0DiscoveryGeometryReportingFeature), decoder (Proxy :: Proxy SiisFeature)
      , decoder (Proxy :: Proxy DataStoreFeature), decoder (Proxy :: Proxy Level0DiscoveryOpalSSCV2Feature)
      , decoder (Proxy :: Proxy BlockSIDFeature), decoder (Proxy :: Proxy NamespaceLockingFeature)
      , decoder (Proxy :: Proxy Level0DiscoverySupportedDataRemovalMechanismFeature) ]
    decoder :: forall f. Feature f => Proxy f -> (Word16, ByteString -> Either LayoutError ([Difference], ByteString))
    decoder p = (fromInteger (featureCode p), fmap (\r -> (drDifferences r, drRest r)) . decodeAs)
      where
        decodeAs :: ByteString -> Either LayoutError (DecodedRecord f)
        decodeAs = decodeFeatureStrict

-- | The record of a type among descriptors, decoded strictly from the one of its code.
decodedIn :: forall f. Feature f => [Descriptor] -> Proxy f -> Maybe f
decodedIn ds p = do
  d <- L.find ((== fromInteger (featureCode p)) . dCode) ds
  either (const Nothing) (Just . drRecord) (decodeFeatureStrict (dBytes d) :: Either LayoutError (DecodedRecord f))
