{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.FeatureSets
Description : The feature descriptors no generated module has (SIIS 0x0005, DataStore 0x0202, Block SID 0x0402, Configurable Namespace Locking 0x0403), and Opal's Table 7
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Four Level 0 feature descriptors the simulator sends or the drive does whose
layouts no compiled spec gives (design 13.10, decided by the lead at M3:
hand-written, each citing its source):

* SIIS (0x0005): SIIS 1.20 Table 2 (PDF 21-22), a PDF hsedc does not read
  (untagged);
* Additional DataStore Table (0x0202) and Block SID Authentication (0x0402):
  feature sets whose PDFs are not in @tcg/@; their layouts are sedutil's
  structures (@Common/DtaStructures.h@ at 01746a5, lines 222-231 and
  342-355), their names the Test Cases' (Test Cases 1.02 UCT-01 steps 4b and
  4c, PDF 30); Opal 2.30 makes both feature sets mandatory (2.10, PDF 18);
* Configurable Namespace Locking (0x0403): CNL 1.30 Table 6 (PDF 51) and
  5.2.1.2-5.2.1.4 (PDF 52), untagged too.

Each is what a generated descriptor is ("Hsed.Gen.Core.Features"): a
'Layout' whose pins are the values its source makes exact, a record of its
other fields ("Hsed.Core.Record"), and an instance of 'Feature' with its
code, version, length, Core Table 41's general header (as the generated
descriptors have it) and what its source allows the fields
('featureAllowances'). 'geometryAllowances' is what the simulator allows
Opal's Geometry Reporting descriptor until LockingInfo is simulated (M5).

One table of Opal's that is no descriptor is hand-written here too, with
its page, until Opal's tables other than its descriptors are compiled (M5):
Table 7, the SSC minor versions ('sscMinorVersions', PDF 24), which the
Pair runner evaluates the Opal SSC V2 descriptor's SSC Minor Version Number
against and UCT-01 names in its note. No other module of hsed writes a
value of either spec by hand (design 13.28 (11)).

Verified at M3 (group G2) against the sources: SIIS byte 2 (Data Structure
Version bits 7-4, reserved 3-0) and byte 5 (reserved bits 7-3, Identifier
Usage Scope 2-1, Key Change Zone Behavior 0) by the column rules of Table 2's
grid (PDF 21-22); CNL Table 6's bytes 2 and 4 by its grid's (PDF 51); the
DataStore and Block SID layouts bit for bit against sedutil's structures,
which declare a byte's bitfields bit 0 first (the test suite holds both).

Decided at M3 (group G2), reversible: the DataStore and Block SID
descriptors' Version. Their feature sets' PDFs (Opal 2.30's references [6]
and [8], 2.10, PDF 18) are not in @tcg/@ and sedutil reads the version nibble
without pinning or printing it, so no source gives a value: the simulator
sends 0x1, the first version a feature set's descriptor takes (as Core's
Tables 42-43 and SIIS's Table 2 pin it), and each instance allows another
('AnyVersion' 0x1), as Opal's own descriptors allow theirs, so that a
drive's version differs from the simulator's only as an allowed difference.
Neither pins it ('featureVersion' 'Nothing'). Open for Scott: check both
against the feature sets' PDFs when they are added to @tcg/@.
-}
module Hsed.Sim.FeatureSets
  ( -- * SIIS (0x0005)
    SiisFeature (..)
  , siisFeatureLayout
    -- * Additional DataStore Table (0x0202)
  , DataStoreFeature (..)
  , dataStoreFeatureLayout
    -- * Block SID Authentication (0x0402)
  , BlockSIDFeature (..)
  , blockSIDFeatureLayout
    -- * Configurable Namespace Locking (0x0403)
  , NamespaceLockingFeature (..)
  , namespaceLockingFeatureLayout
    -- * Geometry Reporting, until M5
  , geometryAllowances
    -- * Supported Data Removal Mechanism
  , dataRemovalAllowances
    -- * Opal Table 7, until M5
  , sscMinorVersions
  ) where

import           RIO

import           Hsed.Core.Feature      (Allowance (..), Feature (..))
import           Hsed.Core.Layout       (FieldKind (..), Layout (..), spanning)
import           Hsed.Core.Record       (LayoutRecord (..), flag, flagField, number, numberField)
import           Hsed.Gen.Core.Features (TPerFeature)


-- | Core Table 41's general format of a descriptor's header, as the
--   generated descriptors have it.
generalHeader :: Proxy TPerFeature
generalHeader = Proxy


-- * SIIS (0x0005)

-- | SIIS 1.20 Table 2, Level 0 Discovery - SIIS Feature Descriptor (PDF
--   21-22): Feature Code, Data Structure Version and Length pinned by the
--   bullets after it (PDF 22).
siisFeatureLayout :: Layout
siisFeatureLayout = Layout
  { layoutName   = "Level 0 Discovery - SIIS Feature Descriptor"
  , layoutSize   = 16
  , layoutFields =
      [ spanning "Feature Code" (0, 7) (1, 0) (Pinned 0x0005)
      , spanning "Data Structure Version" (2, 7) (2, 4) (Pinned 0x1)
      , spanning "Reserved" (2, 3) (2, 0) Reserved
      , spanning "Length" (3, 7) (3, 0) (Pinned 0x0c)
      , spanning "SIIS Revision Number" (4, 7) (4, 0) Unsigned
      , spanning "Reserved" (5, 7) (5, 3) Reserved
      , spanning "Identifier Usage Scope" (5, 2) (5, 1) Unsigned
      , spanning "Key Change Zone Behavior" (5, 0) (5, 0) Flag
      , spanning "Reserved" (6, 7) (15, 0) Reserved
      ]
  , layoutTail   = Nothing
  }

-- | The SIIS descriptor's fields that carry a value.
data SiisFeature = SiisFeature
  { siisFeatureSIISRevisionNumber    :: !Word8  -- ^ byte 4: Table 3's revision, 0x20 for v1.20 (PDF 22)
  , siisFeatureIdentifierUsageScope  :: !Word8  -- ^ byte 5 bits 2-1 (Table 4, PDF 23)
  , siisFeatureKeyChangeZoneBehavior :: !Bool   -- ^ byte 5 bit 0
  }
  deriving (Eq, Show)

instance LayoutRecord SiisFeature where
  recordLayout _ = siisFeatureLayout
  recordFields r =
    [ ("SIIS Revision Number", number (siisFeatureSIISRevisionNumber r))
    , ("Identifier Usage Scope", number (siisFeatureIdentifierUsageScope r))
    , ("Key Change Zone Behavior", flag (siisFeatureKeyChangeZoneBehavior r))
    ]
  fromFields vs = pure SiisFeature
    <*> numberField "SIIS Revision Number" vs
    <*> numberField "Identifier Usage Scope" vs
    <*> flagField "Key Change Zone Behavior" vs

-- | The bullets after Table 2 (PDF 22): the revision as specified in Table
--   3, the two behaviours VU.
instance Feature SiisFeature where
  featureCode _       = 0x0005
  featureVersion _    = Just 0x1
  featureLength _     = Just 0x0c
  featureHeader _     = featureHeader generalHeader
  featureAllowances _ =
    [ ("SIIS Revision Number", AsPrinted "As specified in Table 3")
    , ("Key Change Zone Behavior", VendorUnique)
    , ("Identifier Usage Scope", VendorUnique)
    ]


-- * Additional DataStore Table (0x0202)

-- | The Additional DataStore Table Feature Descriptor: sedutil's 16-byte
--   structure (DtaStructures.h:222-231), so a Length of 0x0C; its fields
--   named as Test Cases 1.02 UCT-01 step 4b names them (PDF 30). Its
--   version is no pin: the feature set's PDF is not in @tcg/@.
dataStoreFeatureLayout :: Layout
dataStoreFeatureLayout = Layout
  { layoutName   = "Additional DataStore Table Feature Descriptor"
  , layoutSize   = 16
  , layoutFields =
      [ spanning "Feature Code" (0, 7) (1, 0) (Pinned 0x0202)
      , spanning "Version" (2, 7) (2, 4) Unsigned
      , spanning "Reserved" (2, 3) (2, 0) Reserved
      , spanning "Length" (3, 7) (3, 0) (Pinned 0x0c)
      , spanning "Reserved" (4, 7) (5, 0) Reserved
      , spanning "Maximum number of DataStore Tables" (6, 7) (7, 0) Unsigned
      , spanning "Maximum total size of DataStore Tables" (8, 7) (11, 0) Unsigned
      , spanning "DataStore Table size alignment" (12, 7) (15, 0) Unsigned
      ]
  , layoutTail   = Nothing
  }

-- | The DataStore table descriptor's fields that carry a value.
data DataStoreFeature = DataStoreFeature
  { dataStoreFeatureVersion                           :: !Word8   -- ^ byte 2 bits 7-4
  , dataStoreFeatureMaximumNumberOfDataStoreTables    :: !Word16  -- ^ bytes 6 to 7
  , dataStoreFeatureMaximumTotalSizeOfDataStoreTables :: !Word32  -- ^ bytes 8 to 11
  , dataStoreFeatureDataStoreTableSizeAlignment       :: !Word32  -- ^ bytes 12 to 15
  }
  deriving (Eq, Show)

instance LayoutRecord DataStoreFeature where
  recordLayout _ = dataStoreFeatureLayout
  recordFields r =
    [ ("Version", number (dataStoreFeatureVersion r))
    , ("Maximum number of DataStore Tables", number (dataStoreFeatureMaximumNumberOfDataStoreTables r))
    , ("Maximum total size of DataStore Tables", number (dataStoreFeatureMaximumTotalSizeOfDataStoreTables r))
    , ("DataStore Table size alignment", number (dataStoreFeatureDataStoreTableSizeAlignment r))
    ]
  fromFields vs = pure DataStoreFeature
    <*> numberField "Version" vs
    <*> numberField "Maximum number of DataStore Tables" vs
    <*> numberField "Maximum total size of DataStore Tables" vs
    <*> numberField "DataStore Table size alignment" vs

-- | The least UCT-01 accepts of each value (step 4b, Test Cases PDF 30), and
--   the version decided at M3 (see the module header): 0x1 or another.
instance Feature DataStoreFeature where
  featureCode _       = 0x0202
  featureVersion _    = Nothing
  featureLength _     = Just 0x0c
  featureHeader _     = featureHeader generalHeader
  featureAllowances _ =
    [ ("Version", AnyVersion 0x1)
    , ("Maximum number of DataStore Tables", Minimum 1)
    , ("Maximum total size of DataStore Tables", Minimum 0xA0000)
    , ("DataStore Table size alignment", Minimum 1)
    ]


-- * Block SID Authentication (0x0402)

-- | The Block SID Authentication Feature Descriptor: sedutil's 16-byte
--   structure (DtaStructures.h:342-355), so a Length of 0x0C; byte 4 bit 0
--   the SID Value State Test Cases 1.02 UCT-01 step 4c reads (PDF 30), bit 1
--   the SID Blocked State, byte 5 bit 0 the Hardware Reset. Its version is
--   no pin: the feature set's PDF is not in @tcg/@.
blockSIDFeatureLayout :: Layout
blockSIDFeatureLayout = Layout
  { layoutName   = "Block SID Authentication Feature Descriptor"
  , layoutSize   = 16
  , layoutFields =
      [ spanning "Feature Code" (0, 7) (1, 0) (Pinned 0x0402)
      , spanning "Version" (2, 7) (2, 4) Unsigned
      , spanning "Reserved" (2, 3) (2, 0) Reserved
      , spanning "Length" (3, 7) (3, 0) (Pinned 0x0c)
      , spanning "Reserved" (4, 7) (4, 2) Reserved
      , spanning "SID Blocked State" (4, 1) (4, 1) Flag
      , spanning "SID Value State" (4, 0) (4, 0) Flag
      , spanning "Reserved" (5, 7) (5, 1) Reserved
      , spanning "Hardware Reset" (5, 0) (5, 0) Flag
      , spanning "Reserved" (6, 7) (15, 0) Reserved
      ]
  , layoutTail   = Nothing
  }

-- | The Block SID descriptor's fields that carry a value.
data BlockSIDFeature = BlockSIDFeature
  { blockSIDFeatureVersion         :: !Word8  -- ^ byte 2 bits 7-4
  , blockSIDFeatureSIDBlockedState :: !Bool   -- ^ byte 4 bit 1
  , blockSIDFeatureSIDValueState   :: !Bool   -- ^ byte 4 bit 0
  , blockSIDFeatureHardwareReset   :: !Bool   -- ^ byte 5 bit 0
  }
  deriving (Eq, Show)

instance LayoutRecord BlockSIDFeature where
  recordLayout _ = blockSIDFeatureLayout
  recordFields r =
    [ ("Version", number (blockSIDFeatureVersion r))
    , ("SID Blocked State", flag (blockSIDFeatureSIDBlockedState r))
    , ("SID Value State", flag (blockSIDFeatureSIDValueState r))
    , ("Hardware Reset", flag (blockSIDFeatureHardwareReset r))
    ]
  fromFields vs = pure BlockSIDFeature
    <*> numberField "Version" vs
    <*> flagField "SID Blocked State" vs
    <*> flagField "SID Value State" vs
    <*> flagField "Hardware Reset" vs

-- | No source in @tcg/@ lists what its fields may hold: the version decided
--   at M3 (see the module header), 0x1 or another; and its three flags, each
--   the TPer's present state, which a host's Block SID command and the
--   resets set (sedutil's @Discovery0BlockSIDFeatures@), not a constant of
--   the descriptor: as printed, compared unchecked (UCT-01 step 4c holds SID
--   Value State to 0 where the case asks it, Test Cases PDF 30).
instance Feature BlockSIDFeature where
  featureCode _       = 0x0402
  featureVersion _    = Nothing
  featureLength _     = Just 0x0c
  featureHeader _     = featureHeader generalHeader
  featureAllowances _ =
    [ ("Version", AnyVersion 0x1)
    , ("SID Value State", AsPrinted "the present state: whether C_PIN_SID's PIN is still MSID's")
    , ("SID Blocked State", AsPrinted "the present state: whether a Block SID command has blocked SID's authentication")
    , ("Hardware Reset", AsPrinted "the present state: whether a hardware reset clears the block, as the Block SID command set it")
    ]


-- * Configurable Namespace Locking (0x0403)

-- | CNL 1.30 Table 6, Level 0 Discovery - Configurable Namespace Locking
--   Feature Descriptor (PDF 51): Feature Code (5.2.1.1, PDF 51), Feature
--   Descriptor Version Number 0x2 and Length 0x10 (5.2.1.2 and 5.2.1.4, PDF
--   52) pinned.
namespaceLockingFeatureLayout :: Layout
namespaceLockingFeatureLayout = Layout
  { layoutName   = "Level 0 Discovery - Configurable Namespace Locking Feature Descriptor"
  , layoutSize   = 20
  , layoutFields =
      [ spanning "Feature Code" (0, 7) (1, 0) (Pinned 0x0403)
      , spanning "Feature Descriptor Version Number" (2, 7) (2, 4) (Pinned 0x2)
      , spanning "Feature Set Minor Version Number" (2, 3) (2, 0) Unsigned
      , spanning "Length" (3, 7) (3, 0) (Pinned 0x10)
      , spanning "Range_C" (4, 7) (4, 7) Flag
      , spanning "Range_P" (4, 6) (4, 6) Flag
      , spanning "SUM_C" (4, 5) (4, 5) Flag
      , spanning "Reserved" (4, 4) (4, 0) Reserved
      , spanning "Reserved" (5, 7) (7, 0) Reserved
      , spanning "Maximum Key Count" (8, 7) (11, 0) Unsigned
      , spanning "Unused Key Count" (12, 7) (15, 0) Unsigned
      , spanning "Maximum Ranges Per Namespace" (16, 7) (19, 0) Unsigned
      ]
  , layoutTail   = Nothing
  }

-- | The Configurable Namespace Locking descriptor's fields that carry a value.
data NamespaceLockingFeature = NamespaceLockingFeature
  { namespaceLockingFeatureFeatureSetMinorVersionNumber :: !Word8   -- ^ byte 2 bits 3-0 (Table 7, PDF 52)
  , namespaceLockingFeatureRangeC                       :: !Bool    -- ^ byte 4 bit 7
  , namespaceLockingFeatureRangeP                       :: !Bool    -- ^ byte 4 bit 6
  , namespaceLockingFeatureSUMC                         :: !Bool    -- ^ byte 4 bit 5
  , namespaceLockingFeatureMaximumKeyCount              :: !Word32  -- ^ bytes 8 to 11
  , namespaceLockingFeatureUnusedKeyCount               :: !Word32  -- ^ bytes 12 to 15
  , namespaceLockingFeatureMaximumRangesPerNamespace    :: !Word32  -- ^ bytes 16 to 19
  }
  deriving (Eq, Show)

instance LayoutRecord NamespaceLockingFeature where
  recordLayout _ = namespaceLockingFeatureLayout
  recordFields r =
    [ ("Feature Set Minor Version Number", number (namespaceLockingFeatureFeatureSetMinorVersionNumber r))
    , ("Range_C", flag (namespaceLockingFeatureRangeC r))
    , ("Range_P", flag (namespaceLockingFeatureRangeP r))
    , ("SUM_C", flag (namespaceLockingFeatureSUMC r))
    , ("Maximum Key Count", number (namespaceLockingFeatureMaximumKeyCount r))
    , ("Unused Key Count", number (namespaceLockingFeatureUnusedKeyCount r))
    , ("Maximum Ranges Per Namespace", number (namespaceLockingFeatureMaximumRangesPerNamespace r))
    ]
  fromFields vs = pure NamespaceLockingFeature
    <*> numberField "Feature Set Minor Version Number" vs
    <*> flagField "Range_C" vs
    <*> flagField "Range_P" vs
    <*> flagField "SUM_C" vs
    <*> numberField "Maximum Key Count" vs
    <*> numberField "Unused Key Count" vs
    <*> numberField "Maximum Ranges Per Namespace" vs

-- | The minor version is a value as specified in Table 7 (5.2.1.3, PDF 52).
instance Feature NamespaceLockingFeature where
  featureCode _       = 0x0403
  featureVersion _    = Just 0x2
  featureLength _     = Just 0x10
  featureHeader _     = featureHeader generalHeader
  featureAllowances _ = [("Feature Set Minor Version Number", AsPrinted "a value as specified in Table 7")]


-- * Geometry Reporting, until M5

-- | What the simulator allows Opal's Geometry Reporting descriptor (Opal
--   2.30 Table 5): ALIGN, LogicalBlockSize, AlignmentGranularity and
--   LowestAlignedLBA as the LockingInfo table has them (3.1.1.4.2-5, PDF 22),
--   by the field names of the generated Opal layout. Table 5's bullets pin
--   only its header (PDF 22), so the generated instance allows nothing; each
--   value is the LockingInfo table's, which the simulator's parameters stand
--   in for until M5, and a drive's own LockingInfo may differ.
geometryAllowances :: [(Text, Allowance)]
geometryAllowances =
  [ ("ALIGN", AsPrinted "the AlignmentRequired column of the LockingInfo table")                                -- 3.1.1.4.2
  , ("LogicalBlockSize", AsPrinted "the value of the LogicalBlockSize column in the LockingInfo table")          -- 3.1.1.4.3
  , ("AlignmentGranularity", AsPrinted "the value of the AlignmentGranularity column in the LockingInfo table")  -- 3.1.1.4.4
  , ("LowestAlignedLBA", AsPrinted "the value of the LowestAlignedLBA column in the LockingInfo table")          -- 3.1.1.4.5
  ]


-- * Supported Data Removal Mechanism

-- | What Opal 2.30 allows the Supported Data Removal Mechanism descriptor's
--   fields (Table 8; Tables 9-12, PDF 25-28), by the field names of the
--   generated layout, whose bullets give no allowance (Table 9 is a table,
--   not bullets):
--
-- * each Data Removal Time Format bit and Data Removal Time, the device's
--   own worst-case estimate of its mechanism, any value Tables 11 and 12
--   give a meaning (0 not reported): any value (3.1.1.6.4);
-- * the Supported Data Removal Mechanism, a bit for each mechanism the TPer
--   supports, Cryptographic Erase (bit 2) mandatory (3.1.1.6.3, Table 10):
--   as printed;
-- * Data Removal Operation Processing and Interrupted, the TPer's present
--   state (3.1.1.6.1-2): as printed.
--
-- Its Version has none: Table 9 prints 0x02 alone, with no other version
-- allowed (decided at the M3 review, reversible: design 13.29).
dataRemovalAllowances :: [(Text, Allowance)]
dataRemovalAllowances =
  [ ("Data Removal Operation Processing", AsPrinted "see section 3.1.1.6.1: set while a data removal operation is in progress")
  , ("Data Removal Operation Interrupted", AsPrinted "see section 3.1.1.6.2: set while one was interrupted")
  , ("Supported Data Removal Mechanism", AsPrinted "see section 3.1.1.6.3: a bit for each mechanism supported, Cryptographic Erase (bit 2) mandatory")
  ]
  <> [ (format b, VendorUnique) | b <- bits ]
  <> [ (time b, VendorUnique) | b <- bits ]
  where
    bits     = ["0", "1", "2", "5"] :: [Text]
    format b = "Data Removal Time Format for Bit " <> b
    time b   = "Data Removal Time for Supported Data Removal Mechanism Bit " <> b


-- * Opal Table 7, until M5

-- | Opal 2.30 Table 7, SSC Minor Versions (PDF 24): the values the Opal SSC
--   V2 descriptor's SSC Minor Version Number may hold (Table 6's bullets,
--   PDF 23: \"As specified in Table 7\"), each with the specification it
--   references; every other value is Reserved. Written by hand, as the
--   module header says: Opal's tables other than its descriptors are not
--   compiled until M5.
sscMinorVersions :: [(Integer, Text)]
sscMinorVersions =
  [ (0x0, "TCG Opal SSC Specification v2.00")
  , (0x1, "TCG Opal SSC Specification v2.01")
  , (0x2, "TCG Opal SSC Specification v2.02")
  , (0x3, "TCG Opal SSC Specification v2.30")
  ]
