{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Sim.Level0
Description : The simulated TPer's Level 0 Discovery response
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The simulator's answer to IF-RECV on protocol 1, ComID 1 (Core 3.3.6.2,
PDF 67): its header and its descriptors in increasing code order (3.3.6.3,
PDF 68), exactly the bytes the transfer length asks for, truncated or
zero-padded. Outside the synchronous protocol (3.3.10.3 b, PDF 84): it
changes no state.

The descriptors are an Opal 2.30 TPer's (design 8.2): TPer, Locking and
Geometry Reporting (Opal Tables 3-5), SIIS (SIIS 1.20 Table 2), Additional
DataStore Table, Opal SSC V2 (Opal Table 6), Block SID Authentication and
Supported Data Removal Mechanism (Opal Table 8), codes 0x0001, 0x0002,
0x0003, 0x0005, 0x0202, 0x0203, 0x0402 and 0x0404: Opal 2.30 makes the
DataStore and Block SID feature sets mandatory (2.10, PDF 18), and UCT-01
reads both (steps 4b-4c, Test Cases PDF 30). Each field holds what its
descriptor's allowances give it ("Hsed.Core.Feature".'Allowance': the
version an 'AnyVersion' prints, the least a 'Minimum' allows, zero for a
vendor unique value and for a present state, nothing being locked and no MBR
shadowing enabled or done at M3) unless the parameters choose it
("Hsed.Sim.Types".'SimParams': the Locking SP's life cycle, the geometry,
the SIIS revision, the DataStore tables, the ComIDs, the SSC minor version,
the data removal version). A profile (@--profile@, a drive's response) adds
each descriptor of a code the simulator lacks, as it is; none of the
simulator's is dropped or replaced (design 9.2).
-}
module Hsed.Sim.Level0
  ( level0Response
  , simDescriptors
  ) where

import           Data.Bits              (bit)
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO

import           Hsed.Core.Feature      (Allowance (..), Feature (..))
import           Hsed.Core.Layout       (Content (..), Field (..), FieldKind (..), Layout (..), LayoutError)
import           Hsed.Core.Record       (LayoutRecord (..))
import           Hsed.Gen.Opal.Features (Level0DiscoveryGeometryReportingFeature (..),
                                         Level0DiscoveryLockingFeature (..), Level0DiscoveryOpalSSCV2Feature (..),
                                         Level0DiscoverySupportedDataRemovalMechanismFeature (..),
                                         Level0DiscoveryTPerFeature)
import           Hsed.Level0            (Descriptor (..), featureDescriptor, renderLevel0)
import           Hsed.Sim.FeatureSets   (BlockSIDFeature, DataStoreFeature (..), SiisFeature (..))
import           Hsed.Sim.Types         (Geometry (..), Level0Profile (..), LifeCycle (..), SimParams (..),
                                         TPerState (..))


-- | The response to a Level 0 IF-RECV of a transfer length: exactly that
--   many bytes, the header's vendor unique bytes zeros.
level0Response :: TPerState -> Word32 -> ByteString
level0Response st = renderLevel0 mempty (simDescriptors st)

-- | The simulator's descriptors, in increasing code order: Opal's and the
--   hand-written feature sets' ("Hsed.Sim.FeatureSets"), their values from
--   the parameters and the state, and a profile's codes the simulator lacks.
simDescriptors :: TPerState -> [Descriptor]
simDescriptors st = L.sortOn dCode (own <> added)
  where
    p = tsParams st
    (tables, size, alignment) = spDataStore p
    geometry = spGeometry p
    own = rights
      [ described (id :: Level0DiscoveryTPerFeature -> Level0DiscoveryTPerFeature)
        -- LockingEnabled is one once an SP of the Locking template is other than Nonexistent or
        -- Manufactured-Inactive (Opal 3.1.1.3.1, PDF 21)
      , described (\r -> r { level0DiscoveryLockingFeatureLockingEnabled = spLockingOFS p /= ManufacturedInactive })
      , featureDescriptor Level0DiscoveryGeometryReportingFeature
          { level0DiscoveryGeometryReportingFeatureALIGN                = gAlign geometry
          , level0DiscoveryGeometryReportingFeatureLogicalBlockSize     = gLogicalBlockSize geometry
          , level0DiscoveryGeometryReportingFeatureAlignmentGranularity = gGranularity geometry
          , level0DiscoveryGeometryReportingFeatureLowestAlignedLBA     = gLowestAligned geometry
          }
      , described (\r -> r { siisFeatureSIISRevisionNumber = spSiisRevision p })
      , described (\r -> r { dataStoreFeatureMaximumNumberOfDataStoreTables    = tables
                           , dataStoreFeatureMaximumTotalSizeOfDataStoreTables = size
                           , dataStoreFeatureDataStoreTableSizeAlignment       = alignment })
      , described (\r -> r { level0DiscoveryOpalSSCV2FeatureSSCMinorVersionNumber = spSscMinor p
                           , level0DiscoveryOpalSSCV2FeatureBaseComID             = spBaseComId p
                           , level0DiscoveryOpalSSCV2FeatureNumberOfComIDs        = spNComIds p })
        -- SID Value State 0: C_PIN_SID's PIN is MSID's, as UCT-01 step 4c asks (Test Cases PDF 30)
      , described (id :: BlockSIDFeature -> BlockSIDFeature)
        -- Cryptographic Erase, bit 2 of Supported Data Removal Mechanism, which an Opal TPer SHALL support
        -- (3.1.1.6.3, Table 10, PDF 27); no removal processing or interrupted, no time reported (Table 11, PDF 28)
      , described (\r -> r { level0DiscoverySupportedDataRemovalMechanismFeatureVersion = spDataRemovalVersion p
                           , level0DiscoverySupportedDataRemovalMechanismFeatureSupportedDataRemovalMechanism = bit 2 })
      ]
    -- a profile's descriptors of codes the simulator lacks, the first of each code
    added = L.nubBy ((==) `on` dCode)
      [ d | d <- maybe [] lpDescriptors (spProfile p), dCode d `notElem` map dCode own ]

-- | A descriptor whose fields hold what its allowances give them, then what
--   the simulator chooses of them.
described :: Feature f => (f -> f) -> Either LayoutError Descriptor
described choose = featureDescriptor . choose =<< allowedRecord

-- | A feature record whose every field holds what the descriptor's
--   allowances give it: the version an 'AnyVersion' prints, the least a
--   'Minimum' allows, and zero for the rest (a vendor unique value, a
--   present state).
allowedRecord :: forall f. Feature f => Either LayoutError f
allowedRecord = fromFields [ (fieldName f, value f) | f <- layoutFields (recordLayout proxy) ]
  where
    proxy = Proxy :: Proxy f
    value f = case (fieldKind f, L.lookup (fieldName f) (featureAllowances proxy)) of
      (Bytes, _)               -> Octets (B.replicate (fieldWidth f `div` 8) 0)
      (_, Just (AnyVersion v)) -> Number v
      (_, Just (Minimum v))    -> Number v
      _                        -> Number 0
