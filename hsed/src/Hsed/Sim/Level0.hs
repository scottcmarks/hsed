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
the data removal version) or the state does: the Opal SSC V2 descriptor's
Initial C_PIN_SID PIN Indicator says whether the initial C_PIN_SID PIN was
C_PIN_MSID's, a fact of manufacture the state holds
("Hsed.Sim.Types".@tsInitialSidPinIsMsid@, set from the parameters; Opal
Table 6's bullets, PDF 23; changed by no Set, only by M5's Revert of the
Admin SP after SID authenticated, Opal 5.1.2.2.1 item 2 a, PDF 93), and the
Block SID descriptor's SID Value State whether C_PIN_SID's PIN is C_PIN_MSID's
now, read from the Admin SP's C_PIN rows (Test Cases UCT-01 step 4c, PDF 30,
and UCT-03 step 1, PDF 33: on a drive taken ownership of, byte 13 stays 0x00
while SID Value State goes 1). A profile (@--profile@, a drive's response)
adds each descriptor of a code the simulator lacks, as it is; none of the
simulator's is dropped or replaced (design 9.2). Every one of its own
descriptors encodes for parameters in range ("Hsed.Sim.Types".'simParamsProblems',
which the transport is opened only without); one that does not is an error,
never a descriptor left out.
-}
module Hsed.Sim.Level0
  ( level0Response
  , simDescriptors
  ) where

import           Data.Bits              (bit)
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO

import           Hsed.Core.Feature        (Allowance (..), Feature (..))
import           Hsed.Core.Layout         (Content (..), Field (..), FieldKind (..), Layout (..), LayoutError)
import           Hsed.Core.Record         (LayoutRecord (..))
import           Hsed.Core.Table          (Cell (..))
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs  (uC_PINTable)
import           Hsed.Gen.Core.UIDs       (uC_PIN_SID, uSP_Admin)
import           Hsed.Gen.Opal.Features   (Level0DiscoveryGeometryReportingFeature (..),
                                           Level0DiscoveryLockingFeature (..), Level0DiscoveryOpalSSCV2Feature (..),
                                           Level0DiscoverySupportedDataRemovalMechanismFeature (..),
                                           Level0DiscoveryTPerFeature)
import           Hsed.Level0              (Descriptor (..), featureDescriptor, renderLevel0)
import           Hsed.OpalUIDs            (uC_PIN_MSID)
import           Hsed.Sim.FeatureSets     (BlockSIDFeature (..), DataStoreFeature (..), SiisFeature (..))
import           Hsed.Sim.State           (cellOf, columnOf, rowWithUID)
import           Hsed.Sim.Types           (Geometry (..), Level0Profile (..), LifeCycle (..), SimParams (..),
                                           TPerState (..))


-- | The response to a Level 0 IF-RECV of a transfer length: exactly that
--   many bytes, the header's vendor unique bytes zeros.
level0Response :: TPerState -> Word32 -> ByteString
level0Response st = renderLevel0 mempty (simDescriptors st)

-- | The simulator's descriptors, in increasing code order: Opal's and the
--   hand-written feature sets' ("Hsed.Sim.FeatureSets"), their values from
--   the parameters and the state, and a profile's codes the simulator lacks.
--   An own descriptor that does not encode is an error naming it: parameters
--   out of range, which the transport refuses.
simDescriptors :: TPerState -> [Descriptor]
simDescriptors st = L.sortOn dCode (own <> added)
  where
    p = tsParams st
    (tables, size, alignment) = spDataStore p
    geometry = spGeometry p
    own = either (\e -> error ("Hsed.Sim.Level0: an own descriptor does not encode, the parameters out of range: " <> show e)) id
        $ sequence
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
        -- the Initial C_PIN_SID PIN Indicator (byte 13): 0x00 where the initial C_PIN_SID PIN equalled C_PIN_MSID's,
        -- 0xFF where it was VU and may differ (Opal Table 6's bullets, PDF 23), a fact of manufacture the state
        -- holds; the generated allowance is VU, the whole byte, though the bullets make only 0x00 and 0xFF valid and
        -- 0x01 to 0xFE reserved
      , described (\r -> r { level0DiscoveryOpalSSCV2FeatureSSCMinorVersionNumber    = spSscMinor p
                           , level0DiscoveryOpalSSCV2FeatureBaseComID                = spBaseComId p
                           , level0DiscoveryOpalSSCV2FeatureNumberOfComIDs           = spNComIds p
                           , level0DiscoveryOpalSSCV2FeatureInitialC_PIN_SIDPINIndicator = if tsInitialSidPinIsMsid st then 0x00 else 0xFF })
        -- SID Value State (byte 4 bit 0): 0 while C_PIN_SID's PIN is MSID's, as UCT-01 step 4c asks of a drive at
        -- its factory state (Test Cases PDF 30), 1 once it is another: the present state, from the live rows
      , described (\r -> r { blockSIDFeatureSIDValueState = not sidPinIsMsid })
        -- Cryptographic Erase, bit 2 of Supported Data Removal Mechanism, which an Opal TPer SHALL support
        -- (3.1.1.6.3, Table 10, PDF 27); no removal processing or interrupted, no time reported (Table 11, PDF 28)
      , described (\r -> r { level0DiscoverySupportedDataRemovalMechanismFeatureVersion = spDataRemovalVersion p
                           , level0DiscoverySupportedDataRemovalMechanismFeatureSupportedDataRemovalMechanism = bit 2 })
      ]
    -- a profile's descriptors of codes the simulator lacks, the first of each code
    added = L.nubBy ((==) `on` dCode)
      [ d | d <- maybe [] lpDescriptors (spProfile p), dCode d `notElem` map dCode own ]
    -- whether C_PIN_SID's PIN is C_PIN_MSID's now, read from the Admin SP's C_PIN rows (Opal Table 25, PDF 48-49),
    -- which the parameters fill (spSidPin, spMsid) and a Set may later change (M5); a row without a PIN cell holds
    -- the parameters' value
    sidPinIsMsid = pinOf uC_PIN_SID (fromMaybe (spMsid p) (spSidPin p)) == pinOf uC_PIN_MSID (spMsid p)
    pinOf u fallback = case rowWithUID uSP_Admin uC_PINTable u st >>= \row -> columnOf c_PINTableDesc "PIN" >>= (`cellOf` row) of
      Just (BytesCell pin) -> pin
      _                    -> fallback

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
