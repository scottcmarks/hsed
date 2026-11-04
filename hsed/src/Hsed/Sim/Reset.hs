{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Reset
Description : What a reset does to the simulated TPer: an interface event, TPER_RESET, STACK_RESET on a ComID
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The resets of design 8.4 as M4a has them. An interface reset (a power cycle,
a hardware reset, a hot plug: Opal 2.30 3.3.5.1 and Table 16, PDF 31-32) and
TPER_RESET (Opal 3.2.3 a-g, PDF 28-29) abort every session, with no
CloseSession prepared and no notification (Core 3.3.7.1.5, PDF 75-76),
invalidate every command and response buffer (a ComID's staged response and
its ComID management buffer, Core 3.3.4.7.5, PDF 66 b) and return every ComID
to Awaiting IF-SEND (Core 3.3.10.6 S2:S1 a and S3:S1 a, PDF 85-86). Only a
power cycle, a hardware reset and TPER_RESET reset the TPer's knowledge of the
host's properties to the initial assumptions; a hot plug, another TCG reset
event, keeps it (Core 5.2.2.4.4, PDF 148; Opal 3.2.3 g, PDF 29). A power cycle
alone resets a C_PIN's Tries, where its Persistence is False; no other reset
does (Core 5.3.4.1.1.2, PDF 210). Since M5a every reset of the TPer also
applies the Locking SP's reset columns while the SP is Manufactured (since
the M5a review, not while it is Manufactured-Inactive, where "the Locking
SP's management of the Storage Device's locking and media encryption
features SHALL be disabled", Opal 2.30 5.2.2.3.1, PDF 97, the plan's D3
having applied them at either life): a Locking object whose LockOnReset lists the
reset's type has ReadLocked and WriteLocked set True, whatever its
ReadLockEnabled and WriteLockEnabled (Core 5.7.2.2.10, PDF 275; Opal 3.2.3 h,
PDF 29, for TPER_RESET's Programmatic), and an empty set changes neither;
MBRControl's Done is set False where its MBRDoneOnReset lists it (Core
5.7.2.5.4, PDF 278; Opal 3.2.3 i, PDF 29). STACK_RESET does a ComID's share (Core
3.3.4.7.5 items 1-8, PDF 65-66; 5.2.2.4.5, PDF 148): its sessions aborted
with no CloseSession, its staged response dropped, Awaiting IF-SEND, its host
properties the initial assumptions; its ComID management buffer is kept, the
STACK_RESET response being what "Hsed.Sim.ComIdMgmt" then stages in it. The
simulator has no transactions, no session startup in progress between two
commands and no method processing between them, so items 2, 3 and 5 of each
list have nothing to do, and it changes no Locking or MBRControl row (its
items 1-8 name none).
-}
module Hsed.Sim.Reset
  ( ResetKind (..)
  , resetType
  , event
  , resetTPer
  , stackReset
  , lockingReset
  , resetListed
  ) where

import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO

import           Hsed.Core.Table          (Cell (..), Row)
import           Hsed.Core.Types          (enumValue)
import           Hsed.Gen.Core.TableUIDs  (uC_PINTable, uLockingTable, uMBRControlTable)
import           Hsed.Gen.Opal.Enums      (Opal_reset_types (..))
import           Hsed.Sim.Cells           (booleanCell, putCell)
import           Hsed.Sim.State           (abortSessions, booleanNamedIn, cellOf, columnOf, initialHostProperties, spLifeCycle)
import           Hsed.Sim.Types           (ComIdState (..), LifeCycle (..), MgmtBuffer (..), SPState (..), Session (..), SyncState (..),
                                           TPerState (..), lifeCycleValue)
import           Hsed.Wire                (Event (..))


-- $setup
-- >>> :set -XOverloadedStrings

-- | What reset the TPer: an interface event of each kind (Opal 2.30 Table
--   16, PDF 32: reset types 0 to 2), or TPER_RESET (type 3, Programmatic;
--   Opal 3.2.3, PDF 28-29).
data ResetKind
  = OnPowerCycle
  | OnHardwareReset
  | OnHotPlug
  | OnTPerReset
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A reset's kind as a reset_types value, what a LockOnReset or an
--   MBRDoneOnReset cell holds of it (Opal 2.30 Table 16, PDF 32: Power Cycle
--   0, Hardware 1, HotPlug 2, Programmatic 3), the value Opal's generated
--   reset_types gives its constructor (Table 16, which replaces Core Table
--   128's, PDF 119, where 3 is Reserved; since M5a's integration).
--
-- >>> map resetType [minBound .. maxBound]
-- [0,1,2,3]
resetType :: ResetKind -> Natural
resetType = fromIntegral . enumValue . \case
  OnPowerCycle    -> Opal_reset_types_Power_Cycle
  OnHardwareReset -> Opal_reset_types_Hardware
  OnHotPlug       -> Opal_reset_types_HotPlug
  OnTPerReset     -> Opal_reset_types_Programmatic

-- | An interface event (Opal 2.30 3.3.5.1, PDF 31-32): the reset of its
--   kind.
event :: Event -> TPerState -> TPerState
event = resetTPer . \case
  PowerCycle    -> OnPowerCycle
  HardwareReset -> OnHardwareReset
  HotPlug       -> OnHotPlug

-- | A reset of the whole TPer, of a kind: every session gone (Opal 2.30
--   3.3.5.1 item 1, PDF 31; 3.2.3 b, PDF 29; Core 3.3.7.1.5, PDF 76), every
--   ComID awaiting IF-SEND with its buffers empty (3.3.5.1 items 4 and 6;
--   3.2.3 d-e; Core 3.3.4.7.5, PDF 66 b), the host's properties reset to the
--   initial assumptions on a power cycle, a hardware reset and TPER_RESET
--   and kept on a hot plug (Core 5.2.2.4.4, PDF 148; Opal 3.2.3 g), and Tries
--   reset to 0 on a power cycle in every C_PIN row whose Persistence is False
--   (Core 5.3.4.1.1.2, PDF 210), in every SP; and in every SP not
--   Manufactured-Inactive, the Locking and MBRControl rows as the reset's
--   type and their reset columns say ('lockingReset'): an SP's locking is
--   disabled while it is Manufactured-Inactive (Opal 2.30 5.2.2.3.1, PDF 97),
--   so that the rows the fold gives it are the rows Activate finds (since
--   the M5a review; the plan's D3 applied them at either life).
resetTPer :: ResetKind -> TPerState -> TPerState
resetTPer kind st = (abortSessions (const True) st)
  { tsComIds = fmap comId (tsComIds st)
  , tsSPs    = Map.mapWithKey (\sp -> locking sp . if kind == OnPowerCycle then triesReset else id) (tsSPs st)
  }
  where
    locking sp
      | spLifeCycle sp st == Just (lifeCycleValue ManufacturedInactive) = id
      | otherwise                                                       = lockingReset kind
    comId cs = cs
      { cSync      = S1
      , cMgmt      = MgmtBuffer False Nothing
      , cHostProps = if kind == OnHotPlug then cHostProps cs else initialHostProperties
      }

-- | STACK_RESET on a ComID (Core 3.3.4.7.5 items 1-8, PDF 65-66): its
--   sessions aborted with no CloseSession prepared (item 1), its staged
--   response dropped and the ComID awaiting IF-SEND (items 4 and 6; 3.3.10.6
--   S2:S1 b and S3:S1 b, PDF 85-86), its host properties the initial
--   assumptions (item 7; 5.2.2.4.5, PDF 148), nothing sent for any of it
--   (item 8); its ComID management buffer kept, for the response the command
--   stages. A ComID the TPer lacks is left as it is.
stackReset :: Word16 -> TPerState -> TPerState
stackReset c st = (abortSessions ((== c) . sesComId) st) { tsComIds = Map.adjust reset c (tsComIds st) }
  where reset cs = cs { cSync = S1, cHostProps = initialHostProperties }

-- | An SP's C_PIN rows with Tries 0 wherever Persistence is False (Core
--   5.3.4.1.1.2, PDF 210: Tries persists across power cycles only where
--   Persistence is True); a row with no Tries cell is as it was.
triesReset :: SPState -> SPState
triesReset sps = sps { spsTables = Map.adjust (map row) uC_PINTable (spsTables sps) }
  where
    row :: Row -> Row
    row r
      | booleanNamedIn sps uC_PINTable "Persistence" r = r
      | otherwise = [ (col, if Just col == triesColumn then UIntCell 0 else cell) | (col, cell) <- r ]
    -- the column found in the SP's own description of its C_PIN table (since the M5a review)
    triesColumn = Map.lookup uC_PINTable (spsDescs sps) >>= (`columnOf` "Tries")

-- | An SP's rows after a reset of a kind, as their reset columns list the
--   reset's type ('resetType'): each Locking row whose LockOnReset lists it
--   ReadLocked and WriteLocked True, whatever its ReadLockEnabled and
--   WriteLockEnabled (Core 5.7.2.2.10, PDF 275: "the reset types that cause
--   the values of the ReadLocked and WriteLocked columns of the Locking table
--   to be set to True"; an empty set changes neither), and each MBRControl
--   row whose MBRDoneOnReset lists it Done False (5.7.2.5.4, PDF 278); every
--   other cell, and every row whose set lists it not, as it was. The columns
--   are found by name in the SP's descriptions; an SP without the tables (the
--   Admin SP) is as it was.
lockingReset :: ResetKind -> SPState -> SPState
lockingReset kind sps = L.foldl' resetIn sps
  [ (uLockingTable, "LockOnReset", [("ReadLocked", booleanCell True), ("WriteLocked", booleanCell True)])
  , (uMBRControlTable, "MBRDoneOnReset", [("Done", booleanCell False)])
  ]
  where
    resetIn s (table, listing, puts) = fromMaybe s $ do
      desc    <- Map.lookup table (spsDescs s)
      listed  <- columnOf desc listing
      targets <- traverse (\(name, cell) -> (\c -> (c, cell)) <$> columnOf desc name) puts
      let reset r = if maybe False (resetListed kind) (cellOf listed r) then L.foldl' (flip (uncurry putCell)) r targets else r
      pure s { spsTables = Map.adjust (map reset) table (spsTables s) }

-- | Whether a reset_types set cell (Core Table 128, PDF 119, which Opal
--   2.30 Table 16 replaces, PDF 32) lists a reset's kind: its elements'
--   values, an enumeration's or a number, include the kind's 'resetType'.
--   A cell other than a set lists nothing.
--
-- >>> resetListed OnTPerReset (ListCell [EnumCell "Power Cycle" 0, EnumCell "Programmatic" 3])
-- True
-- >>> resetListed OnHardwareReset (ListCell [EnumCell "Power Cycle" 0])
-- False
-- >>> resetListed OnPowerCycle (ListCell [])
-- False
resetListed :: ResetKind -> Cell -> Bool
resetListed kind = \case
  ListCell cells -> resetType kind `elem` mapMaybe value cells
  _              -> False
  where
    value = \case
      EnumCell _ v -> Just (fromIntegral v)
      UIntCell v   -> Just (fromIntegral v)
      _            -> Nothing
