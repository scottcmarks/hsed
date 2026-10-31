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
does (Core 5.3.4.1.1.2, PDF 210). STACK_RESET does a ComID's share (Core
3.3.4.7.5 items 1-8, PDF 65-66; 5.2.2.4.5, PDF 148): its sessions aborted
with no CloseSession, its staged response dropped, Awaiting IF-SEND, its host
properties the initial assumptions; its ComID management buffer is kept, the
STACK_RESET response being what "Hsed.Sim.ComIdMgmt" then stages in it. The
simulator has no transactions, no session startup in progress between two
commands and no method processing between them, so items 2, 3 and 5 of each
list have nothing to do.
-}
module Hsed.Sim.Reset
  ( ResetKind (..)
  , event
  , resetTPer
  , stackReset
  ) where

import qualified RIO.Map                  as Map
import           RIO

import           Hsed.Core.Table          (Cell (..), Row)
import           Hsed.Core.Types          (enumValue)
import           Hsed.Gen.Core.Enums      (Core_boolean (..))
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs  (uC_PINTable)
import           Hsed.Sim.State           (abortSessions, columnOf, initialHostProperties)
import           Hsed.Sim.Types           (ComIdState (..), MgmtBuffer (..), SPState (..), Session (..), SyncState (..),
                                           TPerState (..))
import           Hsed.Wire                (Event (..))


-- | What reset the TPer: an interface event of each kind (Opal 2.30 Table
--   16, PDF 32: reset types 0 to 2), or TPER_RESET (type 3, Programmatic;
--   Opal 3.2.3, PDF 28-29).
data ResetKind
  = OnPowerCycle
  | OnHardwareReset
  | OnHotPlug
  | OnTPerReset
  deriving (Eq, Ord, Show, Enum, Bounded)

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
--   (Core 5.3.4.1.1.2, PDF 210).
resetTPer :: ResetKind -> TPerState -> TPerState
resetTPer kind st = (abortSessions (const True) st)
  { tsComIds = fmap comId (tsComIds st)
  , tsSPs    = if kind == OnPowerCycle then fmap triesReset (tsSPs st) else tsSPs st
  }
  where
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
      | persistent r = r
      | otherwise    = [ (col, if Just col == triesColumn then UIntCell 0 else cell) | (col, cell) <- r ]
    persistent r = case persistenceColumn >>= (`lookup` r) of
      Just (EnumCell _ v) -> v == enumValue Core_boolean_True
      _                   -> False
    triesColumn       = columnOf c_PINTableDesc "Tries"
    persistenceColumn = columnOf c_PINTableDesc "Persistence"
