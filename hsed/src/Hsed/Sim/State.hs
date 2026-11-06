{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.State
Description : The simulated TPer's state read and changed: sessions, a ComID's life, an SP's rows, cells and life cycle, the properties
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The helpers over "Hsed.Sim.Types"' 'TPerState' that the Session Manager, the
regular sessions and the resets share (design 8.1-8.2, M4a):

* sessions: opened with the next TSN (Core 3.3.7.1.1, PDF 72: distinct per
  ComID, never 0 to 4095, so from 4096), the ComID's host properties
  snapshotted (5.2.2.4, PDF 144-145); closed by ComID and TSN; aborted by a
  predicate (a ComID's, an SP's, all, for the resets of 3.3.7.1.5, PDF 75-76);
  resolved from a packet's Session, TSN then HSN (3.3.7.1.2, PDF 73); and a
  ComID's life derived from them (3.3.3, PDF 56: Associated while a session
  is open, else what the ComID holds);
* an SP's tables: a table's rows and its description as the SP holds them,
  the row of an object by its UID (column 0x00), a row changed (in the
  TPer, or in an SP as a session sees it, M5b's Set), a cell by column
  number, a column by name in a generated description, and a row's cell by
  its column's name in the description the SP holds;
* a session's transactions (since M5b; design 11 M5b, Scott's answer Q5):
  the session as the TPer now holds it, its SP as it sees it and put back
  as it changes it, how many levels it has open, and a level opened,
  committed or dropped, each level a working copy of the SP in the
  session ("Hsed.Sim.Types".@sesTxn@), the committed SP in @tsSPs@ (Core
  3.3.7.3, PDF 77-78); the bodies are group G2's (M5b), until then M5a's
  behaviour, every session outside a transaction;
* the Locking SP's tables (since M5a): its Locking table's rows, the
  global range and each range after it, its MBRControl row, and the size of
  a byte table it holds, the MBR or the DataStore (Opal 2.30 Tables 36, 45
  and 46, PDF 54 and 86-87);
* an SP's life cycle: the LifeCycleState cell of the SP's row of the Admin
  SP's SP table (Core 4.1, PDF 91), the one place the TPer keeps it, which
  the Session Manager, Level 0's Locking descriptor and the jig read;
* TPER_RESET's enabling: the ProgrammaticResetEnable cell of the Admin SP's
  TPerInfo row (Opal 2.30 3.2.3 and Table 26, PDF 28-29 and 49);
* the properties: the TPer's by name from the parameters, the host's by
  name from what a ComID or a session keeps, and what a ComID starts with
  ('initialHostProperties', "Hsed.Sim.Params"').
-}
module Hsed.Sim.State
  ( -- * Sessions
    openSession
  , closeSession
  , abortSessions
  , resolveSession
  , sessionsOn
  , lifeOf
    -- * A session's transactions (M5b)
  , liveSession
  , spView
  , putSPView
  , txnDepth
  , beginTxn
  , commitTxn
  , dropTxn
    -- * Tables and cells
  , tableRows
  , descOf
  , rowWithUID
  , updateRow
  , updateRowIn
  , cellOf
  , columnOf
  , cellNamedIn
  , booleanNamedIn
    -- * The Locking SP's tables
  , lockingRows
  , mbrControl
  , byteTableSize
    -- * Life cycles
  , spLifeCycle
    -- * TPER_RESET
  , programmaticResetEnabled
    -- * Properties
  , tperProperty
  , hostProperty
  , initialHostProperties
  ) where

import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import           RIO

import           Hsed.Core.Table         (Cell (..), ColNum, Row, TableDesc, columnOf)
import           Hsed.Core.Types         (enumFromValue)
import           Hsed.Core.UID           (UID)
import           Hsed.Gen.Core.Enums     (Core_boolean (..))
import           Hsed.Gen.Core.TableUIDs (uLockingTable, uMBRControlTable, uSPTable, uTPerInfoTable)
import           Hsed.Gen.Core.UIDs      (uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import           Hsed.Sim.Cells          (isTrue)
import           Hsed.Sim.Types


-- * Sessions

-- | Open a regular session on a ComID: the host's HSN, the SP, whether it
--   is read-write, the authorities authenticated at startup (and their
--   classes, Core 3.3.7.1.4, PDF 74-75); the TPer assigns the next TSN
--   ('tsNextTSN', 'firstTSN' first, back to it past the last, skipping one a
--   session of the ComID still holds, so that every session on a ComID has
--   a TSN of its own, 3.3.7.1.1, PDF 72) and the session keeps the ComID's
--   host properties as they are (5.2.2.4, PDF 144-145), no transaction open
--   (3.3.7.3, PDF 77: one is opened by the host's Start Transaction). The
--   session, and the TPer with it open.
openSession :: Word16 -> Word32 -> UID -> Bool -> [UID] -> TPerState -> (Session, TPerState)
openSession c hsn sp write authorities st =
  (session, st { tsSessions = Map.insert (c, tsn) session (tsSessions st), tsNextTSN = next })
  where
    -- the next TSN not held on the ComID, from tsNextTSN round to the one before it (the first free is found at once
    -- unless 2^32 - 4096 sessions were opened on one TPer and that one is still open)
    tsn     = fromMaybe (tsNextTSN st) (L.find (\t -> Map.notMember (c, t) (tsSessions st)) ([tsNextTSN st .. maxBound] <> [firstTSN .. tsNextTSN st - 1]))
    next    = if tsn == maxBound then firstTSN else tsn + 1
    session = Session
      { sesComId       = c
      , sesTSN         = tsn
      , sesHSN         = hsn
      , sesSP          = sp
      , sesWrite       = write
      , sesAuthorities = authorities
      , sesHostProps   = maybe initialHostProperties cHostProps (Map.lookup c (tsComIds st))
      , sesTxn         = []
      }

-- | Close the session of a ComID and TSN, if open (Core 3.3.7.1.5, PDF 75:
--   its resources released).
closeSession :: Word16 -> Word32 -> TPerState -> TPerState
closeSession c tsn st = st { tsSessions = Map.delete (c, tsn) (tsSessions st) }

-- | Abort every session a predicate selects: gone, with nothing sent for
--   them (what the caller stages, a CloseSession, is its own, 3.3.10.7 b,
--   PDF 86).
abortSessions :: (Session -> Bool) -> TPerState -> TPerState
abortSessions gone st = st { tsSessions = Map.filter (not . gone) (tsSessions st) }

-- | The session a packet on a ComID names, by its TSN and HSN (Core
--   3.3.7.1.2, PDF 73), if it is open with that HSN.
resolveSession :: Word16 -> Word32 -> Word32 -> TPerState -> Maybe Session
resolveSession c tsn hsn st = L.find ((== hsn) . sesHSN) (Map.lookup (c, tsn) (tsSessions st))

-- | The sessions open on a ComID, by TSN.
sessionsOn :: Word16 -> TPerState -> [Session]
sessionsOn c st = [ s | ((c', _), s) <- Map.toList (tsSessions st), c' == c ]

-- | A ComID's life (Core 3.3.3, PDF 56), if the TPer has the ComID: Associated
--   while a session is open on it, else as the ComID holds it, Inactive or
--   Issued.
lifeOf :: Word16 -> TPerState -> Maybe ComIdLife
lifeOf c st = life <$> Map.lookup c (tsComIds st)
  where
    life cs
      | cLife cs == Inactive     = Inactive
      | null (sessionsOn c st)   = cLife cs
      | otherwise                = Associated


-- * A session's transactions (M5b)

-- | The session as the TPer now holds it, by its ComID and TSN: a 'Session'
--   read before an item of a payload changed it (a transaction level opened
--   by an item before) is stale; 'Nothing' once the session is closed or
--   aborted.
liveSession :: Session -> TPerState -> Maybe Session
liveSession ses st = Map.lookup (sesComId ses, sesTSN ses) (tsSessions st)

-- | The session's SP as the session sees it: within a transaction the
--   innermost level's copy (Core 3.3.7.3, PDF 77: changes made within a
--   transaction are visible within it), else the committed SP the TPer
--   holds; 'Nothing' for an SP the TPer lacks. Group G2's (M5b): until then
--   the committed SP alone, as at M5a.
spView :: Session -> TPerState -> Maybe SPState
spView ses st = Map.lookup (sesSP ses) (tsSPs st)

-- | The session's SP put back as a method changed it: within a transaction
--   into the innermost level's copy, committed when the top-level one is
--   (Core 3.3.7.3 b, PDF 77), else committed at once (3.3.7.3 a, PDF 77).
--   Group G2's (M5b): until then committed at once, as at M5a.
putSPView :: Session -> SPState -> TPerState -> TPerState
putSPView ses sps st = st { tsSPs = Map.insert (sesSP ses) sps (tsSPs st) }

-- | How many transaction levels a session has open (Core 3.3.7.3.1, PDF
--   78), none outside a transaction.
txnDepth :: Session -> Int
txnDepth = length . sesTxn

-- | A transaction level opened in a session by the host's Start
--   Transaction (Core 3.2.2.3.3.4, PDF 37): a copy of the SP as the session
--   sees it ('spView') pushed. Group G2's (M5b): until then the TPer
--   unchanged, no level opened.
beginTxn :: Session -> TPerState -> TPerState
beginTxn _ = id

-- | A session's innermost transaction level committed by the host's End
--   Transaction of status 0x00 (Core 3.2.2.3.3.5, PDF 37): at the top level
--   its copy becomes the committed SP (3.3.7.3 b, PDF 77), deeper its
--   parent's (3.3.7.3.1, PDF 78: the nested level's changes part of its
--   parent's). Group G2's (M5b): until then the TPer unchanged.
commitTxn :: Session -> TPerState -> TPerState
commitTxn _ = id

-- | A session's innermost transaction level aborted, its copy dropped, so
--   that the SP is as it was when the level opened (Core 3.3.7.3, PDF 78:
--   rolled back; 3.2.2.3.3.5, PDF 37-38: the host's End Transaction of a
--   status other than 0x00). Group G2's (M5b): until then the TPer
--   unchanged.
dropTxn :: Session -> TPerState -> TPerState
dropTxn _ = id


-- * Tables and cells

-- | The rows of an SP's table, by the SP's and the table's UIDs; none for a
--   table or an SP the TPer lacks.
tableRows :: UID -> UID -> TPerState -> [Row]
tableRows sp table st = fromMaybe [] (Map.lookup sp (tsSPs st) >>= Map.lookup table . spsTables)

-- | The description of an SP's table, by the SP's and the table's UIDs, as
--   the SP holds it with the table ("Hsed.Sim.Types".@spsDescs@: with an
--   SSC's columns added); none for a table or an SP the TPer lacks.
descOf :: UID -> UID -> TPerState -> Maybe TableDesc
descOf sp table st = Map.lookup sp (tsSPs st) >>= Map.lookup table . spsDescs

-- | The row of an SP's table whose UID column (0x00) holds a UID.
rowWithUID :: UID -> UID -> UID -> TPerState -> Maybe Row
rowWithUID sp table u st = L.find ((== Just (UIDCell u)) . cellOf 0) (tableRows sp table st)

-- | A row of an SP's table changed, found by its UID ('updateRowIn' in the
--   SP the TPer holds); the TPer as it was when there is no such row.
updateRow :: UID -> UID -> UID -> (Row -> Row) -> TPerState -> TPerState
updateRow sp table u f st = st { tsSPs = Map.adjust (updateRowIn table u f) sp (tsSPs st) }

-- | A row of an SP's table changed, by the table's UID and the row's UID
--   (its column 0x00), in an SP: the TPer's or a session's view of it
--   ('spView', where M5b's Set writes, 'putSPView' putting it back); the SP
--   as it was when the table holds no such row.
updateRowIn :: UID -> UID -> (Row -> Row) -> SPState -> SPState
updateRowIn table u f sps = sps { spsTables = Map.adjust (map change) table (spsTables sps) }
  where change r = if cellOf 0 r == Just (UIDCell u) then f r else r

-- | A row's cell in a column, if the row gives one.
cellOf :: ColNum -> Row -> Maybe Cell
cellOf = L.lookup

-- | A row's cell in the column of a name, the column found in the
--   description an SP holds of the row's table, by the table's UID
--   ("Hsed.Sim.Types".@spsDescs@: the description with an SSC's columns
--   added, as Opal 2.30 adds TPerInfo's ProgrammaticResetEnable, Table 26,
--   PDF 49, which no generated Core description has); 'Nothing' where the SP
--   describes no such table or column, or the row gives no cell in it.
cellNamedIn :: SPState -> UID -> Text -> Row -> Maybe Cell
cellNamedIn sps table name r = Map.lookup table (spsDescs sps) >>= \desc -> columnOf desc name >>= (`cellOf` r)

-- | Whether a row holds True in a boolean column of a name (Core Table 59,
--   PDF 103; "Hsed.Sim.Cells".'isTrue'), the column found as 'cellNamedIn'
--   finds it; False where the row holds no such cell. The one boolean
--   reader of the resets, Level 0 and the resets' laws.
booleanNamedIn :: SPState -> UID -> Text -> Row -> Bool
booleanNamedIn sps table name r = maybe False isTrue (cellNamedIn sps table name r)


-- * The Locking SP's tables

-- | The rows of the Locking SP's Locking table, the global range and each
--   range after it (Opal 2.30 Table 45, PDF 86: Locking_GlobalRange,
--   Locking_Range1 and Locking_RangeNNNN, a row for each range
--   'spLockingRanges' gives), in the order the fold gives them; none for a
--   TPer without the table.
--
-- >>> import Hsed.Sim.Types (defaultSimParams, initialState)
-- >>> length (lockingRows (initialState defaultSimParams))
-- 9
lockingRows :: TPerState -> [Row]
lockingRows = tableRows uSP_Locking uLockingTable

-- | The Locking SP's MBRControl row, its one row (Opal 2.30 Table 46, PDF
--   87); none for a TPer without it.
--
-- >>> import Hsed.Sim.Types (defaultSimParams, initialState)
-- >>> isJust (mbrControl (initialState defaultSimParams))
-- True
mbrControl :: TPerState -> Maybe Row
mbrControl = listToMaybe . tableRows uSP_Locking uMBRControlTable

-- | The size in bytes of an SP's byte table, by the SP's and the table's
--   UIDs ("Hsed.Sim.Types".@spsBytes@: its Table row's Rows, Core
--   5.3.2.3.8, PDF 157); none for a table the SP does not hold as a byte
--   table.
--
-- >>> import Hsed.Gen.Core.TableUIDs (uMBRTable)
-- >>> import Hsed.Sim.Types (defaultSimParams, initialState)
-- >>> byteTableSize uSP_Locking uMBRTable (initialState defaultSimParams)
-- Just 134217728
byteTableSize :: UID -> UID -> TPerState -> Maybe Natural
byteTableSize sp table st = Map.lookup sp (tsSPs st) >>= fmap btSize . Map.lookup table . spsBytes


-- * Life cycles

-- | An SP's life cycle state, the value of the life_cycle_state enumeration
--   (Opal 2.30 Table 49, PDF 98: Manufactured-Inactive 8, Manufactured 9;
--   "Hsed.Sim.Params".'lifeCycleValue') that the LifeCycleState cell of its
--   row of the Admin SP's SP table holds: life cycle states are recorded
--   there, and the TPer changes that cell whenever an SP's state changes
--   (Core 4.1, PDF 91; Opal 2.30 5.2.2.2.1-2, PDF 97: Activate and Revert,
--   M6's), so the simulator keeps it nowhere else. The column is found by
--   name in the Admin SP's description of its SP table ('descOf'). 'Nothing'
--   where the Admin SP holds no row for the SP (no such SP) or the row no
--   enumeration in that column.
--
-- >>> spLifeCycle uSP_Admin (initialState defaultSimParams)
-- Just 9
spLifeCycle :: UID -> TPerState -> Maybe Int
spLifeCycle sp st = do
  desc <- descOf uSP_Admin uSPTable st
  col  <- columnOf desc "LifeCycleState"
  rowWithUID uSP_Admin uSPTable sp st >>= cellOf col >>= \case
    EnumCell _ v -> Just v
    _            -> Nothing


-- * TPER_RESET

-- | Whether TPER_RESET is enabled: the ProgrammaticResetEnable cell of the
--   Admin SP's TPerInfo row, TRUE enabling the command (Opal 2.30 3.2.3, PDF
--   28-29; Table 26, PDF 49; FALSE at the factory, Table 27, PDF 50), a
--   boolean (Core Table 59, PDF 103). The row is the table's one (Core
--   5.4.2.1, PDF 229: exactly one row), whatever its UID (Opal 2.30 Table
--   27's, PDF 50, is not Core Table 244's, PDF 305), and the column is found
--   by name in the Admin SP's description of the table ('cellNamedIn').
--   'Nothing' where the Admin SP holds no TPerInfo row, or the row no
--   boolean in that column; the generated Admin SP holds the row, its cell
--   folded from the parameters' @spTPerResetEnabled@ ("Hsed.Sim.Preconfig").
programmaticResetEnabled :: TPerState -> Maybe Bool
programmaticResetEnabled st = do
  sps <- Map.lookup uSP_Admin (tsSPs st)
  r   <- listToMaybe (tableRows uSP_Admin uTPerInfoTable st)
  cellNamedIn sps uTPerInfoTable "ProgrammaticResetEnable" r >>= \case
    EnumCell _ v -> (== Core_boolean_True) <$> enumFromValue v
    _            -> Nothing


-- * Properties

-- | A TPer property by name, as the parameters report it.
tperProperty :: Text -> SimParams -> Maybe Integer
tperProperty name = L.lookup name . spTPerProperties

-- | A host property by name, from what a ComID or a session keeps.
hostProperty :: Text -> Map Text Natural -> Maybe Natural
hostProperty = Map.lookup
