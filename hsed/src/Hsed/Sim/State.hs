{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim.State
Description : The simulated TPer's state read and changed: sessions, a ComID's life, an SP's rows and cells, the properties
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
* an SP's tables: a table's rows, the row of an object by its UID (column
  0x00), a row changed, a cell by column number and a column by name in a
  generated description;
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
    -- * Tables and cells
  , tableRows
  , rowWithUID
  , updateRow
  , cellOf
  , columnOf
    -- * Properties
  , tperProperty
  , hostProperty
  , initialHostProperties
  ) where

import qualified RIO.List        as L
import qualified RIO.Map         as Map
import           RIO

import           Hsed.Core.Table (Cell (..), Col (..), ColNum, Row, TableDesc, columnNamed)
import           Hsed.Core.UID   (UID)
import           Hsed.Sim.Types


-- * Sessions

-- | Open a regular session on a ComID: the host's HSN, the SP, whether it
--   is read-write, the authorities authenticated at startup (and their
--   classes, Core 3.3.7.1.4, PDF 74-75); the TPer assigns the next TSN
--   ('tsNextTSN', 'firstTSN' first, back to it past the last, skipping one a
--   session of the ComID still holds, so that every session on a ComID has
--   a TSN of its own, 3.3.7.1.1, PDF 72) and the session keeps the ComID's
--   host properties as they are (5.2.2.4, PDF 144-145). The session, and
--   the TPer with it open.
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


-- * Tables and cells

-- | The rows of an SP's table, by the SP's and the table's UIDs; none for a
--   table or an SP the TPer lacks.
tableRows :: UID -> UID -> TPerState -> [Row]
tableRows sp table st = fromMaybe [] (Map.lookup sp (tsSPs st) >>= Map.lookup table . spsTables)

-- | The row of an SP's table whose UID column (0x00) holds a UID.
rowWithUID :: UID -> UID -> UID -> TPerState -> Maybe Row
rowWithUID sp table u st = L.find ((== Just (UIDCell u)) . cellOf 0) (tableRows sp table st)

-- | A row of an SP's table changed, found by its UID; the TPer as it was
--   when there is no such row.
updateRow :: UID -> UID -> UID -> (Row -> Row) -> TPerState -> TPerState
updateRow sp table u f st = st { tsSPs = Map.adjust changed sp (tsSPs st) }
  where
    changed sps = sps { spsTables = Map.adjust (map change) table (spsTables sps) }
    change r    = if cellOf 0 r == Just (UIDCell u) then f r else r

-- | A row's cell in a column, if the row gives one.
cellOf :: ColNum -> Row -> Maybe Cell
cellOf = L.lookup

-- | The number of the column of a name in a generated table description, if
--   the description has one.
columnOf :: TableDesc -> Text -> Maybe ColNum
columnOf desc name = colNumber <$> columnNamed name desc


-- * Properties

-- | A TPer property by name, as the parameters report it.
tperProperty :: Text -> SimParams -> Maybe Integer
tperProperty name = L.lookup name . spTPerProperties

-- | A host property by name, from what a ComID or a session keeps.
hostProperty :: Text -> Map Text Natural -> Maybe Natural
hostProperty = Map.lookup
