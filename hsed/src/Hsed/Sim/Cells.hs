{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Cells
Description : A simulated SP's cells built, read and put in a row: the boolean cell, whether a cell is True, a cell put in a column, a value as a column's cell
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The few operations on a row's cells that the fold ("Hsed.Sim.Preconfig"),
the resets ("Hsed.Sim.Reset"), Level 0 ("Hsed.Sim.Level0"), the resets'
laws ("Hsed.Law.Reset") and their generator ("Hsed.Sim.Gen") share, in one
place (since the M5a review, which found each written four times): a
boolean cell as Core Table 59 has it (PDF 103), whether a cell holds True,
and a cell put in a row's column. It imports hsed-core and the generated
Core enumerations alone, so that the fold, which "Hsed.Sim.Types" imports,
can import it too ("Hsed.Sim.State" cannot be imported there). A row is
written only through 'putCell', as M5b's Set will be; the value a Set gives
a column is read as the column's cell by 'cellFor' (since M5b), or refused,
INVALID_PARAMETER for a value not of the column's type, FAIL for a type the
simulator's Set does not carry out (Scott's answer Q3, design 11 M5b).
-}
module Hsed.Sim.Cells
  ( booleanCell
  , isTrue
  , putCell
    -- * A value as a column's cell (M5b)
  , CellRefusal (..)
  , cellFor
  ) where

import qualified RIO.List            as L
import           RIO

import           Hsed.Core.Table     (Cell (..), Col (..), ColNum, Row)
import           Hsed.Core.Types     (enumValue)
import           Hsed.Core.Value     (Value)
import           Hsed.Gen.Core.Enums (Core_boolean (..))


-- $setup
-- >>> :set -XOverloadedStrings

-- | A boolean cell, labelled and valued as Core Table 59 has it (PDF 103:
--   False 0, True 1).
--
-- >>> map booleanCell [False, True]
-- [EnumCell "False" 0,EnumCell "True" 1]
booleanCell :: Bool -> Cell
booleanCell b = if b then EnumCell "True" (enumValue Core_boolean_True) else EnumCell "False" (enumValue Core_boolean_False)

-- | Whether a cell is a boolean's True, by its value whatever its label (a
--   preconfiguration prints @T@, Opal 2.30 Table 45, PDF 86); any other cell
--   is not.
--
-- >>> map isTrue [EnumCell "T" 1, booleanCell True, booleanCell False, UIntCell 1]
-- [True,True,False,False]
isTrue :: Cell -> Bool
isTrue = \case
  EnumCell _ v -> v == enumValue Core_boolean_True
  _            -> False

-- | A row with a cell put in a column, in column order, replacing the cell
--   the column held.
--
-- >>> putCell 2 (UIntCell 9) [(1, UIntCell 1), (3, UIntCell 3), (2, UIntCell 2)]
-- [(1,UIntCell 1),(2,UIntCell 9),(3,UIntCell 3)]
putCell :: ColNum -> Cell -> Row -> Row
putCell c new r = L.sortOn fst ((c, new) : filter ((/= c) . fst) r)


-- * A value as a column's cell (M5b)

-- | Why a value a Set gives a column is not taken as the column's cell, and
--   why in words: not of the column's type (Core 5.1.5.11 b, PDF 139:
--   INVALID_PARAMETER for a value outside, or of another type than, the
--   column's; Set's Fails c, 5.3.3.7.4, PDF 203), or of a type the
--   simulator's Set does not carry out, a gap of the simulator's until M6
--   (5.1.5.16, PDF 139: FAIL, none of the other statuses applying; Scott's
--   answer Q3, design 11 M5b).
data CellRefusal
  = NotOfType !Text
  | NotSimulated !Text
  deriving (Eq, Show)

-- | A value a Set gives a column read as the column's cell, in the form the
--   fold gives a cell of the column's type ("Hsed.Sim.Preconfig"), so that
--   the resets, Level 0 and their laws read it as they read a folded one;
--   or why not ('CellRefusal'). M5b's Set carries out two types: boolean
--   (Core Table 59, PDF 103) and reset_types, a set over 0 to 31 (Core
--   Tables 127-128, PDF 119; its values Opal 2.30 Table 16's, PDF 32).
--   Group G1's (M5b): until then every value is refused, of a type the
--   simulator's Set does not carry out.
cellFor :: Col -> Value -> Either CellRefusal Cell
cellFor col _ = Left (NotSimulated ("column " <> colName col <> ": the simulator's Set reads no column's type yet (group G1's, M5b)"))
