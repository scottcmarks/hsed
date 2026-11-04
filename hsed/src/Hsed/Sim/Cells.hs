{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Cells
Description : A simulated SP's cells built, read and put in a row: the boolean cell, whether a cell is True, a cell put in a column
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
written only through 'putCell', as M5b's Set will be.
-}
module Hsed.Sim.Cells
  ( booleanCell
  , isTrue
  , putCell
  ) where

import qualified RIO.List            as L
import           RIO

import           Hsed.Core.Table     (Cell (..), ColNum, Row)
import           Hsed.Core.Types     (enumValue)
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
