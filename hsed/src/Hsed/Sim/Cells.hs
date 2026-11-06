{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
enumerations alone (Core's boolean, and since M5b Opal's reset_types,
"Hsed.Gen.Opal.Enums"), so that the fold, which "Hsed.Sim.Types" imports,
can import it too ("Hsed.Sim.State" cannot be imported there). A row is
written only through 'putCell', as M5b's Set does ("Hsed.Sim.Base"); the
value a Set gives a column is read as the column's cell by 'cellFor' (since
M5b), or refused, INVALID_PARAMETER for a value not of the column's type,
FAIL for a type the simulator's Set does not carry out (Scott's answer Q3,
design 11 M5b).
-}
module Hsed.Sim.Cells
  ( booleanCell
  , isTrue
  , putCell
    -- * A value as a column's cell (M5b)
  , CellRefusal (..)
  , cellFor
  , resetLabel
  ) where

import qualified RIO.List            as L
import           RIO
import qualified RIO.Text            as T

import           Hsed.Core.Table     (Cell (..), Col (..), ColNum, Row)
import           Hsed.Core.Token     (Token (..))
import           Hsed.Core.TypeName  (TypeName (..))
import           Hsed.Core.Types     (Enumerated (..), enumFromValue, enumValue)
import           Hsed.Core.Value     (Datum (..), List (..), Value (..))
import           Hsed.Gen.Core.Enums (Core_boolean (..))
import           Hsed.Gen.Core.Types (ubooleanType, ureset_typesType)
import           Hsed.Gen.Opal.Enums (Opal_reset_types)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hsed.Core.TypeName (TypeName (..))
-- >>> import Hsed.Core.Value (Datum (..), List (..), Value (..))
-- >>> import Hsed.Gen.Core.Types (ubooleanType, unameType, ureset_typesType)

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
--   or why not ('CellRefusal'). The column's type is told by the UID of its
--   Type object ("Hsed.Core.TypeName".@typeNameUID@, the generated
--   descriptions' "Hsed.Gen.Core.Types"), and M5b's Set carries out two:
--
--   * boolean (Core Table 59, PDF 103): a uinteger 0 or 1 (5.1.2 b, PDF 99:
--     an enumeration is its value), the cell 'booleanCell' gives; any other
--     value is not of the type;
--
--   * reset_types (Core Table 127, a Set_Type over 0 to 31, PDF 119; its
--     values Opal 2.30 Table 16's, PDF 32, which replaces Core Table 128's:
--     Power Cycle 0, Hardware 1, HotPlug 2, Programmatic 3): a list of
--     uintegers in 0 to 31, none twice (5.1.2 i, PDF 100: a set is encoded
--     as a list), the cell a list of the enumeration's cells in ascending
--     value, each labelled as the fold labels it ('resetLabel'); a value
--     that is no list, an element past 31 or not a uinteger, or one listed
--     twice, is not of the type.
--
--   A column of any other type is one the simulator's Set does not carry
--   out (M6's).
--
-- >>> let enabled = Col 5 "Enabled" False (TypeName "boolean" (Just ubooleanType))
-- >>> cellFor enabled (D (Datum (Unsigned 1)))
-- Right (EnumCell "True" 1)
-- >>> cellFor enabled (D (Datum (Unsigned 2)))
-- Left (NotOfType "column 5, Enabled, holds a boolean, 0 False or 1 True (Core Table 59, PDF 103), not D (Datum (Unsigned 2))")
-- >>> let lockOnReset = Col 9 "LockOnReset" False (TypeName "reset_types" (Just ureset_typesType))
-- >>> cellFor lockOnReset (L (List [D (Datum (Unsigned 3)), D (Datum (Unsigned 0))]))
-- Right (ListCell [EnumCell "Power Cycle" 0,EnumCell "Programmatic" 3])
-- >>> either (const True) (const False) (cellFor (Col 2 "CommonName" True (TypeName "name" (Just unameType))) (D (Datum (Bytes "a name"))))
-- True
cellFor :: Col -> Value -> Either CellRefusal Cell
cellFor col v = case typeNameUID (colType col) of
  Just t
    | t == ubooleanType     -> boolean
    | t == ureset_typesType -> resetTypes
  _ -> Left (NotSimulated (column <> " is of type " <> typeNameText (colType col)
                           <> ", which the simulator's Set does not carry out until M6 (design 11 M5b, Scott's answer Q3)"))
  where
    column = "column " <> tshow (colNumber col) <> ", " <> colName col <> ","
    boolean = case v of
      D (Datum (Unsigned n)) | Just b <- inRange @Core_boolean n -> Right (booleanCell (b == Core_boolean_True))
      _ -> Left (NotOfType (column <> " holds a boolean, 0 False or 1 True (Core Table 59, PDF 103), not " <> tshow v))
    resetTypes = case v of
      L (List vs) -> do
        kinds <- traverse element vs
        unless (L.nub kinds == kinds) $
          Left (NotOfType (column <> " holds a set of reset types, each listed once (Core 5.1.2 i, PDF 100), not " <> tshow v))
        pure (ListCell [ EnumCell (resetLabel k) (enumValue k) | k <- L.sort kinds ])
      _ -> Left (NotOfType (column <> " holds a set of reset types, a list of uintegers 0 to 31 (Core Table 127, PDF 119; 5.1.2 i, PDF 100), not " <> tshow v))
    element = \case
      D (Datum (Unsigned n)) | Just k <- inRange @Opal_reset_types n -> Right k
      e -> Left (NotOfType (column <> " lists reset types, each a uinteger 0 to 31 (Core Table 127, PDF 119; Opal 2.30 Table 16, PDF 32), not " <> tshow e))

-- | A uinteger as a value of a generated enumeration, where the type's
--   range has it ("Hsed.Core.Types".'enumFromValue', the uinteger first
--   held within the range so that one past 'Int' is no value).
inRange :: forall e. (Enumerated e, Enum e, Bounded e) => Natural -> Maybe e
inRange n = guard (n <= fromIntegral (max 0 (snd (range @e)))) >> enumFromValue (fromIntegral n)

-- | A reset type's label as the fold prints it, Opal 2.30 Table 16's words
--   (PDF 32: Power Cycle, Hardware, HotPlug, Programmatic; the Locking SP's
--   LockOnReset prints @Power Cycle@, Table 45, PDF 86): the generated
--   constructor's name less the type's prefix, its underscores spaces.
--
-- >>> import Hsed.Gen.Opal.Enums (Opal_reset_types (..))
-- >>> map resetLabel [Opal_reset_types_Power_Cycle, Opal_reset_types_Programmatic, Opal_reset_types_Vendor_Unique_16]
-- ["Power Cycle","Programmatic","Vendor Unique 16"]
resetLabel :: Opal_reset_types -> Text
resetLabel k = T.map (\c -> if c == '_' then ' ' else c) (fromMaybe shown (T.stripPrefix "Opal_reset_types_" shown))
  where shown = tshow k
