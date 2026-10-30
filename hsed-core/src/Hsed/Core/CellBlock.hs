{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Core.CellBlock
Description : The cell_block abstract type (Core 5.1.4.2.3): Get's parameter, written, read and resolved against its target
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A cell_block names a portion of a table (Core 5.1.4.2.3, PDF 129-130). In
the messaging stream it is a list of Named values, one per component given,
each named by its position, 0x00 Table to 0x04 endColumn (5.1.4.1, PDF
128), and every component is optional:

> [ Table = uidref, startRow = typeOr { UID : uidref, Row : uinteger }, endRow = uinteger,
>   startColumn = uinteger, endColumn = uinteger ]

'cellBlockValue' writes one (the jig's Get), 'cellBlock' reads one (the
simulator's), through "Hsed.Core.Method".'Hsed.Core.Method.namedComponents'.
What a cell_block asks depends on what the method is invoked on, an object,
an object table or a byte table ('CellTarget'), by the context rules of
5.1.4.2.3, a.a to e.b: a component that must be omitted and is given, one
that must be given and is omitted, or a value past the table's columns or
rows fails the method with INVALID_PARAMETER (5.1.5.11 e, PDF 139; Get's
Fails b-d, 5.3.3.6.3, PDF 201). 'resolveCells' applies them, giving the
cells asked ('CellsAsked') or why the method fails, naming the rule.

The components are the five the Format above prints (PDF 130); this is the
hand-written reader of one abstract type, until each Type object has a
Haskell type (M6).
-}
module Hsed.Core.CellBlock
  ( -- * The grouping
    RowAddress (..)
  , CellBlock (..)
  , noCells
  , columnRange
  , cellBlockValue
  , cellBlock
    -- * What it asks of a target
  , CellTarget (..)
  , CellsAsked (..)
  , resolveCells
  ) where

import           RIO

import           Hsed.Core.Method (namedComponents, paramErrorText)
import           Hsed.Core.Token  (IsToken (..), Token (..))
import           Hsed.Core.UID    (UID)
import           Hsed.Core.Value  (Datum (..), List (..), Name (..), NamedValue (..), Value (..))


-- * The grouping

-- | startRow's value: the uid of an object (on an object table), or the
--   RowNumber of a byte table's row (5.1.4.2.3 b).
data RowAddress
  = RowUID !UID
  | RowNumber !Natural
  deriving (Eq, Show)

-- | A cell_block: its five components, each optional, named 0x00 to 0x04
--   in the messaging stream (5.1.4.2.3, PDF 129-130).
data CellBlock = CellBlock
  { cbTable       :: !(Maybe UID)         -- ^ 0x00 Table
  , cbStartRow    :: !(Maybe RowAddress)  -- ^ 0x01 startRow
  , cbEndRow      :: !(Maybe Natural)     -- ^ 0x02 endRow
  , cbStartColumn :: !(Maybe Natural)     -- ^ 0x03 startColumn
  , cbEndColumn   :: !(Maybe Natural)     -- ^ 0x04 endColumn
  }
  deriving (Eq, Show)

-- | No component: every default applies (the whole object, or every row of
--   a byte table).
noCells :: CellBlock
noCells = CellBlock Nothing Nothing Nothing Nothing Nothing

-- | The columns from one to another of the object or table a method is
--   invoked on: UCT-03's Get of C_PIN_MSID's PIN column alone.
columnRange :: Natural -> Natural -> CellBlock
columnRange start end = noCells { cbStartColumn = Just start, cbEndColumn = Just end }

-- | The grouping as the stream carries it: a list of the components given,
--   each a Named value named by its number, in order (5.1.4.1, PDF 128).
--
-- >>> cellBlockValue (columnRange 3 3)
-- L (List [N (NamedValue (Name (Datum (Unsigned 3))) (D (Datum (Unsigned 3)))),N (NamedValue (Name (Datum (Unsigned 4))) (D (Datum (Unsigned 3))))])
cellBlockValue :: CellBlock -> Value
cellBlockValue cb = L (List [ N (NamedValue (Name (Datum (Unsigned i))) v) | (i, Just v) <- zip [0 ..] components ])
  where
    components =
      [ uidValue <$> cbTable cb, rowValue <$> cbStartRow cb, natValue <$> cbEndRow cb
      , natValue <$> cbStartColumn cb, natValue <$> cbEndColumn cb ]
    rowValue = \case
      RowUID u    -> uidValue u
      RowNumber n -> natValue n

-- | A cell_block read from a value: a list of Named components, named 0x00
--   to 0x04 in order, Table a uid, startRow a uid or a uinteger, the rest
--   uintegers (5.1.4.2.3); or why the value is none, which fails the method
--   with INVALID_PARAMETER (5.1.5.11 d-e, PDF 139).
--
-- >>> cellBlock (cellBlockValue (columnRange 3 3)) == Right (columnRange 3 3)
-- True
cellBlock :: Value -> Either Text CellBlock
cellBlock = \case
  L (List vs) -> do
    cs <- first paramErrorText (namedComponents 5 vs)
    case cs of
      [table, startRow, endRow, startColumn, endColumn] ->
        CellBlock <$> traverse (uidOf "Table") table <*> traverse rowOf startRow <*> traverse (natOf "endRow") endRow
                  <*> traverse (natOf "startColumn") startColumn <*> traverse (natOf "endColumn") endColumn
      _ -> Left "a cell_block has five components (Core 5.1.4.2.3, PDF 130)"
  v -> Left ("a cell_block is a list of Named components, not " <> tshow v <> " (Core 5.1.4.2.3, PDF 129)")
  where
    uidOf name = \case
      D (Datum t) | Just u <- fromToken t -> Right (u :: UID)
      v -> Left (name <> " is not an eight-byte uid: " <> tshow v <> " (Core 5.1.4.2.3, PDF 129)")
    rowOf = \case
      D (Datum (Unsigned n)) -> Right (RowNumber n)
      D (Datum t) | Just u <- fromToken t -> Right (RowUID u)
      v -> Left ("startRow is neither a uid nor a uinteger row number: " <> tshow v <> " (Core 5.1.4.2.3 b, PDF 129)")
    natOf name = \case
      D (Datum (Unsigned n)) -> Right n
      v -> Left (name <> " is not a uinteger: " <> tshow v <> " (Core 5.1.4.2.3, PDF 129-130)")

uidValue :: UID -> Value
uidValue u = D (Datum (token u))

natValue :: Natural -> Value
natValue n = D (Datum (Unsigned n))


-- * What it asks of a target

-- | What a method with a cell_block is invoked on, and the bound its
--   components are held to: an object's or an object table's last column
--   number (0 for a table of one column), a byte table's last row number.
data CellTarget
  = OnObject !Int
  | OnObjectTable !Int
  | OnByteTable !Natural
  deriving (Eq, Show)

-- | The cells a cell_block asks of its target, every default applied.
data CellsAsked
  = Columns !Int !Int           -- ^ an object's columns, the first and the last
  | ObjectRow !UID !Int !Int    -- ^ an object table's row, the object's uid, and its columns, the first and the last
  | ByteRows !Natural !Natural  -- ^ a byte table's rows, the first and the last
  deriving (Eq, Show)

-- | The cells a cell_block asks of a target, by 5.1.4.2.3's context rules
--   (PDF 129-130), or why the method fails, naming the rule: a component
--   given where the target forbids it (a.b, a.c, b.c, c.b, d.b, e.b; Get's
--   Fails b and c, 5.3.3.6.3), startRow omitted on an object table (b.b), a
--   value past the target's last column or row, or a start past its end
--   (Fails d). Omitted, startRow is a byte table's first row (b.a), endRow
--   its last (c.a), startColumn the first column (d.a) and endColumn the
--   last (e.a). Table is refused on any Get (a.b, a.c).
resolveCells :: CellTarget -> CellBlock -> Either Text CellsAsked
resolveCells target cb = case target of
  OnObject lastColumn -> do
    omitted "Table" (cbTable cb) "an object (Core 5.1.4.2.3 a.b; 5.3.3.6.3 b)"
    omitted "startRow" (cbStartRow cb) "an object (Core 5.1.4.2.3 b.c; 5.3.3.6.3 b)"
    omitted "endRow" (cbEndRow cb) "an object (Core 5.1.4.2.3 c.b; 5.3.3.6.3 b)"
    uncurry Columns <$> columns lastColumn
  OnObjectTable lastColumn -> do
    omitted "Table" (cbTable cb) "a table, for Get (Core 5.1.4.2.3 a.c)"
    object <- case cbStartRow cb of
      Just (RowUID u)    -> Right u
      Just (RowNumber n) -> Left ("startRow " <> tshow n <> " is a row number where an object table takes the uid of the object "
                                  <> "(Core 5.1.4.2.3 b.b)")
      Nothing            -> Left "startRow is omitted where an object table requires the uid of the object (Core 5.1.4.2.3 b.b)"
    omitted "endRow" (cbEndRow cb) "an object table (Core 5.1.4.2.3 c.b)"
    (start, end) <- columns lastColumn
    pure (ObjectRow object start end)
  OnByteTable lastRow -> do
    omitted "Table" (cbTable cb) "a table, for Get (Core 5.1.4.2.3 a.c)"
    omitted "startColumn" (cbStartColumn cb) "a byte table (Core 5.1.4.2.3 d.b; 5.3.3.6.3 c)"
    omitted "endColumn" (cbEndColumn cb) "a byte table (Core 5.1.4.2.3 e.b; 5.3.3.6.3 c)"
    start <- case cbStartRow cb of
      Nothing            -> Right 0                                     -- b.a: the first row
      Just (RowNumber n) -> Right n
      Just (RowUID u)    -> Left ("startRow " <> tshow u <> " is a uid where a byte table takes a row number (Core 5.1.4.2.3 b)")
    let end = fromMaybe lastRow (cbEndRow cb)                           -- c.a: the last row
    within "startRow" start lastRow
    within "endRow" end lastRow
    ascending "startRow" start "endRow" end
    pure (ByteRows start end)
  where
    omitted name component what = case component of
      Nothing -> Right ()
      Just _  -> Left (name <> " is given where the method is invoked on " <> what)
    columns lastColumn = do
      let lastN = fromIntegral (max 0 lastColumn)
          start = fromMaybe 0 (cbStartColumn cb)                        -- d.a: the first column
          end   = fromMaybe lastN (cbEndColumn cb)                       -- e.a: the last column
      within "startColumn" start lastN
      within "endColumn" end lastN
      ascending "startColumn" start "endColumn" end
      pure (fromIntegral start, fromIntegral end)
    within name n lastN
      | n > lastN = Left (name <> " " <> tshow n <> " is past the last, " <> tshow lastN <> " (Core 5.3.3.6.3 d)")
      | otherwise = Right ()
    ascending startName start endName end
      | start > end = Left (startName <> " " <> tshow start <> " is past " <> endName <> " " <> tshow end <> " (Core 5.3.3.6.3 d)")
      | otherwise   = Right ()
