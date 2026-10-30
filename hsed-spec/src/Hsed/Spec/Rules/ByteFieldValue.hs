{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.ByteFieldValue
Description : The byteFieldValue rule: a structure printed as rows of byte ranges, each a field and its value
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4, example E7. @byteFieldValue@ fires on a table headed
@[Byte(s), Field, Value(s)]@ or, in two columns, @[Bytes, Values]@ (the
@ByteFieldValue@ shape, which picks out exactly these tables across Core, so
the rule rides on the @doc@ anchor, design 13.24). In Core 2.01 those are the
protocol 0x02 payloads of section 3.3.4.7 (Table 28 GET_COMID Payload, Table
31 No Response Available, Tables 32 and 33 VERIFY_COMID_VALID request and
response, Tables 35-37 STACK_RESET request, response and pending) and Table
236 System Log Entry Structure (section 5.8.4.2).

Each row gives its bytes, @0 to 3@, or a run to an end the text gives, @46 to
TRNSFLEN - 1@ (the rest of the transfer); a field; and its value. A table is
read as the byte grid it is (@Hsed.Spec.Readers.Grid@): its Byte(s) column the
corner, its Field column (two columns: its Values column) one cell over the
whole of each row's bytes. So the two rules read labels, name fields and
type them alike ("Hsed.Spec.Readers.Layout".'Hsed.Spec.Readers.Layout.gridLayout':
reserved by name, a flag at one bit, a byte string wider than eight bytes,
else an unsigned number; a field named as printed, a parenthesis it ends
with apart), and the run to the end is the
layout's tail, its length the rest of the input, reserved bytes when its name
says so. In two columns a field is named by the first sentence of its Values
cell (Table 236's @The uid of the method involved in the operation.@, its
@If there is no method, these bytes SHALL be zeroes.@ kept in the layout's
comment). Tables 31 and 37 print their first row's bytes in the BYTES
column, but the PDF's tags put them in the VALUE header cell (@VALUE@, @0 to
3@ its two lines) and leave the row two cells: the rule reads them as that
row's (@N-BYTES-IN-HEADER@, a Note).

A VALUE cell ('readValue') is hex bytes, as many as the field's (@00 00 00
01@), or a number (@0x0004@, or decimal digits not two to a word), which pins
the field (a 'Pinned' constant of "Hsed.Core.Layout"); hex bytes for the run to
the end are each of its bytes (@00@); anything else is prose (@Allocated
ComID@, @00 00 00 00 = Invalid, …@, @10 byte format – see Table 34.@), which
pins nothing and is kept, as printed, in the declaration's comment, as is
every value the layout does not hold. A reserved field takes no pin: its
value, if a number, must be zero.

Checks:

* @C-BYTES-ROW@ (Error): every row is its bytes, a field and (in three
  columns) a value, and at most one runs to an end the text gives.
* @C-BYTES-VALUE@ (Error): a value of hex bytes has as many as its field, and a
  reserved field's is zero.
* the byte grid's codes (@C-LAYOUT-EXTENT@: a row whose bytes do not read).
* @C-LAYOUT-TILE@: the fields cover each byte of the fixed part once and the
  run to the end starts where they end, by the one policy of the grid rules
  ("Hsed.Spec.Readers.Layout".'tileFindings'). Bytes no row names are a
  Warning: the layout is generated as printed, its comment naming them, with
  no record, and "Hsed.Core.Layout" refuses to encode or decode it
  (@NotTiled@) until an erratum names them. Table 236 prints @9 to 15@ after
  @0 to 7@ (PDF 298), byte 8 in no row, and which of its rows is misprinted
  is not for hsedc to guess (design 13.26 (a)). Any other tiling problem (an
  overlap, a pin its field cannot hold) is an Error, and the table is left
  out.

Compiled into the layouts module ('layoutsModule', which @byteGrid@ writes
too), named and printed as @byteGrid@ names and prints its grids, each table
read as a "Hsed.Spec.Readers.Record".'Structure' and printed by that
module's 'layoutValue', 'recordDecl' and 'nameFindings', so the two rules'
declarations follow one convention: for each table @\<name\>Layout ::
Layout@, the name its caption's title gives
(@verify_COMID_VALIDCommandResponseLayout@) or its @layout "Name"@ modifier,
printed by 'layoutDecl', its comment listing the values the layout does not
hold and the bytes no field covers; and, for a table whose layout tiles, the
record of the fields that carry a value with its @LayoutRecord@ instance
(@VERIFY_COMID_VALIDCommandResponse@, the run of reserved bytes to the end of
the transfer the layout's). A name the declarations share with @byteGrid@'s
or an earlier table's is @C-GRID-NAME@ (an Error).
-}
module Hsed.Spec.Rules.ByteFieldValue
  ( byteFieldValueRule
    -- * Reading the bound tables
  , BytesRead (..)
  , BytesRow (..)
  , Value (..)
  , bfLayout
  , bytesReads
  , readBytes
  , readValue
  , firstSentence
  ) where

import qualified Data.Text                as DT
import qualified RIO.List                 as L
import qualified RIO.Text                 as T
import           RIO

import           Hsed.Core.Layout         (Field (..), FieldKind (..), Layout (..))
import           Hsed.Spec.Anchor         (AnchorId (..))
import           Hsed.Spec.Common         (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.DSL            (SomeBinding (..), byteFieldValue)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (CellIR (..), TableIR (..), cellWithText)
import           Hsed.Spec.Readers.Grid   (Grid (..), GridField (..), gridFindings, readGrid)
import           Hsed.Spec.Readers.Layout (gridFieldKind, gridTiling, layoutDecl, layoutsModule, tileFindings, untiledNote)
import           Hsed.Spec.Readers.Record (Structure (..), declScope, earlierNames, layoutValue, nameFindings, recordDecl,
                                           recordNames, structure)
import           Hsed.Spec.Readers.Text   (readHexByte, readNumber)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.ByteGrid (gridDecls, gridStructures, structureName)
import           Hsed.Spec.Shape          (Shape (ByteFieldValue), captionTitle, shapeMismatch)


-- | The rule.
byteFieldValueRule :: RuleDef
byteFieldValueRule = RuleDef
  { ruleName    = RuleName "byteFieldValue"
  , bindsTo     = SomeBinding byteFieldValue
  , exemplar    = (SpecId "core-2.01", TableId 33)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch ByteFieldValue form)
  , ruleIdent   = const Nothing
  , ruleModule  = layoutsModule
  , ruleCodes   =
      [ CodeDef "C-BYTES-ROW"       [Error] (Defect "a row of a Byte(s) | Field | Value(s) table is not its bytes, a field and its value, or a second run to the end of the transfer")
      , CodeDef "C-BYTES-VALUE"     [Error] (Defect "a value printed as hex bytes, other than as many as its field's, or a reserved field's value other than zero")
      , CodeDef "N-BYTES-IN-HEADER" [Note]  (AsPrinted "a row's bytes read from the header cell the PDF's tags put them in, where the row prints them in its first column") ]
  , ruleChecks  = \ctx bts -> concatMap snd (checked ctx bts)
  , ruleCompile = compile
  }


-- * Reading the bound tables

-- | One table read under the errata set.
data BytesRead = BytesRead
  { bfBound     :: !BoundTable
  , bfRows      :: ![BytesRow]          -- ^ the rows that read as bytes, a field and a value
  , bfBadRows   :: ![(Int, [CellIR])]   -- ^ the rows of another number of cells (0-based)
  , bfHeader    :: !(Maybe CellIR)      -- ^ the header cell the first row's bytes were read from
  , bfGrid      :: !Grid                -- ^ the Byte(s) and Field columns read as a byte grid
  , bfStructure :: !Structure           -- ^ the grid's layout, the VALUE column's constants pinned, and its record's name
  }

-- | A table's layout.
bfLayout :: BytesRead -> Layout
bfLayout = stLayout . bfStructure

-- | One row: its bytes, its field, its value.
data BytesRow = BytesRow
  { bwIndex   :: !Int            -- ^ 0-based among the data rows
  , bwLabel   :: !CellIR         -- ^ the Byte(s) cell (or the header cell's lines that print it)
  , bwField   :: !CellIR         -- ^ the Field cell; in two columns, the Values cell, named by its first sentence
  , bwPrinted :: !Text           -- ^ the Field (Values) cell as printed
  , bwValue   :: !(Maybe Text)   -- ^ the VALUE cell, its spacing squeezed; Nothing in two columns
  , bwRow     :: ![CellIR]       -- ^ the row as printed, which locates a finding about it
  }

-- | A VALUE cell as the rule reads it, for a field of so many bytes (Nothing
--   for the run to the end of the transfer).
data Value
  = Exact !Integer    -- ^ hex bytes as many as the field's, a @0x@ number, or a decimal one: a constant
  | Fill ![Word8]     -- ^ hex bytes for the run to the end (@00@): each of its bytes
  | Misfit !Int       -- ^ hex bytes, this many, not as many as the field's
  | Printed !Text     -- ^ anything else, as printed: a name the text defines, a list of values, a format
  deriving (Eq, Show)

-- | Every table bound to the rule's shape, read under the errata set.
bytesReads :: Context -> [BytesRead]
bytesReads ctx = map readBytes (boundTables (Just ByteFieldValue) ctx)

-- | A table read: its rows; its Byte(s) and Field columns read as a byte grid,
--   each row one cell over its bytes; the grid's structure, each field the
--   VALUE column gives a constant pinned to it.
readBytes :: BoundTable -> BytesRead
readBytes bt = BytesRead
  { bfBound     = bt
  , bfRows      = good
  , bfBadRows   = [ (i, cells) | Left (i, cells) <- classified ]
  , bfHeader    = if moved then headerValue else Nothing
  , bfGrid      = grid
  , bfStructure = st { stConstants = constants }
  }
  where
    t = btRead bt
    title = fromMaybe "" (t.caption >>= captionTitle)
    header = concat (take 1 t.headerRows)
    threeColumns = length header == 3
    headerValue = listToMaybe (drop 2 header)
    -- the first row's bytes printed as the VALUE header cell's later lines (Tables 31 and 37)
    headerLabel = case headerValue of
      Just hv | ls@(_ : _) <- drop 1 hv.lines -> Just (cellWithText (T.unwords ls) hv)
      _                                        -> Nothing
    moved = case take 1 t.rows of
      [[_, _]] -> threeColumns && isJust headerLabel
      _        -> False
    classified = zipWith row [0 ..] t.rows
    good = rights classified
    row :: Int -> [CellIR] -> Either (Int, [CellIR]) BytesRow
    row i cells = case cells of
      [b, f, v] | threeColumns                          -> Right (BytesRow i b f f.text (Just (squeeze v.text)) cells)
      [f, v] | threeColumns, i == 0, Just l <- headerLabel -> Right (BytesRow i l f f.text (Just (squeeze v.text)) cells)
      [b, v] | not threeColumns                         -> Right (BytesRow i b (cellWithText (firstSentence v.text) v) v.text Nothing cells)
      _                                                 -> Left (i, cells)
    grid = readGrid [] t { headerRows = [take 2 header], rows = [ [bwLabel r, bwField r] | r <- good ], nCols = 2 }
    st = structure bt (fromMaybe (structureName title) (mLayout (btMods bt))) title grid pin
    pin (f, gf) = case (fieldKind f, rowOf good gf >>= bwValue) of
      (Reserved, _) -> f
      (_, Just v) | Exact n <- readValue (Just (fieldWidth f `div` 8)) v -> f { fieldKind = Pinned n }
      _ -> f
    constants = [ "the constants the table prints are the layout's: " <> T.intercalate ", " ps
                | let ps = [ fieldName f <> " " <> v | (f, gf) <- stFields st, Pinned _ <- [fieldKind f], Just r <- [rowOf good gf], Just v <- [bwValue r] ]
                , not (null ps) ]

-- | The first sentence of a cell, its full stop dropped: what names a field
--   in a two-column table, whose Values cell may go on (@The uid of the method
--   involved in the operation. If there is no method, these bytes SHALL be
--   zeroes.@ gives @The uid of the method involved in the operation@).
firstSentence :: Text -> Text
firstSentence cell = T.dropWhileEnd (== '.') (T.strip (fst (DT.breakOn ". " (squeeze cell))))

-- | The row a grid field was read from: the one whose field cell it is.
rowOf :: [BytesRow] -> GridField -> Maybe BytesRow
rowOf rows gf = L.find (\r -> bwField r `elem` gfCells gf) rows

-- | A VALUE cell, for a field of so many bytes (Nothing: the run to the end of
--   the transfer). Two hex digits to a word are bytes: as many as the field's,
--   a number, big-endian (@00 00 00 01@); for the run, each of its bytes
--   (@00@); any other count, a misfit. One word that is a number as
--   "Hsed.Spec.Readers.Text".'readNumber' reads it (@0x0004@, @1024@,
--   @0004h@, @C000h@) is that number. Anything else is prose, its spacing
--   squeezed.
--
-- >>> map (readValue (Just 4)) ["00 00 00 01", "0x0004", "1024", "00 00", "Allocated  ComID"]
-- [Exact 1,Exact 4,Exact 1024,Misfit 2,Printed "Allocated ComID"]
-- >>> map (readValue Nothing) ["00", "zero"]
-- [Fill [0],Printed "zero"]
readValue :: Maybe Int -> Text -> Value
readValue width cell
  | null ws = Printed ""
  | Just bs <- traverse readHexByte ws = case width of
      Nothing -> Fill bs
      Just n | length bs == n -> Exact (L.foldl' (\acc b -> acc * 256 + toInteger b) 0 bs)
             | otherwise      -> Misfit (length bs)
  | [w] <- ws, Just v <- readNumber w = Exact v
  | otherwise = Printed (T.unwords ws)
  where
    ws = T.words cell

squeeze :: Text -> Text
squeeze = T.unwords . T.words


-- * Checks

-- | Every bound table read, with its findings: its own, and @C-GRID-NAME@ for
--   a name its declarations share with the @byteGrid@ rule's grids or an
--   earlier table's (the one layouts module defines each once), or with what
--   the module imports; and for a name of a @byteGrid@ grid that its own
--   declarations import and @byteGrid@'s do not, which the @byteGrid@ rule,
--   below this one, cannot see (it checks its names against its own
--   declarations' imports).
checked :: Context -> [BoundTable] -> [(BytesRead, [Finding])]
checked ctx bts = [ (br, checkBytes ctx br <> names br <> gridNames br) | br <- brs ]
  where
    brs = map readBytes bts
    named = gridStructures ctx <> map bfStructure brs
    -- the one layouts module's imports: byteGrid's declarations' and these tables'
    scope = declScope (gridDecls ctx <> concatMap (decls ctx) brs)
    names br = nameFindings ctx (earlierNames named (bfStructure br)) scope (bfStructure br)
    gridScope = declScope (gridDecls ctx)
    gridNames br =
      [ atTable ctx (bfBound br) (finding Error "C-GRID-NAME" (n <> ", a name of byteGrid's " <> layoutName (stLayout st)
                                                               <> ", is also a name these declarations import"))
      | let here = declScope (decls ctx br), st <- gridStructures ctx, n <- L.nub (recordNames st), n `elem` here, n `notElem` gridScope ]

checkBytes :: Context -> BytesRead -> [Finding]
checkBytes ctx br = moved <> rowShape <> gridFindings ctx bt (bfGrid br) <> values <> tiles
  where
    bt = bfBound br
    rowLabel :: Int -> Text
    rowLabel i = "row " <> tshow (i + 1)
    moved = [ atCells ctx bt [hv] (finding Note "N-BYTES-IN-HEADER"
                (rowLabel 0 <> " has no bytes cell: its bytes " <> tshow (bwLabel r).text <> " are read from the header cell "
                 <> tshow (squeeze hv.text) <> ", whose later lines print them"))
            | Just hv <- [bfHeader br], r <- take 1 (bfRows br) ]
    rowShape =
      [ atCells ctx bt cells (finding Error "C-BYTES-ROW" (rowLabel i <> " has " <> tshow (length cells) <> " cells, not "
                                                             <> if columns == 3 then "its bytes, a field and a value" else "its bytes and a field"))
      | (i, cells) <- bfBadRows br ]
      <> [ atCells ctx bt (bwRow r) (finding Error "C-BYTES-ROW" (rowLabel (bwIndex r) <> ": the field " <> tshow (squeeze (bwField r).text) <> " has no value"))
         | r <- bfRows br, bwValue r == Just "" ]
      <> [ atCells ctx bt (gfCells gf) (finding Error "C-BYTES-ROW" ("a second run to an end the text gives, " <> tshow (fromMaybe "" (gfEnd gf))
                                                                       <> ": only one can follow the fixed part"))
         | gf <- drop 1 (runs br) ]
    columns = length (concat (take 1 (btRead bt).headerRows))
    values = concat [ valueFindings r gf (Just (fieldWidth f `div` 8)) | (f, gf) <- stFields (bfStructure br), Just r <- [rowOf (bfRows br) gf] ]
          <> concat [ valueFindings r gf Nothing | gf <- take 1 (runs br), Just r <- [rowOf (bfRows br) gf] ]
    valueFindings r gf width = case readValue width <$> bwValue r of
      Just (Misfit k) -> [ bad r (rowLabel (bwIndex r) <> ": " <> describe r <> " prints " <> tshow k <> " bytes for a field of " <> maybe "" tshow width) ]
      Just v | gridFieldKind gf == Reserved, nonzero v -> [ bad r (rowLabel (bwIndex r) <> ": " <> describe r <> " is reserved, its value not zero") ]
      _ -> []
    bad r = atCells ctx bt (bwRow r) . finding Error "C-BYTES-VALUE"
    nonzero = \case
      Exact n -> n /= 0
      Fill bs -> any (/= 0) bs
      _       -> False
    describe r = tshow (squeeze (bwField r).text) <> " (" <> squeeze (bwLabel r).text <> ")"
    tiles = tileFindings ctx bt (bfGrid br) (bfLayout br)

-- | The grid fields that run to an end the text gives; the first is the layout's tail.
runs :: BytesRead -> [GridField]
runs br = [ gf | gf <- gFields (bfGrid br), isNothing (gfLast gf) ]


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = concatMap one (checked ctx bts)
  where
    one (br, fs) = case L.nub [ Cm.code f | f <- fs, Cm.isError f ] of
      []    -> map Emit (decls ctx br)
      codes -> [ Excluded (btAnchor (bfBound br)) (T.intercalate ", " codes <> " failed") ]

-- | A table's declarations: @\<name\>Layout :: Layout@, its Haddock listing
--   the rows whose VALUE the layout does not hold, as printed, the Values
--   cells its field names shorten, and the bytes no field covers; and its
--   record when the layout tiles.
decls :: Context -> BytesRead -> [Gen.Decl]
decls ctx br =
  layoutDecl (boundProvenance ctx (bfBound br)) (layoutValue st) (bfLayout br) (values <> shortened <> untiledNote (gridTiling (bfGrid br) (bfLayout br)))
  : [ recordDecl ctx st | stTiles st ]
  where
    st = bfStructure br
    values = case unheld br of
      [] -> []
      rs -> [ "--", "-- The VALUE cells the layout does not hold (bytes | field | value, as printed):", "--" ]
            <> [ "-- > " <> T.intercalate " | " (map tshow [squeeze (bwLabel r).text, squeeze (bwField r).text, v]) | (r, v) <- rs ]
    shortened = case [ r | r <- bfRows br, isNothing (bwValue r), let n = (bwField r).text, squeeze (bwPrinted r) `notElem` [n, n <> "."] ] of
      [] -> []
      rs -> [ "--", "-- The Values cells whose first sentence names a field (bytes | values, as printed):", "--" ]
            <> [ "-- > " <> T.intercalate " | " (map tshow [squeeze (bwLabel r).text, squeeze (bwPrinted r)]) | r <- rs ]

-- | The rows whose value the layout does not hold: all but a pinned field's
--   constant and a reserved field's zero.
unheld :: BytesRead -> [(BytesRow, Text)]
unheld br = [ (r, v) | r <- bfRows br, Just v <- [bwValue r], not (held r) ]
  where
    held r = or [ case fieldKind f of
                    Pinned _ -> True
                    Reserved -> maybe False (zero . readValue (Just (fieldWidth f `div` 8))) (bwValue r)
                    _        -> False
                | (f, gf) <- stFields (bfStructure br), bwField r `elem` gfCells gf ]
    zero = \case
      Exact 0 -> True
      _        -> False
