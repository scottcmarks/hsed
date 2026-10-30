{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Readers.Grid
Description : The byte grid of a table: its rows' bytes, its cells' bits, its fields
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The reader of a byte grid (design section 3.4, examples E7 and E8), which the
@byteGrid@, @feature@ and @byteFieldValue@ rules share and no comment names.
A byte grid is a table whose first row is a corner cell then the bit columns
@7 … 0@ (Core T17, T18, T20, T21, T38, T39, T41-T44, T212; Opal T2-T6, T8;
@byteFieldValue@ reads its tables' Byte(s) and Field columns as one, each row
one cell over its bytes): each further row is one byte, a range of bytes, or a
variable run to an end the text gives, labelled in the corner column; its
other cells are fields over the bits under them.

* The corner: @Byte\\Bit@, or as "Hsed.Spec.Shape".'gridCornerNormalisation'
  reads it (Core T17 prints @Bit@ over a row @Byte@ alone, which is no byte).
* A row's bytes ('parseByteLabel'): @4@; @5 - 15@, @0 – 47@, @8 to 9@; @48 – n@,
  @20 to n+19@, @If Length > 0, 12 to (n+(-n modulo 4)) + 11@ (from a byte to
  an end the text gives); @…@ (rows a field's run leaves out). The label is the
  cell over the corner column, wherever the row prints it (Opal T2 prints some
  in its second cell).
* A row's cells' bits, by the extent rule of design E8 ('Mapping'): eight
  cells, positionally, bit 7 leftmost; one cell, the whole byte; any other
  count, each cell's drawn x-extent (IR @x_extent@) mapped onto the header's
  bit columns, a column under a cell when its centre lies within the cell's
  extent ('extentBits'); failing that, the @bits k [...]@ modifier of the
  byte (each listed bit a one-bit field of that name, the rest reserved),
  reported as @N-LAYOUT-BITS@; failing that, @C-LAYOUT-EXTENT@. The modifier
  names the cells the row prints and orders them as printed: each name it
  lists is a cell of the row, which names the field as printed; its bits
  descend from left to right; each is one of a byte's bits 7 to 0, listed
  once; a printed @Reserved@ cell lies over a bit it does not list, and each
  run of bits it does not list under a printed @Reserved@ or empty cell, where
  the row prints it among the named cells (before the first, between two,
  after the last: a map shifted by a bit leaves one under none); else
  @C-LAYOUT-EXTENT@.
* The fields ('GridField'), in order: a cell over some bits of one byte; a
  run from an @(MSB)@ mark to an @(LSB)@ mark, over the rows between (Core
  T17's @0 (MSB) Reserved … 3 (LSB)@, T212's @(MSB) IEEE COMPANY ID@ in bits
  3-0 of byte 4 to @IEEE COMPANY ID (LSB)@ in bits 7-4 of byte 7), named by
  the cell that names it, a mark with no partner being @C-LAYOUT-ENDIAN@; a
  byte range's one cell; a variable run's one cell, which has no last bit.
  Each field keeps its name as printed, the marks removed, and the
  parenthesis it ends with apart ('gfNote': @Length (n)@'s variable,
  @NAA (0x05)@'s constant, @(see Table 39)@, or the parts it names,
  @Session (0-3 = TSN, 4-7 = HSN)@, 'fieldParts'). A field whose name starts
  @Reserved@ (@Reserved for common SSC parameters@) is reserved bits
  ('reservedName'), the PDF's own word.
* The fixed part's size ('gSize'): one past the last byte a row prints (a
  byte, a range, the start of a variable run); bytes it prints that no field
  names are a gap for the layout's tiling check, not a shorter layout.

The reader returns data and the problems it found ('GridProblem', each with
its code and cells); the rules locate them ('gridFindings') and emit them.
Its codes are its own ('gridCodes'), whichever rule reads the grid.
-}
module Hsed.Spec.Readers.Grid
  ( -- * A table's byte grid
    Grid (..)
  , ByteRow (..)
  , ByteSpan (..)
  , Mapping (..)
  , CellBits (..)
  , GridField (..)
  , BitPos (..)
  , GridProblem (..)
  , readGrid
  , gridOf
  , gridFindings
    -- * Pieces
  , parseByteLabel
  , extentBits
  , cellMarks
  , fieldConstant
  , fieldVariable
  , fieldParts
  , reservedName
    -- * Codes
  , gridCodes
  ) where

import qualified Data.Text              as DT
import qualified Data.Text.Read         as TR
import qualified RIO.Char               as C
import qualified RIO.List               as L
import qualified RIO.Text               as T
import           RIO

import           Hsed.Spec.Common       (CodeDef (..), Finding, Reports (..), Severity (..), finding)
import           Hsed.Spec.IR           (CellIR (..), TableIR (..))
import           Hsed.Spec.Readers.Text (nameKey, readNumber)
import           Hsed.Spec.RuleDef      (BoundTable (..), Context, Mods (..), atCells, atTable)
import           Hsed.Spec.Shape        (gridCornerNormalisation)


-- * A table's byte grid

-- | A bit as a grid prints it: a byte, and a bit of it, 7 the most significant.
data BitPos = BitPos { bpByte :: !Int, bpBit :: !Int }
  deriving (Eq, Ord, Show)

-- | The bytes a row's label gives.
data ByteSpan
  = OneByte !Int          -- ^ @4@
  | ByteRange !Int !Int   -- ^ @5 - 15@, @0 – 47@, @8 to 9@
  | FromByte !Int !Text   -- ^ @48 – n@, @If Length > 0, 20 to n + 19@: from a byte to an end the text gives (the label)
  | Elided                -- ^ @…@: rows a field's run leaves out
  deriving (Eq, Show)

-- | How a row's cells were mapped to bits (design E8's extent rule).
data Mapping
  = Positional   -- ^ eight cells, bit 7 leftmost
  | WholeByte    -- ^ one cell, the whole byte (or bytes)
  | ByExtent     -- ^ each cell by its drawn x-extent over the header's bit columns
  | ByModifier   -- ^ the byte's @bits k [...]@ modifier
  deriving (Eq, Show)

-- | A cell of a row, its text as printed and the bits under it, @(hi, lo)@;
--   Nothing for a cell the rule maps to no bits (an empty cell without an
--   extent, or one a failed mapping leaves).
data CellBits = CellBits
  { cbText :: !Text
  , cbBits :: !(Maybe (Int, Int))
  , cbCell :: !CellIR
  }
  deriving (Eq, Show)

-- | One row after the header.
data ByteRow = ByteRow
  { brLabel   :: !Text              -- ^ the corner column's cell, as printed
  , brSpan    :: !(Maybe ByteSpan)  -- ^ Nothing when the label does not read
  , brMapping :: !Mapping
  , brCells   :: ![CellBits]        -- ^ its other cells, left to right
  , brBits    :: ![(Int, Text)]     -- ^ under 'ByModifier', the modifier's bits and names
  , brRow     :: ![CellIR]          -- ^ the row as printed
  }
  deriving (Eq, Show)

-- | One field of the grid.
data GridField = GridField
  { gfName  :: !Text            -- ^ as printed, the @(MSB)@ and @(LSB)@ marks removed: @Length (n)@
  , gfBase  :: !Text            -- ^ the name without the parenthesis it ends with: @Length@
  , gfNote  :: !(Maybe Text)    -- ^ that parenthesis' text: @n@, @0x05@, @see Table 39@
  , gfFirst :: !BitPos          -- ^ its most significant bit
  , gfLast  :: !(Maybe BitPos)  -- ^ its least; Nothing for a run to an end the text gives
  , gfEnd   :: !(Maybe Text)    -- ^ for such a run, its row's label as printed
  , gfCells :: ![CellIR]        -- ^ where it is printed
  }
  deriving (Eq, Show)

-- | What the reader found wrong, as data: the rules emit it ('gridFindings').
data GridProblem = GridProblem
  { gpSeverity :: !Severity
  , gpCode     :: !Text
  , gpCells    :: ![CellIR]  -- ^ where (none: the table as a whole)
  , gpMessage  :: !Text
  }
  deriving (Eq, Show)

-- | A table's byte grid.
data Grid = Grid
  { gCorner   :: !Text           -- ^ the corner cell as printed
  , gRows     :: ![ByteRow]
  , gFields   :: ![GridField]    -- ^ in order of their first bit
  , gSize     :: !Int            -- ^ the fixed part's bytes: one past the last byte a row prints or a field covers, or where a variable run starts
  , gProblems :: ![GridProblem]
  }
  deriving (Eq, Show)

-- | The grid of a bound table, read under the errata set, with the bit maps
--   its @bits k [...]@ modifiers declare.
gridOf :: BoundTable -> Grid
gridOf bt = readGrid (mBits (btMods bt)) (btRead bt)

-- | The grid's problems as findings, each at the page its first cell is
--   printed on (at the table when it has none).
gridFindings :: Context -> BoundTable -> Grid -> [Finding]
gridFindings ctx bt g =
  [ (if null (gpCells p) then atTable ctx bt else atCells ctx bt (gpCells p)) (finding (gpSeverity p) (gpCode p) (gpMessage p))
  | p <- gProblems g ]

-- | Read a table as a byte grid, given the bit maps of its @bits k [...]@
--   modifiers (byte, [(bit, name)]).
readGrid :: [(Int, [(Int, Text)])] -> TableIR -> Grid
readGrid bitMaps t = Grid
  { gCorner   = maybe "" (.text) (listToMaybe header)
  , gRows     = rows
  , gFields   = fields
  , gSize     = foldl' max 0 ([ bpByte l + 1 | f <- fields, Just l <- [gfLast f] ] <> mapMaybe (printedEnd . brSpan) rows)
  , gProblems = assembled <> unused
  }
  where
    (header, body) = case t.headerRows <> t.rows of
      h : rest -> (h, drop (if maybe False (isJust . snd) (gridCornerNormalisation t) then 1 else 0) rest)
      []       -> ([], [])
    columns = zip [7, 6 .. 0] (map (.xExtent) (drop 1 header))
    corner  = listToMaybe header >>= (.xExtent)
    rows    = map (readRow bitMaps columns corner) body
    (fields, assembled) = assemble rows
    -- a bits modifier whose byte no row read by it
    unused =
      [ GridProblem Warning "W-LAYOUT-BITS-UNUSED" []
          ("the bits modifier of byte " <> tshow k <> " maps nothing: " <> why k)
      | (k, _) <- bitMaps, k `notElem` [ n | r <- rows, brMapping r == ByModifier, Just (OneByte n) <- [brSpan r] ] ]
    why k
      | any (\r -> brSpan r == Just (OneByte k)) rows = "the extent rule reads that byte's cells"
      | otherwise                                    = "no row is that byte alone"
    -- one past the bytes a row prints, or where a variable run starts
    printedEnd = \case
      Just (OneByte n)     -> Just (n + 1)
      Just (ByteRange _ b) -> Just (b + 1)
      Just (FromByte a _)  -> Just a
      _                    -> Nothing

-- | One row: its label (the cell over the corner column, else its first),
--   the bytes it gives and its other cells' bits.
readRow :: [(Int, [(Int, Text)])] -> [(Int, Maybe (Double, Double))] -> Maybe (Double, Double) -> [CellIR] -> ByteRow
readRow bitMaps columns corner cells = ByteRow
  { brLabel   = label
  , brSpan    = byteSpan
  , brMapping = mapping
  , brCells   = mapped
  , brBits    = if mapping == ByModifier then fromMaybe [] modifier else []
  , brRow     = cells }
  where
    labelAt = fromMaybe 0 (L.findIndex inCorner cells)
    inCorner :: CellIR -> Bool
    inCorner c = case (c.xExtent, corner) of
      (Just (x0, x1), Just (c0, c1)) -> let m = (x0 + x1) / 2 in c0 <= m && m <= c1
      _                              -> False
    (before, rest) = L.splitAt labelAt cells
    (label, others) = case rest of
      l : after -> (T.strip l.text, before <> after)
      []        -> ("", before)
    byteSpan = parseByteLabel label
    byExtent = [ CellBits (T.strip c.text) (c.xExtent >>= extentBits columns) c | c <- others ]
    named = filter (not . T.null . cbText)
    modifier = case byteSpan of
      Just (OneByte n) -> L.lookup n bitMaps
      _                -> Nothing
    (mapping, mapped) = case (length others, modifier) of
      (8, _) -> (Positional, zipWith (\b c -> CellBits (T.strip c.text) (Just (b, b)) c) [7, 6 .. 0] others)
      (1, _) -> (WholeByte, [ CellBits (T.strip c.text) (Just (7, 0)) c | c <- others ])
      (_, Just ps)
        | any (isNothing . cbBits) (named byExtent)
          -> (ByModifier, [ cb { cbBits = (\b -> (b, b)) <$> L.lookup (nameKey (cbText cb)) [ (nameKey nm, b) | (b, nm) <- ps ] } | cb <- byExtent ])
      _ -> (ByExtent, byExtent)

-- | The bit columns under a drawn extent: those whose header cell's centre lies
--   within it, as @(hi, lo)@ (Core T43 byte 4's Reserved, drawn over the
--   columns of bits 7 and 6, gives @(7, 6)@); Nothing when none does.
extentBits :: [(Int, Maybe (Double, Double))] -> (Double, Double) -> Maybe (Int, Int)
extentBits columns (x0, x1) = case [ b | (b, Just (c0, c1)) <- columns, let m = (c0 + c1) / 2, x0 < m, m < x1 ] of
  b : bs -> Just (foldl' max b bs, foldl' min b bs)
  []     -> Nothing

-- | What a row's label says of its bytes: @4@ is one byte; @5 - 15@, @0 – 47@,
--   @8 to 9@ a range; @48 – n@ and @If Length > 0, 20 to n + 19@ a run from
--   byte 48 or 20 to an end the text gives; @…@ rows a run leaves out; Nothing
--   for anything else (@Byte@).
parseByteLabel :: Text -> Maybe ByteSpan
parseByteLabel raw
  | label `elem` ["…", "..."] = Just Elided
  | otherwise = do
      (a, rest) <- number (T.stripStart unconditioned)
      case T.strip rest of
        "" -> Just (OneByte a)
        r  -> do
          after <- listToMaybe (mapMaybe (`T.stripPrefix` r) ["to ", "- ", "– ", "— ", "-", "–", "—"])
          case number (T.strip after) of
            Just (b, more) | T.null (T.strip more) -> Just (ByteRange a b)
            _                                      -> Just (FromByte a label)
  where
    label = T.unwords (T.words raw)
    -- "If Length > 0, 20 to n + 19": the condition before the bytes
    unconditioned = if "If " `T.isPrefixOf` label then T.drop 2 (snd (DT.breakOn ", " label)) else label
    number t = either (const Nothing) Just (TR.decimal t)

-- | A cell's text split at its marks: whether it starts @(MSB)@, the name
--   between, whether it ends @(LSB)@ (@(MSB) IEEE COMPANY ID@ gives @(True,
--   "IEEE COMPANY ID", False)@, a lone @(LSB)@ @(False, "", True)@).
cellMarks :: Text -> (Bool, Text, Bool)
cellMarks t0 = (isJust msb, T.strip (fromMaybe afterMsb (T.stripSuffix "(LSB)" afterMsb)), "(LSB)" `T.isSuffixOf` afterMsb)
  where
    t = T.unwords (T.words t0)
    msb = T.stripPrefix "(MSB)" t
    afterMsb = T.strip (fromMaybe t msb)

-- | A field's constant, printed as its name (Core T212's @0x02@) or in the
--   parenthesis its name ends with (@NAA (0x05)@, Opal's @Feature Code (0x0001)@).
fieldConstant :: GridField -> Maybe Integer
fieldConstant f = (gfNote f >>= readNumber) <|> readNumber (gfBase f)

-- | The parts a field's parenthesis names, each a run of its bytes and a
--   name: @Session (0-3 = TSN, 4-7 = HSN)@ gives @[(0, 3, "TSN"), (4, 7,
--   "HSN")]@ (Core T18, whose Session §3.2.3.3.1.1 reads as two numbers, the
--   TPer's first). Nothing for a parenthesis of another form.
fieldParts :: GridField -> Maybe [(Int, Int, Text)]
fieldParts f = gfNote f >>= \note -> case traverse part (DT.splitOn "," note) of
  Just ps@(_ : _ : _) -> Just ps
  _                   -> Nothing
  where
    part p = case DT.breakOn "=" p of
      (range, rest) | Just name <- T.stripPrefix "=" rest, not (T.null (T.strip name)) -> do
        ByteRange a b <- parseByteLabel (T.strip range)
        pure (a, b, T.strip name)
      _ -> Nothing

-- | The variable a field's parenthesis names (@Length (n)@ gives @n@), which a
--   variable run's end refers to.
fieldVariable :: GridField -> Maybe Text
fieldVariable f = case gfNote f of
  Just v | T.length v == 1, T.all C.isLower v -> Just v
  _                                          -> Nothing


-- * The fields

-- | A run of rows from an @(MSB)@ mark, not yet closed by its @(LSB)@.
data Run = Run
  { runName  :: !(Maybe Text)
  , runFirst :: !BitPos
  , runCells :: ![CellIR]
  }

-- | The fields of the rows, in order, and what does not assemble.
assemble :: [ByteRow] -> ([GridField], [GridProblem])
assemble rows = (reverse fs, reverse ps <> unclosed)
  where
    (open, fs, ps) = foldl' step (Nothing, [], []) (zip [1 :: Int ..] rows)
    unclosed = [ endian (runCells r) ("the (MSB) of " <> describe r <> " at byte " <> tshow (bpByte (runFirst r)) <> " has no (LSB)") | Just r <- [open] ]
    describe r = maybe "a field with no name" tshow (runName r)
    step (run, done, probs) (i, row) = case brSpan row of
      Nothing -> (run, done, extent (brRow row) ("row " <> tshow i <> ": the byte number " <> tshow (brLabel row) <> " does not read") : probs)
      Just Elided
        | isJust run -> (run, done, probs)
        | otherwise  -> (run, done, extent (brRow row) ("row " <> tshow i <> ": " <> tshow (brLabel row) <> " lies in no field's run of rows") : probs)
      Just (ByteRange a b) -> whole i row run done probs (BitPos a 7) (Just (BitPos b 0)) Nothing
      Just (FromByte a _)  -> whole i row run done probs (BitPos a 7) Nothing (Just (brLabel row))
      Just (OneByte n)
        | brMapping row == ByModifier -> modified n row run done probs
        | otherwise                   -> foldl' (cell n) (run, done, probs) (L.sortOn (fmap (negate . fst) . cbBits) (named row))
    named row = filter (not . T.null . cbText) (brCells row)
    -- a row that is one field: a byte range, or a variable run
    whole i row run done probs from lastBit end = case named row of
      [cb] | isNothing run -> (run, field (cbText cb) from lastBit end [cbCell cb] : done, probs)
      cbs  -> (run, done, extent (brRow row) ("row " <> tshow i <> " (" <> brLabel row <> "): " <> tshow (length cbs)
                                              <> " named cells" <> (if isJust run then " inside a field's run" else "") <> ", not one field") : probs)
    -- a byte read by its bits modifier: its listed bits, each named as its cell prints it, the rest reserved; the
    -- modifier holds to the row: each name a printed cell, its bits descending left to right as the row prints
    -- them, each a bit of a byte listed once, a printed Reserved cell over a bit it leaves reserved, and each run of
    -- bits it leaves reserved under a printed Reserved or empty cell
    modified n row run done probs =
      let listed = brBits row
          cells = named row
          bitOf cb = fst <$> cbBits cb
          printedAs nm = listToMaybe [ cb | cb <- cells, nameKey (cbText cb) == nameKey nm ]
          flags = [ field (maybe nm cbText cb) (BitPos n b) (Just (BitPos n b)) Nothing (map cbCell (maybeToList cb))
                  | (b, nm) <- listed, let cb = printedAs nm ]
          unlisted = [ b | b <- [7, 6 .. 0], b `notElem` map fst listed ]
          reserved = [ field "Reserved" (BitPos n hi) (Just (BitPos n lo)) Nothing [] | (hi, lo) <- runsOf unlisted ]
          byte = "byte " <> tshow n <> ": "
          strays = [ extent [cbCell cb] (byte <> tshow (cbText cb) <> " is none of the bits its bits modifier names")
                   | cb <- cells, isNothing (cbBits cb), not (reservedName (cbText cb)) ]
          unprinted = [ extent (brRow row) (byte <> "its bits modifier names " <> tshow nm <> " (bit " <> tshow b <> "), which no cell of the row prints")
                      | (b, nm) <- listed, isNothing (printedAs nm) ]
          outside = [ extent (brRow row) (byte <> "its bits modifier lists bit " <> tshow b <> " (" <> tshow nm <> "), not one of a byte's bits 7 to 0")
                    | (b, nm) <- listed, b < 0 || b > 7 ]
          twice = [ extent (brRow row) (byte <> "its bits modifier lists bit " <> tshow b <> " " <> tshow (length nms) <> " times (" <> T.intercalate ", " nms <> ")")
                  | b <- L.nub (map fst listed), let nms = [ nm | (b', nm) <- listed, b' == b ], length nms > 1 ]
          -- the named cells' bits, left to right, must descend
          ordered = [ (cbText cb, b) | cb <- cells, Just b <- [bitOf cb] ]
          misordered = [ extent (map cbCell cells) (byte <> "its bits modifier gives " <> tshow a <> " bit " <> tshow ba <> " and " <> tshow c
                                                     <> " bit " <> tshow bc <> ", against the order the row prints them, bit 7 leftmost")
                       | ((a, ba), (c, bc)) <- zip ordered (drop 1 ordered), ba <= bc ]
          -- a Reserved cell lies between the named cells beside it: over a bit between theirs that the modifier leaves
          reservedCells = [ (i, cb) | (i, cb) <- zip [0 :: Int ..] cells, isNothing (cbBits cb), reservedName (cbText cb) ]
          bitsBeside i = ( fromMaybe 8 (L.lastMaybe [ b | (j, cb) <- zip [0 ..] cells, j < i, Just b <- [bitOf cb] ])
                         , fromMaybe (-1) (listToMaybe [ b | (j, cb) <- zip [0 ..] cells, j > i, Just b <- [bitOf cb] ]) )
          overReserved = [ extent [cbCell cb] (byte <> "the cell " <> tshow (cbText cb) <> " lies where its bits modifier leaves no bit reserved")
                         | (i, cb) <- reservedCells, let (hi, lo) = bitsBeside i, not (any (\u -> lo < u && u < hi) unlisted) ]
          -- and conversely, each run of bits the modifier leaves reserved lies under a Reserved or empty cell printed where
          -- the run lies among the named cells: between the cell of the listed bit above it (or the row's start) and the cell
          -- of the listed bit below it (or the row's end); a map shifted by one bit leaves a bit at an end under no cell
          printed = zip [0 :: Int ..] (brCells row)
          cellOf b = listToMaybe [ i | (i, cb) <- printed, fmap fst (cbBits cb) == Just b ]
          filler cb = isNothing (cbBits cb) && (T.null (cbText cb) || reservedName (cbText cb))
          bitsText hi lo = if hi == lo then "bit " <> tshow hi else "bits " <> tshow hi <> "-" <> tshow lo
          underNone =
            [ extent (brRow row) (byte <> "its bits modifier leaves " <> bitsText hi lo <> " reserved where the row prints no Reserved or empty cell")
            | null misordered
            , (hi, lo) <- runsOf unlisted
            , let after = fromMaybe (-1) (listToMaybe (mapMaybe cellOf (filter (> hi) [0 .. 7])))
                  before = fromMaybe (length printed) (listToMaybe (mapMaybe cellOf (filter (< lo) [7, 6 .. 0])))
            , not (any (\(i, cb) -> after < i && i < before && filler cb) printed) ]
          note = GridProblem Note "N-LAYOUT-BITS" (brRow row)
                   ("byte " <> tshow n <> "'s bits read from its bits modifier, the extent rule reading none: "
                    <> T.intercalate ", " [ tshow b <> " " <> nm | (b, nm) <- listed ])
          inRun = [ endian (brRow row) ("byte " <> tshow n <> ", read by its bits modifier, lies inside a field's run") | isJust run ]
          found = strays <> unprinted <> outside <> twice <> misordered <> overReserved <> underNone <> inRun
      in (run, reverse (L.sortOn (inByte . gfFirst) (flags <> reserved)) <> done, reverse (note : found) <> probs)
    -- bit 7 first, as the other mappings give a byte's fields
    inByte (BitPos b i) = (b, negate i)
    -- one cell of a byte, left to right
    cell n (run, done, probs) cb = case (cbBits cb, cellMarks (cbText cb)) of
      (Nothing, _) -> (run, done, extent [cbCell cb] ("byte " <> tshow n <> ": the cell " <> tshow (cbText cb)
                                                       <> " lies over none of the header's bit columns") : probs)
      (Just (hi, lo), (msb, name, lsb)) -> case (msb, lsb, run) of
        (True, True, Nothing) -> (Nothing, field name (BitPos n hi) (Just (BitPos n lo)) Nothing [cbCell cb] : done, probs)
        (True, _, Just r)     -> (run, done, endian [cbCell cb] ("byte " <> tshow n <> ": an (MSB) inside the run of " <> describe r) : probs)
        (True, False, Nothing) -> (Just (Run (nonEmpty name) (BitPos n hi) [cbCell cb]), done, probs)
        (False, True, Just r) -> case (runName r, nonEmpty name) of
          (Just a, Just b) | nameKey a /= nameKey b ->
            (Nothing, done, endian (runCells r <> [cbCell cb]) ("byte " <> tshow n <> ": the (LSB) of " <> tshow b <> " closes the run of " <> tshow a) : probs)
          (named', other) -> (Nothing, field (fromMaybe "" (named' <|> other)) (runFirst r) (Just (BitPos n lo)) Nothing (runCells r <> [cbCell cb]) : done, probs)
        (False, True, Nothing) -> (Nothing, done, endian [cbCell cb] ("byte " <> tshow n <> ": an (LSB) with no (MSB) before it") : probs)
        (False, False, Just r) -> case runName r of
          Nothing -> (Just r { runName = Just name, runCells = runCells r <> [cbCell cb] }, done, probs)
          Just a | nameKey a == nameKey name -> (Just r { runCells = runCells r <> [cbCell cb] }, done, probs)
                 | otherwise -> (run, done, endian [cbCell cb] ("byte " <> tshow n <> ": " <> tshow name <> " inside the run of " <> tshow a) : probs)
        (False, False, Nothing) -> (Nothing, field name (BitPos n hi) (Just (BitPos n lo)) Nothing [cbCell cb] : done, probs)
    nonEmpty x = if T.null x then Nothing else Just x
    extent cells = GridProblem Error "C-LAYOUT-EXTENT" cells
    endian cells = GridProblem Error "C-LAYOUT-ENDIAN" cells

-- | A field from its name as printed (marks removed).
field :: Text -> BitPos -> Maybe BitPos -> Maybe Text -> [CellIR] -> GridField
field name from lastBit end cells = GridField
  { gfName = name, gfBase = base, gfNote = note, gfFirst = from, gfLast = lastBit, gfEnd = end, gfCells = cells }
  where
    (base, note) = case T.stripSuffix ")" name of
      Just open | (b, p) <- DT.breakOnEnd "(" open, not (T.null b) -> (T.strip (T.dropEnd 1 b), Just (T.strip p))
      _                                                             -> (name, Nothing)

-- | A name that says its bits are reserved: a field's (@Reserved@, @Reserved
--   for common SSC parameters@), or a variable run's (the reserved bytes to the
--   end of a transfer).
reservedName :: Text -> Bool
reservedName = T.isPrefixOf "reserved" . T.toLower

-- | The runs of consecutive bits, high to low: @[7, 6, 4]@ gives @[(7, 6), (4, 4)]@.
runsOf :: [Int] -> [(Int, Int)]
runsOf = foldr place []
  where
    place b ((hi, lo) : rest) | b == hi + 1 = (b, lo) : rest
    place b acc                             = (b, b) : acc


-- * Codes

-- | The codes of what reading a grid finds, whichever rule reads it.
gridCodes :: [CodeDef]
gridCodes =
  [ CodeDef "C-LAYOUT-EXTENT"      [Error]   (Defect "a byte grid's row whose bytes or bits do not read: its label is no byte number, or its cells are neither eight, one, drawn over the header's bit columns nor named by a bits modifier")
  , CodeDef "C-LAYOUT-ENDIAN"      [Error]   (Defect "a field's (MSB) and (LSB) marks disagree: one without the other, or each naming another field")
  , CodeDef "N-LAYOUT-BITS"        [Note]    Bookkeeping
  , CodeDef "W-LAYOUT-BITS-UNUSED" [Warning] Bookkeeping ]
