{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.Ranges
Description : The ranges rule: the LSB ranges reserved per category, and C-UID-RANGE
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4. @ranges@ fires on a three-column table headed @[\<X\> Name,
… Start, … End]@: in Core 2.01 Tables 237 and 238 alone, so the rule rides on
the @doc@ anchor (design 13.24). Each row reserves a range of the lower four
bytes of a UID (its LSB) to a category the first column names; the first
header says what the categories are (Core section 6.2): @Template Name@ (the
rows of the Table and MethodID tables by template, Table 237) or @Type Format
Name@ (the Type objects by their Format, Table 238). A row whose one value
cell spans both columns reserves that one value (Table 238's last row,
@Unassigned 00 00 00 00@), the Note @N-RANGE-ONE-CELL@.

Checks, each with the code its findings carry:

* @C-UID-RANGE@ on the range tables: every value is four bytes, every range
  starts at or below its end, and no two ranges of a table overlap;
* @C-UID-RANGE@ against a template range table: every row that names a
  template, in Table 240 (the descriptor object's UID: its row of the Table
  table, "Hsed.Spec.Rules.TableUIDs") and in the UID tables
  ("Hsed.Spec.Rules.UidTable": the MethodID and Authority UIDs), names a
  template the table gives a range, and its UID's LSB lies in that range; the
  rows that name no template are listed once per table, with the range their
  LSB lies in (@N-UID-RANGE-NO-TEMPLATE@);
* @C-UID-RANGE@ against a type format range table: every type row
  ("Hsed.Spec.Rules.TypeRow") names in its Format a type format the table
  gives a range, and its UID's LSB lies in it. A format is found by its name
  as the Format prints it (@Restricted_Reference_Type{6}@), else by its name
  without the code in braces (@General_Reference_Type@ for codes 7, 8 and 9);
  a range name read with its spaces as underscores (@Struct Type@ for
  @Struct_Type@) is the Note @N-RANGE-NAME-NORMALISED@;
* @N-RANGE-UNCHECKED@: a range table whose first header names categories no
  rows are checked against (none in Core 2.01).

Compiled into 'rangesModule': per table, @\<x\>Ranges :: [(String, (Word32,
Word32))]@, each category with its first and last LSB, in the table's order
(a list, not a map: a category such as @Unassigned@ can hold several ranges),
named after the first header (@templateRanges@, @typeFormatRanges@).
-}
module Hsed.Spec.Rules.Ranges
  ( rangesRule
  , rangesModule
    -- * Reading the bound tables
  , RangeRow (..)
  , rangeRows
  , rangeKey
  , lsb
  ) where

import qualified RIO.List                  as L
import qualified RIO.Text                  as T
import           RIO

import           Hsed.Spec.Anchor          (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common          (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import           Hsed.Spec.DSL             (SomeBinding (..), ranges)
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.IR              (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..), tableUIDRows)
import           Hsed.Spec.Rules.TypeRow   (TypeRowRead (..), typeRowReads)
import           Hsed.Spec.Rules.UidTable  (UidRow (..), UidValue (..), lowerIdent, rowText, uidRows)
import           Hsed.Spec.Shape           (Shape (..), normHeader, normaliseCaption, shapeMismatch)


-- | The rule.
rangesRule :: RuleDef
rangesRule = RuleDef
  { ruleName    = RuleName "ranges"
  , bindsTo     = SomeBinding ranges
  , exemplar    = (SpecId "core-2.01", TableId 238)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch Ranges form)
  , ruleIdent   = const Nothing
  , ruleModule  = rangesModule
  , ruleChecks  = checks
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.UIDRanges@.
rangesModule :: Gen.GenModule
rangesModule = Gen.GenModule
  { Gen.leaf        = "UIDRanges"
  , Gen.description = "UID ranges: the LSB ranges reserved per template and per type format"
  , Gen.notes       = [ "Each range is a category's first and last value of the lower four bytes of a UID, in"
                      , "the table's order." ] }


-- * Reading the bound tables

-- | One row of a range table, read under the errata set.
data RangeRow = RangeRow
  { rgBound   :: !BoundTable
  , rgRow     :: !Int              -- ^ 0-based among the data rows
  , rgCells   :: ![CellIR]
  , rgName    :: !Text             -- ^ the category, as printed
  , rgStart   :: !(Maybe Word32)
  , rgEnd     :: !(Maybe Word32)
  , rgOneCell :: !Bool             -- ^ one value cell spans Start and End
  }

-- | What a range table's categories are: its first header without @Name@,
--   @Template@ or @Type Format@.
rangeKey :: BoundTable -> Text
rangeKey bt = fromMaybe first' (T.stripSuffix " Name" first')
  where first' = maybe "" (\c -> T.unwords (T.words c.text)) (listToMaybe (btRead bt).headerRows >>= listToMaybe)

-- | Every row of every table bound to the rule's shape.
rangeRows :: Context -> [RangeRow]
rangeRows ctx = concatMap tableRows (boundTables (Just Ranges) ctx)

tableRows :: BoundTable -> [RangeRow]
tableRows bt = zipWith row [0 ..] (btRead bt).rows
  where
    row i cells = case map (\c -> T.unwords (T.words c.text)) cells of
      [n, s, e] -> RangeRow bt i cells n (value s) (value e) False
      [n, v]    -> RangeRow bt i cells n (value v) (value v) True
      ns        -> RangeRow bt i cells (T.intercalate " | " ns) Nothing Nothing False
    value t = case T.words t of
      ws@[_, _, _, _] -> foldl' (\acc b -> acc * 256 + fromIntegral b) 0 <$> traverse hexByte ws
      _               -> Nothing
    hexByte w = case parseUIDCell (T.replicate 7 "00" <> w) of
      Just bs | T.length w == 2 -> listToMaybe (reverse bs)
      _                         -> Nothing

-- | The LSB of a UID: its lower four bytes.
lsb :: [Word8] -> Word32
lsb = foldl' (\acc b -> acc * 256 + fromIntegral b) 0 . drop 4

hex32 :: Word32 -> Text
hex32 w = "0x" <> T.concat [ T.drop 2 (Gen.hexByte (fromIntegral (w `div` (256 ^ k) `mod` 256))) | k <- [3, 2, 1, 0 :: Int] ]

spaced :: Word32 -> Text
spaced w = hexUID [ fromIntegral (w `div` (256 ^ k) `mod` 256) | k <- [3, 2, 1, 0 :: Int] ]


-- * Checks

-- | A row another table gives a category and a UID: where it is, what it says.
data Subject = Subject
  { sBound    :: !BoundTable
  , sLabel    :: !Text
  , sCategory :: !(Maybe Text)
  , sLSB      :: !Word32
  }

checks :: Context -> [BoundTable] -> [Finding]
checks ctx bts = concatMap tableChecks bts
  where
    tableChecks bt = let rs = tableRows bt in concatMap (rowChecks bt) rs <> overlaps bt rs <> members bt rs
    rowChecks bt r = map (atTable ctx bt) $
      [ finding Error "C-UID-RANGE" (rowLabel r <> "the " <> what <> " " <> tshow cell <> " is not four bytes")
      | (what, Nothing, cell) <- [("start", rgStart r, cellText 1), ("end", rgEnd r, cellText (if rgOneCell r then 1 else 2))] ]
      <> [ finding Error "C-UID-RANGE" (rowLabel r <> "the range starts at " <> spaced s <> ", after its end " <> spaced e)
         | Just s <- [rgStart r], Just e <- [rgEnd r], s > e ]
      <> [ finding Note "N-RANGE-ONE-CELL" (rowLabel r <> "one cell spans Start and End: the range is " <> spaced s <> " alone")
         | rgOneCell r, Just s <- [rgStart r] ]
      where cellText i = maybe "" (\c -> c.text) (listToMaybe (drop i (rgCells r)))
    overlaps bt rs =
      [ atTable ctx bt (finding Error "C-UID-RANGE" (rowLabel b <> "the range " <> spaced bs <> " to " <> spaced be <> " overlaps row "
                                                    <> tshow (rgRow a + 1) <> " " <> rgName a <> ", " <> spaced as' <> " to " <> spaced ae))
      | (a, i) <- zip rs [0 :: Int ..], (b, j) <- zip rs [0 ..], i < j
      , Just as' <- [rgStart a], Just ae <- [rgEnd a], Just bs <- [rgStart b], Just be <- [rgEnd b], as' <= be, bs <= ae ]

    -- the rows the table's categories categorise
    members bt rs = case normHeader (rangeKey bt) of
      "template"   -> templateMembers bt rs
      "typeformat" -> typeMembers bt rs
      other        -> [ atTable ctx bt (finding Note "N-RANGE-UNCHECKED" ("no rows are categorised by " <> other)) ]

    templateMembers bt rs = concatMap check subjects <> untemplated
      where
        subjects =
          [ Subject (tuBound r) ("row " <> tshow (tuRow r + 1) <> " " <> tuName r <> ": ")
                    (mfilter (not . T.null) (T.strip . (.text) <$> listToMaybe (drop 3 (tuCells r)))) (lsb o)
          | Right r <- tableUIDRows ctx, Just o <- [tuObject r] ]
          <> [ Subject (urBound r) ("row " <> tshow (urRow r + 1) <> " \"" <> rowText r <> "\": ") (urTemplate r) (lsb bs)
             | r <- uidRows ctx, Exact bs <- [urValue r], any (isJust . urTemplate) (sameTable r) ]
        sameTable r = [ x | x <- uidRows ctx, btAnchor (urBound x) == btAnchor (urBound r) ]
        check s = case sCategory s of
          Nothing -> []
          Just t -> case [ r | r <- rs, rgName r == t ] of
            [] -> [ atTable ctx (sBound s) (finding Error "C-UID-RANGE" (sLabel s <> "the template " <> t <> " is none "
                                                                          <> renderAnchorId (btAnchor bt) <> " gives a range")) ]
            named -> outside bt s t named
        untemplated =
          [ atTable ctx b (finding Note "N-UID-RANGE-NO-TEMPLATE"
              ("rows that name no template, with the range of " <> renderAnchorId (btAnchor bt) <> " their LSB lies in: "
                <> T.intercalate "; " [ T.dropEnd 2 (sLabel s) <> " in " <> maybe "none" rgName (containing s) | s <- ss ]))
          | ss@(s0 : _) <- L.groupBy (\a b -> btAnchor (sBound a) == btAnchor (sBound b)) [ s | s <- subjects, isNothing (sCategory s) ]
          , let b = sBound s0 ]
        containing s = L.find (\r -> maybe False (\(lo, hi) -> lo <= sLSB s && sLSB s <= hi) (bounds r)) rs

    typeMembers bt rs = concatMap check reads <> normalised
      where
        reads = rights (typeRowReads ctx)
        check tr = case (trUID tr, fst (formatRanges rs (formatName tr))) of
          (Nothing, _) -> []
          (Just _, []) -> [ here tr (finding Error "C-UID-RANGE" (trName tr <> ": its Format names " <> formatName tr
                                                               <> ", which is no type format " <> renderAnchorId (btAnchor bt) <> " gives a range")) ]
          (Just bs, named) -> [ here tr (finding Error "C-UID-RANGE" (trName tr <> ": the LSB " <> spaced (lsb bs) <> " of "
                                                                     <> hexUID bs <> " lies outside " <> ranged named <> ", which "
                                                                     <> renderAnchorId (btAnchor bt) <> " gives " <> formatName tr))
                              | not (any (inside (lsb bs)) named) ]
        here tr = locatedAt (cxSpec ctx) (renderAnchorId (trAnchor tr)) (fromMaybe 1 (tablePage (trTable tr)))
        normalised =
          [ atTable ctx bt (finding Note "N-RANGE-NAME-NORMALISED"
              (rowLabel r <> "read as " <> T.map (\c -> if c == ' ' then '_' else c) (rgName r) <> ", the name " <> tshow (length trs) <> " type rows' Format gives ("
                <> T.intercalate ", " (map renderAnchorId trs) <> ")"))
          | r <- rs
          , let trs = [ trAnchor tr | tr <- reads, (matched, True) <- [formatRanges rs (formatName tr)], rgRow r `elem` map rgRow matched ]
          , not (null trs) ]

    -- a subject named in the table, outside every range its category has
    outside bt s t named =
      [ atTable ctx (sBound s) (finding Error "C-UID-RANGE" (sLabel s <> "the LSB " <> spaced (sLSB s) <> " lies outside "
                                                             <> ranged named <> ", which " <> renderAnchorId (btAnchor bt)
                                                             <> " gives " <> t))
      | not (any (inside (sLSB s)) named) ]
    inside v r = maybe False (\(lo, hi) -> lo <= v && v <= hi) (bounds r)
    ranged named = T.intercalate " or " [ spaced lo <> " to " <> spaced hi | r <- named, Just (lo, hi) <- [bounds r] ]

bounds :: RangeRow -> Maybe (Word32, Word32)
bounds r = (,) <$> rgStart r <*> rgEnd r

rowLabel :: RangeRow -> Text
rowLabel r = "row " <> tshow (rgRow r + 1) <> " " <> rgName r <> ": "

-- | A type row's format name, as its Format prints it with the spaces removed: @Restricted_Reference_Type{6}@.
formatName :: TypeRowRead -> Text
formatName tr = T.filter (/= ' ') (T.strip (T.takeWhile (/= ',') (trFormat tr)))

-- | The ranges a type format name has: by its name (spaces removed), by its
--   name with spaces and underscores removed (True: a normalised name), or by
--   its name without its @{n}@ code, in that order.
formatRanges :: [RangeRow] -> Text -> ([RangeRow], Bool)
formatRanges rs name = case (by exact name, by loose name) of
  (found@(_ : _), _) -> (found, False)
  (_, found@(_ : _)) -> (found, True)
  _ | family /= name -> formatRanges rs family
    | otherwise      -> ([], False)
  where
    by key n = [ r | r <- rs, key (rgName r) == key n ]
    exact = T.filter (/= ' ')
    loose = T.toLower . T.filter (\c -> c /= ' ' && c /= '_')
    family = T.takeWhile (/= '{') name


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx = map table
  where
    table bt = Emit (rangesDecl (provenanceOf ctx (btAnchor bt) (btTable bt) (normaliseCaption (fromMaybe "" (btRead bt).caption)))
                                (lowerIdent (rangeKey bt) <> "Ranges") (tableRows bt))

-- | A table's ranges, each category with its first and last LSB.
rangesDecl :: Gen.Provenance -> Text -> [RangeRow] -> Gen.Decl
rangesDecl p n rows = (Gen.decl p n body)
  { Gen.declImports = [Gen.ImportSome "Prelude" ["String"], Gen.ImportSome "Data.Word" ["Word32"]] }
  where
    entries = [ "(" <> tshow (rgName r) <> ", (" <> hex32 lo <> ", " <> hex32 hi <> "))" | r <- rows, Just (lo, hi) <- [bounds r] ]
    body = [ Gen.provenanceComment p <> ": each category's first and last LSB"
           , n <> " :: [(String, (Word32, Word32))]", n <> " =" ]
           <> zipWith (\i e -> (if i == (0 :: Int) then "  [ " else "  , ") <> e) [0 ..] entries <> ["  ]"]
