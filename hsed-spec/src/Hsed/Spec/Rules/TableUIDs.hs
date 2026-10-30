{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.TableUIDs
Description : The tableUIDs rule: every table's UID, its descriptor object's UID and its half-UID
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E6. @tableUIDs@ fires on a table headed
@[UID of Table Descriptor Object, UID of Table, Table Name, Template]@: in Core
2.01 that is Table 240 alone, so the rule rides on the @doc@ anchor (design
13.24). Each row names a table and the two UIDs of its row in the Table table
and of the table itself.

Checks:

* @C-T240-HALVES@: both UID cells are eight bytes, and on every row the low
  half of the descriptor object's UID is the high half of the table's UID (the
  table's half-UID), the low half of the table's UID is zero, and the high half
  of the descriptor object's UID is the half-UID of the table of tables: the
  one row whose descriptor object lies in its own table (row 1, @Table@, in
  Core; Core section 3.2.5.3 b.i and b.ii, printed pages 34-35);
* @C-T240-NAME@: every table name is an identifier, and no two rows name one
  table (each names generated definitions).

Each row's definitions cite the page the row is printed on (Table 240 spans
two), with the table's ordinal and cell hash.

Compiled into 'tableUIDsModule' in the shapes the hand transcription (the
oracle's fixture, @Hand.TableUIDs@) has: @u\<T\>TableObject :: UID@, @u\<T\>Table ::
UID@ and @h\<T\> :: HalfUID@ per row, @nameHalfUID :: Map HalfUID String@ and
@nameUID :: Map UID String@ (@"\<T\> Table"@, @"\<T\> Table Object"@). The typed
half-UIDs @oth\<T\>@ and @bth\<T\>@ need each table's kind (byte or object
table), which Core does not give as data; they wait for Opal's Kind column
(design M5) and are listed in @spec/expected-diff.txt@.
-}
module Hsed.Spec.Rules.TableUIDs
  ( tableUIDsRule
  , tableUIDsModule
    -- * Reading the bound tables
  , TableUIDRow (..)
  , tableUIDRows
  , tableNames
    -- * Generated names
  , tableUIDName
  , tableObjectUIDName
    -- * Messages
  , tableUIDsNamed
  ) where

import qualified RIO.Char         as C
import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Anchor (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import           Hsed.Spec.DSL    (SomeBinding (..), tableUIDs)
import qualified Hsed.Spec.Gen    as Gen
import           Hsed.Spec.IR     (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Shape  (Shape (..), shapeMismatch)


-- | The rule.
tableUIDsRule :: RuleDef
tableUIDsRule = RuleDef
  { ruleName    = RuleName "tableUIDs"
  , bindsTo     = SomeBinding tableUIDs
  , exemplar    = (SpecId "core-2.01", TableId 240)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch TableUIDs form)
  , ruleIdent   = const Nothing
  , ruleModule  = tableUIDsModule
  , ruleCodes   =
      [ CodeDef "C-T240-HALVES" [Error]          (Defect "a table's UIDs disagree with its descriptor object's")
      , CodeDef "C-T240-NAME"   [Error]          (Defect "a table name is no identifier, or names two tables") ]
  , ruleChecks  = \ctx bts -> concatMap (either (\(bt, f) -> [atTable ctx bt f]) (rowChecks ctx (tablesTable bts))) (readRows bts)
                               <> tablesTableChecks ctx bts <> nameChecks ctx bts
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.TableUIDs@.
tableUIDsModule :: Gen.GenModule
tableUIDsModule = Gen.GenModule
  { Gen.leaf        = "TableUIDs"
  , Gen.description = "Table UIDs: each table's UID, its descriptor object's UID and its half-UID"
  , Gen.notes       = [ "The typed half-UIDs of hsed's hand-written TableUIDs (oth\\<T\\>, bth\\<T\\>) need each"
                      , "table's kind, which Core does not give as data; they are not generated here." ] }


-- * Reading the bound tables

-- | One row of a table of table UIDs, read under the errata set.
data TableUIDRow = TableUIDRow
  { tuBound      :: !BoundTable
  , tuRow        :: !Int               -- ^ 0-based among the data rows
  , tuName       :: !Text
  , tuObjectText :: !Text              -- ^ the descriptor object's UID cell
  , tuTableText  :: !Text              -- ^ the table's UID cell
  , tuObject     :: !(Maybe [Word8])
  , tuTable      :: !(Maybe [Word8])
  , tuCells      :: ![CellIR]          -- ^ the row's cells, for the page it is printed on
  }

-- | Every row of every table bound to the rule's shape; a row that is not four cells is a finding.
tableUIDRows :: Context -> [Either Finding TableUIDRow]
tableUIDRows ctx = map (first (located ctx)) (readRows (boundTables (Just TableUIDs) ctx))
  where located c (bt, f) = atTable c bt f

readRows :: [BoundTable] -> [Either (BoundTable, Finding) TableUIDRow]
readRows bts = [ row bt i cells | bt <- bts, (i, cells) <- zip [0 ..] (btRead bt).rows ]
  where
    row :: BoundTable -> Int -> [CellIR] -> Either (BoundTable, Finding) TableUIDRow
    row bt i = \case
      [object, table, name, _template] -> Right TableUIDRow
        { tuBound = bt, tuRow = i, tuName = T.strip name.text, tuObjectText = object.text, tuTableText = table.text
        , tuObject = parseUIDCell object.text, tuTable = parseUIDCell table.text, tuCells = [object, table, name, _template] }
      cells -> Left (bt, finding Error "C-T240-HALVES" ("row " <> tshow (i + 1) <> " has " <> tshow (length cells) <> " cells, not four"))

-- | The table names a @uidref{...}@ may name: the Table Name column of every
--   table bound to the rule's shape.
tableNames :: Context -> [Text]
tableNames ctx = [ tuName r | Right r <- tableUIDRows ctx, not (T.null (tuName r)) ]


-- * Checks

rowChecks :: Context -> Either Text (TableUIDRow, [Word8]) -> TableUIDRow -> [Finding]
rowChecks ctx tables r = map (atCells ctx (tuBound r) (tuCells r) . finding Error "C-T240-HALVES" . (label r <>)) (halvesProblems tables r)

-- | The table of tables: the one row whose descriptor object lies in its own
--   table (its descriptor's high half is its table's half-UID), with that
--   half-UID; Left saying why there is none.
tablesTable :: [BoundTable] -> Either Text (TableUIDRow, [Word8])
tablesTable bts = case L.nubBy (\a b -> snd a == snd b) selfDescribing of
  [one]   -> Right one
  []      -> Left "no row's descriptor object lies in its own table, so no row is the table of tables"
  several -> Left ("rows " <> T.intercalate " and " [ tshow (tuRow r + 1) <> " " <> tuName r | (r, _) <- several ]
                 <> " each have their descriptor object in their own table")
  where
    selfDescribing = [ (r, take 4 t) | Right r <- readRows bts, Just o <- [tuObject r], Just t <- [tuTable r], take 4 o == take 4 t ]

-- | C-T240-HALVES, once for the table: no table of tables, reported only when
--   every UID cell reads (an unreadable cell is its own finding).
tablesTableChecks :: Context -> [BoundTable] -> [Finding]
tablesTableChecks ctx bts = case (tablesTable bts, bts) of
  (Left why, bt : _) | all readable rows -> [atTable ctx bt (finding Error "C-T240-HALVES" why)]
  _                                      -> []
  where
    rows = [ r | Right r <- readRows bts ]
    readable r = isJust (tuObject r) && isJust (tuTable r)

-- | What C-T240-HALVES finds wrong with a row, given the table of tables.
halvesProblems :: Either Text (TableUIDRow, [Word8]) -> TableUIDRow -> [Text]
halvesProblems tables r = case (tuObject r, tuTable r) of
  (Nothing, _) -> ["the descriptor object's UID cell " <> tshow (tuObjectText r) <> " is not eight bytes"]
  (_, Nothing) -> ["the table's UID cell " <> tshow (tuTableText r) <> " is not eight bytes"]
  (Just o, Just t) -> catMaybes
    [ unless' (drop 4 o == take 4 t)
        ("the low half " <> hexUID (drop 4 o) <> " of the descriptor object's UID is not the high half "
          <> hexUID (take 4 t) <> " of the table's UID")
    , unless' (all (== 0) (drop 4 t))
        ("the low half " <> hexUID (drop 4 t) <> " of the table's UID is not zero")
    , case tables of
        Right (tr, h) | take 4 o /= h ->
          Just ("the high half " <> hexUID (take 4 o) <> " of the descriptor object's UID is not " <> hexUID h
                <> ", the half-UID of the table of tables (row " <> tshow (tuRow tr + 1) <> " " <> tuName tr <> ")")
        _ -> Nothing ]
  where unless' ok msg = if ok then Nothing else Just msg

nameChecks :: Context -> [BoundTable] -> [Finding]
nameChecks ctx bts = [ atCells ctx (tuBound r) (tuCells r) (finding Error "C-T240-NAME" (label r <> why)) | (r, why) <- nameProblems [ r | Right r <- readRows bts ] ]

-- | What C-T240-NAME finds wrong with the rows: a name that is no identifier,
--   or one an earlier row has.
nameProblems :: [TableUIDRow] -> [(TableUIDRow, Text)]
nameProblems rows = [ (r, "the table name is not an identifier") | r <- rows, not (isIdent (tuName r)) ]
                    <> concatMap twice (Map.elems byName)
  where
    byName = Map.fromListWith (flip (<>)) [ (tuName r, [r]) | r <- rows ]
    twice (first' : rest) = [ (r, "the table name is also that of row " <> tshow (tuRow first' + 1)) | r <- rest ]
    twice []              = []
    isIdent n = case T.uncons n of
      Just (c, cs) -> (C.isAlpha c || c == '_') && T.all (\x -> C.isAlphaNum x || x == '_') cs
      Nothing      -> False

label :: TableUIDRow -> Text
label r = "row " <> tshow (tuRow r + 1) <> " " <> tuName r <> ": "


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = map one rows <> maps
  where
    rows   = [ r | Right r <- readRows bts ]
    named  = [ (btAnchor (tuBound r), tuRow r) | (r, _) <- nameProblems rows ]
    good r = null (halvesProblems (tablesTable bts) r) && (btAnchor (tuBound r), tuRow r) `notElem` named
    one r
      | good r, Just o <- tuObject r, Just t <- tuTable r = Emit (rowDecl ctx r o t)
      | otherwise = Excluded (btAnchor (tuBound r)) (label r <> "C-T240-HALVES or C-T240-NAME failed")
    kept   = [ r | r <- rows, good r ]
    maps   = case kept of
      []      -> []
      r0 : _  -> [Emit (mapsDecl (provenance ctx r0) (length rows) kept)]

provenance :: Context -> TableUIDRow -> Gen.Provenance
provenance ctx r = let bt = tuBound r in boundProvenance ctx bt

-- | The rule as a message names it when no table is bound to it, by its
--   exemplar: @tableUIDs (exemplar core-2.01 T240)@.
tableUIDsNamed :: Text
tableUIDsNamed = ruleNameText (ruleName tableUIDsRule) <> " (exemplar " <> specIdText (fst ex) <> " " <> renderAnchorId (snd ex) <> ")"
  where ex = exemplar tableUIDsRule

-- | The name of a table's generated UID, @u\<T\>Table@ (@uAuthorityTable@).
tableUIDName :: Text -> Text
tableUIDName n = "u" <> n <> "Table"

-- | The name of a table's descriptor object's generated UID, @u\<T\>TableObject@.
tableObjectUIDName :: Text -> Text
tableObjectUIDName n = "u" <> n <> "TableObject"

-- | @u\<T\>TableObject@, @u\<T\>Table@ and @h\<T\>@ for one row, citing the page it is printed on.
rowDecl :: Context -> TableUIDRow -> [Word8] -> [Word8] -> Gen.Decl
rowDecl ctx r object table = (Gen.decl p n body)
  { Gen.declRank    = tuRow r
  , Gen.declImports = [Gen.ImportSome Gen.uidModule ["HalfUID", "UID", "halfUID", "uid"]] }
  where
    p    = atRow ctx (tuCells r) (provenance ctx r)
    n    = tuName r
    doc  = Gen.provenanceComment p <> ", row " <> tshow (tuRow r + 1) <> ": " <> n
    body =
      [ doc, tableObjectUIDName n <> " :: UID", tableObjectUIDName n <> " = " <> Gen.uidExpr object
      , "", doc, tableUIDName n <> " :: UID", tableUIDName n <> " = " <> Gen.uidExpr table
      , "", doc, "h" <> n <> " :: HalfUID", "h" <> n <> " = " <> Gen.bytesExpr "halfUID" (take 4 table) ]

-- | @nameHalfUID@ and @nameUID@, after every row.
mapsDecl :: Gen.Provenance -> Int -> [TableUIDRow] -> Gen.Decl
mapsDecl p rank rows = (Gen.decl p "nameHalfUID" body)
  { Gen.declRank    = rank
  , Gen.declImports = [ Gen.ImportSome "Data.Map" ["Map", "fromList"], Gen.ImportSome "Prelude" ["String"]
                      , Gen.ImportSome Gen.uidModule ["HalfUID", "UID"] ] }
  where
    doc what = Gen.provenanceComment p <> ": " <> what
    names = map tuName rows
    body =
      [ doc "each table's half-UID, by name"
      , "nameHalfUID :: Map HalfUID String"
      , "nameHalfUID = fromList" ]
      <> Gen.listLines "  " [ "(h" <> n <> ", " <> tshow n <> ")" | n <- names ]
      <> [ ""
         , doc "each table's UID and its descriptor object's UID, by name"
         , "nameUID :: Map UID String"
         , "nameUID = fromList" ]
      <> Gen.listLines "  " (concat [ [ "(" <> tableUIDName n <> ", " <> tshow (n <> " Table") <> ")"
                                      , "(" <> tableObjectUIDName n <> ", " <> tshow (n <> " Table Object") <> ")" ] | n <- names ])
