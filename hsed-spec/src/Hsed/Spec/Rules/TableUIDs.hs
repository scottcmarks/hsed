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

* @C-T240-HALVES@: both UID cells are eight bytes, and the low half of the
  descriptor object's UID is the high half of the table's UID (the table's
  half-UID), on every row;
* @C-T240-NAME@: every table name is an identifier, and no two rows name one
  table (each names generated definitions).

Compiled into 'tableUIDsModule' in the shapes hsed's hand-written
@System.SED.MCTP.Common.TableUIDs@ has: @u\<T\>TableObject :: UID@, @u\<T\>Table ::
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
  ) where

import qualified RIO.Char         as C
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Anchor (AnchorId (..))
import           Hsed.Spec.Common (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import           Hsed.Spec.DSL    (SomeBinding (..), tableUIDs)
import qualified Hsed.Spec.Gen    as Gen
import           Hsed.Spec.IR     (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Shape  (Shape (..), normaliseCaption, shapeMismatch)


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
  , ruleChecks  = \ctx bts -> concatMap (either (\(bt, f) -> [atTable ctx bt f]) (rowChecks ctx)) (readRows bts) <> nameChecks ctx bts
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
        , tuObject = parseUIDCell object.text, tuTable = parseUIDCell table.text }
      cells -> Left (bt, finding Error "C-T240-HALVES" ("row " <> tshow (i + 1) <> " has " <> tshow (length cells) <> " cells, not four"))

-- | The table names a @uidref{...}@ may name: the Table Name column of every
--   table bound to the rule's shape.
tableNames :: Context -> [Text]
tableNames ctx = [ tuName r | Right r <- tableUIDRows ctx, not (T.null (tuName r)) ]


-- * Checks

rowChecks :: Context -> TableUIDRow -> [Finding]
rowChecks ctx r = map (atTable ctx (tuBound r) . finding Error "C-T240-HALVES" . (label r <>)) (halvesProblems r)

-- | What C-T240-HALVES finds wrong with a row.
halvesProblems :: TableUIDRow -> [Text]
halvesProblems r = case (tuObject r, tuTable r) of
  (Nothing, _) -> ["the descriptor object's UID cell " <> tshow (tuObjectText r) <> " is not eight bytes"]
  (_, Nothing) -> ["the table's UID cell " <> tshow (tuTableText r) <> " is not eight bytes"]
  (Just o, Just t)
    | drop 4 o == take 4 t -> []
    | otherwise -> ["the low half " <> hexUID (drop 4 o) <> " of the descriptor object's UID is not the high half "
                    <> hexUID (take 4 t) <> " of the table's UID"]

nameChecks :: Context -> [BoundTable] -> [Finding]
nameChecks ctx bts = [ atTable ctx (tuBound r) (finding Error "C-T240-NAME" (label r <> why)) | (r, why) <- nameProblems [ r | Right r <- readRows bts ] ]

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
    good r = null (halvesProblems r) && (btAnchor (tuBound r), tuRow r) `notElem` named
    one r
      | good r, Just o <- tuObject r, Just t <- tuTable r = Emit (rowDecl ctx r o t)
      | otherwise = Excluded (btAnchor (tuBound r)) (label r <> "C-T240-HALVES or C-T240-NAME failed")
    kept   = [ r | r <- rows, good r ]
    maps   = case kept of
      []      -> []
      r0 : _  -> [Emit (mapsDecl (provenance ctx r0) (length rows) kept)]

provenance :: Context -> TableUIDRow -> Gen.Provenance
provenance ctx r = let bt = tuBound r in provenanceOf ctx (btAnchor bt) (btTable bt) (normaliseCaption (fromMaybe "" (btRead bt).caption))

-- | @u\<T\>TableObject@, @u\<T\>Table@ and @h\<T\>@ for one row.
rowDecl :: Context -> TableUIDRow -> [Word8] -> [Word8] -> Gen.Decl
rowDecl ctx r object table = (Gen.decl p n body)
  { Gen.declRank    = tuRow r
  , Gen.declImports = [Gen.ImportSome "System.SED.MCTP.Common.UID" ["HalfUID", "UID", "halfUID", "uid"]] }
  where
    p    = provenance ctx r
    n    = tuName r
    doc  = Gen.provenanceComment p <> ", row " <> tshow (tuRow r + 1) <> ": " <> n
    body =
      [ doc, "u" <> n <> "TableObject :: UID", "u" <> n <> "TableObject = " <> Gen.uidExpr object
      , "", doc, "u" <> n <> "Table :: UID", "u" <> n <> "Table = " <> Gen.uidExpr table
      , "", doc, "h" <> n <> " :: HalfUID", "h" <> n <> " = " <> Gen.bytesExpr "halfUID" (take 4 table) ]

-- | @nameHalfUID@ and @nameUID@, after every row.
mapsDecl :: Gen.Provenance -> Int -> [TableUIDRow] -> Gen.Decl
mapsDecl p rank rows = (Gen.decl p "nameHalfUID" body)
  { Gen.declRank    = rank
  , Gen.declImports = [ Gen.ImportSome "Data.Map" ["Map", "fromList"], Gen.ImportSome "Prelude" ["String"]
                      , Gen.ImportSome "System.SED.MCTP.Common.UID" ["HalfUID", "UID"] ] }
  where
    doc what = Gen.provenanceComment p <> ": " <> what
    names = map tuName rows
    body =
      [ doc "each table's half-UID, by name"
      , "nameHalfUID :: Map HalfUID String"
      , "nameHalfUID = fromList" ]
      <> listLines [ "(h" <> n <> ", " <> tshow n <> ")" | n <- names ]
      <> [ ""
         , doc "each table's UID and its descriptor object's UID, by name"
         , "nameUID :: Map UID String"
         , "nameUID = fromList" ]
      <> listLines (concat [ [ "(u" <> n <> "Table, " <> tshow (n <> " Table") <> ")"
                             , "(u" <> n <> "TableObject, " <> tshow (n <> " Table Object") <> ")" ] | n <- names ])
    listLines items = zipWith (\i x -> (if i == (0 :: Int) then "  [ " else "  , ") <> x) [0 ..] items <> ["  ]"]
