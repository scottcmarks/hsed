{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.TableDesc
Description : The tableDesc rule: each table's columns, its UIDs from Table 240, its column types
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E5. @tableDesc@ fires on a table headed
@[Column Number, Column Name, IsUnique, Column Type]@ whose caption is
@\<T\> Table Description@: in Core 2.01 the 45 descriptions of sections
5.3-5.8, and Table 3, the @Foo Table Description@ example of section 3.2.1.1,
whose @prose@ comment wins (design section 3.4). The shape picks them out
across Core, so the rule rides on the @doc@ anchor (design 13.24). Each
description names its table (@T@), whose UID and descriptor object's UID come
from the table the @tableUIDs@ rule binds (Table 240,
"Hsed.Spec.Rules.TableUIDs"), and gives one row per column.

Checks, each with the code its findings carry:

* @C-COL-CONTIG@: the Column Number cells read @0x00@, @0x01@, ... in row
  order, with no gap;
* @C-COL-CELL@: every row is four cells, IsUnique is @Yes@ or blank, every
  column name is an identifier (Core 3.2.5: a letter, then letters, digits or
  underscores) and no two columns of a table share one;
* @C-FMT-RESOLVE@: every Column Type names a Type object, the Name of a table
  the @typeRow@ rule binds ("Hsed.Spec.Rules.TypeRow"). Core 2.01 prints four
  column types no Type table row defines (@boolean_def_true@, T172;
  @int_1_def_0@, T182 and T183; @boolean_def_false@, T215; @log_kind@, T233)
  and no erratum of Scott's notes corrects them: each is a Warning citing the
  page its row is printed on, and the column is generated with its type's
  name and no UID;
* @C-DESC-TABLE@: the caption names a table of Table 240, and no other
  description names it.

Compiled into 'tableDescsModule': @\<t\>TableDesc :: TableDesc@ per description
(the table's name with its first letter in lower case, @tableTableDesc@ for
Table 171), the runtime 'Hsed.Core.Table.TableDesc' whose UIDs are Table 240's
@u\<T\>Table@ and @u\<T\>TableObject@ and whose columns' types are
@TypeName@s, each its text and its Type object's @u\<type\>Type@
("Hsed.Spec.Rules.TypeRow".'typeNameExpr'), both by name from the modules the
two rules write.

The rules whose tables a description types (@constValues@'s default values,
@preconfig@'s rows) read a column through "Hsed.Spec.Readers.Cell" (since
M4b's scaffold; this module re-exports it): what a column holds ('ColKind',
from its Type object's Format and, for an enumeration, its enumeration
table's labels; a column of the @uid@ type, Core Table 145, PDF 123, is
'KUID', a uid cell, where any other @bytes@ type is 'KBytes'), a cell read as
that kind ('readCell') and the runtime 'Hsed.Core.Table.Cell' it prints as
('renderCell': a 'UIDCell', a 'BytesCell' for bytes of any other type, since
M4a). The types it reads by are 'typeEnv''s: the spec's type rows and
enumerations, then its base's ("Hsed.Spec.Rules.TypeRow".'allTypeRows',
"Hsed.Spec.Rules.EnumValues".'allEnums'), as a column's Type object is read
here ('crTypeRow': Opal's columns take Core's types).
-}
module Hsed.Spec.Rules.TableDesc
  ( tableDescRule
  , tableDescsModule
    -- * Reading the bound tables
  , DescRead (..)
  , ColRead (..)
  , descReads
  , readDescs
  , descIdent
  , descDeclName
  , descSection
  , parseColNumber
    -- * What a column holds ("Hsed.Spec.Readers.Cell")
  , TypeEnv (..)
  , typeEnv
  , ColKind (..)
  , colKind
  , CellValue (..)
  , readCell
  , renderCell
  , colNumberLit
  ) where

import qualified Data.Text.Read            as TR
import qualified RIO.Char                  as C
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           Numeric                   (showHex)
import           RIO

import           Hsed.Spec.Anchor          (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common          (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.DSL             (SomeBinding (..), tableDesc)
import           Hsed.Spec.Format          (nearest)
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.IR              (CellIR (..), TableIR (..))
import           Hsed.Spec.Readers.Cell    (CellValue (..), ColKind (..), TypeEnv (..), colKind, readCell, renderCell)
import           Hsed.Spec.Readers.Text    (lowerIdent)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.EnumValues (EnumRead (..), allEnums, parseEnumValue)
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..), tableObjectUIDName, tableUIDName, tableUIDRows, tableUIDsModule,
                                            tableUIDsNamed)
import           Hsed.Spec.Rules.TypeRow   (TypeRowRead (..), allTypeRows, typeNameExpr, typeRowReads, typesModule)
import           Hsed.Spec.Shape           (CaptionKind (..), Form (..), Shape (..), measure,
                                            shapeMismatch)


-- | The rule.
tableDescRule :: RuleDef
tableDescRule = RuleDef
  { ruleName    = RuleName "tableDesc"
  , bindsTo     = SomeBinding tableDesc
  , exemplar    = (SpecId "core-2.01", TableId 171)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch TableDesc form) && isJust (descIdent form)
  , ruleIdent   = descIdent
  , ruleModule  = tableDescsModule
  , ruleCodes   =
      [ CodeDef "C-COL-CONTIG" [Error]          (Defect "a table description's column numbers leave a gap")
      , CodeDef "C-COL-CELL"   [Error]          (Defect "a table description's row is not four cells, or names a column twice or by no identifier")
      , CodeDef "C-DESC-TABLE" [Error]          (Defect "a table description names no table of Table 240, or one another description names") ]
  , ruleChecks  = \ctx bts -> let ds = readDescs ctx bts in concatMap (descFindings ctx) ds <> twice ctx ds
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.TableDescs@.
tableDescsModule :: Gen.GenModule
tableDescsModule = Gen.GenModule
  { Gen.leaf        = "TableDescs"
  , Gen.description = "Table descriptions: each table's UIDs and columns"
  , Gen.notes       = [ "A column whose type no Type table row defines has its type's name and no UID"
                      , "(C-FMT-RESOLVE in docs/LEDGER.md)." ] }

-- | The table a description's caption names: @Authority@ for @Authority Table Description@.
descIdent :: Form -> Maybe Text
descIdent form = case fCaptionKind form of
  CaptionTableDesc s | not (T.null s) -> Just s
  _                                   -> Nothing


-- * Reading the bound tables

-- | One table description read under the errata set.
data DescRead = DescRead
  { drBound   :: !BoundTable
  , drTable   :: !(Maybe Text)          -- ^ the table its caption names
  , drColumns :: ![ColRead]             -- ^ its rows of four cells, in order
  , drShort   :: ![(Int, [CellIR])]     -- ^ its rows that are not four cells: 0-based index and cells
  }

-- | One column, a row of four cells.
data ColRead = ColRead
  { crRow        :: !Int                  -- ^ 0-based among the data rows
  , crCells      :: ![CellIR]
  , crNumberText :: !Text
  , crNumber     :: !(Maybe Int)          -- ^ the Column Number cell read as @0x@ and hex digits
  , crName       :: !Text
  , crUnique     :: !Text                 -- ^ the IsUnique cell
  , crType       :: !Text                 -- ^ the Column Type cell: a Type object's name
  , crTypeRow    :: !(Maybe TypeRowRead)  -- ^ the type row whose Name it is
  }

-- | Every table bound to the rule's shape, read under the errata set, in anchor order.
descReads :: Context -> [DescRead]
descReads ctx = readDescs ctx (boundTables (Just TableDesc) ctx)

-- | Tables read as descriptions, under the errata set: each column's Type
--   object the spec's type row of its name, else its base's ('allTypeRows').
readDescs :: Context -> [BoundTable] -> [DescRead]
readDescs ctx = map readDesc
  where
    types = Map.fromListWith (\_ firstOne -> firstOne) [ (trName tr, tr) | (_, tr) <- allTypeRows ctx ]
    readDesc bt = DescRead
      { drBound   = bt
      , drTable   = descIdent (measure (cxSpec ctx) (btRead bt))
      , drColumns = [ col i cells (map (T.strip . (.text)) cells) | (i, cells@[_, _, _, _]) <- rows ]
      , drShort   = [ (i, cells) | (i, cells) <- rows, length cells /= 4 ] }
      where rows = zip [0 ..] (btRead bt).rows
    col i cells texts = ColRead
      { crRow = i, crCells = cells, crNumberText = at 0, crNumber = parseColNumber (at 0)
      , crName = at 1, crUnique = at 2, crType = at 3, crTypeRow = Map.lookup (at 3) types }
      where at k = fromMaybe "" (listToMaybe (drop k texts))

-- | A Column Number cell: @0x@ and hex digits.
parseColNumber :: Text -> Maybe Int
parseColNumber t = case T.stripPrefix "0x" (T.toLower (T.strip t)) of
  Just hex | not (T.null hex), T.all C.isHexDigit hex, Right (n, "") <- TR.hexadecimal hex -> Just n
  _ -> Nothing

-- | The section a description lies in.
descSection :: Context -> DescRead -> Maybe Text
descSection ctx = sectionOf ctx . drBound

-- | @authorityTableDesc@: the table's name as a value's name ('lowerIdent', the
--   convention of every generated value: @MBRControl@ gives
--   @mbrControlTableDesc@), then @TableDesc@.
descDeclName :: Text -> Text
descDeclName n = lowerIdent n <> "TableDesc"


-- * Checks

-- | Every finding about one description.
descFindings :: Context -> DescRead -> [Finding]
descFindings ctx d = shortRows <> contig <> cellChecks <> typeChecks <> tableCheck
  where
    bt = drBound d
    cols = drColumns d
    here = atCells ctx bt
    err c cells msg = here cells (finding Error c msg)
    label cr = "column " <> crNumberText cr <> " " <> crName cr <> ": "

    shortRows = [ err "C-COL-CELL" cells ("row " <> tshow (i + 1) <> " has " <> tshow (length cells) <> " cells, not four")
                | (i, cells) <- drShort d ]

    -- C-COL-CONTIG: one finding for the first number out of place, every unreadable number
    unreadable = [ err "C-COL-CONTIG" (crCells cr) ("the column number " <> tshow (crNumberText cr) <> " is not 0x and hex digits")
                 | cr <- cols, isNothing (crNumber cr) ]
    misplaced  = take 1 [ err "C-COL-CONTIG" (crCells cr)
                            (label cr <> "the column numbers run 0x00 to " <> colNumberLit (length cols - 1)
                              <> " in row order, and row " <> tshow (crRow cr + 1) <> " is " <> crNumberText cr
                              <> " where " <> colNumberLit i <> " is expected")
                        | (i, cr) <- zip [0 ..] cols, Just n <- [crNumber cr], n /= i ]
    contig = unreadable <> misplaced

    -- C-COL-CELL: IsUnique, names
    cellChecks =
      [ err "C-COL-CELL" (crCells cr) (label cr <> "IsUnique is " <> tshow (crUnique cr) <> ", not Yes or blank")
      | cr <- cols, crUnique cr `notElem` ["", "Yes"] ]
      <> [ err "C-COL-CELL" (crCells cr) (label cr <> "the column name is not an identifier (a letter, then letters, digits or underscores)")
         | cr <- cols, not (isIdent (crName cr)) ]
      <> concat [ [ err "C-COL-CELL" (crCells cr) (label cr <> "the column name is also that of column " <> crNumberText c0) | cr <- rest ]
                | c0 : rest <- Map.elems (Map.fromListWith (flip (<>)) [ (crName cr, [cr]) | cr <- cols ]) ]

    -- C-FMT-RESOLVE: the column type is a Type object's name; a Warning, generated without a UID
    known = map trName (rights (typeRowReads ctx))
    typeChecks =
      [ here (crCells cr) (finding Warning "C-FMT-RESOLVE"
          (label cr <> "the column type " <> crType cr <> " names no Type object (no type row binds it)"
            <> maybe "" (\n -> "; nearest: " <> n <> nearestTable n) (nearest known (crType cr))
            <> "; generated with its name and no UID"))
      | cr <- cols, isNothing (crTypeRow cr) ]
    nearestTable n = maybe "" (\tr -> ", " <> renderAnchorId (trAnchor tr)) (L.find ((== n) . trName) (rights (typeRowReads ctx)))

    -- C-DESC-TABLE: the table the caption names is one of Table 240's
    tableCheck = case (drTable d, tableUIDRowNamed ctx <$> drTable d) of
      (Nothing, _) -> [atTable ctx bt (finding Error "C-DESC-TABLE" "the caption names no table (\"<T> Table Description\")")]
      (Just n, Just Nothing)
        | null (rights (tableUIDRows ctx)) ->
            [atTable ctx bt (finding Error "C-DESC-TABLE" ("no table is bound by " <> tableUIDsNamed <> " to give " <> n <> " its UID"))]
        | otherwise ->
            [atTable ctx bt (finding Error "C-DESC-TABLE" ("the caption names " <> n <> ", which no row of the table of table UIDs names"))]
      _ -> []

-- | C-DESC-TABLE: a table two descriptions name, reported at every one after the first.
twice :: Context -> [DescRead] -> [Finding]
twice ctx ds =
  [ atTable ctx (drBound d) (finding Error "C-DESC-TABLE"
      ("the caption names " <> n <> ", which " <> renderAnchorId (btAnchor (drBound d0)) <> " describes too"))
  | (n, d0, d) <- repeats ds ]

-- | Each description naming a table an earlier one names: the table, the first, the later one.
repeats :: [DescRead] -> [(Text, DescRead, DescRead)]
repeats ds = concat
  [ [ (n, d0, d) | d <- rest ]
  | (n, d0 : rest) <- Map.toList (Map.fromListWith (flip (<>)) [ (n, [d]) | d <- ds, Just n <- [drTable d] ]) ]

-- | The row of Table 240 naming a table.
tableUIDRowNamed :: Context -> Text -> Maybe TableUIDRow
tableUIDRowNamed ctx n = L.find ((== n) . tuName) (rights (tableUIDRows ctx))

isIdent :: Text -> Bool
isIdent n = case T.uncons n of
  Just (c, cs) -> C.isAlpha c && T.all (\x -> C.isAlphaNum x || x == '_') cs
  Nothing      -> False


-- * What a column holds

-- | The types a spec's columns are read by ("Hsed.Spec.Readers.Cell"): the
--   Type objects of its type rows, then of its base's ('allTypeRows'), and
--   the labels of its enumerations, then of its base's ('allEnums': Opal's
--   Table 49 labels @life_cycle_state@, shadowing Core's Table 103), each
--   label with the first value its enumeration gives it.
typeEnv :: Context -> TypeEnv
typeEnv ctx = TypeEnv
  { teType   = \n -> first Cm.message . trTypeDef <$> Map.lookup n types
  , teLabels = labels }
  where
    types = Map.fromListWith (\_ firstOne -> firstOne) [ (trName tr, tr) | (_, tr) <- allTypeRows ctx ]
    enums = map snd (allEnums ctx)
    labels n = L.nubBy (\a b -> fst a == fst b)
      [ (T.strip label, v)
      | er <- enums, erIdent er == Just n
      , (cell, label) <- erRows er, Just (v : _) <- [parseEnumValue (erRangeWords er) cell] ]

showHexInteger :: Integer -> String
showHexInteger v = showHex v ""

-- | A column number as the descriptions print it: @0x0C@.
colNumberLit :: Int -> Text
colNumberLit n = "0x" <> T.justifyRight 2 '0' (T.toUpper (T.pack (showHexInteger (fromIntegral n))))


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = map one ds
  where
    ds = readDescs ctx bts
    later = [ btAnchor (drBound d) | (_, _, d) <- repeats ds ]
    one d = case (drTable d >>= tableUIDRowNamed ctx, any Cm.isError (descFindings ctx d)) of
      (Just tu, False) | isJust (tuObject tu), isJust (tuTable tu), btAnchor (drBound d) `notElem` later ->
        Emit (descDecl ctx d (tuName tu))
      _ -> Excluded (btAnchor (drBound d)) "C-COL-CONTIG, C-COL-CELL or C-DESC-TABLE failed"

-- | @\<t\>TableDesc :: TableDesc@.
descDecl :: Context -> DescRead -> Text -> Gen.Decl
descDecl ctx d n = (Gen.decl p name body)
  { Gen.declImports =
      [ Gen.ImportSome Gen.coreTableModule ["Col (..)", "TableDesc (..)"]
      , Gen.ImportSome Gen.typeNameModule ["TypeName (..)"]
      , Gen.ImportAll (Gen.moduleName sid tableUIDsModule)
      , Gen.ImportSome "Prelude" ["Bool (..)", "Maybe (..)"] ]
      <> [ Gen.ImportAll (Gen.moduleName sid typesModule) | any (isJust . crTypeRow) (drColumns d) ] }
  where
    sid  = cxSpec ctx
    bt   = drBound d
    p    = boundProvenance ctx bt
    name = descDeclName n
    colLine i cr = (if i == (0 :: Int) then "      [ " else "      , ")
      <> T.unwords [ "Col", colNumberLit (fromMaybe i (crNumber cr)), tshow (crName cr)
                   , if crUnique cr == "Yes" then "True" else "False", typeNameExpr (crType cr) (crTypeRow cr) ]
    body =
      [ Gen.provenanceComment p
      , name <> " :: TableDesc"
      , name <> " = TableDesc"
      , "  { tdName      = " <> tshow n
      , "  , tdTableUID  = " <> tableUIDName n
      , "  , tdObjectUID = " <> tableObjectUIDName n
      , "  , tdColumns   =" ]
      <> zipWith colLine [0 ..] (drColumns d)
      <> [ "      ]", "  }" ]
