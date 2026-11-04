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
  the @typeRow@ rule binds ("Hsed.Spec.Rules.TypeRow"), the spec's or its
  base's. Core 2.01 prints four column types no Type table row defines
  (@boolean_def_true@, T172; @int_1_def_0@, T182 and T183;
  @boolean_def_false@, T215; @log_kind@, T233) and no erratum of Scott's
  notes corrects them: each is a Warning citing the page its row is printed
  on and the nearest type name (a base's with its spec), and the column is
  generated with its type's name and no UID;
* @C-DESC-TABLE@: the caption names a table of Table 240, the spec's or its
  base's ("Hsed.Spec.Rules.TableUIDs".'allTableUIDRows', which for Opal
  adds the DataRemovalMechanism row of its Table 20, PDF 37), and no other
  description names it.

The column checks are 'columnFindings', which @columnsAddition@
("Hsed.Spec.Rules.ColumnsAddition") runs on the columns a table adds to
another's description, their numbers running on from that description's
('ColumnsFrom').

Compiled into 'tableDescsModuleFor' the spec, whose note names the spec's own
ledger: @\<t\>TableDesc :: TableDesc@ per description
(the table's name with its first letter in lower case, @tableTableDesc@ for
Table 171), the runtime 'Hsed.Core.Table.TableDesc' whose UIDs are Table 240's
@u\<T\>Table@ and @u\<T\>TableObject@ and whose columns' types are
@TypeName@s, each its text and its Type object's @u\<type\>Type@
("Hsed.Spec.Rules.TypeRow".'typeNameExpr'), both by name from the modules the
two rules write ('tableDescDecl'): each name from the generated module of the
spec whose read defines it, the spec's own whole and a base's by name (since
M4b: Opal's descriptions take Core's types from @Hsed.Gen.Core.Types@ and
their own from @Hsed.Gen.Opal.Types@). A table whose UIDs only an SP's
preconfiguration of the table of tables gives (Opal's DataRemovalMechanism,
Table 20 row 12, PDF 37) has them declared beside its description, in the
spec's 'tableUIDsModuleFor' ("Hsed.Spec.Rules.TableUIDs".'rowDecl').

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
  , tableDescsModuleFor
    -- * Reading the bound tables
  , DescRead (..)
  , ColRead (..)
  , descReads
  , readDescs
  , descIdent
  , descDeclName
  , descSection
  , parseColNumber
  , tableUIDRowNamed
  , declaredTableObjects
    -- * Checking a table's columns
  , ColumnsFrom (..)
  , columnFindings
    -- * Declarations
  , tableDescDecl
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
import           Hsed.Spec.DSL             (SomeBinding (..), someBindingShape, tableDesc)
import           Hsed.Spec.Format          (nearest)
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.IR              (CellIR (..), TableIR (..))
import           Hsed.Spec.Readers.Cell    (CellValue (..), ColKind (..), TypeEnv (..), cellTypos, colKind, readCell, renderCell)
import           Hsed.Spec.Readers.Text    (lowerIdent)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.EnumValues (EnumRead (..), allEnums, parseEnumValue)
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..), allTableUIDRows, rowDecl, tableObjectUIDName, tableUIDName,
                                            tableUIDRows, tableUIDsModule, tableUIDsModuleFor, tableUIDsNamed)
import           Hsed.Spec.Rules.TypeRow   (TypeRowRead (..), allTypeRows, typeNameExpr, typeUIDName, typesModule)
import           Hsed.Spec.Shape           (CaptionKind (..), Form (..), Shape (..), measure,
                                            shapeMismatch)


-- $setup
-- >>> :set -XOverloadedStrings

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

-- | @Hsed.Gen.\<Spec\>.TableDescs@: its name and description, which the
--   rules importing a description name it by; the declarations go to
--   'tableDescsModuleFor' the spec.
tableDescsModule :: Gen.GenModule
tableDescsModule = Gen.GenModule
  { Gen.leaf        = "TableDescs"
  , Gen.description = "Table descriptions: each table's UIDs and columns"
  , Gen.notes       = [] }

-- | 'tableDescsModule' for a spec, its note naming the spec's own ledger
--   ('Gen.ledgerFile': @docs/LEDGER.md@ for Core, @docs/LEDGER-opal.md@ for
--   Opal; the lead's decision at the M4b review, design 13.32 (22)), where a
--   column's C-FMT-RESOLVE is counted: the module every description of the
--   spec goes to, this rule's and @columnsAddition@'s.
tableDescsModuleFor :: SpecId -> Gen.GenModule
tableDescsModuleFor sid = tableDescsModule
  { Gen.notes = [ "A column whose type no Type table row defines has its type's name and no UID"
                , "(C-FMT-RESOLVE in " <> Gen.ledgerFile sid <> ")." ] }

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
  , crType       :: !Text                 -- ^ the Column Type cell: a Type object's name, as printed
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
      , crName = at 1, crUnique = at 2, crType = at 3, crTypeRow = Map.lookup (typeRead types (at 3)) types }
      where at k = fromMaybe "" (listToMaybe (drop k texts))

-- | The Type object's name a Column Type cell is read as: the cell, or,
--   where no type row defines it and 'cellTypos' lists it, the name it
--   misspells (since M5a: Opal Table 43's @uniteger_8@ as @uinteger_8@,
--   PDF 85; the reading's Note is 'columnFindings'').
--
-- >>> typeRead (Map.fromList [("uinteger_8", ())]) "uniteger_8"
-- "uinteger_8"
-- >>> typeRead (Map.fromList [("uinteger_8", ())]) "uinteger_4"
-- "uinteger_4"
typeRead :: Map Text a -> Text -> Text
typeRead types t
  | Map.member t types = t
  | otherwise          = maybe t (\r -> if Map.member r types then r else t) (lookup t cellTypos)

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
descFindings ctx d = columnFindings ctx bt FromStart (drShort d) (drColumns d) <> tableCheck
  where
    bt = drBound d
    -- C-DESC-TABLE: the table the caption names is one of Table 240's, the spec's or its base's
    tableCheck = case (drTable d, tableUIDRowNamed ctx <$> drTable d) of
      (Nothing, _) -> [atTable ctx bt (finding Error "C-DESC-TABLE" "the caption names no table (\"<T> Table Description\")")]
      (Just n, Just Nothing)
        | null (allTableUIDRows ctx) ->
            [atTable ctx bt (finding Error "C-DESC-TABLE" ("no table is bound by " <> tableUIDsNamed <> " to give " <> n <> " its UID"))]
        | otherwise ->
            [atTable ctx bt (finding Error "C-DESC-TABLE" ("the caption names " <> n <> ", which no row of the table of table UIDs names"))]
      _ -> []

-- | Where the columns a table's rows give begin ('columnFindings'): a
--   description's own, at 0x00; after the columns of the description of the
--   table they extend, named by the table and where its columns are read (a
--   column addition: Opal Table 26's 0x08 after Core Table 211's TPerInfo
--   columns 0x00 to 0x07, PDF 49 and 229); or nowhere known (an addition
--   whose target no description describes, C-COLS-TARGET's).
data ColumnsFrom
  = FromStart
  | After !Text ![Text] ![ColRead]
    -- ^ the table (@TPerInfo@), the tables its columns are read from
    --   (@core-2.01 T211@) and its columns, in order
  | Unplaced

-- | The findings about a table's rows read as a description's columns,
--   each located at the row it is about (the page the row is printed on):
--
--   * @C-COL-CELL@: a row that is not four cells, an IsUnique that is not
--     @Yes@ or blank, a name that is no identifier, a name two of the
--     columns give, or one of them and a column they extend;
--   * @C-COL-CONTIG@: a column number that is not @0x@ and hex digits, and
--     the first number out of place, the numbers running on from where
--     'ColumnsFrom' says, in row order;
--   * @C-FMT-RESOLVE@: a type no type row of the spec or its base defines,
--     a Warning naming the nearest type name (a base's with its spec): the
--     column is generated with its type's name and no UID;
--   * @N-CELL-NORMALISED@: a type read as the name 'cellTypos' says it
--     misspells ('typeRead'), a Note: the column is generated with that
--     type (since M5a: Opal Table 43, PDF 85).
columnFindings :: Context -> BoundTable -> ColumnsFrom -> [(Int, [CellIR])] -> [ColRead] -> [Finding]
columnFindings ctx bt from short cols = shortRows <> contig <> cellChecks <> typeChecks
  where
    here = atCells ctx bt
    err c cells msg = here cells (finding Error c msg)
    label cr = "column " <> crNumberText cr <> " " <> crName cr <> ": "
    extended = case from of
      After _ _ cs -> cs
      _            -> []
    k = length extended
    sources srcs = " (" <> T.intercalate ", " srcs <> ")"

    shortRows = [ err "C-COL-CELL" cells ("row " <> tshow (i + 1) <> " has " <> tshow (length cells) <> " cells, not four")
                | (i, cells) <- short ]

    -- C-COL-CONTIG: one finding for the first number out of place, every unreadable number
    unreadable = [ err "C-COL-CONTIG" (crCells cr) ("the column number " <> tshow (crNumberText cr) <> " is not 0x and hex digits")
                 | cr <- cols, isNothing (crNumber cr) ]
    misplaced  = case from of
      Unplaced -> []
      _        -> take 1 [ err "C-COL-CONTIG" (crCells cr)
                             (label cr <> "the column numbers run 0x00 to " <> colNumberLit (k + length cols - 1)
                               <> " in row order" <> runOn <> ", and row " <> tshow (crRow cr + 1) <> " is " <> crNumberText cr
                               <> " where " <> colNumberLit i <> " is expected")
                         | (i, cr) <- zip [k ..] cols, Just n <- [crNumber cr], n /= i ]
    runOn = case from of
      After t srcs (_ : _) -> ", " <> t <> "'s 0x00 to " <> colNumberLit (k - 1) <> " first" <> sources srcs
      _                    -> ""
    contig = unreadable <> misplaced

    -- C-COL-CELL: IsUnique, names
    cellChecks =
      [ err "C-COL-CELL" (crCells cr) (label cr <> "IsUnique is " <> tshow (crUnique cr) <> ", not Yes or blank")
      | cr <- cols, crUnique cr `notElem` ["", "Yes"] ]
      <> [ err "C-COL-CELL" (crCells cr) (label cr <> "the column name is not an identifier (a letter, then letters, digits or underscores)")
         | cr <- cols, not (isIdent (crName cr)) ]
      <> [ err "C-COL-CELL" (crCells cr) (label cr <> "the column name is also that of " <> t <> "'s column " <> crNumberText c0 <> sources srcs)
         | After t srcs _ <- [from], cr <- cols, Just c0 <- [L.find ((== crName cr) . crName) extended] ]
      <> concat [ [ err "C-COL-CELL" (crCells cr) (label cr <> "the column name is also that of column " <> crNumberText c0) | cr <- rest ]
                | c0 : rest <- Map.elems (Map.fromListWith (flip (<>)) [ (crName cr, [cr]) | cr <- cols ]) ]

    -- C-FMT-RESOLVE: the column type is a Type object's name; a Warning, generated without a UID
    typeChecks =
      [ here (crCells cr) (finding Warning "C-FMT-RESOLVE"
          (label cr <> "the column type " <> crType cr <> " names no Type object (no type row binds it)"
            <> maybe "" ("; nearest: " <>) (nearestType ctx (crType cr))
            <> "; generated with its name and no UID"))
      | cr <- cols, isNothing (crTypeRow cr) ]
      <> [ here (crCells cr) (finding Note "N-CELL-NORMALISED"
             (label cr <> "the column type " <> tshow (crType cr) <> " is read as " <> trName tr
               <> ", a reviewed misspelling (Hsed.Spec.Readers.Cell.cellTypos)"))
         | cr <- cols, Just tr <- [crTypeRow cr], trName tr /= crType cr ]

-- | The type name nearest a column type no type row defines, among the
--   spec's and its base's ('allTypeRows'), with the table that defines it:
--   @integer_1, T93@; a base's with its spec, @uinteger_8, core-2.01 T157@.
nearestType :: Context -> Text -> Maybe Text
nearestType ctx t = do
  n <- nearest (map (trName . snd) rows) t
  (c, tr) <- L.find ((== n) . trName . snd) rows
  pure (n <> ", " <> (if cxSpec c == cxSpec ctx then "" else specIdText (cxSpec c) <> " ") <> renderAnchorId (trAnchor tr))
  where rows = allTypeRows ctx

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

-- | The row of a table of table UIDs naming a table, the spec's or its
--   base's ('allTableUIDRows': Core's Table 240, and for Opal its Table 20's
--   DataRemovalMechanism row), with the Context that owns it.
tableUIDRowNamed :: Context -> Text -> Maybe (Context, TableUIDRow)
tableUIDRowNamed ctx n = L.find ((== n) . tuName . snd) (allTableUIDRows ctx)

-- | The descriptor objects the generated TableUIDs modules of a spec and its
--   bases declare, @u\<T\>TableObject@: every row of a table of table UIDs
--   ("Hsed.Spec.Rules.TableUIDs"' own declarations: Core Table 240's), and
--   the row an SP's preconfiguration of the table of tables gives a table no
--   such table names, where the spec describes the table ('compile' here,
--   'declaresRow': Opal Table 20 row 12's DataRemovalMechanism, which Table
--   32 describes, PDF 37, 51), the first row naming it. A table only an SP's
--   Table table names and no spec describes has none (M5's DataStore, Opal
--   Table 36, PDF 54), nor has a second row naming one table.
declaredTableObjects :: Context -> [[Word8]]
declaredTableObjects ctx = concatMap declared (contextChain ctx)
  where
    declared c =
      [ o | Right tu <- tableUIDRows c, Just o <- [tuObject tu] ]
      <> [ o | d <- descReads c, Just row@(_, tu) <- [drTable d >>= tableUIDRowNamed c], declaresRow c row, Just o <- [tuObject tu] ]

-- | Whether a spec's TableUIDs module declares a description's row of a
--   table of table UIDs beside the description ('compile'): a row the
--   spec's own preconfiguration of the table of tables gives, which no table
--   of table UIDs of the spec writes.
declaresRow :: Context -> (Context, TableUIDRow) -> Bool
declaresRow ctx (owner, tu) = cxSpec owner == cxSpec ctx && someBindingShape (btBinding (tuBound tu)) /= Just TableUIDs

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

-- | Each description's declaration, or why it has none; with it, for a
--   table whose UIDs only the spec's preconfiguration of the table of tables
--   gives (Opal's DataRemovalMechanism, Table 20 row 12, PDF 37), those
--   UIDs' declarations in the spec's 'tableUIDsModuleFor', which no table of
--   table UIDs of the spec writes ("Hsed.Spec.Rules.TableUIDs".'rowDecl').
compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = concatMap one ds
  where
    ds = readDescs ctx bts
    later = [ btAnchor (drBound d) | (_, _, d) <- repeats ds ]
    one d = case (drTable d >>= tableUIDRowNamed ctx, any Cm.isError (descFindings ctx d)) of
      (Just row@(owner, tu), False) | Just o <- tuObject tu, Just t <- tuTable tu, btAnchor (drBound d) `notElem` later ->
        EmitIn (tableDescsModuleFor (cxSpec ctx)) (tableDescDecl ctx (boundProvenance ctx (drBound d)) [] d row)
          : [ EmitIn (tableUIDsModuleFor ctx) (rowDecl owner tu o t) | declaresRow ctx row ]
      _ -> [Excluded (btAnchor (drBound d)) "C-COL-CONTIG, C-COL-CELL or C-DESC-TABLE failed"]

-- | @\<t\>TableDesc :: TableDesc@ for a description read in a spec's
--   Context, under a provenance and the comment lines that follow it: its
--   UIDs those of the table's row of a table of table UIDs, with the Context
--   that owns the row ('tableUIDRowNamed'), and its columns' types the type
--   rows the spec reads by their names, its own and then its base's
--   ('allTypeRows'). Each name is imported from the generated module of the
--   spec whose read defines it, by origin: the spec's own module whole (as
--   Core's descriptions import @Hsed.Gen.Core.TableUIDs@ and
--   @Hsed.Gen.Core.Types@), a base's by name (Opal's TPerInfo takes
--   @uTPerInfoTable@ from @Hsed.Gen.Core.TableUIDs@ and @ubooleanType@ from
--   @Hsed.Gen.Core.Types@, its DataRemovalMechanism @udata_removal_mechanismType@
--   from its own @Hsed.Gen.Opal.Types@).
tableDescDecl :: Context -> Gen.Provenance -> [Text] -> DescRead -> (Context, TableUIDRow) -> Gen.Decl
tableDescDecl ctx p notes d (owner, tu) = (Gen.decl p name body) { Gen.declImports = imports }
  where
    sid   = cxSpec ctx
    n     = tuName tu
    name  = descDeclName n
    types = Map.fromListWith (\_ firstOne -> firstOne) [ (trName tr, (cxSpec c, tr)) | (c, tr) <- allTypeRows ctx ]
    typed cr = Map.lookup (typeRead types (crType cr)) types
    byOrigin m s names
      | s == sid  = Gen.ImportAll (Gen.moduleName sid m)
      | otherwise = Gen.ImportSome (Gen.moduleName s m) names
    imports =
      [ Gen.ImportSome Gen.coreTableModule ["Col (..)", "TableDesc (..)"]
      , Gen.ImportSome Gen.typeNameModule ["TypeName (..)"]
      , byOrigin tableUIDsModule (cxSpec owner) [tableUIDName n, tableObjectUIDName n]
      , Gen.ImportSome "Prelude" ["Bool (..)", "Maybe (..)"] ]
      <> [ byOrigin typesModule s [typeUIDName (trName tr)] | cr <- drColumns d, Just (s, tr) <- [typed cr] ]
    colLine i cr = (if i == (0 :: Int) then "      [ " else "      , ")
      <> T.unwords [ "Col", colNumberLit (fromMaybe i (crNumber cr)), tshow (crName cr)
                   , if crUnique cr == "Yes" then "True" else "False", typeNameExpr (typeRead types (crType cr)) (snd <$> typed cr) ]
    body =
      Gen.provenanceComment p : notes
      <> [ name <> " :: TableDesc"
         , name <> " = TableDesc"
         , "  { tdName      = " <> tshow n
         , "  , tdTableUID  = " <> tableUIDName n
         , "  , tdObjectUID = " <> tableObjectUIDName n
         , "  , tdColumns   =" ]
      <> zipWith colLine [0 ..] (drColumns d)
      <> [ "      ]", "  }" ]
