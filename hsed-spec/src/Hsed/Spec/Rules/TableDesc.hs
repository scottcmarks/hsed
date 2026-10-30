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
@u\<T\>Table@ and @u\<T\>TableObject@ and whose columns name their Type object's
@u\<type\>Type@, both by name from the modules the two rules write.

The module also reads, for the rules whose tables a description types
(@constValues@'s default values, @preconfig@'s rows): what a column holds
('ColKind', from its Type object's Format and, for an enumeration, its
enumeration table, "Hsed.Spec.Rules.EnumValues"), a cell read as that kind
('readCell') and the runtime 'Hsed.Core.Table.Cell' it prints as
('renderCell').
-}
module Hsed.Spec.Rules.TableDesc
  ( tableDescRule
  , tableDescsModule
    -- * Reading the bound tables
  , DescRead (..)
  , ColRead (..)
  , descReads
  , descIdent
  , descDeclName
  , descSection
  , parseColNumber
    -- * What a column holds
  , ColKind (..)
  , colKind
  , CellValue (..)
  , readCell
  , renderCell
  , colNumberLit
  ) where

import qualified Data.Char                 as DC
import qualified Data.Text                 as DT
import qualified Data.Text.Read            as TR
import qualified RIO.ByteString            as B
import qualified RIO.Char                  as C
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           Numeric                   (showHex)
import           RIO

import           Hsed.Core.Types           (Operand (..), RestrictedKind (..), TableRef (..), TypeDef (..),
                                            TypeRef (..), formatName)
import           Hsed.Spec.Anchor          (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common          (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.DSL             (SomeBinding (..), tableDesc)
import           Hsed.Spec.Format          (nearest)
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.IR              (CellIR (..), TableIR (..))
import           Hsed.Spec.Readers.Text    (lowerIdent)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.EnumValues (EnumRead (..), enumReads, parseEnumValue)
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..), tableObjectUIDName, tableUIDName, tableUIDRows, tableUIDsModule,
                                            tableUIDsNamed)
import           Hsed.Spec.Rules.TypeRow   (TypeRowRead (..), typeRowReads, typeUIDName, typesModule)
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

readDescs :: Context -> [BoundTable] -> [DescRead]
readDescs ctx = map readDesc
  where
    types = Map.fromListWith (\_ firstOne -> firstOne) [ (trName tr, tr) | tr <- rights (typeRowReads ctx) ]
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

-- | What a column of a described table holds, as its Type object's Format says.
data ColKind
  = KBytes !Int                    -- ^ a Simple_Type of @bytes@: that many bytes (@uid@: eight)
  | KText !Int                     -- ^ a Simple_Type of @max_bytes@: text of at most that many bytes (@name@)
  | KUInt !Int                     -- ^ a Simple_Type of @uinteger@: an unsigned integer of that many bytes
  | KInt !Int                      -- ^ a Simple_Type of @integer@
  | KEnum !Text ![(Int, Int)] ![(Text, Int)]
    -- ^ an Enumeration_Type: its name, its ranges and, from its enumeration table, each label's first value
  | KRef ![TableRef]               -- ^ a Restricted_Reference_Type{6}: an object of one of the tables
  deriving (Eq, Show)

-- | What a column of a type holds, or why it is not read from a cell. A
--   Simple_Type's base is one of the four base types of Core 5.1.1 (@bytes@,
--   @max_bytes@, @uinteger@, @integer@), a Type object whose Format is
--   @Base_Type@.
colKind :: Context -> Text -> Either Text ColKind
colKind ctx name = do
  tr <- maybe (Left (name <> " names no Type object")) Right (lookupType name)
  td <- first (const (name <> "'s Format did not parse")) (trTypeDef tr)
  case td of
    SimpleType (Ref base) (Lit n) -> do
      btr <- maybe (Left (name <> "'s base " <> base <> " names no Type object")) Right (lookupType base)
      case trTypeDef btr of
        Right BaseType -> case base of
          "bytes"     -> Right (KBytes n)
          "max_bytes" -> Right (KText n)
          "uinteger"  -> Right (KUInt n)
          "integer"   -> Right (KInt n)
          _           -> Left (name <> "'s base " <> base <> " is not a base type of Core 5.1.1")
        _ -> Left (name <> "'s base " <> base <> " is not a Base_Type")
    EnumType ranges -> do
      rs <- maybe (Left (name <> "'s range is not literal")) Right (traverse lits ranges)
      Right (KEnum name rs (labels name))
    RestrictedRef ObjectTableUID tables -> Right (KRef tables)
    other -> Left ("a column of type " <> name <> " (" <> formatName other <> ") is not read from a table cell")
  where
    types = rights (typeRowReads ctx)
    lookupType n = L.find ((== n) . trName) types
    lits (Lit a, Lit b) = Just (a, b)
    lits _              = Nothing
    -- each label of the type's enumeration table with the first value it is given
    labels n = L.nubBy (\a b -> fst a == fst b)
      [ (T.strip label, v)
      | er <- rights (enumReads ctx), erIdent er == Just n
      , (cell, label) <- erRows er, Just (v : _) <- [parseEnumValue (erRangeWords er) cell] ]

-- | A cell read as what its column holds.
data CellValue
  = CvBytes ![Word8]
  | CvText !Text
  | CvUInt !Integer !(Maybe Int)   -- ^ the value, and the hex digits it is printed with (Nothing: in decimal)
  | CvInt !Integer
  | CvEnum !Text !Int
  | CvRef !Text ![Word8]           -- ^ an object named, and its UID
  deriving (Eq, Show)

-- | Read a non-blank cell as a column of a kind holds it; an object reference
--   is resolved by the caller's function from the object's name and the tables
--   the type allows to the object's UID.
readCell :: (Text -> [TableRef] -> Either Text [Word8]) -> ColKind -> Text -> Either Text CellValue
readCell resolve kind raw = case kind of
  KBytes n -> case bytesOf cell of
    Just bs | length bs == n -> Right (CvBytes bs)
    _ -> Left (tshow cell <> " is not " <> tshow n <> " bytes in hex")
  KText n
    | T.null cell                    -> Left "the text is empty"
    | B.length (encodeUtf8 cell) > n -> Left (tshow cell <> " is longer than " <> tshow n <> " bytes")
    | otherwise                      -> Right (CvText cell)
  KUInt n -> do
    v <- maybe (Left (tshow cell <> " is not an unsigned integer")) Right (number cell)
    if v < 256 ^ n then Right (CvUInt v (hexDigits cell)) else Left (tshow cell <> " does not fit in " <> tshow n <> " bytes")
  KInt n -> do
    v <- maybe (Left (tshow cell <> " is not an integer")) Right (signed cell)
    let bound = 2 ^ (8 * n - 1)
    if negate bound <= v && v < bound then Right (CvInt v) else Left (tshow cell <> " does not fit in " <> tshow n <> " bytes")
  KEnum name ranges labelled -> case (lookup cell labelled, number cell) of
    (Just v, _) -> Right (CvEnum cell v)
    (Nothing, Just v)
      | any (\(lo, hi) -> toInteger lo <= v && v <= toInteger hi) ranges -> Right (CvEnum (tshow v) (fromIntegral v))
    _ -> Left (tshow cell <> " is no label of " <> name <> "'s enumeration ("
               <> T.intercalate ", " (map fst labelled) <> ")")
  KRef tables -> CvRef cell <$> resolve cell tables
  where
    cell = T.strip raw
    hexDigits t = T.length <$> T.stripPrefix "0x" (T.toLower t)
    number t = case T.stripPrefix "0x" (T.toLower t) of
      Just h  -> either (const Nothing) (\(v, rest) -> if T.null rest then Just v else Nothing) (TR.hexadecimal h)
      Nothing -> either (const Nothing) (\(v, rest) -> if T.null rest then Just v else Nothing) (TR.decimal t)
    signed t = case T.stripPrefix "-" t of
      Just rest -> negate <$> number rest
      Nothing   -> number t
    bytesOf t = let hex = T.filter (not . C.isSpace) t
                in if even (T.length hex) && T.all C.isHexDigit hex
                     then Just (map byte (DT.chunksOf 2 hex)) else Nothing
    byte = T.foldl' (\acc c -> acc * 16 + fromIntegral (DC.digitToInt c)) 0

-- | The runtime 'Hsed.Core.Table.Cell' a value prints as; Left for a kind the
--   runtime has no cell for (bytes other than a UID's eight).
renderCell :: CellValue -> Either Text Text
renderCell = \case
  CvBytes bs | length bs == 8 -> Right ("UIDCell (" <> Gen.uidExpr bs <> ")")
             | otherwise      -> Left ("a cell of " <> tshow (length bs) <> " bytes has no runtime cell yet")
  CvText t                    -> Right ("TextCell " <> tshow t)
  CvUInt v digits             -> Right ("UIntCell " <> maybe (tshow v) (hexLiteral v) digits)
  CvInt v                     -> Right ("IntCell " <> if v < 0 then "(" <> tshow v <> ")" else tshow v)
  CvEnum l v                  -> Right ("EnumCell " <> tshow l <> " " <> tshow v)
  CvRef n bs                  -> Right ("RefCell " <> tshow n <> " (" <> Gen.uidExpr bs <> ")")
  where hexLiteral v w = "0x" <> T.justifyRight w '0' (T.toUpper (T.pack (showHexInteger v)))

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
                   , if crUnique cr == "Yes" then "True" else "False", tshow (crType cr)
                   , maybe "Nothing" (\tr -> "(Just " <> typeUIDName (trName tr) <> ")") (crTypeRow cr) ]
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
