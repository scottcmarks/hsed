{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

{-|
Module      : Hsed.Spec.Rules.Preconfig
Description : The preconfig rule: the rows a template or an SSC gives a table, typed by its description
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E12. @preconfig@ fires on a table of at least
two columns whose caption is @… \<T\> Table Preconfiguration@ (Opal) or
@Default … \<Ts\>@ (Core): in Core 2.01, Table 209 @Default Base Template
Authorities@ (section 5.3.4.1.2) and Table 216 @Default Admin Template
Authorities@ (5.4.4.4). Their captions are theirs alone across Core, so the
rule rides on the @doc@ anchor (design 13.24). In Opal 2.30 since M4b it is
anchored at section 4.2 (PDF 35) and binds the Admin SP's twelve tables,
Tables 18-25, 27-29 and 33 (PDF 35-52); the Locking SP's are M5's.

A table's target is the table its caption names (@Authority@ in @… Authority
Table Preconfiguration@; in @Default … Authorities@ the last word, a plural:
@Authority@ + @s@, @es@, or @y@ made @ies@), read through that table's
description, the spec's or its base's, with the spec's column additions
("Hsed.Spec.Rules.ColumnsAddition".'Hsed.Spec.Rules.ColumnsAddition.allDescs':
Opal's TPerInfo with Table 26's ProgrammaticResetEnable, PDF 49). An Opal
caption names its SP before a dash ('spOf'); the SP scopes the rows, their
names and their UIDs, so that at M5 the Locking SP's Admin1 never meets the
Admin SP's. A column whose header mentions @informative@ is dropped; where
that is the first column, a row whose first cell alone is printed is a group
row, the context of the rows below it (Opal T22, T23, T38, T39), not a row of
the target; elsewhere a row of one printed cell is a row of the target, its
blank cells their defaults (since the M4b review: a UID-only row is never
dropped unread). A header names
its target's column as the headers compare (whitespace and case aside), else
the one column whose name it begins (Opal Table 29's @LifeCycle@ for Core
Table 215's LifeCycleState, PDF 50, 232: @N-PRECONF-HEADER@; the lead's
answer 1).

Each cell is read as an SSC prints it ("Hsed.Spec.Readers.Cell": its
footnote markers and a digit after its placeholder the row's notes, @(O)@ the
row optional, quotes removed, line breaks joined as its column's kind reads
them, @VU@ and @MSID@ values the TPer chooses) and then as its column's type
says ('Hsed.Spec.Readers.Cell.readCellWith'), a reference naming the object
by the Name column of a row of the SP preconfigured for a table the type
allows (case and spaces aside), or by its UID; where the SP preconfigures no
row of those tables, a row another SP preconfigures (Core 5.1.3.78, PDF 123:
a Template_object_ref is \"a uidref to an object in the Admin SP's Template
table\", which the Locking SP's SPTemplates rows print, Opal Table 35, PDF
53, at M5); a blank cell keeps the column's default. A type with no
enumeration table of its own takes the labels its section cross-references,
read by the @codes@ rule (Core's @messaging_type@, section 5.1.3.55, PDF
116: Table 179, PDF 167; 'preconfTypeEnv'). A row
carrying @(+XX)@ is a template, one row for each instance the TPer holds, its
columns that carry the placeholder listed (its UID or InvokingID, names and
references carrying the placeholder's letters: Table 24's AdminXX, PDF 48);
an access control row whose InvokingID is @TT TT TT TT@ after a table's
half-UID stands for every object of that table (*AC1-*AC7, PDF 38-39) and has
no InvokingID cell ("Hsed.Spec.Readers.AccessControl"). The AccessControl
and ACE tables are read through "Hsed.Spec.Readers.AccessControl": each
ACE's Columns in each table the rows naming it invoke on, each BooleanExpr
in postfix.

Checks, each with the code its findings carry:

* @C-PRECONF-HEADER@: the caption names a described table, and every header
  cell kept names one of its columns (exactly, else as the one column whose
  name it begins), no column twice, the UID column among them;
* @C-UID-TABLE@: every row's UID lies in the target table: its upper half is
  the half-UID the table of table UIDs gives the target
  ("Hsed.Spec.Rules.TableUIDs".'Hsed.Spec.Rules.TableUIDs.allTableUIDRows':
  Core's Table 240, or a row of an SP's Table table, Opal Table 20 row 12's
  DataRemovalMechanism); a table whose every row leaves its UID column blank
  has UIDs the TPer assigns (Core 5.3.2.7.1, PDF 162: the AccessControl
  table's), the Note @N-PRECONF-UID-TPER@ (Opal Table 22, PDF 39-46);
* @C-UID-UNIQUE@: no two rows of an SP preconfigured for one table share a
  UID (a template's base, which is no instance's, aside);
* @C-PRECONF-CELL@: every printed cell reads as its column's type (an
  enumeration's label, a name's text, a reference to a preconfigured row by
  its Name or its UID; a credential's PIN printed in quotes, unless it is
  @VU@ or @MSID@, so that Opal Table 41's @SID or MSID@, PDF 84, is never
  read as its letters); an access control or ACE row's references and
  expressions are "Hsed.Spec.Readers.AccessControl"'s @C-ACL-REFS@ and
  @C-ACL-EXPR@; a label printed otherwise is @N-PRECONF-LABEL@ and a name
  longer than its type @W-PRECONF-NAME-LENGTH@ ("Hsed.Spec.Readers.Cell"'s);
  a header that fails C-PRECONF-HEADER hides nothing of the columns it does
  name: their cells are read, and every finding about them (these, the
  UIDs', the templates', the legend's) reported as a passing header's;
* @W-PRECONF-TEMPLATE-NAME@: a template row whose Name does not carry its
  placeholder's letters, which would give every instance one Name (Opal
  Table 39 row 13, PDF 79, at M5: @(+NN NN)@ and @ACE_UserMMMM_Set_CommonName@);
* @W-PRECONF-DEFAULT@: a column of an SSC's table whose type gives no
  default ('columnDefaults'; Opal Table 43's @uniteger_8@, PDF 85, at M5),
  which a row that leaves it blank would hold no value in;
* the legend's classes and findings for each kept cell of an SSC's table
  ("Hsed.Spec.Rules.Legend".'Hsed.Spec.Rules.Legend.cellAccess': its codes,
  located at the cell), where the spec has a legend;
* @C-UID-NAME@ (the @uidTable@ rule's): an SSC row's generated UID name
  ('uidDeclName') is no other row's of the SP (the SP's module defines it),
  nor one the spec or its base names another UID
  ("Hsed.Spec.Rules.UidTable"; 'uidNames');
* @C-PRECONF-UID@: the table of a table's UIDs, which the @uidTable@ rule
  reads ("Hsed.Spec.Rules.UidTable": its header or caption claims every row
  for that table, Core Table 243 for the Authority table), gives exactly the
  rows preconfigured for that table ('preEntries': Tables 209 and 216), UID
  for UID, each with the same name and in the template the
  preconfiguration's caption names; one finding per difference, at the table
  of UIDs (at its row, where the difference is one). Rows are matched by
  UID: their order is not compared (Table 243 lists Table 209's rows, then
  Table 216's, but a reordering would pass). And each row an SP's
  preconfiguration gives the Table table is the descriptor object of the
  table its Name names, the one a table of table UIDs of the spec or its
  base gives it, else the first row an SP's Table table gives a table no
  such table names, so that no table has two (Core 3.2.5.3 b.i, PDF 51-52).

Compiled into 'preconfigModule' for a table whose caption names no SP (Core):
per table the caption's words in camel case (@defaultBaseTemplateAuthorities
:: [Pre Row]@), one 'Hsed.Core.Table.Pre' per row but a template's, giving
its cells by column number, with no access (@[]@). A table whose caption
names its SP (@Admin SP - C_PIN Table Preconfiguration@) goes to that SP's
module ('preconfigModuleFor': @Hsed.Gen.Opal.Preconfig.AdminSP@) as a
@PreconfTable@ of "Hsed.Core.Preconfig" ('preconfTableDecl': the target's
description from the module that defines it, each column's default but the
UID's from its kind, 'Hsed.Spec.Readers.Cell.zeroValue', every row with its
kind, its optional mark, its cells and its notes, and the legend's access of
its kept cells); with the SP's index of its tables in table order
(@adminSP :: [PreconfTable]@) and a UID for each plain row with a Name,
@u\<T\>_\<Name\>@ ('uidDeclName'), one generated name for each object: none
for a row whose object a generated module of the spec or its base names
already, under the same name or under its table's (a table's descriptor
object a TableUIDs module declares, a single-row table's row); a Table table
row of a table no module describes keeps its own (M5's DataStore, Opal Table
36, PDF 54), and a row at a UID the base gives another object's Name keeps
its own, its comment naming the other ('uidNames').
-}
module Hsed.Spec.Rules.Preconfig
  ( preconfigRule
  , preconfigModule
  , preconfigModuleFor
    -- * Reading the bound tables
  , PreRead (..)
  , PreCol (..)
  , PreRow (..)
  , RowRead (..)
  , RowShape (..)
  , CellRead (..)
  , preReads
  , preconfTypeEnv
  , targetNoun
  , spOf
  , captionTemplate
  , PreEntry (..)
  , preEntries
    -- * Declarations
  , preDeclName
  , preconfTableDecl
  , cellClasses
  , uidDeclName
  ) where

import qualified Data.Text                       as DT
import qualified RIO.Char                        as C
import qualified RIO.List                        as L
import qualified RIO.Map                         as Map
import qualified RIO.Set                         as Set
import qualified RIO.Text                        as T
import           RIO

import           Hsed.Core.Code                  (Span (..))
import           Hsed.Core.Preconfig             (Placeholder (..))
import           Hsed.Core.Table                 (Access)
import           Hsed.Core.Types                 (TableRef (..))
import           Hsed.Spec.Anchor                (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common                (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common                as Cm
import           Hsed.Spec.DSL                   (SomeBinding (..), preconfig)
import           Hsed.Spec.Format                (globMatch)
import qualified Hsed.Spec.Gen                   as Gen
import           Hsed.Spec.IR                    (CellIR (..), SpecIR (..), TableIR (..), XrefIR (..))
import           Hsed.Spec.Readers.AccessControl (Invocation (..), aceHalves, aceNamedHalves, aclCode, aclColumn, columnDependent,
                                                  everyObject, invokedHalf, isAccessControl, thisSP)
import           Hsed.Spec.Readers.Cell          (CellValue (..), ColKind (..), PrintedCell (..), Problem (..), Remark (..),
                                                  Resolver (..), TypeEnv (..), colKind, hasValue, joinedText, listItems,
                                                  printedCell, printedText, problemText, readCellWith, renderCell,
                                                  vendorMarker, zeroValue)
import           Hsed.Spec.Readers.Text          (lowerIdent, nameKey, readNumber, titleIdent)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.Codes           (CodeRead (..), CodeRowRead (..), cellSpan, codeReads)
import           Hsed.Spec.Rules.ColumnsAddition (allDescs)
import           Hsed.Spec.Rules.Legend          (LegendScale, cellAccess, legendScale)
import           Hsed.Spec.Rules.TableDesc       (ColRead (..), DescRead (..), colNumberLit, declaredTableObjects, descDeclName,
                                                  descReads, tableDescsModule, typeEnv)
import           Hsed.Spec.Rules.TableUIDs       (TableUIDRow (..), allTableUIDRows)
import           Hsed.Spec.Rules.TypeRow         (TypeRowRead (..), allTypeRows)
import           Hsed.Spec.Rules.UidTable        (UidRow (..), UidValue (..), asName, tableClaim, uidRows)
import qualified Hsed.Spec.Rules.UidTable        as UidTable (rowLabel)
import           Hsed.Spec.Shape                 (Shape (..), captionTitle, normHeader, normaliseCaption, shapeMismatch)


-- | The rule.
preconfigRule :: RuleDef
preconfigRule = RuleDef
  { ruleName    = RuleName "preconfig"
  , bindsTo     = SomeBinding preconfig
  , exemplar    = (SpecId "core-2.01", TableId 209)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch Preconfig form)
  , ruleIdent   = const Nothing
  , ruleModule  = preconfigModule
  , ruleCodes   =
      [ CodeDef "C-PRECONF-HEADER"   [Error]          (Defect "a preconfiguration table's columns are not its table's")
      , CodeDef "C-PRECONF-CELL"     [Error]          (Defect "a preconfigured cell does not read as its column's type")
      , CodeDef "C-PRECONF-UID"      [Error]          (Defect "the table of a table's UIDs disagrees with the rows preconfigured for that table")
      , CodeDef "N-PRECONF-HEADER"   [Note]           (AsPrinted "a header read as the one column of its table whose name it begins (LifeCycle as LifeCycleState)")
      , CodeDef "N-PRECONF-UID-TPER" [Note]           (AsPrinted "a preconfiguration table that prints no UID in any row, its UIDs read as the TPer's to assign")
      , CodeDef "W-PRECONF-TEMPLATE-NAME" [Warning]   (Defect "a template row whose Name does not carry its placeholder's letters, so that its instances share one Name")
      , CodeDef "W-PRECONF-DEFAULT"  [Warning]        (Defect "a column of an SSC's preconfiguration table whose type gives it no default") ]
  , ruleChecks  = \ctx bts -> let ps = readPres ctx bts
                              in concatMap (preFindings ctx) ps <> uidUnique ctx ps <> snd (uidNames ctx ps) <> preconfUID ctx ps
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.Preconfig@.
preconfigModule :: Gen.GenModule
preconfigModule = Gen.GenModule
  { Gen.leaf        = "Preconfig"
  , Gen.description = "Preconfigured rows: the rows a template or an SSC gives a table, by column number"
  , Gen.notes       = [ "A column a row does not give keeps its default; template rows ((+XX)) are not generated yet." ] }

-- | The module of an SP's preconfiguration tables, under 'preconfigModule':
--   @Hsed.Gen.Opal.Preconfig.AdminSP@ for the @Admin SP@.
preconfigModuleFor :: Text -> Gen.GenModule
preconfigModuleFor sp = Gen.GenModule
  { Gen.leaf        = preconfigModule.leaf <> "." <> titleIdent sp
  , Gen.description = "The " <> sp <> "'s preconfiguration: each table's description, its columns' defaults and its rows"
  , Gen.notes       = [] }

-- | The runtime module of an SSC's preconfiguration tables, which only this
--   rule's declarations import.
preconfRuntime :: Text
preconfRuntime = "Hsed.Core.Preconfig"


-- * Reading the bound tables

-- | One preconfiguration table read under the errata set.
data PreRead = PreRead
  { pBound    :: !BoundTable
  , pNoun     :: !(Maybe Text)              -- ^ the target as the caption names it (@Authorities@)
  , pTarget   :: !(Either Text DescRead)    -- ^ the target's description, or why there is none
  , pHeader   :: ![(Int, Text)]             -- ^ the columns kept: index in the table and header cell
  , pDropped  :: ![Text]                    -- ^ the informative header cells, whose columns are dropped
  , pRows     :: ![PreRow]                  -- ^ the rows that are not group rows
  , pSP       :: !(Maybe Text)              -- ^ the SP the caption names ('spOf'); Core's name none
  , pDescSpec :: !(Maybe SpecId)            -- ^ the spec whose table descriptions define the target's description
  , pColumns  :: ![PreCol]                  -- ^ the columns kept, each with the target's column it names
  , pReads    :: ![RowRead]                 -- ^ each of 'pRows' read as its columns hold it
  }

-- | One kept column: its index in the table, its header cell, the target's
--   column it names (True: as the one column whose name it begins), and that
--   column's kind or why it has none.
data PreCol = PreCol
  { kcIndex  :: !Int
  , kcHeader :: !Text
  , kcColumn :: !(Maybe (ColRead, Bool))
  , kcKind   :: !(Either Text ColKind)
  }

-- | One row of a preconfiguration table.
data PreRow = PreRow
  { prRow         :: !Int                   -- ^ 0-based among the table's rows, group rows counted
  , prCells       :: ![CellIR]
  , prGroup       :: !(Maybe Text)          -- ^ the group row above it
  , prTemplate    :: !Bool                  -- ^ a template row: a cell carries @(+XX)@
  , prValues      :: ![(Text, Text)]        -- ^ each kept column's header cell and this row's value, its
                                            --   conventions read ("Hsed.Spec.Readers.Cell".'printedCell'),
                                            --   its lines joined as its column's kind reads them
  , prPrinted     :: ![(Int, PrintedCell)]  -- ^ every cell's conventions, by its index in the table
  , prNotes       :: ![Text]                -- ^ its cells' notes in column order, informative ones' included
  , prOptional    :: !Bool                  -- ^ a cell prints @(O)@
  , prPlaceholder :: !(Maybe Placeholder)   -- ^ a template's placeholder
  }

-- | One row read as its columns hold it.
data RowRead = RowRead
  { rdShape :: !RowShape
  , rdCells :: ![(PreCol, ColRead, CellRead)]   -- ^ each kept column with a value, in header order
  }

-- | What a row stands for.
data RowShape
  = PlainRow                         -- ^ one row, as printed
  | TemplateRow !Placeholder ![Int]  -- ^ one row for each instance, the columns carrying the placeholder by number
  | EveryObjectRow ![Word8]          -- ^ one row for each object of the table of this half-UID (its InvokingID not a cell)
  deriving (Eq, Show)

-- | One printed cell read.
data CellRead
  = ReadVendor !Text                 -- ^ a value the TPer chooses, as printed (an SSC's table's @VU@ or @MSID@)
  | ReadValue !CellValue ![Remark]
  | ReadFailed !Problem
  deriving (Eq, Show)

-- | Every table bound to the rule's shape, read under the errata set.
preReads :: Context -> [PreRead]
preReads ctx = readPres ctx (boundTables (Just Preconfig) ctx)

-- | The tables read, in anchor order: each against its target, then each row
--   against the names, the invocations and the tables of its SP's tables
--   (and the other SPs' rows, for a reference to a table its SP
--   preconfigures no row of: 'resolveRef').
readPres :: Context -> [BoundTable] -> [PreRead]
readPres ctx bts = [ p { pReads = map (readRow (envOf (pSP p)) p) (pRows p) } | p <- firsts ]
  where
    types    = preconfTypeEnv ctx
    descs    = allDescs ctx
    firsts   = map (readTable types descs) bts
    envs     = Map.fromList [ (sp, groupEnv ctx [ p | p <- firsts, pSP p == sp ] [ p | p <- firsts, pSP p /= sp ])
                            | sp <- L.nub (map pSP firsts) ]
    envOf sp = Map.findWithDefault (groupEnv ctx [] firsts) sp envs

-- | One table against its target: its kept columns, its rows and their cells' conventions.
readTable :: TypeEnv -> [(Context, DescRead)] -> BoundTable -> PreRead
readTable types descs bt = PreRead
  { pBound = bt, pNoun = noun, pTarget = target, pHeader = kept
  , pDropped = [ h | h <- header, isInformative h ], pRows = rows
  , pSP = title >>= spOf, pDescSpec = cxSpec . fst <$> found, pColumns = cols, pReads = [] }
  where
    t      = btRead bt
    title  = t.caption >>= captionTitle
    header = maybe [] (map (T.strip . (.text))) (listToMaybe t.headerRows)
    kept   = [ (i, h) | (i, h) <- zip [0 ..] header, not (isInformative h) ]
    noun   = title >>= targetNoun
    found  = noun >>= \n -> L.find (maybe False (names n) . drTable . snd) descs
    target = case (noun, found) of
      (Nothing, _)      -> Left "the caption names no table (\"… <T> Table Preconfiguration\" or \"Default … <Ts>\")"
      (Just n, Nothing) -> Left ("the caption names " <> n <> ", which no table description describes")
      (_, Just (_, d))  -> Right d
    cols = [ PreCol i h col (maybe (Left ("the header " <> tshow h <> " names no column")) (colKind types . crType . fst) col)
           | (i, h) <- kept, let col = either (const Nothing) (`columnOf` h) target ]
    rows = catMaybes (snd (L.mapAccumL step Nothing (zip [0 ..] t.rows)))
    step grp (i, cells)
      | isGroup cells = (Just (groupText cells), Nothing)
      | otherwise     = (grp, Just (preRow cols i cells grp))
    -- a group row prints its first cell alone, in an informative first column the table drops (Opal T22, T23,
    -- T38, T39); elsewhere a row of one printed cell is a row of the target, its blank cells their defaults
    informativeFirst = maybe False isInformative (listToMaybe header)
    isGroup :: [CellIR] -> Bool
    isGroup cells = informativeFirst && case map (T.strip . (.text)) cells of
      firstCell : rest@(_ : _) -> not (T.null firstCell) && all T.null rest
      _                        -> False

-- | One row's cells, their conventions read.
preRow :: [PreCol] -> Int -> [CellIR] -> Maybe Text -> PreRow
preRow cols i cells grp = PreRow
  { prRow = i, prCells = cells, prGroup = grp
  , prTemplate    = isJust inside
  , prValues      = [ (kcHeader kc, maybe "" (joinedText (rightToMaybe (kcKind kc))) (lookup (kcIndex kc) printed)) | kc <- cols ]
  , prPrinted     = printed
  , prNotes       = concatMap (pcNotes . snd) printed
  , prOptional    = any (pcOptional . snd) printed
  , prPlaceholder = inside >>= placeholderOf }
  where
    printed = zip [0 ..] (map printedCell cells)
    inside  = listToMaybe (mapMaybe (pcPlaceholder . snd) printed)

-- | A group row's label: its first cell, a name broken over lines joined.
groupText :: [CellIR] -> Text
groupText cells = case map printedCell (take 1 cells) of
  [pc] | let tight = T.concat (pcLines pc), not (T.null tight), T.all (\c -> C.isAlphaNum c || c == '_') tight -> tight
       | otherwise -> T.unwords (pcLines pc)
  _ -> ""

-- | A placeholder from what @(+…)@ holds: its letters as the names print them
--   (@XX XX@ as @XXXX@), a byte for each two (Hsed.Core.Preconfig's 'Placeholder').
placeholderOf :: Text -> Maybe Placeholder
placeholderOf inside = case T.filter (not . C.isSpace) inside of
  ""      -> Nothing
  letters -> Just (Placeholder letters (max 1 (T.length letters `div` 2)))

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- | Does a caption's target name (@Authorities@) name a described table (@Authority@)?
names :: Text -> Text -> Bool
names noun n = noun `elem` ([n, n <> "s", n <> "es"] <> maybe [] (\stem -> [stem <> "ies"]) (T.stripSuffix "y" n))

-- | Is a column informative: does its header mention it?
isInformative :: Text -> Bool
isInformative h = "informative" `T.isInfixOf` normHeader h

-- | The target a preconfiguration caption's title names: the word before
--   @Table Preconfiguration@, or the last word of @Default … \<Ts\>@.
targetNoun :: Text -> Maybe Text
targetNoun title
  | Just before <- T.stripSuffix " Table Preconfiguration" title = lastWord before
  | "Default " `T.isPrefixOf` title = lastWord title
  | otherwise = Nothing
  where lastWord = fmap T.strip . listToMaybe . reverse . T.words

-- | The SP a preconfiguration caption's title names, the words before its
--   first dash: @Admin SP@ in @Admin SP - C_PIN Table Preconfiguration@ (Opal
--   prints a hyphen or an en dash, PDF 35-89); none in Core's
--   @Default … Authorities@, which name no SP.
spOf :: Text -> Maybe Text
spOf title = do
  before <- T.stripSuffix " Table Preconfiguration" title
  listToMaybe (L.sortOn T.length
    [ sp | sep <- [" - ", " \x2013 ", " \x2014 "], let (sp, rest) = DT.breakOn sep before
         , not (T.null rest), not (T.null (T.strip sp)) ])

-- | The template a Core caption names: @Base@ in @Default Base Template Authorities@.
captionTemplate :: PreRead -> Maybe Text
captionTemplate p = do
  title <- (btRead (pBound p)).caption >>= captionTitle
  rest  <- T.stripPrefix "Default " title
  let (template, after) = DT.breakOn " Template " rest
  guard (not (T.null after) && not (T.null template))
  pure template

-- | The target's column a header cell names: the one its name is, whitespace
--   and case aside; else the one column whose name it begins (True).
columnOf :: DescRead -> Text -> Maybe (ColRead, Bool)
columnOf d h = case L.find ((== normHeader h) . normHeader . crName) (drColumns d) of
  Just c  -> Just (c, False)
  Nothing -> case beginning d h of
    [c] -> Just (c, True)
    _   -> Nothing

-- | The target's columns whose names a header cell begins.
beginning :: DescRead -> Text -> [ColRead]
beginning d h = [ c | not (T.null key), c <- drColumns d, key `T.isPrefixOf` normHeader (crName c) ]
  where key = normHeader h

-- | A row's value in the target's column of a name (@UID@, @Name@), blank without one.
valueAt :: PreRead -> PreRow -> Text -> Text
valueAt p r name = fromMaybe "" (listToMaybe [ v | (kc, (_, v)) <- zip (pColumns p) (prValues r), Just (c, _) <- [kcColumn kc], crName c == name ])

-- | The types a preconfiguration's columns are read by: the spec's and its
--   base's ("Hsed.Spec.Rules.TableDesc".'typeEnv'), an enumeration with no
--   labels of its own taking those of the code table its type's section
--   cross-references, read by the @codes@ rule in the Context that owns the
--   type (Core's @messaging_type@, 5.1.3.55, PDF 116: "The enumeration values
--   and their associations defined in Table 179", PDF 167; 'None' 0).
preconfTypeEnv :: Context -> TypeEnv
preconfTypeEnv ctx = env { teLabels = \n -> case teLabels env n of
                                              [] -> xrefLabels ctx n
                                              ls -> ls }
  where env = typeEnv ctx

-- | The labels of the code tables a type's section cross-references: each
--   row's first value and its first other cell.
xrefLabels :: Context -> Text -> [(Text, Int)]
xrefLabels ctx n = L.nubBy (\a b -> fst a == fst b)
  [ lv
  | (c, tr) <- take 1 [ ctr | ctr@(_, tr) <- allTypeRows ctx, trName tr == n ]
  , Just sec <- [join (Map.lookup (trTable tr).structPath (cxSections c))]
  , k <- L.nub [ x.number | x <- (cxIR c).xrefs, x.section == Just sec ]
  , Right cr <- codeReads c, btAnchor (crBound cr) == TableId k
  , lv <- [ (T.strip l, fromInteger v) | row <- crRows cr, Just v <- [firstValue (cellSpan (rrCode row))]
                                       , l : _ <- [rrCells row], not (T.null (T.strip l)) ] ]
  where
    firstValue = \case
      One v       -> Just v
      Between a _ -> Just a
      Above a     -> Just (a + 1)
      Others      -> Nothing


-- * An SP's tables read together

-- | What reading a row needs of its SP's tables (Core's are one group): every
--   row with a UID (its table, its Name, blank without one, its UID), every
--   such row of the other SPs, the half-UIDs of the tables the SP holds,
--   every access control row's invocation, each table's name by its
--   half-UID, and every table description, the spec's and its base's.
data GroupEnv = GroupEnv
  { geRows        :: ![(Text, Text, [Word8])]
  , geElsewhere   :: ![(Text, Text, [Word8])]
  , geTableHalves :: ![[Word8]]
  , geInvocations :: ![Invocation]
  , geTables      :: !(Map [Word8] Text)
  , geDescs       :: ![DescRead]
  }

-- | An SP's tables read together, beside the other SPs' tables.
groupEnv :: Context -> [PreRead] -> [PreRead] -> GroupEnv
groupEnv ctx ps others = GroupEnv
  { geRows        = own
  , geElsewhere   = elsewhere
  , geTableHalves = if null spTables then L.nub [ take 4 t | (_, tu) <- allTableUIDRows ctx, Just t <- [tuTable tu] ] else spTables
  , geInvocations = [ invocation p r | p <- ps, isAccessControl (rights (map kcKind (pColumns p))), r <- pRows p ]
  , geTables      = Map.fromListWith (\_ old -> old) [ (take 4 t, tuName tu) | (_, tu) <- allTableUIDRows ctx, Just t <- [tuTable tu] ]
  , geDescs       = map snd (allDescs ctx) }
  where
    own       = [ (tn, name, uid) | (tn, name, uid, _, _) <- index ps ]
    elsewhere = [ (tn, name, uid) | (tn, name, uid, _, _) <- index others ]
    -- the tables the SP's Table table holds, each row's lower half (Core 3.2.5.3 b.i-b.ii, PDF 51-52); the
    -- tables the spec and its base name, where the SP preconfigures no Table table
    spTables = L.nub [ drop 4 uid | (tn, _, uid, _, _) <- index ps, tn == "Table" ]
    invocation p r = Invocation
      { ivHalf     = listToMaybe [ h | (kc, _, pc) <- printedIn p r, kcKind kc == Right KUIDRef, Just h <- [invokedHalf (printedText pc)] ]
      , ivColumns  = or [ columnDependent (methodName k pc) | (kc, c, pc) <- printedIn p r, crName c == "MethodID", Right k <- [kcKind kc] ]
      , ivACEs     = concat [ listItems k (printedText pc) | (kc, c, pc) <- printedIn p r, Right k <- [kcKind kc], aclColumn k, crName c == "ACL" ]
      , ivMetaACEs = concat [ listItems k (printedText pc) | (kc, c, pc) <- printedIn p r, Right k <- [kcKind kc], aclColumn k, crName c /= "ACL" ] }
    printedIn p r = [ (kc, c, pc) | kc <- pColumns p, Just (c, _) <- [kcColumn kc], Just pc <- [lookup (kcIndex kc) (prPrinted r)] ]
    -- a MethodID by the Name of the row it names, printed as that Name or as its UID (since the M4b review's
    -- verification: Get printed as 00 00 00 06 00 00 00 16 holds its ACEs' Columns as Get does), else as printed
    methodName k pc = case refTables k of
      Just tables | Right (n, _) <- resolveAmong own elsewhere (printedText pc) tables -> n
      _                                                                                -> printedText pc
    refTables = \case
      KRef tables  -> Just tables
      KDefault _ k -> refTables k
      _            -> Nothing

-- | Whether a UID is one an access control row of the SP may invoke on
--   (Core 5.3.2.7.2, PDF 162: the SP, a table or an object): ThisSP, a
--   table's UID, the half of a table the SP holds and four zero bytes, or
--   the UID of an object the SP's preconfiguration gives, a template's base
--   among them.
invocable :: GroupEnv -> [Word8] -> Bool
invocable ge u = u == thisSP || (drop 4 u == [0, 0, 0, 0] && take 4 u `elem` geTableHalves ge) || u `elem` [ v | (_, _, v) <- geRows ge ]

-- | One row read: its shape, then each kept column with a value as its kind
--   holds it (an every-object row's InvokingID left out, but where its half
--   is no table's of the SP); an access control row's InvokingID held to the
--   SP ('invocable').
readRow :: GroupEnv -> PreRead -> PreRow -> RowRead
readRow ge p r = RowRead shape [ (kc, c, readOne kc pc) | (kc, c, pc) <- given, hasValue pc, not (wildcardCell kc pc) || not halfHeld ]
  where
    given = [ (kc, c, pc) | kc <- pColumns p, Just (c, _) <- [kcColumn kc], Just pc <- [lookup (kcIndex kc) (prPrinted r)] ]
    wildcard = listToMaybe [ h | (kc, _, pc) <- given, kcKind kc == Right KUIDRef, Just h <- [everyObject (printedText pc)] ]
    wildcardCell kc pc = isJust wildcard && kcKind kc == Right KUIDRef && isJust (everyObject (printedText pc))
    halfHeld = maybe True (`elem` geTableHalves ge) wildcard
    accessControl = isAccessControl (rights (map kcKind (pColumns p)))
    shape = case (prPlaceholder r, wildcard) of
      (Just ph, _)      -> TemplateRow ph (L.sort [ n | (_, c, pc) <- given, Just n <- [crNumber c], carries ph pc ])
      (Nothing, Just h) -> EveryObjectRow h
      _                 -> PlainRow
    -- a cell printing the placeholder, or a name or reference carrying its letters (AdminXX, C_PIN_AdminXX)
    carries ph pc = isJust (pcPlaceholder pc) || phLetters ph `T.isInfixOf` T.concat (pcLines pc)
    readOne kc pc = case (vendorMarker pc, kcKind kc) of
      (Just m, _) | isJust (pSP p) -> ReadVendor m
      (_, Left why)                -> ReadFailed (Unread why)
      (_, Right KUIDRef)
        | wildcardCell kc pc         -> ReadFailed (Unresolved (wildcardText pc))
        | accessControl              -> either ReadFailed (uncurry ReadValue) (readCellWith resolver KUIDRef (printedText pc) >>= heldToSP)
      (_, Right k)
        | isPassword k, not (pcQuoted pc) -> ReadFailed (Unread (tshow (T.unwords (pcLines pc)) <> " is a credential printed unquoted:"
                                                <> " a preconfiguration prints a PIN in quotes, or VU or MSID for one the TPer chooses"
                                                <> " (Opal Table 25, PDF 48-49)"))
        | otherwise -> either ReadFailed (uncurry ReadValue) (readCellWith resolver k (printedText pc))
    resolver = Resolver { rvRef = resolveRef ge, rvColumn = resolveColumn ge (valueAt p r "Name") }
    isPassword = \case
      KPassword _  -> True
      KDefault _ k -> isPassword k
      _            -> False
    heldToSP read'@(v, _) = case uidOf v of
      Just u | invocable ge u -> Right read'
      held -> Left (Unresolved ("the InvokingID " <> maybe (renderCell v) hexUID held <> " is not ThisSP, nor a table the SP's"
                                <> " Table table holds, nor an object the SP's preconfiguration gives (Core 5.3.2.7.2, PDF 162)"))
    uidOf = \case
      CvUID u   -> Just u
      CvRef _ u -> Just u
      _         -> Nothing
    wildcardText pc = "the InvokingID " <> tshow (T.unwords (pcLines pc)) <> " stands for every object of the table of the half-UID "
                      <> maybe "" hexUID wildcard <> ", which the SP's Table table does not hold (Opal *AC1-*AC7, PDF 38-39)"

-- | The object a name or a UID names among the SP's rows of the tables a type
--   allows: by its Name, case and spaces aside (the name kept as printed), or
--   by its UID, the UID of a row of one of those tables (the name its row
--   gives, else the UID as printed); where the SP preconfigures no row of
--   those tables, among the other SPs' rows of them ('resolveAmong').
resolveRef :: GroupEnv -> Text -> [TableRef] -> Either Text (Text, [Word8])
resolveRef ge = resolveAmong (geRows ge) (geElsewhere ge)

-- | 'resolveRef' among an SP's rows and the other SPs' (each its table, its
--   Name, blank without one, and its UID). The other SPs' rows of the tables
--   a type allows are read only where the SP preconfigures none, so that a
--   name the SP gives is its own (the Locking SP's Admin1 never the Admin
--   SP's), and a table only another SP holds is that SP's (Core 5.1.3.78,
--   PDF 123: a Template_object_ref is \"a uidref to an object in the Admin
--   SP's Template table\"; the Locking SP's SPTemplates rows print the Admin
--   SP's Template UIDs, Opal Table 35, PDF 53, at M5, its SP having no
--   Template table).
resolveAmong :: [(Text, Text, [Word8])] -> [(Text, Text, [Word8])] -> Text -> [TableRef] -> Either Text (Text, [Word8])
resolveAmong own elsewhere name tables = case parseUIDCell name of
  Just uid -> case [ n | (_, n, u) <- rows, u == uid ] of
    n : _ -> Right (if T.null n then name else n, uid)
    []    -> Left (tshow name <> " is the UID of no row preconfigured for " <> T.intercalate " or " (map tableRefText tables))
  Nothing  -> case L.nub [ u | (_, n, u) <- rows, not (T.null n), nameKey n == nameKey name ] of
    [uid] -> Right (name, uid)
    []    -> Left (tshow name <> " names no row preconfigured for " <> T.intercalate " or " (map tableRefText tables))
    uids  -> Left (tshow name <> " names " <> tshow (length uids) <> " rows with different UIDs")
  where
    allowed rs = [ r | r@(tn, _, _) <- rs, any (allows tn) tables ]
    rows = case allowed own of
      []    -> allowed elsewhere
      mine  -> mine
    allows tn = \case
      Table t     -> t == tn
      TableGlob g -> globMatch g tn
      TableStar   -> True
    tableRefText = \case
      Table t     -> t
      TableGlob g -> g
      TableStar   -> "*"

-- | The number of a column an ACE's Columns names, by its name or its
--   number, in each table an access control row of Get or Set whose ACL
--   names the ACE invokes on (Core 5.3.2.9.5, PDF 165): one number, the same
--   in them all. An ACE no such row names, but others do (a meta-ACL's, a
--   column-independent method's ACL), has Columns no method reads (PDF 165):
--   a name is read in the first table a row naming it invokes on that has it.
resolveColumn :: GroupEnv -> Text -> Text -> Either Text Int
resolveColumn ge ace item = case aceHalves (geInvocations ge) ace of
  [] -> case aceNamedHalves (geInvocations ge) ace of
    []    -> Left (tshow ace <> " is in no access control row's ACL, so no table holds the columns its Columns name")
    named -> case [ n | Right n <- map columnIn named ] of
      n : _ -> Right n
      []    -> Left (tshow item <> " is no column of the tables the rows naming " <> ace <> " invoke on ("
                     <> T.intercalate ", " (map tableName named) <> "), none of whose methods reads Columns")
  halves -> do
    ns <- traverse columnIn halves
    case L.nub ns of
      [n] -> Right n
      _   -> Left (tshow item <> " is not one column of the tables " <> ace <> " applies to: "
                   <> T.intercalate ", " [ tableName h <> "'s " <> colNumberLit n | (h, n) <- zip halves ns ])
  where
    tableName h = Map.findWithDefault (hexUID h) h (geTables ge)
    columnIn h = do
      let tname = tableName h
      d <- maybe (Left (tshow ace <> " applies to the " <> tname <> " table, which no table description describes")) Right
                 (L.find ((== Just tname) . drTable) (geDescs ge))
      let numbered = [ (n, c) | c <- drColumns d, Just n <- [crNumber c] ]
          where' = " of the " <> tname <> " table (" <> renderAnchorId (btAnchor (drBound d)) <> "), which " <> ace <> " applies to"
      case readNumber item of
        Just n | fromInteger n `elem` map fst numbered -> Right (fromInteger n)
               | otherwise -> Left (tshow item <> " is no column number" <> where')
        Nothing -> maybe (Left (tshow item <> " names no column" <> where')) Right
                         (listToMaybe [ n | (n, c) <- numbered, normHeader (crName c) == normHeader item ])

-- | Every row with a UID of a table with a target, by the target's name: its
--   Name column's value (blank without one), its UID, its table and row.
index :: [PreRead] -> [(Text, Text, [Word8], PreRead, PreRow)]
index ps =
  [ (fromMaybe "" (drTable d), valueAt p r "Name", uid, p, r)
  | p <- ps, Right d <- [pTarget p], r <- pRows p
  , Just uid <- [parseUIDCell (valueAt p r "UID")] ]

-- | One row preconfigured for a table, as the table of that table's UIDs
--   must give it.
data PreEntry = PreEntry
  { peTarget   :: !Text          -- ^ the target's name, @Authority@
  , peName     :: !Text          -- ^ its Name column's cell, blank without one
  , peUID      :: ![Word8]
  , peTemplate :: !(Maybe Text)  -- ^ the template its table's caption names ('captionTemplate')
  , peWhere    :: !Text          -- ^ its table and row, @T209 row 1@
  }

-- | Every row preconfigured for a table, in anchor and row order (the
--   template rows left out): what the table of that table's UIDs must give.
preEntries :: Context -> [PreEntry]
preEntries ctx = entriesOf (preReads ctx)

entriesOf :: [PreRead] -> [PreEntry]
entriesOf ps =
  [ PreEntry tn name uid (captionTemplate p) (renderAnchorId (btAnchor (pBound p)) <> " row " <> tshow (prRow r + 1))
  | (tn, name, uid, p, r) <- index ps, not (prTemplate r) ]


-- * Checks

rowLabel :: PreRow -> Text
rowLabel r = "row " <> tshow (prRow r + 1) <> maybe "" (" " <>) (lookup "Name" (prValues r) <|> nameLike) <> ": "
  where nameLike = listToMaybe [ v | (h, v) <- prValues r, normHeader h == "name", not (T.null v) ]

-- | Every finding about one table: its target and header, then each row, then
--   the labels it reads otherwise.
preFindings :: Context -> PreRead -> [Finding]
preFindings ctx p = case pTarget p of
  Left why -> [atTable ctx bt (finding Error "C-PRECONF-HEADER" why)]
  -- a header that fails hides nothing of the columns it does name (the M4b review, and its verification): every
  -- finding about them follows, as a passing header's would, the UIDs' where it names the UID column
  Right d  -> map (atTable ctx bt . finding Error "C-PRECONF-HEADER") (headerProblems d)
              <> headerNotes d <> tperNote <> defaultFindings d <> concat (zipWith (rowFindings d) (pRows p) (pReads p)) <> labelNotes
  where
    bt = pBound p
    target d = renderAnchorId (btAnchor (drBound d)) <> " " <> fromMaybe "" (drTable d)
    headerProblems d =
      [ case beginning d (kcHeader kc) of
          cs@(_ : _ : _) -> "the header " <> tshow (kcHeader kc) <> " begins the names of " <> tshow (length cs) <> " columns of "
                              <> target d <> " (" <> T.intercalate ", " (map crName cs) <> "), and names none of them"
          _              -> "the header " <> tshow (kcHeader kc) <> " names no column of " <> target d
      | kc <- pColumns p, isNothing (kcColumn kc) ]
      <> [ "the headers " <> T.intercalate " and " (map tshow hs) <> " name one column of " <> target d
         | hs@(_ : _ : _) <- Map.elems (Map.fromListWith (flip (<>)) [ (crName c, [kcHeader kc]) | kc <- pColumns p, Just (c, _) <- [kcColumn kc] ]) ]
      <> [ "the header gives no UID column of " <> target d | all ((/= Just "UID") . fmap (crName . fst) . kcColumn) (pColumns p) ]
    headerNotes d =
      [ atTable ctx bt (finding Note "N-PRECONF-HEADER"
          ("the header " <> tshow (kcHeader kc) <> " is read as " <> crName c <> ", the one column of " <> target d <> " whose name it begins"))
      | kc <- pColumns p, Just (c, True) <- [kcColumn kc] ]
    -- N-PRECONF-UID-TPER: no row prints a UID
    uidColumn = listToMaybe [ kc | kc <- pColumns p, Just (c, _) <- [kcColumn kc], crName c == "UID" ]
    tperAssigned = case uidColumn of
      Just kc -> not (null (pRows p)) && not (any (maybe False hasValue . lookup (kcIndex kc) . prPrinted) (pRows p))
      Nothing -> False
    tperNote = [ atTable ctx bt (finding Note "N-PRECONF-UID-TPER"
                   ("every row leaves the UID column blank, its UIDs the TPer's to assign (Core 5.3.2.7.1, PDF 162, the"
                    <> " AccessControl table's): none is read or generated"))
               | tperAssigned ]
    accessControl = isAccessControl (rights (map kcKind (pColumns p)))
    rowFindings d r rd =
      [ f | not tperAssigned, isJust uidColumn, f <- uidTable d r ]
      <> readFindings r rd
      <> [ atCells ctx bt (prCells r) (finding Warning "W-PRECONF-NAME-LENGTH"
             (rowLabel r <> crName c <> ": " <> tshow txt <> " is " <> tshow len <> " bytes, longer than the " <> tshow n
               <> " its type (" <> crType c <> ") allows; kept as printed"))
         | (_, c, ReadValue _ remarks) <- rdCells rd, OverLength txt len n <- remarks ]
      <> templateName d r rd
      <> legendFindings r
    readFindings r rd =
      [ atCells ctx bt (prCells r) (finding Error (codeOf kc prob) (rowLabel r <> crName c <> ": " <> problemText prob))
      | (kc, c, ReadFailed prob) <- rdCells rd ]
    -- W-PRECONF-TEMPLATE-NAME: a template whose Name does not carry its placeholder's letters gives every instance
    -- that one Name, instantiate renaming only the cells that carry them (Opal Table 39 row 13's (+NN NN) and
    -- ACE_UserMMMM_Set_CommonName, PDF 79, at M5)
    templateName d r rd = case rdShape rd of
      TemplateRow ph _
        | any ((== "Name") . crName) (drColumns d), let name = valueAt p r "Name", not (T.null name)
        , not (phLetters ph `T.isInfixOf` name) ->
            [ atCells ctx bt (prCells r) (finding Warning "W-PRECONF-TEMPLATE-NAME"
                (rowLabel r <> "the template's Name " <> tshow name <> " does not carry its placeholder's letters "
                  <> phLetters ph <> ", so every instance would take that one Name"))
            ]
      _ -> []
    -- W-PRECONF-DEFAULT: a column of an SSC's table with no default, which a row that leaves it blank holds no value in
    defaultFindings d =
      [ atTable ctx bt (finding Warning "W-PRECONF-DEFAULT"
          ("the column " <> crName c <> " (" <> colNumberLit n <> ", type " <> crType c <> ") of " <> target d <> " has no default: " <> why
            <> "; a row that leaves it blank holds no value there (design 13.31 (a))"))
      | isJust (pSP p), (n, c, Left why) <- columnDefaults ctx d ]
    codeOf kc prob = fromMaybe "C-PRECONF-CELL" (either (const Nothing) (\k -> aclCode accessControl k prob) (kcKind kc))
    -- C-UID-TABLE: the UID's upper half is the target's half-UID
    uidTable d r = map (atCells ctx bt (prCells r) . finding Error "C-UID-TABLE" . (rowLabel r <>)) $
      case (parseUIDCell (valueAt p r "UID"), halfOf ctx d) of
        (Nothing, _)          -> ["the UID cell " <> tshow (valueAt p r "UID") <> " is not eight bytes"]
        (Just u, Just h)
          | take 4 u /= h     -> ["the UID " <> hexUID u <> " does not lie in the " <> fromMaybe "" (drTable d)
                                  <> " table, whose half-UID is " <> hexUID h]
        (Just _, Nothing)     -> ["Table 240 gives the " <> fromMaybe "" (drTable d) <> " table no UID"]
        _                     -> []
    -- the legend's findings about each kept cell, located at the cell
    legendFindings r = case legendScale ctx of
      Nothing -> []
      Just scale ->
        [ atCells ctx bt [cell] (finding sev code (rowLabel r <> crName c <> ": " <> msg))
        | kc <- pColumns p, Just (c, _) <- [kcColumn kc], cell <- take 1 (drop (kcIndex kc) (prCells r))
        , (sev, code, msg) <- snd (cellAccess scale cell) ]
    -- N-PRECONF-LABEL: each label read otherwise, once, at the first row reading it
    labelNotes =
      [ atCells ctx bt (prCells r) (finding Note "N-PRECONF-LABEL"
          (tshow printed <> " is read as " <> ty <> "'s " <> label <> (if byPrefix then ", the one label it begins" else ", case and spaces aside")))
      | ((ty, printed, label, byPrefix), r) <- L.nubBy (\a b -> fst a == fst b)
          [ ((ty, printed, label, byPrefix), r)
          | (r, rd) <- zip (pRows p) (pReads p), (_, _, ReadValue _ remarks) <- rdCells rd, LabelRead ty printed label byPrefix <- remarks ] ]

-- | The half-UID the table of table UIDs gives a described table, the spec's or its base's.
halfOf :: Context -> DescRead -> Maybe [Word8]
halfOf ctx d = listToMaybe [ take 4 t | (_, tu) <- allTableUIDRows ctx, Just (tuName tu) == drTable d, Just t <- [tuTable tu] ]

-- | C-UID-UNIQUE: a UID two rows of an SP preconfigured for one table share,
--   reported at every row after the first; a template's base, which is no
--   instance's UID (the Locking SP's (+NNNN) bases are its GlobalRange rows'
--   UIDs, Opal Table 39, PDF 80), left out.
uidUnique :: Context -> [PreRead] -> [Finding]
uidUnique ctx ps = concat
  [ [ atCells ctx (pBound p) (prCells r) (finding Error "C-UID-UNIQUE"
        (rowLabel r <> "the UID " <> hexUID uid <> " is also that of " <> renderAnchorId (btAnchor (pBound p0)) <> " " <> T.dropEnd 2 (rowLabel r0)))
    | (p, r) <- rest ]
  | ((_, _, uid), (p0, r0) : rest) <- Map.toList (Map.fromListWith (flip (<>))
      [ ((pSP p, tn, uid), [(p, r)]) | (tn, _, uid, p, r) <- index ps, not (prTemplate r) ]) ]

-- | The UIDs an SSC's plain rows with a Name are generated as, @u\<T\>_\<Name\>@
--   ('uidDeclName'), one generated name for each object (the lead's decision
--   at the M4b review, design 13.32 (22)), and C-UID-NAME: a name the spec or
--   its base gives another UID ("Hsed.Spec.Rules.UidTable"), or two rows of
--   the SP give, defines nothing. A row defines no name of its own where a
--   generated module of the spec or its base names its object already:
--
--   * under the same name, at the same UID (Core's @uAuthority_Anybody@,
--     @uSP_Admin@, @uMethodID_Get@);
--   * under a name of what the object is rather than a Name of its own
--     ('sameObjectUIDs'): a table's descriptor object, the Table table's
--     row of the table (@u\<T\>TableObject@, "Hsed.Spec.Rules.TableUIDs":
--     its UID fixed for every SP, Core 3.2.5.3 b.i, PDF 51-52, so that
--     Opal Table 20's rows, PDF 36-37, are the objects Core Table 240 and
--     Opal's own DataRemovalMechanism row name), where a generated module
--     declares it, or a row a table of UIDs names by its table and no name
--     (a single-row table's row, Core Table 244's @uSPInfo_Row@, PDF 305:
--     the SPInfo row whatever its SP's name).
--
--   A UID the spec or its base gives under another object's Name is another
--   object at that UID, which the SP's row replaces: the row keeps its own
--   name, its comment naming the other ('shadowsOf'): Opal Table 24's Admin1
--   (PDF 48), an individual authority (IsClass F) of the class Admins, at
--   the UID Core Table 243 gives Issuers (PDF 305), the Admin template's
--   class authority (Table 216, PDF 236). A list entry with no name (Core
--   Table 242's @Reserved for SSC Usage@, PDF 303-304) names nothing.
uidNames :: Context -> [PreRead] -> ([(Text, [Word8], PreRead, PreRow)], [Finding])
uidNames ctx ps = (fresh, baseClashes <> ownClashes)
  where
    based = Map.fromListWith (\_ old -> old) [ (i, (bs, c, r)) | (i, bs, c, r) <- baseUIDNames ctx ]
    same  = sameObjectUIDs ctx
    candidates =
      [ (uidDeclName tn piece, uid, p, r)
      | p <- ps, isJust (pSP p), Right d <- [pTarget p], Just tn <- [drTable d]
      , (r, rd) <- zip (pRows p) (pReads p), rdShape rd == PlainRow
      , Just piece <- [asName (valueAt p r "Name")], Just uid <- [parseUIDCell (valueAt p r "UID")] ]
    at p r why = atCells ctx (pBound p) (prCells r) (finding Error "C-UID-NAME" (rowLabel r <> why))
    baseClashes =
      [ at p r ("it is generated as " <> i <> ", as " <> specIdText (cxSpec c) <> " " <> renderAnchorId (btAnchor (urBound br))
                <> " row " <> tshow (urRow br + 1) <> " is, for the UID " <> hexUID bs)
      | (i, uid, p, r) <- candidates, Just (bs, c, br) <- [Map.lookup i based], bs /= uid ]
    -- the names each SP's module would define, by SP (an SP's names are its module's)
    unknown = Map.fromListWith (flip (<>))
      [ ((pSP p, i), [cand]) | cand@(i, uid, p, _) <- candidates, Map.notMember i based, uid `Set.notMember` same ]
    fresh = [ cand | [cand] <- Map.elems unknown ]
    ownClashes =
      [ at p r ("it is generated as " <> i <> ", as " <> renderAnchorId (btAnchor (pBound p0)) <> " row " <> tshow (prRow r0 + 1) <> " is")
      | (_, p0, r0) : rest <- [ [ (i, p, r) | (i, _, p, r) <- cands ] | cands@(_ : _ : _) <- Map.elems unknown ]
      , (i, p, r) <- rest ]

-- | Every UID a table of UIDs of the spec or its base names, with the name it
--   generates, its Context and its row, the spec's first: the rows with a
--   name ('urIdent'), a list entry with none (@Reserved for SSC Usage@) left
--   out.
baseUIDNames :: Context -> [(Text, [Word8], Context, UidRow)]
baseUIDNames ctx = [ (i, bs, c, r) | c <- contextChain ctx, r <- uidRows c, Just i <- [urIdent r], Exact bs <- [urValue r] ]

-- | The UIDs a generated module of the spec or its base names by what the
--   object is, not by a Name of its own, an object the same in every SP
--   ('uidNames'): every table's descriptor object a TableUIDs module
--   declares ("Hsed.Spec.Rules.TableDesc".'declaredTableObjects': Core
--   Table 240's rows, and the row an SP's Table table gives a table it adds
--   and the spec describes, Opal's DataRemovalMechanism, not one no module
--   describes, M5's DataStore, nor a second row naming one table, which
--   C-PRECONF-UID refuses), and every row a table of UIDs names with no
--   name cell (Core Table 244's single-row tables' rows, by their table).
sameObjectUIDs :: Context -> Set [Word8]
sameObjectUIDs ctx = Set.fromList
  (declaredTableObjects ctx <> [ bs | (_, bs, _, r) <- baseUIDNames ctx, isNothing (urName r) ])

-- | The rows of the tables of UIDs of the spec or its base that give a UID
--   under another object's Name than the generated name's ('uidNames'): the
--   object an SSC's row replaces at that UID, with its Context and its
--   generated name.
shadowsOf :: Context -> Text -> [Word8] -> [(Context, UidRow, Text)]
shadowsOf ctx i uid = [ (c, r, i') | (i', bs, c, r) <- baseUIDNames ctx, bs == uid, i' /= i, isJust (urName r) ]

-- | @u\<T\>_\<Name\>@, the name of an SSC row's generated UID, a Name that
--   begins @\<T\>_@ standing alone: @uAuthority_Admin1@, @uC_PIN_MSID@,
--   @uACE_C_PIN_SID_Get_NOPIN@ (as the @uidTable@ rule names Core's rows).
uidDeclName :: Text -> Text -> Text
uidDeclName t n = "u" <> (if (t <> "_") `T.isPrefixOf` n then n else t <> "_" <> n)

-- | C-PRECONF-UID: every table of UIDs the @uidTable@ rule reads that claims
--   its rows for a table something preconfigures, against those rows; and
--   every row an SP's preconfiguration gives the Table table against the
--   descriptor object of the table its Name names, the one a table of table
--   UIDs of the spec or its base gives it, else the first row an SP's Table
--   table gives a table no such table names ('allTableUIDRows': since the M4b
--   review's verification, so that a second row naming Opal's
--   DataRemovalMechanism at another UID is refused as a second C_PIN row is)
--   (Core 3.2.5.3 b.i, PDF 51-52: a table's descriptor object's UID is fixed,
--   so Opal Table 20's rows are Core Table 240's, PDF 36-37, 302-303), at the
--   row (since the M4b review).
preconfUID :: Context -> [PreRead] -> [Finding]
preconfUID ctx ps =
  [ f
  | bt <- boundTables (Just UidTable) ctx, Just t <- [tableClaim bt]
  , let es = [ e | e <- entriesOf ps, peTarget e == t ], not (null es)
  , f <- preconfDifferences ctx bt t [ r | r <- uidRows ctx, btAnchor (urBound r) == btAnchor bt ] es ]
  <> [ atCells ctx (pBound p) (prCells r) (finding Error "C-PRECONF-UID"
         (rowLabel r <> "the UID " <> hexUID uid <> " is not " <> hexUID o <> ", the " <> tn <> " table's descriptor object, as "
            <> specIdText (cxSpec c) <> " " <> renderAnchorId (btAnchor (tuBound tu)) <> " row " <> tshow (tuRow tu + 1)
            <> " gives it (Core 3.2.5.3 b.i, PDF 51-52)"))
     | (tn', tn, uid, p, r) <- index ps, tn' == "Table", isJust (pSP p)
     , (c, tu, o) <- take 1 [ (c, tu, o) | (c, tu) <- allTableUIDRows ctx, tuName tu == tn, Just o <- [tuObject tu] ]
     , o /= uid ]

-- | How the rows of the table of a table's UIDs differ from the rows
--   preconfigured for that table: a UID one side has and the other lacks, or
--   one UID with another name or in another template.
preconfDifferences :: Context -> BoundTable -> Text -> [UidRow] -> [PreEntry] -> [Finding]
preconfDifferences ctx bt t trs es =
  [ atRow' r (UidTable.rowLabel r <> "no row preconfigured for the " <> t <> " table (" <> wheres <> ") has this UID")
  | (r, bs) <- exact, bs `notElem` map peUID es ]
  <> [ atTable ctx bt (bad (peWhere e <> " preconfigures " <> peName e <> ", " <> hexUID (peUID e) <> ", which is not among the rows"))
     | e <- es, peUID e `notElem` map snd exact ]
  <> concat [ [ atRow' r (UidTable.rowLabel r <> "it is " <> peName e <> " in " <> peWhere e) | Just (peName e) /= urName r ]
              <> [ atRow' r (UidTable.rowLabel r <> peWhere e <> " preconfigures it for the " <> tpl <> " template")
                 | Just tpl <- [peTemplate e], Just tpl /= urTemplate r ]
            | (r, bs) <- exact, e <- es, peUID e == bs ]
  where
    bad      = finding Error "C-PRECONF-UID"
    atRow' r = atCells ctx bt (urCells r) . bad
    exact    = [ (r, bs) | r <- trs, Exact bs <- [urValue r] ]
    wheres   = T.intercalate ", " (L.nub (map (T.takeWhile (/= ' ') . peWhere) es))


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = map one ps <> indexes <> uids
  where
    ps = readPres ctx bts
    (named, nameFindings) = uidNames ctx ps
    failed = Set.fromList [ a | f <- concatMap (preFindings ctx) ps <> uidUnique ctx ps <> nameFindings, Cm.isError f, Just a <- [Cm.anchor f] ]
    compiles p = isRight (pTarget p) && renderAnchorId (btAnchor (pBound p)) `Set.notMember` failed
    one p = case (pTarget p, pSP p) of
      (Right d, Just sp) | compiles p -> EmitIn (preconfigModuleFor sp) (preconfTableDecl ctx p d sp)
      (Right d, Nothing) | compiles p -> Emit (preDecl ctx p d (coreRows p))
      (_, sp) -> Excluded (btAnchor (pBound p)) $ if isJust sp
        then "C-PRECONF-HEADER, C-UID-TABLE, C-UID-UNIQUE, C-UID-NAME, C-PRECONF-CELL, C-ACL-REFS, C-ACL-EXPR or C-LEGEND-TOTAL failed"
        else "C-PRECONF-HEADER, C-UID-TABLE, C-UID-UNIQUE or C-PRECONF-CELL failed"
    coreRows p = [ (r, [ (c, renderCell v) | (_, c, ReadValue v _) <- rdCells rd ]) | (r, rd) <- zip (pRows p) (pReads p), not (prTemplate r) ]
    indexes =
      [ EmitIn (preconfigModuleFor sp) (indexDecl ctx sp lastOne good)
      | sp <- L.nub (mapMaybe pSP ps), let good = [ p | p <- ps, pSP p == Just sp, compiles p ], lastOne : _ <- [reverse good] ]
    uids = [ EmitIn (preconfigModuleFor sp) (uidDecl ctx i uid p r) | (i, uid, p, r) <- named, Just sp <- [pSP p], compiles p ]

-- | @adminSP\<T\>TablePreconfiguration :: PreconfTable@ ("Hsed.Core.Preconfig")
--   for a table an SP's caption names (design example E12): its target's
--   description from the module that defines it, each column's default but
--   the UID's, and its rows, each with its kind, its optional mark, its cells
--   by column number (a printed value 'Given', a value the TPer chooses
--   'VendorUnique') and its notes, and the legend's access of its kept cells
--   (none where the spec has no legend), for 'preconfigModuleFor' the SP.
preconfTableDecl :: Context -> PreRead -> DescRead -> Text -> Gen.Decl
preconfTableDecl ctx p d sp = (Gen.decl prov name body)
  { Gen.declImports =
      importSome preconfRuntime (["PreconfTable (..)"] <> [ x | hasRows, x <- ["PreconfRow (..)", "RowKind (..)"] ]
                                 <> [ "PreCell (..)" | hasCells ] <> [ "Placeholder (..)" | any isTemplate shapes ])
      <> importSome Gen.coreTableModule ([ "Cell (..)" | not (null values) ] <> [ "Pre (..)" | hasRows ] <> [ "Access (..)" | any (not . null) accesses ])
      <> importSome Gen.uidModule ([ "uid" | any usesUID values ] <> [ "halfUID" | any isEvery shapes ])
      <> importSome "Prelude" [ "Bool (..)" | hasRows ]
      <> importSome (Gen.moduleName descSpec tableDescsModule) [descDeclName tname] }
  where
    bt       = pBound p
    caption  = normaliseCaption (fromMaybe "" (btRead bt).caption)
    prov     = provenanceOf ctx (btAnchor bt) (btTable bt) caption
    name     = preDeclName (fromMaybe caption (captionTitle caption))
    tname    = fromMaybe "" (drTable d)
    descSpec = fromMaybe (cxSpec ctx) (pDescSpec p)
    extended = if describedIn ctx d /= descSpec then ", with " <> specIdText descSpec <> "'s columns" else ""
    scale    = legendScale ctx
    defaults = [ (n, c, v) | (n, c, Right v) <- columnDefaults ctx d ]
    rows     = zip (pRows p) (pReads p)
    shapes   = map (rdShape . snd) rows
    hasRows  = not (null rows)
    -- each cell a row prints, by column number: a value given, or one the TPer chooses (a failed one leaves the table out)
    given rd = [ (n, c, e) | (_, c, cr) <- rdCells rd, Just n <- [crNumber c], Just e <- [preCellExpr cr] ]
    hasCells = any (not . null . given . snd) rows
    values   = [ v | (_, _, v) <- defaults ] <> [ v | (_, rd) <- rows, (_, _, ReadValue v _) <- rdCells rd ]
    accesses = map (access . fst) rows
    access r = maybe [] (\s -> cellClasses s p r) scale
    isTemplate = \case
      TemplateRow {} -> True
      _              -> False
    isEvery = \case
      EveryObjectRow _ -> True
      _                -> False
    body =
      [ Gen.provenanceComment prov <> ": the " <> Gen.ascii tname <> " table (" <> specIdText (describedIn ctx d) <> " "
          <> renderAnchorId (btAnchor (drBound d)) <> extended <> ") as the " <> Gen.ascii sp <> " holds it"
      , name <> " :: PreconfTable"
      , name <> " = PreconfTable"
      , "  { ptDesc     = " <> descDeclName tname
      , "  , ptDefaults =" ]
      <> Gen.listBlock "      " [ ["(" <> colNumberLit n <> ", " <> renderCell v <> ")  -- " <> crName c] | (n, c, v) <- defaults ]
      <> [ "  , ptRows     =" ]
      <> Gen.listBlock "      " (L.zipWith3 rowItem (map fst rows) (map snd rows) accesses)
      <> [ "  }" ]
    rowItem r rd acc =
      [ "Pre  -- " <> Gen.ascii (rowNote r rd)
      , "  (PreconfRow " <> kindExpr (rdShape rd) <> " " <> (if prOptional r then "True" else "False") ]
      <> map ("    " <>) (bracketed [ "(" <> colNumberLit n <> ", " <> e <> ")  -- " <> crName c
                                    | (n, c, e) <- L.sortOn (\(n, _, _) -> n) (given rd) ])
      <> [ "    [" <> T.intercalate ", " (map tshow (prNotes r)) <> "])" ]
      <> map ("  " <>) (bracketed (map (T.intercalate ", ") (chunks 4 [ "(" <> colNumberLit n <> ", " <> tshow a <> ")" | (n, a) <- acc ])))
    rowNote r rd = T.dropEnd 2 (rowLabel r)
      <> maybe "" (\g -> " (group " <> g <> ")") (prGroup r)
      <> (case rdShape rd of
            TemplateRow ph _ -> "; a template, " <> phLetters ph
            EveryObjectRow h -> "; every object of the table " <> hexUID h
            PlainRow         -> "")
      <> (if prOptional r then "; optional" else "")
      <> (if null (prNotes r) then "" else "; notes " <> T.unwords (prNotes r))
    kindExpr = \case
      PlainRow           -> "Plain"
      TemplateRow ph ns  -> "(Template (Placeholder " <> tshow (phLetters ph) <> " " <> tshow (phBytes ph) <> ") ["
                              <> T.intercalate ", " (map colNumberLit ns) <> "])"
      EveryObjectRow h   -> "(EveryObject (" <> Gen.bytesExpr "halfUID" h <> "))"
    preCellExpr = \case
      ReadVendor m  -> Just ("VendorUnique " <> tshow m)
      ReadValue v _ -> Just ("Given (" <> renderCell v <> ")")
      ReadFailed _  -> Nothing

-- | Each column's default but the UID's, from its kind ('zeroValue'), or why
--   it has none: the value a column takes where a row of an SSC's table
--   leaves it blank (design 13.31 (a)), and @W-PRECONF-DEFAULT@ where there
--   is none (Opal Table 43's @uniteger_8@, PDF 85, at M5).
columnDefaults :: Context -> DescRead -> [(Int, ColRead, Either Text CellValue)]
columnDefaults ctx d =
  [ (n, c, colKind types (crType c) >>= \k -> maybe (Left ("its kind gives none (" <> tshow k <> ")")) Right (zeroValue k))
  | c <- drColumns d, Just n <- [crNumber c], n /= 0 ]
  where types = preconfTypeEnv ctx

-- | The spec whose table describes a description: the first of the spec and
--   its bases that reads it among its own (Core T211 for Opal's TPerInfo, to
--   which Opal adds a column).
describedIn :: Context -> DescRead -> SpecId
describedIn ctx d = fromMaybe (cxSpec ctx) (listToMaybe [ cxSpec c | c <- contextChain ctx, any same (descReads c) ])
  where
    same d' = btAnchor (drBound d') == btAnchor (drBound d) && drTable d' == drTable d

-- | The legend's class of each kept cell of a row by column number, in
--   column order ("Hsed.Spec.Rules.Legend".'cellAccess'): documentation, the
--   ACL alone deciding access (the lead's answer 2 to the M4b plan).
cellClasses :: LegendScale -> PreRead -> PreRow -> [(Int, Access)]
cellClasses scale p r = L.sortOn fst
  [ (n, fst (cellAccess scale cell)) | kc <- pColumns p, Just (c, _) <- [kcColumn kc], Just n <- [crNumber c]
                                     , cell <- take 1 (drop (kcIndex kc) (prCells r)) ]

-- | An import of some names of a module, none when there are none.
importSome :: Text -> [Text] -> [Gen.Import]
importSome m xs = [ Gen.ImportSome m xs | not (null xs) ]

-- | A list literal's lines: @[ a@, @, b@, @]@; @[]@ when empty.
bracketed :: [Text] -> [Text]
bracketed = \case
  []    -> ["[]"]
  items -> zipWith (\i x -> (if i == (0 :: Int) then "[ " else ", ") <> x) [0 ..] items <> ["]"]

-- | A list in pieces of so many.
chunks :: Int -> [a] -> [[a]]
chunks n xs = case L.splitAt n xs of
  (piece, [])   -> [ piece | not (null piece) ]
  (piece, rest) -> piece : chunks n rest

-- | Does a value print a @uid@ (a UID or a reference)?
usesUID :: CellValue -> Bool
usesUID = \case
  CvUID _   -> True
  CvRef _ _ -> True
  CvList vs -> any usesUID vs
  _         -> False

-- | @adminSP :: [PreconfTable]@: an SP's tables, in table order, after the
--   last one's declarations.
indexDecl :: Context -> Text -> PreRead -> [PreRead] -> Gen.Decl
indexDecl ctx sp lastOne ps = (Gen.decl prov name body)
  { Gen.declRank    = maxBound
  , Gen.declImports = [Gen.ImportSome preconfRuntime ["PreconfTable (..)"]] }
  where
    name = lowerIdent sp
    prov = boundProvenance ctx (pBound lastOne)
    tableName p = let caption = normaliseCaption (fromMaybe "" (btRead (pBound p)).caption)
                  in preDeclName (fromMaybe caption (captionTitle caption))
    body = [ "-- | The " <> Gen.ascii sp <> "'s preconfiguration tables, in table order: " <> specIdText (cxSpec ctx) <> " "
               <> T.intercalate ", " (map (renderAnchorId . btAnchor . pBound) ps)
           , name <> " :: [PreconfTable]" ]
           <> Gen.listDefinition (name <> " =") (map tableName ps)

-- | One SSC row's UID, @u\<T\>_\<Name\>@, citing the page the row is printed
--   on, and the object of another Name a table of UIDs gives the same UID,
--   which the row replaces ('shadowsOf').
uidDecl :: Context -> Text -> [Word8] -> PreRead -> PreRow -> Gen.Decl
uidDecl ctx i uid p r = (Gen.decl prov i body)
  { Gen.declRank    = 1 + prRow r
  , Gen.declImports = [Gen.ImportSome Gen.uidModule ["UID", "uid"]] }
  where
    prov = atRow ctx (prCells r) (boundProvenance ctx (pBound p))
    body = [ Gen.provenanceComment prov <> ", row " <> tshow (prRow r + 1) <> ": " <> Gen.ascii (valueAt p r "Name") ]
           <> [ "--   another object than " <> specIdText (cxSpec c) <> " " <> renderAnchorId (btAnchor (urBound br)) <> " row "
                  <> tshow (urRow br + 1) <> "'s " <> Gen.ascii (fromMaybe "" (urName br))
                  <> maybe "" (\t -> " (the " <> Gen.ascii t <> " template's)") (urTemplate br) <> ", " <> i' <> ", at the same UID"
              | (c, br, i') <- shadowsOf ctx i uid ]
           <> [ i <> " :: UID", i <> " = " <> Gen.uidExpr uid ]

-- | @defaultBaseTemplateAuthorities@: the caption's words, the first in lower case.
preDeclName :: Text -> Text
preDeclName title = case filter (not . T.null) (T.split (not . C.isAlphaNum) title) of
  w : ws -> maybe w (\(c, cs) -> T.cons (C.toLower c) cs) (T.uncons w) <> T.concat ws
  []     -> "preconfigured"

-- | @\<name\> :: [Pre Row]@, each row's cells in column order.
preDecl :: Context -> PreRead -> DescRead -> [(PreRow, [(ColRead, Text)])] -> Gen.Decl
preDecl ctx p d rows = (Gen.decl prov name body)
  { Gen.declImports = [ Gen.ImportSome Gen.coreTableModule ["Cell (..)", "Pre (..)", "Row"]
                      , Gen.ImportSome Gen.uidModule ["uid"] ] }
  where
    bt   = pBound p
    caption = normaliseCaption (fromMaybe "" (btRead bt).caption)
    prov = provenanceOf ctx (btAnchor bt) (btTable bt) caption
    name = preDeclName (fromMaybe caption (captionTitle caption))
    target = fromMaybe "" (drTable d)
    rowLines i (r, cs) =
      [ (if i == (0 :: Int) then "  [ Pre  -- " else "  , Pre  -- ") <> T.dropEnd 2 (rowLabel r)
          <> maybe "" (\g -> " (group " <> g <> ")") (prGroup r) ]
      <> zipWith (\j (c, cell) -> (if j == (0 :: Int) then "      [ " else "      , ")
                                    <> "(" <> colNumberLit (fromMaybe 0 (crNumber c)) <> ", " <> cell <> ")")
                 [0 ..] (L.sortOn (crNumber . fst) cs)
      <> [ if null cs then "      []" else "      ]", "      []" ]
    body =
      [ Gen.provenanceComment prov <> ": rows of the " <> target <> " table (" <> renderAnchorId (btAnchor (drBound d)) <> ")"
      , name <> " :: [Pre Row]"
      , name <> " =" ]
      <> concat (zipWith rowLines [0 ..] rows)
      <> [ if null rows then "  []" else "  ]" ]
