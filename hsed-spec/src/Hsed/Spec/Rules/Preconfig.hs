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
rule rides on the @doc@ anchor (design 13.24).

A table's target is the table its caption names (@Authority@ in @… Authority
Table Preconfiguration@; in @Default … Authorities@ the last word, a plural:
@Authority@ + @s@, @es@, or @y@ made @ies@), read through that table's
description by the @tableDesc@ rule ("Hsed.Spec.Rules.TableDesc"). A column
whose header mentions @informative@ is dropped; a row whose first cell alone is
printed is a group row, the context of the rows below it (Opal T22, T23, T38,
T39), not a row of the target; a row with a @(+XX)@ cell is a template row,
which the @expand@ modifier is for (design M4b) and which is not generated
yet. Every other row gives the target's columns its header names, each cell
read as its column's type says ('Hsed.Spec.Rules.TableDesc.readCell'); a
blank cell keeps the column's default; an object reference names the object
by the Name column of a row preconfigured for the referenced table.

Checks, each with the code its findings carry:

* @C-PRECONF-HEADER@: the caption names a described table, and every header
  cell kept names one of its columns (compared as the headers are, whitespace
  and case aside), no column twice, the UID column among them;
* @C-UID-TABLE@: every row's UID lies in the target table: its upper half is
  the half-UID Table 240 gives the target ("Hsed.Spec.Rules.TableUIDs");
* @C-UID-UNIQUE@: no two rows preconfigured for one table share a UID;
* @C-PRECONF-CELL@: every printed cell reads as its column's type (an
  enumeration's label, a name's text, a reference to a preconfigured row);
* @W-PRECONF-TEMPLATE@: a template row, left out of the generated rows.

Compiled into 'preconfigModule': per table the caption's words in camel case
(@defaultBaseTemplateAuthorities :: [Pre Row]@), one 'Hsed.Core.Table.Pre' per
row giving its cells by column number; Core gives no access ('access' is
@[]@). Whether the rows' UIDs and names agree with the table of the target's
UIDs (Core Table 243 for the authorities) is the @uidTable@ rule's to check
('preRowsFor' gives them).
-}
module Hsed.Spec.Rules.Preconfig
  ( preconfigRule
  , preconfigModule
    -- * Reading the bound tables
  , PreRead (..)
  , PreRow (..)
  , preReads
  , targetNoun
  , captionTemplate
  , preRowsFor
  , preDeclName
  ) where

import qualified Data.Text                 as DT
import qualified RIO.Char                  as C
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO

import           Hsed.Core.Types           (TableRef (..))
import           Hsed.Spec.Anchor          (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common          (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.DSL             (SomeBinding (..), preconfig)
import           Hsed.Spec.Format          (globMatch)
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.IR              (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TableDesc (CellValue (..), ColRead (..), DescRead (..), colKind, colNumberLit,
                                            descReads, readCell, renderCell)
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..), tableUIDRows)
import           Hsed.Spec.Shape           (Shape (..), captionTitle, normHeader, normaliseCaption, shapeMismatch)


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
  , ruleChecks  = \ctx bts -> let ps = readPres ctx bts in concatMap (preFindings ctx ps) ps <> uidUnique ctx ps
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.Preconfig@.
preconfigModule :: Gen.GenModule
preconfigModule = Gen.GenModule
  { Gen.leaf        = "Preconfig"
  , Gen.description = "Preconfigured rows: the rows a template or an SSC gives a table, by column number"
  , Gen.notes       = [ "A column a row does not give keeps its default; template rows ((+XX)) are not generated yet." ] }


-- * Reading the bound tables

-- | One preconfiguration table read under the errata set.
data PreRead = PreRead
  { pBound   :: !BoundTable
  , pNoun    :: !(Maybe Text)              -- ^ the target as the caption names it (@Authorities@)
  , pTarget  :: !(Either Text DescRead)    -- ^ the target's description, or why there is none
  , pHeader  :: ![(Int, Text)]             -- ^ the columns kept: index in the table and header cell
  , pDropped :: ![Text]                    -- ^ the informative header cells, whose columns are dropped
  , pRows    :: ![PreRow]                  -- ^ the rows that are not group rows
  }

-- | One row of a preconfiguration table.
data PreRow = PreRow
  { prRow      :: !Int                     -- ^ 0-based among the table's rows, group rows counted
  , prCells    :: ![CellIR]
  , prGroup    :: !(Maybe Text)            -- ^ the group row above it
  , prTemplate :: !Bool                    -- ^ a template row: a cell carries @(+XX)@
  , prValues   :: ![(Text, Text)]          -- ^ each kept column's header cell and this row's cell
  }

-- | Every table bound to the rule's shape, read under the errata set.
preReads :: Context -> [PreRead]
preReads ctx = readPres ctx (boundTables (Just Preconfig) ctx)

readPres :: Context -> [BoundTable] -> [PreRead]
readPres ctx = map readPre
  where
    descs = descReads ctx
    readPre bt = PreRead
      { pBound = bt, pNoun = noun, pTarget = target, pHeader = kept
      , pDropped = [ h | h <- header, isInformative h ], pRows = rows }
      where
        t      = btRead bt
        header = maybe [] (map (T.strip . (.text))) (listToMaybe t.headerRows)
        kept   = [ (i, h) | (i, h) <- zip [0 ..] header, not (isInformative h) ]
        noun   = t.caption >>= captionTitle >>= targetNoun
        target = case noun of
          Nothing -> Left "the caption names no table (\"… <T> Table Preconfiguration\" or \"Default … <Ts>\")"
          Just n  -> case [ d | d <- descs, Just dn <- [drTable d], names n dn ] of
            d : _ -> Right d
            []    -> Left ("the caption names " <> n <> ", which no table description describes")
        rows = catMaybes (snd (L.mapAccumL step Nothing (zip [0 ..] t.rows)))
        step grp (i, cells)
          | isGroup cells = (Just (cellText 0 cells), Nothing)
          | otherwise = (grp, Just PreRow
              { prRow = i, prCells = cells, prGroup = grp
              , prTemplate = any (("(+" `T.isInfixOf`) . (.text)) cells
              , prValues = [ (h, cellText k cells) | (k, h) <- kept ] })
    cellText :: Int -> [CellIR] -> Text
    cellText k cells = maybe "" (T.strip . (.text)) (listToMaybe (drop k cells))
    isGroup :: [CellIR] -> Bool
    isGroup cells = case map (T.strip . (.text)) cells of
      firstCell : rest@(_ : _) -> not (T.null firstCell) && all T.null rest
      _                        -> False

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

-- | The template a Core caption names: @Base@ in @Default Base Template Authorities@.
captionTemplate :: PreRead -> Maybe Text
captionTemplate p = do
  title <- (btRead (pBound p)).caption >>= captionTitle
  rest  <- T.stripPrefix "Default " title
  let (template, after) = DT.breakOn " Template " rest
  guard (not (T.null after) && not (T.null template))
  pure template

-- | The name and UID of every row preconfigured for a table, in anchor and
--   row order: what the table of that table's UIDs must agree with.
preRowsFor :: Context -> Text -> [(Text, [Word8])]
preRowsFor ctx n = [ (name, uid) | (tn, name, uid, _, _) <- index (preReads ctx), tn == n ]

-- | Every non-template row of a table with a target, by the target's name: its
--   Name column's cell (blank without one), its UID, its table and row.
index :: [PreRead] -> [(Text, Text, [Word8], PreRead, PreRow)]
index ps =
  [ (fromMaybe "" (drTable d), byColumn "Name", uid, p, r)
  | p <- ps, Right d <- [pTarget p], r <- pRows p, not (prTemplate r)
  , let byColumn c = fromMaybe "" (lookup c [ (crName col, v) | (h, v) <- prValues r, Just col <- [columnOf d h] ])
  , Just uid <- [parseUIDCell (byColumn "UID")] ]

-- | The target's column a header cell names.
columnOf :: DescRead -> Text -> Maybe ColRead
columnOf d h = L.find ((== normHeader h) . normHeader . crName) (drColumns d)


-- * Checks

-- | A finding about one row, located on the page the row is printed on.
atCells :: Context -> BoundTable -> [CellIR] -> Finding -> Finding
atCells ctx bt cells = case listToMaybe (mapMaybe (\c -> c.page) cells) of
  Just p0 -> locatedAt (cxSpec ctx) (renderAnchorId (btAnchor bt)) (p0 + 1)
  Nothing -> atTable ctx bt

rowLabel :: PreRow -> Text
rowLabel r = "row " <> tshow (prRow r + 1) <> maybe "" (" " <>) (lookup "Name" (prValues r) <|> nameLike) <> ": "
  where nameLike = listToMaybe [ v | (h, v) <- prValues r, normHeader h == "name", not (T.null v) ]

-- | Every finding about one table: its target and header, then each row.
preFindings :: Context -> [PreRead] -> PreRead -> [Finding]
preFindings ctx ps p = case pTarget p of
  Left why -> [atTable ctx bt (finding Error "C-PRECONF-HEADER" why)]
  Right d  -> case headerProblems d of
    []    -> concatMap (rowFindings d) (pRows p)
    probs -> map (atTable ctx bt . finding Error "C-PRECONF-HEADER") probs
  where
    bt = pBound p
    target d = renderAnchorId (btAnchor (drBound d)) <> " " <> fromMaybe "" (drTable d)
    headerProblems d =
      [ "the header " <> tshow h <> " names no column of " <> target d | (_, h) <- pHeader p, isNothing (columnOf d h) ]
      <> [ "the headers " <> T.intercalate " and " (map tshow hs) <> " name one column of " <> target d
         | hs@(_ : _ : _) <- Map.elems (Map.fromListWith (flip (<>)) [ (crName c, [h]) | (_, h) <- pHeader p, Just c <- [columnOf d h] ]) ]
      <> [ "the header gives no UID column of " <> target d | all ((/= Just "UID") . fmap crName . columnOf d . snd) (pHeader p) ]
    rowFindings d r
      | prTemplate r = [ atCells ctx bt (prCells r) (finding Warning "W-PRECONF-TEMPLATE"
                           (rowLabel r <> "a template row ((+XX)), not generated: the expand modifier is to expand it")) ]
      | otherwise = uidTable d r <> map (atCells ctx bt (prCells r) . finding Error "C-PRECONF-CELL" . (rowLabel r <>)) (lefts (cells d r))
    -- C-UID-TABLE: the UID's upper half is the target's half-UID
    uidTable d r = map (atCells ctx bt (prCells r) . finding Error "C-UID-TABLE" . (rowLabel r <>)) $
      case (parseUIDCell (uidCell d r), halfOf ctx d) of
        (Nothing, _)          -> ["the UID cell " <> tshow (uidCell d r) <> " is not eight bytes"]
        (Just u, Just h)
          | take 4 u /= h     -> ["the UID " <> hexUID u <> " does not lie in the " <> fromMaybe "" (drTable d)
                                  <> " table, whose half-UID is " <> hexUID h]
        (Just _, Nothing)     -> ["Table 240 gives the " <> fromMaybe "" (drTable d) <> " table no UID"]
        _                     -> []
    cells d r = rowCells ctx ps d r

-- | The UID cell of a row.
uidCell :: DescRead -> PreRow -> Text
uidCell d r = fromMaybe "" (listToMaybe [ v | (h, v) <- prValues r, fmap crName (columnOf d h) == Just "UID" ])

-- | The half-UID Table 240 gives a described table.
halfOf :: Context -> DescRead -> Maybe [Word8]
halfOf ctx d = listToMaybe [ take 4 t | Right tu <- tableUIDRows ctx, Just (tuName tu) == drTable d, Just t <- [tuTable tu] ]

-- | Every printed cell of a row read as its column's type: the column and the value, or why not.
rowCells :: Context -> [PreRead] -> DescRead -> PreRow -> [Either Text (ColRead, CellValue)]
rowCells ctx ps d r =
  [ do kind <- colKind ctx (crType col)
       first (\why -> crName col <> ": " <> why) ((col,) <$> readCell resolve kind v)
  | (h, v) <- prValues r, not (T.null v), Just col <- [columnOf d h] ]
  where
    rows = index ps
    resolve name tables = case parseUIDCell name of
      Just uid -> Right uid
      Nothing  -> case L.nub [ uid | (tn, n, uid, _, _) <- rows, n == name, any (allows tn) tables ] of
        [uid] -> Right uid
        []    -> Left (tshow name <> " names no row preconfigured for " <> T.intercalate " or " (map tableRefText tables))
        uids  -> Left (tshow name <> " names " <> tshow (length uids) <> " rows with different UIDs")
    allows tn = \case
      Table t     -> t == tn
      TableGlob g -> globMatch g tn
      TableStar   -> True
    tableRefText = \case
      Table t     -> t
      TableGlob g -> g
      TableStar   -> "*"

-- | C-UID-UNIQUE: a UID two rows preconfigured for one table share, reported at every row after the first.
uidUnique :: Context -> [PreRead] -> [Finding]
uidUnique ctx ps = concat
  [ [ atCells ctx (pBound p) (prCells r) (finding Error "C-UID-UNIQUE"
        (rowLabel r <> "the UID " <> hexUID uid <> " is also that of " <> renderAnchorId (btAnchor (pBound p0)) <> " " <> T.dropEnd 2 (rowLabel r0)))
    | (p, r) <- rest ]
  | ((_, uid), (p0, r0) : rest) <- Map.toList (Map.fromListWith (flip (<>)) [ ((tn, uid), [(p, r)]) | (tn, _, uid, p, r) <- index ps ]) ]


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = map one ps
  where
    ps = readPres ctx bts
    unique = uidUnique ctx ps
    one p = case pTarget p of
      Right d | not (any Cm.isError (preFindings ctx ps p))
              , not (any ((== Just (renderAnchorId (btAnchor (pBound p)))) . Cm.anchor) unique)
              , Right rows <- traverse (\r -> (r,) <$> sequence (rowCells ctx ps d r)) [ r | r <- pRows p, not (prTemplate r) ]
              , Right printed <- traverse (\(r, vs) -> (r,) <$> traverse (\(c, v) -> (c,) <$> renderCell v) vs) rows ->
          Emit (preDecl ctx p d printed)
      _ -> Excluded (btAnchor (pBound p)) "C-PRECONF-HEADER, C-UID-TABLE, C-UID-UNIQUE or C-PRECONF-CELL failed"

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
