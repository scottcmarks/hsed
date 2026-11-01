{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

{-|
Module      : Hsed.Spec.Rules.EnumValues
Description : The enumValues rule: an enumeration's labelled values, joined to its type row
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E4. @enumValues@ fires on a two-column table
whose first header cell is @Enumeration Value@ or @Set Value@, under a caption
@\<ident\> Enumeration Values@ or @\<ident\> Set Values@, joined to the
@typeRow@ table of the same identifier ("Hsed.Spec.Rules.TypeRow"), whose
Format gives the range and whose UID the type. A wider table reaches the shape
through a comment with @cols a b@ (Table 87: @rangeWords ["to"] (cols 0 1
enumValues)@), which also names the words that spell a range besides @-@.

Check @C-ENUM-RANGE@: the values lie within the type row's range (the
Format's lower bound, so @reencrypt_state@'s 1..16 gets no @_NA@ at 0), each
value is assigned once (@W-ENUM-OVERLAP@ otherwise, the first row kept), every
label has a letter or a digit to name a constructor (an empty cell, or a
@cols@ binding pointing at a sparse column, is an Error), the constructors are
distinct, and two labels that scrub to one constructor, or a label that scrubs
to the one the gaps of the range take (@NA@), are an Error (the names
'constructorNames' gives are the ones checked); a label used for more than one
row is @W-ENUM-LABEL-REPEATED@ (each of its values gets a suffixed
constructor). A finding about rows is located at the page the first of them
is printed on (a table may span pages); one about the table as a whole (its
range, a constructor defined twice, two labels merged) at the table.

Under @enumInDescription@ an enumeration is printed in its type row's
Description column (Opal Table 49, @life_cycle_state@, PDF 98, which modifies
Core's type: @enumInDescription (replaces [core 102, core 103])@): the one
row's Name cell names the type, its UID and Format cells are kept as its own
('erOwnType'), and its values are the Description's @n = label@ items, read
from the cell's lines, a line ending in a hyphen joined to the next
('descriptionItems': @12 = manufactured-disabled-@ and @frozen@). A spec read
over another reads the base's enumerations too, its own shadowing the base's
of the same type ('allEnums').

The type row an enumeration joins ('joinable', since M4b): under
@enumInDescription@ the table's own UID, Name and Format cells, read as a
type row is ('ownTypeRow': Table 49's @Enumeration_Type, 0, 15@ gives the
range C-ENUM-RANGE checks); otherwise the spec's type row of its identifier,
else its base's ("Hsed.Spec.Rules.TypeRow".'allTypeRows': Opal's Table 31
joins its own Table 30). Check @C-PATCH-UID@ (an Error): a table that
@replaces@ tables of a base names them there, each the base's type row of
its type, with the table's own UID and Name (Core Table 102: @00 00 00 05 00
00 04 05@, @life_cycle_state@, PDF 113), or the base's enumeration of that
type (Core Table 103, PDF 113-114); a target in no spec it is read over, or
no type row or enumeration there, is the same Error. Such a table is left
out.

Compiled into 'enumsModule': @data Core_\<type\>@ (the spec's label,
"Hsed.Spec.Gen".'Gen.specLabel', then the type's name) with a constructor per
value (the label scrubbed; a label that names several values, a range or
repeated rows, gives each its own constructor suffixed @_\<v\>@, the first
included, @Core_adv_key_mode_Reserved_2@, and a label naming one value stays
bare; @_NA@ fills the gaps of the range, each suffixed when there are several:
design section 13.15, as decided at M2d), plus an
'Hsed.Core.Types.Enumerated' instance naming the type row's @u\<type\>Type@
and the range, imported from the generated module of the spec whose type row
it is ('UIDRef', by origin): the spec's own @Types@ whole, a base's by name
(Opal's @Opal_life_cycle_state@ takes @ulife_cycle_stateType@ from
@Hsed.Gen.Core.Types@, the type row it replaces, whose UID C-PATCH-UID holds
equal to Table 49's; its @Opal_data_removal_mechanism@ takes Table 30's from
@Hsed.Gen.Opal.Types@); a table under @enumInDescription@ replacing no type
row spells its UID out.
-}
module Hsed.Spec.Rules.EnumValues
  ( enumValuesRule
  , enumsModule
    -- * Reading the bound tables
  , EnumRead (..)
  , enumReads
  , allEnums
  , enumIdent
  , parseEnumValue
  , descriptionItems
    -- * The enumeration check
  , EnumCheck (..)
  , checkEnum
  , joinable
  , ownTypeRow
  , patchFindings
    -- * Declarations
  , EnumDecl (..)
  , enumDecl
  , UIDRef (..)
  , renderEnum
  , enumTypeName
  , constructorNames
  ) where

import qualified Data.Text               as DT
import           RIO
import qualified RIO.Char                as C
import           RIO.List                (sortOn, (\\))
import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import qualified RIO.Text                as T

import           Hsed.Core.Types         (Operand (..), TypeDef (..))
import           Hsed.Spec.Anchor        (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common        (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common        as Cm
import           Hsed.Spec.DSL           (SomeBinding (..), enumValues)
import           Hsed.Spec.Format        (parseFormat)
import qualified Hsed.Spec.Gen           as Gen
import           Hsed.Spec.IR            (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TypeRow (TypeRowRead (..), allTypeRows, formatEnv, typeRowReads, typeUIDName, typesModule)
import           Hsed.Spec.Shape         (CaptionKind (..), Form (..), Shape (..), measure, normHeader, normaliseCaption,
                                          shapeMismatch)


-- | The rule.
enumValuesRule :: RuleDef
enumValuesRule = RuleDef
  { ruleName    = RuleName "enumValues"
  , bindsTo     = SomeBinding enumValues
  , exemplar    = (SpecId "core-2.01", TableId 52)
  , refinesRule = Nothing
  , joinsTo     = Just (RuleName "typeRow")
  , fires       = \_ _ form -> isNothing (shapeMismatch EnumValues form)
                              && take 1 (fHeaderNorm form) `elem` [["enumerationvalue"], ["setvalue"]]
                              && isJust (enumIdent form)
  , ruleIdent   = enumIdent
  , ruleModule  = enumsModule
  , ruleCodes   =
      [ CodeDef "C-ENUM-RANGE"          [Error]          (Defect "an enumeration's values do not fit its type's range")
      , CodeDef "W-ENUM-OVERLAP"        [Warning]        (Defect "two rows of an enumeration assign one value")
      , CodeDef "W-ENUM-LABEL-REPEATED" [Warning]        (Suspect "an enumeration gives one label to several rows")
      , CodeDef "C-PATCH-UID"           [Error]
          (Defect "a table that replaces a base's type row or enumeration is not that type's: another UID or name, or no such table") ]
  , ruleChecks  = \ctx bts -> concatMap (findingsOf ctx) bts
  , ruleCompile = compile
  }

-- | Every finding about one bound table: C-ENUM-RANGE and its Warnings
--   against the type row it joins ('joinable'), the own type row's UID cell
--   under @enumInDescription@ (C-UID-TABLE, as "Hsed.Spec.Rules.TypeRow"
--   reads one) and C-PATCH-UID.
findingsOf :: Context -> BoundTable -> [Finding]
findingsOf ctx bt = case readEnum ctx bt of
  Left f   -> [f]
  Right er -> ecFindings (checkEnum ctx (joinable ctx er) er) <> ownUIDFindings ctx er <> patchFindings ctx bt er

-- | C-UID-TABLE: the UID cell of the type row a Description column's table
--   prints itself is eight bytes (the code is @typeRow@'s, which reads every
--   other type row's).
ownUIDFindings :: Context -> EnumRead -> [Finding]
ownUIDFindings ctx er =
  [ locatedAt (cxSpec ctx) (renderAnchorId (erAnchor er)) (fromMaybe 1 (tablePage (erTable er)))
      (finding Error "C-UID-TABLE" ("the UID cell " <> tshow (trUIDText tr) <> " is not eight bytes"))
  | Just tr <- [ownTypeRow ctx er], isNothing (trUID tr) ]

-- | @Hsed.Gen.\<Spec\>.Enums@; it imports the type rows' module for each type's UID.
enumsModule :: Gen.GenModule
enumsModule = Gen.GenModule
  { Gen.leaf        = "Enums"
  , Gen.description = "Enumerations of the Type objects: each value's constructor, the type's UID and range"
  , Gen.notes       = [ "The constructors are the type's name and each label scrubbed (commas dropped,"
                      , "spaces and dashes made underscores); a label that names several values gives"
                      , "each its own constructor suffixed with the value, _NA the gaps of the range"
                      , "(each suffixed when there are several)." ] }

-- | The type an enumeration's caption names: @\<ident\> Enumeration Values@ or @\<ident\> Set Values@.
enumIdent :: Form -> Maybe Text
enumIdent form = case fCaptionKind form of
  CaptionEnumValues s -> ident s
  CaptionOther t      -> T.stripSuffix " Set Values" t >>= ident . T.strip
  _                   -> Nothing
  where ident s = if not (T.null s) && T.all (\c -> C.isAlphaNum c || c == '_') s then Just s else Nothing


-- * Reading the bound tables

-- | One enumeration table read under the errata set.
data EnumRead = EnumRead
  { erAnchor     :: !AnchorId
  , erTable      :: !TableIR              -- ^ as printed
  , erCaption    :: !Text                 -- ^ after errata, normalised
  , erIdent      :: !(Maybe Text)         -- ^ the type the corrected caption names
  , erRows       :: ![(Text, Text)]       -- ^ value cell and label after errata, in the bound columns
  , erPages      :: ![Maybe Int]          -- ^ the 1-based page each row is printed on
  , erRangeWords :: ![Text]
  , erOwnType    :: !(Maybe (Text, Text))
    -- ^ under @enumInDescription@, the table's own UID and Format cells (Table
    --   49's @00 00 00 05 00 00 04 05@ and @Enumeration_Type, 0, 15@)
  }

-- | Every table bound as an enumeration, read under the errata set in its bound
--   columns (@cols a b@, else 0 and 1), or under @enumInDescription@ from its
--   Description column.
enumReads :: Context -> [Either Finding EnumRead]
enumReads ctx = readEnums ctx (boundTables (Just EnumValues) ctx)

-- | Every enumeration a spec reads: its own, then its base's whose type none
--   of its own names (Opal's Table 49 @life_cycle_state@ shadowing Core's
--   Table 103), each with the Context that owns it ('ownThenBase').
allEnums :: Context -> [(Context, EnumRead)]
allEnums = ownThenBase (rights . enumReads) erIdent

readEnums :: Context -> [BoundTable] -> [Either Finding EnumRead]
readEnums ctx = map (readEnum ctx)

-- | One table bound as an enumeration, read under the errata set ('enumReads').
readEnum :: Context -> BoundTable -> Either Finding EnumRead
readEnum ctx = readOne
  where
    readOne bt
      | mEnumInDescription (btMods bt) = inDescription bt
      | otherwise =
          let (a, b)    = fromMaybe (0, 1) (mCols (btMods bt))
              cell :: Int -> [CellIR] -> Text
              cell i r  = maybe "" (.text) (listToMaybe (drop i r))
              t'        = btRead bt
          in Right EnumRead
               { erAnchor = btAnchor bt, erTable = btTable bt, erCaption = normaliseCaption (fromMaybe "" t'.caption)
               , erIdent = enumIdent (measure (cxSpec ctx) t'), erRows = [ (cell a r, cell b r) | r <- t'.rows ]
               , erPages = [ (+ 1) <$> listToMaybe (mapMaybe (.page) r) | r <- t'.rows ]
               , erRangeWords = mRangeWords (btMods bt), erOwnType = Nothing }
    -- the one type row of a table printing its enumeration in a Description
    -- column: its Name the type, its UID and Format kept, its values the
    -- Description's items, each on the Description's page
    inDescription bt =
      let t'       = btRead bt
          header   = map (normHeader . (.text)) (concat (take 1 t'.headerRows))
          column h = L.elemIndex h header
          at :: Int -> [CellIR] -> Maybe CellIR
          at i r   = listToMaybe (drop i r)
      in case (t'.rows, traverse column ["uid", "name", "format", "description"]) of
           ([r], Just [u, n, f, d]) | Just dc <- at d r ->
             let items  = descriptionItems (if null dc.lines then [dc.text] else dc.lines)
                 text i = maybe "" (T.strip . (.text)) (at i r)
             in Right EnumRead
                  { erAnchor = btAnchor bt, erTable = btTable bt, erCaption = normaliseCaption (fromMaybe "" t'.caption)
                  , erIdent = identifier (text n), erRows = items
                  , erPages = map (const ((+ 1) <$> dc.page)) items
                  , erRangeWords = mRangeWords (btMods bt), erOwnType = Just (text u, text f) }
           (rows, _) -> Left (atTable ctx bt (finding Error "C-ENUM-RANGE"
                          ("an enumeration in a Description column is one row with UID, Name, Format and Description cells; this table has "
                            <> tshow (length rows) <> " rows under the header " <> tshow header)))
    identifier s = if not (T.null s) && T.all (\c -> C.isAlphaNum c || c == '_') s then Just s else Nothing

-- | The @n = label@ items of an enumeration a Description cell prints (Opal
--   Table 49, PDF 98: @… The valid values are: 0 = issued, 1 =
--   issued-disabled, …, 14-15 = reserved@), from the cell's lines: a line
--   ending in a hyphen is joined to the next with no space (@12 =
--   manufactured-disabled-@ and @frozen,@), any other with one; the items are
--   the comma-separated pieces holding an @=@, each item's value cell the
--   number or range just before its @=@ (the words before it dropped) and its
--   label the text after it.
--
-- >>> descriptionItems ["The values are:", "0 = issued,", "5-7 = reserved,", "12 = manufactured-disabled-", "frozen,", "14-15 = reserved"]
-- [("0","issued"),("5-7","reserved"),("12","manufactured-disabled-frozen"),("14-15","reserved")]
descriptionItems :: [Text] -> [(Text, Text)]
descriptionItems ls = mapMaybe item (T.split (== ',') joined)
  where
    joined = foldl' glue "" (filter (not . T.null) (map T.strip ls))
    glue acc l
      | T.null acc             = l
      | "-" `T.isSuffixOf` acc = acc <> l
      | otherwise              = acc <> " " <> l
    item piece = case DT.breakOn "=" piece of
      (before, after)
        | Just rest <- T.stripPrefix "=" after
        , let value = T.unwords (reverse (takeWhile isValue (reverse (T.words before))))
              label = T.dropWhileEnd (== '.') (T.strip rest)
        , not (T.null value), not (T.null label) -> Just (value, label)
      _ -> Nothing
    isValue w = T.all (\c -> C.isDigit c || c `elem` ['-', '\x2013', '\x2014']) w

-- | An enumeration's value cell: @n@, @a-b@ (a hyphen, an en dash or an em
--   dash, spaced or not: Opal T31 prints @3 – 4@, Core T52 @2-7@), or @a W b@
--   for a range word @W@ the binding declares (@rangeWords ["to"]@).
parseEnumValue :: [Text] -> Text -> Maybe [Int]
parseEnumValue rangeWords cell = case T.words (squeezed cell) of
  [single] -> case DT.splitOn "-" single of
    [n]    -> pure <$> number n
    [a, b] -> fromTo a b
    _      -> Nothing
  [a, w, b] | w `elem` rangeWords -> fromTo a b
  _ -> Nothing
  where
    -- every dash a hyphen, with no space on either side of it
    squeezed = T.intercalate "-" . map T.strip . DT.splitOn "-" . T.map dash . T.strip
    dash c = if c `elem` ['–', '—'] then '-' else c
    number n = if not (T.null n) && T.all C.isDigit n then readMaybe (T.unpack n) else Nothing
    fromTo a b = do
      lo <- number a
      hi <- number b
      if lo <= hi then Just [lo .. hi] else Nothing


-- * The enumeration check

-- | What C-ENUM-RANGE concludes about one enumeration: its findings, and the
--   pieces of its declaration when it can be generated (name, labelled values,
--   type UID, range).
data EnumCheck = EnumCheck
  { ecFindings :: ![Finding]
  , ecDecl     :: !(Maybe (Text, [(Text, Int)], [Word8], (Int, Int)))
  }

-- | Check an enumeration against the type row it joins (by the identifier its caption names).
checkEnum :: Context -> [TypeRowRead] -> EnumRead -> EnumCheck
checkEnum ctx rows e = case companion of
  Nothing -> failed [bad ("no type row for " <> fromMaybe "the caption's type" (erIdent e) <> " to take the range from")]
  Just tr -> case trTypeDef tr >>= rangeOf tr of
    Left f         -> failed [f]
    Right (lo, hi) -> case traverse parsed (zip [0 ..] (erRows e)) of
      Left f      -> failed [f]
      Right given ->
        let assigned  = concat [ [ (v, (label, i)) | v <- vs ] | (i, label, vs) <- given ]
            outside   = [ (v, label, i) | (v, (label, i)) <- assigned, v < lo || v > hi ]
            kept      = Map.toList (Map.fromListWith (\_ old -> old) [ (v, label) | (v, (label, _)) <- assigned ])
            overlaps  = [ (v, lis) | (v, lis) <- Map.toList (Map.fromListWith (flip (<>)) [ (v, [li]) | (v, li) <- assigned ]), length lis > 1 ]
            repeated  = [ (l, is) | (l, is) <- Map.toList (Map.fromListWith (flip (<>)) [ (label, [i]) | (i, label, _) <- given ]), length is > 1 ]
            labelled  = [ (label, v) | (v, label) <- kept ]
            nameless  = [ (cell, label, i) | (i, (cell, label)) <- zip [0 ..] (erRows e), not (T.any C.isAlphaNum label) ]
            coreName  = enumTypeName sid (trName tr)
            names     = map fst (constructorNames coreName (lo, hi) labelled)
            clashes   = [ n | (n, k) <- Map.toList (Map.fromListWith (+) [ (n, 1 :: Int) | n <- names ]), k > 1 ]
            merged    = [ srcs | srcs <- map (L.nub . map fst) (Map.elems (constructorGroups coreName (lo, hi) labelled)), length srcs > 1 ]
            findings  = map (\(v, l, i) -> atRows [i] (err ("value " <> tshow v <> " (" <> tshow l <> ") lies outside " <> tshow lo <> ".." <> tshow hi
                                                            <> " of " <> renderAnchorId (trAnchor tr) <> " " <> trName tr))) outside
                     <> map (\(c, l, i) -> atRows [i] (err ("the value " <> tshow c <> " has the label " <> tshow l <> ", which has no letter or digit to name a constructor"))) nameless
                     <> map (\(v, lis) -> atRows (map snd lis) (finding Warning "W-ENUM-OVERLAP"
                                            ("value " <> tshow v <> " is assigned by " <> T.intercalate " and " (map (tshow . fst) lis) <> "; the first is kept"))) overlaps
                     <> map (\(l, is) -> atRows is (finding Warning "W-ENUM-LABEL-REPEATED"
                                           ("the label " <> tshow l <> " is used by " <> tshow (length is) <> " rows; each of its values gets a suffixed constructor"))) repeated
                     <> map (\n -> bad ("the constructor " <> n <> " would be defined twice")) clashes
                     <> map (\srcs -> bad ("the " <> merging srcs <> " give the same constructor")) merged
        in EnumCheck findings
             (if any Cm.isError findings then Nothing
              else (\uidBytes -> (trName tr, labelled, uidBytes, (lo, hi))) <$> trUID tr)
  where
    sid      = cxSpec ctx
    aid      = renderAnchorId (erAnchor e)
    atTable' = fromMaybe 1 (tablePage (erTable e))
    here     = locatedAt sid aid atTable'
    -- a finding about rows, at the page the first of them is printed on
    atRows is = locatedAt sid aid (fromMaybe atTable' (listToMaybe (mapMaybe (\i -> join (listToMaybe (drop i (erPages e)))) (L.sort is))))
    err      = finding Error "C-ENUM-RANGE"
    bad      = here . err
    failed fs = EnumCheck fs Nothing
    companion = erIdent e >>= \i -> L.find ((== Just i) . trIdent) rows
    parsed (i, (cell, label)) = maybe (Left (atRows [i] (err ("the value cell " <> tshow cell <> " is not a number or a range"))))
                                      (Right . (i, label,)) (parseEnumValue (erRangeWords e) cell)
    -- the labels a merge joins, and the gaps of the range when their constructor is among them
    merging srcs = (if length (catMaybes srcs) > 1 then "labels " else "label ") <> T.intercalate " and " (map tshow (catMaybes srcs))
                   <> (if any isNothing srcs then " and the gaps of the range (_NA)" else "")
    rangeOf tr td = case td of
      EnumType ps -> bounds tr ps
      SetType ps  -> bounds tr ps
      _           -> Left (bad (renderAnchorId (trAnchor tr) <> " " <> trName tr <> " is not an Enumeration_Type or a Set_Type"))
    bounds tr ps = case traverse lits ps of
      Just ((lo, hi) : rest) -> Right (foldl' min lo (map fst rest), foldl' max hi (map snd rest))
      _ -> Left (bad ("the range of " <> renderAnchorId (trAnchor tr) <> " " <> trName tr <> " is not literal"))
    lits (Lit a, Lit b) = Just (a, b)
    lits _              = Nothing

-- | The type rows an enumeration may join by the identifier it names
--   ('checkEnum'): under @enumInDescription@ the table's own ('ownTypeRow');
--   otherwise the spec's type rows, then its base's ('allTypeRows': Opal's
--   Table 31 joins its own Table 30; Core's enumerations join Core's).
joinable :: Context -> EnumRead -> [TypeRowRead]
joinable ctx e = maybe (map snd (allTypeRows ctx)) pure (ownTypeRow ctx e)

-- | The type row an enumeration printed in a Description column prints
--   itself (Opal Table 49's UID, Name and Format cells, PDF 98), read as
--   "Hsed.Spec.Rules.TypeRow" reads a type row: its Format parsed against the
--   spec's Format environment ('formatEnv'), a failure located at the table;
--   none for any other enumeration.
ownTypeRow :: Context -> EnumRead -> Maybe TypeRowRead
ownTypeRow ctx e = typeRowOf <$> erOwnType e
  where
    typeRowOf (uidText, format) = TypeRowRead
      { trAnchor  = erAnchor e
      , trTable   = erTable e
      , trCaption = erCaption e
      , trIdent   = erIdent e
      , trName    = fromMaybe "" (erIdent e)
      , trUIDText = uidText
      , trUID     = parseUIDCell uidText
      , trFormat  = format
      , trTypeDef = first (locatedAt (cxSpec ctx) (renderAnchorId (erAnchor e)) (fromMaybe 1 (tablePage (erTable e))))
                          (parseFormat (fst (formatEnv ctx)) format)
      , trWaivers = [] }

-- | C-PATCH-UID, located at the table: each table a bound table's @replaces@
--   names (Opal Table 49's @enumInDescription (replaces [core 102, core
--   103])@) lies in a spec it is read over ('cxBase', and its base), and is
--   there either the type row of the type the table enumerates, with the
--   UID, the Name and (since the M4b review) the Format of the type row it
--   joins (Core Table 102, @00 00 00 05 00 00 04 05@, @life_cycle_state@ and
--   @Enumeration_Type, 0, 15@, PDF 113; Opal prints the same Format, PDF 98),
--   or the enumeration of that type (Core Table 103, PDF 113-114). So the
--   type the modified enumeration's instance names, the base's
--   @u\<type\>Type@, is the UID the table prints, and a cell of the type,
--   which the replaced row's Format reads, reads the range the generated
--   enumeration has. Nothing for a table that replaces nothing.
patchFindings :: Context -> BoundTable -> EnumRead -> [Finding]
patchFindings ctx bt e = map (atTable ctx bt . finding Error "C-PATCH-UID") (concatMap problems (replacesOf (btBinding bt)))
  where
    joined = erIdent e >>= \i -> L.find ((== Just i) . trIdent) (joinable ctx e)
    named b aid = specIdText (cxSpec b) <> " " <> renderAnchorId aid
    problems ref = case replacedIn ctx ref of
      Left why -> [why]
      Right (b, aid) -> case ( L.find ((== aid) . trAnchor) (rights (typeRowReads b))
                             , L.find ((== aid) . erAnchor) (rights (enumReads b)) ) of
        (Just tr, _) -> maybe [] (typeRowProblems b tr) joined
        (_, Just er) -> [ named b aid <> ", which it replaces, enumerates " <> fromMaybe "no type" (erIdent er)
                            <> ", not " <> fromMaybe "the type its caption names" (erIdent e)
                        | erIdent er /= erIdent e ]
        _            -> [ "it replaces " <> named b aid <> ", which is no type row or enumeration of " <> specIdText (cxSpec b) ]
    typeRowProblems b tr own =
      [ "the UID " <> uidOf own <> " is not " <> uidOf tr <> ", that of " <> named b (trAnchor tr) <> " " <> trName tr <> ", which it replaces"
      | trUID own /= trUID tr ]
      <> [ "the type " <> trName own <> " is not " <> named b (trAnchor tr) <> "'s " <> trName tr <> ", which it replaces"
         | trName own /= trName tr ]
      <> [ "the Format " <> tshow (T.strip (trFormat own)) <> " is not " <> named b (trAnchor tr) <> "'s " <> tshow (T.strip (trFormat tr))
             <> ", which it replaces: a cell of the type is read by the replaced Format, the enumeration by this one"
         | Right mine <- [trTypeDef own], Right theirs <- [trTypeDef tr], mine /= theirs ]
    uidOf tr = maybe (tshow (trUIDText tr)) hexUID (trUID tr)

-- * Declarations

-- | One enumeration table: every value of the table with its label, a row
--   @2-7 Reserved@ already expanded to six pairs.
data EnumDecl = EnumDecl
  { name       :: !Text            -- ^ the Type object's name, @adv_key_mode@
  , ctors      :: ![(Text, Int)]
  , typeUID    :: ![Word8]         -- ^ the Type object's UID
  , range      :: !(Int, Int)      -- ^ from the Type object's Format
  , provenance :: !Gen.Provenance
  }
  deriving (Eq, Show)

-- | How the 'Hsed.Core.Types.Enumerated' instance names its type's UID.
data UIDRef
  = SameModule          -- ^ @u\<type\>Type@, declared in the same module
  | InModule !Text      -- ^ @u\<type\>Type@, imported from the named module, the whole of it
  | ImportedFrom !Text
    -- ^ @u\<type\>Type@ alone imported from the named module: a base's
    --   @Types@, beside which the spec's own is imported whole (Opal's
    --   @life_cycle_state@ from @Hsed.Gen.Core.Types@)
  | Literal             -- ^ spelled out, @uid 0x00 ...@
  deriving (Eq, Show)

-- | Each bound table's declaration, or why it has none: an Error of
--   C-ENUM-RANGE, of its own type row's UID cell (C-UID-TABLE) or of
--   C-PATCH-UID leaves it out.
compile :: Context -> [BoundTable] -> [Emitted]
compile ctx = map one
  where
    one bt = case readEnum ctx bt of
      Left _   -> Excluded (btAnchor bt) "C-ENUM-RANGE failed"
      Right er ->
        let ec     = checkEnum ctx (joinable ctx er) er
            failed = [ c | (c, fs) <- [ ("C-ENUM-RANGE", ecFindings ec), ("C-UID-TABLE", ownUIDFindings ctx er)
                                      , ("C-PATCH-UID", patchFindings ctx bt er) ]
                         , any Cm.isError fs ]
        in case (failed, enumDecl ctx er ec) of
             ([], Right d)  -> Emit (renderEnum (uidRefOf ctx bt er) d)
             ([], Left why) -> Excluded (erAnchor er) why
             (cs, _)        -> Excluded (erAnchor er) (T.intercalate " and " cs <> " failed")

-- | Where an enumeration's instance takes its type's UID from, by the origin
--   of the type row it joins: the spec's own @Types@ module, whole; a base's,
--   the one name (Opal's Table 31 from @Hsed.Gen.Opal.Types@, Table 30's;
--   Table 49 from @Hsed.Gen.Core.Types@, Core Table 102's, the type row it
--   replaces); spelled out for a table under @enumInDescription@ that
--   replaces no type row, whose type row no rule generates.
uidRefOf :: Context -> BoundTable -> EnumRead -> UIDRef
uidRefOf ctx bt er = maybe Literal fromSpec origin
  where
    sid = cxSpec ctx
    fromSpec s
      | s /= sid                   = ImportedFrom (Gen.moduleName s typesModule)
      | enumsModule == typesModule = SameModule
      | otherwise                  = InModule (Gen.moduleName sid typesModule)
    origin = case erOwnType er of
      Just _  -> listToMaybe [ cxSpec b | Right (b, aid) <- map (replacedIn ctx) (replacesOf (btBinding bt))
                                        , any ((== aid) . trAnchor) (rights (typeRowReads b)) ]
      Nothing -> listToMaybe [ cxSpec c | Just i <- [erIdent er], (c, tr) <- allTypeRows ctx, trIdent tr == Just i ]

-- | An enumeration's declaration from its C-ENUM-RANGE result, or why it has none.
enumDecl :: Context -> EnumRead -> EnumCheck -> Either Text EnumDecl
enumDecl ctx er ec = case ecDecl ec of
  Nothing -> Left "C-ENUM-RANGE failed"
  Just (n, cs, uidBytes, rng) -> Right EnumDecl
    { name       = n
    , ctors      = cs
    , typeUID    = uidBytes
    , range      = rng
    , provenance = provenanceOf ctx (erAnchor er) (erTable er) (erCaption er) }

-- | @data Core_\<type\>@ and its 'Hsed.Core.Types.Enumerated' instance.
renderEnum :: UIDRef -> EnumDecl -> Gen.Decl
renderEnum ref e = (Gen.decl e.provenance e.name body)
  { Gen.declImports = [ Gen.ImportAll Gen.coreTypesModule, Gen.ImportSome "Prelude" ["Bounded", "Enum", "Eq", "Ord", "Show"] ]
                      <> case ref of
                           SameModule     -> []
                           InModule m     -> [Gen.ImportAll m]
                           ImportedFrom m -> [Gen.ImportSome m [typeUIDName e.name]]
                           Literal        -> [Gen.ImportSome Gen.uidModule ["uid"]] }
  where
    coreName = enumTypeName e.provenance.spec e.name
    doc = Gen.provenanceComment e.provenance
    body =
      [ doc, "data " <> coreName ]
      <> zipWith (\i (ctor, _) -> (if i == (0 :: Int) then "  = " else "  | ") <> ctor) [0 ..] (constructorNames coreName e.range e.ctors)
      <> [ "  deriving (Bounded, Enum, Eq, Ord, Show)"
         , ""
         , doc
         , "instance Enumerated " <> coreName <> " where"
         , "  typeUID = " <> if ref == Literal then Gen.uidExpr e.typeUID else typeUIDName e.name
         , "  range   = (" <> tshow (fst e.range) <> ", " <> tshow (snd e.range) <> ")"
         ]

-- | The data type of an enumeration: the spec's label and the type's name,
--   @Core_adv_key_mode@.
enumTypeName :: SpecId -> Text -> Text
enumTypeName sid typeName = Gen.specLabel sid <> "_" <> typeName

-- | The constructors of an enumeration's data type ('enumTypeName') in value
--   order: the type, then the label scrubbed (commas dropped, every character
--   but a letter, a digit, an underscore or an apostrophe made an underscore),
--   and @_NA@ for every value of the range no label covers; a label (or
--   @_NA@) given several values names each of them suffixed @_\<v\>@, the
--   first included, and one given a single value stays bare (design section
--   13.15: a name carries its value, so Core's @Reserved_4@ (T128) and Opal's
--   (T16) name the same value wherever their ranges start).
constructorNames :: Text -> (Int, Int) -> [(Text, Int)] -> [(Text, Int)]
constructorNames coreName rng given = sortOn snd (concatMap suffixed (Map.toList (constructorGroups coreName rng given)))
  where
    suffixed (n, [(_, v)]) = [(n, v)]
    suffixed (n, vs)       = [ (n <> "_" <> tshow v, v) | (_, v) <- vs ]

-- | Each constructor's name before its suffix, with the values it names and
--   what gives each: its label, or the gaps of the range ('Nothing', @_NA@).
--   A name given by two labels, or by a label and the gaps, merges them, which
--   'checkEnum' refuses.
constructorGroups :: Text -> (Int, Int) -> [(Text, Int)] -> Map Text [(Maybe Text, Int)]
constructorGroups coreName (lo, hi) given = Map.fromListWith (flip (<>))
  ([ (coreName <> "_" <> scrubLabel label, [(Just label, v)]) | (label, v) <- given ]
   <> [ (coreName <> "_NA", [ (Nothing, v) | v <- missing ]) | not (null missing) ])
  where
    missing = [lo .. hi] \\ map snd given

-- | A label as a constructor's name takes it: commas dropped, and every
--   character a name cannot hold (a space, a dash, a parenthesis) an
--   underscore; letters, digits, underscores and apostrophes kept.
scrubLabel :: Text -> Text
scrubLabel = T.map (\c -> if C.isAlphaNum c || c == '_' || c == '\'' then c else '_') . T.filter (/= ',')
