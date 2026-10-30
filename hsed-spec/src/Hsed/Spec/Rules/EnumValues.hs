{-# LANGUAGE DuplicateRecordFields #-}
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

Compiled into 'enumsModule': @data Core_\<type\>@ (the spec's label,
"Hsed.Spec.Gen".'Gen.specLabel', then the type's name) with a constructor per
value (the label scrubbed; a label that names several values, a range or
repeated rows, gives each its own constructor suffixed @_\<v\>@, the first
included, @Core_adv_key_mode_Reserved_2@, and a label naming one value stays
bare; @_NA@ fills the gaps of the range, each suffixed when there are several:
design section 13.15, as decided at M2d), plus an
'Hsed.Core.Types.Enumerated' instance naming the type row's @u\<type\>Type@
and the range.
-}
module Hsed.Spec.Rules.EnumValues
  ( enumValuesRule
  , enumsModule
    -- * Reading the bound tables
  , EnumRead (..)
  , enumReads
  , enumIdent
  , parseEnumValue
    -- * The enumeration check
  , EnumCheck (..)
  , checkEnum
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
import qualified Hsed.Spec.Gen           as Gen
import           Hsed.Spec.IR            (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TypeRow (TypeRowRead (..), typeRowReads, typeUIDName, typesModule)
import           Hsed.Spec.Shape         (CaptionKind (..), Form (..), Shape (..), measure, normaliseCaption,
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
      , CodeDef "W-ENUM-LABEL-REPEATED" [Warning]        (Suspect "an enumeration gives one label to several rows") ]
  , ruleChecks  = \ctx bts -> concatMap (either pure (ecFindings . checkEnum ctx (rights (typeRowReads ctx)))) (readEnums ctx bts)
  , ruleCompile = compile
  }

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
  }

-- | Every table bound as an enumeration, read under the errata set in its bound
--   columns (@cols a b@, else 0 and 1).
enumReads :: Context -> [Either Finding EnumRead]
enumReads ctx = readEnums ctx (boundTables (Just EnumValues) ctx)

readEnums :: Context -> [BoundTable] -> [Either Finding EnumRead]
readEnums ctx = map readEnum
  where
    readEnum bt =
      let (a, b)    = fromMaybe (0, 1) (mCols (btMods bt))
          cell :: Int -> [CellIR] -> Text
          cell i r  = maybe "" (.text) (listToMaybe (drop i r))
          t'        = btRead bt
      in Right EnumRead
           { erAnchor = btAnchor bt, erTable = btTable bt, erCaption = normaliseCaption (fromMaybe "" t'.caption)
           , erIdent = enumIdent (measure (cxSpec ctx) t'), erRows = [ (cell a r, cell b r) | r <- t'.rows ]
           , erPages = [ (+ 1) <$> listToMaybe (mapMaybe (.page) r) | r <- t'.rows ]
           , erRangeWords = mRangeWords (btMods bt) }

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
  = SameModule      -- ^ @u\<type\>Type@, declared in the same module
  | InModule !Text  -- ^ @u\<type\>Type@, imported from the named module
  | Literal         -- ^ spelled out, @uid 0x00 ...@
  deriving (Eq, Show)

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts =
  [ either (Excluded (erAnchor er)) (Emit . renderEnum uidRef) (enumDecl ctx er (checkEnum ctx rows er))
  | er <- rights (readEnums ctx bts) ]
  where
    rows   = rights (typeRowReads ctx)
    uidRef = if enumsModule == typesModule then SameModule else InModule (Gen.moduleName (cxSpec ctx) typesModule)

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
                           SameModule -> []
                           InModule m -> [Gen.ImportAll m]
                           Literal    -> [Gen.ImportSome Gen.uidModule ["uid"]] }
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
--   order: the type, then the label scrubbed ('scrubLabel'), and @_NA@ for
--   every value of the range no label covers; a label (or @_NA@) given several
--   values names each of them suffixed @_\<v\>@, the first included, and one
--   given a single value stays bare (design section 13.15: a name carries its
--   value, so Core's @Reserved_4@ (T128) and Opal's (T16) name the same value
--   wherever their ranges start).
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
