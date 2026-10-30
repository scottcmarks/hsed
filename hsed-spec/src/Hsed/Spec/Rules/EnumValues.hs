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
distinct, and two labels that scrub to one constructor are an Error; a label
used for more than one row is @W-ENUM-LABEL-REPEATED@ (its later values get
suffixed constructors).

Compiled into 'enumsModule': @data Core_\<type\>@ (the spec's label,
"Hsed.Spec.Gen".'Gen.specLabel', then the type's name) with the constructor names
today's @dEnum@ produces (the label scrubbed, the first value of a label bare
and the later ones suffixed @_\<v\>@, @_NA@ fillers for the gaps of the range),
plus an 'Hsed.Core.Types.Enumerated' instance naming the type row's
@u\<type\>Type@ and the range.
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
  , Gen.notes       = [ "The constructors are named as hsed's hand-written dEnum names them (the label"
                      , "scrubbed, a label's later values suffixed, _NA for the gaps of the range), for"
                      , "the M2a runtime comparison." ] }

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
           , erRangeWords = mRangeWords (btMods bt) }

-- | An enumeration's value cell: @n@, @a-b@ (a hyphen or an en dash), or @a W b@
--   for a range word @W@ the binding declares (@rangeWords ["to"]@).
parseEnumValue :: [Text] -> Text -> Maybe [Int]
parseEnumValue rangeWords cell = case T.words (T.map dash (T.strip cell)) of
  [single] -> case DT.splitOn "-" single of
    [n]    -> pure <$> number n
    [a, b] -> fromTo a b
    _      -> Nothing
  [a, w, b] | w `elem` rangeWords -> fromTo a b
  _ -> Nothing
  where
    dash c = if c == '–' then '-' else c
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
    Right (lo, hi) -> case traverse parsed (erRows e) of
      Left f      -> failed [f]
      Right given ->
        let assigned  = concat [ [ (v, label) | v <- vs ] | (label, vs) <- given ]
            outside   = [ (v, label) | (v, label) <- assigned, v < lo || v > hi ]
            kept      = Map.toList (Map.fromListWith (\_ old -> old) [ (v, label) | (v, label) <- assigned ])
            overlaps  = [ (v, ls) | (v, ls) <- Map.toList (Map.fromListWith (flip (<>)) [ (v, [l]) | (v, l) <- assigned ]), length ls > 1 ]
            repeated  = [ (l, n) | (l, n) <- Map.toList (Map.fromListWith (+) [ (label, 1 :: Int) | (label, _) <- given ]), n > 1 ]
            labelled  = [ (label, v) | (v, label) <- kept ]
            nameless  = [ (cell, label) | (cell, label) <- erRows e, not (T.any C.isAlphaNum label) ]
            names     = map fst (constructorNames (enumTypeName sid (trName tr)) (lo, hi) labelled)
            clashes   = [ n | (n, k) <- Map.toList (Map.fromListWith (+) [ (n, 1 :: Int) | n <- names ]), k > 1 ]
            merged    = [ ls | ls <- map L.nub (Map.elems (Map.fromListWith (flip (<>)) [ (scrubbed l, [l]) | (l, _) <- given ])), length ls > 1 ]
            findings  = map (\(v, l) -> bad ("value " <> tshow v <> " (" <> tshow l <> ") lies outside " <> tshow lo <> ".." <> tshow hi
                                              <> " of " <> renderAnchorId (trAnchor tr) <> " " <> trName tr)) outside
                     <> map (\(c, l) -> bad ("the value " <> tshow c <> " has the label " <> tshow l <> ", which has no letter or digit to name a constructor")) nameless
                     <> map (\(v, ls) -> warn "W-ENUM-OVERLAP" ("value " <> tshow v <> " is assigned by " <> T.intercalate " and " (map tshow ls) <> "; the first is kept")) overlaps
                     <> map (\(l, n) -> warn "W-ENUM-LABEL-REPEATED" ("the label " <> tshow l <> " is used by " <> tshow n <> " rows; its later values get suffixed constructors")) repeated
                     <> map (\n -> bad ("the constructor " <> n <> " would be defined twice")) clashes
                     <> map (\ls -> bad ("the labels " <> T.intercalate " and " (map tshow ls) <> " give the same constructor")) merged
        in EnumCheck findings
             (if any Cm.isError findings then Nothing
              else (\uidBytes -> (trName tr, labelled, uidBytes, (lo, hi))) <$> trUID tr)
  where
    sid      = cxSpec ctx
    here     = locatedAt sid (renderAnchorId (erAnchor e)) (fromMaybe 1 (tablePage (erTable e)))
    bad      = here . finding Error "C-ENUM-RANGE"
    warn c   = here . finding Warning c
    failed fs = EnumCheck fs Nothing
    companion = erIdent e >>= \i -> L.find ((== Just i) . trIdent) rows
    parsed (cell, label) = maybe (Left (bad ("the value cell " <> tshow cell <> " is not a number or a range")))
                                 (Right . (label,)) (parseEnumValue (erRangeWords e) cell)
    rangeOf tr td = case td of
      EnumType ps -> bounds tr ps
      SetType ps  -> bounds tr ps
      _           -> Left (bad (renderAnchorId (trAnchor tr) <> " " <> trName tr <> " is not an Enumeration_Type or a Set_Type"))
    bounds tr ps = case traverse lits ps of
      Just ((lo, hi) : rest) -> Right (foldl' min lo (map fst rest), foldl' max hi (map snd rest))
      _ -> Left (bad ("the range of " <> renderAnchorId (trAnchor tr) <> " " <> trName tr <> " is not literal"))
    lits (Lit a, Lit b) = Just (a, b)
    lits _              = Nothing
    scrubbed l = T.map (\c -> if C.isAlphaNum c then c else '_') (T.filter (/= ',') l)


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
--   order, as today's @dEnum@ names them: the type, then the label with commas
--   dropped and spaces and dashes turned to underscores, the first value of a
--   label bare and the later ones suffixed @_\<v\>@, and @_NA@ for every value
--   of the range no label covers.
constructorNames :: Text -> (Int, Int) -> [(Text, Int)] -> [(Text, Int)]
constructorNames coreName (lo, hi) given = sortOn snd (concatMap suffixed (Map.toList groups))
  where
    groups   = Map.fromListWith (flip (<>)) ([ (coreName <> "_" <> scrub label, [v]) | (label, v) <- given ] <> filler)
    missing  = [lo .. hi] \\ map snd given
    filler   = [ (coreName <> "_NA", missing) | not (null missing) ]
    suffixed (n, v : vs) = (n, v) : [ (n <> "_" <> tshow v', v') | v' <- vs ]
    suffixed (_, [])     = []
    scrub = T.map identifier . T.intercalate "_" . T.split (`elem` [' ', '-']) . T.filter (/= ',')
    identifier c = if C.isAlphaNum c || c == '_' || c == '\'' then c else '_'
