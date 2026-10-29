{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}

{-|
Module      : Hsed.Spec.Check
Description : The checks of hsedc check, and the Compile monad they and the generator run in
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 4.2: @Compile = RWST Env [Finding] Artefacts (Either Finding)@.
The environment is one IR under one errata set with its classification
("Hsed.Spec.Rules") and the 'Env' the Format cells resolve against, built from
the IR itself: the Type objects of the type-row tables and the table names of
the T240-shaped tables. The checks of M1, each emitting findings with the code
in brackets:

* classification: annotations refused (@E-ANCHOR@, @E-ANCHOR-KIND@,
  @E-SYNTAX@, @E-GRAMMAR@, @E-ERRATUM-PUBLISHED@, @E-MODIFIER@), @E-AMBIGUOUS@,
  @W-UNBOUND@ ("Hsed.Spec.Rules");
* @C-EXEMPLAR@: each rule still fires on its exemplar;
* @E-SHAPE@: every bound table's form is accepted by its binding ('bindingAccepts');
* @C-UID-TABLE@: a type row's UID is eight bytes with upper half @00 00 00 05@;
* @C-UID-UNIQUE@: no two type rows share a UID; a type row's @waive
  "C-UID-UNIQUE" "T\<n\> \<uid\>" "\<reason\>"@ makes the Error a Warning for
  that table when it and table @n@, and no other, print that UID, and a waiver
  that waives nothing is @W-WAIVER-UNUSED@;
* @C-FMT-RESOLVE@: every Format cell parses ('parseFormat'), its findings
  carrying "Hsed.Spec.Format"'s codes (@E-FMT-RESOLVE@ and friends);
* @C-ENUM-RANGE@: an enumeration's values lie within its type row's range, each
  value is assigned once (@W-ENUM-OVERLAP@ otherwise, the first row kept) and its
  constructors are distinct; a label used for more than one row is
  @W-ENUM-LABEL-REPEATED@.

Errata ("Hsed.Spec.Errata"): every bound table is read under the selected set,
its caption, header cells and data cells corrected by the @erratum scott [...]@
pairs its own annotation carries, and by nothing else: under @scott@ each
correction made is @W-ERRATUM-APPLIED@, under @published@ each defect left as
printed is @W-ERRATUM-KNOWN@, and under both a correction whose text the table
does not contain is @W-ERRATUM-UNUSED@. Everything that reads a bound table
(the Format environment, the type rows, the enumerations, the identifier a
caption names) reads the corrected one; the rules fire on the text as printed.

Normalisations, the same under both sets, are Notes: a caption printed
@\<X\>_object _ref@ (@N-CAPTION-NORMALISED@), a header cell in the typo table
(@N-HEADER-NORMALISED@), and a Format cell's spacing at a brace or its table's
@Table_UID@ or @Table@ suffix (@N-FORMAT-NORMALISED@).
-}
module Hsed.Spec.Check
  ( -- * The Compile monad
    CompileEnv (..)
  , mkCompileEnv
  , Artefacts (..)
  , Compile
  , runCompile
    -- * Reading the bound tables
  , TypeRowRead (..)
  , EnumRead (..)
  , EnumCheck (..)
  , typeRowReads
  , enumReads
  , checkEnum
  , bindingOf
  , typeEnvFrom
  , tableNamesFrom
  , parseUIDCell
  , parseEnumValue
  , hexUID
    -- * Checks
  , checks
  , checkIR
  , sortFindings
  , Mods (..)
  , modsOf
  ) where

import           Control.Monad.RWS.Strict (RWST, runRWST, tell)
import qualified Data.Char                as DC
import qualified Data.Text                as DT
import qualified RIO.Char                 as C
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import           RIO

import           Hsed.Core.Types          (Operand (..), TypeDef (..))
import           Hsed.Spec.Anchor         (AnchorId (..), locationText, naturalKey, parseAnchorId, renderAnchorId)
import           Hsed.Spec.Common         (ErrataSet (..), Finding, Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.DSL            (Binding (..), SomeBinding (..), bindingAccepts,
                                           someBindingShape)
import           Hsed.Spec.Errata         (Erratum (..), correctTable)
import           Hsed.Spec.Format         (Env, formatNormalisations, mkEnv, parseFormat)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (CellIR (..), SpecIR (..), TableIR (..))
import           Hsed.Spec.Rules
import           Hsed.Spec.Shape          (Shape (..), headerNormalisations, measure, normHeader, normaliseCaption,
                                           rawCaptionTitle)


-- * The Compile monad

-- | What every check and every generator reads.
data CompileEnv = CompileEnv
  { ceIR             :: !SpecIR
  , ceSpec           :: !SpecId
  , ceErrata         :: !ErrataSet
  , ceTables         :: !(Map AnchorId TableIR)          -- ^ the numbered tables
  , ceClasses        :: !(Map AnchorId Classification)
  , ceClassFindings  :: ![Finding]                        -- ^ what the classification found
  , ceCorrected      :: !(Map AnchorId TableIR)           -- ^ the bound tables, read under the errata set
  , ceErrataFindings :: ![Finding]                        -- ^ what reading them under the set found
  , ceFormatEnv      :: !Env
  , ceEnvFindings    :: ![Finding]                        -- ^ what building the 'Env' found
  }

mkCompileEnv :: ErrataSet -> SpecIR -> CompileEnv
mkCompileEnv es ir = CompileEnv
  { ceIR             = ir
  , ceSpec           = sid
  , ceErrata         = es
  , ceTables         = tables
  , ceClasses        = classes
  , ceClassFindings  = classFindings
  , ceCorrected      = fmap fst corrected
  , ceErrataFindings = concatMap snd (Map.elems corrected)
  , ceFormatEnv      = mkEnv (typeEnvFrom (Map.union (fmap fst corrected) tables) classes) names
  , ceEnvFindings    = nameFindings
  }
  where
    sid                    = SpecId ir.spec
    tables                 = numberedTables ir
    (classes, classFindings) = classifyWith ir
    (nameFindings, names)  = either (\f -> ([f], [])) ([],) (tableNamesFrom ir)
    corrected = Map.fromList
      [ (aid, correctTable es sid tid page (map erratum (mErrata (modsOf sb))) t)
      | (aid, t) <- Map.toList tables, Just sb <- [Map.lookup aid classes >>= boundBinding]
      , let tid = renderAnchorId aid
            page = tablePage t
            erratum (from, into) = Erratum sid tid from into ("annotation at " <> locationText sid (fromMaybe 1 page) aid) ]

-- | What a compilation produces besides findings.
data Artefacts = Artefacts
  { artDecls    :: ![Gen.Decl]
  , artExcluded :: ![(AnchorId, Text)]   -- ^ bound tables left out of the generated module, and why
  }
  deriving (Eq, Show)

instance Semigroup Artefacts where
  Artefacts a b <> Artefacts c d = Artefacts (a <> c) (b <> d)
instance Monoid Artefacts where
  mempty = Artefacts [] []

-- | Design section 4.2. 'Left' is a failure that stops the compilation.
type Compile = RWST CompileEnv [Finding] Artefacts (Either Finding)

runCompile :: CompileEnv -> Compile a -> Either Finding (a, Artefacts, [Finding])
runCompile env m = runRWST m env mempty


-- * The Format environment, from the IR itself

-- | The Type objects: the Name cell of every table bound as a type row, with its table number.
typeEnvFrom :: Map AnchorId TableIR -> Map AnchorId Classification -> [(Text, Maybe Int)]
typeEnvFrom tables classes =
  [ (name, t.tableNumber)
  | (aid, c) <- Map.toList classes, bindingShape c == Just TypeRow
  , Just t <- [Map.lookup aid tables], [_, nameCell, _] : _ <- [t.rows], let name = T.strip nameCell.text ]
  where bindingShape c = boundBinding c >>= someBindingShape

-- | The table names a @uidref{...}@ may name: the @Table Name@ column of every
--   table with T240's header (@UID of Table Descriptor Object, UID of Table,
--   Table Name, Template@).
tableNamesFrom :: SpecIR -> Either Finding [Text]
tableNamesFrom ir = case [ t | t <- ir.tables, header t == t240 ] of
  []     -> Left ((finding Error "C-FMT-RESOLVE" "no table of table UIDs (T240's header) to resolve uidref{...} against")
                    { Cm.spec = Just (SpecId ir.spec) })
  ts     -> Right [ T.strip c.text | t <- ts, row <- t.rows, c <- take 1 (drop 2 row), not (T.null (T.strip c.text)) ]
  where
    header :: TableIR -> [Text]
    header t = maybe [] (map (normHeader . (.text))) (listToMaybe t.headerRows)
    t240 = ["uidoftabledescriptorobject", "uidoftable", "tablename", "template"]


-- * Reading the bound tables under an errata set

-- | The binding a table ends up with.
bindingOf :: CompileEnv -> AnchorId -> Maybe SomeBinding
bindingOf env aid = Map.lookup aid (ceClasses env) >>= boundBinding

-- | One type-row table read under the errata set.
data TypeRowRead = TypeRowRead
  { trAnchor  :: !AnchorId
  , trTable   :: !TableIR                    -- ^ as printed (its cells are what the manifest hashes)
  , trCaption :: !Text                       -- ^ after errata, normalised
  , trIdent   :: !(Maybe Text)               -- ^ the corrected caption's identifier
  , trName    :: !Text                       -- ^ the Name cell after errata
  , trUIDText :: !Text                       -- ^ the UID cell after errata
  , trUID     :: !(Maybe [Word8])
  , trFormat  :: !Text                       -- ^ the Format cell after errata
  , trTypeDef :: !(Either Finding TypeDef)   -- ^ located at the table
  , trWaivers :: ![(Text, Text, Text)]       -- ^ the annotation's waivers: check, subject, reason
  }

-- | One enumeration table read under the errata set.
data EnumRead = EnumRead
  { erAnchor  :: !AnchorId
  , erTable   :: !TableIR              -- ^ as printed
  , erCaption :: !Text                 -- ^ after errata, normalised
  , erIdent   :: !(Maybe Text)         -- ^ the type the corrected caption names
  , erRows    :: ![(Text, Text)]       -- ^ value cell and label after errata, in the bound columns
  , erRangeWords :: ![Text]
  }

-- | The tables bound with a shape, in anchor order, with their bindings and
--   their text read under the errata set.
boundWith :: Shape -> CompileEnv -> [(AnchorId, TableIR, TableIR, SomeBinding)]
boundWith shape env =
  [ (aid, t, Map.findWithDefault t aid (ceCorrected env), sb)
  | (aid, t) <- Map.toList (ceTables env), Just sb <- [bindingOf env aid], someBindingShape sb == Just shape ]

-- | The modifiers of a binding that the readers need: Scott's corrections (the
--   grammar admits no other set), the bound columns, the range words and the waivers.
data Mods = Mods
  { mErrata     :: [(Text, Text)]
  , mCols       :: Maybe (Int, Int)
  , mRangeWords :: [Text]
  , mWaivers    :: [(Text, Text, Text)]
  }

modsOf :: SomeBinding -> Mods
modsOf = \case
  SomeBinding b -> go b
  PolyBinding b -> go (b :: Binding 'Prose)
  where
    go :: Binding s -> Mods
    go = \case
      WithErratum Scott ps b    -> let m = go b in m { mErrata = ps <> mErrata m }
      WithErratum Published _ b -> go b
      WithWaiver c sj r b       -> let m = go b in m { mWaivers = (c, sj, r) : mWaivers m }
      WithCols a c b            -> (go b) { mCols = Just (a, c) }
      WithRangeWords ws b       -> let m = go b in m { mRangeWords = ws <> mRangeWords m }
      WithEnumInDescription b   -> go b
      WithExpand b              -> go b
      WithAs b                  -> go b
      _                         -> Mods [] Nothing [] []

-- | Every table bound as a type row, read under the errata set; a table that is
--   not one row of three cells is a finding.
typeRowReads :: CompileEnv -> [Either Finding TypeRowRead]
typeRowReads env = [ readRow aid t t' sb | (aid, t, t', sb) <- boundWith TypeRow env ]
  where
    sid = ceSpec env
    readRow aid t t' sb = case t'.rows of
      [[uidCell, nameCell, formatCell]] ->
        let here = locatedAt sid (renderAnchorId aid) (fromMaybe 1 (tablePage t))
        in Right TypeRowRead
             { trAnchor = aid, trTable = t, trCaption = normaliseCaption (fromMaybe "" t'.caption)
             , trIdent = tableIdent (measure sid t')
             , trName = T.strip nameCell.text, trUIDText = uidCell.text, trUID = parseUIDCell uidCell.text
             , trFormat = formatCell.text, trTypeDef = first here (parseFormat (ceFormatEnv env) formatCell.text)
             , trWaivers = mWaivers (modsOf sb) }
      rows -> Left (locatedAt sid (renderAnchorId aid) (fromMaybe 1 (tablePage t)) (finding Error "E-SHAPE"
                ("a type row table holds one row of three cells; this one has " <> tshow (map length rows))))

-- | Every table bound as an enumeration, read under the errata set in its bound
--   columns (@cols a b@, else 0 and 1).
enumReads :: CompileEnv -> [Either Finding EnumRead]
enumReads env = [ readEnum aid t t' sb | (aid, t, t', sb) <- boundWith EnumValues env ]
  where
    sid = ceSpec env
    readEnum aid t t' sb =
      let mods      = modsOf sb
          (a, b)    = fromMaybe (0, 1) (mCols mods)
          cell :: Int -> [CellIR] -> Text
          cell i r  = maybe "" (.text) (listToMaybe (drop i r))
      in Right EnumRead
           { erAnchor = aid, erTable = t, erCaption = normaliseCaption (fromMaybe "" t'.caption)
           , erIdent = tableIdent (measure sid t'), erRows = [ (cell a r, cell b r) | r <- t'.rows ]
           , erRangeWords = mRangeWords mods }

-- | A UID cell: sixteen hex digits once whitespace is removed.
parseUIDCell :: Text -> Maybe [Word8]
parseUIDCell cell
  | T.length hex == 16, T.all C.isHexDigit hex = Just (map byte (DT.chunksOf 2 hex))
  | otherwise = Nothing
  where
    hex = T.filter (not . C.isSpace) cell
    byte = T.foldl' (\acc c -> acc * 16 + fromIntegral (DC.digitToInt c)) 0

hexUID :: [Word8] -> Text
hexUID = T.unwords . map hex2
  where hex2 w = T.pack [digit (w `div` 16), digit (w `mod` 16)]
        digit d = C.toUpper (DC.intToDigit (fromIntegral d))

-- | An enumeration's value cell: @n@, @a-b@ (a hyphen or an en dash), or @a W b@
--   for a range word @W@ the binding declares (@rangeWords ["to"]@).
parseEnumValue :: [Text] -> Text -> Maybe [Int]
parseEnumValue rangeWords cell = case T.words (T.map dash (T.strip cell)) of
  [single] -> case DT.splitOn "-" single of
    [n]    -> pure <$> number n
    [a, b] -> range a b
    _      -> Nothing
  [a, w, b] | w `elem` rangeWords -> range a b
  _ -> Nothing
  where
    dash c = if c == '–' then '-' else c
    number n = if not (T.null n) && T.all C.isDigit n then readMaybe (T.unpack n) else Nothing
    range a b = do
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
checkEnum :: CompileEnv -> [TypeRowRead] -> EnumRead -> EnumCheck
checkEnum env rows e = case companion of
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
            ctors     = [ (label, v) | (v, label) <- kept ]
            names     = map fst (Gen.constructorNames (trName tr) (lo, hi) ctors)
            clashes   = [ n | (n, k) <- Map.toList (Map.fromListWith (+) [ (n, 1 :: Int) | n <- names ]), k > 1 ]
            merged    = [ ls | ls <- map L.nub (Map.elems (Map.fromListWith (flip (<>)) [ (scrubbed l, [l]) | (l, _) <- given ])), length ls > 1 ]
            findings  = map (\(v, l) -> bad ("value " <> tshow v <> " (" <> tshow l <> ") lies outside " <> tshow lo <> ".." <> tshow hi
                                              <> " of " <> renderAnchorId (trAnchor tr) <> " " <> trName tr)) outside
                     <> map (\(v, ls) -> warn "W-ENUM-OVERLAP" ("value " <> tshow v <> " is assigned by " <> T.intercalate " and " (map tshow ls) <> "; the first is kept")) overlaps
                     <> map (\(l, n) -> warn "W-ENUM-LABEL-REPEATED" ("the label " <> tshow l <> " is used by " <> tshow n <> " rows; its later values get suffixed constructors")) repeated
                     <> map (\n -> bad ("the constructor " <> n <> " would be defined twice")) clashes
                     <> map (\ls -> bad ("the labels " <> T.intercalate " and " (map tshow ls) <> " give the same constructor")) merged
        in EnumCheck findings
             (if any Cm.isError findings then Nothing
              else (\uidBytes -> (trName tr, ctors, uidBytes, (lo, hi))) <$> trUID tr)
  where
    sid      = ceSpec env
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


-- * The checks

-- | Every M1 check, in a fixed order.
checks :: Compile ()
checks = do
  env <- ask
  let ir       = ceIR env
      sid      = ceSpec env
      rowReads = typeRowReads env
      rowsOk   = rights rowReads
  tell (ceClassFindings env)
  tell (ceEnvFindings env)
  tell (checkExemplars ir)
  -- E-SHAPE on every bound table
  tell [ f | (aid, t) <- Map.toList (ceTables env), Just (SomeBinding b) <- [bindingOf env aid]
           , Just f <- [bindingAccepts b (measure sid t)] ]
  -- errata, then the normalisations every numbered table is read with
  tell (ceErrataFindings env)
  tell (concatMap (captionNote sid) (Map.toList (ceTables env)))
  tell (concatMap (headerNote sid) (Map.toList (ceTables env)))
  -- type rows
  tell (lefts rowReads)
  tell (concatMap (formatNote sid) rowsOk)
  tell (concatMap (uidTable sid) rowsOk)
  tell (uidUnique sid rowsOk)
  tell (lefts (map trTypeDef rowsOk))
  -- enumerations
  let enums = enumReads env
  tell (lefts enums)
  tell (concatMap (ecFindings . checkEnum env rowsOk) (rights enums))

-- | @N-CAPTION-NORMALISED@: a caption read other than as printed.
captionNote :: SpecId -> (AnchorId, TableIR) -> [Finding]
captionNote sid (aid, t) =
  [ locatedAt sid (renderAnchorId aid) (fromMaybe 1 (tablePage t))
      (finding Note "N-CAPTION-NORMALISED" ("caption " <> tshow raw <> " read as " <> tshow (normaliseCaption raw)))
  | Just raw <- [t.caption >>= rawCaptionTitle], normaliseCaption raw /= raw ]

-- | @N-HEADER-NORMALISED@: a header cell read through the typo table.
headerNote :: SpecId -> (AnchorId, TableIR) -> [Finding]
headerNote sid (aid, t) =
  [ locatedAt sid (renderAnchorId aid) (fromMaybe 1 (tablePage t))
      (finding Note "N-HEADER-NORMALISED" ("header " <> tshow bad <> " read as " <> tshow good))
  | (bad, good) <- headerNormalisations t ]

-- | @N-FORMAT-NORMALISED@: a Format cell whose spacing at a brace or table suffix the parser reads past.
formatNote :: SpecId -> TypeRowRead -> [Finding]
formatNote sid tr = case formatNormalisations (trFormat tr) of
  []    -> []
  pairs -> [ locatedAt sid (renderAnchorId (trAnchor tr)) (fromMaybe 1 (tablePage (trTable tr)))
               (finding Note "N-FORMAT-NORMALISED"
                 ("Format read with " <> T.intercalate ", " [ tshow a <> " as " <> tshow b | (a, b) <- pairs ])) ]

uidTable :: SpecId -> TypeRowRead -> [Finding]
uidTable sid tr = map (locatedAt sid (renderAnchorId (trAnchor tr)) (fromMaybe 1 (tablePage (trTable tr)))) $ case trUID tr of
  Nothing -> [finding Error "C-UID-TABLE" ("the UID cell " <> tshow (trUIDText tr) <> " is not eight bytes")]
  Just bs
    | take 4 bs == [0, 0, 0, 5] -> []
    | otherwise -> [finding Error "C-UID-TABLE" ("the UID " <> hexUID bs <> " of " <> trName tr <> " is not a Type object's (upper half 00 00 00 05)")]

-- | @C-UID-UNIQUE@, reported at every type row after the first with a UID. A
--   waiver on that row naming the other table and the UID
--   (@waive "C-UID-UNIQUE" "T94 00 00 00 05 00 00 02 15" "reason"@) makes it a
--   Warning carrying the waiver's reason when exactly those two type rows print
--   exactly that UID. A waiver that waives nothing (its subject is not a table
--   and a UID, the row's UID is another, no row before it has it, or the rows
--   that have it are not the two it names) is @W-WAIVER-UNUSED@, and the Error
--   stands.
uidUnique :: SpecId -> [TypeRowRead] -> [Finding]
uidUnique sid rows = concatMap clash (Map.elems groups) <> unusedWaivers
  where
    -- the rows of each UID, in anchor order (rows comes in anchor order)
    groups = Map.fromListWith (flip (<>)) [ (bs, [tr]) | tr <- rows, Just bs <- [trUID tr] ]
    here tr = locatedAt sid (renderAnchorId (trAnchor tr)) (fromMaybe 1 (tablePage (trTable tr)))
    -- why a waiver with this subject on this row waives nothing; Nothing when it waives its clash
    whyUnused tr subject = case waiverSubject subject of
      Nothing -> Just "that is not a table and a UID (\"T<n> 00 00 00 05 ...\")"
      Just (partner, uid)
        | Just uid /= trUID tr -> Just ("the table's UID cell is " <> tshow (trUIDText tr))
        | otherwise -> case map trAnchor (Map.findWithDefault [] uid groups) of
            first' : _ | first' == trAnchor tr -> Just "no type row before it has that UID"
            [first', self] | first' == partner && self == trAnchor tr -> Nothing
            held -> Just ("the type rows with that UID are " <> T.intercalate ", " (map renderAnchorId held))
    waiverOf tr = listToMaybe [ reason | ("C-UID-UNIQUE", subject, reason) <- trWaivers tr, isNothing (whyUnused tr subject) ]
    clash (first' : rest@(_ : _)) =
      [ here tr (finding (if isJust waived then Warning else Error) "C-UID-UNIQUE"
          ("the UID " <> maybe "" hexUID (trUID tr) <> " of " <> trName tr <> " is also that of "
            <> renderAnchorId (trAnchor first') <> " " <> trName first'
            <> maybe "" (\p -> " (PDF " <> tshow p <> ")") (tablePage (trTable first'))
            <> maybe "" ("; waived by its annotation: " <>) waived))
      | tr <- rest, let waived = waiverOf tr ]
    clash _ = []
    unusedWaivers =
      [ here tr (finding Warning "W-WAIVER-UNUSED"
          ("the annotation waives C-UID-UNIQUE for " <> tshow subject <> ", and " <> why))
      | tr <- rows, ("C-UID-UNIQUE", subject, _) <- trWaivers tr, Just why <- [whyUnused tr subject] ]

-- | The subject of a C-UID-UNIQUE waiver: the other table of the clash and
--   the UID, @T94 00 00 00 05 00 00 02 15@.
waiverSubject :: Text -> Maybe (AnchorId, [Word8])
waiverSubject subject = case T.words subject of
  table : uid | Just aid@(TableId _) <- parseAnchorId table, Just bs <- parseUIDCell (T.unwords uid) -> Just (aid, bs)
  _ -> Nothing

-- | Every check of one IR under one errata set, errors first.
checkIR :: ErrataSet -> SpecIR -> [Finding]
checkIR es ir = either pure (\(_, _, fs) -> sortFindings fs) (runCompile (mkCompileEnv es ir) checks)

-- | Errors, then Warnings, then Notes; within each by page, anchor in its
--   natural order (T8 before T10), code and message.
sortFindings :: [Finding] -> [Finding]
sortFindings = L.sortOn (\f -> (Cm.severity f, Cm.page f, naturalKey <$> Cm.anchor f, Cm.code f, Cm.message f))
