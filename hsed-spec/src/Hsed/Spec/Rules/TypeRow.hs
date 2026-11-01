{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.Rules.TypeRow
Description : The typeRow rule: one Type object per table, its UID, Format and TypeDef
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E2. @typeRow@ fires on a table headed @[UID, Name,
Format]@ whose caption is a single identifier (@Table 46 AC_element@; the seven
@\<X\>_object _ref@ captions are normalised by "Hsed.Spec.Shape"). Each such
table holds one row of the Type table: its UID, its name and its Format, which
"Hsed.Spec.Format" parses into a 'TypeDef' against the Type objects of every
type row and the table names a @uidref{...}@ may name, those of the table the
@tableUIDs@ rule binds (Table 240), the spec's and then its base's (since M4b:
Opal's Table 30 is read over Core's Table 240, 'formatEnv').

Checks, each with the code its findings carry:

* @E-SHAPE@: the table is one row of three cells;
* @C-UID-TABLE@: the UID is eight bytes, and every type row's UID lies in one
  table, a table of Table 240: the type rows' table is the one whose half-UID
  (@00 00 00 05@ in Core) most of their upper halves carry, the spec's and
  its base's together (since the M4b review: an SSC's few rows are held to
  Core's half, Core 3.2.5.3 b.iii, PDF 52), and a row whose upper half is
  another is an Error, as is a half that names no table of the spec's or its
  base's tables of table UIDs;
* @C-UID-UNIQUE@: no two type rows share a UID, nor a spec's type row a UID
  its base's give; a type row's @waive "C-UID-UNIQUE" "T\<n\> \<uid\>"
  "\<reason\>"@ makes the Error a Warning for that table when it and table
  @n@, and no other, print that UID, and a waiver that waives nothing is
  @W-WAIVER-UNUSED@;
* @C-TYPE-SHADOW@: a spec's type row names no type its base's rows name,
  unless its binding @replaces@ that table (the spec's own would shadow the
  base's wherever a type is read by name);
* @C-FMT-RESOLVE@: every Format cell parses, its findings carrying
  "Hsed.Spec.Format"'s codes (@E-FMT-RESOLVE@ and friends), its type names
  resolving over the spec's Type objects and its base's ('formatEnv'), and a
  Format cell's spacing at a brace or its table's @Table_UID@ or @Table@
  suffix is the Note @N-FORMAT-NORMALISED@;
* @C-FMT-CYCLE@: no Format names its own type, directly or through other
  types ('typeCycle'), a type no value could end, which a reader following
  it would follow for ever.

Compiled into 'typesModule': @u\<Name\>Type :: UID@ and, when the Format
parsed, @t\<Name\> :: TypeDef@. (The Format strings @f\<Name\>Type :: String@,
printed for M2a's runtime comparison with the hand transcription, were
retired at M2d.) Every other rule that prints a type as the spec names it, a
column's, a field's, a property's or a parameter's, prints it through
'typeNameExpr': the runtime's @TypeName@ ("Hsed.Core.TypeName"), the text
and, when a type row defines it, @u\<Name\>Type@.
-}
module Hsed.Spec.Rules.TypeRow
  ( typeRowRule
  , typesModule
    -- * Reading the bound tables
  , TypeRowRead (..)
  , typeRowReads
  , allTypeRows
  , formatEnv
  , typeEnvFrom
  , typeIdent
  , typeCycle
    -- * Declarations
  , TypeRowDecl (..)
  , typeRowDecl
  , renderTypeRow
  , joinFormat
  , typeUIDName
  , typeNameExpr
  ) where

import qualified Data.Text        as DT
import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Set          as Set
import qualified RIO.Text         as T
import           RIO

import           Hsed.Core.Types  (TypeDef (..), TypeRef (..))
import           Hsed.Spec.Anchor (AnchorId (..), parseAnchorId, renderAnchorId)
import           Hsed.Spec.Common (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.DSL    (SomeBinding (..), typeRow)
import           Hsed.Spec.Format (Env, formatNormalisations, mkEnv, parseFormat)
import qualified Hsed.Spec.Gen    as Gen
import           Hsed.Spec.IR     (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..), allTableUIDRows, tableUIDsNamed)
import           Hsed.Spec.Shape  (CaptionKind (..), Form (..), Shape (..), measure, normaliseCaption, shapeMismatch)


-- $setup
-- >>> :set -XOverloadedStrings


-- | The rule.
typeRowRule :: RuleDef
typeRowRule = RuleDef
  { ruleName    = RuleName "typeRow"
  , bindsTo     = SomeBinding typeRow
  , exemplar    = (SpecId "core-2.01", TableId 46)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch TypeRow form) && fCols form == 3 && isJust (typeIdent form)
  , ruleIdent   = typeIdent
  , ruleModule  = typesModule
  , ruleCodes   =
      [ CodeDef "C-UID-TABLE"         [Error]          (Defect "a UID is not one of its table's")
      , CodeDef "C-UID-UNIQUE"        [Error, Warning] (Defect "a UID printed twice, in one table or in two")
      , CodeDef "C-FMT-RESOLVE"       [Error, Warning] (Defect "a Format or a column's type names a type no table defines")
      , CodeDef "N-FORMAT-NORMALISED" [Note]           (AsPrinted "a Format cell read with its spacing or spelling normalised")
      , CodeDef "W-WAIVER-UNUSED"     [Warning]        Bookkeeping
      , CodeDef "C-FMT-CYCLE"         [Error]          (Defect "a Format names its own type, directly or through other types")
      , CodeDef "C-TYPE-SHADOW"       [Error]          (Defect "a type row of a spec read over another names a type of the other, which it does not replace") ]
  , ruleChecks  = checks
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.Types@.
typesModule :: Gen.GenModule
typesModule = Gen.GenModule
  { Gen.leaf        = "Types"
  , Gen.description = "Type objects: each Type table row's UID and the TypeDef its Format gives"
  , Gen.notes       = [] }

-- | The type a type row's caption names: a single identifier.
typeIdent :: Form -> Maybe Text
typeIdent form = case fCaptionKind form of
  CaptionIdent s -> Just s
  _              -> Nothing


-- * Reading the bound tables

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

-- | Every table bound as a type row, read under the errata set; a table that is
--   not one row of three cells is a finding.
typeRowReads :: Context -> [Either Finding TypeRowRead]
typeRowReads ctx = readRows ctx (boundTables (Just TypeRow) ctx)

-- | Every type row a spec reads: its own, then its base's whose name none of
--   its own gives (Opal's Table 30 @data_removal_mechanism@ beside Core's
--   hundred), each with the Context that owns it ('ownThenBase').
allTypeRows :: Context -> [(Context, TypeRowRead)]
allTypeRows = ownThenBase (rights . typeRowReads) trName

readRows :: Context -> [BoundTable] -> [Either Finding TypeRowRead]
readRows ctx = map readRow
  where
    env = fst (formatEnv ctx)
    readRow bt = case (btRead bt).rows of
      [[uidCell, nameCell, formatCell]] -> Right TypeRowRead
        { trAnchor = btAnchor bt, trTable = btTable bt, trCaption = normaliseCaption (fromMaybe "" (btRead bt).caption)
        , trIdent = typeIdent (measure (cxSpec ctx) (btRead bt))
        , trName = T.strip nameCell.text, trUIDText = uidCell.text, trUID = parseUIDCell uidCell.text
        , trFormat = formatCell.text, trTypeDef = first (atTable ctx bt) (parseFormat env formatCell.text)
        , trWaivers = mWaivers (btMods bt) }
      rows -> Left (atTable ctx bt (finding Error "E-SHAPE"
                ("a type row table holds one row of three cells; this one has " <> tshow (map length rows))))

-- | The environment the Format cells resolve against: the Type objects of the
--   type rows, the spec's and then its base's it does not name (since the M4b
--   review), and the table names a @uidref{...}@ may name, those of the tables
--   bound by @tableUIDs@ ("Hsed.Spec.Rules.TableUIDs"), the spec's and then its
--   base's ('allTableUIDRows': Opal, which binds Table 30 and has no table of
--   table UIDs, resolves against Core's Table 240 and its own Table 20 row 12;
--   Core's names are its Table 240's, as before); with what building it found,
--   the spec's own: @C-FMT-RESOLVE@ when there are type rows and no such table
--   in the spec or its base.
formatEnv :: Context -> (Env, [Finding])
formatEnv ctx = (mkEnv types names, nameFindings)
  where
    -- the spec's Type objects, then its base's it does not name (since the M4b review: an SSC's Format may name
    -- Core's types, as Key Per IO's Alternative_Type, bytes_16, bytes_32 does)
    types = map snd (ownThenBase typeEnvFrom fst ctx)
    names = [ tuName r | (_, r) <- allTableUIDRows ctx, not (T.null (tuName r)) ]
    nameFindings =
      [ (finding Error "C-FMT-RESOLVE" ("no table bound by " <> tableUIDsNamed <> " to resolve uidref{...} against"))
          { Cm.spec = Just (cxSpec ctx) }
      | null names, not (null (typeEnvFrom ctx)) ]

-- | The Type objects: the Name cell of every table bound as a type row (read
--   under the errata set), with its table number.
typeEnvFrom :: Context -> [(Text, Maybe Int)]
typeEnvFrom ctx =
  [ (T.strip nameCell.text, t.tableNumber)
  | bt <- boundTables (Just TypeRow) ctx, let t = btRead bt, [_, nameCell, _] : _ <- [t.rows] ]


-- * Checks

checks :: Context -> [BoundTable] -> [Finding]
checks ctx bts = snd (formatEnv ctx) <> lefts reads <> concatMap formatNote rows <> concatMap uidTable rows <> typesTable
                 <> uidUnique rows <> lefts (map trTypeDef rows) <> concatMap baseClash rows <> concatMap cycleOf rows
  where
    reads = readRows ctx bts
    rows  = rights reads
    here tr = locatedAt (cxSpec ctx) (renderAnchorId (trAnchor tr)) (fromMaybe 1 (tablePage (trTable tr)))
    -- the base's type rows, each with the Context that owns it (none for a spec read alone)
    based = maybe [] allTypeRows (cxBase ctx)
    inBase c tr = specIdText (cxSpec c) <> " " <> renderAnchorId (trAnchor tr) <> " " <> trName tr
                  <> maybe "" (\p -> " (PDF " <> tshow p <> ")") (tablePage (trTable tr))

    -- a spec's type row held to its base's (the M4b review): no UID the base's type rows give (C-UID-UNIQUE),
    -- and no name of the base's unless the row's binding replaces that table (C-TYPE-SHADOW: the spec's own
    -- would silently shadow it in every description, cell and signature read over both)
    baseClash tr =
      [ here tr (finding Error "C-UID-UNIQUE" ("the UID " <> hexUID u <> " of " <> trName tr <> " is also that of " <> inBase c btr))
      | Just u <- [trUID tr], (c, btr) <- take 1 [ cb | cb@(_, btr) <- based, trUID btr == Just u ] ]
      <> [ here tr (finding Error "C-TYPE-SHADOW" ("the type " <> trName tr <> " is also " <> inBase c btr
                      <> ", which its binding does not replace: the spec's own would shadow it wherever a type is read by name"))
         | (c, btr) <- take 1 [ cb | cb@(_, btr) <- based, trName btr == trName tr ], not (replaced c btr) ]
      where
        replaced c btr = or [ cxSpec b == cxSpec c && aid == trAnchor btr
                            | bt <- take 1 [ bt | bt <- bts, btAnchor bt == trAnchor tr ]
                            , Right (b, aid) <- map (replacedIn ctx) (replacesOf (btBinding bt)) ]

    -- C-FMT-CYCLE: a Format that names its own type, directly or through other types, a type no value can end
    defs = Map.fromList [ (trName tr, td) | (_, tr) <- allTypeRows ctx, Right td <- [trTypeDef tr] ]
    cycleOf tr = [ here tr (finding Error "C-FMT-CYCLE" ("the Format of " <> trName tr <> " names its own type, through "
                                                          <> T.intercalate " -> " path))
                 | Just path <- [typeCycle (`Map.lookup` defs) (trName tr)] ]

    -- N-FORMAT-NORMALISED: a Format cell whose spacing at a brace or table suffix the parser reads past
    formatNote tr = case formatNormalisations (trFormat tr) of
      []    -> []
      pairs -> [ here tr (finding Note "N-FORMAT-NORMALISED"
                   ("Format read with " <> T.intercalate ", " [ tshow a <> " as " <> tshow b | (a, b) <- pairs ])) ]

    -- the Type table's half-UID, over the spec's type rows and its base's (since the M4b review: an SSC's own few
    -- held to Core's 00 00 00 05, Core 3.2.5.3 b.iii, PDF 52)
    typesHalf = typeTableHalf (map snd (allTypeRows ctx))
    uidTable tr = map (here tr) $ case trUID tr of
      Nothing -> [finding Error "C-UID-TABLE" ("the UID cell " <> tshow (trUIDText tr) <> " is not eight bytes")]
      Just bs
        | Just (take 4 bs) == typesHalf -> []
        | otherwise -> [finding Error "C-UID-TABLE" ("the UID " <> hexUID bs <> " of " <> trName tr <> " is not a Type object's (upper half "
                                                       <> maybe "unknown" hexUID typesHalf <> ")")]
    -- the type rows' table must be a table of Table 240, when tableUIDs binds one in the spec or its base
    halves = [ take 4 t | (_, r) <- allTableUIDRows ctx, Just t <- [tuTable r] ]
    typesTable = case (typesHalf, filter ((== typesHalf) . fmap (take 4) . trUID) rows) of
      (Just h, tr : _) | not (null halves), h `notElem` halves ->
        [ here tr (finding Error "C-UID-TABLE" ("the type rows' upper half " <> hexUID h <> " is the half-UID of no table of those tableUIDs binds")) ]
      _ -> []

    -- C-UID-UNIQUE, reported at every type row after the first with a UID. A
    -- waiver on that row naming the other table and the UID makes it a Warning
    -- carrying the waiver's reason when exactly those two type rows print exactly
    -- that UID. A waiver that waives nothing (its subject is not a table and a
    -- UID, the row's UID is another, no row before it has it, or the rows that
    -- have it are not the two it names) is W-WAIVER-UNUSED, and the Error stands.
    uidUnique trs = concatMap clash (Map.elems groups) <> unusedWaivers
      where
        groups = Map.fromListWith (flip (<>)) [ (bs, [tr]) | tr <- trs, Just bs <- [trUID tr] ]
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
          | tr <- trs, ("C-UID-UNIQUE", subject, _) <- trWaivers tr, Just why <- [whyUnused tr subject] ]

-- | The half-UID of the table the type rows lie in: the upper half most of their
--   UIDs carry, the first in anchor order on a tie; Nothing without a UID.
typeTableHalf :: [TypeRowRead] -> Maybe [Word8]
typeTableHalf rows = listToMaybe (L.sortOn (Down . count) (L.nub halves))
  where
    halves  = [ take 4 bs | tr <- rows, Just bs <- [trUID tr] ]
    count h = length (filter (== h) halves)

-- | The types a TypeDef names: a Simple_Type's base, an alternative's, a
--   list's element, a named value's type, a struct's components.
typeRefs :: TypeDef -> [Text]
typeRefs = \case
  SimpleType (Ref b) _          -> [b]
  AltType rs                    -> [ n | Ref n <- rs ]
  ListType _ (Ref e)            -> [e]
  NamedValueName _ (Ref t)      -> [t]
  NamedValueInteger _ (Ref t)   -> [t]
  NamedValueUinteger _ (Ref t)  -> [t]
  StructType rs                 -> [ n | Ref n <- rs ]
  _                             -> []

-- | The shortest way a type's Format names the type again, from it back to
--   it (@[boolean, boolean]@ for @boolean@ printed @List_Type, 4, boolean@),
--   through the Type objects a name finds; Nothing where it does not.
--
-- >>> typeCycle (\n -> lookup n [("a", AltType [Ref "b"]), ("b", AltType [Ref "a", Ref "c"]), ("c", BaseType)]) "a"
-- Just ["a","b","a"]
-- >>> typeCycle (\n -> lookup n [("c", BaseType)]) "c"
-- Nothing
typeCycle :: (Text -> Maybe TypeDef) -> Text -> Maybe [Text]
typeCycle defOf start = go (Set.singleton start) [[start]]
  where
    go _ [] = Nothing
    go seen (path : rest) = case path of
      [] -> go seen rest
      n : _ | start `elem` next n -> Just (reverse (start : path))
            | otherwise           -> let fresh = L.nub [ m | m <- next n, m `Set.notMember` seen ]
                                     in go (seen <> Set.fromList fresh) (rest <> [ m : path | m <- fresh ])
    next n = maybe [] typeRefs (defOf n)

-- | The subject of a C-UID-UNIQUE waiver: the other table of the clash and
--   the UID, @T94 00 00 00 05 00 00 02 15@.
waiverSubject :: Text -> Maybe (AnchorId, [Word8])
waiverSubject subject = case T.words subject of
  table : uid | Just aid@(TableId _) <- parseAnchorId table, Just bs <- parseUIDCell (T.unwords uid) -> Just (aid, bs)
  _ -> Nothing


-- * Declarations

-- | One Type table row, as the generated module declares it.
data TypeRowDecl = TypeRowDecl
  { name       :: !Text
  , uidBytes   :: ![Word8]
  , formatText :: !Text             -- ^ the Format cell, after errata
  , typeDef    :: !(Maybe TypeDef)  -- ^ absent when the Format did not parse
  , provenance :: !Gen.Provenance
  }
  deriving (Eq, Show)

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = [ either (Excluded (trAnchor tr)) (Emit . renderTypeRow) (typeRowDecl ctx tr) | tr <- rights (readRows ctx bts) ]

-- | A type row's declaration, or why it has none.
typeRowDecl :: Context -> TypeRowRead -> Either Text TypeRowDecl
typeRowDecl ctx tr = case trUID tr of
  Nothing -> Left ("the UID cell " <> tshow (trUIDText tr) <> " is not eight bytes")
  Just bs -> Right TypeRowDecl
    { name       = trName tr
    , uidBytes   = bs
    , formatText = trFormat tr
    , typeDef    = either (const Nothing) Just (trTypeDef tr)
    , provenance = provenanceOf ctx (trAnchor tr) (trTable tr) (trCaption tr) }

-- | The name of a type row's generated UID, @u\<Name\>Type@ (@uAC_elementType@):
--   what every other rule that names a Type object's UID refers to.
typeUIDName :: Text -> Text
typeUIDName n = "u" <> n <> "Type"

-- | A type as the spec names it, as the runtime's @TypeName@ expression, in
--   parentheses: its text as printed and, when a type row defines it, its
--   @u\<Name\>Type@ (@(TypeName "uinteger" (Just uuintegerType))@;
--   @(TypeName "cell_block" Nothing)@). The declaration printing it imports
--   "Hsed.Spec.Gen".'Gen.typeNameModule' and, for the UID, 'typesModule'.
typeNameExpr :: Text -> Maybe TypeRowRead -> Text
typeNameExpr text tr = "(TypeName " <> tshow text <> " " <> maybe "Nothing" (\r -> "(Just " <> typeUIDName (trName r) <> ")") tr <> ")"

-- | @u\<Name\>Type@ and, when the Format parsed, @t\<Name\>@.
renderTypeRow :: TypeRowDecl -> Gen.Decl
renderTypeRow r = (Gen.decl r.provenance r.name body)
  { Gen.declImports = [ Gen.ImportAll Gen.coreTypesModule | isJust r.typeDef ]
                      <> [ Gen.ImportSome Gen.uidModule ["UID", "uid"] ] }
  where
    u   = typeUIDName r.name
    doc = Gen.provenanceComment r.provenance
    body =
      [ doc, u <> " :: UID", u <> " = " <> Gen.uidExpr r.uidBytes
      , "" ]
      <> maybe [ "-- t" <> r.name <> ": no TypeDef, the Format did not parse under this errata set" ]
               (\t -> [ doc, "t" <> r.name <> " :: TypeDef", "t" <> r.name <> " = " <> tshow t ])
               r.typeDef

-- | The Format cell's pieces trimmed and joined by commas, no space after a
--   comma, as the fixture's quasiquoter joined a table's lines: the Format's
--   text as the hand transcription states it (the generated module no longer
--   prints it, since M2d).
joinFormat :: Text -> Text
joinFormat = T.intercalate "," . map T.strip . DT.splitOn ","
