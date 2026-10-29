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
@tableUIDs@ rule binds (Table 240).

Checks, each with the code its findings carry:

* @E-SHAPE@: the table is one row of three cells;
* @C-UID-TABLE@: the UID is eight bytes, and every type row's UID lies in one
  table, a table of Table 240: the type rows' table is the one whose half-UID
  (@00 00 00 05@ in Core) most of their upper halves carry, and a row whose
  upper half is another is an Error, as is a half that names no table of
  Table 240;
* @C-UID-UNIQUE@: no two type rows share a UID; a type row's @waive
  "C-UID-UNIQUE" "T\<n\> \<uid\>" "\<reason\>"@ makes the Error a Warning for
  that table when it and table @n@, and no other, print that UID, and a waiver
  that waives nothing is @W-WAIVER-UNUSED@;
* @C-FMT-RESOLVE@: every Format cell parses, its findings carrying
  "Hsed.Spec.Format"'s codes (@E-FMT-RESOLVE@ and friends), and a Format
  cell's spacing at a brace or its table's @Table_UID@ or @Table@ suffix is the
  Note @N-FORMAT-NORMALISED@.

Compiled into 'typesModule': @u\<Name\>Type :: UID@, @f\<Name\>Type :: String@
(the Format cell joined as hsed's hand transcription joins it, commas and no
spaces, for the M2a runtime comparison) and @t\<Name\> :: TypeDef@ when the
Format parsed.
-}
module Hsed.Spec.Rules.TypeRow
  ( typeRowRule
  , typesModule
    -- * Reading the bound tables
  , TypeRowRead (..)
  , typeRowReads
  , formatEnv
  , typeEnvFrom
  , typeIdent
    -- * Declarations
  , TypeRowDecl (..)
  , typeRowDecl
  , renderTypeRow
  , joinFormat
  ) where

import qualified Data.Text        as DT
import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO

import           Hsed.Core.Types  (TypeDef)
import           Hsed.Spec.Anchor (AnchorId (..), parseAnchorId, renderAnchorId)
import           Hsed.Spec.Common (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.DSL    (SomeBinding (..), typeRow)
import           Hsed.Spec.Format (Env, formatNormalisations, mkEnv, parseFormat)
import qualified Hsed.Spec.Gen    as Gen
import           Hsed.Spec.IR     (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..), tableNames, tableUIDRows)
import           Hsed.Spec.Shape  (CaptionKind (..), Form (..), Shape (..), measure, normaliseCaption, shapeMismatch)


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
  , ruleChecks  = checks
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.Types@.
typesModule :: Gen.GenModule
typesModule = Gen.GenModule
  { Gen.leaf        = "Types"
  , Gen.description = "Type objects: each Type table row's UID, Format and TypeDef"
  , Gen.notes       = [ "The @f\\<Name\\>Type@ strings are the Format cells joined as hsed's hand"
                      , "transcription joins them, for the M2a runtime comparison." ] }

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
--   type rows and the table names a @uidref{...}@ may name, those of the tables
--   bound by @tableUIDs@ ("Hsed.Spec.Rules.TableUIDs"); with what building it
--   found: @C-FMT-RESOLVE@ when there are type rows and no such table.
formatEnv :: Context -> (Env, [Finding])
formatEnv ctx = (mkEnv types names, nameFindings)
  where
    types = typeEnvFrom ctx
    names = tableNames ctx
    nameFindings =
      [ (finding Error "C-FMT-RESOLVE" "no table bound by tableUIDs (T240's header) to resolve uidref{...} against")
          { Cm.spec = Just (cxSpec ctx) }
      | null names, not (null types) ]

-- | The Type objects: the Name cell of every table bound as a type row (read
--   under the errata set), with its table number.
typeEnvFrom :: Context -> [(Text, Maybe Int)]
typeEnvFrom ctx =
  [ (T.strip nameCell.text, t.tableNumber)
  | bt <- boundTables (Just TypeRow) ctx, let t = btRead bt, [_, nameCell, _] : _ <- [t.rows] ]


-- * Checks

checks :: Context -> [BoundTable] -> [Finding]
checks ctx bts = snd (formatEnv ctx) <> lefts reads <> concatMap formatNote rows <> concatMap uidTable rows <> typesTable
                 <> uidUnique rows <> lefts (map trTypeDef rows)
  where
    reads = readRows ctx bts
    rows  = rights reads
    here tr = locatedAt (cxSpec ctx) (renderAnchorId (trAnchor tr)) (fromMaybe 1 (tablePage (trTable tr)))

    -- N-FORMAT-NORMALISED: a Format cell whose spacing at a brace or table suffix the parser reads past
    formatNote tr = case formatNormalisations (trFormat tr) of
      []    -> []
      pairs -> [ here tr (finding Note "N-FORMAT-NORMALISED"
                   ("Format read with " <> T.intercalate ", " [ tshow a <> " as " <> tshow b | (a, b) <- pairs ])) ]

    typesHalf = typeTableHalf rows
    uidTable tr = map (here tr) $ case trUID tr of
      Nothing -> [finding Error "C-UID-TABLE" ("the UID cell " <> tshow (trUIDText tr) <> " is not eight bytes")]
      Just bs
        | Just (take 4 bs) == typesHalf -> []
        | otherwise -> [finding Error "C-UID-TABLE" ("the UID " <> hexUID bs <> " of " <> trName tr <> " is not a Type object's (upper half "
                                                       <> maybe "unknown" hexUID typesHalf <> ")")]
    -- the type rows' table must be a table of Table 240, when tableUIDs binds one
    halves = [ take 4 t | Right r <- tableUIDRows ctx, Just t <- [tuTable r] ]
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

-- | @u\<Name\>Type@, @f\<Name\>Type@ and, when the Format parsed, @t\<Name\>@.
renderTypeRow :: TypeRowDecl -> Gen.Decl
renderTypeRow r = (Gen.decl r.provenance r.name body)
  { Gen.declImports = [ Gen.ImportAll Gen.coreTypesModule | isJust r.typeDef ]
                      <> [ Gen.ImportSome "Prelude" ["String"], Gen.ImportSome Gen.uidModule ["UID", "uid"] ] }
  where
    tag = r.name <> "Type"
    doc = Gen.provenanceComment r.provenance
    body =
      [ doc, "u" <> tag <> " :: UID", "u" <> tag <> " = " <> Gen.uidExpr r.uidBytes
      , "", doc, "f" <> tag <> " :: String", "f" <> tag <> " = " <> tshow (joinFormat r.formatText)
      , "" ]
      <> maybe [ "-- t" <> r.name <> ": no TypeDef, the Format did not parse under this errata set" ]
               (\t -> [ doc, "t" <> r.name <> " :: TypeDef", "t" <> r.name <> " = " <> tshow t ])
               r.typeDef

-- | The Format cell as hsed's quasiquoter joins the table's lines: the pieces
--   between commas trimmed, no space after a comma.
joinFormat :: Text -> Text
joinFormat = T.intercalate "," . map T.strip . DT.splitOn ","
