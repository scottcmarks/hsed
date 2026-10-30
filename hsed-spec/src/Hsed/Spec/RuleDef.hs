{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.RuleDef
Description : What one rule is: the record every rule module exports, and the context it reads
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The interface between the generic compiler and the rules (docs/adding-a-rule.md;
design section 4.2). Each rule is one module under @Hsed/Spec/Rules/@ exporting
one 'RuleDef', listed by "Hsed.Spec.Registry". The generic modules
("Hsed.Spec.Rules" classifies, "Hsed.Spec.Check" checks, "Hsed.Spec.Compile"
compiles, "Hsed.Spec.Gen" prints) iterate the registry; everything a rule
knows lives in its 'RuleDef':

* when it fires and what it binds a table to there (its name, default binding,
  exemplar, precedence and join);
* its checks: a function of the shared, read-only 'Context' and the tables
  bound to its shape ('BoundTable', rule-bound or comment-bound alike);
* its compilation of the same tables to declarations ('Hsed.Spec.Gen.Decl')
  for its generated module ('Hsed.Spec.Gen.GenModule'), or for another module
  it names ('EmitIn'), or the tables it leaves out and why.

Two kinds of unit are bound ('Hsed.Spec.Shape.shapeUnit' of the rule's
shape): the numbered tables, which a rule fires on by their form and reads
as 'BoundTable's, and the method signature blocks (IR @sigs@, design section
5.1 step 7), the signature units, which carry no annotation: every one in a
rule's scope is bound to the rule whose shape takes signature units
("Hsed.Spec.Rules".'Hsed.Spec.Rules.classifySignatures'), and that rule
reads them as 'BoundSig's ('boundSigs'; 'ruleChecks' and 'ruleCompile' get
it no tables, and its 'fires' is never true of a table). A signature unit's
anchor id is @\<section\>/sig@; locate its findings with 'atSig' and its
declarations with 'sigProvenanceOf'.

A rule that needs what another rule reads (an enumeration takes its range from
its type row) imports that rule's module and calls its readers on the same
'Context'. The 'Context' itself holds only what is generic: one IR under one
errata set, its numbered tables, their classification, the section of every
table ("Hsed.Spec.Scope") and the bound tables read under the errata set
("Hsed.Spec.Check".'Hsed.Spec.Check.mkContext' builds it).

Several bindings may share a shape (@propsNames@ and @propsInitial@ are both
@Props@): the shape has one 'RuleDef', which reads every table bound with it
and tells them apart by 'btBinding'.
-}
module Hsed.Spec.RuleDef
  ( -- * Rules
    RuleDef (..)
  , ruleShape
  , Emitted (..)
    -- * Classification
  , Classification (..)
  , boundBinding
    -- * The shared context
  , Context (..)
  , BoundTable (..)
  , boundTables
  , sectionOf
    -- * Signature units
  , BoundSig (..)
  , boundSigs
  , signatureUnits
  , atSig
  , sigProvenanceOf
  , Mods (..)
  , modsOf
    -- * Locating findings
  , locatedAt
  , tablePage
  , atTable
  , atCells
  , numberedTables
    -- * Cells and provenance
  , parseUIDCell
  , hexUID
  , provenanceOf
  , boundProvenance
  , atRow
  , cellsHash
  ) where

import           Crypto.Hash      (SHA256 (..), hashWith)
import qualified Data.Char        as DC
import qualified Data.Text        as DT
import qualified RIO.Char         as C
import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Anchor (AnchorId (..), parseAnchorId, renderAnchorId)
import           Hsed.Spec.Common (ErrataSet (..), Finding, RuleName, SpecId (..))
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.DSL    (Binding (..), Modifier, SomeBinding (..), bindingAccepts, someBindingShape)
import qualified Hsed.Spec.Gen    as Gen
import           Hsed.Spec.IR     (CellIR (..), SigIR (..), SpecIR (..), StructPath (..), TableIR (..))
import           Hsed.Spec.Shape  (Form, Shape (..), measure, normaliseCaption)


-- * Rules

-- | Everything one rule is (design section 4.2's @Rule@ and @Compile'@ in one record).
data RuleDef = RuleDef
  { ruleName    :: !RuleName
    -- ^ the name a @rule@ token gives it (@rule typeRow@); a reader reached only
    --   by a comment has a name that is no rule token, and never fires
  , bindsTo     :: !SomeBinding
    -- ^ what it binds a table to where it fires; its shape is the shape of the
    --   tables the rule reads ('ruleShape'), comment-bound ones included
  , exemplar    :: !(SpecId, AnchorId)
    -- ^ the table it must keep firing on (@C-EXEMPLAR@, re-verified on every run)
  , refinesRule :: !(Maybe RuleName)
    -- ^ the rule it wins over where both fire
  , joinsTo     :: !(Maybe RuleName)
    -- ^ the rule whose table naming the same identifier ('ruleIdent') must exist
  , fires       :: SpecId -> TableIR -> Form -> Bool
    -- ^ whether it fires on a table, by the table's measured form
  , ruleIdent   :: Form -> Maybe Text
    -- ^ the identifier a table it fires on names (a type's name), for 'joinsTo'
  , ruleModule  :: !Gen.GenModule
    -- ^ the generated module its declarations go to
  , ruleChecks  :: Context -> [BoundTable] -> [Finding]
    -- ^ its checks of the tables bound to its shape
  , ruleCompile :: Context -> [BoundTable] -> [Emitted]
    -- ^ its declarations for 'ruleModule', and the tables it leaves out
  }

-- | The shape of the tables a rule reads: that of its default binding.
ruleShape :: RuleDef -> Maybe Shape
ruleShape = someBindingShape . bindsTo

-- | What a rule's compilation gives.
data Emitted
  = Emit !Gen.Decl                   -- ^ a declaration for the rule's module
  | EmitIn !Gen.GenModule !Gen.Decl  -- ^ a declaration for another module (one rule writing several)
  | Excluded !AnchorId !Text         -- ^ a bound table left out, and why (its Error is among the rule's findings)
  deriving (Eq, Show)


-- * Classification

-- | Where a numbered table stands ("Hsed.Spec.Rules".'Hsed.Spec.Rules.classify').
data Classification
  = CommentBound !SomeBinding                     -- ^ its annotation is a full binding
  | RuleBound !RuleName ![Modifier] !SomeBinding  -- ^ bound by a rule; the modifiers of a bare chain, and the result
  | Ambiguous ![RuleName]                         -- ^ two or more rules fire, neither refining the other
  | Unbound                                       -- ^ no annotation, no rule
  | Invalid !Finding                              -- ^ the annotation does not parse or does not apply
  deriving (Eq, Show)

-- | The binding a table ends up with, if any.
boundBinding :: Classification -> Maybe SomeBinding
boundBinding = \case
  CommentBound sb    -> Just sb
  RuleBound _ _ sb   -> Just sb
  _                  -> Nothing


-- * The shared context

-- | What every rule's checks and compilation read: one IR under one errata set.
data Context = Context
  { cxIR             :: !SpecIR
  , cxSpec           :: !SpecId
  , cxErrata         :: !ErrataSet
  , cxTables         :: !(Map AnchorId TableIR)          -- ^ the numbered tables, as printed
  , cxClasses        :: !(Map AnchorId Classification)
  , cxClassFindings  :: ![Finding]                        -- ^ what the classification found
  , cxSections       :: !(Map StructPath (Maybe Text))    -- ^ every table's section ("Hsed.Spec.Scope".'Hsed.Spec.Scope.tableSections')
  , cxCorrected      :: !(Map AnchorId TableIR)           -- ^ the bound tables, read under the errata set
  , cxErrataFindings :: ![Finding]                        -- ^ what reading them under the set found
  , cxSigs           :: !(Map AnchorId SigIR)             -- ^ the signature units, by id ('signatureUnits')
  , cxSigClasses     :: !(Map AnchorId Classification)    -- ^ their classification
  }

-- | One table bound to a rule's shape.
data BoundTable = BoundTable
  { btAnchor  :: !AnchorId
  , btTable   :: !TableIR       -- ^ as printed (its cells are what the manifest hashes)
  , btRead    :: !TableIR       -- ^ read under the errata set: caption, header and cells corrected
  , btBinding :: !SomeBinding
  , btMods    :: !Mods          -- ^ the binding's modifiers
  }

-- | The tables bound with a shape, by a rule or by a comment, in anchor order;
--   not a table the shape refuses (@E-SHAPE@, an Error: a comment binding it
--   to a shape it is not), whose one finding is then that Error. A table bound
--   under @as@ is read: its @E-SHAPE@ is a Warning.
boundTables :: Maybe Shape -> Context -> [BoundTable]
boundTables shape ctx =
  [ BoundTable aid t (Map.findWithDefault t aid (cxCorrected ctx)) sb (modsOf sb)
  | isJust shape
  , (aid, t) <- Map.toList (cxTables ctx), Just sb <- [Map.lookup aid (cxClasses ctx) >>= boundBinding]
  , someBindingShape sb == shape, not (refused sb t) ]
  where
    refused (SomeBinding b) t = maybe False Cm.isError (bindingAccepts b (measure (cxSpec ctx) t))
    refused (PolyBinding _) _ = False

-- | The section of a bound table: that of the nearest numbered heading before it.
sectionOf :: Context -> BoundTable -> Maybe Text
sectionOf ctx bt = join (Map.lookup (btTable bt).structPath (cxSections ctx))


-- * Signature units

-- | One signature unit (a method signature block) bound to a rule's shape. It
--   carries no annotation, so no modifiers and no errata: it is read as printed.
data BoundSig = BoundSig
  { bsAnchor  :: !AnchorId      -- ^ @5.3.3.6/sig@
  , bsSig     :: !SigIR
  , bsBinding :: !SomeBinding   -- ^ the binding rule's default
  }

-- | The signature units bound with a shape, in anchor order.
boundSigs :: Maybe Shape -> Context -> [BoundSig]
boundSigs shape ctx =
  [ BoundSig aid s sb
  | isJust shape
  , (aid, s) <- Map.toList (cxSigs ctx), Just sb <- [Map.lookup aid (cxSigClasses ctx) >>= boundBinding]
  , someBindingShape sb == shape ]

-- | Every signature unit of an IR by its anchor id, @\<section\>/sig@: the IR
--   has one block per section (docs/ir-v1.md); a second would be the first's.
signatureUnits :: SpecIR -> Map AnchorId SigIR
signatureUnits ir = Map.fromListWith (\_ firstOne -> firstOne) [ (SigId s.section, s) | s <- ir.sigs ]

-- | Locate a finding at a signature unit: @core-2.01.pdf#page=201&5.3.3.6/sig@.
atSig :: Context -> BoundSig -> Finding -> Finding
atSig ctx bs = locatedAt (cxSpec ctx) (renderAnchorId (bsAnchor bs)) ((bsSig bs).page + 1)

-- | Where a declaration made from a signature unit comes from: its anchor id,
--   its heads as the caption, its PDF and printed page, its 1-based position
--   among the IR's signature blocks as the ordinal, its first paragraph's
--   structure path and the sha256 of its text.
sigProvenanceOf :: Context -> BoundSig -> Gen.Provenance
sigProvenanceOf ctx bs = Gen.Provenance
  { Gen.spec        = cxSpec ctx
  , Gen.tableId     = renderAnchorId (bsAnchor bs)
  , Gen.caption     = T.intercalate ", " s.heads
  , Gen.pdfPage1    = s.page + 1
  , Gen.printedPage = s.page + 1 - (cxIR ctx).labelOffset
  , Gen.ordinal     = maybe 0 (+ 1) (L.elemIndex s (cxIR ctx).sigs)
  , Gen.structPath  = let StructPath sp = s.structPath in sp
  , Gen.cellsHash   = tshow (hashWith SHA256 (encodeUtf8 s.text)) }
  where s = bsSig bs


-- | The modifiers of a binding a rule reads, one field for each modifier of
--   "Hsed.Spec.DSL": Scott's corrections (the grammar admits no other set), the
--   bound columns, the range words, the waivers, and whether the binding is
--   under @enumInDescription@, @expand@ or @as@. A new modifier adds its field
--   here, in the serial commit that adds it to the vocabulary.
data Mods = Mods
  { mErrata            :: [(Text, Text)]
  , mCols              :: Maybe (Int, Int)
  , mRangeWords        :: [Text]
  , mWaivers           :: [(Text, Text, Text)]
  , mEnumInDescription :: Bool
  , mExpand            :: Bool
  , mAs                :: Bool
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
      WithEnumInDescription b   -> (go b) { mEnumInDescription = True }
      WithExpand b              -> (go b) { mExpand = True }
      WithAs b                  -> (go b) { mAs = True }
      _                         -> Mods [] Nothing [] [] False False False


-- * Locating findings

-- | Give a finding a location unless it has one.
locatedAt :: SpecId -> Text -> Int -> Finding -> Finding
locatedAt sid aid page f = f { Cm.spec = Cm.spec f <|> Just sid, Cm.anchor = Cm.anchor f <|> Just aid, Cm.page = Cm.page f <|> Just page }

-- | The 1-based PDF page of a table's first fragment.
tablePage :: TableIR -> Maybe Int
tablePage t = (+ 1) <$> listToMaybe t.pages

-- | Locate a finding at a bound table: @core-2.01.pdf#page=100&T46@.
atTable :: Context -> BoundTable -> Finding -> Finding
atTable ctx bt = locatedAt (cxSpec ctx) (renderAnchorId (btAnchor bt)) (fromMaybe 1 (tablePage (btTable bt)))

-- | Locate a finding about one row at the page the row is printed on (that of
--   its first cell that has one; a table spanning pages prints a row on the
--   page it lies on), else at its table.
atCells :: Context -> BoundTable -> [CellIR] -> Finding -> Finding
atCells ctx bt cells = case listToMaybe (mapMaybe (\c -> c.page) cells) of
  Just p0 -> locatedAt (cxSpec ctx) (renderAnchorId (btAnchor bt)) (p0 + 1)
  Nothing -> atTable ctx bt

-- | Every numbered table by its anchor id; a table Word split in two (Core T7)
--   is its first part.
numberedTables :: SpecIR -> Map AnchorId TableIR
numberedTables ir = Map.fromListWith earlier
  [ (aid, t) | t <- ir.tables, Just i <- [t.id], Just aid@(TableId _) <- [parseAnchorId i] ]
  where
    earlier a b = if order a <= order b then a else b
    order :: TableIR -> (Maybe Int, Maybe Int)
    order t = (t.part, t.ordinal)


-- * Cells and provenance

-- | A UID cell: sixteen hex digits once whitespace is removed.
parseUIDCell :: Text -> Maybe [Word8]
parseUIDCell cell
  | T.length hex == 16, T.all C.isHexDigit hex = Just (map byte (DT.chunksOf 2 hex))
  | otherwise = Nothing
  where
    hex = T.filter (not . C.isSpace) cell
    byte = T.foldl' (\acc c -> acc * 16 + fromIntegral (DC.digitToInt c)) 0

-- | @00 00 00 05 00 00 08 01@
hexUID :: [Word8] -> Text
hexUID = T.unwords . map hex2
  where hex2 w = T.pack [digit (w `div` 16), digit (w `mod` 16)]
        digit d = C.toUpper (DC.intToDigit (fromIntegral d))

-- | Where a declaration comes from (design example E2): the 1-based PDF page,
--   the printed page (page - labelOffset), the structure ordinal and path, and
--   the sha256 of the cells as the manifest records it.
provenanceOf :: Context -> AnchorId -> TableIR -> Text -> Gen.Provenance
provenanceOf ctx aid t caption = Gen.Provenance
  { Gen.spec        = cxSpec ctx
  , Gen.tableId     = renderAnchorId aid
  , Gen.caption     = caption
  , Gen.pdfPage1    = page
  , Gen.printedPage = page - (cxIR ctx).labelOffset
  , Gen.ordinal     = fromMaybe 0 t.ordinal
  , Gen.structPath  = let StructPath sp = t.structPath in sp
  , Gen.cellsHash   = cellsHash t }
  where page = fromMaybe 1 (tablePage t)

-- | The provenance of a bound table's declarations, under its caption as read
--   ('btRead': corrected under the errata set) and normalised.
boundProvenance :: Context -> BoundTable -> Gen.Provenance
boundProvenance ctx bt = provenanceOf ctx (btAnchor bt) (btTable bt) (normaliseCaption (fromMaybe "" (btRead bt).caption))

-- | A provenance moved to the page of one row: the PDF and printed page of the
--   row's first cell that has one (a table spanning pages prints a row on the
--   page it lies on); the table's ordinal and hash are kept.
atRow :: Context -> [CellIR] -> Gen.Provenance -> Gen.Provenance
atRow ctx cells p = case listToMaybe (mapMaybe (\c -> c.page) cells) of
  Nothing -> p
  Just page0 -> p { Gen.pdfPage1 = page0 + 1, Gen.printedPage = page0 + 1 - (cxIR ctx).labelOffset }

-- | sha256 over a table's cell texts, header rows first, a tab between cells and
--   a newline between rows: the manifest's @hash@ (@tools/extract/annotate.py@).
cellsHash :: TableIR -> Text
cellsHash t = tshow (hashWith SHA256 (encodeUtf8 joined))
  where
    joined = T.intercalate "\n" [ T.intercalate "\t" (map (\c -> c.text) row) | row <- t.headerRows <> t.rows ]
