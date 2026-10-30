{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Check
Description : The checks of hsedc check: the generic ones, then every rule's
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 4.2. 'mkContext' builds the shared, read-only
'Hsed.Spec.RuleDef.Context' every rule reads: one IR under one errata set,
its numbered tables, their classification ("Hsed.Spec.Rules"), every table's
section ("Hsed.Spec.Scope"), the bound tables read under the errata set and
the paragraph and heading units its comments bind; 'mkContextWith' the same over a base,
the read-only Context of the spec this one is read over (Core's for an Opal
run, @hsedc --base@), whose checks are its own run's. 'checks' runs the generic checks, then the
checks of every rule of "Hsed.Spec.Registry" on the tables bound to its shape
(each rule's own module documents its checks and their codes). The generic
checks, each emitting findings with the code in brackets:

* classification: annotations refused (@E-ANCHOR@, @E-ANCHOR-KIND@,
  @E-SYNTAX@, @E-GRAMMAR@, @E-ERRATUM-PUBLISHED@, @E-MODIFIER@), @E-AMBIGUOUS@,
  @E-UNBOUND@, @W-RULE-UNIMPLEMENTED@ ("Hsed.Spec.Rules"), for the numbered
  tables and the signature units alike; over a spec annotated only in part
  (@hsedc --partial@, 'partially'), its @E-UNBOUND@ is the Warning @W-UNBOUND@;
* @C-EXEMPLAR@: each rule still fires on its exemplar;
* @W-RULE-SCOPE-EMPTY@, @N-RULE-SCOPE-REDUNDANT@: a rule anchor whose scope
  binds nothing to its rule, or that another anchor of the rule covers;
* @E-SHAPE@: every bound table's form is accepted by its binding;
* the checks of the document as a whole ("Hsed.Spec.Document"): its List of
  Tables against the captions (@LOT-VS-BODY@) and every table reference in its
  prose against the table it names (@XREF-RESOLVES@), Warnings that report the PDF.

Errata ("Hsed.Spec.Errata"): every bound table is read under the selected set,
its caption, header cells and data cells corrected by the @erratum scott [...]@
pairs its own annotation carries, and by nothing else: under @scott@ each
correction made is @W-ERRATUM-APPLIED@, under @published@ each defect left as
printed is @W-ERRATUM-KNOWN@, and under both a correction whose text the table
does not contain is @W-ERRATUM-UNUSED@. Everything a rule reads of a bound
table it reads corrected ('Hsed.Spec.RuleDef.btRead'); the rules fire on the
text as printed. 'errataFindingsUnder' gives what the same corrections report
under either set, for @build/report/ERRATA.md@ ("Hsed.Spec.Report").

Normalisations, the same under both sets, are Notes: a caption printed
@\<X\>_object _ref@ (@N-CAPTION-NORMALISED@), and a header cell in the typo table
or a byte grid's corner read as @Byte\\Bit@ (@N-HEADER-NORMALISED@), on every
numbered table.
-}
module Hsed.Spec.Check
  ( -- * The shared context
    mkContext
  , mkContextWith
  , bindingOf
  , errataFindingsUnder
    -- * Checks
  , checks
  , checkIR
  , sortFindings
  , partially
  ) where

import qualified RIO.List          as L
import qualified RIO.Map           as Map
import           RIO

import           Hsed.Spec.Anchor  (AnchorId (..), anchorParagraph, locationText, naturalKey, renderAnchorId)
import           Hsed.Spec.Common  (ErrataSet (..), Finding, Severity (..), SpecId (..), finding, matchesSpec)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.DSL     (SomeBinding (..), bindingAccepts, someBindingShape)
import           Hsed.Spec.Document (documentChecks)
import           Hsed.Spec.Errata  (Erratum (..), correctTable)
import           Hsed.Spec.Grammar (Payload (..))
import           Hsed.Spec.IR      (HeadingIR (..), ParaIR, SpecIR (..), TableIR (..))
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules   (checkExemplars, checkRuleAnchors, classifySignaturesWith, classifyWithRules)
import           Hsed.Spec.Scope   (ParsedAnnot (..), parseAnnots, tableSections)
import           Hsed.Spec.Shape   (UnitKind (..), gridCornerNormalisation, headerNormalisations, measure,
                                    normaliseCaption, rawCaptionTitle, shapeUnit)


-- * The shared context

-- | The context of one IR under one errata set, read alone (no base).
mkContext :: ErrataSet -> SpecIR -> Context
mkContext = mkContextWith registry Nothing

-- | 'mkContext' with the units classified by a set of rules (a test's stub
--   rule beside the registry's), over a base or none: the Context of the
--   spec this one is read over, read-only (Core's for an Opal run, @hsedc
--   --base@), where a rule's lookup this spec does not answer runs.
mkContextWith :: [RuleDef] -> Maybe Context -> ErrataSet -> SpecIR -> Context
mkContextWith ruleSet base es ir = Context
  { cxIR             = ir
  , cxSpec           = sid
  , cxErrata         = es
  , cxTables         = tables
  , cxClasses        = classes
  , cxClassFindings  = classFindings <> sigFindings
  , cxSections       = tableSections ir
  , cxCorrected      = fmap fst corrected
  , cxErrataFindings = concatMap snd (Map.elems corrected)
  , cxSigs           = signatureUnits ir
  , cxSigClasses     = sigClasses
  , cxParas          = paragraphBindings ir
  , cxHeadingUnits   = headingBindings ir
  , cxBase           = base
  }
  where
    sid                       = SpecId ir.spec
    tables                    = numberedTables ir
    (classes, classFindings)  = classifyWithRules ruleSet ir
    (sigClasses, sigFindings) = classifySignaturesWith ruleSet ir
    corrected                 = correctedUnder es sid tables classes

-- | The paragraph units of an IR: each list item or paragraph whose comment
--   (validated as every annotation is, "Hsed.Spec.Scope".'parseAnnots') binds
--   it with a shape that takes paragraph units, with that binding. A comment
--   that does not validate is reported by the classification, as any other.
paragraphBindings :: SpecIR -> Map AnchorId (ParaIR, SomeBinding)
paragraphBindings ir = Map.fromList
  [ (aid, (p, sb))
  | pa <- parseAnnots ir, Just aid@(ItemId _ _) <- [paAnchor pa], Right (Full sb) <- [paPayload pa]
  , fmap shapeUnit (someBindingShape sb) == Just ParagraphUnit, Right p <- [anchorParagraph ir aid] ]

-- | The heading units of an IR: each heading whose comment (validated as every
--   annotation is, so that it names exactly one heading) binds it with a shape
--   that takes heading units, with that binding. A heading carrying rule tokens
--   is a rule anchor, not a unit.
headingBindings :: SpecIR -> Map AnchorId (HeadingIR, SomeBinding)
headingBindings ir = Map.fromList
  [ (aid, (h, sb))
  | pa <- parseAnnots ir, Just aid@(SectionId s) <- [paAnchor pa], Right (Full sb) <- [paPayload pa]
  , fmap shapeUnit (someBindingShape sb) == Just HeadingUnit, h : _ <- [[ h | h <- ir.headings, h.section == Just s ]] ]

-- | Every bound table read under an errata set, with what reading it found: its
--   caption and cells corrected by the @erratum scott@ pairs of its own binding.
correctedUnder :: ErrataSet -> SpecId -> Map AnchorId TableIR -> Map AnchorId Classification -> Map AnchorId (TableIR, [Finding])
correctedUnder es sid tables classes = Map.fromList
  [ (aid, correctTable es sid tid page (map erratum (mErrata (modsOf sb))) t)
  | (aid, t) <- Map.toList tables, Just sb <- [Map.lookup aid classes >>= boundBinding]
  , let tid = renderAnchorId aid
        page = tablePage t
        erratum (from, into) = Erratum sid tid from into ("annotation at " <> locationText sid (fromMaybe 1 page) aid) ]

-- | What the corrections of a context's annotations report under an errata set,
--   whichever set the context was made for: @W-ERRATUM-APPLIED@ under @scott@,
--   @W-ERRATUM-KNOWN@ under @published@, @W-ERRATUM-UNUSED@ under both.
errataFindingsUnder :: ErrataSet -> Context -> [Finding]
errataFindingsUnder es ctx
  | es == cxErrata ctx = cxErrataFindings ctx
  | otherwise          = concatMap snd (Map.elems (correctedUnder es (cxSpec ctx) (cxTables ctx) (cxClasses ctx)))

-- | The binding a table ends up with.
bindingOf :: Context -> AnchorId -> Maybe SomeBinding
bindingOf ctx aid = Map.lookup aid (cxClasses ctx) >>= boundBinding


-- * The checks

-- | Every check: the generic ones, then each rule's on the tables bound to its shape.
checks :: Context -> [Finding]
checks ctx = concat
  [ cxClassFindings ctx
  , checkExemplars (cxIR ctx)
  , checkRuleAnchors (cxIR ctx) (cxClasses ctx) (cxSigClasses ctx)
    -- E-SHAPE on every bound table
  , [ f | (aid, t) <- Map.toList (cxTables ctx), Just (SomeBinding b) <- [bindingOf ctx aid]
        , Just f <- [bindingAccepts b (measure (cxSpec ctx) t)] ]
    -- the List of Tables and the table references
  , documentChecks (cxIR ctx)
    -- errata, then the normalisations every numbered table is read with
  , cxErrataFindings ctx
  , concatMap (captionNote (cxSpec ctx)) (Map.toList (cxTables ctx))
  , concatMap (headerNote (cxSpec ctx)) (Map.toList (cxTables ctx))
  , concatMap (cornerNote (cxSpec ctx)) (Map.toList (cxTables ctx))
  , concat [ ruleChecks r ctx (boundTables (ruleShape r) ctx) | r <- registry ]
  ]

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

-- | @N-HEADER-NORMALISED@: a byte grid's corner read as @Byte\\Bit@ (design
--   section 3.4: Core T17's @Bit@ over a row @Byte@, @Bit Byte@ in T18, T20,
--   T21, T38, T39 and T41-T44, and in Opal T2-T6 and T8).
cornerNote :: SpecId -> (AnchorId, TableIR) -> [Finding]
cornerNote sid (aid, t) =
  [ locatedAt sid (renderAnchorId aid) (fromMaybe 1 (tablePage t))
      (finding Note "N-HEADER-NORMALISED" ("the byte grid's corner " <> tshow corner
         <> maybe "" (\l -> " and the row " <> tshow l <> " under it") lone <> " read as Byte\\Bit: bytes down, bits 7 to 0 across"))
  | Just (corner, lone) <- [gridCornerNormalisation t] ]

-- | Every check of one IR under one errata set, over a base or none, errors first.
checkIR :: Maybe Context -> ErrataSet -> SpecIR -> [Finding]
checkIR base es ir = sortFindings (checks (mkContextWith registry base es ir))

-- | Errors, then Warnings, then Notes; within each by page, anchor in its
--   natural order (T8 before T10), code and message.
sortFindings :: [Finding] -> [Finding]
sortFindings = L.sortOn (\f -> (Cm.severity f, Cm.page f, naturalKey <$> Cm.anchor f, Cm.code f, Cm.message f))

-- | The findings of a run over specs annotated only in part (@hsedc --partial
--   SPEC@, each spec named as @--target@ names one; Opal until M5, design 13.26
--   (k)): each @E-UNBOUND@ of a unit of one of them, a numbered table or a
--   signature block nothing binds yet, is the Warning @W-UNBOUND@ (bookkeeping:
--   the spec's comments are still to come); every other finding is as it was.
--   No PDF byte and no spec data: the spec named is the run's choice.
partially :: [Text] -> [Finding] -> [Finding]
partially names = map relax
  where
    relax f
      | Cm.code f == "E-UNBOUND", Just s <- Cm.spec f, any (`matchesSpec` s) names = f { Cm.severity = Warning, Cm.code = "W-UNBOUND" }
      | otherwise = f
