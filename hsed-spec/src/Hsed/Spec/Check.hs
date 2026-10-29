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
section ("Hsed.Spec.Scope") and the bound tables read under the errata set. 'checks' runs the generic checks, then the
checks of every rule of "Hsed.Spec.Registry" on the tables bound to its shape
(each rule's own module documents its checks and their codes). The generic
checks, each emitting findings with the code in brackets:

* classification: annotations refused (@E-ANCHOR@, @E-ANCHOR-KIND@,
  @E-SYNTAX@, @E-GRAMMAR@, @E-ERRATUM-PUBLISHED@, @E-MODIFIER@), @E-AMBIGUOUS@,
  @W-UNBOUND@, @W-RULE-UNIMPLEMENTED@ ("Hsed.Spec.Rules"), for the numbered
  tables and the signature units alike;
* @C-EXEMPLAR@: each rule still fires on its exemplar;
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
@\<X\>_object _ref@ (@N-CAPTION-NORMALISED@) and a header cell in the typo table
(@N-HEADER-NORMALISED@), on every numbered table.
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
  ) where

import qualified RIO.List          as L
import qualified RIO.Map           as Map
import           RIO

import           Hsed.Spec.Anchor  (AnchorId, locationText, naturalKey, renderAnchorId)
import           Hsed.Spec.Common  (ErrataSet (..), Finding, Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.DSL     (SomeBinding (..), bindingAccepts)
import           Hsed.Spec.Document (documentChecks)
import           Hsed.Spec.Errata  (Erratum (..), correctTable)
import           Hsed.Spec.IR      (SpecIR (..), TableIR (..))
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules   (checkExemplars, classifySignaturesWith, classifyWithRules)
import           Hsed.Spec.Scope   (tableSections)
import           Hsed.Spec.Shape   (headerNormalisations, measure, normaliseCaption, rawCaptionTitle)


-- * The shared context

-- | The context of one IR under one errata set.
mkContext :: ErrataSet -> SpecIR -> Context
mkContext = mkContextWith registry

-- | 'mkContext' with the units classified by a set of rules (a test's stub rule beside the registry's).
mkContextWith :: [RuleDef] -> ErrataSet -> SpecIR -> Context
mkContextWith ruleSet es ir = Context
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
  }
  where
    sid                       = SpecId ir.spec
    tables                    = numberedTables ir
    (classes, classFindings)  = classifyWithRules ruleSet ir
    (sigClasses, sigFindings) = classifySignaturesWith ruleSet ir
    corrected                 = correctedUnder es sid tables classes

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
    -- E-SHAPE on every bound table
  , [ f | (aid, t) <- Map.toList (cxTables ctx), Just (SomeBinding b) <- [bindingOf ctx aid]
        , Just f <- [bindingAccepts b (measure (cxSpec ctx) t)] ]
    -- the List of Tables and the table references
  , documentChecks (cxIR ctx)
    -- errata, then the normalisations every numbered table is read with
  , cxErrataFindings ctx
  , concatMap (captionNote (cxSpec ctx)) (Map.toList (cxTables ctx))
  , concatMap (headerNote (cxSpec ctx)) (Map.toList (cxTables ctx))
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

-- | Every check of one IR under one errata set, errors first.
checkIR :: ErrataSet -> SpecIR -> [Finding]
checkIR es ir = sortFindings (checks (mkContext es ir))

-- | Errors, then Warnings, then Notes; within each by page, anchor in its
--   natural order (T8 before T10), code and message.
sortFindings :: [Finding] -> [Finding]
sortFindings = L.sortOn (\f -> (Cm.severity f, Cm.page f, naturalKey <$> Cm.anchor f, Cm.code f, Cm.message f))
