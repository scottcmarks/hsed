{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Report
Description : The ledger, coverage, findings and errata reports of hsedc
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What @hsedc ledger@ and @hsedc orphans@ print and what @hsedc compile@ writes to
@build\/report\/{findings,coverage,ERRATA,ledger}.md@ (design section 5.2): how every
numbered table of a spec is bound (rule-bound, through a bare modifier comment,
comment-bound, ambiguous, invalid, unbound), how every signature unit (method
signature block) is (rule-bound, unbound, ambiguous), what every anchor does,
what the checks found, and which of those findings report the PDF itself.
Plain text, no timestamps, so that two runs on the same input write the same
bytes.

The ledger ('ledgerMarkdown') is also committed: @make gen@ copies it to
@docs/LEDGER.md@ and @make gen-diff@ keeps it equal to what the committed
sources give. It holds the totals (tables and signature blocks per binding,
the List of Tables entries and table references the document checks read,
findings per code), so that the tests can hold invariants instead and a new
rule edits no generic test.

@ERRATA.md@ ('errataMarkdown') is the list Scott can send to TCG: the
corrections the annotations carry, under each errata set, then the findings
whose code reports the PDF ('catalogue'): its defects, the text hsedc reads
other than as printed, and what is worth a look. The catalogue is assembled
('codeCatalogue') from the codes the generic modules give ('genericCodes'),
those of the readers several rules share ("Hsed.Spec.Readers") and those each
rule claims ('Hsed.Spec.RuleDef.ruleCodes'), so that a new rule names its own
codes and edits no generic module. A finding of a code the catalogue does not
know is listed apart, so that it is never dropped from the report.
-}
module Hsed.Spec.Report
  ( Ledger (..)
  , ledger
  , renderLedger
  , ledgerMarkdown
  , coverageMarkdown
  , findingsMarkdown
  , errataMarkdown
  , Reports (..)
  , CodeDef (..)
  , genericCodes
  , codeCatalogue
  , catalogue
  , reportsOf
  , orphans
  , findingCounts
  ) where

import qualified Data.Text        as DT
import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Anchor (AnchorId (..), locationText, parseAnchorId, renderAnchorId)
import           Hsed.Spec.Check  (errataFindingsUnder)
import           Hsed.Spec.Common (CodeDef (..), ErrataSet (..), Finding, Reports (..), RuleName (..), Severity (..),
                                   SpecId (..), errataSetName, renderFinding)
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.DSL    (someBindingShape)
import           Hsed.Spec.Grammar (printPayload)
import           Hsed.Spec.IR     (AnnotIR (..), SigIR (..), SpecIR (..), TableIR (..))
import           Hsed.Spec.Readers (readerCodes)
import           Hsed.Spec.RuleDef (Context (..), Mods (..), modsOf)
import           Hsed.Spec.Rules
import           Hsed.Spec.Shape  (captionTitle, shapeName)


-- | How the numbered tables and signature blocks of one spec are bound, and
--   how much of the document the document checks read.
data Ledger = Ledger
  { lSpec          :: !SpecId
  , lTables        :: !Int
  , lRuleBound     :: !Int   -- ^ bound by a rule, with or without a bare modifier comment
  , lByModifiers   :: !Int   -- ^ of those, through a bare modifier comment
  , lCommentBound  :: !Int   -- ^ bound by a full binding in a comment
  , lAmbiguous     :: !Int
  , lInvalid       :: !Int
  , lUnbound       :: !Int
  , lByBinding     :: ![(Text, Int)]  -- ^ the bound tables by what binds them: @rule typeRow@, @comment EnumValues@
  , lSigs          :: !Int   -- ^ the signature units
  , lSigsBound     :: !Int   -- ^ of those, bound by a rule
  , lSigsUnbound   :: !Int
  , lSigsAmbiguous :: !Int
  , lSigsByBinding :: ![(Text, Int)]  -- ^ the bound signature units by the rule binding them
  , lLot           :: !Int   -- ^ the List of Tables entries (LOT-VS-BODY)
  , lXrefs         :: !Int   -- ^ the table references in the prose (XREF-RESOLVES)
  }
  deriving (Eq, Show)

ledger :: SpecIR -> Ledger
ledger ir = Ledger
  { lSpec          = SpecId ir.spec
  , lTables        = Map.size classes
  , lRuleBound     = count (\case RuleBound {} -> True; _ -> False)
  , lByModifiers   = count (\case RuleBound _ (_ : _) _ -> True; _ -> False)
  , lCommentBound  = count (\case CommentBound _ -> True; _ -> False)
  , lAmbiguous     = count (\case Ambiguous _ -> True; _ -> False)
  , lInvalid       = count (\case Invalid _ -> True; _ -> False)
  , lUnbound       = count (\case Unbound -> True; _ -> False)
  , lByBinding     = tally (Map.elems classes)
  , lSigs          = Map.size sigClasses
  , lSigsBound     = countSigs (\case RuleBound {} -> True; _ -> False)
  , lSigsUnbound   = countSigs (\case Unbound -> True; _ -> False)
  , lSigsAmbiguous = countSigs (\case Ambiguous _ -> True; _ -> False)
  , lSigsByBinding = tally (Map.elems sigClasses)
  , lLot           = length ir.lot
  , lXrefs         = length ir.xrefs
  }
  where
    classes = classify ir
    sigClasses = fst (classifySignatures ir)
    count p = length (filter p (Map.elems classes))
    countSigs p = length (filter p (Map.elems sigClasses))
    tally cs = Map.toList (Map.fromListWith (+) [ (b, 1) | c <- cs, Just b <- [binder c] ])
    binder = \case
      RuleBound r _ _  -> Just ("rule " <> ruleNameText r)
      CommentBound sb  -> Just ("comment " <> maybe "shaped by its target" shapeName (someBindingShape sb))
      _                -> Nothing

-- | @hsedc ledger@: one line per spec.
renderLedger :: [Ledger] -> Text
renderLedger = T.unlines . map line
  where
    line l = T.concat
      [ specIdText (lSpec l), ": ", tshow (lTables l), " numbered tables: "
      , tshow (lRuleBound l), " rule-bound (", tshow (lByModifiers l), " through a bare modifier comment), "
      , tshow (lCommentBound l), " comment-bound, ", tshow (lUnbound l), " unbound, "
      , tshow (lAmbiguous l), " ambiguous, ", tshow (lInvalid l), " invalid; "
      , tshow (lByModifiers l + lCommentBound l + lInvalid l), " carry a comment; "
      , tshow (lSigs l), " signature blocks: ", tshow (lSigsBound l), " rule-bound, "
      , tshow (lSigsUnbound l), " unbound, ", tshow (lSigsAmbiguous l), " ambiguous; "
      , tshow (lLot l), " List of Tables entries, ", tshow (lXrefs l), " table references" ]

-- | The ledger of @hsedc compile@ (@build/report/ledger.md@, committed as
--   @docs/LEDGER.md@): how each numbered table and signature block is bound,
--   what binds the bound ones, what the document checks read, and the findings
--   of the run by code, for each spec with its findings.
ledgerMarkdown :: ErrataSet -> [(Ledger, [Finding])] -> Text
ledgerMarkdown es ls = T.unlines $
  [ "# Binding ledger"
  , ""
  , "Generated by `hsedc compile` (`build/report/ledger.md`), copied here by `make gen` and kept equal"
  , "to what the committed sources give by `make gen-diff`: the totals live here, not in the tests."
  , "How each numbered table is bound (docs/spec-as-source.md section 3.4); rule-bound includes the"
  , "tables bound through a bare modifier comment on the rule default."
  , ""
  , "| spec | tables | rule-bound | through a modifier comment | comment-bound | unbound | ambiguous | invalid |"
  , "|---|---:|---:|---:|---:|---:|---:|---:|" ]
  <> [ "| " <> T.intercalate " | " (specIdText (lSpec l) : map tshow
         [lTables l, lRuleBound l, lByModifiers l, lCommentBound l, lUnbound l, lAmbiguous l, lInvalid l]) <> " |"
     | (l, _) <- ls ]
  <> [ ""
     , "The method signature blocks (signature units, docs/ir-v1.md) carry no comment: a rule binds them by its scope."
     , ""
     , "| spec | signature blocks | rule-bound | unbound | ambiguous |"
     , "|---|---:|---:|---:|---:|" ]
  <> [ "| " <> T.intercalate " | " (specIdText (lSpec l) : map tshow [lSigs l, lSigsBound l, lSigsUnbound l, lSigsAmbiguous l]) <> " |"
     | (l, _) <- ls ]
  <> [ ""
     , "What the document checks read (docs/ir-v1.md): LOT-VS-BODY compares each List of Tables entry with"
     , "the caption it names, XREF-RESOLVES each table reference in the prose with the table it names; what"
     , "they report is among the findings below and in `build/report/ERRATA.md`."
     , ""
     , "| spec | List of Tables entries | table references |"
     , "|---|---:|---:|" ]
  <> [ "| " <> T.intercalate " | " (specIdText (lSpec l) : map tshow [lLot l, lXrefs l]) <> " |" | (l, _) <- ls ]
  <> concatMap perSpec ls
  where
    perSpec (l, fs) =
      [ "", "## " <> specIdText (lSpec l) <> ": what binds the tables", ""
      , "| binding | tables |", "|---|---:|" ]
      <> [ "| " <> b <> " | " <> tshow n <> " |" | (b, n) <- lByBinding l ]
      <> (if null (lSigsByBinding l) then [] else
           [ "", "## " <> specIdText (lSpec l) <> ": what binds the signature blocks", ""
           , "| binding | signature blocks |", "|---|---:|" ]
           <> [ "| " <> b <> " | " <> tshow n <> " |" | (b, n) <- lSigsByBinding l ])
      <> [ "", "## " <> specIdText (lSpec l) <> ": findings under --errata " <> errataSetName es, "" ]
      <> codeTable fs

-- | The findings counted by severity and code.
codeTable :: [Finding] -> [Text]
codeTable fs =
  [ "| severity | code | findings |", "|---|---|---:|" ]
  <> [ "| " <> tshow sev <> " | " <> c <> " | " <> tshow n <> " |"
     | ((sev, c), n) <- Map.toList (Map.fromListWith (+) [ ((Cm.severity f, Cm.code f), 1 :: Int) | f <- fs ]) ]

-- | Every numbered table (anchor, caption title, section, PDF page, binding),
--   every signature unit (anchor, heads, PDF page, binding), both in document
--   order, and every anchor (what it is, its payload, and what it binds).
coverageMarkdown :: SpecIR -> Text
coverageMarkdown ir = T.unlines $
  [ "# Coverage: " <> ir.spec
  , ""
  , "Generated by `hsedc compile`: every numbered table, every method signature block and every anchor, and what binds"
  , "what. " <> tshow (Map.size classes) <> " numbered tables, " <> tshow (length ir.sigs) <> " signature blocks, "
      <> tshow (length annots) <> " anchors."
  , ""
  , "## Numbered tables"
  , ""
  , "| table | caption | section | PDF page | binding |"
  , "|---|---|---|---:|---|" ]
  <> [ row [renderAnchorId aid, title t, fromMaybe "" (sectionOf t), maybe "" tshow (tablePage t), describeClassification c]
     | (aid, c) <- Map.toList classes, Just t <- [Map.lookup aid tables] ]
  <> [ "", "## Signature blocks", "", "| signature block | heads | PDF page | binding |", "|---|---|---:|---|" ]
  <> [ row [renderAnchorId aid, T.intercalate ", " s.heads, tshow (s.page + 1), maybe "" describeClassification (Map.lookup aid sigClasses)]
     | (aid, s) <- inDocumentOrder ]
  <> [ "", "## Anchors", ""
     , "Each annotation of the PDF, by its anchor id: a rule token binds by its scope (the whole document from the"
     , "`doc` anchor, a section's subtree from a heading), a table anchor binds or modifies its table."
     , ""
     , "| anchor | kind | PDF page | payload | what it binds |", "|---|---|---:|---|---|" ]
  <> [ row [a.id, tshow a.kind, tshow (a.page + 1), a.contents, effect pa] | pa@(ParsedAnnot a _ _) <- annots ]
  where
    classes    = classify ir
    sigClasses = fst (classifySignatures ir)
    tables     = numberedTables ir
    sections   = tableSections ir
    annots     = parseAnnots ir
    sectionOf :: TableIR -> Maybe Text
    sectionOf t = join (Map.lookup t.structPath sections)
    title :: TableIR -> Text
    title t = fromMaybe "" (t.caption >>= captionTitle)
    row cells = "| " <> T.intercalate " | " (map cell cells) <> " |"
    cell = DT.replace "|" "\\|" . T.unwords . T.words
    -- the IR's blocks in document order, one per anchor id
    inDocumentOrder = L.nubBy (\a b -> fst a == fst b) [ (SigId s.section, s) | s <- ir.sigs ]
    effect pa = case (paAnchor pa, paPayload pa) of
      (_, Left f)               -> "invalid: " <> renderFinding f
      (Just aid@(TableId _), _) -> maybe "no such table" describeClassification (Map.lookup aid classes)
      (Just DocId, Right p)     -> scoped WholeDocument p
      (Just (SectionId s), Right p) -> scoped (Subtree s) p
      (_, Right p)              -> printPayload p
    -- what each rule token of an anchor binds within the anchor's scope
    scoped sec p = case ruleTokens p of
      [] -> "the document"
      rs -> T.intercalate "; " (map (bindsWithinText sec) rs)
    bindsWithinText sec r = T.concat
      [ "rule ", ruleNameText r, ": "
      , case bindsWithin ir classes sigClasses sec r of
          (0, 0) | isNothing (ruleNamed r) -> "not implemented yet"
                 | otherwise               -> "binds nothing here"
          (n, 0) -> counted n "table"
          (0, m) -> counted m "signature block"
          (n, m) -> counted n "table" <> ", " <> counted m "signature block" ]
    counted n w = tshow n <> " " <> w <> (if n == (1 :: Int) then "" else "s")

-- | The findings of a run: their counts by code, then each, errors first, in
--   'renderFinding' form.
findingsMarkdown :: SpecId -> ErrataSet -> [Finding] -> Text
findingsMarkdown sid es fs = T.unlines $
  [ "# Findings: " <> specIdText sid <> ", errata " <> errataSetName es
  , ""
  , "Generated by `hsedc compile`. " <> findingCounts fs <> "; the findings that report the PDF itself are"
  , "also in `ERRATA.md`."
  , "" ]
  <> codeTable fs
  <> concatMap section [(Error, "Errors"), (Warning, "Warnings"), (Note, "Notes")]
  where
    section (sev, heading) = case filter ((== sev) . Cm.severity) fs of
      []  -> []
      sel -> ["", "## " <> heading <> " (" <> tshow (length sel) <> ")", "", "```"] <> map renderFinding sel <> ["```"]

-- | @2 errors, 130 warnings, 0 notes@
findingCounts :: [Finding] -> Text
findingCounts fs = T.intercalate ", " [ tshow (n sev) <> " " <> w | (sev, w) <- [(Error, "errors"), (Warning, "warnings"), (Note, "notes")] ]
  where n sev = length (filter ((== sev) . Cm.severity) fs)


-- * ERRATA.md

-- | The codes the generic modules give (design section 2 Q2): the
--   classification and the annotations ("Hsed.Spec.Rules", "Hsed.Spec.Scope",
--   "Hsed.Spec.Grammar", "Hsed.Spec.DSL", "Hsed.Spec.Shape",
--   "Hsed.Spec.Sidecar"; @W-UNBOUND@, an unbound unit of a spec annotated in
--   part, "Hsed.Spec.Check".'Hsed.Spec.Check.partially'), the errata
--   ("Hsed.Spec.Errata"), the normalisations ("Hsed.Spec.Check"), the Format
--   parser ("Hsed.Spec.Format"), compilation ("Hsed.Spec.Compile") and the
--   document checks ("Hsed.Spec.Document"). A
--   rule's codes are its own ('ruleCodes'), a shared reader's its
--   ("Hsed.Spec.Readers".'readerCodes').
genericCodes :: [CodeDef]
genericCodes =
  [ CodeDef "LOT-VS-BODY"          [Warning] (Defect "the List of Tables disagrees with the captions in the body")
  , CodeDef "XREF-RESOLVES"        [Warning] (Defect "a table reference in the prose names no table, or not the table its text means (one in a table cell is not read)")
  , CodeDef "E-FMT-RESOLVE"        [Error]   (Defect "a Format names a type no table defines")
  , CodeDef "N-CAPTION-NORMALISED" [Note]    (AsPrinted "a caption read as one identifier where the PDF prints a space in it")
  , CodeDef "N-HEADER-NORMALISED"  [Note]    (AsPrinted "a header cell read through the typo table, or a byte grid's corner read as Byte\\Bit")
  ]
  <> [ CodeDef c [Warning] Correction | c <- ["W-ERRATUM-APPLIED", "W-ERRATUM-KNOWN", "W-ERRATUM-UNUSED"] ]
  <> [ CodeDef c sevs Bookkeeping | (c, sevs) <-
    [ ("E-UNBOUND", [Error]), ("W-UNBOUND", [Warning]), ("E-AMBIGUOUS", [Error]), ("W-RULE-UNIMPLEMENTED", [Warning])
    , ("W-GEN-EXCLUDED", [Warning]), ("C-EXEMPLAR", [Error]), ("W-RULE-SCOPE-EMPTY", [Warning]), ("N-RULE-SCOPE-REDUNDANT", [Note])
    , ("E-SHAPE", [Error, Warning]), ("E-ANCHOR", [Error]), ("E-ANCHOR-KIND", [Error]), ("E-SYNTAX", [Error]), ("E-GRAMMAR", [Error])
    , ("E-MODIFIER", [Error]), ("E-SIDECAR", [Error]), ("E-ERRATUM-PUBLISHED", [Error]), ("E-GEN-MODULE", [Error])
    , ("E-FMT-SYNTAX", [Error]), ("E-FMT-CODE", [Error]), ("E-FMT-ARITY", [Error]) ] ]

-- | Every code of the catalogue: the generic ones, the shared readers', then
--   each rule's in registry order. Each is claimed once (@RegistrySpec@).
codeCatalogue :: [CodeDef]
codeCatalogue = genericCodes <> readerCodes <> concatMap ruleCodes rules

-- | The catalogue codes by what their findings say. A code it does not name
--   is listed apart in @ERRATA.md@ until its rule claims it.
catalogue :: Map Text Reports
catalogue = Map.fromList [ (codeName c, codeReports c) | c <- codeCatalogue ]

-- | What a code's findings say; Nothing for a code the catalogue does not name.
reportsOf :: Text -> Maybe Reports
reportsOf c = Map.lookup c catalogue

-- | @build/report/ERRATA.md@: the corrections the annotations carry, under each
--   errata set, and the findings of the run (the context's set) that report the
--   PDF, grouped by code, each at its page with the printed page beside it.
errataMarkdown :: Context -> [Finding] -> Text
errataMarkdown ctx fs = T.unlines $
  [ "# Errata: " <> specIdText sid
  , ""
  , "Generated by `hsedc compile` (`build/report/ERRATA.md`) from " <> ir.source <> " under `--errata "
      <> errataSetName (cxErrata ctx) <> "`:"
  , "the corrections its annotations carry, then what the checks find in the text as printed, each at its PDF page"
  , "(" <> example <> "the printed page is the PDF page less " <> tshow ir.labelOffset <> ")."
  , ""
  , "## Corrections the annotations carry"
  , "" ]
  <> corrections
  <> concatMap section sections
  <> unclassified
  where
    ir  = cxIR ctx
    sid = cxSpec ctx
    sections =
      [ ("Defects of the PDF", "What the checks find wrong in the text as printed.", grouped (\case Defect d -> Just d; _ -> Nothing))
      , ("Read other than as printed", "Text hsedc reads other than as printed, the same under both errata sets."
        , grouped (\case AsPrinted d -> Just d; _ -> Nothing))
      , ("Worth a look", "What may be wrong; some are as meant.", grouped (\case Suspect d -> Just d; _ -> Nothing)) ]
    -- how a location reads, shown on the first finding listed
    example = case [ (loc, p, u) | (_, _, gs) <- sections, (_, _, f : _) <- take 1 gs, Just loc <- [location f]
                                 , Just p <- [Cm.page f], Just u <- [T.stripSuffix ", " (unitOf f)] ] of
      (loc, p, u) : _ -> "`" <> loc <> "` is page " <> tshow p <> ", " <> u <> " there; "
      []              -> ""
    corrections = case annotated of
      [] -> ["No annotation carries a correction."]
      _  ->
        [ "Each `erratum scott [...]` annotation corrects the text of its own table: `--errata scott` reads the text"
        , "corrected, `--errata published` reads it as printed and reports each correction as a known defect."
        , ""
        , "| table | PDF page | printed page | as printed | corrected | annotation |"
        , "|---|---:|---:|---|---|---|" ]
        <> [ "| " <> T.intercalate " | " [renderAnchorId aid <> " " <> title t, tshow pdf, printed pdf, quote from, quote into
                                         , "`" <> locationText sid pdf aid <> "`"] <> " |"
           | (aid, t, pdf, from, into) <- annotated ]
        <> concat [ ["", "Under `--errata " <> errataSetName es <> "`:", ""] <> map item (errataFindingsUnder es ctx)
                  | es <- [Scott, Published] ]
    annotated =
      [ (aid, t, fromMaybe 1 (tablePage t), from, into)
      | (aid, c) <- Map.toList (cxClasses ctx), Just sb <- [boundBinding c], Just t <- [Map.lookup aid (cxTables ctx)]
      , (from, into) <- mErrata (modsOf sb) ]
    title :: TableIR -> Text
    title t = fromMaybe "" (t.caption >>= captionTitle)
    grouped pick = [ (c, d, sel) | (c, sel) <- byCode, Just r <- [reportsOf c], Just d <- [pick r] ]
    section (heading, intro, gs) = case gs of
      [] -> []
      _  -> ["", "## " <> heading, "", intro]
            <> concat [ ["", "### " <> c <> ": " <> d <> " (" <> tshow (length sel) <> ")", ""] <> map item sel | (c, d, sel) <- gs ]
    unclassified = case [ (c, sel) | (c, sel) <- byCode, isNothing (reportsOf c) ] of
      [] -> []
      gs -> ["", "## Not yet classified", ""
            , "Findings whose code `Hsed.Spec.Report.catalogue` does not name yet: they may report the PDF or hsedc."]
            <> concat [ ["", "### " <> c <> " (" <> tshow (length sel) <> ")", ""] <> map item sel | (c, sel) <- gs ]
    byCode = Map.toList (Map.fromListWith (flip (<>)) [ (Cm.code f, [f]) | f <- fs ])
    item f = "- " <> T.concat [ "`" <> loc <> "` " | Just loc <- [location f] ] <> where' f <> Cm.message f
    location f = case (Cm.spec f, Cm.page f) of
      (Just s, Just p) -> Just (specIdText s <> ".pdf#page=" <> tshow p <> maybe "" ("&" <>) (Cm.anchor f))
      _                -> Cm.anchor f
    where' f = case Cm.page f of
      Just p | p > ir.labelOffset -> "(" <> unitOf f <> "printed page " <> printed p <> "): "
      _                           -> T.concat [ "(" <> u <> "): " | Just u <- [T.stripSuffix ", " (unitOf f)] ]
    unitOf f = case Cm.anchor f >>= parseAnchorId of
      Just (TableId n)    -> "Table " <> tshow n <> ", "
      Just (SectionId s)  -> "section " <> s <> ", "
      Just (ItemId s i)   -> "section " <> s <> " item " <> i <> ", "
      Just (SigId s)      -> "the method signature of section " <> s <> ", "
      Just DocId          -> "the document, "
      Nothing             -> ""
    printed p = tshow (p - ir.labelOffset)
    quote t = "\"" <> t <> "\""

-- | The tables nothing binds: unbound (@E-UNBOUND@), ambiguous (@E-AMBIGUOUS@)
--   or with an annotation that does not bind (its Error), each as its finding
--   says it; then the signature units nothing binds, likewise.
orphans :: SpecIR -> [(AnchorId, Finding)]
orphans ir =
  [ (aid, f) | (cs, fs) <- [classifyWith ir, classifySignatures ir], (aid, c) <- Map.toList cs, orphan c, f <- located fs aid c ]
  where
    orphan = \case
      Unbound     -> True
      Ambiguous _ -> True
      Invalid _   -> True
      _           -> False
    located fs aid = \case
      Invalid f -> [f]
      c         -> take 1 [ f | f <- fs, Cm.code f == codeOf c, Cm.anchor f == Just (renderAnchorId aid) ]
    codeOf = \case
      Ambiguous _ -> "E-AMBIGUOUS"
      _           -> "E-UNBOUND"
