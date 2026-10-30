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
other than as printed, and what is worth a look. A finding of a code the
catalogue does not know is listed apart, so that a new rule's finding is
never dropped from the report before the catalogue names its code.
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
import           Hsed.Spec.Common (ErrataSet (..), Finding, RuleName (..), Severity (..), SpecId (..), errataSetName,
                                   renderFinding)
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.DSL    (someBindingShape)
import           Hsed.Spec.Grammar (printPayload)
import           Hsed.Spec.IR     (AnnotIR (..), SigIR (..), SpecIR (..), TableIR (..))
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

-- | What the findings of a code say, for @ERRATA.md@.
data Reports
  = Defect !Text     -- ^ the PDF is wrong, or disagrees with itself: what the code finds
  | AsPrinted !Text  -- ^ hsedc reads the text other than as printed (a Note, the same under both errata sets)
  | Suspect !Text    -- ^ the text may be wrong: worth a look
  | Correction       -- ^ an annotation's erratum, applied, left as printed, or matching nothing
  | Bookkeeping      -- ^ hsedc's own state: what binds what, what is generated, the annotations themselves
  deriving (Eq, Show)

-- | The catalogue codes (design section 4.2) by what their findings say. A
--   code it does not name is listed apart in @ERRATA.md@ until it does.
catalogue :: Map Text Reports
catalogue = Map.fromList $
  [ (c, Defect d) | (c, d) <-
    [ ("LOT-VS-BODY",      "the List of Tables disagrees with the captions in the body")
    , ("XREF-RESOLVES",    "a table reference names no table, or not the table its text means")
    , ("C-UID-UNIQUE",     "two tables print one UID")
    , ("C-UID-TABLE",      "a UID is not one of its table's")
    , ("C-UID-RANGE",      "a UID lies outside the range assigned to its kind")
    , ("C-ENUM-RANGE",     "an enumeration's values do not fit its type's range")
    , ("C-T240-HALVES",    "a table's UIDs disagree with its descriptor object's")
    , ("C-T240-NAME",      "a table name is no identifier, or names two tables")
    , ("C-FMT-RESOLVE",    "a Format or a column's type names a type no table defines")
    , ("E-FMT-RESOLVE",    "a Format names a type no table defines")
    , ("C-COL-CONTIG",     "a table description's column numbers leave a gap")
    , ("C-COL-CELL",       "a table description's row is not four cells, or names a column twice or by no identifier")
    , ("C-DESC-TABLE",     "a table description names no table of Table 240, or one another description names")
    , ("C-CONST-DESC",     "a table of column values names no described table enclosing it, or one another names")
    , ("C-CONST-VALUE",    "a column value names no column, does not read or fit, or disagrees with its remark")
    , ("C-LAYOUT-TILE",    "a byte layout's fields do not tile its bytes")
    , ("C-LAYOUT-ENDIAN",  "a field's (MSB) and (LSB) marks disagree")
    , ("C-PRECONF-HEADER", "a preconfiguration table's columns are not its table's")
    , ("C-PRECONF-CELL",   "a preconfigured cell does not read as its column's type")
    , ("C-PRECONF-UID",    "the table of a table's UIDs disagrees with the rows preconfigured for that table")
    , ("C-UID-NAME",       "two rows of the UID tables give one name")
    , ("C-SIG-PARSE",      "a method signature does not parse as printed")
    , ("C-CODE-VALUE",     "a code table has no code column, a row of another width, a downward range or two catch-alls")
    , ("W-CODE-OVERLAP",   "two rows of a code table assign one value")
    , ("C-CODE-TYPE",      "a code column's type names no Type object")
    , ("C-STATUS-CODE",    "a status row is not one name and one value, or gives a value twice")
    , ("C-FIELD-VALUE",    "a command block's row is not one named field and its value, or names a field twice")
    , ("C-FIELD-TYPE",     "a record field's type names no Type object, or its range is not one the type holds")
    , ("C-TOKEN-TABLE",    "the token table covers a byte twice or not at all, or a row's bit pattern and hex range disagree")
    , ("C-STATE-TABLE",    "a state table's row names no state, or leaves a field without a setting")
    , ("C-PROP-TYPE",      "a property's type names no Type object, or its applicability no communicator")
    , ("C-PROP-INITIAL",   "an initial assumption does not read by its property's type, or its arithmetic does not hold")
    , ("C-LEGEND-TOTAL",   "a preconfiguration cell's style maps to no class of the colour key")
    , ("C-PATCH-UID",      "an SSC's change to a Core definition changes its UID")
    , ("C-PATCH-MONOTONE", "an SSC's value lies below the Core minimum")
    , ("C-PROP-NAME",      "a property name the Properties tables do not give")
    , ("C-SIG-METHOD",     "a method signature whose method has no UID")
    , ("C-ACL-REFS",       "an access control row names no ACE, authority or method")
    , ("W-ENUM-OVERLAP",   "two rows of an enumeration assign one value")
    , ("W-LEGEND-OFFSCALE", "a cell's fill is none of the colour key's") ] ]
  <> [ (c, AsPrinted d) | (c, d) <-
    [ ("N-CAPTION-NORMALISED", "a caption read as one identifier where the PDF prints a space in it")
    , ("N-HEADER-NORMALISED",  "a header cell read through the typo table")
    , ("N-FORMAT-NORMALISED",  "a Format cell read with its spacing or spelling normalised")
    , ("N-SIG-NORMALISED",     "a method signature read with its punctuation normalised")
    , ("N-RANGE-NAME-NORMALISED", "a type format's name read as the Format cells spell it")
    , ("N-RANGE-ONE-CELL",     "a range printed as one cell spanning its start and end")
    , ("N-STATUS-NAME-SUFFIXED", "a status name given to several values, the later ones suffixed")
    , ("W-PROP-NAME-CASE",     "a property name printed in two spellings, read as the Properties table spells it") ] ]
  <> [ (c, Suspect d) | (c, d) <-
    [ ("W-ENUM-LABEL-REPEATED",   "an enumeration gives one label to several rows")
    , ("C-TOKEN-CODEC",           "the token table and hsed's hand-written codec disagree")
    , ("W-STATUS-SECTION",        "a subsection of the status table's section names no status of it")
    , ("N-STATUS-UNDESCRIBED",    "a status no subsection of its section describes")
    , ("N-UID-RANGE-NO-TEMPLATE", "UID rows that name no template, with the template range each lies in") ] ]
  <> [ (c, Correction) | c <- ["W-ERRATUM-APPLIED", "W-ERRATUM-KNOWN", "W-ERRATUM-UNUSED"] ]
  <> [ (c, Bookkeeping) | c <-
    [ "W-UNBOUND", "E-UNBOUND", "E-AMBIGUOUS", "W-RULE-UNIMPLEMENTED", "W-GEN-EXCLUDED", "C-EXEMPLAR", "W-WAIVER-UNUSED"
    , "W-PINS-NONE", "E-SHAPE", "E-ANCHOR", "E-ANCHOR-KIND", "E-SYNTAX", "E-GRAMMAR", "E-MODIFIER", "E-SIDECAR"
    , "W-RULE-SCOPE-EMPTY", "N-RULE-SCOPE-REDUNDANT"
    , "E-ERRATUM-PUBLISHED", "E-GEN-MODULE", "E-FMT-SYNTAX", "E-FMT-CODE", "E-FMT-ARITY"
    , "N-UID-UNNAMED", "N-RANGE-UNCHECKED", "W-PRECONF-TEMPLATE" ] ]

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
  , "(`" <> specIdText sid <> ".pdf#page=116&5.1.3.55` is page 116, section 5.1.3.55 there; the printed page is the PDF"
  , "page less " <> tshow ir.labelOffset <> ")."
  , ""
  , "## Corrections the annotations carry"
  , "" ]
  <> corrections
  <> groups "Defects of the PDF" "What the checks find wrong in the text as printed." (\case Defect d -> Just d; _ -> Nothing)
  <> groups "Read other than as printed" "What hsedc reads other than as printed, the same under both errata sets: spellings the PDF prints inconsistently." (\case AsPrinted d -> Just d; _ -> Nothing)
  <> groups "Worth a look" "What may be wrong; some are as meant." (\case Suspect d -> Just d; _ -> Nothing)
  <> unclassified
  where
    ir  = cxIR ctx
    sid = cxSpec ctx
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
    groups heading intro pick = case [ (c, d, sel) | (c, sel) <- byCode, Just r <- [reportsOf c], Just d <- [pick r] ] of
      [] -> []
      gs -> ["", "## " <> heading, "", intro]
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

-- | The tables nothing binds: unbound (@W-UNBOUND@), ambiguous (@E-AMBIGUOUS@)
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
      _           -> "W-UNBOUND"
