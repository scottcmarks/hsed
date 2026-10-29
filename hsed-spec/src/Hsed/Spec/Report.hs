{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Report
Description : The ledger, coverage and findings reports of hsedc
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What @hsedc ledger@ and @hsedc orphans@ print and what @hsedc compile@ writes to
@build\/report\/{findings,coverage,ledger}.md@ (design section 5.2): how every
numbered table of a spec is bound (rule-bound, through a bare modifier comment,
comment-bound, ambiguous, invalid, unbound), how every signature unit (method
signature block) is (rule-bound, unbound, ambiguous), and what the checks
found. Plain text, no timestamps, so that two runs on the same input write the
same bytes.

The ledger ('ledgerMarkdown') is also committed: @make gen@ copies it to
@docs/LEDGER.md@ and @make gen-diff@ keeps it equal to what the committed
sources give. It holds the totals (tables per binding, per rule, findings per
code), so that the tests can hold invariants instead and a new rule edits no
generic test.
-}
module Hsed.Spec.Report
  ( Ledger (..)
  , ledger
  , renderLedger
  , ledgerMarkdown
  , coverageMarkdown
  , findingsMarkdown
  , orphans
  , findingCounts
  ) where

import qualified Data.Text        as DT
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Anchor (AnchorId, renderAnchorId)
import           Hsed.Spec.Common (ErrataSet, Finding, RuleName (..), Severity (..), SpecId (..), errataSetName,
                                   renderFinding)
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.DSL    (someBindingShape)
import           Hsed.Spec.IR     (SigIR (..), SpecIR (..), TableIR (..))
import           Hsed.Spec.RuleDef (signatureUnits)
import           Hsed.Spec.Rules
import           Hsed.Spec.Shape  (captionTitle, shapeName)


-- | How the numbered tables of one spec are bound.
data Ledger = Ledger
  { lSpec         :: !SpecId
  , lTables       :: !Int
  , lRuleBound    :: !Int   -- ^ bound by a rule, with or without a bare modifier comment
  , lByModifiers  :: !Int   -- ^ of those, through a bare modifier comment
  , lCommentBound :: !Int   -- ^ bound by a full binding in a comment
  , lAmbiguous    :: !Int
  , lInvalid      :: !Int
  , lUnbound      :: !Int
  , lByBinding    :: ![(Text, Int)]  -- ^ the bound tables by what binds them: @rule typeRow@, @comment EnumValues@
  , lSigs         :: !Int   -- ^ the signature units
  , lSigsBound    :: !Int   -- ^ of those, bound by a rule
  , lSigsUnbound  :: !Int
  , lSigsAmbiguous :: !Int
  }
  deriving (Eq, Show)

ledger :: SpecIR -> Ledger
ledger ir = Ledger
  { lSpec         = SpecId ir.spec
  , lTables       = Map.size classes
  , lRuleBound    = count (\case RuleBound {} -> True; _ -> False)
  , lByModifiers  = count (\case RuleBound _ (_ : _) _ -> True; _ -> False)
  , lCommentBound = count (\case CommentBound _ -> True; _ -> False)
  , lAmbiguous    = count (\case Ambiguous _ -> True; _ -> False)
  , lInvalid      = count (\case Invalid _ -> True; _ -> False)
  , lUnbound      = count (\case Unbound -> True; _ -> False)
  , lByBinding    = Map.toList (Map.fromListWith (+) [ (b, 1) | c <- Map.elems classes, Just b <- [binder c] ])
  , lSigs         = Map.size sigClasses
  , lSigsBound    = countSigs (\case RuleBound {} -> True; _ -> False)
  , lSigsUnbound  = countSigs (\case Unbound -> True; _ -> False)
  , lSigsAmbiguous = countSigs (\case Ambiguous _ -> True; _ -> False)
  }
  where
    classes = classify ir
    sigClasses = fst (classifySignatures ir)
    count p = length (filter p (Map.elems classes))
    countSigs p = length (filter p (Map.elems sigClasses))
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
      , tshow (lSigsUnbound l), " unbound, ", tshow (lSigsAmbiguous l), " ambiguous" ]

-- | The ledger of @hsedc compile@ (@build/report/ledger.md@, committed as
--   @docs/LEDGER.md@): how each numbered table is bound, what binds the bound
--   ones, and the findings of the run by code, for each spec with its findings.
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
  <> concatMap perSpec ls
  where
    perSpec (l, fs) =
      [ "", "## " <> specIdText (lSpec l) <> ": what binds the tables", ""
      , "| binding | tables |", "|---|---:|" ]
      <> [ "| " <> b <> " | " <> tshow n <> " |" | (b, n) <- lByBinding l ]
      <> [ "", "## " <> specIdText (lSpec l) <> ": findings under --errata " <> errataSetName es, ""
         , "| severity | code | findings |", "|---|---|---:|" ]
      <> [ "| " <> tshow sev <> " | " <> code <> " | " <> tshow n <> " |"
         | ((sev, code), n) <- Map.toList (Map.fromListWith (+) [ ((Cm.severity f, Cm.code f), 1 :: Int) | f <- fs ]) ]

-- | One row per numbered table: anchor, caption title, PDF page, binding; then
--   one per signature unit: anchor, heads, PDF page, binding.
coverageMarkdown :: SpecIR -> Text
coverageMarkdown ir = T.unlines $
  [ "# Coverage: " <> ir.spec
  , ""
  , "Generated by `hsedc compile`: every numbered table and every method signature block, and what binds it."
  , ""
  , "| table | caption | PDF page | binding |"
  , "|---|---|---:|---|" ]
  <> [ "| " <> T.intercalate " | " [renderAnchorId aid, cell (title t), maybe "" tshow (tablePage t), cell (describeClassification c)] <> " |"
     | (aid, c) <- Map.toList (classify ir), Just t <- [Map.lookup aid tables] ]
  <> [ "", "| signature block | heads | PDF page | binding |", "|---|---|---:|---|" ]
  <> [ "| " <> T.intercalate " | " [renderAnchorId aid, cell (T.intercalate ", " s.heads), tshow (s.page + 1), cell (describeClassification c)] <> " |"
     | (aid, c) <- Map.toList (fst (classifySignatures ir)), Just s <- [Map.lookup aid (signatureUnits ir)] ]
  where
    tables  = numberedTables ir
    title :: TableIR -> Text
    title t = fromMaybe "" (t.caption >>= captionTitle)
    cell    = DT.replace "|" "\\|"

-- | The findings of a run, errors first, each in 'renderFinding' form.
findingsMarkdown :: SpecId -> ErrataSet -> [Finding] -> Text
findingsMarkdown sid es fs = T.unlines $
  [ "# Findings: " <> specIdText sid <> ", errata " <> errataSetName es
  , ""
  , "Generated by `hsedc compile`. " <> findingCounts fs <> "." ]
  <> concatMap section [(Error, "Errors"), (Warning, "Warnings"), (Note, "Notes")]
  where
    section (sev, heading) = case filter ((== sev) . Cm.severity) fs of
      []  -> []
      sel -> ["", "## " <> heading <> " (" <> tshow (length sel) <> ")", "", "```"] <> map renderFinding sel <> ["```"]

-- | @2 errors, 130 warnings, 0 notes@
findingCounts :: [Finding] -> Text
findingCounts fs = T.intercalate ", " [ tshow (n sev) <> " " <> w | (sev, w) <- [(Error, "errors"), (Warning, "warnings"), (Note, "notes")] ]
  where n sev = length (filter ((== sev) . Cm.severity) fs)

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
