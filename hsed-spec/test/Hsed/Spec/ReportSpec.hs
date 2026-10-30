{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.ReportSpec
Description : Test specifications for Hsed.Spec.Report: ERRATA.md, coverage, findings and the ledger
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The reports of @hsedc compile@ on the committed Core extraction with the
committed annotations, as invariants: @ERRATA.md@ carries every correction of
the annotations under both errata sets with its page and annotation, every
finding whose code reports the PDF (the acceptance's among them: the
@C-UID-UNIQUE@ waiver, @W-ENUM-OVERLAP@, @XREF-RESOLVES@ at PDF 116) and none
of hsedc's own, names every code the committed run gives in its catalogue
(assembled from the generic modules', the shared readers' and every rule's
codes) at a severity the catalogue gives it, and lists a code the catalogue
does not name apart;
@coverage.md@ has a row for every numbered table, every signature block in
document order and every anchor, each anchor with what it binds;
@findings.md@ counts the findings by code; the ledger counts the List of
Tables entries and table references the document checks read.
-}
module Hsed.Spec.ReportSpec (spec) where

import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Text          as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Check   (mkContext)
import           Hsed.Spec.Common  (ErrataSet (..), RuleName (..), Severity (..), finding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.Compile (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR      (AnnotIR (..), SigIR (..), SpecIR (..))
import           Hsed.Spec.Report
import           Hsed.Spec.RuleDef (Classification (..), Context (..))
import           Hsed.Spec.Rules   (classify)


spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  cr  <- runIO (either (throwString . show) pure (compileIR Nothing Scott ir))
  let fs     = crFindings cr
      ctx    = mkContext Scott ir
      errata = errataMarkdown "build/report/ERRATA.md" ctx fs
      lines' = T.lines errata
      has t  = any (t `T.isInfixOf`) lines'

  describe "ERRATA.md" $ do
    it "carries the correction of each erratum annotation, with its page and annotation, under both sets" $ do
      lines' `shouldContain` ["| T75 clock_time | 107 | 90 | \"Fractoin\" | \"Fraction\" | `core-2.01.pdf#page=107&T75` |"]
      lines' `shouldContain` ["| T101 last_reenc_stat Enumeration Values | 113 | 96 | \"Verify Eror\" | \"Verify Error\" | `core-2.01.pdf#page=113&T101` |"]
      forM_ ["Under `--errata scott`:", "Under `--errata published`:"] $ \l -> lines' `shouldContain` [l]
      lines' `shouldContain`
        ["- `core-2.01.pdf#page=107&T75` (Table 75, printed page 90): Format \"Fractoin\" -> \"Fraction\" (annotation at core-2.01.pdf#page=107&T75)"]
      lines' `shouldContain`
        [ "- `core-2.01.pdf#page=107&T75` (Table 75, printed page 90): published text left as printed; scott: "
          <> "Format \"Fractoin\" -> \"Fraction\" (annotation at core-2.01.pdf#page=107&T75)" ]
    it "lists every finding whose code reports the PDF, under its code, and none of hsedc's own" $ do
      let reports f = case reportsOf (Cm.code f) of
            Just (Defect _)    -> Just True
            Just (AsPrinted _) -> Just True
            Just (Suspect _)   -> Just True
            Just Bookkeeping   -> Just False
            _                  -> Nothing
      [ Cm.message f | f <- fs, reports f == Just True, not (has (Cm.message f)) ] `shouldBe` []
      [ Cm.message f | f <- fs, reports f == Just False, has (Cm.message f) ] `shouldBe` []
      [ Cm.code f | f <- fs, Just True <- [reports f], not (has ("### " <> Cm.code f <> ": ")) ] `shouldBe` []
    it "lists the PDF's defects the acceptance names: Table 179 at PDF 116, the C-UID-UNIQUE waiver, W-ENUM-OVERLAP" $ do
      has "- `core-2.01.pdf#page=116&5.1.3.55` (section 5.1.3.55, printed page 99): \"Table 179\" in \"The enumeration values and their associations defined in Table 179.\" names Secure Column Values" `shouldBe` True
      has "- `core-2.01.pdf#page=124&T149` (Table 149, printed page 107): the UID 00 00 00 05 00 00 02 15 of uinteger_2" `shouldBe` True
      has "waived by its annotation" `shouldBe` True
      has "- `core-2.01.pdf#page=110&T87` (Table 87, printed page 93): value 34 is assigned by" `shouldBe` True
      L.sort [ l | l <- lines', "### " `T.isPrefixOf` l, any (`T.isInfixOf` l) ["XREF-RESOLVES", "C-UID-UNIQUE", "W-ENUM-OVERLAP"] ]
        `shouldBe` L.sort [ "### " <> c <> ": " <> d <> " (" <> tshow n <> ")"
                          | c <- ["XREF-RESOLVES", "C-UID-UNIQUE", "W-ENUM-OVERLAP"], Just (Defect d) <- [reportsOf c]
                          , let n = length (filter ((== c) . Cm.code) fs) ]
    it "names every code the committed run gives in its catalogue, so that nothing is left unclassified" $ do
      L.nub [ Cm.code f | f <- fs, isNothing (reportsOf (Cm.code f)) ] `shouldBe` []
      has "## Not yet classified" `shouldBe` False
    it "gives every finding of the committed run a severity its code's catalogue entry allows" $ do
      let severities = Map.fromList [ (codeName c, codeSeverities c) | c <- codeCatalogue ]
      L.nub [ (Cm.code f, Cm.severity f) | f <- fs, Just sevs <- [Map.lookup (Cm.code f) severities], Cm.severity f `notElem` sevs ]
        `shouldBe` []
    it "lists the findings of a code its catalogue does not name apart, so that none is dropped" $ do
      let new = (finding Warning "W-NOVEL" "something new") { Cm.spec = Just core, Cm.page = Just 40, Cm.anchor = Just "T17" }
          text = errataMarkdown "build/report/ERRATA.md" ctx [new]
      forM_ ["## Not yet classified", "### W-NOVEL (1)", "- `core-2.01.pdf#page=40&T17` (Table 17, printed page 23): something new"] $ \l ->
        T.lines text `shouldContain` [l]
    it "says when no annotation carries a correction" $ do
      bare <- annotated pristine (withoutLine "T75" (withoutLine "T101" hsa))
      T.lines (errataMarkdown "build/report/ERRATA.md" (mkContext Scott bare) []) `shouldContain` ["No annotation carries a correction."]

  describe "coverage.md" $ do
    let cov = T.lines (coverageMarkdown ir)
        rowsFor prefix = [ l | l <- cov, prefix `T.isPrefixOf` l ]
    it "has a row for every numbered table" $
      length [ l | l <- takeWhile (/= "## Signature blocks") cov, "| T" `T.isPrefixOf` l ] `shouldBe` Map.size (classify ir)
    it "has a row for every signature block, in document order" $
      [ T.takeWhile (/= ' ') (T.drop 2 l) | l <- cov, "/sig |" `T.isInfixOf` l ] `shouldBe` [ s.section <> "/sig" | s <- ir.sigs ]
    it "has a row for every anchor, with what it binds" $ do
      forM_ ir.annots $ \a -> rowsFor ("| " <> a.id <> " | " <> tshow a.kind <> " | ") `shouldSatisfy` ((== 1) . length)
      let bound r = length [ () | RuleBound r' _ _ <- Map.elems (classify ir), r' == RuleName r ]
      rowsFor "| 5.1.3 | Rule | " `shouldSatisfy`
        all (("rule typeRow: " <> tshow (bound "typeRow") <> " tables; rule enumValues: " <> tshow (bound "enumValues") <> " tables") `T.isInfixOf`)
      rowsFor "| T75 | Bind | " `shouldSatisfy` all ("rule typeRow, modified: " `T.isInfixOf`)
      rowsFor "| T2 | Bind | " `shouldSatisfy` all ("comment: prose" `T.isInfixOf`)

  describe "findings.md" $
    it "counts the findings by code" $ do
      let md = T.lines (findingsMarkdown core Scott fs)
      forM_ (Map.toList (Map.fromListWith (+) [ ((Cm.severity f, Cm.code f), 1 :: Int) | f <- fs ])) $ \((sev, c), n) ->
        md `shouldContain` ["| " <> tshow sev <> " | " <> c <> " | " <> tshow n <> " |"]

  describe "the ledger" $
    it "counts the List of Tables entries and the table references the document checks read" $ do
      let l = ledger ir
      (lLot l, lXrefs l) `shouldBe` (length ir.lot, length ir.xrefs)
      renderLedger [l] `shouldSatisfy` T.isInfixOf (tshow (length ir.lot) <> " List of Tables entries, " <> tshow (length ir.xrefs) <> " table references")
      cxSpec ctx `shouldBe` lSpec l
