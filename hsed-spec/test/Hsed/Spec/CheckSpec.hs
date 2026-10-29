{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.CheckSpec
Description : Test specifications for Hsed.Spec.Check
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The generic checks on the committed Core extraction with the committed
annotations, under both errata sets (M1 acceptance (e)): what the
classification found reported, the errata applied, known and unused, the
normalisations as Notes, E-SHAPE, errors first; each per-table annotation
shown to matter (without it the check under scott goes red, or what is read
changes). The tests hold invariants, not totals: how many findings of each
code a run gives is in the committed ledger @docs/LEDGER.md@
("Hsed.Spec.CompileSpec"), and each rule's own checks are tested in
@Rules/\<Rule\>Spec.hs@, so a new rule edits neither this module nor its
counts.
-}
module Hsed.Spec.CheckSpec (spec) where

import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Check
import           Hsed.Spec.Common (ErrataSet (..), Finding, Severity (..), finding, isError, renderFinding)
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR     (SpecIR)
import           Hsed.Spec.RuleDef (Classification (..), Mods (..), boundBinding, modsOf, parseUIDCell)
import           Hsed.Spec.Rules  (classify, classifySignatures, classifyWith)
import           Hsed.Spec.Rules.EnumValues (EnumRead (..), enumReads)


codesOf :: [Finding] -> Map Text Int
codesOf fs = Map.fromListWith (+) [ (Cm.code f, 1) | f <- fs ]

rendered :: [Finding] -> [Text]
rendered = map renderFinding

about :: Text -> [Finding] -> [Text]
about anchor = rendered . filter ((== Just anchor) . Cm.anchor)

errorsOf :: ErrataSet -> SpecIR -> [Text]
errorsOf es = rendered . filter isError . checkIR es

-- | The findings of some codes.
withCodes :: [Text] -> [Finding] -> [Finding]
withCodes cs = filter ((`elem` cs) . Cm.code)

spec :: Spec
spec = do
  describe "parseUIDCell" $
    it "reads sixteen hex digits, whitespace ignored" $ do
      parseUIDCell "00 00 00 05 00 00 08 01" `shouldBe` Just [0, 0, 0, 5, 0, 0, 8, 1]
      parseUIDCell "0000000500000 4 0F" `shouldBe` Just [0, 0, 0, 5, 0, 0, 4, 0x0F]
      parseUIDCell "00 00 00 05 00 00 08" `shouldBe` Nothing
      parseUIDCell "00 00 00 05 00 00 08 0G" `shouldBe` Nothing

  describe "sortFindings" $
    it "orders the anchors of one page naturally: T8, T9, T10" $ do
      let at' a = (finding Warning "W-UNBOUND" "x") { Cm.spec = Nothing, Cm.anchor = Just a, Cm.page = Just 34 }
      map Cm.anchor (sortFindings (map at' ["T10", "T8", "T9"])) `shouldBe` map Just ["T8", "T9", "T10"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let scott     = checkIR Scott ir
        published = checkIR Published ir
        without a = annotated pristine (withoutLine a hsa)
        -- the tables whose binding carries an erratum scott, from the classification
        withErrata = L.sort [ renderAnchorId aid | (aid, c) <- Map.toList (classify ir), Just sb <- [boundBinding c]
                                                 , not (null (mErrata (modsOf sb))) ]

    describe "checkIR under --errata scott" $ do
      it "finds no Error" $
        filter isError scott `shouldBe` []
      it "reports everything the classification found, one W-UNBOUND per unbound table or signature block" $ do
        let (classes, classFindings) = classifyWith ir
            (sigClasses, sigFindings) = classifySignatures ir
        filter (`notElem` scott) (classFindings <> sigFindings) `shouldBe` []
        Map.lookup "W-UNBOUND" (codesOf scott) `shouldBe`
          (let n = length (filter (== Unbound) (Map.elems classes <> Map.elems sigClasses)) in if n == 0 then Nothing else Just n)
      it "applies the corrections of the T75 and T101 annotations, and of no other table" $ do
        about "T75" scott `shouldBe`
          ["W-ERRATUM-APPLIED core-2.01.pdf#page=107&T75: Format \"Fractoin\" -> \"Fraction\" (annotation at core-2.01.pdf#page=107&T75)"]
        about "T101" scott `shouldBe`
          ["W-ERRATUM-APPLIED core-2.01.pdf#page=113&T101: Associated Value \"Verify Eror\" -> \"Verify Error\" (annotation at core-2.01.pdf#page=113&T101)"]
        L.sort (L.nub [ a | f <- withCodes ["W-ERRATUM-APPLIED"] scott, Just a <- [Cm.anchor f] ]) `shouldBe` withErrata
      it "reports each _object _ref caption read as one identifier, as a Note" $ do
        about "T72" scott `shouldContain` ["N-CAPTION-NORMALISED core-2.01.pdf#page=106&T72: caption \"Certificates_object _ref\" read as \"Certificates_object_ref\""]
        L.sort [ a | f <- scott, Cm.code f == "N-CAPTION-NORMALISED", Just a <- [Cm.anchor f] ]
          `shouldBe` L.sort ["T72", "T76", "T112", "T131", "T140", "T142", "T144"]
      it "lists errors first" $
        map Cm.severity scott `shouldBe` L.sort (map Cm.severity scott)

    describe "checkIR under --errata published (acceptance (e))" $ do
      it "leaves Fractoin as printed and fails to resolve it, citing the page" $
        filter ("T75" `T.isInfixOf`) (errorsOf Published ir) `shouldBe`
          ["E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)"]
      it "applies no correction and reports, as known, each defect the annotations record" $ do
        Map.lookup "W-ERRATUM-APPLIED" (codesOf published) `shouldBe` Nothing
        L.sort (L.nub [ a | f <- withCodes ["W-ERRATUM-KNOWN"] published, Just a <- [Cm.anchor f] ]) `shouldBe` withErrata
        length (withCodes ["W-ERRATUM-KNOWN"] published) `shouldBe` length (withCodes ["W-ERRATUM-APPLIED"] scott)
        about "T101" published `shouldBe`
          ["W-ERRATUM-KNOWN core-2.01.pdf#page=113&T101: published text left as printed; scott: Associated Value \"Verify Eror\" -> \"Verify Error\" (annotation at core-2.01.pdf#page=113&T101)"]
      it "reads the normalisations as under scott" $ do
        let notes = rendered . filter ((== Note) . Cm.severity)
        notes published `shouldBe` notes scott

    describe "each per-table annotation matters" $ do
      it "T75: without it, Fractoin fails to resolve under scott too" $ do
        noT75 <- without "T75"
        errorsOf Scott noT75 `shouldBe`
          ["E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)"]
        about "T75" (checkIR Published noT75) `shouldBe` ["E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)"]
      it "T101: without it, nothing corrects Verify Eror and nothing reports it" $ do
        noT101 <- without "T101"
        about "T101" (checkIR Scott noT101) `shouldBe` []
        about "T101" (checkIR Published noT101) `shouldBe` []
      it "T149: without it, the duplicate UID is an Error" $ do
        noT149 <- without "T149"
        map Cm.code (filter isError (checkIR Scott noT149)) `shouldBe` ["C-UID-UNIQUE"]

    describe "the errata" $ do
      it "refuse an erratum of the published set: published is the text as printed" $ do
        ir' <- annotated pristine (withoutLine "T75" hsa <> "core-2.01 | T75 | erratum published [\"Fractoin\" ~> \"Fraction\"]\n")
        forM_ [Scott, Published] $ \es ->
          about "T75" (filter isError (checkIR es ir')) `shouldBe`
            ["E-ERRATUM-PUBLISHED core-2.01.pdf#page=107&T75: 1:9: erratum published: the published set is the text as printed and takes no corrections; write erratum scott"]
      it "apply a chain in order and call none of it unused" $ do
        ir' <- annotated pristine (withoutLine "T75" hsa <> "core-2.01 | T75 | erratum scott [\"Fractoin\" ~> \"Zq\", \"Zq\" ~> \"Fraction\"]\n")
        let fs = checkIR Scott ir'
        filter isError fs `shouldBe` []
        map Cm.code (filter ((== Just "T75") . Cm.anchor) fs) `shouldBe` ["W-ERRATUM-APPLIED", "W-ERRATUM-APPLIED"]
      it "reach every cell: a value cell of an enumeration" $ do
        ir' <- annotated pristine (withoutLine "T101" hsa <> "core-2.01 | T101 | erratum scott [\"4-7\" ~> \"4-6\"]\n")
        about "T101" (checkIR Scott ir') `shouldBe`
          ["W-ERRATUM-APPLIED core-2.01.pdf#page=113&T101: Enumeration Value \"4-7\" -> \"4-6\" (annotation at core-2.01.pdf#page=113&T101)"]
      it "reach the identifier a caption names" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T52 | erratum scott [\"adv_key_mode Enumeration\" ~> \"adv_key_mod Enumeration\"]\n")
        let env = mkContext Scott ir'
        [ erIdent e | Right e <- enumReads env, erAnchor e == TableId 52 ]
          `shouldBe` [Just "adv_key_mod"]
        errorsOf Scott ir' `shouldBe` ["C-ENUM-RANGE core-2.01.pdf#page=102&T52: no type row for adv_key_mod to take the range from"]
      it "report a correction that matches no text of the table" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T46 | erratum scott [\"Nonesuch\" ~> \"X\"]\n")
        forM_ [Scott, Published] $ \es ->
          about "T46" (checkIR es ir') `shouldBe`
            ["W-ERRATUM-UNUSED core-2.01.pdf#page=100&T46: the annotation's correction \"Nonesuch\" -> \"X\" matches no text of the table"]

    describe "the normalisations" $
      it "report a header read through the typo table" $
        about "T46" (checkIR Scott (setHeaderCell "T46" 0 0 "Colum Type" ir)) `shouldContain`
          ["N-HEADER-NORMALISED core-2.01.pdf#page=100&T46: header \"Colum Type\" read as \"Column Type\""]

    describe "E-SHAPE" $
      it "fails on a binding whose shape does not accept the table: a feature comment on a type table" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T46 | feature\n")
        errorsOf Scott ir' `shouldSatisfy` any ("E-SHAPE core-2.01.pdf#page=100&T46: Feature does not accept this table" `T.isPrefixOf`)
