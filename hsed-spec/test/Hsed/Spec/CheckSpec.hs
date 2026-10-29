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

The M1 checks on the committed Core extraction with the committed annotations,
under both errata sets (acceptance (e)); each per-table annotation shown to
matter (without it the check under scott goes red, or what is read changes);
each check made to fail by one edited cell; the waiver of C-UID-UNIQUE; the
errata reaching every cell; the normalisations reported as Notes.
-}
module Hsed.Spec.CheckSpec (spec) where

import qualified Data.Text        as DT
import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Check
import           Hsed.Spec.Common (ErrataSet (..), Finding, Severity (..), finding, isError, renderFinding)
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR     (SpecIR)
import           Hsed.Spec.RuleDef (parseUIDCell)
import           Hsed.Spec.Rules.EnumValues (EnumRead (..), enumReads)
import           Hsed.Spec.Rules.TableUIDs (tableNames)
import           Hsed.Spec.Rules.TypeRow (typeEnvFrom)


codesOf :: [Finding] -> Map Text Int
codesOf fs = Map.fromListWith (+) [ (Cm.code f, 1) | f <- fs ]

rendered :: [Finding] -> [Text]
rendered = map renderFinding

about :: Text -> [Finding] -> [Text]
about anchor = rendered . filter ((== Just anchor) . Cm.anchor)

errorsOf :: ErrataSet -> SpecIR -> [Text]
errorsOf es = rendered . filter isError . checkIR es

-- | The committed sidecar's T149 line with another subject in its waiver.
waiverFor :: Text -> Text
waiverFor subject = "core-2.01 | T149 | waive \"C-UID-UNIQUE\" \"" <> subject <> "\" \"no erratum says which is wrong\"\n"

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

    describe "the Format environment" $ do
      it "holds the 100 Type objects of the type-row tables" $ do
        length (typeEnvFrom (mkContext Scott ir)) `shouldBe` 100
      it "holds the 46 table names of T240, the table the tableUIDs rule binds" $
        length (tableNames (mkContext Scott ir)) `shouldBe` 46
      it "has no table names to resolve uidref{...} against when tableUIDs binds nothing" $ do
        noToken <- annotated pristine (DT.replace "doc core201 & rule tableUIDs" "doc core201" hsa)
        errorsOf Scott noToken `shouldSatisfy`
          elem "C-FMT-RESOLVE core-2.01: no table bound by tableUIDs (T240's header) to resolve uidref{...} against"

    describe "checkIR under --errata scott" $ do
      it "finds no Error" $
        filter isError scott `shouldBe` []
      it "applies the corrections of the T75 and T101 annotations, and no others" $ do
        about "T75" scott `shouldBe`
          ["W-ERRATUM-APPLIED core-2.01.pdf#page=107&T75: Format \"Fractoin\" -> \"Fraction\" (annotation at core-2.01.pdf#page=107&T75)"]
        about "T101" scott `shouldBe`
          ["W-ERRATUM-APPLIED core-2.01.pdf#page=113&T101: Associated Value \"Verify Eror\" -> \"Verify Error\" (annotation at core-2.01.pdf#page=113&T101)"]
        Map.lookup "W-ERRATUM-APPLIED" (codesOf scott) `shouldBe` Just 2
      it "reports the unbound tables, the repeated labels, the waived duplicate UID and the normalisations" $
        codesOf scott `shouldBe` Map.fromList
          [ ("W-ERRATUM-APPLIED", 2), ("W-UNBOUND", 127), ("W-ENUM-LABEL-REPEATED", 3), ("W-ENUM-OVERLAP", 1), ("C-UID-UNIQUE", 1)
          , ("N-CAPTION-NORMALISED", 7), ("N-FORMAT-NORMALISED", 14) ]
      it "demotes the duplicate UID the PDF prints for T94 and T149 to a Warning by T149's waiver" $ do
        about "T149" scott `shouldBe`
          ["C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T94 integer_2 (PDF 111); waived by its annotation: no erratum says which is wrong"]
        [ Cm.severity f | f <- scott, Cm.anchor f == Just "T149" ] `shouldBe` [Warning]
      it "reports each _object _ref caption read as one identifier, as a Note" $ do
        about "T72" scott `shouldContain` ["N-CAPTION-NORMALISED core-2.01.pdf#page=106&T72: caption \"Certificates_object _ref\" read as \"Certificates_object_ref\""]
        L.sort [ a | f <- scott, Cm.code f == "N-CAPTION-NORMALISED", Just a <- [Cm.anchor f] ]
          `shouldBe` L.sort ["T72", "T76", "T112", "T131", "T140", "T142", "T144"]
      it "reports a Format cell's spacing and T76's Table_UID, as Notes" $
        about "T76" scott `shouldBe`
          [ "N-CAPTION-NORMALISED core-2.01.pdf#page=107&T76: caption \"Column_object _ref\" read as \"Column_object_ref\""
          , "N-FORMAT-NORMALISED core-2.01.pdf#page=107&T76: Format read with \"uidref {ColumnTable_UID}\" as \"uidref{ColumnTable_UID}\", \"ColumnTable_UID\" as \"ColumnTableUID\"" ]
      it "passes C-ENUM-RANGE for T51/T52 (acceptance (d))" $ do
        about "T51" scott `shouldBe` []
        about "T52" scott `shouldBe` []
      it "lists errors first" $
        map Cm.severity scott `shouldBe` L.sort (map Cm.severity scott)

    describe "checkIR under --errata published (acceptance (e))" $ do
      it "leaves Fractoin as printed and fails to resolve it, citing the page" $
        errorsOf Published ir `shouldBe`
          ["E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)"]
      it "applies no correction and reports each defect the annotations record" $ do
        Map.lookup "W-ERRATUM-APPLIED" (codesOf published) `shouldBe` Nothing
        Map.lookup "W-ERRATUM-KNOWN" (codesOf published) `shouldBe` Just 2
        about "T101" published `shouldBe`
          ["W-ERRATUM-KNOWN core-2.01.pdf#page=113&T101: published text left as printed; scott: Associated Value \"Verify Eror\" -> \"Verify Error\" (annotation at core-2.01.pdf#page=113&T101)"]
      it "reads the normalisations as under scott" $ do
        Map.lookup "N-CAPTION-NORMALISED" (codesOf published) `shouldBe` Just 7
        Map.lookup "N-FORMAT-NORMALISED" (codesOf published) `shouldBe` Just 14

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
      it "T149: without it, the duplicate UID is the Error C-UID-UNIQUE" $ do
        noT149 <- without "T149"
        errorsOf Scott noT149 `shouldBe`
          ["C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T94 integer_2 (PDF 111)"]

    describe "the waiver" $ do
      it "waives only the UID it records: on another UID it is unused and the Error stands" $ do
        ir' <- annotated pristine (withoutLine "T149" hsa <> waiverFor "T94 00 00 00 05 00 00 09 99")
        let fs = checkIR Scott ir'
        about "T149" fs `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T94 integer_2 (PDF 111)"
          , "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 09 99\", and the table's UID cell is \"00 00 00 05 00 00 02 15\"" ]
      it "becomes W-WAIVER-UNUSED when the PDF no longer prints the UID twice, and the Error moves with a new clash (ir-uid999)" $ do
        let moved = setCell "T94" 0 0 "00 00 00 05 00 00 09 99" (setCell "T149" 0 0 "00 00 00 05 00 00 09 99" ir)
        about "T149" (checkIR Scott moved) `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 09 99 of uinteger_2 is also that of T94 integer_2 (PDF 111)"
          , "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 02 15\", and the table's UID cell is \"00 00 00 05 00 00 09 99\"" ]
        let fixed = setCell "T149" 0 0 "00 00 00 05 00 00 02 16" ir
        about "T149" (checkIR Scott fixed) `shouldBe`
          [ "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 02 15\", and the table's UID cell is \"00 00 00 05 00 00 02 16\"" ]
      it "waives only the clash with the table it names: T149 against T51 stays an Error (ir-partner)" $ do
        let partner = setCell "T94" 0 0 "00 00 00 05 00 00 09 99" (setCell "T51" 0 0 "00 00 00 05 00 00 02 15" ir)
            fs = checkIR Scott partner
        about "T149" fs `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T51 adv_key_mode (PDF 101)"
          , "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 02 15\", and the type rows with that UID are T51, T149" ]
        map Cm.code (filter isError fs) `shouldBe` ["C-UID-UNIQUE"]
      it "waives nothing when a third table prints the UID too" $ do
        let three = setCell "T51" 0 0 "00 00 00 05 00 00 02 15" ir
        errorsOf Scott three `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=111&T94: the UID 00 00 00 05 00 00 02 15 of integer_2 is also that of T51 adv_key_mode (PDF 101)"
          , "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T51 adv_key_mode (PDF 101)" ]
        about "T149" (checkIR Scott three) `shouldContain`
          [ "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 02 15\", and the type rows with that UID are T51, T94, T149" ]
      it "waives nothing when its subject does not name the other table" $ do
        ir' <- annotated pristine (withoutLine "T149" hsa <> waiverFor "00 00 00 05 00 00 02 15")
        about "T149" (checkIR Scott ir') `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T94 integer_2 (PDF 111)"
          , "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"00 00 00 05 00 00 02 15\", and that is not a table and a UID (\"T<n> 00 00 00 05 ...\")" ]
      it "waives nothing on the first of the two tables, where no finding is reported" $ do
        ir' <- annotated pristine (withoutLine "T149" hsa <> "core-2.01 | T94 | waive \"C-UID-UNIQUE\" \"T149 00 00 00 05 00 00 02 15\" \"x\"\n")
        let fs = checkIR Scott ir'
        about "T94" fs `shouldBe`
          ["W-WAIVER-UNUSED core-2.01.pdf#page=111&T94: the annotation waives C-UID-UNIQUE for \"T149 00 00 00 05 00 00 02 15\", and no type row before it has that UID"]
        filter isError fs `shouldSatisfy` any ((== "C-UID-UNIQUE") . Cm.code)
      it "is refused on a table that is not a type row (onEnum.hsa)" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T52 | waive \"C-UID-UNIQUE\" \"T51 00 00 00 05 00 00 08 01\" \"y\"\n")
        errorsOf Scott ir' `shouldBe`
          ["E-MODIFIER core-2.01.pdf#page=102&T52: waive applies to a TypeRow binding, not to a EnumValues one"]

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

    describe "each check, made to fail" $ do
      it "E-SHAPE: a feature comment on a type table" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T46 | feature\n")
        errorsOf Scott ir' `shouldSatisfy` any ("E-SHAPE core-2.01.pdf#page=100&T46: Feature does not accept this table" `T.isPrefixOf`)
      it "C-UID-TABLE: a UID outside the Type table" $
        errorsOf Scott (setCell "T46" 0 0 "00 00 00 06 00 00 08 01" ir) `shouldBe`
          ["C-UID-TABLE core-2.01.pdf#page=100&T46: the UID 00 00 00 06 00 00 08 01 of AC_element is not a Type object's (upper half 00 00 00 05)"]
      it "C-UID-TABLE: a UID that is not eight bytes" $
        errorsOf Scott (setCell "T46" 0 0 "00 00 00 05 00 08 01" ir) `shouldBe`
          ["C-UID-TABLE core-2.01.pdf#page=100&T46: the UID cell \"00 00 00 05 00 08 01\" is not eight bytes"]
      it "C-UID-UNIQUE: two type rows with one UID" $
        errorsOf Scott (setCell "T47" 0 0 "00 00 00 05 00 00 08 01" ir) `shouldBe`
          ["C-UID-UNIQUE core-2.01.pdf#page=100&T47: the UID 00 00 00 05 00 00 08 01 of ACE_columns is also that of T46 AC_element (PDF 100)"]
      it "C-FMT-RESOLVE: an unknown type name" $
        errorsOf Scott (setCell "T46" 0 2 "List_Type, *, ACE_expresion" ir) `shouldBe`
          ["E-FMT-RESOLVE core-2.01.pdf#page=100&T46: unresolved type name ACE_expresion (nearest: ACE_expression, T48)"]
      it "C-ENUM-RANGE: a value outside the type's range" $
        errorsOf Scott (setCell "T52" 2 0 "2-8" ir) `shouldBe`
          ["C-ENUM-RANGE core-2.01.pdf#page=102&T52: value 8 (\"Reserved\") lies outside 0..7 of T51 adv_key_mode"]
      it "C-ENUM-RANGE: a value cell that is not a number" $
        errorsOf Scott (setCell "T52" 0 0 "zero" ir) `shouldBe`
          ["C-ENUM-RANGE core-2.01.pdf#page=102&T52: the value cell \"zero\" is not a number or a range"]
      it "C-ENUM-RANGE: a constructor defined twice" $
        errorsOf Scott (setCell "T52" 1 1 "Reserved 3" ir) `shouldBe`
          ["C-ENUM-RANGE core-2.01.pdf#page=102&T52: the constructor Core_adv_key_mode_Reserved_3 would be defined twice"]
      it "C-ENUM-RANGE: two labels that give one constructor" $
        errorsOf Scott (setCell "T52" 1 1 "Wait-for AdvKey_Req" ir) `shouldSatisfy`
          any ("C-ENUM-RANGE core-2.01.pdf#page=102&T52: the labels \"Wait for AdvKey_Req\" and \"Wait-for AdvKey_Req\" give the same constructor" ==)
      it "W-ENUM-OVERLAP: a value assigned twice, the first kept" $
        about "T52" (checkIR Scott (setCell "T52" 1 0 "0" ir)) `shouldBe`
          ["W-ENUM-OVERLAP core-2.01.pdf#page=102&T52: value 0 is assigned by \"Wait for AdvKey_Req\" and \"Auto-advance keys\"; the first is kept"]
      it "the T87 comment of design E4: rangeWords, cols, an overlap and a repeated label, under either errata set" $
        forM_ [scott, published] $ \fs ->
          about "T87" fs `shouldBe`
            [ "W-ENUM-LABEL-REPEATED core-2.01.pdf#page=109&T87: the label \"reserved\" is used by 2 rows; its later values get suffixed constructors"
            , "W-ENUM-OVERLAP core-2.01.pdf#page=109&T87: value 34 is assigned by \"active_reset_stop_detect\" and \"reserved\"; the first is kept" ]
