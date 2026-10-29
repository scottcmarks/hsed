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
under both errata sets (acceptance (e)), and each check made to fail by one
edited cell.
-}
module Hsed.Spec.CheckSpec (spec) where

import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Check
import           Hsed.Spec.Common (ErrataSet (..), Finding, isError, renderFinding)
import qualified Hsed.Spec.Common as Cm
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR     (SpecIR)


codesOf :: [Finding] -> Map Text Int
codesOf fs = Map.fromListWith (+) [ (Cm.code f, 1) | f <- fs ]

rendered :: [Finding] -> [Text]
rendered = map renderFinding

about :: Text -> [Finding] -> [Text]
about anchor = rendered . filter ((== Just anchor) . Cm.anchor)

errorsOf :: ErrataSet -> SpecIR -> [Text]
errorsOf es = rendered . filter isError . checkIR es

spec :: Spec
spec = do
  describe "parseUIDCell" $
    it "reads sixteen hex digits, whitespace ignored" $ do
      parseUIDCell "00 00 00 05 00 00 08 01" `shouldBe` Just [0, 0, 0, 5, 0, 0, 8, 1]
      parseUIDCell "0000000500000 4 0F" `shouldBe` Just [0, 0, 0, 5, 0, 0, 4, 0x0F]
      parseUIDCell "00 00 00 05 00 00 08" `shouldBe` Nothing
      parseUIDCell "00 00 00 05 00 00 08 0G" `shouldBe` Nothing

  describe "parseEnumValue" $
    it "reads a value, a dash or en-dash range, and a declared range word" $ do
      parseEnumValue [] "2" `shouldBe` Just [2]
      parseEnumValue [] "2-7" `shouldBe` Just [2 .. 7]
      parseEnumValue [] "14–15" `shouldBe` Just [14, 15]
      parseEnumValue [] "7 to 31" `shouldBe` Nothing
      parseEnumValue ["to"] "7 to 31" `shouldBe` Just [7 .. 31]
      parseEnumValue [] "7-2" `shouldBe` Nothing

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let scott     = checkIR Scott ir
        published = checkIR Published ir

    describe "the Format environment" $ do
      it "holds the 100 Type objects of the type-row tables" $ do
        let env = mkCompileEnv Scott ir
        length (typeEnvFrom (ceTables env) (ceClasses env)) `shouldBe` 100
      it "holds the 46 table names of T240" $
        fmap length (tableNamesFrom ir) `shouldBe` Right 46

    describe "checkIR under --errata scott" $ do
      it "finds no Error" $
        filter isError scott `shouldBe` []
      it "applies T75's correction from its annotation, and the Errata module's ten others" $ do
        about "T75" scott `shouldBe`
          ["W-ERRATUM-APPLIED core-2.01.pdf#page=107&T75: Format \"Fractoin\" -> \"Fraction\" (annotation at core-2.01.pdf#page=107&T75)"]
        Map.lookup "W-ERRATUM-APPLIED" (codesOf scott) `shouldBe` Just 11
      it "reports the 129 unbound tables, the two repeated labels and the duplicate UID the PDF prints" $ do
        codesOf scott `shouldBe` Map.fromList
          [("W-ERRATUM-APPLIED", 11), ("W-UNBOUND", 129), ("W-ENUM-LABEL-REPEATED", 2), ("W-UID-DUPLICATE", 1)]
        about "T149" scott `shouldBe`
          ["W-UID-DUPLICATE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T94 integer_2 (PDF 111); the published text prints it twice and no erratum records which is wrong"]
      it "passes C-ENUM-RANGE for T51/T52 (acceptance (d))" $ do
        about "T51" scott `shouldBe` []
        about "T52" scott `shouldBe` []
      it "lists errors first" $
        map Cm.severity scott `shouldBe` L.sort (map Cm.severity scott)

    describe "checkIR under --errata published (acceptance (e))" $ do
      it "leaves Fractoin as printed and fails to resolve it, citing the page" $
        errorsOf Published ir `shouldBe`
          ["E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)"]
      it "applies no correction and reports each known defect" $ do
        Map.lookup "W-ERRATUM-APPLIED" (codesOf published) `shouldBe` Nothing
        Map.lookup "W-ERRATUM-KNOWN" (codesOf published) `shouldBe` Just 11
      it "fails the same way without the T75 annotation, which the Errata module also corrects" $ do
        noT75 <- annotated pristine (T.unlines (filter (not . ("| T75 |" `T.isInfixOf`)) (T.lines hsa)))
        errorsOf Published noT75 `shouldBe` errorsOf Published ir
        about "T75" (checkIR Scott noT75) `shouldBe`
          ["W-ERRATUM-APPLIED core-2.01.pdf#page=107&T75: Format \"Fractoin\" -> \"Fraction\" (tcg/Core Spec errata.txt: Table 75 \"Fractoin\" should be \"Fraction\")"]

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
      it "W-ERRATUM-UNUSED: an annotation's correction that matches nothing" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T46 | erratum scott [\"Nonesuch\" ~> \"X\"]\n")
        about "T46" (checkIR Scott ir') `shouldBe`
          ["W-ERRATUM-UNUSED core-2.01.pdf#page=100&T46: the annotation's correction \"Nonesuch\" -> \"X\" matches no text of the table"]
      it "the T87 comment of design E4: rangeWords, cols, an overlap and a repeated label" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T87 | rangeWords [\"to\"] (cols 0 1 enumValues)\n")
        about "T87" (checkIR Scott ir') `shouldBe`
          [ "W-ENUM-LABEL-REPEATED core-2.01.pdf#page=109&T87: the label \"reserved\" is used by 2 rows; its later values get suffixed constructors"
          , "W-ENUM-OVERLAP core-2.01.pdf#page=109&T87: value 34 is assigned by \"active_reset_stop_detect\" and \"reserved\"; the first is kept" ]
