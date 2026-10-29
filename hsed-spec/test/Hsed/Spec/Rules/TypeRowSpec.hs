{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.Rules.TypeRowSpec
Description : Test specifications for the typeRow rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The typeRow rule (design example E2) on the committed Core extraction with the
committed annotations: the 100 type-row tables of section 5.1.3 it binds and
declares, the identifier a caption names, the Format environment, each of its
checks made to fail by one edited cell (C-UID-TABLE, C-UID-UNIQUE and T149's
waiver, C-FMT-RESOLVE, the N-FORMAT-NORMALISED Notes), and its declarations:
the provenance of E2, T46, and T75 under each errata set. The Format string a
type row prints for the M2a comparison is tested on its own.
-}
module Hsed.Spec.Rules.TypeRowSpec (spec) where

import qualified Data.Text               as DT
import qualified RIO.Map                 as Map
import qualified RIO.Text                as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Types         (Operand (..), TypeDef (..), TypeRef (..))
import           Hsed.Spec.Anchor        (AnchorId (..))
import           Hsed.Spec.Check         (checkIR, mkContext)
import           Hsed.Spec.Common        (ErrataSet (..), Finding, RuleName (..), Severity (..), isError, renderFinding)
import qualified Hsed.Spec.Common        as Cm
import           Hsed.Spec.Compile       (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import qualified Hsed.Spec.Gen           as Gen
import           Hsed.Spec.IR            (SpecIR)
import           Hsed.Spec.RuleDef       (Classification (..), Context, numberedTables)
import           Hsed.Spec.Rules         (Section (..), classify, scopeOf)
import           Hsed.Spec.Rules.TypeRow
import           Hsed.Spec.Shape         (measure)


-- | The type rows a context declares.
rowsOf :: Context -> [TypeRowDecl]
rowsOf ctx = rights [ typeRowDecl ctx tr | tr <- rights (typeRowReads ctx) ]

about :: Text -> [Finding] -> [Text]
about anchor = map renderFinding . filter ((== Just anchor) . Cm.anchor) . filter ownCheck

errorsOf :: ErrataSet -> SpecIR -> [Text]
errorsOf es = map renderFinding . filter isError . filter ownCheck . checkIR es

-- | The findings of this rule's checks: not the ranges rule's C-UID-RANGE,
--   which an edited UID also fails (its own tests are RangesSpec's).
ownCheck :: Finding -> Bool
ownCheck f = Cm.code f /= "C-UID-RANGE"

-- | The committed sidecar's T149 line with another subject in its waiver.
waiverFor :: Text -> Text
waiverFor subject = "core-2.01 | T149 | waive \"C-UID-UNIQUE\" \"" <> subject <> "\" \"no erratum says which is wrong\"\n"

spec :: Spec
spec = do
  describe "joinFormat" $
    it "joins the pieces with commas and no spaces, as hsed's quasiquoter does" $
      map joinFormat [ "List_Type, *, ACE_expression", "Simple_Type,uinteger, 2", "Restricted_Reference_Type{6}, uidref {AuthorityTableUID}", "Base_Type" ]
        `shouldBe` [ "List_Type,*,ACE_expression", "Simple_Type,uinteger,2", "Restricted_Reference_Type{6},uidref {AuthorityTableUID}", "Base_Type" ]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx       = mkContext Scott ir
        scott     = checkIR Scott ir
        without a = annotated pristine (withoutLine a hsa)

    describe "the typeRow rule" $ do
      it "binds the 100 type-row tables of section 5.1.3, from the rule anchor there" $ do
        scopeOf ir (RuleName "typeRow") `shouldBe` [Subtree "5.1.3"]
        length [ () | RuleBound (RuleName "typeRow") _ _ <- Map.elems (classify ir) ] `shouldBe` 100
      it "names the type a type row's caption names, normalised; no other caption" $ do
        let ident n = typeIdent . measure core =<< Map.lookup (TableId n) (numberedTables ir)
        map ident [46, 72, 52, 171] `shouldBe` [Just "AC_element", Just "Certificates_object_ref", Nothing, Nothing]
      it "declares every one of them, leaving none out" $ do
        length (rowsOf ctx) `shouldBe` 100
        let typeRows = [ aid | (aid, RuleBound (RuleName "typeRow") _ _) <- Map.toList (classify ir) ]
        fmap (filter ((`elem` typeRows) . fst) . crExcluded) (compileIR Scott ir) `shouldBe` Right []

    describe "the Format environment" $ do
      it "holds the 100 Type objects of the type-row tables" $
        length (typeEnvFrom ctx) `shouldBe` 100
      it "has no table names to resolve uidref{...} against when tableUIDs binds nothing" $ do
        noToken <- annotated pristine (DT.replace " & rule tableUIDs" "" hsa)
        errorsOf Scott noToken `shouldSatisfy`
          elem "C-FMT-RESOLVE core-2.01: no table bound by tableUIDs (T240's header) to resolve uidref{...} against"

    describe "C-UID-TABLE" $ do
      it "passes on the 100 type rows, all in the table of half-UID 00 00 00 05" $
        filter (T.isPrefixOf "C-UID-TABLE") (map renderFinding scott) `shouldBe` []
      it "fails on a UID outside the type rows' table" $
        errorsOf Scott (setCell "T46" 0 0 "00 00 00 06 00 00 08 01" ir) `shouldBe`
          ["C-UID-TABLE core-2.01.pdf#page=100&T46: the UID 00 00 00 06 00 00 08 01 of AC_element is not a Type object's (upper half 00 00 00 05)"]
      it "fails on a UID that is not eight bytes" $
        errorsOf Scott (setCell "T46" 0 0 "00 00 00 05 00 08 01" ir) `shouldBe`
          ["C-UID-TABLE core-2.01.pdf#page=100&T46: the UID cell \"00 00 00 05 00 08 01\" is not eight bytes"]
      it "fails when the type rows' table is no table of Table 240 (its half taken from the PDF, not from the rule)" $
        filter (T.isPrefixOf "C-UID-TABLE") (errorsOf Scott (setCell "T240" 4 1 "00 00 00 55 00 00 00 00" ir)) `shouldBe`
          ["C-UID-TABLE core-2.01.pdf#page=100&T46: the type rows' upper half 00 00 00 05 is the half-UID of no table of those tableUIDs binds"]

    describe "C-UID-UNIQUE" $ do
      it "fails on two type rows with one UID" $
        errorsOf Scott (setCell "T47" 0 0 "00 00 00 05 00 00 08 01" ir) `shouldBe`
          ["C-UID-UNIQUE core-2.01.pdf#page=100&T47: the UID 00 00 00 05 00 00 08 01 of ACE_columns is also that of T46 AC_element (PDF 100)"]
      it "is demoted to a Warning for the duplicate the PDF prints for T94 and T149, by T149's waiver" $ do
        about "T149" scott `shouldBe`
          ["C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T94 integer_2 (PDF 111); waived by its annotation: no erratum says which is wrong"]
        [ Cm.severity f | f <- scott, Cm.anchor f == Just "T149" ] `shouldBe` [Warning]
      it "is the Error again without T149's annotation" $ do
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
        map Cm.code (filter isError (filter ownCheck fs)) `shouldBe` ["C-UID-UNIQUE"]
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

    describe "C-FMT-RESOLVE" $ do
      it "fails on an unknown type name" $
        errorsOf Scott (setCell "T46" 0 2 "List_Type, *, ACE_expresion" ir) `shouldBe`
          ["E-FMT-RESOLVE core-2.01.pdf#page=100&T46: unresolved type name ACE_expresion (nearest: ACE_expression, T48)"]
      it "reports a Format cell's spacing and T76's Table_UID as Notes, the same under both errata sets" $ do
        about "T76" scott `shouldBe`
          [ "N-CAPTION-NORMALISED core-2.01.pdf#page=107&T76: caption \"Column_object _ref\" read as \"Column_object_ref\""
          , "N-FORMAT-NORMALISED core-2.01.pdf#page=107&T76: Format read with \"uidref {ColumnTable_UID}\" as \"uidref{ColumnTable_UID}\", \"ColumnTable_UID\" as \"ColumnTableUID\"" ]
        let notes fs = length [ () | f <- fs, Cm.code f == "N-FORMAT-NORMALISED" ]
        notes scott `shouldBe` 14
        notes (checkIR Published ir) `shouldBe` notes scott

    describe "the declarations" $ do
      it "record the provenance of design example E2, with the manifest's cell hash" $ do
        let p46 = [ r.provenance | r <- rowsOf ctx, r.name == "AC_element" ]
            p75 = [ r.provenance | r <- rowsOf ctx, r.name == "clock_time" ]
        map (\p -> (p.tableId, p.pdfPage1, p.printedPage, p.ordinal, p.structPath)) p46 `shouldBe` [("T46", 100, 83, 48, "0/1288")]
        map (.cellsHash) p75 `shouldBe` ["5a5e8787c29a7124035961d972b7b81ab1635117d2d207a0942f1a9c43291655"]
      it "read T46 as design example E2 does" $
        [ (r.uidBytes, joinFormat r.formatText, r.typeDef) | r <- rowsOf ctx, r.name == "AC_element" ]
          `shouldBe` [([0, 0, 0, 5, 0, 0, 8, 1], "List_Type,*,ACE_expression", Just (ListType Star (Ref "ACE_expression")))]
      it "type T75 as its annotation corrects it under scott" $
        [ r.typeDef | r <- rowsOf ctx, r.name == "clock_time" ]
          `shouldBe` [Just (StructType (map Ref ["Year", "Month", "Day", "Hour", "Minute", "Seconds", "Fraction"]))]
      it "keep Fractoin under published, giving T75 no TypeDef and the module a comment saying so" $ do
        [ (joinFormat r.formatText, r.typeDef) | r <- rowsOf (mkContext Published ir), r.name == "clock_time" ]
          `shouldBe` [("Struct_Type,Year,Month,Day,Hour,Minute,Seconds,Fractoin", Nothing)]
        cr <- either (throwString . show) pure (compileIR Published ir)
        T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Types" (crModules cr)))
          `shouldSatisfy` elem "-- tclock_time: no TypeDef, the Format did not parse under this errata set"
