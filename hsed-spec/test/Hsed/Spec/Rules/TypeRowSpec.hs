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
the provenance of E2, T46, and T75 under each errata set. 'joinFormat', a
Format cell's text as the hand transcription states it, is tested on its own.
On Opal under the M4b sidecar, read over Core: the Format environment's
table names, Core's Table 240 and Opal's Table 20 row 12 (the lead's answer
13 to the M4b plan; Core's names unchanged, Opal read alone still the
Error), Table 30 declared in @Hsed.Gen.Opal.Types@, and 'allTypeRows'; since
the M4b review, Opal's type rows held to Core's (C-UID-UNIQUE, C-UID-TABLE,
C-TYPE-SHADOW, C-FMT-CYCLE, a Format naming a Core type resolved), and since
its verification a row its binding lets replace a Core type (C-PATCH-UID
holding it to that type's UID and Name) and an unresolved name's hint citing
a Core type with its spec.

The Format column of the hand transcription, the oracle's test fixture
(hsed/fixture/Hand/TypeUIDs.hs, design 5.5), is compared here since M2d
retired the generated Format strings the oracle compared with it: each of its
100 @ttype@ rows' Format, read from the fixture's source, parses to the
TypeDef the generator prints as that row's @t\<Name\>@.
-}
module Hsed.Spec.Rules.TypeRowSpec (spec) where

import qualified Data.Text               as DT
import qualified RIO.Char                as C
import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import qualified RIO.Set                 as Set
import qualified RIO.Text                as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Types         (Operand (..), TypeDef (..), TypeRef (..))
import           Hsed.Spec.Anchor        (AnchorId (..))
import           Hsed.Spec.Check         (checkIR, mkContext)
import           Hsed.Spec.Common        (ErrataSet (..), Finding, RuleName (..), Severity (..), SpecId (..), isError, renderFinding)
import qualified Hsed.Spec.Common        as Cm
import           Hsed.Spec.Compile       (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.Format        (Env (..), mkEnv, parseFormat)
import qualified Hsed.Spec.Gen           as Gen
import           Hsed.Spec.IR            (SpecIR)
import           Hsed.Spec.RuleDef       (Classification (..), Context (..), numberedTables, ownThenBase)
import           Hsed.Spec.Rules         (Section (..), classify, scopeOf)
import           Hsed.Spec.Rules.TableUIDs (tableNames)
import           Hsed.Spec.Rules.TypeRow
import           Hsed.Spec.Shape         (measure)


-- | The type rows a context declares.
rowsOf :: Context -> [TypeRowDecl]
rowsOf ctx = rights [ typeRowDecl ctx tr | tr <- rights (typeRowReads ctx) ]

about :: Text -> [Finding] -> [Text]
about anchor = map renderFinding . filter ((== Just anchor) . Cm.anchor) . filter ownCheck

errorsOf :: ErrataSet -> SpecIR -> [Text]
errorsOf es = map renderFinding . filter isError . filter ownCheck . checkIR Nothing es

-- | The findings of this rule's checks: not the ranges rule's C-UID-RANGE,
--   which an edited UID also fails (its own tests are RangesSpec's).
ownCheck :: Finding -> Bool
ownCheck f = Cm.code f /= "C-UID-RANGE"

-- | Each Type table row of the hand transcription, the oracle's fixture
--   (hsed/fixture/Hand/TypeUIDs.hs): every @ttype@ quasiquote's Name and
--   Format cells, a cell's lines joined, each line's piece stripped and the
--   pieces concatenated (@List_Type,*,ACE_expression@). On this fixture that
--   is the quasiquoter's text, though the quasiquoter cuts a line at the
--   separator's columns, trims trailing space only and drops a final comma:
--   no bar lies off those columns, no piece starts with a space and no Format
--   ends in a comma.
handFormats :: Text -> [(Text, Text)]
handFormats src = [ (typeName, format) | body <- quasiquotes (T.lines src), [_, typeName, format] : _ <- [drop 1 (tableRows body)] ]
  where
    -- a line opening a quasiquote of ttype (spelled in pieces here, so that the opener stays
    -- the fixture's alone, as the M2d acceptance greps it)
    opener = "[" <> "ttype" <> "|"
    quasiquotes ls = case break (opener `T.isPrefixOf`) ls of
      (_, _ : rest) -> let (body, later) = break ("|]" `T.isPrefixOf`) rest in body : quasiquotes later
      _             -> []
    lead = T.stripStart
    tableRows = map joinRow . filter (not . null) . rowGroups . filter (\l -> any (`T.isPrefixOf` lead l) ["|", "+"])
    rowGroups ls = case break (("+" `T.isPrefixOf`) . lead) ls of
      (row, _ : rest) -> row : rowGroups rest
      (row, [])       -> [row]
    joinRow ls = map T.concat (L.transpose [ map T.strip (pieces l) | l <- ls ])
    -- the cells of a line, the blank piece after its last bar dropped
    pieces l = case reverse (T.split (== '|') (T.drop 1 (lead l))) of
      z : zs | T.all C.isSpace z -> reverse zs
      zs                         -> reverse zs

-- | The committed sidecar's T149 line with another subject in its waiver.
waiverFor :: Text -> Text
waiverFor subject = "core-2.01 | T149 | waive \"C-UID-UNIQUE\" \"" <> subject <> "\" \"no erratum says which is wrong\"\n"

spec :: Spec
spec = do
  describe "joinFormat" $
    it "joins the pieces with commas and no spaces, as the fixture's quasiquoter does" $
      map joinFormat [ "List_Type, *, ACE_expression", "Simple_Type,uinteger, 2", "Restricted_Reference_Type{6}, uidref {AuthorityTableUID}", "Base_Type" ]
        `shouldBe` [ "List_Type,*,ACE_expression", "Simple_Type,uinteger,2", "Restricted_Reference_Type{6},uidref {AuthorityTableUID}", "Base_Type" ]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx       = mkContext Scott ir
        scott     = checkIR Nothing Scott ir
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
        fmap (filter ((`elem` typeRows) . fst) . crExcluded) (compileIR Nothing Scott ir) `shouldBe` Right []

    describe "the Format environment" $ do
      it "holds the 100 Type objects of the type-row tables" $
        length (typeEnvFrom ctx) `shouldBe` 100
      it "has no table names to resolve uidref{...} against when tableUIDs binds nothing" $ do
        noToken <- annotated pristine (DT.replace " & rule tableUIDs" "" hsa)
        errorsOf Scott noToken `shouldSatisfy`
          elem "C-FMT-RESOLVE core-2.01: no table bound by tableUIDs (exemplar core-2.01 T240) to resolve uidref{...} against"

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
        let fs = checkIR Nothing Scott ir'
        about "T149" fs `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T94 integer_2 (PDF 111)"
          , "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 09 99\", and the table's UID cell is \"00 00 00 05 00 00 02 15\"" ]
      it "becomes W-WAIVER-UNUSED when the PDF no longer prints the UID twice, and the Error moves with a new clash (ir-uid999)" $ do
        let moved = setCell "T94" 0 0 "00 00 00 05 00 00 09 99" (setCell "T149" 0 0 "00 00 00 05 00 00 09 99" ir)
        about "T149" (checkIR Nothing Scott moved) `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 09 99 of uinteger_2 is also that of T94 integer_2 (PDF 111)"
          , "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 02 15\", and the table's UID cell is \"00 00 00 05 00 00 09 99\"" ]
        let fixed = setCell "T149" 0 0 "00 00 00 05 00 00 02 16" ir
        about "T149" (checkIR Nothing Scott fixed) `shouldBe`
          [ "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 02 15\", and the table's UID cell is \"00 00 00 05 00 00 02 16\"" ]
      it "waives only the clash with the table it names: T149 against T51 stays an Error (ir-partner)" $ do
        let partner = setCell "T94" 0 0 "00 00 00 05 00 00 09 99" (setCell "T51" 0 0 "00 00 00 05 00 00 02 15" ir)
            fs = checkIR Nothing Scott partner
        about "T149" fs `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T51 adv_key_mode (PDF 101)"
          , "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 02 15\", and the type rows with that UID are T51, T149" ]
        map Cm.code (filter isError (filter ownCheck fs)) `shouldBe` ["C-UID-UNIQUE"]
      it "waives nothing when a third table prints the UID too" $ do
        let three = setCell "T51" 0 0 "00 00 00 05 00 00 02 15" ir
        errorsOf Scott three `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=111&T94: the UID 00 00 00 05 00 00 02 15 of integer_2 is also that of T51 adv_key_mode (PDF 101)"
          , "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T51 adv_key_mode (PDF 101)" ]
        about "T149" (checkIR Nothing Scott three) `shouldContain`
          [ "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"T94 00 00 00 05 00 00 02 15\", and the type rows with that UID are T51, T94, T149" ]
      it "waives nothing when its subject does not name the other table" $ do
        ir' <- annotated pristine (withoutLine "T149" hsa <> waiverFor "00 00 00 05 00 00 02 15")
        about "T149" (checkIR Nothing Scott ir') `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=124&T149: the UID 00 00 00 05 00 00 02 15 of uinteger_2 is also that of T94 integer_2 (PDF 111)"
          , "W-WAIVER-UNUSED core-2.01.pdf#page=124&T149: the annotation waives C-UID-UNIQUE for \"00 00 00 05 00 00 02 15\", and that is not a table and a UID (\"T<n> 00 00 00 05 ...\")" ]
      it "waives nothing on the first of the two tables, where no finding is reported" $ do
        ir' <- annotated pristine (withoutLine "T149" hsa <> "core-2.01 | T94 | waive \"C-UID-UNIQUE\" \"T149 00 00 00 05 00 00 02 15\" \"x\"\n")
        let fs = checkIR Nothing Scott ir'
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
        notes (checkIR Nothing Published ir) `shouldBe` notes scott

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
        cr <- either (throwString . show) pure (compileIR Nothing Published ir)
        T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Types" (crModules cr)))
          `shouldSatisfy` elem "-- tclock_time: no TypeDef, the Format did not parse under this errata set"

    describe "the hand transcription's Format column (the oracle's fixture, design 5.5)" $ do
      hand <- runIO (handFormats <$> readFileUtf8 "../hsed/fixture/Hand/TypeUIDs.hs")
      let generated = Map.fromList [ (r.name, r.typeDef) | r <- rowsOf ctx ]
          env       = fst (formatEnv ctx)
      it "gives a Format to each of the 100 type rows, once" $
        L.sort (map fst hand) `shouldBe` Map.keys generated
      it "reads them joined as the quasiquoter joined them (T46)" $
        lookup "AC_element" hand `shouldBe` Just "List_Type,*,ACE_expression"
      it "parses each to the TypeDef generated from the PDF's Format, the row's t<Name>" $
        [ (n, f) | (n, f) <- hand, either (const True) (\d -> Map.lookup n generated /= Just (Just d)) (parseFormat env f) ]
          `shouldBe` []

  withM4bOpal $ \base opal -> do
    let octx = opalOver base opal
        alls = allTypeRows octx
    describe "the Format environment over the base (the lead's answer 13 to the M4b plan)" $ do
      it "gives Core the table names of its own Table 240, as before" $
        formatEnv base `shouldBe` (mkEnv (typeEnvFrom base) (tableNames base), [])
      it "resolves Opal's uidref{...} names against Core's Table 240 and Opal's own Table 20 row 12: no spec-level C-FMT-RESOLVE" $ do
        Set.toList (envTables (fst (formatEnv octx))) `shouldBe` L.sort ("DataRemovalMechanism" : tableNames base)
        snd (formatEnv octx) `shouldBe` []
        [ renderFinding f | f <- checkIR (Just base) Scott opal, Cm.code f == "C-FMT-RESOLVE", isNothing (Cm.anchor f) ] `shouldBe` []
      it "keeps its findings the spec's own: Opal read alone, Table 30 bound and no table of table UIDs, is still the Error" $
        map renderFinding (snd (formatEnv (mkContext Scott opal)))
          `shouldBe` ["C-FMT-RESOLVE opal-2.30: no table bound by tableUIDs (exemplar core-2.01 T240) to resolve uidref{...} against"]

    describe "Table 30, data_removal_mechanism (PDF 51), on Opal under the M4b sidecar" $ do
      cr <- runIO (either (throwString . show) pure (compileIR (Just base) Scott opal))
      let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Opal.Types" (crModules cr)))
      it "is read and checked as Opal's one type row, with no finding" $ do
        map (\tr -> (tr.trName, tr.trUID, joinFormat tr.trFormat)) (rights (typeRowReads octx))
          `shouldBe` [("data_removal_mechanism", Just [0, 0, 0, 5, 0, 0, 4, 0x20], "Enumeration_Type,0,7")]
        about "T30" (checkIR (Just base) Scott opal) `shouldBe` []
      it "is declared in Hsed.Gen.Opal.Types: its UID and its TypeDef, an enumeration 0..7" $ do
        lns `shouldContain` ["udata_removal_mechanismType :: UID", "udata_removal_mechanismType = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x04 0x20"]
        lns `shouldContain` ["tdata_removal_mechanism :: TypeDef", "tdata_removal_mechanism = EnumType [(Lit 0,Lit 7)]"]
        lns `shouldSatisfy` any ("-- | opal-2.30 T30 - data_removal_mechanism Type Table Addition (PDF 51 / p.50, ord 32, sha256:" `T.isPrefixOf`)
        [ l | l <- lns, " :: UID" `T.isSuffixOf` l ] `shouldBe` ["udata_removal_mechanismType :: UID"]

    describe "Opal's type rows held to Core's (the M4b review)" $ do
      let checked ir' = about "T30" (checkIR (Just base) Scott ir')
      it "resolve a Format naming a Core type, as Key Per IO's Alternative_Type, bytes_16, bytes_32 does: no E-FMT-RESOLVE" $ do
        let simple = setCell "T30" 0 2 "Simple_Type, uinteger, 1" opal
        [ tr.trTypeDef | Right tr <- typeRowReads (opalOver base simple) ] `shouldBe` [Right (SimpleType (Ref "uinteger") (Lit 1))]
        checked simple `shouldBe` []
        [ n | (n, _) <- typeEnvFrom octx ] `shouldBe` ["data_removal_mechanism"]
        Map.member "uinteger" (envTypes (fst (formatEnv octx))) `shouldBe` True
      it "C-FMT-CYCLE: a Format naming its own type is an Error, not a read that never ends" $
        checked (setCell "T30" 0 2 "List_Type, 4, data_removal_mechanism" opal) `shouldBe`
          [ "C-FMT-CYCLE opal-2.30.pdf#page=51&T30: the Format of data_removal_mechanism names its own type, through"
            <> " data_removal_mechanism -> data_removal_mechanism" ]
      it "C-UID-UNIQUE: a UID a Core type row gives, Core T58 boolean's" $
        checked (setCell "T30" 0 0 "00 00 00 05 00 00 04 01" opal) `shouldBe`
          [ "C-UID-UNIQUE opal-2.30.pdf#page=51&T30: the UID 00 00 00 05 00 00 04 01 of data_removal_mechanism is also that of core-2.01 T58 boolean (PDF 103)" ]
      it "C-UID-TABLE: a UID outside the Type table's half-UID over Core's type rows too, 00 00 00 05 (Core 3.2.5.3 b.iii, PDF 52)" $
        checked (setCell "T30" 0 0 "00 00 00 06 00 00 04 20" opal) `shouldBe`
          [ "C-UID-TABLE opal-2.30.pdf#page=51&T30: the UID 00 00 00 06 00 00 04 20 of data_removal_mechanism is not a Type object's (upper half 00 00 00 05)" ]
      it "C-TYPE-SHADOW: a name a Core type row gives, which the row's binding does not replace" $
        checked (setCell "T30" 0 1 "boolean" opal) `shouldBe`
          [ "C-TYPE-SHADOW opal-2.30.pdf#page=51&T30: the type boolean is also core-2.01 T58 boolean (PDF 103), which its binding does not"
            <> " replace: the spec's own would shadow it wherever a type is read by name" ]
      it "E-FMT-RESOLVE: the nearest name an unresolved one's hint gives cites its table, a Core type's with its spec, Opal having no T149 (the review's verification)" $ do
        let resolve ir' = L.nub [ f | f <- checked ir', "E-FMT-RESOLVE" `T.isPrefixOf` f ]
        resolve (setCell "T30" 0 2 "List_Type, 4, uintegr_2" opal) `shouldBe`
          [ "E-FMT-RESOLVE opal-2.30.pdf#page=51&T30: unresolved type name uintegr_2 (nearest: uinteger_2, core-2.01 T149)" ]
        resolve (setCell "T30" 0 2 "List_Type, 4, data_removal_mechanisn" opal) `shouldBe`
          [ "E-FMT-RESOLVE opal-2.30.pdf#page=51&T30: unresolved type name data_removal_mechanisn (nearest: data_removal_mechanism, T30)" ]
      it "C-UID-UNIQUE and C-TYPE-SHADOW pass a row restating the Core type its binding replaces, which C-PATCH-UID holds to that type's UID and Name (the review's verification)" $ do
        replacing <- annotated opal (m4bOpalHsa <> "opal-2.30 | T30 | as @'TypeRow (replaces [core 58])\n")
        let held ir' = [ f | f <- checked ir', any (`T.isPrefixOf` f) ["C-UID-UNIQUE", "C-TYPE-SHADOW", "C-PATCH-UID"] ]
            boolean  = setCell "T30" 0 1 "boolean"
            restated = setCell "T30" 0 0 "00 00 00 05 00 00 04 01" (boolean (setCell "T30" 0 2 "Enumeration_Type, 0, 1" replacing))
            uidMoved = "C-PATCH-UID opal-2.30.pdf#page=51&T30: the UID 00 00 00 05 00 00 04 20 is not 00 00 00 05 00 00 04 01, that of"
                       <> " core-2.01 T58 boolean, which it replaces"
        held restated `shouldBe` []
        held (boolean replacing) `shouldBe` [uidMoved]
        held replacing `shouldBe`
          [ uidMoved, "C-PATCH-UID opal-2.30.pdf#page=51&T30: the type data_removal_mechanism is not core-2.01 T58's boolean, which it replaces" ]
      it "find a cycle through other types, the shortest, and none where a name ends in a base type" $ do
        typeCycle (`lookup` [("a", AltType [Ref "b"]), ("b", AltType [Ref "a", Ref "c"]), ("c", BaseType)]) "a" `shouldBe` Just ["a", "b", "a"]
        typeCycle (`lookup` [("a", ListType (Lit 4) (Ref "a"))]) "a" `shouldBe` Just ["a", "a"]
        typeCycle (`lookup` [("a", ListType (Lit 4) (Ref "c")), ("c", BaseType)]) "a" `shouldBe` Nothing

    describe "allTypeRows, on Opal under the M4b sidecar, read over Core (ownThenBase)" $ do
      it "gives Opal's own type row first, Table 30's data_removal_mechanism (PDF 51), then Core's hundred, each with its spec" $ do
        [ (cxSpec c, tr.trAnchor, tr.trName) | (c, tr) <- take 1 alls ] `shouldBe` [(SpecId "opal-2.30", TableId 30, "data_removal_mechanism")]
        length alls `shouldBe` 101
        [ cxSpec c | (c, _) <- drop 1 alls ] `shouldSatisfy` all (== core)
        map ((.trName) . snd) (drop 1 alls) `shouldBe` [ tr.trName | Right tr <- typeRowReads base ]
      it "names each type once, the spec's own first" $
        L.nub (map ((.trName) . snd) alls) `shouldBe` map ((.trName) . snd) alls
      it "reads Core alone as its own type rows" $
        map ((.trAnchor) . snd) (allTypeRows base) `shouldBe` [ tr.trAnchor | Right tr <- typeRowReads base ]
      it "leaves out a base's item whose key an own item gives, and keeps the base's base (ownThenBase)" $ do
        let key = (.trName)
            shadowing = ownThenBase (\c -> if cxSpec c == SpecId "opal-2.30" then [ tr | (_, tr) <- take 2 (allTypeRows base) ] else rights (typeRowReads c)) key octx
        length shadowing `shouldBe` 100
        [ cxSpec c | (c, _) <- take 2 shadowing ] `shouldBe` replicate 2 (SpecId "opal-2.30")
