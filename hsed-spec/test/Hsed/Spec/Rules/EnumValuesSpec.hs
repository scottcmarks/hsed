{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.Rules.EnumValuesSpec
Description : Test specifications for the enumValues rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The value cells an enumeration reads and the constructor names it prints; then
the rule (design example E4) on the committed Core extraction with the
committed annotations: the 19 enumerations of section 5.1.3 it binds, T87
through its comment, the 20 it declares, a range printed with a spaced en dash
read as one, two labels told apart as their names are, C-ENUM-RANGE made to
fail by one edited cell (a value outside the range, a value cell that is no
number, a constructor defined twice, two labels giving one constructor, a
label giving the one the gaps take, a label with nothing to name a
constructor), the overlap and repeated-label Warnings, and the declarations
of T52, T87, T101 and T126. Then Opal under the M4b sidecar, read over Core:
Table 49's enumeration in its Description column, its range its own Format's
(C-ENUM-RANGE made to fail by narrowing it), held to the Core tables it
replaces by C-PATCH-UID (made to fail by another UID, another type's name,
another Format since the M4b review, other targets, or no base), its own UID cell eight bytes (C-UID-TABLE);
Tables 31 and 49 each one W-ENUM-LABEL-REPEATED; and @Hsed.Gen.Opal.Enums@:
@Opal_life_cycle_state@'s 16 constructors (manufactured-inactive 8,
manufactured 9), its UID Core T102's imported by name, and
@Opal_data_removal_mechanism@'s 8, its UID Opal T30's. Then Opal under the
M5a sidecar: Table 16 (@reset_types@, PDF 32), bound @replaces [core
128]@, an enumeration as Core's Table 128 is (PDF 119), its type its
caption's identifier, its range Core Table 127's 0..31, held to Core's
Table 128 by C-PATCH-UID (made to fail by another target); generated as
@Opal_reset_types@, 3 Programmatic where @Core_reset_types@ keeps 3 Reserved.
-}
module Hsed.Spec.Rules.EnumValuesSpec (spec) where

import qualified Data.Text                  as DT
import qualified RIO.Map                    as Map
import qualified RIO.Text                   as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Types            (Operand (..), TypeDef (..))
import           Hsed.Spec.Anchor           (AnchorId (..))
import           Hsed.Spec.Check            (checkIR, mkContext)
import           Hsed.Spec.Common           (ErrataSet (..), Finding, RuleName (..), isError, renderFinding)
import qualified Hsed.Spec.Common           as Cm
import           Hsed.Spec.Compile          (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL              (SomeBinding (..), cols, enumValues, rangeWords)
import           Hsed.Spec.IR               (SpecIR)
import           Hsed.Spec.RuleDef          (Classification (..), Context (..), numberedTables)
import           Hsed.Spec.Rules            (Section (..), classify, scopeOf)
import           Hsed.Spec.Rules.EnumValues
import           Hsed.Spec.Rules.TypeRow    (TypeRowRead (..), typeRowReads)
import           Hsed.Spec.Shape            (measure)


-- | The enumerations a context declares.
enumsOf :: Context -> [EnumDecl]
enumsOf ctx = rights [ enumDecl ctx er (checkEnum ctx rows er) | er <- rights (enumReads ctx) ]
  where rows = rights (typeRowReads ctx)

about :: Text -> [Finding] -> [Text]
about anchor = map renderFinding . filter ((== Just anchor) . Cm.anchor)

errorsOf :: ErrataSet -> SpecIR -> [Text]
errorsOf es = map renderFinding . filter isError . checkIR Nothing es

spec :: Spec
spec = do
  describe "parseEnumValue" $
    it "reads a value, a dash or en-dash range spaced or not, and a declared range word" $ do
      parseEnumValue [] "2" `shouldBe` Just [2]
      parseEnumValue [] "2-7" `shouldBe` Just [2 .. 7]
      parseEnumValue [] "14–15" `shouldBe` Just [14, 15]
      parseEnumValue [] "3 – 4" `shouldBe` Just [3, 4]
      parseEnumValue [] "3 - 4" `shouldBe` Just [3, 4]
      parseEnumValue [] "3 —4" `shouldBe` Just [3, 4]
      parseEnumValue ["–"] "3 – 4" `shouldBe` Just [3, 4]
      parseEnumValue [] "7 to 31" `shouldBe` Nothing
      parseEnumValue ["to"] "7 to 31" `shouldBe` Just [7 .. 31]
      parseEnumValue [] "7-2" `shouldBe` Nothing
      parseEnumValue [] "3 – – 4" `shouldBe` Nothing

  describe "descriptionItems" $ do
    it "reads the n = label items of a Description cell's lines, the sentence before them dropped" $
      descriptionItems ["Used to represent the current life", "cycle state. The valid values", "are:", "0 = issued,", "1 = issued-disabled,", "5-7 = reserved,"]
        `shouldBe` [("0", "issued"), ("1", "issued-disabled"), ("5-7", "reserved")]
    it "joins a line ending in a hyphen to the next with no space (Opal T49's 12 = manufactured-disabled- frozen, PDF 98)" $
      descriptionItems ["12 = manufactured-disabled-", "frozen,", "13 = manufactured-failed,", "14-15 = reserved"]
        `shouldBe` [("12", "manufactured-disabled-frozen"), ("13", "manufactured-failed"), ("14-15", "reserved")]
    it "keeps a range printed with an en dash, and reads no item from a piece without one" $
      descriptionItems ["3 \x2013 4 = reserved, and more"] `shouldBe` [("3 \x2013 4", "reserved")]

  describe "enumTypeName" $
    it "prefixes the type's name with the spec's label" $ do
      enumTypeName core "adv_key_mode" `shouldBe` "Core_adv_key_mode"
      enumTypeName (Cm.SpecId "opal-2.30") "life_cycle_state" `shouldBe` "Opal_life_cycle_state"

  describe "constructorNames" $ do
    it "names every value of a label that names several with the value, the first included, and a single-valued label bare (design 13.15)" $
      map fst (constructorNames "Core_adv_key_mode" (0, 7) advKeyMode)
        `shouldBe` [ "Core_adv_key_mode_Wait_for_AdvKey_Req", "Core_adv_key_mode_Auto_advance_keys", "Core_adv_key_mode_Reserved_2"
                   , "Core_adv_key_mode_Reserved_3", "Core_adv_key_mode_Reserved_4", "Core_adv_key_mode_Reserved_5"
                   , "Core_adv_key_mode_Reserved_6", "Core_adv_key_mode_Reserved_7" ]
    it "treats a repeated label as one label (padding_type's two None rows)" $
      take 2 (constructorNames "Core_padding_type" (0, 15) paddingType)
        `shouldBe` [("Core_padding_type_None_0", 0), ("Core_padding_type_None_1", 1)]
    it "keeps a label's values in table order across its rows (life_cycle_state's Unassigned)" $
      filter (T.isInfixOf "Unassigned" . fst) (constructorNames "Core_life_cycle_state" (0, 15) lifeCycleState)
        `shouldBe` [ ("Core_life_cycle_state_Unassigned_5", 5), ("Core_life_cycle_state_Unassigned_6", 6), ("Core_life_cycle_state_Unassigned_7", 7)
                   , ("Core_life_cycle_state_Unassigned_14", 14), ("Core_life_cycle_state_Unassigned_15", 15) ]
    it "fills the gaps of the range with _NA from the range's lower bound, each suffixed when there are several" $ do
      constructorNames "Core_x" (1, 4) [("Idle", 1), ("Active", 3)]
        `shouldBe` [("Core_x_Idle", 1), ("Core_x_NA_2", 2), ("Core_x_Active", 3), ("Core_x_NA_4", 4)]
      constructorNames "Core_x" (0, 3) [("A", 2)]
        `shouldBe` [("Core_x_NA_0", 0), ("Core_x_NA_1", 1), ("Core_x_A", 2), ("Core_x_NA_3", 3)]
      constructorNames "Core_x" (0, 2) [("A", 0), ("B", 2)]
        `shouldBe` [("Core_x_A", 0), ("Core_x_NA", 1), ("Core_x_B", 2)]
      map fst (constructorNames "Core_reencrypt_state" (1, 16) reencryptState) `shouldSatisfy` notElem "Core_reencrypt_state_NA"
      map snd (constructorNames "Core_reencrypt_state" (1, 16) reencryptState) `shouldBe` [1 .. 16]
    it "scrubs a label to an identifier: commas dropped, spaces and dashes to underscores" $
      map fst (constructorNames "Core_e" (0, 3) [("RSAES-PKCS1-v1_5", 0), ("SHA 1", 1), ("Issued-Disabled-Frozen", 2), ("ReadLocked, WriteLocked or (none)", 3)])
        `shouldBe` ["Core_e_RSAES_PKCS1_v1_5", "Core_e_SHA_1", "Core_e_Issued_Disabled_Frozen", "Core_e_ReadLocked_WriteLocked_or__none_"]
    it "keeps an apostrophe, which a name may hold" $
      map fst (constructorNames "Core_e" (0, 1) [("Don't", 0), ("Don_t", 1)]) `shouldBe` ["Core_e_Don't", "Core_e_Don_t"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx       = mkContext Scott ir
        scott     = checkIR Nothing Scott ir
        published = checkIR Nothing Published ir
        compiled ir' = either (throwString . show) pure (compileIR Nothing Scott ir')

    describe "the enumValues rule" $ do
      it "binds the 19 enumerations of section 5.1.3, from the rule anchor there, and T87 through its comment" $ do
        scopeOf ir (RuleName "enumValues") `shouldBe` [Subtree "5.1.3"]
        length [ () | RuleBound (RuleName "enumValues") _ _ <- Map.elems (classify ir) ] `shouldBe` 19
        Map.lookup (TableId 87) (classify ir) `shouldBe` Just (CommentBound (SomeBinding (rangeWords ["to"] (cols 0 1 enumValues))))
      it "names the type an enumeration's caption names; no other caption" $ do
        let ident n = enumIdent . measure core =<< Map.lookup (TableId n) (numberedTables ir)
        map ident [52, 54, 46, 171] `shouldBe` [Just "adv_key_mode", Just "attr_flags", Nothing, Nothing]
      it "declares the 20, leaving none out" $
        length (enumsOf ctx) `shouldBe` 20

    describe "C-ENUM-RANGE" $ do
      it "passes for T51/T52 (M1 acceptance (d))" $ do
        about "T51" scott `shouldBe` []
        about "T52" scott `shouldBe` []
      it "reads a range printed with a spaced en dash as the hyphen's (Opal T31's 3 – 4)" $ do
        let ir' = setCell "T52" 2 0 "2 – 7" ir
        errorsOf Scott ir' `shouldBe` []
        map (.ctors) (enumsOf (mkContext Scott ir')) `shouldBe` map (.ctors) (enumsOf ctx)
      it "fails on a value outside the type's range" $
        errorsOf Scott (setCell "T52" 2 0 "2-8" ir) `shouldBe`
          ["C-ENUM-RANGE core-2.01.pdf#page=102&T52: value 8 (\"Reserved\") lies outside 0..7 of T51 adv_key_mode"]
      it "fails on a value cell that is not a number" $
        errorsOf Scott (setCell "T52" 0 0 "zero" ir) `shouldBe`
          ["C-ENUM-RANGE core-2.01.pdf#page=102&T52: the value cell \"zero\" is not a number or a range"]
      it "fails on a constructor defined twice" $
        errorsOf Scott (setCell "T52" 1 1 "Reserved 3" ir) `shouldBe`
          ["C-ENUM-RANGE core-2.01.pdf#page=102&T52: the constructor Core_adv_key_mode_Reserved_3 would be defined twice"]
      it "fails on two labels that give one constructor" $
        errorsOf Scott (setCell "T52" 1 1 "Wait-for AdvKey_Req" ir) `shouldSatisfy`
          any ("C-ENUM-RANGE core-2.01.pdf#page=102&T52: the labels \"Wait for AdvKey_Req\" and \"Wait-for AdvKey_Req\" give the same constructor" ==)
      it "passes two labels that give two constructors, as the names are scrubbed (an apostrophe kept)" $
        filter ("C-ENUM-RANGE" `T.isPrefixOf`) (errorsOf Scott (setCell "T52" 0 1 "Don't" (setCell "T52" 1 1 "Don_t" ir))) `shouldBe` []
      it "fails on a label that gives the constructor the gaps of the range take (NA)" $
        errorsOf Scott (setCell "T52" 0 1 "NA" (setCell "T52" 2 0 "2-6" ir)) `shouldSatisfy`
          elem "C-ENUM-RANGE core-2.01.pdf#page=102&T52: the label \"NA\" and the gaps of the range (_NA) give the same constructor"
      it "fails on a label with no letter or digit, an erratum's empty correction included, and generates nothing for it" $
        forM_ (["", "+"] :: [Text]) $ \label -> do
          ir' <- annotated pristine (hsa <> "core-2.01 | T52 | erratum scott [\"Auto-advance keys\" ~> " <> tshow label <> "]\n")
          errorsOf Scott ir' `shouldBe`
            ["C-ENUM-RANGE core-2.01.pdf#page=102&T52: the value \"1\" has the label " <> tshow label <> ", which has no letter or digit to name a constructor"]
          newlyExcluded ir ir' `shouldReturn` [(TableId 52, "C-ENUM-RANGE failed")]
      it "fails on a cols binding that points at a sparse column (T87's third)" $ do
        ir' <- annotated pristine (withoutLine "T87" hsa <> "core-2.01 | T87 | rangeWords [\"to\"] (cols 0 2 enumValues)\n")
        let errs = errorsOf Scott ir'
        errs `shouldSatisfy` (not . null)
        -- each at the page its row is printed on: T87 runs over PDF 109 and 110
        errs `shouldSatisfy` all (\x -> any (`T.isPrefixOf` x) [ "C-ENUM-RANGE core-2.01.pdf#page=" <> p <> "&T87: the value " | p <- ["109", "110"] ])
        errs `shouldSatisfy` elem "C-ENUM-RANGE core-2.01.pdf#page=110&T87: the value \"34-63\" has the label \"\", which has no letter or digit to name a constructor"
        errs `shouldSatisfy` all ("has the label \"\", which has no letter or digit to name a constructor" `T.isSuffixOf`)
        newlyExcluded ir ir' `shouldReturn` [(TableId 87, "C-ENUM-RANGE failed")]

    describe "the Warnings" $ do
      it "W-ENUM-OVERLAP: a value assigned twice, the first kept" $
        about "T52" (checkIR Nothing Scott (setCell "T52" 1 0 "0" ir)) `shouldBe`
          ["W-ENUM-OVERLAP core-2.01.pdf#page=102&T52: value 0 is assigned by \"Wait for AdvKey_Req\" and \"Auto-advance keys\"; the first is kept"]
      it "the T87 comment of design E4: rangeWords, cols, an overlap and a repeated label, under either errata set, each at the page of its first row (PDF 110; the table starts on 109)" $
        forM_ [scott, published] $ \fs ->
          about "T87" fs `shouldBe`
            [ "W-ENUM-LABEL-REPEATED core-2.01.pdf#page=110&T87: the label \"reserved\" is used by 2 rows; each of its values gets a suffixed constructor"
            , "W-ENUM-OVERLAP core-2.01.pdf#page=110&T87: value 34 is assigned by \"active_reset_stop_detect\" and \"reserved\"; the first is kept" ]
      it "W-ENUM-LABEL-REPEATED on T87, T103 and T121, under either errata set" $
        forM_ [scott, published] $ \fs ->
          [ a | f <- fs, Cm.code f == "W-ENUM-LABEL-REPEATED", Just a <- [Cm.anchor f] ] `shouldBe` ["T87", "T103", "T121"]

    describe "the declarations" $ do
      it "read T52 as design example E4 does, with the range of T51" $
        [ (e.ctors, e.range, e.typeUID) | e <- enumsOf ctx, e.name == "adv_key_mode" ]
          `shouldBe` [([("Wait for AdvKey_Req", 0), ("Auto-advance keys", 1)] <> [ ("Reserved", v) | v <- [2 .. 7] ], (0, 7), [0, 0, 0, 5, 0, 0, 4, 0x0F])]
      it "name T101's value as its annotation corrects it under scott, and as printed under published" $ do
        [ e.ctors | e <- enumsOf ctx, e.name == "last_reenc_stat" ] `shouldSatisfy` all (elem ("Verify Error", 3))
        [ e.ctors | e <- enumsOf (mkContext Published ir), e.name == "last_reenc_stat" ] `shouldSatisfy` all (elem ("Verify Eror", 3))
      it "read T87 through its comment: two columns, \"7 to 31\", value 34 kept for its first label (design E4)" $ do
        let gen = [ e | e <- enumsOf ctx, e.name == "gen_status" ]
        map (.range) gen `shouldBe` [(0, 63)]
        [ map snd e.ctors | e <- gen ] `shouldBe` [[0 .. 63]]
        [ lookup 34 [ (v, l) | (l, v) <- e.ctors ] | e <- gen ] `shouldBe` [Just "active_reset_stop_detect"]
        [ lookup 31 [ (v, l) | (l, v) <- e.ctors ] | e <- gen ] `shouldBe` [Just "reserved"]
        [ e.ctors | e <- enumsOf (mkContext Published ir), e.name == "gen_status" ] `shouldBe` map (.ctors) gen
      it "take the range of reencrypt_state from its Format, starting at 1" $
        [ e.range | e <- enumsOf ctx, e.name == "reencrypt_state" ] `shouldBe` [(1, 16)]
      it "carry the spec's label in the data type's name" $ do
        cr <- compiled ir
        T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Enums" (crModules cr))) `shouldSatisfy` elem "data Core_adv_key_mode"
        DT.count "data Core_" (fromMaybe "" (lookup "Hsed.Gen.Core.Enums" (crModules cr))) `shouldBe` 20
  withM4bOpal $ \base opal -> do
    let octx = opalOver base opal
        t49  = [ e | Right e <- enumReads octx, e.erAnchor == TableId 49 ]
        values :: EnumRead -> [(Int, Text)]
        values e = concat [ [ (v, label) | v <- fromMaybe [] (parseEnumValue e.erRangeWords cell) ] | (cell, label) <- e.erRows ]
    describe "an enumeration in a Description column, Opal Table 49 under enumInDescription (M4b sidecar, over Core)" $ do
      it "is bound with the EnumValues shape by its comment, and read from its Description's lines (PDF 98)" $ do
        length t49 `shouldBe` 1
        map (.erIdent) t49 `shouldBe` [Just "life_cycle_state"]
        map (length . (.erRows)) t49 `shouldBe` [13]
        map (.erPages) t49 `shouldSatisfy` all (all (== Just 98))
      it "gives its 16 values, 8 manufactured-inactive, 9 manufactured, 12 manufactured-disabled-frozen, reserved 5-7 and 14-15" $ do
        map (map fst . values) t49 `shouldBe` [[0 .. 15]]
        [ (lookup 8 vs, lookup 9 vs, lookup 12 vs) | e <- t49, let vs = values e ]
          `shouldBe` [(Just "manufactured-inactive", Just "manufactured", Just "manufactured-disabled-frozen")]
        [ [ v | (v, "reserved") <- values e ] | e <- t49 ] `shouldBe` [[5, 6, 7, 14, 15]]
      it "keeps its own UID and Format cells, the type row it modifies (Core T102's UID, C-PATCH-UID's, G3's)" $
        map (.erOwnType) t49 `shouldBe` [Just ("00 00 00 05 00 00 04 05", "Enumeration_Type, 0, 15")]
      it "is read as one row with UID, Name, Format and Description cells, else C-ENUM-RANGE" $
        [ Cm.code f | Left f <- enumReads (opalOver base (setHeaderCell "T49" 0 1 "Title" opal)) ] `shouldBe` ["C-ENUM-RANGE"]

    describe "allEnums, on Opal under the M4b sidecar, over Core" $ do
      let alls = allEnums octx
      it "gives Opal's own first (T31, T49), then Core's whose type Opal's do not name: T49 shadows Core's T103" $ do
        [ (c.cxSpec, e.erAnchor) | (c, e) <- take 2 alls ] `shouldBe` [(Cm.SpecId "opal-2.30", TableId 31), (Cm.SpecId "opal-2.30", TableId 49)]
        [ e.erAnchor | (_, e) <- alls, e.erIdent == Just "life_cycle_state" ] `shouldBe` [TableId 49]
        length alls `shouldBe` 2 + length [ () | Right e <- enumReads base, e.erIdent /= Just "life_cycle_state" ]
        [ c.cxSpec | (c, _) <- drop 2 alls ] `shouldSatisfy` all (== core)
      it "reads Core alone as its own enumerations" $
        map (\(_, e) -> e.erAnchor) (allEnums base) `shouldBe` [ e.erAnchor | Right e <- enumReads base ]

    describe "the checks of Tables 31 and 49, on Opal under the M4b sidecar, over Core" $ do
      let fs = checkIR (Just base) Scott opal
          errorsAt a ir' = [ renderFinding f | f <- checkIR (Just base) Scott ir', isError f, Cm.anchor f == Just a ]
          excludedOf a ir' = either (throwString . show) (\cr -> pure [ why | (aid, why) <- crExcluded cr, aid == a ])
                                    (compileIR (Just base) Scott ir')
      it "find no Error: T49's values lie in its own Format's 0..15, and C-PATCH-UID holds it to Core T102 and T103" $ do
        [ renderFinding f | f <- fs, isError f, Cm.anchor f `elem` [Just "T31", Just "T49"] ] `shouldBe` []
        [ renderFinding f | f <- fs, Cm.code f `elem` ["C-ENUM-RANGE", "C-PATCH-UID"] ] `shouldBe` []
      it "give T31 and T49 one W-ENUM-LABEL-REPEATED each, for their reserved values (PDF 51, 98)" $
        [ renderFinding f | f <- fs, Cm.code f == "W-ENUM-LABEL-REPEATED" ] `shouldBe`
          [ "W-ENUM-LABEL-REPEATED opal-2.30.pdf#page=51&T31: the label \"Reserved\" is used by 2 rows; each of its values gets a suffixed constructor"
          , "W-ENUM-LABEL-REPEATED opal-2.30.pdf#page=98&T49: the label \"reserved\" is used by 2 rows; each of its values gets a suffixed constructor" ]
      it "join T49 to the type row it prints itself, its UID, Name and Format, and T31 to Opal's Table 30" $ do
        [ (tr.trAnchor, tr.trName, tr.trUID, tr.trTypeDef) | e <- t49, Just tr <- [ownTypeRow octx e] ]
          `shouldBe` [(TableId 49, "life_cycle_state", Just [0, 0, 0, 5, 0, 0, 4, 5], Right (EnumType [(Lit 0, Lit 15)]))]
        [ [ tr.trAnchor | tr <- joinable octx e, tr.trIdent == e.erIdent ] | Right e <- enumReads octx, e.erAnchor == TableId 31 ]
          `shouldBe` [[TableId 30]]
      it "C-ENUM-RANGE: T49's values against its own Format, edited to 0..13, not Core T102's, which C-PATCH-UID holds it to" $
        errorsAt "T49" (setCell "T49" 0 2 "Enumeration_Type, 0, 13" opal) `shouldBe`
          [ "C-ENUM-RANGE opal-2.30.pdf#page=98&T49: value 14 (\"reserved\") lies outside 0..13 of T49 life_cycle_state"
          , "C-ENUM-RANGE opal-2.30.pdf#page=98&T49: value 15 (\"reserved\") lies outside 0..13 of T49 life_cycle_state"
          , "C-PATCH-UID opal-2.30.pdf#page=98&T49: the Format \"Enumeration_Type, 0, 13\" is not core-2.01 T102's \"Enumeration_Type, 0, 15\","
            <> " which it replaces: a cell of the type is read by the replaced Format, the enumeration by this one" ]
      it "C-PATCH-UID: T49's Format widened to 0..31, which the cell reader, reading Core T102's, would refuse past 15 (the M4b review)" $ do
        let widened = setCell "T49" 0 2 "Enumeration_Type, 0, 31" opal
        errorsAt "T49" widened `shouldBe`
          [ "C-PATCH-UID opal-2.30.pdf#page=98&T49: the Format \"Enumeration_Type, 0, 31\" is not core-2.01 T102's \"Enumeration_Type, 0, 15\","
            <> " which it replaces: a cell of the type is read by the replaced Format, the enumeration by this one" ]
        excludedOf (TableId 49) widened `shouldReturn` ["C-PATCH-UID failed"]
      it "C-PATCH-UID: T49 printing another UID than Core T102's, which it replaces, is left out" $ do
        let moved = setCell "T49" 0 0 "00 00 00 05 00 00 04 06" opal
        errorsAt "T49" moved `shouldBe`
          [ "C-PATCH-UID opal-2.30.pdf#page=98&T49: the UID 00 00 00 05 00 00 04 06 is not 00 00 00 05 00 00 04 05, that of core-2.01 T102"
            <> " life_cycle_state, which it replaces" ]
        excludedOf (TableId 49) moved `shouldReturn` ["C-PATCH-UID failed"]
      it "C-UID-TABLE: T49's own UID cell not eight bytes, no UID C-PATCH-UID can hold to Core T102's; left out for both" $ do
        let short = setCell "T49" 0 0 "00 00 00 05 00 04 05" opal
        errorsAt "T49" short `shouldBe`
          [ "C-PATCH-UID opal-2.30.pdf#page=98&T49: the UID \"00 00 00 05 00 04 05\" is not 00 00 00 05 00 00 04 05, that of core-2.01 T102"
            <> " life_cycle_state, which it replaces"
          , "C-UID-TABLE opal-2.30.pdf#page=98&T49: the UID cell \"00 00 00 05 00 04 05\" is not eight bytes" ]
        excludedOf (TableId 49) short `shouldReturn` ["C-UID-TABLE and C-PATCH-UID failed"]
      it "C-PATCH-UID: T49 naming another type than Core T102's, which T103 then does not enumerate" $
        errorsAt "T49" (setCell "T49" 0 1 "life_cycle_states" opal) `shouldBe`
          [ "C-PATCH-UID opal-2.30.pdf#page=98&T49: core-2.01 T103, which it replaces, enumerates life_cycle_state, not life_cycle_states"
          , "C-PATCH-UID opal-2.30.pdf#page=98&T49: the type life_cycle_states is not core-2.01 T102's life_cycle_state, which it replaces" ]
      it "C-PATCH-UID: Opal read alone, over no base, has no Core to hold T49 to" $
        [ renderFinding f | f <- checkIR Nothing Scott opal, Cm.code f == "C-PATCH-UID" ] `shouldBe`
          [ "C-PATCH-UID opal-2.30.pdf#page=98&T49: it replaces core 102, in no spec opal-2.30 is read over"
          , "C-PATCH-UID opal-2.30.pdf#page=98&T49: it replaces core 103, in no spec opal-2.30 is read over" ]
      withOpal $ \opalPristine ->
        it "C-PATCH-UID: a target that is no type row or enumeration of Core's, or the enumeration of another type, the table left out" $ do
          let retarget refs = annotated opalPristine (withoutLine "T49" m4bOpalHsa <> "opal-2.30 | T49 | enumInDescription (replaces [" <> refs <> "])\n")
          onT171 <- retarget "core 102, core 171"
          errorsAt "T49" onT171 `shouldBe`
            [ "C-PATCH-UID opal-2.30.pdf#page=98&T49: it replaces core-2.01 T171, which is no type row or enumeration of core-2.01" ]
          excludedOf (TableId 49) onT171 `shouldReturn` ["C-PATCH-UID failed"]
          onT101 <- retarget "core 102, core 101"
          errorsAt "T49" onT101 `shouldBe`
            [ "C-PATCH-UID opal-2.30.pdf#page=98&T49: core-2.01 T101, which it replaces, enumerates last_reenc_stat, not life_cycle_state" ]

    describe "the declarations in Hsed.Gen.Opal.Enums, Opal under the M4b sidecar, over Core" $ do
      cr <- runIO (either (throwString . show) pure (compileIR (Just base) Scott opal))
      let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Opal.Enums" (crModules cr)))
          from l = takeWhile (/= "") (dropWhile (/= l) lns)
          decls = [ d | Right e <- enumReads octx, Right d <- [enumDecl octx e (checkEnum octx (joinable octx e) e)] ]
      it "declare Opal_life_cycle_state's 16 constructors in value order (T49, PDF 98)" $
        from "data Opal_life_cycle_state" `shouldBe`
          [ "data Opal_life_cycle_state"
          , "  = Opal_life_cycle_state_issued"
          , "  | Opal_life_cycle_state_issued_disabled"
          , "  | Opal_life_cycle_state_issued_frozen"
          , "  | Opal_life_cycle_state_issued_disabled_frozen"
          , "  | Opal_life_cycle_state_issued_failed"
          , "  | Opal_life_cycle_state_reserved_5"
          , "  | Opal_life_cycle_state_reserved_6"
          , "  | Opal_life_cycle_state_reserved_7"
          , "  | Opal_life_cycle_state_manufactured_inactive"
          , "  | Opal_life_cycle_state_manufactured"
          , "  | Opal_life_cycle_state_manufactured_disabled"
          , "  | Opal_life_cycle_state_manufactured_frozen"
          , "  | Opal_life_cycle_state_manufactured_disabled_frozen"
          , "  | Opal_life_cycle_state_manufactured_failed"
          , "  | Opal_life_cycle_state_reserved_14"
          , "  | Opal_life_cycle_state_reserved_15"
          , "  deriving (Bounded, Enum, Eq, Ord, Show)" ]
      it "give manufactured-inactive 8 and manufactured 9, the range 0..15 (enumValue: the constructor's place)" $ do
        [ (n, v) | d <- decls, d.name == "life_cycle_state", (n, v) <- constructorNames "Opal_life_cycle_state" d.range d.ctors
                 , n `elem` ["Opal_life_cycle_state_manufactured_inactive", "Opal_life_cycle_state_manufactured"] ]
          `shouldBe` [("Opal_life_cycle_state_manufactured_inactive", 8), ("Opal_life_cycle_state_manufactured", 9)]
        [ (d.range, map snd (constructorNames "Opal_life_cycle_state" d.range d.ctors)) | d <- decls, d.name == "life_cycle_state" ]
          `shouldBe` [((0, 15), [0 .. 15])]
      it "name Core T102's UID, the type row T49 replaces, imported by name from Hsed.Gen.Core.Types" $ do
        from "instance Enumerated Opal_life_cycle_state where"
          `shouldBe` ["instance Enumerated Opal_life_cycle_state where", "  typeUID = ulife_cycle_stateType", "  range   = (0, 15)"]
        lns `shouldContain` ["import           Hsed.Gen.Core.Types (ulife_cycle_stateType)", "import           Hsed.Gen.Opal.Types"]
      it "declare Opal_data_removal_mechanism's 8 constructors (T31, PDF 51), its UID Table 30's from Hsed.Gen.Opal.Types" $ do
        from "data Opal_data_removal_mechanism" `shouldBe`
          [ "data Opal_data_removal_mechanism"
          , "  = Opal_data_removal_mechanism_Overwrite_Data_Erase"
          , "  | Opal_data_removal_mechanism_Block_Erase"
          , "  | Opal_data_removal_mechanism_Cryptographic_Erase"
          , "  | Opal_data_removal_mechanism_Reserved_3"
          , "  | Opal_data_removal_mechanism_Reserved_4"
          , "  | Opal_data_removal_mechanism_Vendor_Specific_Erase"
          , "  | Opal_data_removal_mechanism_Reserved_6"
          , "  | Opal_data_removal_mechanism_Reserved_7"
          , "  deriving (Bounded, Enum, Eq, Ord, Show)" ]
        from "instance Enumerated Opal_data_removal_mechanism where"
          `shouldBe` ["instance Enumerated Opal_data_removal_mechanism where", "  typeUID = udata_removal_mechanismType", "  range   = (0, 7)"]
      it "declare those two and no other" $
        [ l | l <- lns, "data " `T.isPrefixOf` l ] `shouldBe` ["data Opal_data_removal_mechanism", "data Opal_life_cycle_state"]
  withM5aOpal $ \base opal -> do
    let octx = opalOver base opal
        t16  = [ e | Right e <- enumReads octx, e.erAnchor == TableId 16 ]
        -- each declaration from the type rows it joins, Core's among them
        joinedDecls = rights [ enumDecl octx er (checkEnum octx (joinable octx er) er) | er <- t16 ]
        errorsAt a ir' = [ renderFinding f | f <- checkIR (Just base) Scott ir', isError f, Cm.anchor f == Just a ]
    describe "an enumeration replacing a base's, Opal Table 16 under replaces [core 128] (M5a sidecar, over Core)" $ do
      it "names its type by its caption, Table 16 - reset_types (PDF 32), and reads its six rows" $ do
        map (.erIdent) t16 `shouldBe` [Just "reset_types"]
        map (.erRows) t16 `shouldBe`
          [[("0", "Power Cycle"), ("1", "Hardware"), ("2", "HotPlug"), ("3", "Programmatic"), ("4-15", "Reserved"), ("16-31", "Vendor Unique")]]
      it "takes Core Table 127's range and UID (Set_Type, 0, 31; 00 00 00 05 00 00 1A 01, PDF 119), held to Core's Table 128 by C-PATCH-UID" $ do
        [ (d.name, d.typeUID, d.range) | d <- joinedDecls, d.name == "reset_types" ] `shouldBe` [("reset_types", [0, 0, 0, 5, 0, 0, 0x1a, 1], (0, 31))]
        errorsAt "T16" opal `shouldBe` []
      it "gives 3 Programmatic, which Core's Table 128 leaves Reserved, and 32 constructors" $ do
        let opalCtors = concat [ constructorNames "Opal_reset_types" d.range d.ctors | d <- joinedDecls, d.name == "reset_types" ]
        take 5 opalCtors `shouldBe` [ ("Opal_reset_types_Power_Cycle", 0), ("Opal_reset_types_Hardware", 1), ("Opal_reset_types_HotPlug", 2)
                                , ("Opal_reset_types_Programmatic", 3), ("Opal_reset_types_Reserved_4", 4) ]
        length opalCtors `shouldBe` 32
        lookup "Core_reset_types_Reserved_3" (concat [ constructorNames "Core_reset_types" d.range d.ctors | d <- enumsOf base, d.name == "reset_types" ])
          `shouldBe` Just 3
      it "C-PATCH-UID: replacing another enumeration (Core's Table 52, adv_key_mode) is an Error" $ do
        ir' <- annotated opal (DT.replace "replaces [core 128]" "replaces [core 52]" m5aOpalHsa)
        errorsAt "T16" ir' `shouldBe`
          ["C-PATCH-UID opal-2.30.pdf#page=32&T16: core-2.01 T52, which it replaces, enumerates adv_key_mode, not reset_types"]
      it "names no type by its caption when it replaces nothing: bound enumValues alone, C-ENUM-RANGE finds no type row" $ do
        ir' <- annotated opal (DT.replace "replaces [core 128]" "enumValues" m5aOpalHsa)
        errorsAt "T16" ir' `shouldBe` ["C-ENUM-RANGE opal-2.30.pdf#page=32&T16: no type row for the caption's type to take the range from"]
      it "is generated in Hsed.Gen.Opal.Enums, its UID Core's ureset_typesType imported by name" $ do
        cr <- either (throwString . show) pure (compileIR (Just base) Scott opal)
        let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Opal.Enums" (crModules cr)))
        lns `shouldContain` ["data Opal_reset_types", "  = Opal_reset_types_Power_Cycle", "  | Opal_reset_types_Hardware", "  | Opal_reset_types_HotPlug"
                            , "  | Opal_reset_types_Programmatic", "  | Opal_reset_types_Reserved_4"]
        lns `shouldContain` ["instance Enumerated Opal_reset_types where", "  typeUID = ureset_typesType", "  range   = (0, 31)"]
        lns `shouldSatisfy` any (\l -> "import" `T.isPrefixOf` l && "Hsed.Gen.Core.Types" `T.isInfixOf` l && "ureset_typesType" `T.isInfixOf` l)

  where
    advKeyMode     = [("Wait for AdvKey_Req", 0), ("Auto-advance keys", 1)] <> [ ("Reserved", v) | v <- [2 .. 7] ]
    paddingType    = [("None", 0), ("None", 1), ("RSAES-PKCS1-v1_5", 2), ("RSAES-OAEP", 3), ("RSASSA-PKCS1-v1_5", 4)] <> [ ("Reserved", v) | v <- [5 .. 15] ]
    lifeCycleState = [("Issued", 0), ("Issued-Disabled", 1), ("Issued-Frozen", 2), ("Issued-Disabled-Frozen", 3), ("Issued-Failed", 4)]
                     <> [ ("Unassigned", v) | v <- [5 .. 7] ] <> [ ("Reserved for SSC Usage", v) | v <- [8 .. 13] ] <> [ ("Unassigned", v) | v <- [14, 15] ]
    reencryptState = [("Idle", 1), ("Pending", 2), ("Active", 3), ("Completed", 4), ("Paused", 5)] <> [ ("Reserved", v) | v <- [6 .. 16] ]

