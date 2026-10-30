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
of T52, T87, T101 and T126.
-}
module Hsed.Spec.Rules.EnumValuesSpec (spec) where

import qualified Data.Text                  as DT
import qualified RIO.Map                    as Map
import qualified RIO.Text                   as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor           (AnchorId (..))
import           Hsed.Spec.Check            (checkIR, mkContext)
import           Hsed.Spec.Common           (ErrataSet (..), Finding, RuleName (..), isError, renderFinding)
import qualified Hsed.Spec.Common           as Cm
import           Hsed.Spec.Compile          (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL              (SomeBinding (..), cols, enumValues, rangeWords)
import           Hsed.Spec.IR               (SpecIR)
import           Hsed.Spec.RuleDef          (Classification (..), Context, numberedTables)
import           Hsed.Spec.Rules            (Section (..), classify, scopeOf)
import           Hsed.Spec.Rules.EnumValues
import           Hsed.Spec.Rules.TypeRow    (typeRowReads)
import           Hsed.Spec.Shape            (measure)


-- | The enumerations a context declares.
enumsOf :: Context -> [EnumDecl]
enumsOf ctx = rights [ enumDecl ctx er (checkEnum ctx rows er) | er <- rights (enumReads ctx) ]
  where rows = rights (typeRowReads ctx)

about :: Text -> [Finding] -> [Text]
about anchor = map renderFinding . filter ((== Just anchor) . Cm.anchor)

errorsOf :: ErrataSet -> SpecIR -> [Text]
errorsOf es = map renderFinding . filter isError . checkIR es

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
        scott     = checkIR Scott ir
        published = checkIR Published ir
        compiled ir' = either (throwString . show) pure (compileIR Scott ir')

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
        about "T52" (checkIR Scott (setCell "T52" 1 0 "0" ir)) `shouldBe`
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
  where
    advKeyMode     = [("Wait for AdvKey_Req", 0), ("Auto-advance keys", 1)] <> [ ("Reserved", v) | v <- [2 .. 7] ]
    paddingType    = [("None", 0), ("None", 1), ("RSAES-PKCS1-v1_5", 2), ("RSAES-OAEP", 3), ("RSASSA-PKCS1-v1_5", 4)] <> [ ("Reserved", v) | v <- [5 .. 15] ]
    lifeCycleState = [("Issued", 0), ("Issued-Disabled", 1), ("Issued-Frozen", 2), ("Issued-Disabled-Frozen", 3), ("Issued-Failed", 4)]
                     <> [ ("Unassigned", v) | v <- [5 .. 7] ] <> [ ("Reserved for SSC Usage", v) | v <- [8 .. 13] ] <> [ ("Unassigned", v) | v <- [14, 15] ]
    reencryptState = [("Idle", 1), ("Pending", 2), ("Active", 3), ("Completed", 4), ("Paused", 5)] <> [ ("Reserved", v) | v <- [6 .. 16] ]
