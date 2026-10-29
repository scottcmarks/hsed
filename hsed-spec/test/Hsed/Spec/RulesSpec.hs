{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.RulesSpec
Description : Test specifications for Hsed.Spec.Rules
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The rules of design section 3.4 on the committed Core extraction: scope only from
the anchors, the M1 pair binding Tables 46-165 but T87, precedence by
@refines@, the bare modifier chain of Table 75, bindings shaped by their
target, the exemplars and C-RULE-ANCHORED.
-}
module Hsed.Spec.RulesSpec (spec) where

import qualified RIO.Map           as Map
import qualified RIO.Text          as T
import           RIO               hiding ((&))
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Common  (Finding, Severity (..), renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.CoreIR  hiding (core)
import           Hsed.Spec.DSL
import           Hsed.Spec.Grammar (printSomeBinding)
import           Hsed.Spec.IR      (StructPath (..))
import           Hsed.Spec.Rules
import           Hsed.Spec.Shape   (measure)


codesOf :: [Finding] -> [Text]
codesOf = map Cm.code

count :: (Classification -> Bool) -> Map AnchorId Classification -> Int
count p = length . filter p . Map.elems

isRuleBound, isUnbound :: Classification -> Bool
isRuleBound c = case c of
  RuleBound {} -> True
  _            -> False
isUnbound c = c == Unbound

-- | A rule that fires wherever typeRow does: the second of an ambiguous pair.
shadow :: Maybe RuleName -> Rule
shadow refining = typeRowRule
  { ruleName = RuleName "tableDesc", refinesRule = refining, bindsTo = SomeBinding tableDesc }

spec :: Spec
spec = do
  describe "covers" $
    it "takes a section and its subsections, nothing else" $ do
      map (covers (Subtree "5.1.3")) [Just "5.1.3", Just "5.1.3.6", Just "5.1.30", Just "5.1", Nothing]
        `shouldBe` [True, True, False, False, False]
      covers WholeDocument Nothing `shouldBe` True

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let classes = classify ir
        at' n = Map.lookup (TableId n) classes

    describe "tableSections" $
      it "puts each table under the nearest numbered heading before it" $ do
        let secs = tableSections ir
        Map.lookup (StructPath "0/1288") secs `shouldBe` Just (Just "5.1.3.1")   -- T46
        Map.lookup (StructPath "0/1318") secs `shouldBe` Just (Just "5.1.3.6")   -- T52

    describe "tableIdent" $
      it "names the type of a type row and of an enumeration" $ do
        let ident n = tableIdent . measure core201 =<< Map.lookup (TableId n) (numberedTables ir)
        map ident [46, 52, 54, 72, 171] `shouldBe` [Just "AC_element", Just "adv_key_mode", Just "attr_flags", Just "Certificates_object_ref", Nothing]

    describe "scopeOf" $ do
      it "finds typeRow and enumValues at section 5.1.3 and nowhere else" $ do
        scopeOf ir (RuleName "typeRow") `shouldBe` [Subtree "5.1.3"]
        scopeOf ir (RuleName "enumValues") `shouldBe` [Subtree "5.1.3"]
        scopeOf ir (RuleName "codes") `shouldBe` []
      it "comes only from the anchors: the pristine IR has none" $ do
        scopeOf pristine (RuleName "typeRow") `shouldBe` []
        count isUnbound (classify pristine) `shouldBe` 248
      it "spans the whole document from a rule token on the doc anchor" $ do
        ir' <- annotated pristine "core-2.01 | doc | doc core201 & rule typeRow & rule enumValues\n"
        scopeOf ir' (RuleName "typeRow") `shouldBe` [WholeDocument]
        count isRuleBound (classify ir') `shouldBe` 119

    describe "classify" $ do
      it "binds the 100 type rows and 19 enumerations of section 5.1.3, and nothing else" $ do
        count isRuleBound classes `shouldBe` 119
        count isUnbound classes `shouldBe` 129
        Map.size classes `shouldBe` 248
      it "binds T46 by typeRow and T52 by enumValues, with no comment" $ do
        at' 46 `shouldBe` Just (RuleBound (RuleName "typeRow") [] (SomeBinding typeRow))
        at' 52 `shouldBe` Just (RuleBound (RuleName "enumValues") [] (SomeBinding enumValues))
      it "applies Table 75's bare erratum to the typeRow default" $
        at' 75 `shouldBe` Just (RuleBound (RuleName "typeRow") [Erratum Scott [("Fractoin", "Fraction")]]
                                          (SomeBinding (erratum scott ["Fractoin" ~> "Fraction"] typeRow)))
      it "leaves T87 (three columns) and the tables outside 5.1.3 unbound, each a W-UNBOUND" $ do
        at' 87 `shouldBe` Just Unbound
        at' 240 `shouldBe` Just Unbound
        let (_, fs) = classifyWith ir
        length [ f | f <- fs, Cm.code f == "W-UNBOUND", Cm.severity f == Warning ] `shouldBe` 129
        filter ("T87" `T.isInfixOf`) (map renderFinding fs)
          `shouldBe` ["W-UNBOUND core-2.01.pdf#page=109&T87: no annotation binds it and no rule in scope fires on it (Table 87 gen_status Enumeration Values)"]
      it "binds a wider enumeration through a full binding with cols" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T87 | rangeWords [\"to\"] (cols 0 1 enumValues)\n")
        Map.lookup (TableId 87) (classify ir')
          `shouldBe` Just (CommentBound (SomeBinding (rangeWords ["to"] (cols 0 1 enumValues))))
      it "refuses a bare modifier chain where no rule fires" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T240 | erratum scott [\"a\" ~> \"b\"]\n")
        fmap describeClassification (Map.lookup (TableId 240) (classify ir'))
          `shouldSatisfy` maybe False ("invalid: E-MODIFIER core-2.01.pdf#page=302&T240" `T.isPrefixOf`)
      it "keeps a parse failure as Invalid, located at the anchor" $ do
        ir' <- annotated pristine "core-2.01 | T46 | frobnicate\n"
        fmap describeClassification (Map.lookup (TableId 46) (classify ir'))
          `shouldBe` Just "invalid: E-GRAMMAR core-2.01.pdf#page=100&T46: 1:1: unknown name frobnicate"
      it "gives a binding shaped by its target the target's shape" $ do
        ir' <- annotated pristine (hsa <> "core-2.01 | T47 | refines (core 46)\n")
        fmap (fmap someBindingShape . boundBinding) (Map.lookup (TableId 47) (classify ir')) `shouldBe` Just (Just (Just TypeRow))
        fmap (fmap printSomeBinding . boundBinding) (Map.lookup (TableId 47) (classify ir')) `shouldBe` Just (Just "refines (core 46)")
      it "cannot shape a binding by a target in another spec" $ do
        ir' <- annotated pristine "core-2.01 | T47 | refines (opal 3)\n"
        fmap describeClassification (Map.lookup (TableId 47) (classify ir'))
          `shouldSatisfy` maybe False ("invalid: E-SHAPE core-2.01.pdf#page=100&T47" `T.isPrefixOf`)
      it "reports a rule named but not implemented" $ do
        ir' <- annotated pristine "core-2.01 | doc | doc core201 & rule codes\n"
        codesOf (snd (classifyWith ir')) `shouldContain` ["W-RULE-UNIMPLEMENTED"]

    describe "precedence" $ do
      ir' <- runIO (annotated pristine "core-2.01 | 5.1.3 | rule typeRow & rule tableDesc\n")
      it "makes two rules firing on one table E-AMBIGUOUS" $ do
        let (classes', fs) = classifyWithRules [typeRowRule, shadow Nothing] ir'
        Map.lookup (TableId 46) classes' `shouldBe` Just (Ambiguous [RuleName "typeRow", RuleName "tableDesc"])
        length (filter ((== "E-AMBIGUOUS") . Cm.code) fs) `shouldBe` 100
      it "lets a rule that refines another win where both fire" $ do
        let (classes', fs) = classifyWithRules [typeRowRule, shadow (Just (RuleName "typeRow"))] ir'
        Map.lookup (TableId 46) classes' `shouldBe` Just (RuleBound (RuleName "tableDesc") [] (SomeBinding tableDesc))
        filter ((== "E-AMBIGUOUS") . Cm.code) fs `shouldBe` []

    describe "explain" $
      it "names the rule and its exemplar (acceptance (g))" $ do
        explain ir (TableId 46) `shouldBe` "typeRow (exemplar core T46)"
        explain ir (TableId 52) `shouldBe` "enumValues (exemplar core T52)"
        explain ir (TableId 75) `shouldBe` "erratum scott [\"Fractoin\" ~> \"Fraction\"] typeRow (exemplar core T46)"
        explain ir (TableId 87) `shouldBe` "unbound"

    describe "checkExemplars" $ do
      it "finds both exemplars still firing" $
        checkExemplars ir `shouldBe` []
      it "reports an exemplar that is gone, and one that no longer fires" $ do
        codesOf (checkExemplars (dropTable "T46" ir)) `shouldBe` ["C-EXEMPLAR"]
        map renderFinding (checkExemplars (setCell "T52" 0 0 "zero" ir)) `shouldBe` []   -- the rule reads headers, not values
        codesOf (checkExemplars (dropTable "T51" ir)) `shouldBe` ["C-EXEMPLAR"]          -- T52 has no type row to join

    describe "checkRulesAnchored" $ do
      it "passes when every binding rule has an anchor" $
        checkRulesAnchored ir classes `shouldBe` []
      it "reports a rule that binds without an anchor" $
        map renderFinding (checkRulesAnchored pristine (Map.fromList [(TableId 46, RuleBound (RuleName "typeRow") [] (SomeBinding typeRow))]))
          `shouldBe` ["C-RULE-ANCHORED core-2.01: rule typeRow binds 1 tables and has no anchor in core-2.01"]
