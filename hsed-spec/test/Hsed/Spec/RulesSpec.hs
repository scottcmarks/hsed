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
the anchors, every numbered table classified once, precedence by @refines@, the
bare modifier chain of Table 75, bindings shaped by their target (since M5a
across specs: Opal Tables 14-16 over Core, and their @narrows@ checks), the
exemplars, and the validation of every annotation: an anchor of the payload's
kind, naming something in the IR, alone on its anchor; @hsedc check@ and
@hsedc bind@ refusing exactly the same annotations. The tests hold invariants
of the classification, not its totals (those are in @docs/LEDGER.md@, and each
rule's own count is in @Rules/\<Rule\>Spec.hs@), so that a new rule edits
nothing here.
-}
module Hsed.Spec.RulesSpec (spec) where

import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Text          as T
import           RIO               hiding ((&))
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Bind    (bindEntries, bindEntriesOver)
import           Hsed.Spec.Check   (checkIR)
import           Hsed.Spec.Common  (Finding, Severity (..), isError, renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.CoreIR  hiding (core)
import           Hsed.Spec.DSL
import           Hsed.Spec.Grammar (printSomeBinding)
import           Hsed.Spec.IR      (AnnotIR (..), Rect (..), SpecIR (..), StructPath (..))
import qualified Hsed.Spec.IR      as IR
import           Hsed.Spec.Rules
import           Hsed.Spec.Rules.TypeRow (typeRowRule)
import           Hsed.Spec.Shape   (measure, shapeMismatch)


codesOf :: [Finding] -> [Text]
codesOf = map Cm.code

count :: (Classification -> Bool) -> Map AnchorId Classification -> Int
count p = length . filter p . Map.elems

isRuleBound, isUnbound, isAmbiguousOrInvalid :: Classification -> Bool
isRuleBound c = case c of
  RuleBound {} -> True
  _            -> False
isUnbound c = c == Unbound
isAmbiguousOrInvalid c = case c of
  Ambiguous _ -> True
  Invalid _   -> True
  _           -> False

-- | A rule that fires wherever typeRow does: the second of an ambiguous pair.
shadow :: Maybe RuleName -> RuleDef
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

    describe "scopeOf" $ do
      it "finds typeRow and enumValues at section 5.1.3 and nowhere else, tableUIDs on the doc anchor" $ do
        scopeOf ir (RuleName "typeRow") `shouldBe` [Subtree "5.1.3"]
        scopeOf ir (RuleName "enumValues") `shouldBe` [Subtree "5.1.3"]
        scopeOf ir (RuleName "tableUIDs") `shouldBe` [WholeDocument]
      it "gives a rule no anchor names no scope" $ do
        ir' <- annotated pristine "core-2.01 | doc | doc core201\ncore-2.01 | 5.1.3 | rule typeRow\n"
        scopeOf ir' (RuleName "typeRow") `shouldBe` [Subtree "5.1.3"]
        scopeOf ir' (RuleName "enumValues") `shouldBe` []
      it "comes only from the anchors: the pristine IR has none" $ do
        scopeOf pristine (RuleName "typeRow") `shouldBe` []
        count isUnbound (classify pristine) `shouldBe` 248
      it "spans the whole document from a rule token on the doc anchor" $ do
        ir' <- annotated pristine "core-2.01 | doc | doc core201 & rule typeRow & rule enumValues\n"
        scopeOf ir' (RuleName "typeRow") `shouldBe` [WholeDocument]
        count isRuleBound (classify ir') `shouldBe` 119

    describe "classify" $ do
      it "classifies every numbered table once, binding none ambiguously or invalidly" $ do
        Map.keys classes `shouldBe` Map.keys (numberedTables ir)
        Map.size classes `shouldBe` 248
        [ aid | (aid, c) <- Map.toList classes, isAmbiguousOrInvalid c ] `shouldBe` []
      it "reports one E-UNBOUND, an Error, per unbound table, at the table" $ do
        let (_, fs) = classifyWith ir
        L.sort [ a | f <- fs, Cm.code f == "E-UNBOUND", Cm.severity f == Error, Just a <- [Cm.anchor f] ]
          `shouldBe` L.sort [ renderAnchorId aid | (aid, Unbound) <- Map.toList classes ]
        [ f | f <- fs, Cm.code f == "W-UNBOUND" ] `shouldBe` []
      it "leaves no numbered table of Core unbound (design section 11 M2c: E-UNBOUND an Error, orphans empty)" $
        [ aid | (aid, Unbound) <- Map.toList classes ] `shouldBe` []
      it "binds T46 by typeRow and T52 by enumValues, with no comment" $ do
        at' 46 `shouldBe` Just (RuleBound (RuleName "typeRow") [] (SomeBinding typeRow))
        at' 52 `shouldBe` Just (RuleBound (RuleName "enumValues") [] (SomeBinding enumValues))
      it "applies Table 75's bare erratum to the typeRow default" $
        at' 75 `shouldBe` Just (RuleBound (RuleName "typeRow") [Erratum Scott [("Fractoin", "Fraction")]]
                                          (SomeBinding (erratum scott ["Fractoin" ~> "Fraction"] typeRow)))
      it "leaves every table of the pristine IR unbound, each an E-UNBOUND Error naming its caption" $ do
        let (pristineClasses, fs) = classifyWith pristine
        L.nub (Map.elems pristineClasses) `shouldBe` [Unbound]
        length [ f | f <- fs, Cm.code f == "E-UNBOUND", Cm.severity f == Error ] `shouldBe` Map.size pristineClasses
        filter ("T239" `T.isInfixOf`) (map renderFinding fs)
          `shouldBe` ["E-UNBOUND core-2.01.pdf#page=302&T239: no annotation binds it and no rule in scope fires on it (Table 239 Special Purpose UIDs)"]
      it "binds a wider enumeration through a full binding with cols: T87's comment (design E4)" $ do
        at' 87 `shouldBe` Just (CommentBound (SomeBinding (rangeWords ["to"] (cols 0 1 enumValues))))
        ir' <- annotated pristine (withoutLine "T87" hsa)
        Map.lookup (TableId 87) (classify ir') `shouldBe` Just Unbound
      it "refuses a bare modifier chain where no rule fires (no rule anchor at all)" $ do
        ir' <- annotated pristine "core-2.01 | T46 | erratum scott [\"a\" ~> \"b\"]\n"
        fmap describeClassification (Map.lookup (TableId 46) (classify ir'))
          `shouldSatisfy` maybe False ("invalid: E-MODIFIER core-2.01.pdf#page=100&T46" `T.isPrefixOf`)
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
      it "reports a rule named but not implemented, whichever that is (every rule of the vocabulary is, since M4b: the registry less one)" $ do
        [ r | r <- knownRules, r `notElem` map ruleName rules ] `shouldBe` []
        forM_ (take 1 knownRules) $ \r -> do
          ir' <- annotated pristine ("core-2.01 | doc | doc core201 & rule " <> ruleNameText r <> "\n")
          codesOf (snd (classifyWithRules [ rl | rl <- rules, ruleName rl /= r ] ir')) `shouldContain` ["W-RULE-UNIMPLEMENTED"]
          codesOf (snd (classifyWith ir')) `shouldNotContain` ["W-RULE-UNIMPLEMENTED"]

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

    describe "a comment on a table a rule fires on" $
      it "wins over the rule: T3, Foo Table Description, stays prose under a tableDesc that fires on it" $ do
        let tableDescLike = typeRowRule
              { ruleName = RuleName "tableDesc", bindsTo = SomeBinding tableDesc, joinsTo = Nothing
              , ruleIdent = const Nothing, fires = \_ _ form -> isNothing (shapeMismatch TableDesc form) }
            ruleSet = filter ((/= RuleName "tableDesc") . ruleName) rules <> [tableDescLike]
            (classes', fs) = classifyWithRules ruleSet ir
        scopeOf ir (RuleName "tableDesc") `shouldBe` [WholeDocument]
        fires tableDescLike (SpecId "core-2.01") <$> Map.lookup (TableId 3) (numberedTables ir)
          <*> (measure (SpecId "core-2.01") <$> Map.lookup (TableId 3) (numberedTables ir)) `shouldBe` Just True
        Map.lookup (TableId 3) classes' `shouldBe` Just (CommentBound (SomeBinding prose))
        Map.lookup (TableId 171) classes' `shouldBe` Just (RuleBound (RuleName "tableDesc") [] (SomeBinding tableDesc))
        [ f | f <- fs, Cm.anchor f == Just "T3" ] `shouldBe` []

    describe "explain" $
      it "names the rule and its exemplar (acceptance (g))" $ do
        explain ir (TableId 46) `shouldBe` "typeRow (exemplar core T46)"
        explain ir (TableId 52) `shouldBe` "enumValues (exemplar core T52)"
        explain ir (TableId 75) `shouldBe` "typeRow (exemplar core T46), binding erratum scott [\"Fractoin\" ~> \"Fraction\"] typeRow"
        explain ir (TableId 87) `shouldBe` "comment: rangeWords [\"to\"] (cols 0 1 enumValues)"

    describe "checkExemplars" $ do
      it "finds both exemplars still firing" $
        checkExemplars ir `shouldBe` []
      it "reports an exemplar that is gone, and one that no longer fires" $ do
        codesOf (checkExemplars (dropTable "T46" ir)) `shouldBe` ["C-EXEMPLAR"]
        map renderFinding (checkExemplars (setCell "T52" 0 0 "zero" ir)) `shouldBe` []   -- the rule reads headers, not values
        codesOf (checkExemplars (dropTable "T51" ir)) `shouldBe` ["C-EXEMPLAR"]          -- T52 has no type row to join

    describe "an annotation of the wrong kind for its anchor (E-ANCHOR-KIND)" $ do
      let errorsWith extra = do
            ir' <- annotated pristine (hsa <> extra)
            pure (filter isError (snd (classifyWith ir')), filter isError (fst (bindEntries ir')), ir')
          refused extra want = do
            (checkErrs, bindErrs, _) <- errorsWith extra
            map renderFinding checkErrs `shouldBe` [want]
            map renderFinding bindErrs `shouldBe` [want]
      it "refuses a rule token on a table, leaving T46 to the rule" $ do
        refused "core-2.01 | T46 | rule typeRow\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=100&T46: a table anchor takes a table binding or a bare modifier chain, not rule typeRow"
        (_, _, ir') <- errorsWith "core-2.01 | T46 | rule typeRow\n"
        fmap describeClassification (Map.lookup (TableId 46) (classify ir'))
          `shouldBe` Just "invalid: E-ANCHOR-KIND core-2.01.pdf#page=100&T46: a table anchor takes a table binding or a bare modifier chain, not rule typeRow"
      it "refuses doc, behaviour and law on a table" $ do
        refused "core-2.01 | T46 | doc core201\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=100&T46: a table anchor takes a table binding or a bare modifier chain, not doc core201"
        refused "core-2.01 | T46 | behaviour \"Foo.bar\"\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=100&T46: a table anchor takes a table binding or a bare modifier chain, not behaviour \"Foo.bar\""
        refused "core-2.01 | T46 | law \"Foo.bar\"\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=100&T46: a table anchor takes a table binding or a bare modifier chain, not law \"Foo.bar\""
      it "refuses a table binding, a cross-spec binding and a doc token on a heading" $ do
        refused "core-2.01 | 5.1.4 | typeRow\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=128&5.1.4: a heading anchor takes rule tokens, a behaviour or fails, not typeRow"
        refused "core-2.01 | 5.1.5 | refines (core 46)\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=137&5.1.5: a heading anchor takes rule tokens, a behaviour or fails, not refines (core 46)"
        refused "core-2.01 | 5.1.5 | doc opal230\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=137&5.1.5: a heading anchor takes rule tokens, a behaviour or fails, not doc opal230"
      it "refuses a rule token, a behaviour and a fails on a paragraph: since M4a behaviour and fails sit on headings alone" $ do
        refused "core-2.01 | 5.1.3#p1 | rule typeRow\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=100&5.1.3#p1: a list item or paragraph anchor takes law, not rule typeRow"
        refused "core-2.01 | 5.1.3#p1 | behaviour \"Foo.bar\"\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=100&5.1.3#p1: a list item or paragraph anchor takes law, not behaviour \"Foo.bar\""
        refused "core-2.01 | 5.1.3#p1 | fails \"Foo.bar\"\n"
          "E-ANCHOR-KIND core-2.01.pdf#page=100&5.1.3#p1: a list item or paragraph anchor takes law, not fails \"Foo.bar\""
      it "refuses a doc anchor naming another spec, or without its doc token first" $ do
        let doc' payload = annotated pristine (T.unlines (("core-2.01 | doc | " <> payload) : drop 1 (T.lines hsa)))
        ir1 <- doc' "doc opal230 & rule typeRow & rule enumValues"
        -- the tables the other doc tokens bound are left unbound, each an E-UNBOUND
        map renderFinding (filter (\f -> isError f && Cm.code f /= "E-UNBOUND") (snd (classifyWith ir1)))
          `shouldBe` ["E-ANCHOR-KIND core-2.01.pdf#page=1&doc: the doc anchor of core-2.01 names its own spec, doc core201, not doc opal230 & rule typeRow & rule enumValues"]
        ir2 <- doc' "rule typeRow"
        map renderFinding (filter isError (fst (bindEntries ir2)))
          `shouldBe` ["E-ANCHOR-KIND core-2.01.pdf#page=1&doc: the doc anchor takes doc core201 first, then rule tokens, not rule typeRow"]
      it "takes what each anchor's kind takes: a behaviour and a fails on headings, a law on an item" $ do
        (checkErrs, bindErrs, _) <- errorsWith (T.unlines
          [ "core-2.01 | 3.3.3.1 | behaviour \"Sync.x\"", "core-2.01 | 3.3.3.2 | fails \"Sync.z\"", "core-2.01 | 3.3.3#1 | law \"Sync.y\"" ])
        (checkErrs, bindErrs) `shouldBe` ([], [])

    describe "an annotation naming nothing in the IR, or sharing its anchor (E-ANCHOR)" $ do
      let ruleAnnot sec = AnnotIR IR.Rule "core-2.01" sec (StructPath "0/1278") 1 99 (Rect 90 595.61 215.4 613.67) 6 "rule typeRow & rule enumValues"
      it "refuses a rule anchor whose section has no heading, so that nothing is silently left unbound" $ do
        let ir' = ir { annots = [ a | a <- ir.annots, a.id /= "5.1.3" ] <> [ruleAnnot "5.1.30"] }
            want = "E-ANCHOR core-2.01.pdf#page=100&5.1.30: no heading numbered 5.1.30 in the IR"
        map renderFinding (filter isError (snd (classifyWith ir'))) `shouldSatisfy` elem want
        map renderFinding (fst (bindEntries ir')) `shouldSatisfy` elem want
      it "refuses two annotations on one heading, in check and in bind" $ do
        let ir' = ir { annots = ir.annots <> [ruleAnnot "5.1.3"] }
            want = "E-ANCHOR core-2.01.pdf#page=100&5.1.3: 2 annotations on one anchor"
        L.nub (map renderFinding (filter isError (snd (classifyWith ir')))) `shouldSatisfy` elem want
        map renderFinding (fst (bindEntries ir')) `shouldSatisfy` elem want
        scopeOf ir' (RuleName "typeRow") `shouldBe` []

  withM5aOpal $ \base opalM5 -> do
    let overCore = classifyOver (Just base) opalM5
        shapeAt n cs = Map.lookup (TableId n) cs >>= boundBinding >>= someBindingShape
        narrowing ir' = [ renderFinding f | f <- checkIR (Just base) Scott ir', "NARROWS" `T.isSuffixOf` Cm.code f ]
        cellsOf n = maybe [] (map (map (.text)) . (.rows)) (Map.lookup (TableId n) (numberedTables opalM5))
    describe "a binding shaped by a target in the spec it is read over (classifyOver, M5a: Opal Tables 14-16, PDF 30-32)" $ do
      it "takes the shapes Core binds its Tables 26 (codes, PDF 55-56), 4 (tokens, PDF 32) and 128 (an enumeration, PDF 119) to" $
        map (`shapeAt` overCore) [14, 15, 16] `shouldBe` [Just Codes, Just Tokens, Just EnumValues]
      it "is E-SHAPE read alone, as it was before M5a" $
        fmap describeClassification (Map.lookup (TableId 16) (classify opalM5)) `shouldBe`
          Just "invalid: E-SHAPE opal-2.30.pdf#page=32&T16: the shape comes from Core 128, which is not in opal-2.30; resolve it with that spec's IR"
      it "is E-SHAPE for a table of the base nothing binds, named with the base's spec" $ do
        ir' <- annotated opalM5 (m5aOpalHsa <> "opal-2.30 | T13 | narrows (core 9999)\n")
        fmap describeClassification (Map.lookup (TableId 13) (classifyOver (Just base) ir')) `shouldBe`
          Just "invalid: E-SHAPE opal-2.30.pdf#page=29&T13: the shape comes from core-2.01 T9999, which nothing binds"
      it "binds as it classifies: bind over the base prints Table 16's entry, bind alone refuses it" $ do
        [ (printSomeBinding sb, someBindingShape sb) | (_, TableId 16, _, _, sb) <- snd (bindEntriesOver (Just base) opalM5) ]
          `shouldBe` [("replaces [core 128]", Just EnumValues)]
        map renderFinding (fst (bindEntriesOver (Just base) opalM5)) `shouldSatisfy` all (not . ("&T16:" `T.isInfixOf`))
        map renderFinding (fst (bindEntries opalM5)) `shouldSatisfy` any ("E-SHAPE opal-2.30.pdf#page=32&T16:" `T.isPrefixOf`)
      it "explains as it classifies" $
        explainOver (Just base) opalM5 (TableId 14) `shouldBe` "comment: narrows (core 26)"

    describe "narrows (core N): the rows of the target, restricted (the codes and tokenEncoding rules' C-/N-CODE-NARROWS, C-/N-TOKEN-NARROWS)" $ do
      it "reads Table 14 against Core's Table 26 and Table 15 against Core's Table 4: one ComID assigned of a reserved range, one token named with a word more" $
        narrowing opalM5 `shouldBe`
          [ "N-CODE-NARROWS opal-2.30.pdf#page=30&T14: row 4: 0x0004 TPER_RESET command assigns codes core-2.01 T26 row 3 reserves (0x0002-0x07FF Reserved for TCG)"
          , "N-TOKEN-NARROWS opal-2.30.pdf#page=31&T15: row 13: \"End of transaction\" (ET) read as core-2.01 T4's \"End transaction\"" ]
      it "has Table 14's assignment where Table 13 puts TPER_RESET's ComID (PDF 29-30)" $ do
        [ v | [f, v] <- cellsOf 13, f == "ComID" ] `shouldBe` ["0x0004"]
        [ c | [c, d] <- cellsOf 14, d == "TPER_RESET command" ] `shouldBe` ["0x0004"]
      it "is an Error for a row its target does not have" $ do
        let errors ir' = filter ("C-" `T.isPrefixOf`) (narrowing ir')
        errors (setCell "T14" 5 1 "Vendor Specific" opalM5) `shouldBe`
          ["C-CODE-NARROWS opal-2.30.pdf#page=30&T14: row 6: 0x0800-0x0FFF Vendor Specific is not core-2.01 T26 row 4's 0x0800-0x0FFF Vendor Unique"]
        errors (setCell "T15" 4 1 "SLX" opalM5) `shouldBe`
          ["C-TOKEN-NARROWS opal-2.30.pdf#page=30&T15: row 5: \"Start List\" (SLX) names no token of core-2.01 T4"]
