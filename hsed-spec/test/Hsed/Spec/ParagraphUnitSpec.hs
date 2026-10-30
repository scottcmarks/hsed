{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.ParagraphUnitSpec
Description : The paragraph unit: how a comment binds a sentence, and how a rule reads it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A list item or a paragraph of the IR (@paras@, docs/ir-v1.md) is a unit its own
comment binds (a @law@ binds its sentence, design example E10): no rule
scope, no classification, a paragraph no comment names being bound by nothing
and missed by nothing. A stub reader stands in for the law reader to come (no
rule of the registry takes paragraph units yet), proving the interface: the
units a context holds and a rule reads ('boundParas', in the anchors' natural
order, each with its IR hash), a rule that never fires on a table, an item
exemplar, findings and declarations located at the paragraph, a comment whose
anchor names several paragraphs refused, and @W-GEN-EXCLUDED@ for a sentence
no rule of the registry reads (pending once one does). The hashes are the IR's
of the paragraphs on Core PDF 82 and 86.
-}
module Hsed.Spec.ParagraphUnitSpec (spec) where

import qualified RIO.Text          as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Check   (mkContextWith)
import           Hsed.Spec.Common  (ErrataSet (..), RuleName (..), Severity (..), SpecId (..), finding, renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.Compile (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL     (SomeBinding (..), law)
import qualified Hsed.Spec.Gen     as Gen
import           Hsed.Spec.IR      (AnnotIR (..), ParaIR (..), Rect (..), SpecIR (..), StructPath (..))
import qualified Hsed.Spec.IR      as IR
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules   (checkExemplarsWith, classifyWithRules)
import           Hsed.Spec.Shape   (UnitKind (..), shapeUnit)


-- | What the rule reading paragraph units looks like: an ordinary 'RuleDef' of
--   the sentence shape, reached by its comments alone, which never fires on a
--   table and reads its units with 'boundParas'.
stub :: RuleDef
stub = RuleDef
  { ruleName    = RuleName "law"
  , bindsTo     = SomeBinding (law "")
  , exemplar    = (SpecId "core-2.01", ItemId "3.3.10.2.1" "3a")
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ _ -> True
  , ruleIdent   = const Nothing
  , ruleCodes   = []
  , ruleModule  = Gen.GenModule { Gen.leaf = "Laws", Gen.description = "Laws", Gen.notes = [] }
  , ruleChecks  = \ctx _ -> [ atPara ctx bp (finding Note "N-STUB" (tshow (bpBinding bp))) | bp <- boundParas (ruleShape stub) ctx ]
  , ruleCompile = \ctx _ -> [ Emit (Gen.decl (paraProvenanceOf ctx bp) "l" [Gen.provenanceComment (paraProvenanceOf ctx bp)])
                            | bp <- boundParas (ruleShape stub) ctx ]
  }

-- | The three sentences M3 binds to the sync machine's laws.
laws :: Text
laws = T.unlines
  [ "core-2.01 | 3.3.10.2.1#3a | law \"Sync.ifSendWhileBusy\""
  , "core-2.01 | 3.3.10.6#dc | law \"Sync.discardToS1\""
  , "core-2.01 | 3.3.10.7#a | law \"Sync.ignorePacket\"" ]

spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  -- the committed sidecar with these three lines, whether it has them or not
  ir  <- runIO (annotated pristine (foldr withoutLine hsa ["3.3.10.2.1#3a", "3.3.10.6#dc", "3.3.10.7#a"] <> laws))
  let ctx = mkContextWith (registry <> [stub]) Nothing Scott ir
      paras = boundParas (ruleShape stub) ctx

  describe "a paragraph unit" $ do
    it "is bound by its comment alone, read in the anchors' natural order with its IR hash" $ do
      map bpAnchor paras `shouldBe` [ItemId "3.3.10.2.1" "3a", ItemId "3.3.10.6" "dc", ItemId "3.3.10.7" "a"]
      [ (T.take 12 (bpPara bp).hash, (bpPara bp).page + 1) | bp <- paras ]
        `shouldBe` [("37bedfb19343", 82), ("cb4d5d489fc0", 86), ("cda4e92af591", 86)]
      map bpBinding (take 1 paras) `shouldBe` [SomeBinding (law "Sync.ifSendWhileBusy")]
      length (paragraphUnits ctx) `shouldBe` 3
    it "is no unit where its comment's anchor names several paragraphs (3.3.10.6 labels c under four transitions), an E-ANCHOR" $ do
      let stray = AnnotIR { kind = IR.Law, spec = "core-2.01", id = "3.3.10.6#c", struct = StructPath "0/1086/2", v = 1
                          , page = 83, rect = Rect 0 0 0 0, flags = 6, contents = "law \"Sync.discardToS1\"" }
          ctx' = mkContextWith (registry <> [stub]) Nothing Scott ir { annots = ir.annots <> [stray] }
      map bpAnchor (paragraphUnits ctx') `shouldBe` map bpAnchor paras
      [ renderFinding f | f <- cxClassFindings ctx', Cm.code f == "E-ANCHOR" ]
        `shouldBe` ["E-ANCHOR core-2.01.pdf#page=84&3.3.10.6#c: 4 paragraphs labelled 3.3.10.6#c in the IR"]

  describe "a rule reading paragraph units" $ do
    it "reads every bound paragraph, and no table" $ do
      length paras `shouldBe` 3
      length (boundTables (ruleShape stub) ctx) `shouldBe` 0
    it "never binds a table, whatever its fires says: the tables classify as they do without it" $
      fst (classifyWithRules (registry <> [stub]) ir) `shouldBe` fst (classifyWithRules registry ir)
    it "locates its findings at the paragraph" $
      take 1 (map renderFinding (ruleChecks stub ctx []))
        `shouldBe` ["N-STUB core-2.01.pdf#page=82&3.3.10.2.1#3a: SomeBinding (Law \"Sync.ifSendWhileBusy\")"]
    it "gives its declarations the paragraph's provenance: the item, the page, the IR hash" $
      [ T.takeWhile (/= ',') (Gen.provenanceComment d.declProvenance) | Emit d <- take 1 (ruleCompile stub ctx []) ]
        `shouldBe` ["-- | core-2.01 3.3.10.2.1#3a list item 3a (PDF 82 / p.65"]
    it "is checked by its exemplar, a list item the IR has once" $ do
      checkExemplarsWith [stub] ir `shouldBe` []
      map renderFinding (checkExemplarsWith [stub { exemplar = (SpecId "core-2.01", ItemId "3.3.10.6" "c") }] ir)
        `shouldBe` ["C-EXEMPLAR core-2.01: the exemplar 3.3.10.6#c of rule law is not in the IR: 4 paragraphs labelled 3.3.10.6#c in the IR"]

  describe "a sentence no rule of the registry reads" $
    it "is W-GEN-EXCLUDED at the paragraph, and among the units compile leaves out" $
      if any ((== Just ParagraphUnit) . fmap shapeUnit . ruleShape) registry
        then pendingWith "a rule of the registry reads the paragraph units (the law reader)"
        else do
          cr <- either (throwString . show) pure (compileIR Nothing Scott ir)
          [ renderFinding f | f <- crFindings cr, Cm.code f == "W-GEN-EXCLUDED", Cm.severity f == Warning ]
            `shouldBe` [ "W-GEN-EXCLUDED core-2.01.pdf#page=" <> pg <> "&" <> aid
                         <> ": hsedc generates nothing for a Sentence paragraph yet (no rule of the registry reads one)"
                       | (pg, aid) <- [("82", "3.3.10.2.1#3a"), ("86", "3.3.10.6#dc"), ("86", "3.3.10.7#a")] ]
          [ aid | (aid@(ItemId _ _), _) <- crExcluded cr ] `shouldBe` map bpAnchor paras
