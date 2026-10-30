{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.HeadingUnitSpec
Description : The heading unit: how a behaviour or fails comment binds a heading, and how a rule reads it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A heading of the IR (@headings@, docs/ir-v1.md) is a unit its own comment binds
(a @behaviour@ binds a method's heading, a @fails@ its Fails list's, design
section 13.16; M4a): no rule scope, no classification, a heading no comment
names being bound by nothing and missed by nothing, and a heading carrying rule
tokens a rule anchor, not a unit. A stub reader stands in for any rule over
heading units (the registry's is M4a's behaviours reader), proving the
interface: the units a context holds and a rule reads ('boundHeadings', in the
anchors' natural order, each with its title and page), a rule that never fires
on a table, a heading exemplar, findings and declarations located at the
heading, a comment on a list item refused (@E-ANCHOR-KIND@: @behaviour@ and
@fails@ sit on headings alone), and @W-GEN-EXCLUDED@ for a heading no rule of
the registry reads (pending once a reader reads them). The provenance hash
of a heading is the manifest's, held to the committed manifest's heading anchors.
-}
module Hsed.Spec.HeadingUnitSpec (spec) where

import           Data.Aeson        (Value (..), decodeStrict')
import qualified Data.Aeson.KeyMap as KM
import qualified RIO.Text          as T
import qualified RIO.Vector        as V
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Check   (mkContextWith)
import           Hsed.Spec.Common  (ErrataSet (..), RuleName (..), Severity (..), SpecId (..), finding, renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.Compile (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL     (SomeBinding (..), behaviour, fails)
import qualified Hsed.Spec.Gen     as Gen
import           Hsed.Spec.IR      (HeadingIR (..))
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules   (checkExemplarsWith, classifyWithRules)
import           Hsed.Spec.Shape   (UnitKind (..), shapeUnit)


-- | What the rule reading heading units looks like: an ordinary 'RuleDef' of
--   the method shape, reached by its comments alone, which never fires on a
--   table and reads its units with 'boundHeadings'.
stub :: RuleDef
stub = RuleDef
  { ruleName    = RuleName "headingStub"
  , bindsTo     = SomeBinding (behaviour "")
  , exemplar    = (SpecId "core-2.01", SectionId "5.3.3.6")
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ _ -> True
  , ruleIdent   = const Nothing
  , ruleCodes   = []
  , ruleModule  = Gen.GenModule { Gen.leaf = "Behaviours", Gen.description = "Behaviours", Gen.notes = [] }
  , ruleChecks  = \ctx _ -> [ atHeading ctx bh (finding Note "N-STUB" (tshow (bhBinding bh))) | bh <- boundHeadings (ruleShape stub) ctx ]
  , ruleCompile = \ctx _ -> [ Emit (Gen.decl (headingProvenanceOf ctx bh) "b" [Gen.provenanceComment (headingProvenanceOf ctx bh)])
                            | bh <- boundHeadings (ruleShape stub) ctx ]
  }

-- | A method's heading and its Fails list's heading, bound as M4a binds Get's (Core PDF 201).
behaviours :: Text
behaviours = T.unlines
  [ "core-2.01 | 5.3.3.6 | behaviour \"Base.get\""
  , "core-2.01 | 5.3.3.6.3 | fails \"Base.getFails\"" ]

-- | The heading anchors of the committed manifest: each id, title and hash.
manifestHeadings :: IO [(Text, Text, Text)]
manifestHeadings = do
  bytes <- readFileBinary "../spec/annot/core-2.01.manifest.json"
  pure $ case decodeStrict' bytes of
    Just (Object o) | Just (Array anchors) <- KM.lookup "anchors" o ->
      [ (i, t, h) | Object a <- V.toList anchors, Just (String i) <- [KM.lookup "id" a], Just (String t) <- [KM.lookup "title" a]
                  , Just (String h) <- [KM.lookup "hash" a], Just (String k) <- [KM.lookup "kind" a], k `elem` ["Rule", "Behaviour"] ]
    _ -> []

-- | A sidecar without its behaviour and fails lines (M4a's five, or none), so that the units are these two alone.
withoutBehaviours :: Text -> Text
withoutBehaviours = T.unlines . filter (\l -> not (any (`T.isInfixOf` l) ["| behaviour ", "| fails "])) . T.lines

spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO (withoutBehaviours <$> committedHsa)
  -- the committed sidecar without its behaviour and fails lines, with these two
  ir  <- runIO (annotated pristine (hsa <> behaviours))
  let ctx = mkContextWith (registry <> [stub]) Nothing Scott ir
      units = boundHeadings (ruleShape stub) ctx

  describe "a heading unit" $ do
    it "is bound by its comment alone, read in the anchors' natural order with its title and page" $ do
      map bhAnchor units `shouldBe` [SectionId "5.3.3.6", SectionId "5.3.3.6.3"]
      [ ((bhHeading bh).title, (bhHeading bh).page + 1) | bh <- units ]
        `shouldBe` [("Basic Table Method Group - Get (Table and Object Method)", 201), ("Fails", 201)]
      map bhBinding units `shouldBe` [SomeBinding (behaviour "Base.get"), SomeBinding (fails "Base.getFails")]
      length (headingUnits ctx) `shouldBe` 2
    it "is no unit where a heading carries rule tokens: 3.2.2 and 5.1.3 are rule anchors" $ do
      map bhAnchor (headingUnits ctx) `shouldNotContain` [SectionId "3.2.2"]
      map bhAnchor (headingUnits ctx) `shouldNotContain` [SectionId "5.1.3"]
    it "is no unit on a list item: a behaviour or a fails on 5.3.3.6.3#a is E-ANCHOR-KIND, a heading anchor's words" $ do
      let (fs, _) = annotatedWith pristine (hsa <> "core-2.01 | 5.3.3.6.3#a | fails \"Base.getFails\"\n")
      fs `shouldBe` []
      forM_ ["behaviour \"Base.get\"", "fails \"Base.getFails\""] $ \payload -> do
        ir' <- annotated pristine (hsa <> "core-2.01 | 5.3.3.6.3#a | " <> payload <> "\n")
        let ctx' = mkContextWith (registry <> [stub]) Nothing Scott ir'
        map bhAnchor (headingUnits ctx') `shouldBe` []
        [ renderFinding f | f <- cxClassFindings ctx', Cm.code f == "E-ANCHOR-KIND" ]
          `shouldBe` ["E-ANCHOR-KIND core-2.01.pdf#page=201&5.3.3.6.3#a: a list item or paragraph anchor takes law, not " <> payload]

  describe "a rule reading heading units" $ do
    it "reads every bound heading, and no table" $ do
      length units `shouldBe` 2
      length (boundTables (ruleShape stub) ctx) `shouldBe` 0
    it "never binds a table, whatever its fires says: the tables classify as they do without it" $
      fst (classifyWithRules (registry <> [stub]) ir) `shouldBe` fst (classifyWithRules registry ir)
    it "locates its findings at the heading" $
      map renderFinding (ruleChecks stub ctx [])
        `shouldBe` [ "N-STUB core-2.01.pdf#page=201&5.3.3.6: SomeBinding (Behaviour \"Base.get\")"
                   , "N-STUB core-2.01.pdf#page=201&5.3.3.6.3: SomeBinding (Fails \"Base.getFails\")" ]
    it "gives its declarations the heading's provenance: the section, its title, the page, its place among the headings" $
      [ T.takeWhile (/= ',') (Gen.provenanceComment d.declProvenance) | Emit d <- take 1 (ruleCompile stub ctx []) ]
        `shouldBe` ["-- | core-2.01 5.3.3.6 Basic Table Method Group - Get (Table and Object Method) (PDF 201 / p.184"]
    it "hashes a heading as the manifest does: the sha256 of its title's normalised words" $ do
      headings <- manifestHeadings
      headings `shouldSatisfy` any (\(i, _, _) -> i == "5.1.3")
      [ (i, T.take 12 h) | (i, t, h) <- headings, headingHash t /= h ] `shouldBe` []
      [ T.take 12 d.declProvenance.cellsHash | Emit d <- take 1 (ruleCompile stub ctx []) ]
        `shouldBe` [T.take 12 (headingHash "Basic Table Method Group - Get (Table and Object Method)")]
    it "is checked by its exemplar, a heading the IR numbers once" $ do
      checkExemplarsWith [stub] ir `shouldBe` []
      map renderFinding (checkExemplarsWith [stub { exemplar = (SpecId "core-2.01", SectionId "5.3.3.99") }] ir)
        `shouldBe` ["C-EXEMPLAR core-2.01: the exemplar 5.3.3.99 of rule headingStub is not in the IR: no heading numbered 5.3.3.99 in the IR"]

  describe "a heading no rule of the registry reads" $
    it "is W-GEN-EXCLUDED at the heading, and among the units compile leaves out" $
      if any ((== Just HeadingUnit) . fmap shapeUnit . ruleShape) registry
        then pendingWith "a rule of the registry reads the heading units (the behaviours reader)"
        else do
          cr <- either (throwString . show) pure (compileIR Nothing Scott ir)
          [ renderFinding f | f <- crFindings cr, Cm.code f == "W-GEN-EXCLUDED", Cm.severity f == Warning ]
            `shouldBe` [ "W-GEN-EXCLUDED core-2.01.pdf#page=201&" <> aid
                         <> ": hsedc generates nothing for a Method heading yet (no rule of the registry reads one)"
                       | aid <- ["5.3.3.6", "5.3.3.6.3"] ]
          [ aid | (aid@(SectionId _), _) <- crExcluded cr ] `shouldBe` map bhAnchor units
