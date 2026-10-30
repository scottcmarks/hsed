{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.SignatureUnitSpec
Description : The signature unit: how a rule binds and reads the method signature blocks
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The method signature blocks of the IR (@sigs@, docs/ir-v1.md) are units a rule
binds the way it binds numbered tables, by its scope alone (they carry no
annotation): "Hsed.Spec.Rules".'classifySignatures' binds each block a rule
anchor covers to the rule whose shape takes signature units, and the rule
reads them from the 'Context' as 'BoundSig's. A stub rule stands in for the
registered signature rule (taken out of the registry here), proving the
interface: the scope from a doc
token or a heading, @W-UNBOUND@ for a block no rule covers (a Warning through
M2b, like an unbound table), @E-AMBIGUOUS@ for two such rules, a rule that
never fires on a table, a signature exemplar, findings and declarations
located at the block, and the blocks in the ledger and among the orphans.
-}
module Hsed.Spec.SignatureUnitSpec (spec) where

import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Text          as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Check   (mkContextWith)
import           Hsed.Spec.Common  (ErrataSet (..), Finding, RuleName (..), Severity (..), SpecId (..), finding,
                                    renderFinding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL     (SomeBinding (..), sig)
import qualified Hsed.Spec.Gen     as Gen
import           Hsed.Spec.IR      (SigIR (..), SpecIR (..))
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.Report  (Ledger (..), ledger, orphans)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules   (checkExemplarsWith, classifySignatures, classifySignaturesWith, classifyWithRules,
                                    explain)
import           Hsed.Spec.Shape   (UnitKind (..), shapeUnit)


-- | What the rule binding signature units looks like: an ordinary 'RuleDef' of
--   the signature shape, which never fires on a table and reads its units with 'boundSigs'.
stub :: RuleDef
stub = RuleDef
  { ruleName    = RuleName "sig"
  , bindsTo     = SomeBinding sig
  , exemplar    = (SpecId "core-2.01", SigId "5.3.3.6")
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ _ -> False
  , ruleIdent   = const Nothing
  , ruleCodes   = []
  , ruleModule  = Gen.GenModule { Gen.leaf = "Methods", Gen.description = "Methods", Gen.notes = [] }
  , ruleChecks  = \ctx _ -> [ atSig ctx bs (finding Note "N-STUB" (T.intercalate ", " (bsSig bs).heads)) | bs <- boundSigs (ruleShape stub) ctx ]
  , ruleCompile = \ctx _ -> [ Emit (Gen.decl (sigProvenanceOf ctx bs) "m" [Gen.provenanceComment (sigProvenanceOf ctx bs)])
                            | bs <- boundSigs (ruleShape stub) ctx ]
  }

-- | The registry without the rules that take signature units: the stub is the only one.
tableRules :: [RuleDef]
tableRules = [ r | r <- registry, fmap shapeUnit (ruleShape r) /= Just SignatureUnit ]

unbound :: [Finding] -> [Text]
unbound fs = [ a | f <- fs, Cm.code f == "W-UNBOUND", Cm.severity f == Warning, Just a <- [Cm.anchor f] ]

spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  let docLine payload = annotated pristine (T.unlines (("core-2.01 | doc | " <> payload) : drop 1 (T.lines hsa)))
      bound classes = [ aid | (aid, RuleBound (RuleName "sig") [] _) <- Map.toList classes ]

  describe "signatureUnits" $
    it "gives every block of the IR its anchor id, <section>/sig" $ do
      let units = signatureUnits pristine
      Map.size units `shouldBe` length pristine.sigs
      fmap (.heads) (Map.lookup (SigId "5.3.3.6") units) `shouldBe` Just ["TableUID.Get", "ObjectUID.Get"]
      fmap (.page) (Map.lookup (SigId "5.2.3.5") units) `shouldBe` Just 152

  describe "classifySignaturesWith" $ do
    it "binds every block to the signature rule from a token on the doc anchor" $ do
      ir <- docLine "doc core201 & rule sig"
      let (classes, fs) = classifySignaturesWith (tableRules <> [stub]) ir
      bound classes `shouldBe` Map.keys (signatureUnits ir)
      fs `shouldBe` []
    it "binds only the blocks under a heading the rule is anchored at, the others W-UNBOUND at the block" $ do
      ir <- annotated pristine "core-2.01 | doc | doc core201\ncore-2.01 | 5.3.3 | rule sig\n"
      let (classes, fs) = classifySignaturesWith [stub] ir
          under = [ aid | aid@(SigId s) <- Map.keys classes, "5.3.3." `T.isPrefixOf` s ]
      bound classes `shouldBe` under
      length under `shouldBe` 18
      L.sort (unbound fs) `shouldBe` L.sort [ renderAnchorId aid | (aid, Unbound) <- Map.toList classes ]
      filter ("5.2.2.1/sig" `T.isInfixOf`) (map renderFinding fs)
        `shouldBe` ["W-UNBOUND core-2.01.pdf#page=140&5.2.2.1/sig: no rule in scope binds this signature block (SMUID.Properties)"]
    it "leaves every block unbound when no rule takes signature units, the rule token being only W-RULE-UNIMPLEMENTED" $ do
      ir <- docLine "doc core201 & rule sig"
      let (classes, fs) = classifySignaturesWith tableRules ir
      L.nub (Map.elems classes) `shouldBe` [Unbound]
      length (unbound fs) `shouldBe` Map.size classes
      [ Cm.code f | f <- snd (classifyWithRules tableRules ir), "rule sig" `T.isInfixOf` Cm.message f ] `shouldBe` ["W-RULE-UNIMPLEMENTED"]
    it "makes two rules taking signature units E-AMBIGUOUS" $ do
      ir <- docLine "doc core201 & rule sig & rule tableDesc"
      let other = stub { ruleName = RuleName "tableDesc" }
          (classes, fs) = classifySignaturesWith [stub, other] ir
      Map.lookup (SigId "5.3.3.6") classes `shouldBe` Just (Ambiguous [RuleName "sig", RuleName "tableDesc"])
      length [ f | f <- fs, Cm.code f == "E-AMBIGUOUS" ] `shouldBe` Map.size classes
    it "never binds a table: the tables classify as they do without the rule" $ do
      ir <- docLine "doc core201 & rule tableUIDs & rule sig"
      fst (classifyWithRules (tableRules <> [stub]) ir) `shouldBe` fst (classifyWithRules tableRules ir)

  describe "a rule reading signature units" $ do
    ir <- runIO (docLine "doc core201 & rule tableUIDs & rule sig")
    let ctx = mkContextWith (tableRules <> [stub]) Scott ir
        sigs' = boundSigs (ruleShape stub) ctx
    it "reads every bound block, and no table" $ do
      map bsAnchor sigs' `shouldBe` Map.keys (signatureUnits ir)
      map bsBinding (take 1 sigs') `shouldBe` [SomeBinding sig]
      length (boundTables (ruleShape stub) ctx) `shouldBe` 0
    it "locates its findings at the block" $
      filter ("5.3.3.6" `T.isInfixOf`) (map renderFinding (ruleChecks stub ctx []))
        `shouldBe` ["N-STUB core-2.01.pdf#page=201&5.3.3.6/sig: TableUID.Get, ObjectUID.Get"]
    it "gives its declarations the block's provenance" $
      [ T.takeWhile (/= ':') (Gen.provenanceComment d.declProvenance) | Emit d <- ruleCompile stub ctx [], d.declProvenance.tableId == "5.3.3.6/sig" ]
        `shouldBe` ["-- | core-2.01 5.3.3.6/sig TableUID.Get, ObjectUID.Get (PDF 201 / p.184, ord 12, sha256"]
    it "is checked by its exemplar, a signature block" $ do
      checkExemplarsWith [stub] ir `shouldBe` []
      map Cm.code (checkExemplarsWith [stub { exemplar = (SpecId "core-2.01", SigId "9.9") }] ir) `shouldBe` ["C-EXEMPLAR"]
    it "puts the W-UNBOUND of the blocks it does not bind among the context's findings" $ do
      ir' <- annotated pristine "core-2.01 | doc | doc core201\ncore-2.01 | 5.3.3 | rule sig\n"
      length (unbound (cxClassFindings (mkContextWith [stub] Scott ir')) `L.intersect` map renderAnchorId (Map.keys (signatureUnits ir')))
        `shouldBe` Map.size (signatureUnits ir') - 18

  describe "the reports" $ do
    it "count the blocks in the ledger and list the unbound ones among the orphans" $ do
      ir <- annotated pristine hsa
      let l = ledger ir
          sigOrphans = [ aid | (aid@(SigId _), _) <- orphans ir ]
          classes = fst (classifySignatures ir)
      lSigs l `shouldBe` length ir.sigs
      (lSigsBound l + lSigsUnbound l + lSigsAmbiguous l) `shouldBe` lSigs l
      sigOrphans `shouldBe` [ aid | (aid, c) <- Map.toList classes, c == Unbound || isAmbiguous c ]
    it "explain a block by its classification" $ do
      ir <- annotated pristine hsa
      explain ir (SigId "9.9") `shouldBe` "no signature block 9.9/sig in core-2.01"
      explain ir (SigId "5.3.3.6") `shouldSatisfy` (`elem` ["unbound", "sig (exemplar core 5.3.3.6/sig)"])
  where
    isAmbiguous c = case c of
      Ambiguous _ -> True
      _           -> False
