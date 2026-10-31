{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.BehavioursSpec
Description : Test specifications for the behaviours reader
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The behaviours reader on the committed Core extraction with the committed
annotations (design section 13.16, M4a): the four method headings the
behaviour comments bind (Core 5.2.2.1 Properties, PDF 140; 5.2.3.1
StartSession, PDF 149; 5.2.3.5 CloseSession, PDF 153; 5.3.3.6 Get, PDF 201)
and the one Fails list the fails comment binds (5.3.3.6.3, PDF 201, items a
to d), each read with its heading's page and the first 12 hex digits of its
signature block's hash, which the hand-written behaviour states;
C-BEHAVIOUR-BOUND refuses a name that is not qualified and a name bound
twice, C-BEHAVIOUR-METHOD a heading whose section has no signature block, or
one whose method has no UID, C-FAILS-LIST a heading that is no Fails list of
a bound method, and the unit is then left out, at its heading; the generated
module lists the four, each under its heading's provenance line, Get's with
its Fails list, names each behaviour at its method's record and imports the
modules qualified, and declares nothing the oracle compares (hsed's build
compiles it, and hsed's BehavioursSpec holds it to the dispatch lists); Opal
binds no behaviour, and gets no module.
-}
module Hsed.Spec.Rules.BehavioursSpec (spec) where

import qualified Data.Text                as DT
import qualified RIO.Text                 as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor         (AnchorId (..))
import           Hsed.Spec.Check          (checkIR, mkContext)
import           Hsed.Spec.Common         (ErrataSet (..), renderFinding)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.Compile        (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (ParaIR (..), SpecIR (..))
import           Hsed.Spec.Rules          (checkExemplarsWith)
import           Hsed.Spec.Rules.Behaviours


-- | The reader's codes.
codes :: [Text]
codes = ["C-BEHAVIOUR-BOUND", "C-BEHAVIOUR-METHOD", "C-FAILS-LIST"]

spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx = mkContext Scott ir
      compiled ir' = either (throwString . show) pure (compileIR Nothing Scott ir')
      behaviourFindings ir' = [ renderFinding f | f <- checkIR Nothing Scott ir', Cm.code f `elem` codes ]
      -- the behaviours module's exclusions alone
      excludedAt ir' = do
        cr <- compiled ir'
        pure [ renderFinding f | f <- crFindings cr, Cm.code f == "W-GEN-EXCLUDED", "left out of Hsed.Gen.Core.Behaviours" `T.isInfixOf` Cm.message f ]
      withComment aid payload = annotated pristine (withoutLine aid hsa <> "core-2.01 | " <> aid <> " | " <> payload <> "\n")
      withLine line = annotated pristine (hsa <> line <> "\n")

  describe "the behaviours reader" $ do
    it "reads the four method headings the behaviour comments bind, then Get's Fails list, in the anchors' order" $ do
      map bvAnchor (behaviourReads ctx) `shouldBe` map SectionId ["5.2.2.1", "5.2.3.1", "5.2.3.5", "5.3.3.6", "5.3.3.6.3"]
      map bvQName (behaviourReads ctx) `shouldBe`
        ["SessionManager.properties", "SessionManager.startSession", "SessionManager.closeSession", "Base.get", "Base.getFails"]
      map bvFails (behaviourReads ctx) `shouldBe` [False, False, False, False, True]
    it "gives each its heading's PDF page and the manifest's hash of its title" $
      [ ((bvProv r).pdfPage1, T.length (bvProv r).cellsHash, (bvProv r).caption) | r <- behaviourReads ctx ] `shouldBe`
        [ (140, 64, "Properties (Method)"), (149, 64, "StartSession Method"), (153, 64, "CloseSession Method")
        , (201, 64, "Basic Table Method Group - Get (Table and Object Method)"), (201, 64, "Fails") ]
    it "reads Get's Fails list's items a to d with their hashes in the IR, the first 12 hex digits the hand-written items state" $
      [ (label, T.take 12 h, T.length h) | (label, h) <- failsItemsOf ctx "5.3.3.6.3" ] `shouldBe`
        [("a", "fbdda8fb3e3a", 64), ("b", "ca5584e1ccdf", 64), ("c", "46db34a6034d", 64), ("d", "1b371d07bd3b", 64)]
    it "is checked by its exemplar, Core 5.3.3.6, a heading the IR numbers once" $
      checkExemplarsWith [behavioursRule] ir `shouldBe` []
    it "names each behaviour's module and refers to it by its qualified name" $ do
      map (behaviourModuleName . bvQName) (behaviourReads ctx) `shouldBe`
        ["Hsed.Sim.SessionManager", "Hsed.Sim.SessionManager", "Hsed.Sim.SessionManager", "Hsed.Sim.Base", "Hsed.Sim.Base"]
      behaviourReference "Base.get" `shouldBe` "Hsed.Sim.Base.get"
      behaviourModuleName "Session.Manager.startSession" `shouldBe` "Hsed.Sim.Session.Manager"
    it "finds a Fails list's method heading, its parent section" $
      map parentSection ["5.3.3.6.3", "5.2.2.1", "5"] `shouldBe` [Just "5.3.3.6", Just "5.2.2", Nothing]

  describe "C-BEHAVIOUR-BOUND, C-BEHAVIOUR-METHOD and C-FAILS-LIST" $ do
    it "pass on the committed annotations, and nothing of the behaviours module is left out" $ do
      behaviourFindings ir `shouldBe` []
      excludedAt ir `shouldReturn` []
    it "C-BEHAVIOUR-BOUND refuses a name that is not qualified (the negative fixture behaviour-qname.hsa's), and the heading is left out" $ do
      ir' <- withComment "5.3.3.6" "behaviour \"base.get\""
      behaviourFindings ir' `shouldBe`
        [ "C-BEHAVIOUR-BOUND core-2.01.pdf#page=201&5.3.3.6: the comment's name \"base.get\" is not a qualified name, Module.name (its modules capitalised, its name a variable)" ]
      newlyExcluded ir ir' `shouldReturn`
        [(SectionId "5.3.3.6", "C-BEHAVIOUR-BOUND failed"), (SectionId "5.3.3.6.3", "its method's heading 5.3.3.6 is left out")]
      excludedAt ir' `shouldReturn`
        [ "W-GEN-EXCLUDED core-2.01.pdf#page=201&5.3.3.6: left out of Hsed.Gen.Core.Behaviours: C-BEHAVIOUR-BOUND failed"
        , "W-GEN-EXCLUDED core-2.01.pdf#page=201&5.3.3.6.3: left out of Hsed.Gen.Core.Behaviours: its method's heading 5.3.3.6 is left out" ]
    it "C-BEHAVIOUR-BOUND refuses a name bound twice, at each unit after the first, behaviours and Fails lists together" $ do
      twice <- withComment "5.2.3.5" "behaviour \"SessionManager.properties\""
      behaviourFindings twice `shouldBe`
        [ "C-BEHAVIOUR-BOUND core-2.01.pdf#page=153&5.2.3.5: the name SessionManager.properties is bound already, by the comment at 5.2.2.1" ]
      newlyExcluded ir twice `shouldReturn` [(SectionId "5.2.3.5", "C-BEHAVIOUR-BOUND failed")]
      asFails <- withComment "5.3.3.6.3" "fails \"Base.get\""
      behaviourFindings asFails `shouldBe`
        [ "C-BEHAVIOUR-BOUND core-2.01.pdf#page=201&5.3.3.6.3: the name Base.get is bound already, by the comment at 5.3.3.6" ]
    it "C-BEHAVIOUR-METHOD refuses a heading whose section has no signature block (behaviour-nosig.hsa's), and one whose method has no UID" $ do
      noSig <- withLine "core-2.01 | 5.2.2.3 | behaviour \"SessionManager.setHostProperties\""
      behaviourFindings noSig `shouldBe`
        [ "C-BEHAVIOUR-METHOD core-2.01.pdf#page=143&5.2.2.3: the section 5.2.2.3 has no signature block: a behaviour binds a method's heading, whose section gives the method's signature" ]
      newlyExcluded ir noSig `shouldReturn` [(SectionId "5.2.2.3", "C-BEHAVIOUR-METHOD failed")]
      stir <- withLine "core-2.01 | 5.6.4.2 | behaviour \"Random.stir\""
      behaviourFindings stir `shouldBe`
        [ "C-BEHAVIOUR-METHOD core-2.01.pdf#page=252&5.6.4.2: the method Stir of 5.6.4.2/sig has no UID (C-SIG-METHOD), so no instance Method runs it" ]
    it "C-BEHAVIOUR-METHOD refuses a heading whose signature block no rule binds" $ do
      unbound <- annotated pristine (T.unlines (map (DT.replace " & rule sig" "") (T.lines hsa)))
      behaviourFindings unbound `shouldSatisfy` elem
        "C-BEHAVIOUR-METHOD core-2.01.pdf#page=201&5.3.3.6: the signature block 5.3.3.6/sig is bound by no rule, so no method record is generated for it"
    it "C-FAILS-LIST refuses a heading that is no Fails list (fails-heading.hsa's), and one whose method heading is bound by no behaviour" $ do
      notFails <- withLine "core-2.01 | 5.3.3.6.2 | fails \"Base.getResultFails\""
      behaviourFindings notFails `shouldBe`
        [ "C-FAILS-LIST core-2.01.pdf#page=201&5.3.3.6.2: the heading 5.3.3.6.2 is titled \"Get Result\", not Fails: a fails comment binds a method's Fails list"
        , "C-FAILS-LIST core-2.01.pdf#page=201&5.3.3.6.2: the section has no labelled list item in the IR: a Fails list's items are a, b, ... (docs/ir-v1.md)" ]
      newlyExcluded ir notFails `shouldReturn` [(SectionId "5.3.3.6.2", "C-FAILS-LIST failed")]
      orphan <- withLine "core-2.01 | 5.3.3.5.3 | fails \"Base.deleteRowFails\""
      behaviourFindings orphan `shouldBe`
        [ "C-FAILS-LIST core-2.01.pdf#page=201&5.3.3.5.3: its method's heading 5.3.3.5 is bound by no behaviour comment: a Fails list is bound beside the method the simulator runs" ]
    it "C-FAILS-LIST refuses an item the IR gives no hash, and the Fails list is left out" $ do
      let unhashed = ir { paras = [ if p.section == Just "5.3.3.6.3" && p.item == Just "b" then p { hash = "" } else p | p <- ir.paras ] }
      behaviourFindings unhashed `shouldBe` [ "C-FAILS-LIST core-2.01.pdf#page=201&5.3.3.6.3: the list item b has no sha256 in the IR (\"\")" ]
      newlyExcluded ir unhashed `shouldReturn` [(SectionId "5.3.3.6.3", "C-FAILS-LIST failed")]
    it "leaves a Fails list out with its method's heading, saying so, and a Fails list bound twice at the second" $ do
      ir' <- withComment "5.3.3.6" "behaviour \"base.get\""
      cr <- compiled ir'
      lookup (SectionId "5.3.3.6.3") (crExcluded cr) `shouldBe` Just "its method's heading 5.3.3.6 is left out"
      -- a second Fails list under one method: 5.3.3.6's has only one Fails heading, so the second is 5.3.3.5's
      -- wrongly numbered under it; the IR cannot say that, so the check is of the committed shape alone
      map parentSection ["5.3.3.6.3"] `shouldBe` [Just "5.3.3.6"]

  describe "the generated module" $ do
    cr <- runIO (compiled ir)
    let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Behaviours" (crModules cr)))
    it "declares behaviours :: [BehaviourRef], one entry per method heading, each under its heading's provenance line" $ do
      lns `shouldContain` [ "behaviours :: [BehaviourRef]", "behaviours ="
                          , "  [ -- core-2.01 5.2.2.1 Properties (Method) (PDF 140 / p.123, heading 364, sha256:" <> T.take 12 (hashOf ctx "5.2.2.1") <> ")"
                          , "    BehaviourRef" ]
      [ l | l <- lns, "  , -- core-2.01 " `T.isPrefixOf` l ] `shouldBe`
        [ "  , -- core-2.01 5.2.3.1 StartSession Method (PDF 149 / p.132, heading 390, sha256:" <> T.take 12 (hashOf ctx "5.2.3.1") <> ")"
        , "  , -- core-2.01 5.2.3.5 CloseSession Method (PDF 153 / p.136, heading 426, sha256:" <> T.take 12 (hashOf ctx "5.2.3.5") <> ")"
        , "  , -- core-2.01 5.3.3.6 Basic Table Method Group - Get (Table and Object Method) (PDF 201 / p.184, heading 799, sha256:" <> T.take 12 (hashOf ctx "5.3.3.6") <> ")" ]
      length [ l | l <- lns, T.strip l == "BehaviourRef" ] `shouldBe` 4
    it "gives each entry its anchor, PDF page, signature hash, name, and the behaviour that name refers to at its method's record" $ do
      lns `shouldContain`
        [ "      { brAnchor    = \"core-2.01:5.2.2.1\"", "      , brPage      = 140", "      , brHash      = \"4096cbe61646\""
        , "      , brName      = \"SessionManager.properties\""
        , "      , brBehaviour = forMethod @Hsed.Gen.Core.Methods.Properties Hsed.Sim.SessionManager.properties"
        , "      , brFails     = Nothing", "      }" ]
      lns `shouldContain` [ "      , brHash      = \"24fca697f37f\"", "      , brName      = \"SessionManager.startSession\"" ]
      lns `shouldContain` [ "      , brHash      = \"993210934add\"", "      , brName      = \"SessionManager.closeSession\"" ]
    it "gives Get its Fails list under the Fails heading's provenance line: anchor, page, name, items a to d, and the Fails at the record" $
      lns `shouldContain`
        [ "      { brAnchor    = \"core-2.01:5.3.3.6\"", "      , brPage      = 201", "      , brHash      = \"9d67d740e4cb\""
        , "      , brName      = \"Base.get\"", "      , brBehaviour = forMethod @Hsed.Gen.Core.Methods.Get Hsed.Sim.Base.get"
        , "        -- core-2.01 5.3.3.6.3 Fails (PDF 201 / p.184, heading 804, sha256:" <> T.take 12 (hashOf ctx "5.3.3.6.3") <> ")"
        , "      , brFails     = Just FailsRef"
        , "          { frAnchor = \"core-2.01:5.3.3.6.3\"", "          , frPage   = 201", "          , frName   = \"Base.getFails\""
        , "          , frItems  ="
        , "              [ (\"a\", \"fbdda8fb3e3a\")", "              , (\"b\", \"ca5584e1ccdf\")", "              , (\"c\", \"46db34a6034d\")", "              , (\"d\", \"1b371d07bd3b\")", "              ]"
        , "          , frFails  = forFails @Hsed.Gen.Core.Methods.Get Hsed.Sim.Base.getFails"
        , "          }", "      }" ]
    it "imports the behaviour types, the methods module and each behaviour's module qualified, and needs TypeApplications" $ do
      filter ("import" `T.isPrefixOf`) lns `shouldBe`
        [ "import qualified Hsed.Gen.Core.Methods"
        , "import qualified Hsed.Sim.Base"
        , "import           Hsed.Sim.Behaviour (BehaviourRef (..), FailsRef (..), forFails, forMethod)"
        , "import qualified Hsed.Sim.SessionManager"
        , "import           Prelude            (Maybe (..))" ]
      lns `shouldContain` ["{-# LANGUAGE TypeApplications #-}"]
    it "declares nothing the oracle compares: no UID, no name map, no data type, nothing under a table's provenance" $ do
      [ l | l <- lns, any (`T.isSuffixOf` l) [":: UID", ":: HalfUID", ":: Map HalfUID String", ":: Map UID String", ":: [(UID, String)]"]
                      || "data " `T.isPrefixOf` l ] `shouldBe` []
      [ l | l <- lns, "-- | core-2.01 T" `T.isPrefixOf` l ] `shouldBe` []
    it "is the committed hsed/gen/Hsed/Gen/Core/Behaviours.hs" $ do
      file <- readFileUtf8 (Gen.modulePath "../hsed/gen" "Hsed.Gen.Core.Behaviours")
      T.lines file `shouldBe` lns

  describe "a spec that binds no behaviour" $
    withOpal $ \opalPristine ->
      it "gets no Behaviours module: Opal, read over Core's base" $ do
        opal <- committedOpalHsa >>= annotated opalPristine
        cr <- either (throwString . show) pure (compileIR (Just ctx) Scott opal)
        filter ("Behaviours" `T.isSuffixOf`) (map fst (crModules cr)) `shouldBe` []
  where
    -- the manifest's hash of a heading: that of the unit the reader gives it
    hashOf ctx s = fromMaybe "" (listToMaybe [ (bvProv r).cellsHash | r <- behaviourReads ctx, bvSection r == s ])
