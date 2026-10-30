{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.LawsSpec
Description : Test specifications for the law reader
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The law reader on the committed Core extraction with the committed
annotations (design example E10, M3): the three sentences the law comments
bind (Core 3.3.10.2.1 item 3a, PDF 82; 3.3.10.6 item d.c and 3.3.10.7 item a,
PDF 86), then Table 45, which its stateTable comment binds (PDF 83), each with
the first 12 hex digits of its IR hash, which the hand-written laws state;
C-LAW-BOUND refuses a name that is not qualified and a name bound twice,
C-LAW-HASH a sentence the IR gives no hash, and the unit is then left out, at
its paragraph or table; the generated module lists the four, each under its
provenance line, refers to each law by its qualified name and imports its
module qualified, in ASCII, and declares nothing the oracle compares (hsed's
build compiles it, and hsed's LawsSpec holds it to the laws); Opal binds no
law, and gets no module.
-}
module Hsed.Spec.Rules.LawsSpec (spec) where

import qualified RIO.Text           as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor   (AnchorId (..))
import           Hsed.Spec.Check    (checkIR, mkContext)
import           Hsed.Spec.Common   (ErrataSet (..), renderFinding)
import qualified Hsed.Spec.Common   as Cm
import           Hsed.Spec.Compile  (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import qualified Hsed.Spec.Gen      as Gen
import           Hsed.Spec.IR       (ParaIR (..), SpecIR (..))
import           Hsed.Spec.Rules    (checkExemplarsWith)
import           Hsed.Spec.Rules.Laws


spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx = mkContext Scott ir
      compiled ir' = either (throwString . show) pure (compileIR Nothing Scott ir')
      lawFindings ir' = [ renderFinding f | f <- checkIR Nothing Scott ir', Cm.code f `elem` ["C-LAW-BOUND", "C-LAW-HASH"] ]
      -- the law module's exclusions alone: since M4a the heading units no reader reads yet are W-GEN-EXCLUDED too
      excludedAt ir' = do
        cr <- compiled ir'
        pure [ renderFinding f | f <- crFindings cr, Cm.code f == "W-GEN-EXCLUDED", "left out of Hsed.Gen.Core.Laws" `T.isInfixOf` Cm.message f ]
      withComment aid payload = annotated pristine (withoutLine aid hsa <> "core-2.01 | " <> aid <> " | " <> payload <> "\n")

  describe "the law reader" $ do
    it "reads the three sentences the law comments bind, then Table 45, which its stateTable comment binds" $ do
      map lwAnchor (lawReads ctx) `shouldBe` [ItemId "3.3.10.2.1" "3a", ItemId "3.3.10.6" "dc", ItemId "3.3.10.7" "a", TableId 45]
      map lwQName (lawReads ctx) `shouldBe` ["Sync.ifSendWhileBusy", "Sync.discardToS1", "Sync.ignorePacket", "Sync.table45"]
      map lwTable (lawReads ctx) `shouldBe` [False, False, False, True]
    it "gives each its PDF page and its hash in the IR, of which the hand-written law states the first 12 hex digits" $
      [ ((lwProv r).pdfPage1, T.take 12 (lwProv r).cellsHash, T.length (lwProv r).cellsHash) | r <- lawReads ctx ] `shouldBe`
        [(82, "37bedfb19343", 64), (86, "cb4d5d489fc0", 64), (86, "cda4e92af591", 64), (83, "498ea00e3c6a", 64)]
    it "reads no state table the stateTable reader leaves out (C-STATE-TABLE's), and checks none" $ do
      ir' <- withComment "T45" "stateTable \"table45\""
      map lwAnchor (lawReads (mkContext Scott ir')) `shouldBe` [ItemId "3.3.10.2.1" "3a", ItemId "3.3.10.6" "dc", ItemId "3.3.10.7" "a"]
      lawFindings ir' `shouldBe` []
    it "is checked by its exemplar, Core 3.3.10.2.1 item 3a, a list item the IR has once" $
      checkExemplarsWith [lawsRule] ir `shouldBe` []
    it "names each law's module and refers to it by its qualified name" $ do
      map (lawModuleName . lwQName) (lawReads ctx) `shouldBe` replicate 4 "Hsed.Law.Sync"
      map (lawReference . lwQName) (take 1 (lawReads ctx)) `shouldBe` ["Hsed.Law.Sync.ifSendWhileBusy"]
      lawModuleName "Session.Manager.startSession" `shouldBe` "Hsed.Law.Session.Manager"
    it "takes a qualified name alone: ASCII identifiers, the modules capitalised, the name a variable and no keyword" $
      map qualifiedLaw ["Sync.ifSendWhileBusy", "Sync.Sub.x'", "Sync._x", "sync.x", "Sync", "Sync.X", "Sync.type", "Sync.if-send", "Sync.\x00e9", ".x", "Sync."]
        `shouldBe` [True, True, True, False, False, False, False, False, False, False, False]

  describe "C-LAW-BOUND and C-LAW-HASH" $ do
    it "pass on the committed annotations" $
      lawFindings ir `shouldBe` []
    it "C-LAW-BOUND refuses a name that is not qualified (the negative fixture law-qname.hsa's), and the sentence is left out" $ do
      ir' <- withComment "3.3.10.2.1#3a" "law \"sync.x\""
      lawFindings ir' `shouldBe`
        [ "C-LAW-BOUND core-2.01.pdf#page=82&3.3.10.2.1#3a: the comment's name \"sync.x\" is not a qualified name, Module.name (its modules capitalised, its name a variable)" ]
      newlyExcluded ir ir' `shouldReturn` [(ItemId "3.3.10.2.1" "3a", "C-LAW-BOUND failed")]
      excludedAt ir' `shouldReturn` ["W-GEN-EXCLUDED core-2.01.pdf#page=82&3.3.10.2.1#3a: left out of Hsed.Gen.Core.Laws: C-LAW-BOUND failed"]
    it "C-LAW-BOUND refuses a name bound twice, at each unit after the first, sentence or state table" $ do
      twice <- withComment "3.3.10.6#dc" "law \"Sync.ifSendWhileBusy\""
      lawFindings twice `shouldBe`
        [ "C-LAW-BOUND core-2.01.pdf#page=86&3.3.10.6#dc: the name Sync.ifSendWhileBusy is bound already, by the comment at 3.3.10.2.1#3a" ]
      newlyExcluded ir twice `shouldReturn` [(ItemId "3.3.10.6" "dc", "C-LAW-BOUND failed")]
      table <- withComment "3.3.10.7#a" "law \"Sync.table45\""
      lawFindings table `shouldBe`
        [ "C-LAW-BOUND core-2.01.pdf#page=83&T45: the name Sync.table45 is bound already, by the comment at 3.3.10.7#a" ]
      excludedAt table `shouldReturn` ["W-GEN-EXCLUDED core-2.01.pdf#page=83&T45: left out of Hsed.Gen.Core.Laws: C-LAW-BOUND failed"]
    it "C-LAW-HASH refuses a sentence the IR gives no hash, and the sentence is left out" $ do
      let unhashed = ir { paras = [ if p.section == Just "3.3.10.2.1" && p.item == Just "3a" then p { hash = "" } else p | p <- ir.paras ] }
      lawFindings unhashed `shouldBe` [ "C-LAW-HASH core-2.01.pdf#page=82&3.3.10.2.1#3a: the paragraph has no sha256 in the IR (\"\")" ]
      newlyExcluded ir unhashed `shouldReturn` [(ItemId "3.3.10.2.1" "3a", "C-LAW-HASH failed")]

  describe "the generated module" $ do
    cr <- runIO (compiled ir)
    let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Laws" (crModules cr)))
    it "declares laws :: [LawRef], one entry per unit, each under its provenance line" $ do
      lns `shouldContain` ["laws :: [LawRef]", "laws =", "  [ -- core-2.01 3.3.10.2.1#3a list item 3a (PDF 82 / p.65, ord 911, sha256:37bedfb19343)", "    LawRef"]
      [ l | l <- lns, "  , -- core-2.01 " `T.isPrefixOf` l ] `shouldBe`
        [ "  , -- core-2.01 3.3.10.6#dc list item dc (PDF 86 / p.69, ord 953, sha256:cb4d5d489fc0)"
        , "  , -- core-2.01 3.3.10.7#a list item a (PDF 86 / p.69, ord 970, sha256:cda4e92af591)"
        , "  , -- core-2.01 T45 IF-RECV ComPacket Field Values Summary (PDF 83 / p.66, ord 47, sha256:498ea00e3c6a)" ]
    it "gives each entry its anchor, PDF page, hash, name and the law that name refers to" $ do
      lns `shouldContain`
        [ "      { lrAnchor = \"core-2.01:3.3.10.2.1#3a\"", "      , lrPage   = 82", "      , lrHash   = \"37bedfb19343\""
        , "      , lrName   = \"Sync.ifSendWhileBusy\"", "      , lrLaw    = Hsed.Law.Sync.ifSendWhileBusy", "      }" ]
      lns `shouldContain`
        [ "      { lrAnchor = \"core-2.01:T45\"", "      , lrPage   = 83", "      , lrHash   = \"498ea00e3c6a\""
        , "      , lrName   = \"Sync.table45\"", "      , lrLaw    = Hsed.Law.Sync.table45", "      }" ]
      length [ l | l <- lns, T.strip l == "LawRef" ] `shouldBe` 4
    it "imports LawRef and each law's module, qualified" $
      filter ("import" `T.isPrefixOf`) lns `shouldBe` ["import           Hsed.Law (LawRef (..))", "import qualified Hsed.Law.Sync"]
    it "declares nothing the oracle compares: no UID, no name map, no data type" $
      [ l | l <- lns, any (`T.isSuffixOf` l) [":: UID", ":: HalfUID", ":: Map HalfUID String", ":: Map UID String", ":: [(UID, String)]"]
                      || "data " `T.isPrefixOf` l ] `shouldBe` []
    it "is the committed hsed/gen/Hsed/Gen/Core/Laws.hs" $ do
      file <- readFileUtf8 (Gen.modulePath "../hsed/gen" "Hsed.Gen.Core.Laws")
      T.lines file `shouldBe` lns

  describe "a spec that binds no law" $
    withOpal $ \opalPristine ->
      it "gets no Laws module: Opal, read over Core's base" $ do
        opal <- committedOpalHsa >>= annotated opalPristine
        cr <- either (throwString . show) pure (compileIR (Just ctx) Scott opal)
        filter ("Laws" `T.isSuffixOf`) (map fst (crModules cr)) `shouldBe` []
