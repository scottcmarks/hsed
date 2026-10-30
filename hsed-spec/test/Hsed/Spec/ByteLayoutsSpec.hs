{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.ByteLayoutsSpec
Description : The three byte-layout rules together: byteGrid, feature and byteFieldValue
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What holds between the rules of design section 11 M2c once all three are
registered, on the committed Core extraction and annotations: @feature@
refines @byteGrid@, so Tables 42 and 43 are @feature@'s alone, and stay so
were @byteGrid@'s shape to take them too; @feature@ reads its template,
Table 41, through @byteGrid@'s binding of it; @byteGrid@ and
@byteFieldValue@ write one module, "Hsed.Gen.Core.Layouts", their layouts and
records named by the same functions ("Hsed.Spec.Readers.Record"), each name
declared once, and a name the two would share is @C-GRID-NAME@ at the
@byteFieldValue@ table, as is a @byteGrid@ name that only the
@byteFieldValue@ declarations import; the names a module imports are each
import's, a type's members taken from the runtime's @Data@ (every type the
generated layouts and features import with @(..)@ known); all three rules'
records import the one runtime module
named in "Hsed.Spec.Gen"; and the three give @C-LAYOUT-TILE@ by one policy, a
gap a Warning and an overlap an Error, whichever rule binds the table.
-}
module Hsed.Spec.ByteLayoutsSpec (spec) where

import qualified Data.Text                      as DT
import qualified RIO.Char                       as C
import qualified RIO.List                       as L
import qualified RIO.Map                        as Map
import qualified RIO.Text                       as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Layout               (tiles)
import           Hsed.Spec.Anchor               (AnchorId (..))
import           Hsed.Spec.Check                (checkIR, mkContext)
import           Hsed.Spec.Common               (ErrataSet (..), RuleName (..), Severity (..), SpecId (..), renderFinding)
import qualified Hsed.Spec.Common               as Cm
import           Hsed.Spec.Compile              (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import qualified Hsed.Spec.Gen                  as Gen
import           Hsed.Spec.Readers.Grid         (Grid (..), GridField (..), gridOf)
import           Hsed.Spec.Registry             (registry)
import           Hsed.Spec.RuleDef              (BoundTable (..), Classification (..), Emitted (..), RuleDef (..),
                                                 boundTables)
import           Hsed.Spec.Rules                (classify, classifyWithRules)
import           Hsed.Spec.Readers.Record       (Structure (..), declScope, layoutValue, recordNames, typeName)
import           Hsed.Spec.Rules.ByteFieldValue (BytesRead (..), bytesReads)
import           Hsed.Spec.Rules.ByteGrid       (byteGridRule, gridStructures)
import           Hsed.Spec.Rules.Feature        (Template (..), templateOf)
import           Hsed.Spec.Shape                (Form (..), Shape (..))


-- | The tables a classification binds to a rule, by number.
boundTo :: Text -> Map AnchorId Classification -> [Int]
boundTo r classes = [ n | (TableId n, RuleBound (RuleName r') _ _) <- Map.toList classes, r' == r ]

-- | The names a line of a module declares at the top level: a value's
--   signature (@comPacketLayout :: Layout@) or a data type (@data ComPacket = ComPacket@).
declared :: Text -> [Text]
declared l = case T.words l of
  "data" : n : _                                     -> [n]
  n : "::" : _ | not (T.null n), not (" " `T.isPrefixOf` l) -> [n]
  _                                                  -> []

spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx = mkContext Scott ir
      classes = classify ir
  cr <- runIO (either (throwString . show) pure (compileIR Scott ir))
  let layouts = fromMaybe "" (lookup "Hsed.Gen.Core.Layouts" (crModules cr))
      names = concatMap declared (T.lines layouts)

  describe "feature and byteGrid" $ do
    it "bind Tables 42 and 43 to feature alone, and byteGrid's nine grids to byteGrid" $ do
      boundTo "feature" classes `shouldBe` [42, 43]
      boundTo "byteGrid" classes `shouldBe` [17, 18, 20, 21, 38, 39, 41, 44, 212]
      [ aid | (aid, Ambiguous _) <- Map.toList classes ] `shouldBe` []
    it "never both bind them: were byteGrid to fire on every byte grid, feature, which refines it, still wins" $ do
      let wide = byteGridRule { fires = \_ _ form -> fGrid8 form }
          everyRule = [ if ruleName r == RuleName "byteGrid" then wide else r | r <- registry ]
          (wider, _) = classifyWithRules everyRule ir
      boundTo "feature" wider `shouldBe` [42, 43]
      boundTo "byteGrid" wider `shouldBe` [17, 18, 20, 21, 38, 39, 41, 44, 212]
      [ aid | (aid, Ambiguous _) <- Map.toList wider ] `shouldBe` []
    it "lets feature read its template, Table 41, as byteGrid binds and reads it" $
      case ([ bt | bt <- boundTables (Just ByteGrid) ctx, btAnchor bt == TableId 41 ], templateOf ctx "TPer Feature Descriptor") of
        ([t41], Right tm) -> do
          tmNumber tm `shouldBe` 41
          tmSize tm `shouldBe` gSize (gridOf t41)
          tmFields tm `shouldBe` [ f | f <- gFields (gridOf t41), isJust (gfLast f) ]
        (bts, tm) -> expectationFailure ("T41 bound " <> show (length bts) <> " times; the template: " <> either T.unpack (const "read") tm)

  describe "byteGrid and byteFieldValue in the one layouts module" $ do
    let grids = gridStructures ctx
        bytes = map bfStructure (bytesReads ctx)
        records = [ st | st <- grids <> bytes, tiles (stLayout st) ]
    it "name every layout and record by the one record printer" $ do
      L.sort [ n | n <- names, "Layout" `T.isSuffixOf` n ] `shouldBe` L.sort (map layoutValue (grids <> bytes))
      [ typeName r | r <- records, typeName r `notElem` names ] `shouldBe` []
      map layoutValue (take 2 bytes) `shouldBe` ["get_COMIDPayloadLayout", "noResponseAvailableLayout"]
    it "declare each name once, the two rules' names distinct" $ do
      [ n | (n, k) <- Map.toList (Map.fromListWith (+) [ (n, 1 :: Int) | n <- names ]), k > 1 ] `shouldBe` []
      let ours = concatMap recordNames grids
          theirs = concatMap recordNames bytes
      filter (`elem` ours) theirs `shouldBe` []
    it "find a name byteFieldValue would share with byteGrid (C-GRID-NAME, an Error at the byteFieldValue table, which is left out)" $ do
      clash <- annotated pristine (hsa <> "core-2.01 | T28 | layout \"ComPacket\"\n")
      [ (Cm.severity f, renderFinding f) | f <- checkIR Scott clash, Cm.code f == "C-GRID-NAME" ] `shouldBe`
        [ (Error, "C-GRID-NAME core-2.01.pdf#page=61&T28: ComPacket is also a name of an earlier grid's declarations")
        , (Error, "C-GRID-NAME core-2.01.pdf#page=61&T28: comPacketLayout is also a name of an earlier grid's declarations") ]
      newlyExcluded ir clash `shouldReturn` [(TableId 28, "C-GRID-NAME failed")]
    it "find a byteGrid name only byteFieldValue's declarations import (C-GRID-NAME at those tables), which byteGrid's check cannot see" $ do
      -- T212 alone bound to byteGrid, so no grid has a tail; its record named Tail, which the reserved runs' layouts import
      only212 <- annotated pristine (DT.replace " & rule byteGrid" "" hsa <> "core-2.01 | T212 | layout \"Tail\" byteGrid\n")
      [ renderFinding f | f <- checkIR Scott only212, Cm.code f == "C-GRID-NAME" ] `shouldBe`
        [ "C-GRID-NAME core-2.01.pdf#page=" <> pg <> "&T" <> n <> ": Tail, a name of byteGrid's GUDID Column Contents Description, is also a name these declarations import"
        | (pg, n) <- [("63", "31"), ("63", "32"), ("64", "33"), ("65", "35"), ("66", "36"), ("66", "37")] ]
    it "import the record class from the one runtime module Hsed.Spec.Gen names, as feature's records do" $ do
      let recordImports r = [ () | Emit d <- ruleCompile r ctx (boundTables (shapeOf r) ctx)
                                 , Gen.ImportSome m _ <- d.declImports, m == Gen.recordModule ]
          shapeOf r = lookup (ruleName r) [(RuleName "byteGrid", ByteGrid), (RuleName "byteFieldValue", ByteFieldValue), (RuleName "feature", Feature)]
          byName n = L.find ((== RuleName n) . ruleName) registry
      [ (n, not (null (recordImports r))) | n <- ["byteGrid", "byteFieldValue", "feature"], Just r <- [byName n] ]
        `shouldBe` [("byteGrid", True), ("byteFieldValue", True), ("feature", True)]
      [ l | l <- T.lines layouts, "import" `T.isPrefixOf` l, Gen.recordModule `T.isInfixOf` l ] `shouldSatisfy` ((== 1) . length)

  describe "the names a generated layouts or features module imports (declScope, for C-GRID-NAME)" $ do
    let prov = Gen.Provenance (SpecId "core-2.01") "T1" "Table 1" 1 1 1 "0/1" ""
        importing xs = (Gen.decl prov "x" []) { Gen.declImports = [Gen.ImportSome "M" xs] }
        features = fromMaybe "" (lookup "Hsed.Gen.Core.Features" (crModules cr))
        imported = L.nub [ T.takeWhileEnd C.isAlphaNum upTo | l <- T.lines (layouts <> "\n" <> features), "import" `T.isPrefixOf` l
                                                           , (upTo, _) <- DT.breakOnAll " (..)" l ]
    it "are each import's names: a class and the methods it lists, a type and its constructors and fields (the runtime's Data)" $
      declScope [importing ["TailKind (..)", "Tail (..)", "LayoutRecord (recordLayout, fromFields)", "(<*>)", "spanning"]] `shouldBe`
        ["TailKind", "Payload", "ReservedRun", "Tail", "tailName", "tailLength", "tailAlign", "tailKind", "LayoutRecord", "recordLayout", "fromFields", "<*>", "spanning"]
    it "know the members of every type the generated layouts and features import with (..)" $ do
      imported `shouldBe` ["FieldKind", "Layout", "Tail", "TailKind", "Maybe", "FeatureHeader"]
      [ t | t <- imported, length (declScope [importing [t <> " (..)"]]) < 2 ] `shouldBe` []

  describe "the one tiling policy of the three rules" $ do
    let tileSeverities ir' tid = [ Cm.severity f | f <- checkIR Scott ir', Cm.code f == "C-LAYOUT-TILE", Cm.anchor f == Just tid ]
    it "gives a gap a Warning, generating the layout without a record, in byteGrid's, byteFieldValue's and feature's tables alike" $ do
      -- Table 39's byte 15 relabelled 13 (bytes 14-15 in no row); Table 42's 5 - 15 relabelled 6 - 15 (byte 5 in no row);
      -- Table 236 as printed (byte 8 in no row)
      let gaps = setCell "T42" 5 0 "6 - 15" (setCell "T39" 10 0 "13" ir)
      [ (tid, tileSeverities gaps tid) | tid <- ["T39", "T42", "T236"] ]
        `shouldBe` [("T39", [Warning]), ("T42", [Warning]), ("T236", [Warning])]
      cr' <- either (throwString . show) pure (compileIR Scott gaps)
      let declared' m = concatMap declared (T.lines (fromMaybe "" (lookup m (crModules cr'))))
      [ n | n <- declared' "Hsed.Gen.Core.Layouts", n `elem` ["level0DiscoveryHeaderLayout", "Level0DiscoveryHeader"] ] `shouldBe` ["level0DiscoveryHeaderLayout"]
      [ n | n <- declared' "Hsed.Gen.Core.Features", n `elem` ["tPerFeatureLayout", "TPerFeature"] ] `shouldBe` ["tPerFeatureLayout"]
    it "gives an overlap an Error, leaving the table out, in each rule's tables alike" $ do
      let overlaps = setCell "T42" 5 0 "4 - 15" (setCell "T33" 1 2 "00 00 00 00 01" (setCell "T33" 1 0 "4 to 8" (setCell "T18" 13 0 "14" ir)))
      -- an overlap of each field of Table 42's byte 4 with the run 4 - 15
      [ (tid, L.nub (tileSeverities overlaps tid)) | tid <- ["T18", "T33", "T42"] ] `shouldBe` [("T18", [Error]), ("T33", [Error]), ("T42", [Error])]
      L.sort <$> newlyExcluded ir overlaps `shouldReturn`
        [(TableId 18, "C-LAYOUT-TILE failed"), (TableId 33, "C-LAYOUT-TILE failed"), (TableId 42, "C-LAYOUT-TILE failed")]
