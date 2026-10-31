{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.BoundUnitSpec
Description : The bound-unit abstraction: the four kinds of unit as one class
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The units a binding holds, numbered tables, signature blocks, paragraphs and
headings, are each a record of its own and an instance of
"Hsed.Spec.RuleDef".'BoundUnit' (M4b's scaffold, the M4a plan's answer 6):
on the committed Core run 'allUnits' holds the 248 numbered tables, the 53
signature blocks, the 3 sentences the laws bind and the 5 headings the
behaviours and fails bind, kind after kind; on Opal read over Core, the 27
tables M4b binds and nothing else. The per-kind names are the class's at their
types: 'boundUnits' reads what 'boundTables' does, a table its comment binds
to a shape that refuses it held ('unitsAll') but not read ('unitReadable');
'atUnit' locates as 'atTable', 'atSig', 'atPara' and 'atHeading' do;
'unitNamed' finds every unit by its anchor id, every unit anchor of both
committed sidecars among them, and no unit at a rule anchor.
-}
module Hsed.Spec.BoundUnitSpec (spec) where

import qualified Data.Text         as DT
import qualified RIO.List          as L
import qualified RIO.Text          as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor  (AnchorId (..), parseAnchorId, renderAnchorId)
import           Hsed.Spec.Check   (mkContext, mkContextWith)
import           Hsed.Spec.Common  (ErrataSet (..), Severity (..), finding, renderFinding)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL     (someBindingShape)
import qualified Hsed.Spec.Gen     as Gen
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Shape   (Shape (..), UnitKind (..))


-- | The kind of a unit.
kindOf :: SomeUnit -> UnitKind
kindOf (SomeUnit u) = unitKind (proxyOf u)
  where
    proxyOf :: u -> Proxy u
    proxyOf _ = Proxy

-- | The anchor id of a unit.
anchorOf :: SomeUnit -> AnchorId
anchorOf (SomeUnit u) = unitAnchor u

-- | How many units of each kind a Context holds, kind after kind in 'allUnits' order.
kindCounts :: Context -> [(UnitKind, Int)]
kindCounts ctx = [ (k, length g) | g@(k : _) <- L.group (map kindOf (allUnits ctx)) ]

-- | The anchors of a sidecar's lines, with each line's payload.
sidecarAnchors :: Text -> [(AnchorId, Text)]
sidecarAnchors hsa =
  [ (aid, T.strip payload)
  | l <- T.lines hsa, _ : a : rest@(_ : _) <- [DT.splitOn "|" l], Just aid <- [parseAnchorId (T.strip a)]
  , let payload = T.intercalate "|" rest ]

-- | Whether a sidecar line's anchor is a unit: a table, an item, or a heading a
--   behaviour or a fails binds (a heading carrying rule tokens is a rule anchor,
--   the doc line the document's).
unitAnchorLine :: (AnchorId, Text) -> Bool
unitAnchorLine (aid, payload) = case aid of
  TableId _   -> True
  ItemId _ _  -> True
  SectionId _ -> any (`T.isPrefixOf` payload) ["behaviour ", "fails "]
  _           -> False

spec :: Spec
spec = withCore $ \pristine -> do
  hsa  <- runIO committedHsa
  ir   <- runIO (annotated pristine hsa)
  let ctx = mkContext Scott ir

  describe "allUnits" $ do
    it "holds the committed Core run's 248 tables, 53 signature blocks, 3 paragraphs and 5 headings, kind after kind" $
      kindCounts ctx `shouldBe` [(TableUnit, 248), (SignatureUnit, 53), (ParagraphUnit, 3), (HeadingUnit, 5)]
    it "holds each kind in the order its per-kind names read it" $ do
      [ anchorOf u | u <- allUnits ctx, kindOf u == TableUnit ] `shouldBe` map btAnchor (unitsAll ctx)
      [ anchorOf u | u <- allUnits ctx, kindOf u == SignatureUnit ] `shouldBe` map bsAnchor (unitsAll ctx)
      [ anchorOf u | u <- allUnits ctx, kindOf u == ParagraphUnit ] `shouldBe` map bpAnchor (paragraphUnits ctx)
      [ anchorOf u | u <- allUnits ctx, kindOf u == HeadingUnit ] `shouldBe` map bhAnchor (headingUnits ctx)
      map bpAnchor (paragraphUnits ctx) `shouldBe` [ItemId "3.3.10.2.1" "3a", ItemId "3.3.10.6" "dc", ItemId "3.3.10.7" "a"]
    withOpal $ \opalPristine ->
      it "holds Opal's 27 bound tables, read over Core, and nothing else (M4b's sidecar: 25 by rule tokens, T17 and T49 by comments)" $ do
        opal <- committedOpalHsa >>= annotated opalPristine
        let octx = mkContextWith registry (Just ctx) Scott opal
        kindCounts octx `shouldBe` [(TableUnit, 27)]
        map anchorOf (allUnits octx) `shouldBe` map TableId ([1, 3, 4, 5, 6, 8, 13, 17] <> [18 .. 33] <> [43, 49, 50])

  describe "boundUnits" $ do
    it "reads, shape by shape, the tables boundTables does: every bound table, once" $ do
      let shapes = L.nub (mapMaybe (someBindingShape . btBinding) (unitsAll ctx))
      length shapes `shouldSatisfy` (> 10)
      forM_ shapes $ \s ->
        (s, map btAnchor (boundUnits (Just s) ctx)) `shouldBe` (s, map btAnchor (boundTables (Just s) ctx))
      sum [ length (boundTables (Just s) ctx) | s <- shapes ] `shouldBe` 248
      map bsAnchor (boundUnits (Just Sig) ctx) `shouldBe` map bsAnchor (boundSigs (Just Sig) ctx)
      length (boundSigs (Just Sig) ctx) `shouldBe` 53
      length (boundParas (Just Sentence) ctx) `shouldBe` 3
      length (boundHeadings (Just Method) ctx) `shouldBe` 5
      length (boundTables Nothing ctx) `shouldBe` 0
    it "holds a table its comment binds to a shape that refuses it (E-SHAPE), and does not read it" $ do
      -- T46 AC_element, a type row, bound by a comment to Codes, whose shape wants a code-like first header cell
      refusing <- annotated pristine (withoutLine "T46" hsa <> "core-2.01 | T46 | codes\n")
      let rctx = mkContext Scott refusing
          t46 = [ bt | bt <- unitsAll rctx, btAnchor bt == TableId 46 ]
      map (someBindingShape . btBinding) t46 `shouldBe` [Just Codes]
      map (unitReadable rctx) t46 `shouldBe` [False]
      map btAnchor (boundTables (Just Codes) rctx) `shouldNotContain` [TableId 46]
      map btAnchor (boundTables (Just TypeRow) rctx) `shouldNotContain` [TableId 46]
      fmap anchorOf (unitNamed rctx (TableId 46)) `shouldBe` Just (TableId 46)
      map (unitReadable ctx) [ bt | bt <- unitsAll ctx, btAnchor bt == TableId 46 ] `shouldBe` [True]

  describe "atUnit" $
    it "locates a finding at a table, a signature block, a paragraph and a heading, as the per-kind names do" $ do
      let note = finding Note "N-TEST" "here"
          at' aid = maybe "no unit" (\(SomeUnit u) -> renderFinding (atUnit ctx u note)) (unitNamed ctx aid)
      map at' [TableId 46, SigId "5.3.3.6", ItemId "3.3.10.2.1" "3a", SectionId "5.3.3.6"]
        `shouldBe` [ "N-TEST core-2.01.pdf#page=100&T46: here", "N-TEST core-2.01.pdf#page=201&5.3.3.6/sig: here"
                   , "N-TEST core-2.01.pdf#page=82&3.3.10.2.1#3a: here", "N-TEST core-2.01.pdf#page=201&5.3.3.6: here" ]
      [ renderFinding (atTable ctx bt note) | bt <- unitsAll ctx, btAnchor bt == TableId 46 ]
        `shouldBe` ["N-TEST core-2.01.pdf#page=100&T46: here"]
      [ renderFinding (atSig ctx bs note) | bs <- unitsAll ctx, bsAnchor bs == SigId "5.3.3.6" ]
        `shouldBe` ["N-TEST core-2.01.pdf#page=201&5.3.3.6/sig: here"]

  describe "unitProvenance" $ do
    it "gives each kind's declarations its provenance, counting its ordinal among its own kind" $
      [ (kindOf u, (unitProvenance ctx su).ordinalOf) | u@(SomeUnit su) <- allUnits ctx, anchorOf u `elem` [TableId 46, SigId "5.3.3.6", ItemId "3.3.10.2.1" "3a", SectionId "5.3.3.6"] ]
        `shouldBe` [(TableUnit, "ord"), (SignatureUnit, "block"), (ParagraphUnit, "paragraph"), (HeadingUnit, "heading")]
    it "is what the per-kind names give" $ do
      [ Gen.provenanceComment (unitProvenance ctx bt) | bt <- unitsAll ctx, btAnchor bt == TableId 46 ]
        `shouldBe` [ Gen.provenanceComment (boundProvenance ctx bt) | bt <- boundTables (Just TypeRow) ctx, btAnchor bt == TableId 46 ]
      [ Gen.provenanceComment (unitProvenance ctx bh) | bh <- headingUnits ctx ]
        `shouldBe` [ Gen.provenanceComment (headingProvenanceOf ctx bh) | bh <- boundHeadings (Just Method) ctx ]

  describe "unitNoun" $
    it "names each kind as a message does" $
      map unitNoun [TableUnit, SignatureUnit, ParagraphUnit, HeadingUnit] `shouldBe` ["table", "signature block", "paragraph", "heading"]

  describe "unitNamed" $ do
    it "finds every unit by its anchor id" $
      [ anchorOf u | u <- allUnits ctx, fmap anchorOf (unitNamed ctx (anchorOf u)) /= Just (anchorOf u) ] `shouldBe` []
    it "finds every unit anchor of the committed Core sidecar, and no unit at the doc anchor or a rule anchor" $ do
      let lines' = sidecarAnchors hsa
      length lines' `shouldBe` length (T.lines hsa)
      [ aid | (aid, payload) <- lines', unitAnchorLine (aid, payload), isNothing (unitNamed ctx aid) ] `shouldBe` []
      [ aid | (aid, payload) <- lines', not (unitAnchorLine (aid, payload)), isJust (unitNamed ctx aid) ] `shouldBe` []
      length (filter unitAnchorLine lines') `shouldBe` 21
    withOpal $ \opalPristine ->
      it "finds every unit anchor of the committed Opal sidecar, read over Core, and no unit at its doc anchor or its rule anchor (4.2)" $ do
        ohsa <- committedOpalHsa
        opal <- annotated opalPristine ohsa
        let octx = mkContextWith registry (Just ctx) Scott opal
            lines' = sidecarAnchors ohsa
        [ renderAnchorId aid | (aid, payload) <- lines', unitAnchorLine (aid, payload) ] `shouldBe` ["T17", "T49"]
        [ aid | (aid, payload) <- lines', unitAnchorLine (aid, payload), isNothing (unitNamed octx aid) ] `shouldBe` []
        [ aid | (aid, payload) <- lines', not (unitAnchorLine (aid, payload)), isJust (unitNamed octx aid) ] `shouldBe` []
        fmap anchorOf (unitNamed octx DocId) `shouldBe` Nothing
        fmap anchorOf (unitNamed octx (TableId 2)) `shouldBe` Nothing
