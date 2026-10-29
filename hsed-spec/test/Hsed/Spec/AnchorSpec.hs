{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Spec.AnchorSpec
Description : Test specifications for Hsed.Spec.Anchor
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental
-}
module Hsed.Spec.AnchorSpec (spec) where

import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor
import           Hsed.Spec.Common (Severity (..), SpecId (..), finding, renderFinding)
import qualified Hsed.Spec.IR     as IR
import           Hsed.Spec.IR     (StructPath (..))
import           Hsed.Spec.Shape  (Shape (..))


ids :: [(Text, AnchorId)]
ids =
  [ ("T46", TableId 46)
  , ("5.1.3", SectionId "5.1.3")
  , ("6", SectionId "6")
  , ("doc", DocId)
  , ("3.3.10.2.1#3a", ItemId "3.3.10.2.1" "3a")
  , ("5.1.3#p2", ItemId "5.1.3" "p2")
  , ("5.3.3.6/sig", SigId "5.3.3.6")
  ]

spec :: Spec
spec = do
  describe "parseAnchorId" $ do
    it "reads every id form" $
      map (parseAnchorId . fst) ids `shouldBe` map (Just . snd) ids

    it "rejects what is not an id" $
      map parseAnchorId ["", "T", "Tx", "table46", "5.1.", ".5", "5.1.3#", "#3a", "/sig", "doc2", "T46/sig"]
        `shouldBe` replicate 11 Nothing

    it "round-trips through renderAnchorId" $
      map (renderAnchorId . snd) ids `shouldBe` map fst ids

  describe "anchorIdent" $
    it "makes a Haskell identifier fragment" $
      map (anchorIdent . snd) ids `shouldBe` ["T46", "5_1_3", "6", "doc", "3_3_10_2_1_3a", "5_1_3_p2", "5_3_3_6_sig"]

  describe "annotKind" $ do
    it "follows the id" $ do
      annotKind DocId "doc core201 & rule codes" `shouldBe` IR.Doc
      annotKind (SectionId "5.1.3") "rule typeRow & rule enumValues" `shouldBe` IR.Rule
      annotKind (TableId 75) "erratum scott [\"Fractoin\" ~> \"Fraction\"]" `shouldBe` IR.Bind
      annotKind (ItemId "3.3.10.2.1" "3a") "law \"Sync.ifSendWhileBusy\"" `shouldBe` IR.Law
      annotKind (SigId "5.3.3.6") "sig" `shouldBe` IR.Bind

    it "is Behaviour for a behaviour or fails payload on a section or an item" $ do
      annotKind (SectionId "5.3.3.6") "behaviour \"Base.get\"" `shouldBe` IR.Behaviour
      annotKind (ItemId "5.3.3.6" "p4") "  fails \"Base.getFails\"" `shouldBe` IR.Behaviour
      annotKind (TableId 46) "behaviour \"Base.get\"" `shouldBe` IR.Bind

  describe "Anchor" $ do
    let a = anchor @'TypeRow "core-2.01" "T46" "0/1288" 100
    it "cites the page like renderFinding" $ do
      anchorLocation a `shouldBe` "core-2.01.pdf#page=100&T46"
      locationText (SpecId "core-2.01") 107 (TableId 75) `shouldBe` "core-2.01.pdf#page=107&T75"

    it "carries id, spec, struct path and page" $ do
      aId a `shouldBe` TableId 46
      aSpec a `shouldBe` SpecId "core-2.01"
      aStruct a `shouldBe` StructPath "0/1288"
      aPage a `shouldBe` 100

    it "gives a finding its location" $
      renderFinding (withLocation a (finding Error "E-SHAPE" "Feature does not accept this table"))
        `shouldBe` "E-SHAPE core-2.01.pdf#page=100&T46: Feature does not accept this table"
