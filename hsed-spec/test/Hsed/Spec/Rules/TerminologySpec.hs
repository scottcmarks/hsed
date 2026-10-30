{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Rules.TerminologySpec
Description : Test specifications for the terminology rule and the prose reader
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The terminology rule on the committed Core extraction with the committed
annotations (design section 3.4; M2b): from the doc anchor's @rule
terminology@ it binds, as @prose@, the six tables whose caption ends
@Terminology@ or @Example@; its 'RuleDef' also reads the six tables a @prose@
comment binds (Tables 2, 3, 24 and 185, and 230 and 231 until M6). All twelve
are bound, none has a finding, none is left out and nothing is generated.
-}
module Hsed.Spec.Rules.TerminologySpec (spec) where

import qualified RIO.Map                    as Map
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor           (AnchorId (..))
import           Hsed.Spec.Check            (checkIR, mkContext)
import           Hsed.Spec.Common           (ErrataSet (..), RuleName (..))
import qualified Hsed.Spec.Common           as Cm
import           Hsed.Spec.Compile          (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL              (SomeBinding (..), prose)
import           Hsed.Spec.RuleDef          (BoundTable (..), Classification (..))
import           Hsed.Spec.Rules            (classify, explain)
import           Hsed.Spec.Rules.Terminology


spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx    = mkContext Scott ir
      tables = [ (n, ptByRule pt) | pt <- proseTables ctx, TableId n <- [btAnchor (ptBound pt)] ]
      anchors = map (\(n, _) -> "T" <> tshow n) tables

  describe "the terminology rule" $ do
    it "binds as prose the six Terminology and Example tables, and no other, by the doc anchor's rule token" $ do
      [ (n, sb) | (TableId n, RuleBound (RuleName "terminology") _ sb) <- Map.toList (classify ir) ]
        `shouldBe` map (\n -> (n, SomeBinding prose)) [1, 210, 217, 219, 224, 232]
      explain ir (TableId 1) `shouldBe` "terminology (exemplar core T1), binding prose"
    it "leaves Table 3, the Foo Table Description, to its prose comment" $
      Map.lookup (TableId 3) (classify ir) `shouldBe` Just (CommentBound (SomeBinding prose))

  describe "the prose reader" $ do
    it "reads the six the rule binds and the six a prose comment binds, telling them apart" $
      tables `shouldBe`
        [ (1, True), (2, False), (3, False), (24, False), (185, False), (210, True), (217, True), (219, True)
        , (224, True), (230, False), (231, False), (232, True) ]
    it "finds nothing in them, leaves none out and generates nothing" $ do
      cr <- either (throwString . show) pure (compileIR Scott ir)
      [ f | f <- checkIR Scott ir, Cm.anchor f `elem` map Just anchors ] `shouldBe` []
      [ aid | (aid, _) <- crExcluded cr, TableId n <- [aid], n `elem` map fst tables ] `shouldBe` []
      lookup "Hsed.Gen.Core.Prose" (crModules cr) `shouldBe` Nothing
