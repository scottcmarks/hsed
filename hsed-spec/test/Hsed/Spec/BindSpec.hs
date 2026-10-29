{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.BindSpec
Description : Test specifications for Hsed.Spec.Bind
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsedc bind@ on the committed Core extraction: one entry per annotation, in
canonical order, a bare modifier chain printed on its rule default, a binding
shaped by its target printed at the target's shape, and the committed bindings
module exactly what the committed annotations give.
-}
module Hsed.Spec.BindSpec (spec) where

import qualified RIO.Text                 as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor         (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Bind
import           Hsed.Spec.Common         (isError, renderFinding)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.Grammar        (printSomeBinding)
import           Hsed.Spec.IR             (AnnotIR (..), SpecIR (..), StructPath (..))


spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)

  describe "bindEntries" $ do
    it "gives one entry per annotation, in canonical order, a bare modifier chain printed on its rule default" $ do
      let (fs, entries) = bindEntries ir
          docPayload = [ a.contents | a <- ir.annots, a.id == "doc" ]
      fs `shouldBe` []
      map (\(_, a, _, _, _) -> renderAnchorId a) entries `shouldBe` map (.id) ir.annots
      [ (a, page, sp, printSomeBinding sb) | (_, a, page, sp, sb) <- entries
                                            , a `elem` [DocId, SectionId "5.1.3", TableId 75, TableId 87, TableId 101, TableId 149] ] `shouldBe`
        [ (DocId, 1, StructPath "", T.concat docPayload)
        , (SectionId "5.1.3", 100, StructPath "0/1278", "rule typeRow & rule enumValues")
        , (TableId 75, 107, StructPath "0/1429", "erratum scott [\"Fractoin\" ~> \"Fraction\"] typeRow")
        , (TableId 87, 109, StructPath "0/1488", "rangeWords [\"to\"] (cols 0 1 enumValues)")
        , (TableId 101, 113, StructPath "0/1555", "erratum scott [\"Verify Eror\" ~> \"Verify Error\"] enumValues")
        , (TableId 149, 124, StructPath "0/1792", "waive \"C-UID-UNIQUE\" \"T94 00 00 00 05 00 00 02 15\" \"no erratum says which is wrong\" typeRow") ]
    it "prints nothing for a rule-bound table without a comment" $ do
      length (snd (bindEntries ir)) `shouldBe` length ir.annots
      [ a | (_, a@(TableId 46), _, _, _) <- snd (bindEntries ir) ] `shouldBe` []
    it "prints a binding shaped by its target at the target's shape" $ do
      ir' <- annotated pristine (hsa <> "core-2.01 | T47 | refines (core 46)\n")
      let (fs, result) = bindModule ir'
      fs `shouldBe` []
      fmap T.lines result `shouldSatisfy` either (const False)
        (elem "b_core_T47 = at (anchor @'TypeRow \"core-2.01\" \"T47\" \"0/1294\" 100) (refines (core 46))")
    it "reports an annotation it cannot resolve, citing the page, and leaves it out" $ do
      ir' <- annotated pristine (hsa <> "core-2.01 | T47 | refines (opal 3)\n")
      let (fs, entries) = bindEntries ir'
      map renderFinding fs `shouldSatisfy` any ("E-SHAPE core-2.01.pdf#page=100&T47: the shape comes from Opal 3" `T.isPrefixOf`)
      length entries `shouldBe` length (snd (bindEntries ir))
    it "refuses an erratum of the published set, as hsedc check does" $ do
      ir' <- annotated pristine (hsa <> "core-2.01 | T46 | erratum published [\"AC_element\" ~> \"x\"]\n")
      map renderFinding (fst (bindEntries ir'))
        `shouldBe` ["E-ERRATUM-PUBLISHED core-2.01.pdf#page=100&T46: 1:9: erratum published: the published set is the text as printed and takes no corrections; write erratum scott"]

  describe "bindModule" $
    it "prints the committed hsed-spec/bindings/Hsed/Bindings/Core.hs" $ do
      committed <- readFileUtf8 "bindings/Hsed/Bindings/Core.hs"
      let (fs, result) = bindModule ir
      filter isError fs `shouldBe` []
      result `shouldBe` Right committed
