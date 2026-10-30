{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.SidecarSpec
Description : Test specifications for Hsed.Spec.Sidecar
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The sidecar read and resolved against the committed Core extraction: the
annotations it gives are the ones the extractor reads back from the PDF that
@tools/extract/annotate.py@ writes from the same sidecar (their values are those
of @build/core-2.01.hsed.json@ after @make ir-annotated@).
-}
module Hsed.Spec.SidecarSpec (spec) where

import           RIO
import           Test.Hspec

import           Hsed.Spec.Common  (renderFinding)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR
import           Hsed.Spec.Sidecar


spec :: Spec
spec = do
  describe "parseSidecar" $ do
    it "skips comments and blank lines and names a malformed line" $ do
      let (fs, ls) = parseSidecar "# a comment\n\ncore-2.01 | T75 | erratum scott [\"a\" ~> \"b\"]\nno bars here\n"
      ls `shouldBe` [SidecarLine 3 "core-2.01" "T75" "erratum scott [\"a\" ~> \"b\"]"]
      map renderFinding fs `shouldBe` ["E-SIDECAR -: line 4: expected spec | id | payload, got \"no bars here\""]
    it "refuses an anchor listed twice, keeping its first line (as hsed_annot.py's parse_hsa refuses it)" $ do
      let (fs, ls) = parseSidecar "core-2.01 | 5.1.3 | rule typeRow\ncore-2.01 | doc | doc core201\ncore-2.01 | 5.1.3 | rule enumValues\n"
      map slLine ls `shouldBe` [1, 2]
      map renderFinding fs `shouldBe` ["E-SIDECAR -: line 3: anchor core-2.01 | 5.1.3 is listed more than once (first at line 1)"]

  withCore $ \ir -> do
    describe "resolveSidecar" $ do
      it "places the committed annotations where the extractor reads them back" $ do
        hsa <- committedHsa
        let (fs, as) = resolveSidecar ir hsa
            ls = snd (parseSidecar hsa)
        fs `shouldBe` []
        map (.id) as `shouldBe` map slId ls
        map (.contents) as `shouldBe` map slPayload ls
        all (\a -> a.v == 1 && a.flags == 6 && a.spec == "core-2.01") as `shouldBe` True
        [ (a.kind, a.id, a.struct, a.page, a.rect) | a <- as, a.id `elem` ["doc", "5.1.3", "T75", "T87", "T101", "T149"] ] `shouldBe`
          [ (Doc, "doc", StructPath "", 0, Rect 36 720 108 756)
          , (Rule, "5.1.3", StructPath "0/1278", 99, Rect 90.0 595.61 215.4 613.67)
          , (Bind, "T75", StructPath "0/1429", 106, Rect 170.88 559.18 468.12 671.1)
          , (Bind, "T87", StructPath "0/1488", 108, Rect 90.0 79.32 549.0 213.78)
          , (Bind, "T101", StructPath "0/1555", 112, Rect 227.4 373.92 411.49 477.42)
          , (Bind, "T149", StructPath "0/1792", 123, Rect 169.44 356.64 469.56 411.18) ]
      it "refuses an anchor the IR does not have, and another spec's line" $ do
        let (fs, as) = resolveSidecar ir "core-2.01 | T999 | typeRow\nopal-2.30 | T46 | typeRow\ncore-2.01 | 9.9.9 | rule typeRow\n"
        as `shouldBe` []
        map renderFinding fs `shouldBe`
          [ "E-SIDECAR core-2.01: line 1: no table T999 in the IR"
          , "E-SIDECAR core-2.01: line 2: spec is opal-2.30, the IR is core-2.01"
          , "E-SIDECAR core-2.01: line 3: no heading numbered 9.9.9 in the IR" ]
      it "refuses a paragraph the IR does not have, a signature block, and one the IR does not have" $ do
        let (fs, _) = resolveSidecar ir "core-2.01 | 5.1.3#p99 | law \"A.b\"\ncore-2.01 | 5.3.3.6/sig | sig\ncore-2.01 | 5.3.3.99/sig | prose\n"
        map renderFinding fs `shouldBe`
          [ "E-SIDECAR core-2.01: line 1: no paragraph 5.1.3#p99 in the IR"
          , "E-SIDECAR core-2.01: line 2: signature blocks are rule-bound and carry no annotation: 5.3.3.6/sig"
          , "E-SIDECAR core-2.01: line 3: no signature block 5.3.3.99/sig in the IR" ]
    describe "withSidecar" $
      it "orders the annotations canonically: kind, then the id's natural order" $ do
        let (_, ir') = withSidecar ir "core-2.01 | T75 | erratum scott [\"a\" ~> \"b\"]\ncore-2.01 | T101 | enumValues\ncore-2.01 | T46 | typeRow\ncore-2.01 | doc | doc core201\n"
        map (.id) ir'.annots `shouldBe` ["doc", "T46", "T75", "T101"]
