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
  describe "parseSidecar" $
    it "skips comments and blank lines and names a malformed line" $ do
      let (fs, ls) = parseSidecar "# a comment\n\ncore-2.01 | T75 | erratum scott [\"a\" ~> \"b\"]\nno bars here\n"
      ls `shouldBe` [SidecarLine 3 "core-2.01" "T75" "erratum scott [\"a\" ~> \"b\"]"]
      map renderFinding fs `shouldBe` ["E-SIDECAR -: line 4: expected spec | id | payload, got \"no bars here\""]

  withCore $ \ir -> do
    describe "resolveSidecar" $ do
      it "places the committed annotations where the extractor reads them back" $ do
        hsa <- committedHsa
        resolveSidecar ir hsa `shouldBe`
          ( []
          , [ AnnotIR Doc "core-2.01" "doc" (StructPath "") 1 0 (Rect 36 720 108 756) 6 "doc core201"
            , AnnotIR Rule "core-2.01" "5.1.3" (StructPath "0/1278") 1 99 (Rect 90.0 595.61 215.4 613.67) 6 "rule typeRow & rule enumValues"
            , AnnotIR Bind "core-2.01" "T75" (StructPath "0/1429") 1 106 (Rect 170.88 559.18 468.12 671.1) 6 "erratum scott [\"Fractoin\" ~> \"Fraction\"]" ] )
      it "refuses an anchor the IR does not have, and another spec's line" $ do
        let (fs, as) = resolveSidecar ir "core-2.01 | T999 | typeRow\nopal-2.30 | T46 | typeRow\ncore-2.01 | 9.9.9 | rule typeRow\n"
        as `shouldBe` []
        map renderFinding fs `shouldBe`
          [ "E-SIDECAR core-2.01: line 1: no table T999 in the IR"
          , "E-SIDECAR core-2.01: line 2: spec is opal-2.30, the IR is core-2.01"
          , "E-SIDECAR core-2.01: line 3: no heading numbered 9.9.9 in the IR" ]
    describe "withSidecar" $
      it "orders the annotations canonically" $ do
        let (_, ir') = withSidecar ir "core-2.01 | T75 | erratum scott [\"a\" ~> \"b\"]\ncore-2.01 | T46 | typeRow\ncore-2.01 | doc | doc core201\n"
        map (.id) ir'.annots `shouldBe` ["doc", "T46", "T75"]
