{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.IRSpec
Description : Test specifications for Hsed.Spec.IR
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The handwritten fixture @test/fixtures/mini-ir.json@ covers every record type
of IR v1; it must decode, and survive a trip through 'toJSON' and 'fromJSON'.
-}
module Hsed.Spec.IRSpec (spec) where

import           Data.Aeson   (Result (..), encode, fromJSON, toJSON)
import           RIO
import           Test.Hspec

import           Hsed.Spec.IR


fixture :: FilePath
fixture = "test/fixtures/mini-ir.json"

loadFixture :: IO SpecIR
loadFixture = readIR fixture >>= either throwString pure

-- | test suite
spec :: Spec
spec = do
  describe "readIR" $ do
    it "decodes the mini fixture" $ do
      ir <- loadFixture
      ir.irVersion `shouldBe` irVersion1
      ir.spec `shouldBe` "core-2.01"
      ir.extractor.script `shouldBe` "extract_tables.py"
      [length ir.tables, length ir.headings, length ir.paras, length ir.annots] `shouldBe` [2, 2, 2, 2]
      ir.sigs `shouldBe` []

    it "reads the table records" $ do
      ir <- loadFixture
      map (.id) ir.tables `shouldBe` [Just "T75", Nothing]
      map (.headerSource) ir.tables `shouldBe` [LeadingTH, NoHeader]
      map (.nestedIn) ir.tables `shouldBe` [Nothing, Just (StructPath "0/1340")]
      map (.rects) ir.tables `shouldBe`
        [ [PageRect 106 (Rect 90 400 522 460)]
        , [PageRect 106 (Rect 360 410 500 430), PageRect 107 (Rect 360 700 500 720)] ]

    it "reads the cells, defaulting the repair flags and fonts" $ do
      ir <- loadFixture
      let cells = concatMap (\t -> concat (t.headerRows <> t.repeatedHeaderRows <> t.rows)) ir.tables
      map (.kind) cells `shouldBe` [TH, TH, TH, TD, TD, TD, TH, TD]
      map (.splitFromMerged) cells `shouldBe` [False, False, False, False, True, False, False, False]
      map (.raggedJoin) cells `shouldBe` [False, False, False, False, False, True, False, False]
      map (.adoptedOrphan) cells `shouldBe` [False, False, False, False, False, False, False, True]
      map (.fonts) cells `shouldBe` [["Arial-BoldMT"], ["Arial-BoldMT"], ["Arial-BoldMT"], ["ArialMT"], ["ArialMT"], ["ArialMT", "ArialMT"], [], ["ArialMT"]]
      map (.fillRgb) (take 1 cells) `shouldBe` [Just (RGB 225 225 225)]
      map (isJust . (.adoptedFrom)) cells `shouldBe` [False, False, False, False, False, True, False, False]
      map (isJust . (.movedOut)) cells `shouldBe` [False, False, False, False, False, False, False, True]

    it "reads the headings, paras and annots" $ do
      ir <- loadFixture
      map (.section) ir.headings `shouldBe` [Just "5.1.3", Nothing]
      map (.kind) ir.paras `shouldBe` [LI, P]
      map (.item) ir.paras `shouldBe` [Just "3a", Nothing]
      map (fmap length . (.words)) ir.paras `shouldBe` [Nothing, Just 6]
      map (.kind) ir.annots `shouldBe` [Doc, Bind]
      map (.id) ir.annots `shouldBe` ["doc", "T75"]
      map (.contents) (drop 1 ir.annots) `shouldBe` ["erratum scott [\"Fractoin\" ~> \"Fraction\"]"]

    it "rejects another irVersion" $ do
      ir <- loadFixture
      result <- withSystemTempFile "ir.json" $ \path h -> do
        hClose h
        writeFileBinary path (toStrictBytes (encode ir { irVersion = 2 }))
        readIR path
      either (`shouldContain` "irVersion 2, expected 1") (const (expectationFailure "accepted irVersion 2")) result

  describe "JSON instances" $ do
    it "round-trip the fixture through toJSON and fromJSON" $ do
      ir <- loadFixture
      fromJSON (toJSON ir) `shouldBe` Success ir

    it "accept the pre-v1 array form of a structure path" $
      fromJSON (toJSON [0 :: Int, 1288]) `shouldBe` Success (StructPath "0/1288")

    it "render the enumerations as the schema spells them" $ do
      map toJSON [minBound .. maxBound :: HeaderSource]
        `shouldBe` map toJSON ["leading-th", "first-of-all-th", "repeat", "bold-first-row", "none" :: Text]
      map toJSON [minBound .. maxBound :: AnnotKind]
        `shouldBe` map toJSON ["Doc", "Rule", "Bind", "Law", "Behaviour" :: Text]
