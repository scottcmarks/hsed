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
of IR v1 in the shapes the extractor writes; it must decode, and survive a trip
through 'toJSON' and 'fromJSON'. The committed @spec/ir/*.json@ must decode too,
with the counts the extractor reports.
-}
module Hsed.Spec.IRSpec (spec) where

import           Data.Aeson    (Result (..), encode, fromJSON, toJSON)
import           RIO
import           RIO.Directory (doesFileExist)
import           Test.Hspec

import           Hsed.Spec.IR


fixture :: FilePath
fixture = "test/fixtures/mini-ir.json"

loadFixture :: IO SpecIR
loadFixture = readIR fixture >>= either throwString pure

-- | The committed extractions (relative to the package directory, where
-- @stack test@ runs) and their table, heading, para and annot counts.
committed :: [(FilePath, (Int, Int, Int, Int))]
committed =
  [ ("../spec/ir/core-2.01.json", (263, 1346, 4112, 0))
  , ("../spec/ir/opal-2.30.json", (63, 178, 657, 0))
  ]

-- | Every cell of the header and data rows, tables in order.
bodyCells :: SpecIR -> [CellIR]
bodyCells ir = concatMap (\t -> concat (t.headerRows <> t.rows)) ir.tables

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

    it "reads the table records, structure paths in either spelling" $ do
      ir <- loadFixture
      map (.id) ir.tables `shouldBe` [Just "T75", Nothing]
      map (.ordinal) ir.tables `shouldBe` [Just 77, Nothing]
      map (.headerSource) ir.tables `shouldBe` [LeadingTH, NoHeader]
      map (.structPath) ir.tables `shouldBe` [StructPath "0/1340", StructPath "0/1340/1/1/0"]
      map (.captionPath) ir.tables `shouldBe` [Just (StructPath "0/1339"), Nothing]
      map (.nestedIn) ir.tables `shouldBe` [Nothing, Just (StructPath "0/1340")]
      map (.rects) ir.tables `shouldBe`
        [ [PageRect 106 (Rect 90 400 522 460), PageRect 107 (Rect 90 700 522 720)]
        , [PageRect 106 (Rect 360 410 500 430)] ]

    it "reads a repeated header row with its page and row index" $ do
      ir <- loadFixture
      let repeated = concatMap (.repeatedHeaderRows) ir.tables
      map (.page) repeated `shouldBe` [107]
      map (.rowIndex) repeated `shouldBe` [1]
      map (map (.text) . (.cells)) repeated `shouldBe` [["UID"]]

    it "reads the cells, defaulting what an empty cell leaves out" $ do
      ir <- loadFixture
      let cells = bodyCells ir
      map (.kind) cells `shouldBe` [TH, TH, TH, TD, TD, TD, TD, TD]
      map (.page) cells `shouldBe` [Just 106, Just 106, Just 106, Just 106, Just 106, Just 106, Nothing, Just 106]
      map (isJust . (.bbox)) cells `shouldBe` [True, True, True, True, True, True, False, True]
      map (isJust . (.estBbox)) cells `shouldBe` [False, False, False, False, True, False, True, False]
      map (.bold) cells `shouldBe` [True, True, True, False, False, False, False, False]
      map (.fonts) cells `shouldBe` [["Arial-BoldMT"], ["Arial-BoldMT"], ["Arial-BoldMT"], ["ArialMT"], ["ArialMT"], ["ArialMT", "ArialMT"], [], ["ArialMT"]]
      map (.fillRgb) (take 1 cells) `shouldBe` [Just (RGB 225 225 225)]

    it "reads the repair flags and their payloads" $ do
      ir <- loadFixture
      let cells = bodyCells ir
      map (.splitFromMerged) cells `shouldBe` [False, False, False, False, True, False, False, False]
      map (.recoveredByGeometry) cells `shouldBe` [False, False, False, False, True, False, False, False]
      map (.raggedJoin) cells `shouldBe` [False, False, False, False, False, True, False, False]
      map (length . (.fragments)) cells `shouldBe` [0, 0, 0, 0, 0, 2, 0, 0]
      map (.nestedTables) (drop 6 cells) `shouldBe` [[StructPath "0/1340/1/1/0"], []]
      map (.adoptedFrom) (drop 5 cells) `shouldBe` [Just (AdoptedFromIR (StructPath "0/1403") "uinteger_2"), Nothing, Nothing]
      map (.adoptedOrphan) (drop 6 cells) `shouldBe` [Nothing, Just "(MSB)"]
      map (.movedOut) (drop 6 cells) `shouldBe` [Nothing, Just (MovedOutIR (StructPath "0/1407") "denominator")]

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

  describe "the committed extractions" $
    forM_ committed $ \(path, counts) ->
      it ("decode " <> path <> " with the extractor's counts") $ do
        present <- doesFileExist path
        if present
          then do
            ir <- readIR path >>= either throwString pure
            (length ir.tables, length ir.headings, length ir.paras, length ir.annots) `shouldBe` counts
          else pendingWith (path <> " is not checked out")

  describe "JSON instances" $ do
    it "round-trip the fixture through toJSON and fromJSON" $ do
      ir <- loadFixture
      fromJSON (toJSON ir) `shouldBe` Success ir

    it "accept a structure path as an int array or a string" $ do
      fromJSON (toJSON [0 :: Int, 1288]) `shouldBe` Success (StructPath "0/1288")
      fromJSON (toJSON ("0/1288" :: Text)) `shouldBe` Success (StructPath "0/1288")

    it "render the enumerations as the schema spells them" $ do
      map toJSON [minBound .. maxBound :: HeaderSource]
        `shouldBe` map toJSON ["leading-th", "first-of-all-th", "repeat", "bold-first-row", "none" :: Text]
      map toJSON [minBound .. maxBound :: AnnotKind]
        `shouldBe` map toJSON ["Doc", "Rule", "Bind", "Law", "Behaviour" :: Text]
