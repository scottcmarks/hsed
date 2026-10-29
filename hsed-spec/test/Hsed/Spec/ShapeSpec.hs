{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.ShapeSpec
Description : Test specifications for Hsed.Spec.Shape
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Hand-built tables for the M1 cases of design section 3.3, then the committed
Core extraction (@spec/ir/core-2.01.json@, skipped when absent) for the
exemplars of section 3.4 and the rule counts.
-}
module Hsed.Spec.ShapeSpec (spec) where

import qualified RIO.Map          as Map
import           RIO
import           RIO.Directory    (doesFileExist)
import qualified RIO.Text         as T
import           Test.Hspec

import           Hsed.Spec.Common (Finding (severity), Severity (..), SpecId (..), renderFinding)
import           Hsed.Spec.IR     hiding (AnnotKind (..))
import           Hsed.Spec.Shape


core :: SpecId
core = SpecId "core-2.01"

-- | A table with a caption, a header row and data rows, on 0-based page 99.
mkTable :: Text -> Text -> [Text] -> [[Text]] -> TableIR
mkTable anchorId caption header rows = TableIR
  { id = Just anchorId, tableNumber = Nothing, caption = Just caption, captionPath = Nothing
  , structPath = StructPath "0/1288", nestedIn = Nothing, part = Nothing, parts = Nothing
  , pages = [99], printedPages = [83], nRows = length rows, nCols = width
  , ordinal = Just 48, rects = [], headerSource = if null header then NoHeader else LeadingTH
  , headerRows = [map (cell TH) header | not (null header)], repeatedHeaderRows = []
  , rows = map (map (cell TD)) rows, warnings = [] }
  where
    width = foldl' max 0 (length header : map length rows)
    cell k t = CellIR
      { kind = k, text = t, lines = [t], page = Just 99, bbox = Nothing, estBbox = Nothing
      , bold = k == TH, underline = False, fonts = [], fillRgb = Nothing, fillBbox = Nothing
      , nestedTables = [], fragments = [], splitFromMerged = False, recoveredByGeometry = False
      , raggedJoin = False, adoptedOrphan = Nothing, adoptedFrom = Nothing, movedOut = Nothing }

t46, t52, t42, t17, t87 :: TableIR
t46 = mkTable "T46" "Table 46 AC_element" ["UID", "Name", "Format"]
  [["00 00 00 05 00 00 08 01", "AC_element", "List_Type, *, ACE_expression"]]
t52 = mkTable "T52" "Table 52 adv_key_mode Enumeration Values" ["Enumeration Value", "Behavior"]
  [["0", "Wait for AdvKey_Req"], ["1", "Auto-advance keys"], ["2-7", "Reserved"]]
t42 = mkTable "T42" "Table 42 TPer Feature Descriptor" ["Bit Byte", "7", "6", "5", "4", "3", "2", "1", "0"]
  [["0", "(MSB)", "Feature Code", ""], ["1", "", "(LSB)"]]
t17 = mkTable "T17" "Table 17 ComPacket Format" []
  [["Bit", "7", "6", "5", "4", "3", "2", "1", "0"], ["Byte"], ["0", "(MSB)", "Reserved"]]
t87 = mkTable "T87" "Table 87 gen_status Enumeration Values" ["Column Value", "Associated Value", "Meaning"]
  [["0", "None", ""], ["7 to 31", "Reserved", ""]]

form46, form52, form42, form17, form87 :: Form
form46 = measure core t46
form52 = measure core t52
form42 = measure core t42
form17 = measure core t17
form87 = measure core t87

spec :: Spec
spec = do
  describe "normHeader" $ do
    it "applies the typo table, strips whitespace and lower-cases" $
      map normHeader ["Enumeration Value", "Colum Type", "M axSize", "MandatoryWrite  Granularity", " UID ", "Byte\\Bit"]
        `shouldBe` ["enumerationvalue", "columntype", "maxsize", "mandatorywritegranularity", "uid", "byte\\bit"]
    it "needs the typo table only for Colum Type: the design's other two typos are whitespace" $
      headerTypos `shouldBe` [("Colum Type", "Column Type")]

  describe "headerNormalisations" $
    it "lists each typo a table's header cells contain, as printed and as read" $ do
      headerNormalisations (mkTable "T26" "Table 26 X Columns" ["Column Number", "Column Name", "IsUnique", "Colum Type"] [])
        `shouldBe` [("Colum Type", "Column Type")]
      headerNormalisations (mkTable "T46" "Table 46 AC_element" ["UID", "Name", "Format"] []) `shouldBe` []

  describe "captionTitle and classifyCaption" $ do
    it "strip the table number in both spellings" $ do
      captionTitle "Table 46 AC_element" `shouldBe` Just "AC_element"
      captionTitle "Table 26 - Admin SP – TPerInfo Columns" `shouldBe` Just "Admin SP – TPerInfo Columns"
      captionTitle "Table 01 Global Terminology" `shouldBe` Just "Global Terminology"
      captionTitle "Figure 3 Something" `shouldBe` Nothing
      captionTitle "Table 71 ACE_object _ref" `shouldBe` Just "ACE_object_ref"
    it "normalise the _object _ref captions, which rawCaptionTitle leaves as printed" $ do
      rawCaptionTitle "Table 72 Certificates_object _ref" `shouldBe` Just "Certificates_object _ref"
      normaliseCaption "Table 72 Certificates_object _ref" `shouldBe` "Table 72 Certificates_object_ref"
      normaliseCaption "Table 46 AC_element" `shouldBe` "Table 46 AC_element"

    it "classify the design's caption kinds" $
      map classifyCaption
        [ "AC_element", "ACE_object_ref", "adv_key_mode Enumeration Values", "Table Table Description"
        , "Admin SP – TPerInfo Columns", "TPer Feature Descriptor", "Admin SP - Authority Table Preconfiguration"
        , "Default Base Template Authorities", "Global Terminology", "data_removal_mechanism Type Table Addition"
        , "Protocol IDs" ]
        `shouldBe`
        [ CaptionIdent "AC_element", CaptionIdent "ACE_object_ref", CaptionEnumValues "adv_key_mode"
        , CaptionTableDesc "Table", CaptionColumns "Admin SP – TPerInfo", CaptionFeature, CaptionPreconfig
        , CaptionPreconfig, CaptionTerminology, CaptionIdent "data_removal_mechanism", CaptionOther "Protocol IDs" ]

  describe "measure" $ do
    it "reads the type row T46" $ do
      fCols form46 `shouldBe` 3
      fHeaderNorm form46 `shouldBe` ["uid", "name", "format"]
      fCaptionKind form46 `shouldBe` CaptionIdent "AC_element"
      fPage form46 `shouldBe` Just 100
      fId form46 `shouldBe` Just "T46"
      fGrid8 form46 `shouldBe` False
      fRagged form46 `shouldBe` False

    it "sees the byte grids, with and without a header row" $ do
      fGrid8 form42 `shouldBe` True
      fGrid8 form17 `shouldBe` True
      fHeaderSource form17 `shouldBe` NoHeader
      fRagged form42 `shouldBe` True

  describe "accepts" $ do
    it "TypeRow accepts [UID, Name, Format] under a Table N <ident> caption" $
      accepts TypeRow form46 `shouldBe` Nothing

    it "EnumValues accepts a two-column enumeration table and not a wider one" $ do
      accepts EnumValues form52 `shouldBe` Nothing
      fmap severity (accepts EnumValues form87) `shouldBe` Just Error
      accepts EnumValues (either (const form87) id (projectColumns 0 1 form87)) `shouldBe` Nothing

    it "Feature rejects a three-column type table with E-SHAPE citing the page" $ do
      let f = accepts Feature form46
      fmap renderFinding f `shouldSatisfy` maybe False ("E-SHAPE core-2.01.pdf#page=100&T46: Feature does not accept" `T.isPrefixOf`)
      fmap severity f `shouldBe` Just Error
      accepts Feature form42 `shouldBe` Nothing
      accepts ByteGrid form17 `shouldBe` Nothing

    it "demotes to a Warning under as" $
      fmap severity (acceptsAs Feature form46) `shouldBe` Just Warning

    it "Doc, Heading, Prose and Sig accept anything" $
      [ accepts s f | s <- [Doc, Heading, Prose, Sig], f <- [form46, form52, form42, form17, form87] ]
        `shouldBe` replicate 20 Nothing

    it "projectColumns refuses a column the table lacks" $
      either (const True) (const False) (projectColumns 0 3 form87) `shouldBe` True

  describe "the committed Core extraction" $ do
    let path = "../spec/ir/core-2.01.json"
        withCore k = do
          present <- doesFileExist path
          if present then readIR path >>= either throwString k
                     else pendingWith (path <> " is not present")
        byId :: SpecIR -> Map.Map Text Form
        byId ir = Map.fromList [ (i, measure core t) | t <- ir.tables, Just i <- [t.id] ]
        exemplars =
          [ ("T46", TypeRow), ("T52", EnumValues), ("T171", TableDesc), ("T240", TableUIDs)
          , ("T242", UidTable), ("T166", StatusCodes), ("T25", Codes), ("T27", FieldValue)
          , ("T34", FieldType), ("T33", ByteFieldValue), ("T17", ByteGrid), ("T42", Feature)
          , ("T190", ConstValues), ("T238", Ranges), ("T209", Preconfig), ("T4", Tokens)
          , ("T45", StateTable), ("T167", Props), ("T168", Props), ("T230", Decision), ("T1", Prose) ]

    it "accepts every exemplar of design section 3.4 under its rule's shape" $ withCore $ \ir -> do
      let forms = byId ir
          verdicts = [ (i, maybe (Just "missing") (fmap renderFinding . accepts s) (Map.lookup i forms)) | (i, s) <- exemplars ]
      filter (isJust . snd) verdicts `shouldBe` []

    it "finds the design's 100 type tables and 19 enumeration tables" $ withCore $ \ir -> do
      let forms = Map.elems (byId ir)
          count s = length (filter (isNothing . accepts s) forms)
      (count TypeRow, count EnumValues) `shouldBe` (100, 19)

    it "cites the page of a real table" $ withCore $ \ir ->
      fmap renderFinding (Map.lookup "T46" (byId ir) >>= accepts Feature)
        `shouldSatisfy` maybe False ("E-SHAPE core-2.01.pdf#page=100&T46: " `T.isPrefixOf`)
