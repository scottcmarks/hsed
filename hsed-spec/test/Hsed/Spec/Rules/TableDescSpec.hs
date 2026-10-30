{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.Rules.TableDescSpec
Description : Test specifications for the tableDesc rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The tableDesc rule (design example E5) on the committed Core extraction with
the committed annotations: the 45 descriptions of sections 5.3-5.8 it binds
from the doc anchor's rule token, Table 3 left to its @prose@ comment; each
names a table of Table 240 (all but MBR, a byte table); T233 has 14 columns
(M2b acceptance); the four column types no Type row defines are the five
C-FMT-RESOLVE Warnings; C-COL-CONTIG, C-COL-CELL and C-DESC-TABLE made to fail
by one edited cell or caption; what a column holds, read from its Type
object; the generated descriptions.
-}
module Hsed.Spec.Rules.TableDescSpec (spec) where

import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Types           (TableRef (..))
import           Hsed.Spec.Anchor          (AnchorId (..))
import           Hsed.Spec.Check           (checkIR, mkContext)
import           Hsed.Spec.Common          (ErrataSet (..), Finding, RuleName (..), Severity (..), isError,
                                            renderFinding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.Compile         (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL             (SomeBinding (..), prose)
import           Hsed.Spec.IR              (SpecIR (..), TableIR (..))
import           Hsed.Spec.RuleDef         (BoundTable (..), Classification (..), cellsHash, numberedTables)
import           Hsed.Spec.Rules           (classify, explain)
import           Hsed.Spec.Rules.TableDesc
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..), tableUIDRows)


errorsOf :: SpecIR -> [Text]
errorsOf = map renderFinding . filter isError . checkIR Nothing Scott

coded :: Text -> [Finding] -> [Text]
coded c = map renderFinding . filter ((== c) . Cm.code)

-- | The IR with a table's caption replaced.
withCaption :: Text -> Text -> SpecIR -> SpecIR
withCaption tid caption ir = ir { tables = map (\t -> if t.id == Just tid then t { caption = Just caption } else t) ir.tables }

spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx   = mkContext Scott ir
      ds    = descReads ctx
      named n = L.find ((== Just n) . drTable) ds
      descIds = [ TableId n | n <- [169 .. 178] <> [180 .. 184] <> [186, 187] <> [189, 191 .. 203] <> [205 .. 208]
                                   <> [211, 213, 214, 215, 218] <> [220 .. 223] <> [225 .. 229] <> [233, 234] ]

  describe "the tableDesc rule" $ do
    it "binds the 45 table descriptions of sections 5.3-5.8 by the doc anchor's rule token" $ do
      [ aid | (aid, RuleBound (RuleName "tableDesc") _ _) <- Map.toList (classify ir) ] `shouldBe` descIds
      length descIds `shouldBe` 45
      explain ir (TableId 171) `shouldBe` "tableDesc (exemplar core T171)"
    it "leaves Table 3, the Foo Table Description example, to its prose comment" $
      Map.lookup (TableId 3) (classify ir) `shouldBe` Just (CommentBound (SomeBinding prose))
    it "reads every description whole: rows of four cells, each naming a table of Table 240" $ do
      map (btAnchor . drBound) ds `shouldBe` descIds
      concatMap drShort ds `shouldSatisfy` null
      [ n | d <- ds, Just n <- [drTable d] ] `shouldSatisfy` ((== 45) . length . L.nub)
      [ tuName r | Right r <- tableUIDRows ctx, tuName r `notElem` mapMaybe drTable ds ] `shouldBe` ["MBR"]
    it "gives Table 233, the Log table, 14 columns (M2b acceptance)" $
      fmap (map crName . drColumns) (named "Log") `shouldBe` Just
        [ "UID", "Prev", "Next", "Session", "SigningAuthority", "SigningAuthName", "ExchangeAuthority"
        , "ExchangeAuthName", "MonotonicTime", "ExactTime", "TimeKind", "LogKind", "Name", "Data" ]
    it "finds no Error on the committed annotations" $
      errorsOf ir `shouldBe` []

  describe "C-FMT-RESOLVE" $ do
    it "warns of the four column types no Type row defines, at the page of each row" $
      coded "C-FMT-RESOLVE" (checkIR Nothing Scott ir) `shouldBe`
        [ "C-FMT-RESOLVE core-2.01.pdf#page=158&T172: column 0x06 Transactional: the column type boolean_def_true names no Type object (no type row binds it); nearest: boolean_ACE, T60; generated with its name and no UID"
        , "C-FMT-RESOLVE core-2.01.pdf#page=172&T182: column 0x0D ChainLimit: the column type int_1_def_0 names no Type object (no type row binds it); nearest: integer_1, T93; generated with its name and no UID"
        , "C-FMT-RESOLVE core-2.01.pdf#page=173&T183: column 0x0D ChainLimit: the column type int_1_def_0 names no Type object (no type row binds it); nearest: integer_1, T93; generated with its name and no UID"
        , "C-FMT-RESOLVE core-2.01.pdf#page=233&T215: column 0x07 Frozen: the column type boolean_def_false names no Type object (no type row binds it); nearest: boolean_ACE, T60; generated with its name and no UID"
        , "C-FMT-RESOLVE core-2.01.pdf#page=291&T233: column 0x0B LogKind: the column type log_kind names no Type object (no type row binds it); nearest: clock_kind, T73; generated with its name and no UID" ]
    it "makes them Warnings: the description is still generated" $
      all ((== Warning) . Cm.severity) (filter ((== "C-FMT-RESOLVE") . Cm.code) (checkIR Nothing Scott ir)) `shouldBe` True
    it "resolves every other column type to a type row" $
      length [ () | d <- ds, cr <- drColumns d, isJust (crTypeRow cr) ] `shouldBe` sum (map (length . drColumns) ds) - 5

  describe "C-COL-CONTIG" $ do
    it "fails on a column number out of order, and leaves the description out" $ do
      let broken = setCell "T171" 4 0 "0x05" ir
      errorsOf broken `shouldBe`
        [ "C-COL-CONTIG core-2.01.pdf#page=157&T171: column 0x05 Kind: the column numbers run 0x00 to 0x0C in row order, and row 5 is 0x05 where 0x04 is expected" ]
      newlyExcluded ir broken `shouldReturn` [(TableId 171, "C-COL-CONTIG, C-COL-CELL or C-DESC-TABLE failed")]
    it "fails on a column number that is not hex" $
      errorsOf (setCell "T233" 13 0 "0xOD" ir) `shouldBe`
        [ "C-COL-CONTIG core-2.01.pdf#page=292&T233: the column number \"0xOD\" is not 0x and hex digits" ]

  describe "C-COL-CELL" $
    it "fails on an IsUnique that is not Yes or blank, a name that is no identifier, and a name given twice" $ do
      errorsOf (setCell "T171" 1 2 "No" ir) `shouldBe`
        [ "C-COL-CELL core-2.01.pdf#page=156&T171: column 0x01 Name: IsUnique is \"No\", not Yes or blank" ]
      errorsOf (setCell "T171" 2 1 "Common Name" ir) `shouldBe`
        [ "C-COL-CELL core-2.01.pdf#page=156&T171: column 0x02 Common Name: the column name is not an identifier (a letter, then letters, digits or underscores)" ]
      errorsOf (setCell "T171" 2 1 "Name" ir) `shouldBe`
        [ "C-COL-CELL core-2.01.pdf#page=156&T171: column 0x02 Name: the column name is also that of column 0x01" ]

  describe "C-DESC-TABLE" $ do
    it "fails on a caption naming no table of Table 240" $
      errorsOf (withCaption "T233" "Table 233 Logs Table Description" ir) `shouldBe`
        [ "C-DESC-TABLE core-2.01.pdf#page=291&T233: the caption names Logs, which no row of the table of table UIDs names" ]
    it "fails on a second description of one table" $
      errorsOf (withCaption "T234" "Table 234 Log Table Description" ir) `shouldBe`
        [ "C-DESC-TABLE core-2.01.pdf#page=293&T234: the caption names Log, which T233 describes too" ]

  describe "colKind and readCell" $ do
    let kind = colKind ctx
        noRefs _ _ = Left "no references here"
    it "reads a column's kind from its Type object's Format" $ do
      kind "uid" `shouldBe` Right (KBytes 8)
      kind "name" `shouldBe` Right (KText 32)
      kind "uinteger_20" `shouldBe` Right (KUInt 20)
      kind "integer_1" `shouldBe` Right (KInt 1)
      kind "Authority_object_ref" `shouldBe` Right (KRef [Table "Authority"])
      fmap (\k -> case k of KEnum n rs ls -> Just (n, rs, ls); _ -> Nothing) (kind "boolean")
        `shouldBe` Right (Just ("boolean", [(0, 1)], [("False", 0), ("True", 1)]))
    it "refuses a type it does not read from a cell" $ do
      kind "date" `shouldBe` Left "a column of type date (Struct_Type) is not read from a table cell"
      kind "log_kind" `shouldBe` Left "log_kind names no Type object"
    it "reads cells as their kind holds them" $ do
      (kind "auth_method" >>= \k -> readCell noRefs k "Sign") `shouldBe` Right (CvEnum "Sign" 3)
      (kind "auth_method" >>= \k -> readCell noRefs k "Sing") `shouldSatisfy` isLeft
      (kind "uid" >>= \k -> readCell noRefs k "00 00 00 09 00 00 00 01") `shouldBe` Right (CvBytes [0, 0, 0, 9, 0, 0, 0, 1])
      (kind "uinteger_1" >>= \k -> readCell noRefs k "256") `shouldBe` Left "\"256\" does not fit in 1 bytes"
      (kind "integer_1" >>= \k -> readCell noRefs k "-1") `shouldBe` Right (CvInt (-1))
      renderCell (CvRef "Makers" [0, 0, 0, 9, 0, 0, 0, 3]) `shouldBe` Right "RefCell \"Makers\" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x03)"
      renderCell (CvUInt 255 (Just 4)) `shouldBe` Right "UIntCell 0x00FF"
      renderCell (CvUInt 255 Nothing) `shouldBe` Right "UIntCell 255"

  describe "the generated module" $ do
    cr <- runIO (either (throwString . show) pure (compileIR Nothing Scott ir))
    let text = fromMaybe "" (lookup "Hsed.Gen.Core.TableDescs" (crModules cr))
        lns  = T.lines text
        block n = takeWhile (/= "") (dropWhile (/= (n <> " :: TableDesc")) lns)
    it "declares one TableDesc per description" $
      length [ l | l <- lns, " :: TableDesc" `T.isSuffixOf` l ] `shouldBe` 45
    it "joins each to its UIDs in Table 240 and its column types to their Type objects, by name" $
      block "tableTableDesc" `shouldBe`
        [ "tableTableDesc :: TableDesc"
        , "tableTableDesc = TableDesc"
        , "  { tdName      = \"Table\""
        , "  , tdTableUID  = uTableTable"
        , "  , tdObjectUID = uTableTableObject"
        , "  , tdColumns   ="
        , "      [ Col 0x00 \"UID\" False \"uid\" (Just uuidType)"
        , "      , Col 0x01 \"Name\" True \"name\" (Just unameType)"
        , "      , Col 0x02 \"CommonName\" True \"name\" (Just unameType)"
        , "      , Col 0x03 \"TemplateID\" True \"Template_object_ref\" (Just uTemplate_object_refType)"
        , "      , Col 0x04 \"Kind\" False \"table_kind\" (Just utable_kindType)"
        , "      , Col 0x05 \"Column\" False \"Column_object_ref\" (Just uColumn_object_refType)"
        , "      , Col 0x06 \"NumColumns\" False \"uinteger_4\" (Just uuinteger_4Type)"
        , "      , Col 0x07 \"Rows\" False \"uinteger_4\" (Just uuinteger_4Type)"
        , "      , Col 0x08 \"RowsFree\" False \"uinteger_4\" (Just uuinteger_4Type)"
        , "      , Col 0x09 \"RowBytes\" False \"uinteger_4\" (Just uuinteger_4Type)"
        , "      , Col 0x0A \"LastID\" False \"uid\" (Just uuidType)"
        , "      , Col 0x0B \"MinSize\" False \"uinteger_4\" (Just uuinteger_4Type)"
        , "      , Col 0x0C \"MaxSize\" False \"uinteger_4\" (Just uuinteger_4Type)"
        , "      ]"
        , "  }" ]
    it "gives T233 its 14 columns, the one whose type no Type row defines without a UID" $ do
      length [ l | l <- block "logTableDesc", "Col 0x" `T.isInfixOf` l ] `shouldBe` 14
      block "logTableDesc" `shouldContain` ["      , Col 0x0B \"LogKind\" False \"log_kind\" Nothing"]
    it "cites the table, with its PDF and printed page" $
      lns `shouldContain` ["-- | core-2.01 T233 Log Table Description (PDF 291 / p.274, ord 235, sha256:"
                             <> T.take 12 (maybe "" cellsHash (Map.lookup (TableId 233) (numberedTables ir))) <> ")"]
    it "imports the UIDs it names from the modules the tableUIDs and typeRow rules write" $ do
      lns `shouldSatisfy` any ("import           Hsed.Core.Table " `T.isPrefixOf`)
      lns `shouldContain` ["import           Hsed.Gen.Core.TableUIDs", "import           Hsed.Gen.Core.Types"]
