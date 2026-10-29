{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.TableUIDsSpec
Description : Test specifications for the tableUIDs rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The tableUIDs rule on the committed Core extraction with the committed
annotations (design example E6, M2a): it fires on Table 240 alone, from the
doc anchor's @rule tableUIDs@; its 46 table names are the names a
@uidref{...}@ may name; C-T240-HALVES passes on all 46 rows and fails on one
edited cell, each of its relations in turn (the descriptor's low half and the
table's high half, the table's zero low half, the descriptor in the table of
tables); C-T240-NAME refuses a name that is no identifier or that two rows
give; the generated module has the hand-written module's shapes, each row
citing the page it is printed on.
-}
module Hsed.Spec.Rules.TableUIDsSpec (spec) where

import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor          (AnchorId (..))
import           Hsed.Spec.Check           (checkIR, mkContext)
import           Hsed.Spec.Common          (ErrataSet (..), RuleName (..), isError, renderFinding)
import           Hsed.Spec.Compile         (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.RuleDef         (Classification (..))
import           Hsed.Spec.Rules           (classify, explain)
import           Hsed.Spec.Rules.TableUIDs


spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx  = mkContext Scott ir
      rows = rights (tableUIDRows ctx)
      errorsOf ir' = map renderFinding (filter isError (checkIR Scott ir'))

  describe "the tableUIDs rule" $ do
    it "binds Table 240, and no other table, by the doc anchor's rule token" $ do
      [ aid | (aid, RuleBound (RuleName "tableUIDs") _ _) <- Map.toList (classify ir) ] `shouldBe` [TableId 240]
      explain ir (TableId 240) `shouldBe` "tableUIDs (exemplar core T240)"
    it "reads the 46 rows, every one of them whole" $ do
      length rows `shouldBe` 46
      lefts (tableUIDRows ctx) `shouldBe` []
      take 3 (map tuName rows) `shouldBe` ["Table", "SPInfo", "SPTemplates"]
    it "gives the 46 table names a uidref{...} may name" $
      length (tableNames ctx) `shouldBe` 46

  describe "C-T240-HALVES" $ do
    it "passes on all 46 rows: the descriptor object's low half is the table's high half" $ do
      [ tuName r | r <- rows, fmap (drop 4) (tuObject r) /= fmap (take 4) (tuTable r) ] `shouldBe` []
      all (\r -> isJust (tuObject r) && isJust (tuTable r)) rows `shouldBe` True
      errorsOf ir `shouldBe` []
    it "fails on a row whose halves disagree, and leaves that row out of the module" $ do
      let broken = setCell "T240" 6 1 "00 00 00 17 00 00 00 00" ir
      errorsOf broken `shouldBe`
        [ "C-T240-HALVES core-2.01.pdf#page=302&T240: row 7 AccessControl: the low half 00 00 00 07 of the descriptor object's UID is not the high half 00 00 00 17 of the table's UID" ]
      newlyExcluded ir broken `shouldReturn` [(TableId 240, "row 7 AccessControl: C-T240-HALVES or C-T240-NAME failed")]
    it "fails on a UID cell that is not eight bytes" $
      errorsOf (setCell "T240" 0 0 "00 00 00 01 00 00 01" ir) `shouldBe`
        [ "C-T240-HALVES core-2.01.pdf#page=302&T240: row 1 Table: the descriptor object's UID cell \"00 00 00 01 00 00 01\" is not eight bytes" ]
    it "fails on a descriptor object outside the table of tables and a table UID whose low half is not zero (the middle halves agreeing)" $ do
      let moved = setCell "T240" 1 1 "00 00 00 02 00 00 00 07" (setCell "T240" 1 0 "00 00 00 09 00 00 00 02" ir)
      errorsOf moved `shouldBe`
        [ "C-T240-HALVES core-2.01.pdf#page=302&T240: row 2 SPInfo: the high half 00 00 00 09 of the descriptor object's UID is not 00 00 00 01, the half-UID of the table of tables (row 1 Table)"
        , "C-T240-HALVES core-2.01.pdf#page=302&T240: row 2 SPInfo: the low half 00 00 00 07 of the table's UID is not zero" ]
      newlyExcluded ir moved `shouldReturn` [(TableId 240, "row 2 SPInfo: C-T240-HALVES or C-T240-NAME failed")]
    it "fails when no row, or more than one, is the table of tables" $ do
      errorsOf (setCell "T240" 0 1 "00 00 00 3F 00 00 00 00" (setCell "T240" 0 0 "00 00 00 01 00 00 00 3F" ir))
        `shouldSatisfy` elem "C-T240-HALVES core-2.01.pdf#page=302&T240: no row's descriptor object lies in its own table, so no row is the table of tables"
      errorsOf (setCell "T240" 1 0 "00 00 00 02 00 00 00 02" ir)
        `shouldSatisfy` elem "C-T240-HALVES core-2.01.pdf#page=302&T240: rows 1 Table and 2 SPInfo each have their descriptor object in their own table"

  describe "C-T240-NAME" $
    it "refuses a name that is no identifier, and one an earlier row gives" $
      filter ("C-T240-NAME" `T.isPrefixOf`) (errorsOf (setCell "T240" 2 2 "SP Templates" (setCell "T240" 1 2 "Table" ir))) `shouldBe`
        [ "C-T240-NAME core-2.01.pdf#page=302&T240: row 2 Table: the table name is also that of row 1"
        , "C-T240-NAME core-2.01.pdf#page=302&T240: row 3 SP Templates: the table name is not an identifier" ]

  describe "the generated module" $ do
    cr <- runIO (either (throwString . show) pure (compileIR Scott ir))
    let text  = fromMaybe "" (lookup "Hsed.Gen.Core.TableUIDs" (crModules cr))
        lns   = T.lines text
    it "declares u<T>TableObject, u<T>Table and h<T> for every row, as hsed's hand module names them" $ do
      length [ l | l <- lns, " :: UID" `T.isSuffixOf` l ] `shouldBe` 92
      length [ l | l <- lns, " :: HalfUID" `T.isSuffixOf` l ] `shouldBe` 46
      lns `shouldContain` ["uAccessControlTableObject :: UID", "uAccessControlTableObject = uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x07"]
      lns `shouldContain` ["hMBR :: HalfUID", "hMBR = halfUID 0x00 0x00 0x08 0x04"]
    it "maps every half-UID and UID to its name, as nameHalfUID and nameUID do" $ do
      lns `shouldContain` ["nameHalfUID :: Map HalfUID String", "nameHalfUID = fromList", "  [ (hTable, \"Table\")"]
      lns `shouldContain` ["nameUID :: Map UID String", "nameUID = fromList", "  [ (uTableTable, \"Table Table\")", "  , (uTableTableObject, \"Table Table Object\")"]
    it "declares no typed half-UID: Core does not say which tables are byte tables" $
      filter (\l -> any (`T.isPrefixOf` l) ["oth", "bth"]) lns `shouldBe` []
    it "cites the page each row is printed on: rows 1-31 on PDF 302, rows 32-46 on PDF 303" $ do
      lns `shouldContain` ["-- | core-2.01 T240 Table UIDs (PDF 302 / p.285, ord 242, sha256:2cd1f420bb70), row 31: CryptoSuite"]
      lns `shouldContain` ["-- | core-2.01 T240 Table UIDs (PDF 303 / p.286, ord 242, sha256:2cd1f420bb70), row 32: Template"]
