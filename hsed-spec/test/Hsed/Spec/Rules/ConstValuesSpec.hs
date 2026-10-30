{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.Rules.ConstValuesSpec
Description : Test specifications for the constValues rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The value cells a table of default values reads (a radix per table, the
remarks checked); then the rule (design section 3.4) on the committed Core
extraction with the committed annotations: the nine tables of elliptic curve
parameters it binds from the doc anchor's rule token, each belonging to the
description its caption names and that encloses it, Table 188 in decimal and
the other eight in hex, every value fitting its column's type;
C-CONST-DESC and C-CONST-VALUE made to fail by one edited cell or caption;
the generated defaults.
-}
module Hsed.Spec.Rules.ConstValuesSpec (spec) where

import qualified RIO.List                   as L
import qualified RIO.Map                    as Map
import qualified RIO.Text                   as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor           (AnchorId (..))
import           Hsed.Spec.Check            (checkIR, mkContext)
import           Hsed.Spec.Common           (ErrataSet (..), RuleName (..), isError, renderFinding)
import           Hsed.Spec.Compile          (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR               (SpecIR (..), TableIR (..))
import           Hsed.Spec.RuleDef          (BoundTable (..), Classification (..))
import           Hsed.Spec.Rules            (classify, explain)
import           Hsed.Spec.Rules.ConstValues
import           Hsed.Spec.Rules.TableDesc  (DescRead (..))


errorsOf :: SpecIR -> [Text]
errorsOf = map renderFinding . filter isError . checkIR Nothing Scott

-- | The IR with a table's caption replaced.
withCaption :: Text -> Text -> SpecIR -> SpecIR
withCaption tid caption ir = ir { tables = map (\t -> if t.id == Just tid then t { caption = Just caption } else t) ir.tables }

spec :: Spec
spec = do
  describe "parseConstNumber" $ do
    it "reads decimal, or hex in groups" $ do
      parseConstNumber False "51841075954883162510413392745168936296187808697" `shouldBe` Right (ConstNumber 51841075954883162510413392745168936296187808697 Nothing)
      parseConstNumber True "01FF FFFFFFFF" `shouldBe` Right (ConstNumber 0x01FFFFFFFFFF Nothing)
      parseConstNumber False "4A" `shouldSatisfy` isLeft
    it "checks a decimal remark" $ do
      parseConstNumber True "4A (= 74 in decimal)" `shouldBe` Right (ConstNumber 74 (Just "(= 74 in decimal)"))
      parseConstNumber True "4B (= 74 in decimal)" `shouldBe` Left "the value is 75 and its remark \"(= 74 in decimal)\" says 74"
    it "checks a sum of powers of two whose exponents the text flattens (2192 is 2^192)" $ do
      parseConstNumber True "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFFFF FFFFFFFF = 2192 – 264 – 1"
        `shouldBe` Right (ConstNumber (2 ^ (192 :: Int) - 2 ^ (64 :: Int) - 1) (Just "= 2192 – 264 – 1"))
      parseConstNumber True "FFFF = 216 + 1" `shouldBe` Left "the value is 65535 and its remark \"= 216 + 1\" says 65537"
      parseConstNumber True "FF = 28 * 1" `shouldSatisfy` isLeft
    it "reads a table's radix from all its values: hex when one is grouped or has a hex letter" $ do
      printedInHex ["07", "06", "04 00000000"] `shouldBe` True
      printedInHex ["0C (= 12 in decimal)"] `shouldBe` True
      printedInHex ["900812823637587646514106462588455890498729007071", "07"] `shouldBe` False

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx = mkContext Scott ir
        ks  = constReads ctx
        valueIds = map TableId [188, 190 .. 204]

    describe "the constValues rule" $ do
      it "binds the nine tables of curve parameters, T188-T204 even, by the doc anchor's rule token" $ do
        [ aid | (aid, RuleBound (RuleName "constValues") _ _) <- Map.toList (classify ir) ] `shouldBe` valueIds
        explain ir (TableId 190) `shouldBe` "constValues (exemplar core T190)"
      it "gives each the description its caption names and that encloses it" $
        [ (btAnchor (kBound k), either (const Nothing) (Just . btAnchor . drBound) (kDesc k)) | k <- ks ]
          `shouldBe` [ (TableId n, Just (TableId (n - 1))) | n <- [188, 190 .. 204] ]
      it "reads Table 188 in decimal (its section says so) and the FIPS tables in hex" $
        [ (btAnchor (kBound k), kHex k) | k <- ks ] `shouldBe` (TableId 188, False) : [ (TableId n, True) | n <- [190, 192 .. 204] ]
      it "finds every value fitting its column's type, and every remark true" $
        errorsOf ir `shouldBe` []
      it "finds a table's values through the table it fills" $
        fmap (btAnchor . kBound) (valuesFor ctx "C_EC_283") `shouldBe` Just (TableId 204)

    describe "C-CONST-VALUE" $ do
      it "fails on a value too wide for its column, and leaves the table out" $ do
        let broken = setCell "T200" 0 1 "1FF" ir
        errorsOf broken `shouldBe`
          [ "C-CONST-VALUE core-2.01.pdf#page=191&T200: row 1 k1: the value does not fit in uinteger_1 (1 bytes)" ]
        newlyExcluded ir broken `shouldReturn` [(TableId 200, "C-CONST-DESC or C-CONST-VALUE failed")]
      it "fails on a remark the value does not equal" $
        errorsOf (setCell "T202" 0 1 "4B (= 74 in decimal)" ir) `shouldBe`
          [ "C-CONST-VALUE core-2.01.pdf#page=193&T202: row 1 k: the value is 75 and its remark \"(= 74 in decimal)\" says 74" ]
      it "fails on a row naming no column of the description, and on a column given twice" $ do
        errorsOf (setCell "T188" 0 0 "q" ir) `shouldBe`
          [ "C-CONST-VALUE core-2.01.pdf#page=179&T188: row 1 q: names no column of T187 C_EC_160" ]
        errorsOf (setCell "T188" 1 0 "p" ir) `shouldBe`
          [ "C-CONST-VALUE core-2.01.pdf#page=179&T188: row 2 p: the column is also given by row 1" ]
      it "fails on a decimal table given a hex digit: the table is then read in hex, and p no longer fits" $
        errorsOf (setCell "T188" 4 1 "5184107595488316251041339274516893629618780869A" ir) `shouldSatisfy` (not . null)

    describe "C-CONST-DESC" $ do
      it "fails on a caption naming a table whose description does not enclose it" $
        errorsOf (withCaption "T188" "Table 188 AACS Values for C_EC_192" ir) `shouldBe`
          [ "C-CONST-DESC core-2.01.pdf#page=179&T188: the caption names C_EC_192, described by T189 in 5.3.2.18, which does not enclose this table (in 5.3.2.17.15)" ]
      it "fails on a caption naming no described table" $
        errorsOf (withCaption "T190" "Table 190 FIPS P-192 Values" ir) `shouldBe`
          [ "C-CONST-DESC core-2.01.pdf#page=180&T190: the caption names no table (\"… Values for <T>\")" ]

    describe "the generated module" $ do
      cr <- runIO (either (throwString . show) pure (compileIR Nothing Scott ir))
      let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.ColumnDefaults" (crModules cr)))
          block n = takeWhile (/= "") (dropWhile (/= (n <> " :: Row")) lns)
      it "declares one Row of defaults per table, nine in all" $
        length [ l | l <- lns, " :: Row" `T.isSuffixOf` l ] `shouldBe` 9
      it "gives each value by its column's number, in decimal as Table 188 prints it" $
        block "c_EC_160Defaults" `shouldBe`
          [ "c_EC_160Defaults :: Row"
          , "c_EC_160Defaults ="
          , "  [ (0x03, UIntCell 900812823637587646514106462588455890498729007071)  -- p"
          , "  , (0x04, UIntCell 900812823637587646514106555566573588779770753047)  -- r"
          , "  , (0x05, UIntCell 366394034647231750324370400222002566844354703832)  -- b"
          , "  , (0x06, UIntCell 264865613959729647018113670854605162895977008838)  -- x"
          , "  , (0x07, UIntCell 51841075954883162510413392745168936296187808697)  -- y"
          , "  ]" ]
      it "in hex, with the digits the FIPS tables print, leading zeros kept" $ do
        block "c_EC_192Defaults" `shouldContain` ["  , (0x07, UIntCell 0x07192B95FFC8DA78631011ED6B24CDD573F977A11E794811)  -- y"]
        block "c_EC_233Defaults" `shouldContain` ["  [ (0x03, UIntCell 0x4A)  -- k"]
      it "names the description each table fills" $
        lns `shouldSatisfy` any ("-- | core-2.01 T204 FIPS K-283 Values for C_EC_283 (PDF 195 / p.178, ord 206, sha256:" `T.isPrefixOf`)
      it "orders the rows by column number" $
        [ T.takeWhile (/= ',') (T.drop 5 l) | l <- block "c_EC_163Defaults", "(0x" `T.isInfixOf` l ]
          `shouldBe` L.sort [ T.takeWhile (/= ',') (T.drop 5 l) | l <- block "c_EC_163Defaults", "(0x" `T.isInfixOf` l ]
