{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.ColumnsAdditionSpec
Description : Test specifications for the columnsAddition rule's reads
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The columnsAddition rule (design example E5) at M4b's scaffold: the target a
caption names; the rule's exemplar, Opal Table 26, on which it still fires;
and, on the committed Opal extraction under the M4b sidecar read over the
committed Core run, the three additions it reads (T26, T43, T50) and every
description Opal reads with them applied: its own (T32,
DataRemovalMechanism, 2 columns), Core's three it extends (TPerInfo 9
columns, 0x08 ProgrammaticResetEnable a boolean, PDF 49; Table 15, 0x0D and
0x0E uinteger_4, PDF 98-99; LockingInfo 11, PDF 85) and Core's other 42 as
Core reads them. Its checks and declarations are the Opal type layer's (G3).
-}
module Hsed.Spec.Rules.ColumnsAdditionSpec (spec) where

import qualified RIO.List                      as L
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor              (AnchorId (..))
import           Hsed.Spec.Common              (SpecId (..))
import qualified Hsed.Spec.Common              as Cm
import           Hsed.Spec.CoreIR
import           Hsed.Spec.RuleDef             (BoundTable (..), Context (..), RuleDef (..))
import           Hsed.Spec.Rules               (checkExemplarsWith)
import           Hsed.Spec.Rules.ColumnsAddition
import           Hsed.Spec.Rules.TableDesc     (ColRead (..), DescRead (..), descReads, tableDescRule)
import           Hsed.Spec.Rules.TypeRow       (TypeRowRead (..))


-- | A description's columns: number, name and type, as printed.
columnsOf :: DescRead -> [(Text, Text, Text)]
columnsOf d = [ (crNumberText c, crName c, crType c) | c <- drColumns d ]

-- | The description of a table among a spec's, with the spec that owns it.
descOf :: Text -> [(Context, DescRead)] -> Maybe (SpecId, DescRead)
descOf n ds = listToMaybe [ (cxSpec c, d) | (c, d) <- ds, drTable d == Just n ]

opalId :: SpecId
opalId = SpecId "opal-2.30"

spec :: Spec
spec = do
  describe "additionTarget" $
    it "reads the table a caption names before Columns: an SP and its dash dropped, then Additional, then Table" $ do
      map additionTarget ["Admin SP \x2013 TPerInfo", "Table Table Additional", "Locking SP \x2013 LockingInfo", "Admin SP - TPerInfo"]
        `shouldBe` [Just "TPerInfo", Just "Table", Just "LockingInfo", Just "TPerInfo"]
      map additionTarget ["Admin SP \x2013", "", "Two Words"] `shouldBe` [Nothing, Nothing, Nothing]

  describe "the rule" $ do
    it "refines tableDesc, whose shape is the same, and claims C-COLS-TARGET" $ do
      refinesRule columnsAdditionRule `shouldBe` Just (ruleName tableDescRule)
      map Cm.codeName (ruleCodes columnsAdditionRule) `shouldBe` ["C-COLS-TARGET"]
      exemplar columnsAdditionRule `shouldBe` (opalId, TableId 26)
    withOpal $ \opalPristine ->
      it "still fires on its exemplar, Opal Table 26, and its shape accepts it (no C-EXEMPLAR)" $
        checkExemplarsWith [columnsAdditionRule] opalPristine `shouldBe` []

  withM4bOpal $ \base opal -> do
    let octx = opalOver base opal
        ds   = allDescs octx
    describe "additions, on Opal under the M4b sidecar" $
      it "reads Tables 26, 43 and 50, each the table its caption names and its columns" $
        [ (btAnchor (adBound a), adTarget a, map crNumberText (adColumns a)) | a <- additions octx ]
          `shouldBe` [ (TableId 26, Just "TPerInfo", ["0x08"]), (TableId 43, Just "LockingInfo", ["0x07", "0x08", "0x09", "0x0A"])
                     , (TableId 50, Just "Table", ["0x0D", "0x0E"]) ]

    describe "allDescs, on Opal under the M4b sidecar, read over Core" $ do
      it "gives TPerInfo Core T211's eight columns and T26's 0x08 ProgrammaticResetEnable, a boolean (PDF 49), as Opal's" $ do
        fmap fst (descOf "TPerInfo" ds) `shouldBe` Just opalId
        fmap (length . columnsOf . snd) (descOf "TPerInfo" ds) `shouldBe` Just 9
        fmap (drop 8 . columnsOf . snd) (descOf "TPerInfo" ds) `shouldBe` Just [("0x08", "ProgrammaticResetEnable", "boolean")]
        fmap (btAnchor . drBound . snd) (descOf "TPerInfo" ds) `shouldBe` Just (TableId 211)
      it "gives Table 15 columns, 0x0D MandatoryWriteGranularity and 0x0E RecommendedAccessGranularity uinteger_4 (PDF 98-99)" $ do
        fmap (length . columnsOf . snd) (descOf "Table" ds) `shouldBe` Just 15
        fmap (drop 13 . columnsOf . snd) (descOf "Table" ds)
          `shouldBe` Just [("0x0D", "MandatoryWriteGranularity", "uinteger_4"), ("0x0E", "RecommendedAccessGranularity", "uinteger_4")]
      it "gives LockingInfo 11 columns, 0x07-0x0A from T43 (PDF 85)" $ do
        fmap (length . columnsOf . snd) (descOf "LockingInfo" ds) `shouldBe` Just 11
        fmap (map (\(n, _, _) -> n) . drop 7 . columnsOf . snd) (descOf "LockingInfo" ds) `shouldBe` Just ["0x07", "0x08", "0x09", "0x0A"]
      it "gives DataRemovalMechanism Opal's own two columns (T32, its caption read through the typo table)" $ do
        fmap fst (descOf "DataRemovalMechanism" ds) `shouldBe` Just opalId
        fmap (columnsOf . snd) (descOf "DataRemovalMechanism" ds)
          `shouldBe` Just [("0x00", "UID", "uid"), ("0x01", "ActiveDataRemovalMechanism", "data_removal_mechanism")]
      it "reads each column's type over the base: Core's boolean for T26's addition, Opal's own T30 for T32's" $ do
        fmap (map (fmap trAnchor . crTypeRow) . drColumns . snd) (descOf "DataRemovalMechanism" ds) `shouldBe` Just [Just (TableId 145), Just (TableId 30)]
        fmap (map (fmap trName . crTypeRow) . drop 8 . drColumns . snd) (descOf "TPerInfo" ds) `shouldBe` Just [Just "boolean"]
      it "gives every other description of Core's as Core reads it, one per table, Opal's first" $ do
        length ds `shouldBe` 1 + length (descReads base)
        L.nub (mapMaybe (drTable . snd) ds) `shouldBe` mapMaybe (drTable . snd) ds
        map (cxSpec . fst) (take 4 ds) `shouldBe` replicate 4 opalId
        [ cxSpec c | (c, _) <- drop 4 ds ] `shouldSatisfy` all (== SpecId "core-2.01")
