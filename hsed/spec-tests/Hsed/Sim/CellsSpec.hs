{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.CellsSpec
Description : Test specifications for Hsed.Sim.Cells: a value a Set gives a column read as the column's cell, or refused
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

M5b's group G1 (design 11 M5b): the two column types the simulator's Set
carries out, told by the UID of the column's Type object. A boolean (Core
Table 59, PDF 103) is a uinteger 0 or 1 (5.1.2 b, PDF 99), the cell the
fold's @booleanCell@ gives, which @isTrue@ reads; 2, a signed integer,
bytes or a list are not of the type (5.1.5.11 b, PDF 139: INVALID_PARAMETER,
Set's Fails c, PDF 203). A reset_types set (Core Table 127, a Set_Type over
0 to 31, PDF 119; its values Opal 2.30 Table 16's, PDF 32, which replaces
Core Table 128's) is a list of uintegers 0 to 31, none twice (5.1.2 i, PDF
100), the cell a list of the enumeration's cells in ascending value,
labelled as the fold labels them (Power Cycle, Hardware, HotPlug,
Programmatic), which "Hsed.Sim.Reset".@resetListed@ reads as it reads a
folded one; a value past 31, one listed twice, a datum or an element that
is no uinteger are not of the type. Any other type (a name, a uinteger_8, a
uid, a reference) is one the simulator's Set does not carry out until M6
(5.1.5.16, PDF 139: FAIL; Scott's answer Q3), the refusal naming the
column and its type.
-}
module Hsed.Sim.CellsSpec (spec) where

import qualified RIO.List              as L
import           RIO
import qualified RIO.Text              as T
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (chooseInt, forAll, shuffle, sublistOf, (===))

import           Hsed.Core.Table       (Cell (..), Col (..), columnNamed)
import           Hsed.Core.Token       (Token (..))
import           Hsed.Core.TypeName    (TypeName (..))
import           Hsed.Core.Value       (Datum (..), List (..), Value (..), (.:))
import           Hsed.Gen.Core.TableDescs (authorityTableDesc, lockingTableDesc, mbrControlTableDesc)
import           Hsed.Gen.Core.Types   (ubooleanType, ureset_typesType)
import           Hsed.Gen.Opal.Enums   (Opal_reset_types (..))
import           Hsed.Sim.Cells
import           Hsed.Sim.Reset        (ResetKind (..), resetListed)


-- | A column of a generated description, by name.
column :: Text -> Col
column name = fromMaybe (error ("no column " <> T.unpack name)) (listToMaybe (mapMaybe (columnNamed name) [authorityTableDesc, lockingTableDesc, mbrControlTableDesc]))

-- | A uinteger value, and a list of them.
uinteger :: Natural -> Value
uinteger = D . Datum . Unsigned

uintegers :: [Natural] -> Value
uintegers = L . List . map uinteger

-- | Whether a refusal is of a value not of the type, or of a type not
--   simulated.
notOfType, notSimulated :: Either CellRefusal Cell -> Bool
notOfType = \case
  Left (NotOfType _) -> True
  _                  -> False
notSimulated = \case
  Left (NotSimulated _) -> True
  _                     -> False

-- | The refusal's words.
reason :: Either CellRefusal Cell -> Text
reason = \case
  Left (NotOfType t)    -> t
  Left (NotSimulated t) -> t
  Right c               -> tshow c

spec :: Spec
spec = do
  describe "a boolean column (Core Table 59, PDF 103), told by its Type object's UID" $ do
    let enabled = column "Enabled"
    it "is Enabled's, the Authority table's column 5 (Core Table 180, PDF 167)" $
      (colNumber enabled, typeNameUID (colType enabled)) `shouldBe` (5, Just ubooleanType)
    it "takes 0 as False and 1 as True, the fold's cells, which isTrue reads (Core 5.1.2 b, PDF 99: an enumeration is its value)" $ do
      cellFor enabled (uinteger 0) `shouldBe` Right (booleanCell False)
      cellFor enabled (uinteger 1) `shouldBe` Right (booleanCell True)
      fmap isTrue (cellFor enabled (uinteger 1)) `shouldBe` Right True
      fmap isTrue (cellFor enabled (uinteger 0)) `shouldBe` Right False
    it "refuses 2 as not of the type (Core 5.1.5.11 b, PDF 139; Set's Fails c, PDF 203), naming the column" $ do
      cellFor enabled (uinteger 2) `shouldSatisfy` notOfType
      reason (cellFor enabled (uinteger 2)) `shouldSatisfy` ("column 5, Enabled," `T.isPrefixOf`)
    it "refuses a signed integer, bytes, a list and a Named value as not of the type" $ do
      cellFor enabled (D (Datum (Signed 1))) `shouldSatisfy` notOfType
      cellFor enabled (D (Datum (Bytes "\x01"))) `shouldSatisfy` notOfType
      cellFor enabled (uintegers [1]) `shouldSatisfy` notOfType
      cellFor enabled (N ((1 :: Natural) .: (1 :: Natural))) `shouldSatisfy` notOfType
    prop "refuses every uinteger past 1" $
      forAll (chooseInt (2, 100000)) $ \n -> notOfType (cellFor enabled (uinteger (fromIntegral n)))
    it "reads every boolean column the M5b cases set: the Global Range's ReadLockEnabled to WriteLocked (Core Table 226, PDF 273), MBRControl's Enable and Done (Table 229, PDF 277)" $
      [ fmap isTrue (cellFor (column name) (uinteger 1)) | name <- ["ReadLockEnabled", "WriteLockEnabled", "ReadLocked", "WriteLocked", "Enable", "Done"] ]
        `shouldBe` replicate 6 (Right True)

  describe "a reset_types column (Core Table 127, PDF 119; Opal 2.30 Table 16, PDF 32), told by its Type object's UID" $ do
    let lockOnReset = column "LockOnReset"
    it "is LockOnReset's, the Locking table's column 9, and MBRDoneOnReset's, MBRControl's column 3" $ do
      (colNumber lockOnReset, typeNameUID (colType lockOnReset)) `shouldBe` (9, Just ureset_typesType)
      (colNumber (column "MBRDoneOnReset"), typeNameUID (colType (column "MBRDoneOnReset"))) `shouldBe` (3, Just ureset_typesType)
    it "takes a list of uintegers as the set's cells in ascending value, labelled as Opal Table 16 and the fold print them (Table 45, PDF 86)" $ do
      cellFor lockOnReset (uintegers [0]) `shouldBe` Right (ListCell [EnumCell "Power Cycle" 0])
      cellFor lockOnReset (uintegers [0, 3]) `shouldBe` Right (ListCell [EnumCell "Power Cycle" 0, EnumCell "Programmatic" 3])
      cellFor lockOnReset (uintegers [3, 0]) `shouldBe` Right (ListCell [EnumCell "Power Cycle" 0, EnumCell "Programmatic" 3])
      cellFor lockOnReset (uintegers [2, 1, 3, 0])
        `shouldBe` Right (ListCell [EnumCell "Power Cycle" 0, EnumCell "Hardware" 1, EnumCell "HotPlug" 2, EnumCell "Programmatic" 3])
      cellFor lockOnReset (uintegers []) `shouldBe` Right (ListCell [])
    it "labels the reserved and vendor unique values as the generated enumeration names them (Core Table 128, PDF 119)" $ do
      cellFor lockOnReset (uintegers [4, 31]) `shouldBe` Right (ListCell [EnumCell "Reserved 4" 4, EnumCell "Vendor Unique 31" 31])
      map resetLabel [Opal_reset_types_Power_Cycle, Opal_reset_types_Hardware, Opal_reset_types_HotPlug, Opal_reset_types_Programmatic]
        `shouldBe` ["Power Cycle", "Hardware", "HotPlug", "Programmatic"]
    it "gives a cell resetListed reads as it reads a folded one (Hsed.Sim.Reset)" $ do
      fmap (resetListed OnTPerReset) (cellFor lockOnReset (uintegers [0, 3])) `shouldBe` Right True
      fmap (resetListed OnPowerCycle) (cellFor lockOnReset (uintegers [0, 3])) `shouldBe` Right True
      fmap (resetListed OnHardwareReset) (cellFor lockOnReset (uintegers [0, 3])) `shouldBe` Right False
      fmap (resetListed OnPowerCycle) (cellFor lockOnReset (uintegers [])) `shouldBe` Right False
    it "refuses a value past 31, a value listed twice (a set, Core 5.1.2 i, PDF 100), a datum, and an element that is no uinteger, as not of the type" $ do
      cellFor lockOnReset (uintegers [32]) `shouldSatisfy` notOfType
      cellFor lockOnReset (uintegers [0, 0]) `shouldSatisfy` notOfType
      cellFor lockOnReset (uintegers [0, 3, 0]) `shouldSatisfy` notOfType
      cellFor lockOnReset (uinteger 0) `shouldSatisfy` notOfType
      cellFor lockOnReset (L (List [D (Datum (Bytes "\x00"))])) `shouldSatisfy` notOfType
      cellFor lockOnReset (L (List [uinteger 0, D (Datum (Signed 3))])) `shouldSatisfy` notOfType
      reason (cellFor lockOnReset (uintegers [32])) `shouldSatisfy` ("column 9, LockOnReset," `T.isPrefixOf`)
    prop "reads any set of values 0 to 31, given in any order, as their cells in ascending value" $
      forAll (sublistOf [0 .. 31] >>= shuffle) $ \vs ->
        cellFor lockOnReset (uintegers vs) === Right (ListCell [ EnumCell (resetLabel k) (fromIntegral v) | v <- L.sort vs, Just k <- [kind v] ])
    prop "refuses a list naming any value twice" $
      forAll ((\vs v -> v : vs <> [v]) <$> sublistOf [0 .. 31] <*> chooseInt (0, 31)) $ \vs ->
        notOfType (cellFor lockOnReset (uintegers (map fromIntegral vs)))

  describe "a column of any other type" $ do
    it "is one the simulator's Set does not carry out until M6, FAIL (Core 5.1.5.16, PDF 139; Scott's answer Q3): a name, a uinteger_8, a uid, a reference, a messaging_type" $
      [ (name, notSimulated (cellFor (column name) (uinteger 1)))
      | name <- ["CommonName", "RangeStart", "UID", "Class", "Secure", "ActiveKey"] ]
        `shouldBe` [ (name, True) | name <- ["CommonName", "RangeStart", "UID", "Class", "Secure", "ActiveKey"] ]
    it "is refused whatever the value, the refusal naming the column and its type" $ do
      cellFor (column "CommonName") (D (Datum (Bytes "a name"))) `shouldSatisfy` notSimulated
      reason (cellFor (column "CommonName") (D (Datum (Bytes "a name")))) `shouldSatisfy` \t ->
        "column 2, CommonName," `T.isPrefixOf` t && "type name" `T.isInfixOf` t
    it "includes a column whose type no Type row defines" $
      cellFor (Col 0x20 "Nameless" False (TypeName "no such type" Nothing)) (uinteger 1) `shouldSatisfy` notSimulated
  where
    kind :: Natural -> Maybe Opal_reset_types
    kind v = L.find ((== fromIntegral v) . fromEnum) [minBound .. maxBound]
