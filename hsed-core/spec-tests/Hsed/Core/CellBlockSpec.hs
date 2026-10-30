{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Core.CellBlockSpec
Description : Test specifications for Hsed.Core.CellBlock
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The cell_block abstract type (Core 5.1.4.2.3, PDF 129-130): UCT-03's
@[3 = 3, 4 = 3]@ written and read back; every component read; what is no
cell_block refused (components out of order, past the fifth, named by other
than a uinteger, of the wrong type, or no list at all); and each context
rule, a.a to e.b, with Get's Fails b-d (PDF 201), on an object, an object
table and a byte table.
-}
module Hsed.Core.CellBlockSpec (spec) where

import           RIO
import qualified RIO.Text         as T
import           Test.Hspec

import           Hsed.Core.CellBlock
import           Hsed.Core.Token  (IsToken (..), Token (..))
import           Hsed.Core.UID    (UID, uid)
import           Hsed.Core.Value


uTable, uObject :: UID
uTable  = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x00 0x00
uObject = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x84 0x02

-- | A component named by its number.
named :: IsValue v => Natural -> v -> Value
named i v = N (i .: v)

-- | Whether a resolution fails naming a rule.
failsWith :: Text -> Either Text a -> Bool
failsWith rule = either (rule `T.isInfixOf`) (const False)

spec :: Spec
spec = do
  describe "cellBlockValue and cellBlock" $ do
    it "writes UCT-03's [3 = 3, 4 = 3], each component Named by its number, and reads it back" $ do
      cellBlockValue (columnRange 3 3) `shouldBe` L (List [named 3 (3 :: Natural), named 4 (3 :: Natural)])
      cellBlock (cellBlockValue (columnRange 3 3)) `shouldBe` Right (columnRange 3 3)
    it "writes nothing for no component" $
      cellBlockValue noCells `shouldBe` L (List [])
    it "reads every component: Table a uid, startRow a uid or a row number, the rest uintegers" $ do
      cellBlock (L (List [named 0 (token uTable), named 1 (token uObject), named 2 (5 :: Natural), named 3 (1 :: Natural), named 4 (2 :: Natural)]))
        `shouldBe` Right (CellBlock (Just uTable) (Just (RowUID uObject)) (Just 5) (Just 1) (Just 2))
      cellBlock (L (List [named 1 (7 :: Natural)])) `shouldBe` Right noCells { cbStartRow = Just (RowNumber 7) }
    it "round-trips every component" $ do
      let full = CellBlock (Just uTable) (Just (RowUID uObject)) (Just 5) (Just 1) (Just 2)
      cellBlock (cellBlockValue full) `shouldBe` Right full
      cellBlock (cellBlockValue full { cbStartRow = Just (RowNumber 9) }) `shouldBe` Right full { cbStartRow = Just (RowNumber 9) }
    it "refuses components out of order or repeated (5.1.4.1 b)" $ do
      cellBlock (L (List [named 4 (3 :: Natural), named 3 (3 :: Natural)])) `shouldSatisfy` failsWith "order"
      cellBlock (L (List [named 3 (3 :: Natural), named 3 (4 :: Natural)])) `shouldSatisfy` failsWith "order"
    it "refuses a component past the fifth, or named by other than a uinteger" $ do
      cellBlock (L (List [named 5 (1 :: Natural)])) `shouldSatisfy` failsWith "indexed 5"
      cellBlock (L (List [N (("startColumn" :: String) .: (3 :: Natural))])) `shouldSatisfy` failsWith "not by a uinteger"
    it "refuses a component of the wrong type, one that is no Named value, and a value that is no list" $ do
      cellBlock (L (List [named 3 (Bytes "ab")])) `shouldSatisfy` failsWith "startColumn is not a uinteger"
      cellBlock (L (List [named 0 (3 :: Natural)])) `shouldSatisfy` failsWith "Table is not an eight-byte uid"
      cellBlock (L (List [named 1 (Signed (-1))])) `shouldSatisfy` failsWith "startRow is neither"
      cellBlock (L (List [D (Datum (Unsigned 3))])) `shouldSatisfy` failsWith "no Named value"
      cellBlock (D (Datum (Unsigned 3))) `shouldSatisfy` failsWith "is a list"

  describe "resolveCells on an object" $ do
    let resolve = resolveCells (OnObject 7)
    it "defaults to every column (d.a, e.a), and takes a column range" $ do
      resolve noCells `shouldBe` Right (Columns 0 7)
      resolve (columnRange 3 3) `shouldBe` Right (Columns 3 3)
      resolve noCells { cbStartColumn = Just 2 } `shouldBe` Right (Columns 2 7)
      resolve noCells { cbEndColumn = Just 2 } `shouldBe` Right (Columns 0 2)
    it "refuses Table (a.b), startRow (b.c) and endRow (c.b): Get's Fails b" $ do
      resolve noCells { cbTable = Just uTable } `shouldSatisfy` failsWith "a.b"
      resolve noCells { cbStartRow = Just (RowUID uObject) } `shouldSatisfy` failsWith "b.c"
      resolve noCells { cbStartRow = Just (RowNumber 0) } `shouldSatisfy` failsWith "b.c"
      resolve noCells { cbEndRow = Just 0 } `shouldSatisfy` failsWith "c.b"
    it "refuses a column past the last, or a start past the end: Get's Fails d" $ do
      resolve (columnRange 0 8) `shouldSatisfy` failsWith "5.3.3.6.3 d"
      resolve noCells { cbStartColumn = Just 8 } `shouldSatisfy` failsWith "5.3.3.6.3 d"
      resolve (columnRange 4 3) `shouldSatisfy` failsWith "5.3.3.6.3 d"

  describe "resolveCells on an object table" $ do
    let resolve = resolveCells (OnObjectTable 7)
    it "requires startRow, the uid of the object (b.b), and takes its columns" $ do
      resolve noCells { cbStartRow = Just (RowUID uObject) } `shouldBe` Right (ObjectRow uObject 0 7)
      resolve (columnRange 3 3) { cbStartRow = Just (RowUID uObject) } `shouldBe` Right (ObjectRow uObject 3 3)
      resolve noCells `shouldSatisfy` failsWith "b.b"
      resolve noCells { cbStartRow = Just (RowNumber 1) } `shouldSatisfy` failsWith "b.b"
    it "refuses Table on a Get (a.c) and endRow (c.b)" $ do
      resolve noCells { cbTable = Just uTable, cbStartRow = Just (RowUID uObject) } `shouldSatisfy` failsWith "a.c"
      resolve noCells { cbStartRow = Just (RowUID uObject), cbEndRow = Just 0 } `shouldSatisfy` failsWith "c.b"

  describe "resolveCells on a byte table" $ do
    let resolve = resolveCells (OnByteTable 99)
    it "defaults to every row (b.a, c.a), and takes a row range" $ do
      resolve noCells `shouldBe` Right (ByteRows 0 99)
      resolve noCells { cbStartRow = Just (RowNumber 10) } `shouldBe` Right (ByteRows 10 99)
      resolve noCells { cbEndRow = Just 5 } `shouldBe` Right (ByteRows 0 5)
      resolve noCells { cbStartRow = Just (RowNumber 5), cbEndRow = Just 5 } `shouldBe` Right (ByteRows 5 5)
    it "refuses startColumn (d.b) and endColumn (e.b): Get's Fails c; Table on a Get (a.c); a uid for startRow" $ do
      resolve noCells { cbStartColumn = Just 0 } `shouldSatisfy` failsWith "d.b"
      resolve noCells { cbEndColumn = Just 0 } `shouldSatisfy` failsWith "e.b"
      resolve noCells { cbTable = Just uTable } `shouldSatisfy` failsWith "a.c"
      resolve noCells { cbStartRow = Just (RowUID uObject) } `shouldSatisfy` failsWith "5.1.4.2.3 b"
    it "refuses a row past the last, or a start past the end: Get's Fails d" $ do
      resolve noCells { cbEndRow = Just 100 } `shouldSatisfy` failsWith "5.3.3.6.3 d"
      resolve noCells { cbStartRow = Just (RowNumber 100) } `shouldSatisfy` failsWith "5.3.3.6.3 d"
      resolve noCells { cbStartRow = Just (RowNumber 50), cbEndRow = Just 10 } `shouldSatisfy` failsWith "5.3.3.6.3 d"
