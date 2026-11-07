{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.BytesSpec
Description : Test specifications for Hsed.Sim.Bytes: a byte table's rows read from one row to another, the fill with the extents written over it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

M5b's group G1 (design 11 M5b): a byte table's rows from a start row to an
end row, both included, the lowest first (Core 5.3.3.6.2.1, PDF 201: Get on
a byte table returns its values from the lowest numbered row to the
highest), each row one byte (3.2.5.2, PDF 51), the fill where nothing was
written and the byte of the extent written over it elsewhere
("Hsed.Sim.Types".@ByteTable@, sparse since M5a): a table nothing was
written to, extents before, within, across and after the rows asked, two
extents touching, one row alone, and, for random tables whose extents never
overlap, the reading a naive table of every row gives. And the most bytes
one byte sequence carries in a token of at most a size, which bounds a byte
Get's rows by the host's MaxIndTokenSize (Core 5.2.2.4.1.5, PDF 146), held
to the codec's own atoms for every size up to 4200 and at a long atom's
most (3.2.2.3.1, PDF 33).
-}
module Hsed.Sim.BytesSpec (spec) where

import qualified Data.IntMap.Strict    as IntMap
import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Gen, arbitrary, chooseInt, counterexample, forAll, vectorOf, (===))

import           Hsed.Core.StreamItem  (StreamItem (..))
import           Hsed.Core.Token       (Token (..))
import           Hsed.Sim.Bytes        (byteRange, bytesWithin)
import           Hsed.Sim.Types        (ByteTable (..))


-- | A table of eight rows, 0xFF where nothing was written, with @ab@ at 0,
--   @cd@ at 3 and @ef@ at 6.
written :: ByteTable
written = ByteTable 8 0xFF (IntMap.fromList [(0, "ab"), (3, "cd"), (6, "ef")])

-- | A random byte table and rows of it to read: a size of 1 to 64, any
--   fill, and extents that never overlap, laid from the first row with a
--   gap before each (none, so that two extents touch, or some), each of
--   random bytes; then a start row and an end row no earlier than it, both
--   within the table.
genTableAndRows :: Gen (ByteTable, Natural, Natural)
genTableAndRows = do
  size    <- chooseInt (1, 64)
  fill    <- arbitrary
  extents <- laid 0 size
  s       <- chooseInt (0, size - 1)
  e       <- chooseInt (s, size - 1)
  pure (ByteTable (fromIntegral size) fill (IntMap.fromList extents), fromIntegral s, fromIntegral e)
  where
    laid at size
      | at >= size = pure []
      | otherwise  = do
          gap <- chooseInt (0, size - at)
          if at + gap >= size then pure [] else do
            len   <- chooseInt (1, size - (at + gap))
            bytes <- B.pack <$> vectorOf len arbitrary
            ((at + gap, bytes) :) <$> laid (at + gap + len) size

-- | The naive reading: every row's byte, the extent's over it or the fill.
naive :: Natural -> Natural -> ByteTable -> ByteString
naive s e bt = B.pack [ fromMaybe (btFill bt) (L.lookup i rows) | i <- [fromIntegral s .. fromIntegral e] ]
  where rows = [ (o + i, b) | (o, bs) <- IntMap.toList (btWritten bt), (i, b) <- zip [0 ..] (B.unpack bs) ]

spec :: Spec
spec = do
  byteRangeSpec
  bytesWithinSpec

-- | 'byteRange'.
byteRangeSpec :: Spec
byteRangeSpec = describe "byteRange, a byte table's rows from one to another (Core 5.3.3.6.2.1, PDF 201)" $ do
  it "reads the fill where nothing was written, as many bytes as rows asked, one row one byte (Core 3.2.5.2, PDF 51)" $ do
    byteRange 2 5 (ByteTable 8 0xFF mempty) `shouldBe` B.replicate 4 0xFF
    byteRange 0 7 (ByteTable 8 0 mempty) `shouldBe` B.replicate 8 0
    byteRange 7 7 (ByteTable 8 0x5A mempty) `shouldBe` "\x5A"
  it "overlays each extent written on the fill, from its offset, the lowest row first" $ do
    byteRange 0 7 written `shouldBe` "ab\255cd\255ef"
    byteRange 2 5 written `shouldBe` "\255cd\255"
    byteRange 1 6 written `shouldBe` "b\255cd\255e"
  it "reads one row of an extent, or one of the fill, alone" $ do
    byteRange 4 4 written `shouldBe` "d"
    byteRange 5 5 written `shouldBe` "\255"
  it "reads rows that an extent only begins or ends in, and rows no extent reaches" $ do
    byteRange 0 0 written `shouldBe` "a"
    byteRange 7 7 written `shouldBe` "f"
    byteRange 2 2 written `shouldBe` "\255"
    byteRange 5 5 (ByteTable 8 0 (IntMap.fromList [(0, "abcde"), (6, "gh")])) `shouldBe` "\NUL"
  it "reads two extents that touch as one run" $
    byteRange 0 5 (ByteTable 6 0 (IntMap.fromList [(0, "abc"), (3, "def")])) `shouldBe` "abcdef"
  it "ignores an empty extent" $
    byteRange 0 3 (ByteTable 4 0x11 (IntMap.fromList [(1, "")])) `shouldBe` B.replicate 4 0x11
  prop "equals a naive reading of the fill with the extents over it, for random tables whose extents never overlap" $
    forAll genTableAndRows $ \(bt, s, e) ->
      counterexample (show (btWritten bt, s, e)) (byteRange s e bt === naive s e bt)
  prop "gives as many bytes as rows asked" $
    forAll genTableAndRows $ \(bt, s, e) -> B.length (byteRange s e bt) === fromIntegral (e - s + 1)

-- | 'bytesWithin', held to the codec: the bytes it writes one byte sequence
--   of n bytes in, its header and its data ("Hsed.Core.Token").
bytesWithinSpec :: Spec
bytesWithinSpec = describe "bytesWithin, the most bytes one byte sequence carries in a token of at most a size (Core 5.2.2.4.1.5, PDF 146; 3.2.2.3.1, PDF 33)" $ do
  it "is, for every size up to 4200, the most bytes whose token the codec writes no larger, short, medium and long atoms among them, and 0 where even an empty sequence's one byte does not fit" $
    forM_ [0 .. 4200] $ \size -> do
      let n = bytesWithin size
      (size, n >= 0, size < 1 || tokenOf n <= size, tokenOf (n + 1) > size) `shouldBe` (size, True, True, True)
  it "carries at most 16,777,215 bytes, the most one long atom holds, the simulator sending no continued token (5.2.2.4.1.8, PDF 146)" $ do
    map bytesWithin [0xFFFFFF + 3, 0xFFFFFF + 4, maxBound] `shouldBe` [0xFFFFFF - 1, 0xFFFFFF, 0xFFFFFF]
    tokenOf 0xFFFFFF `shouldBe` 0xFFFFFF + 4
  where
    tokenOf n = B.length (generate (Bytes (B.replicate n 0)))
