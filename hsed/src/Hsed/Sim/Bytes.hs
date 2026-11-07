{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Bytes
Description : A simulated byte table's rows read, from one row to another
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A byte table as the simulator holds it ("Hsed.Sim.Types".'ByteTable', since
M5a: its size, the byte every row not written reads as, and the extents
written, sparse, so that the Locking SP's MBR of 128 MiB costs nothing until
the host writes it) read from one row to another, as Get on a byte table
returns them (Core 5.3.3.6.2.1, PDF 201: from the lowest numbered row to the
highest). Each row of a byte table is one unnamed byte (3.2.5.2, PDF 51).
Since M5b, for Get on byte tables (design 11 M5b): the fill byte where
nothing was written, with the extents written overlaid on it, which never
overlap one another (none is written before M6, whose Set on a byte table
keeps them so; until then every row of the simulator's byte tables reads as
the fill); and the most rows a Get may return as the one byte sequence of
its result within a token size, the host's MaxIndTokenSize ('bytesWithin').
-}
module Hsed.Sim.Bytes
  ( byteRange
  , bytesWithin
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified RIO.ByteString     as B
import           RIO

import           Hsed.Sim.Types     (ByteTable (..))


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.IntMap.Strict as IntMap

-- | The rows of a byte table from a start row to an end row, both
--   included, the lowest first (Core 5.3.3.6.2.1, PDF 201), each the byte
--   it holds: the byte of the extent written over it
--   ("Hsed.Sim.Types".@btWritten@, each extent by the offset of its first
--   row, no two overlapping), else the fill. The caller sees to it that the
--   start is no later than the end and the end within the table (5.1.4.2.3,
--   PDF 129-130: a cell_block's rows in bounds and in order, which
--   "Hsed.Core.CellBlock" checks before Get reads a row).
--
-- >>> byteRange 2 5 (ByteTable 8 0xFF mempty)
-- "\255\255\255\255"
-- >>> byteRange 2 5 (ByteTable 8 0xFF (IntMap.fromList [(0, "ab"), (3, "cd"), (6, "ef")]))
-- "\255cd\255"
-- >>> byteRange 1 6 (ByteTable 8 0 (IntMap.fromList [(0, "ab"), (3, "cd"), (6, "ef")]))
-- "b\NULcd\NULe"
byteRange :: Natural -> Natural -> ByteTable -> ByteString
byteRange s e bt = B.concat (pieces from overlaid)
  where
    from = fromIntegral s :: Int
    end  = fromIntegral e :: Int
    -- the part of each extent within the rows asked, by its first row, in row order
    overlaid = [ (max from o, B.take (min end lastRow - max from o + 1) (B.drop (max from o - o) bs))
               | (o, bs) <- IntMap.toAscList (btWritten bt), not (B.null bs)
               , let lastRow = o + B.length bs - 1, lastRow >= from, o <= end ]
    -- the fill up to each part, the part, and the fill after the last part
    pieces at []                 = [fill at (end + 1)]
    pieces at ((o, bs) : parts)  = fill at o : bs : pieces (o + B.length bs) parts
    fill at upTo                 = B.replicate (upTo - at) (btFill bt)

-- | The most bytes one byte sequence carries in a token of at most a size,
--   its header and its data counted (Core 5.2.2.4.1.5, PDF 146: what the
--   host's MaxIndTokenSize bounds, the TPer sending no larger token), its
--   header the least atom's that holds the length, as the codec writes it
--   ("Hsed.Core.Token"; Core 3.2.2.3.1, PDF 33): a short atom's one byte
--   for up to 15 bytes, a medium atom's two for up to 2047 and a long
--   atom's four for up to 16,777,215, the most one atom carries, the
--   simulator sending no continued token (5.2.2.4.1.8, PDF 146: the host's
--   ContinuedTokens TRUE alone allows one); 0 for a size of 1 or less. Get
--   bounds a byte table's rows by it ("Hsed.Sim.Base").
--
-- >>> map bytesWithin [0, 1, 16, 17, 18, 1992, 2049, 2051, 2052, 4040]
-- [0,0,15,15,16,1990,2047,2047,2048,4036]
-- >>> map bytesWithin [0xFFFFFF + 3, 0xFFFFFF + 4, maxBound]
-- [16777214,16777215,16777215]
bytesWithin :: Int -> Int
bytesWithin size = foldl' max 0 [ min most (size - header) | (header, least, most) <- forms, size - header >= least ]
  where
    -- a byte sequence's atoms as the codec chooses them by its length: the header's bytes, the least and the most
    -- bytes each carries
    forms = [(1, 0, 15), (2, 16, 2047), (4, 2048, 0xFFFFFF)] :: [(Int, Int, Int)]
