{-# LANGUAGE NoImplicitPrelude #-}

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
Since M5b, for Get on byte tables (design 11 M5b, group G1's).
-}
module Hsed.Sim.Bytes
  ( byteRange
  ) where

import qualified RIO.ByteString as B
import           RIO

import           Hsed.Sim.Types (ByteTable (..))


-- | The rows of a byte table from a start row to an end row, both
--   included, the lowest first (Core 5.3.3.6.2.1, PDF 201), each the byte
--   it holds; the caller sees to it that the start is no later than the end
--   and the end within the table (5.1.4.2.3, PDF 129-130: a cell_block's
--   rows in bounds and in order, which "Hsed.Core.CellBlock" checks before
--   Get reads a row). The fill byte where nothing was written; the extents
--   written overlaid on it are group G1's (M5b), every row reading as the
--   fill until then, as every row of the simulator's byte tables does, no
--   Set on a byte table being the simulator's before M6.
--
-- >>> byteRange 2 5 (ByteTable 8 0xFF mempty)
-- "\255\255\255\255"
byteRange :: Natural -> Natural -> ByteTable -> ByteString
byteRange s e bt = B.replicate (fromIntegral (e - s + 1)) (btFill bt)
