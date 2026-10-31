{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.PreconfigSpec
Description : Test specifications for Hsed.Sim.Preconfig: an SP's tables folded from its preconfiguration tables
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The fold of M4b's scaffold (design 8.1 and 8.3): each table's description by
the table's UID, and its rows in the order the tables give them, each the
cells its table prints over every other column's default, in column order (a
column a row lacks is one a Get does not return, Core 5.3.4.2.2, PDF 223;
design 13.31 (a)); and the Admin SP's tables the hand-written stand-in until
M4b's generated ones.
-}
module Hsed.Sim.PreconfigSpec (spec) where

import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import           RIO
import           Test.Hspec

import           Hsed.Core.Preconfig     (PreCell (..), PreconfRow (..), PreconfTable (..), RowKind (..))
import           Hsed.Core.Table         (Cell (..), Pre (..), Row, TableDesc (..))
import           Hsed.Core.UID           (UID, uNull, uid)
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc, spTableDesc)
import           Hsed.Sim.AdminSP        (handTables)
import           Hsed.Sim.Preconfig      (adminSPTables, foldTables)
import           Hsed.Sim.Types          (defaultSimParams)


-- | A plain row of printed cells, with no access.
plain :: [(Int, Cell)] -> Pre PreconfRow
plain cells = Pre (PreconfRow Plain False [ (c, Given cell) | (c, cell) <- cells ] []) []

-- | A C_PIN table whose CharSet, TryLimit, Tries and Persistence (columns 4
--   to 7) have defaults, the zero values of their types (Core 5.3.4.1.1.1-2,
--   PDF 210; the boolean False), and two rows that give some of them.
c_PIN :: PreconfTable
c_PIN = PreconfTable c_PINTableDesc defaults
  [ plain [(0, UIDCell msid), (1, TextCell "C_PIN_MSID"), (3, BytesCell "an MSID")]
  , plain [(5, UIntCell 3), (0, UIDCell sid), (1, TextCell "C_PIN_SID")] ]
  where defaults = [(4, RefCell "Null" uNull), (5, UIntCell 0), (6, UIntCell 0), (7, EnumCell "False" 0)]

msid, sid :: UID
msid = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x84 0x02
sid  = uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x00 0x01

-- | An SP table with no defaults and one row.
sp :: PreconfTable
sp = PreconfTable spTableDesc [] [plain [(0, UIDCell (uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x01)), (1, TextCell "Admin")]]

-- | The rows a fold gives a table, by its description.
rowsIn :: [PreconfTable] -> TableDesc -> [Row]
rowsIn tables desc = fromMaybe [] (Map.lookup (tdTableUID desc) (snd (foldTables defaultSimParams tables)))

spec :: Spec
spec = describe "an SP's tables folded from its preconfiguration tables" $ do
  it "give each row the cells its table prints and every other column's default, in column order" $
    rowsIn [c_PIN] c_PINTableDesc `shouldBe`
      [ [ (0, UIDCell msid), (1, TextCell "C_PIN_MSID"), (3, BytesCell "an MSID")
        , (4, RefCell "Null" uNull), (5, UIntCell 0), (6, UIntCell 0), (7, EnumCell "False" 0) ]
      , [ (0, UIDCell sid), (1, TextCell "C_PIN_SID")
        , (4, RefCell "Null" uNull), (5, UIntCell 3), (6, UIntCell 0), (7, EnumCell "False" 0) ] ]
  it "hold each table's description and its rows by the table's UID, the rows in the table's order" $ do
    let (descs, tables) = foldTables defaultSimParams [c_PIN, sp]
    descs `shouldBe` Map.fromList [(tdTableUID c_PINTableDesc, c_PINTableDesc), (tdTableUID spTableDesc, spTableDesc)]
    Map.keys tables `shouldBe` Map.keys descs
    map (L.lookup 0) (rowsIn [c_PIN, sp] c_PINTableDesc) `shouldBe` [Just (UIDCell msid), Just (UIDCell sid)]
    rowsIn [c_PIN, sp] spTableDesc `shouldBe` [[(0, UIDCell (uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x01)), (1, TextCell "Admin")]]
  it "of the Admin SP are, until M4b's generated ones, the hand-written stand-in (Hsed.Sim.AdminSP)" $
    adminSPTables defaultSimParams `shouldBe` handTables defaultSimParams
