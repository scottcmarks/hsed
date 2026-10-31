{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.AdminSPSpec
Description : Test specifications for Hsed.Sim.AdminSP: the fourteen hand-written rows of the Admin SP
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Admin SP's rows the simulator holds until M4b (design 8.3): fourteen, in
six tables, every cell's column a column of the generated description; the
C_PIN rows filled from the parameters (the MSID, the SID's PIN and
TryLimit); the two ACEs' columns the C_PIN table's by number; the SP rows'
life cycles, the Locking SP's the parameters'; Anybody's Operation None, as
Opal 2.30 Table 24 (PDF 48) prints it.
-}
module Hsed.Sim.AdminSPSpec (spec) where

import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import           RIO
import           Test.Hspec

import           Hsed.Core.Table         (Cell (..), Col (..), Row, TableDesc (..), columnNamed)
import           Hsed.Core.Types         (enumValue)
import           Hsed.Core.UID           (UID)
import           Hsed.Gen.Core.Enums     (Core_auth_method (..))
import           Hsed.Gen.Core.TableDescs (aceTableDesc, accessControlTableDesc, authorityTableDesc, c_PINTableDesc,
                                          methodIDTableDesc, spTableDesc)
import           Hsed.Gen.Core.UIDs      (uAuthority_Anybody, uC_PIN_SID, uSP_Admin)
import           Hsed.OpalUIDs           (uACE_C_PIN_MSID_Get_PIN, uACE_C_PIN_SID_Get_NOPIN, uC_PIN_MSID, uSP_Locking)
import           Hsed.Sim.AdminSP
import           Hsed.Sim.Types          (LifeCycle (..), SimParams (..), defaultSimParams)


-- | The rows of a table, by its description.
rowsOf :: SimParams -> TableDesc -> [Row]
rowsOf p desc = fromMaybe [] (Map.lookup (tdTableUID desc) (adminSPTables p))

-- | A table's row whose UID column holds a UID.
rowWith :: SimParams -> TableDesc -> UID -> Maybe Row
rowWith p desc u = L.find ((== Just (UIDCell u)) . L.lookup 0) (rowsOf p desc)

-- | A row's cell in a column named in its description.
cellNamed :: TableDesc -> Text -> Row -> Maybe Cell
cellNamed desc name r = columnNamed name desc >>= \col -> L.lookup (colNumber col) r

-- | The numbers of the C_PIN table's columns named.
c_PINNumbers :: [Text] -> [Integer]
c_PINNumbers names = [ toInteger (colNumber col) | name <- names, Just col <- [columnNamed name c_PINTableDesc] ]

spec :: Spec
spec = describe "the Admin SP's hand-written rows" $ do
  it "are fourteen, in six tables: MethodID 1, AccessControl 2, ACE 2, Authority 4, C_PIN 3, SP 2" $ do
    length (handRows defaultSimParams) `shouldBe` 14
    map (length . rowsOf defaultSimParams) [methodIDTableDesc, accessControlTableDesc, aceTableDesc, authorityTableDesc, c_PINTableDesc, spTableDesc]
      `shouldBe` [1, 2, 2, 4, 3, 2]
    Map.size (adminSPTables defaultSimParams) `shouldBe` 6
  it "name a column of the generated description in every cell, each placed once, in column order" $ do
    [ (tdName desc, name) | (desc, cells) <- handRows defaultSimParams, (name, _) <- cells, isNothing (columnNamed name desc) ] `shouldBe` []
    forM_ (concat (Map.elems (adminSPTables defaultSimParams))) $ \r -> do
      map fst r `shouldBe` L.sort (map fst r)
      L.nub (map fst r) `shouldBe` map fst r
    L.sort [ length r | r <- concat (Map.elems (adminSPTables defaultSimParams)) ] `shouldBe` L.sort [ length cells | (_, cells) <- handRows defaultSimParams ]
  it "give every object its UID in column 0x00, distinct within its table; the AccessControl rows none, as Opal prints none" $ do
    forM_ [methodIDTableDesc, aceTableDesc, authorityTableDesc, c_PINTableDesc, spTableDesc] $ \desc -> do
      let uids = [ u | r <- rowsOf defaultSimParams desc, Just (UIDCell u) <- [L.lookup 0 r] ]
      length uids `shouldBe` length (rowsOf defaultSimParams desc)
      L.nub uids `shouldBe` uids
    [ L.lookup 0 r | r <- rowsOf defaultSimParams accessControlTableDesc ] `shouldBe` [Nothing, Nothing]
  it "fill C_PIN_MSID's PIN with the MSID, and C_PIN_SID's with the MSID by default or the SID PIN given (Opal 2.30 Table 25, 4.2.1.8)" $ do
    (rowWith defaultSimParams c_PINTableDesc uC_PIN_MSID >>= cellNamed c_PINTableDesc "PIN") `shouldBe` Just (BytesCell (spMsid defaultSimParams))
    (rowWith defaultSimParams c_PINTableDesc uC_PIN_SID >>= cellNamed c_PINTableDesc "PIN") `shouldBe` Just (BytesCell (spMsid defaultSimParams))
    let own = defaultSimParams { spSidPin = Just "a SID PIN of its own", spSidTryLimit = 3 }
    (rowWith own c_PINTableDesc uC_PIN_SID >>= cellNamed c_PINTableDesc "PIN") `shouldBe` Just (BytesCell "a SID PIN of its own")
    (rowWith own c_PINTableDesc uC_PIN_SID >>= cellNamed c_PINTableDesc "TryLimit") `shouldBe` Just (UIntCell 3)
    (rowWith own c_PINTableDesc uC_PIN_SID >>= cellNamed c_PINTableDesc "Tries") `shouldBe` Just (UIntCell 0)
    (rowWith own c_PINTableDesc uC_PIN_MSID >>= cellNamed c_PINTableDesc "PIN") `shouldBe` Just (BytesCell (spMsid own))
  it "give the ACEs their columns as the C_PIN table numbers them (Opal 2.30 Table 23): the NOPIN ACE every column but the PIN, the MSID ACE the UID and the PIN" $ do
    (rowWith defaultSimParams aceTableDesc uACE_C_PIN_SID_Get_NOPIN >>= cellNamed aceTableDesc "Columns")
      `shouldBe` Just (ListCell (map UIntCell (c_PINNumbers ["UID", "CharSet", "TryLimit", "Tries", "Persistence"])))
    (rowWith defaultSimParams aceTableDesc uACE_C_PIN_MSID_Get_PIN >>= cellNamed aceTableDesc "Columns")
      `shouldBe` Just (ListCell (map UIntCell (c_PINNumbers ["UID", "PIN"])))
    c_PINNumbers ["UID", "PIN", "CharSet", "TryLimit", "Tries", "Persistence"] `shouldBe` [0, 3, 4, 5, 6, 7]
  it "give the SP rows their life cycles: the Admin SP Manufactured, the Locking SP the parameters' Original Factory State (Opal 2.30 Table 29)" $ do
    let lifeOf p u = rowWith p spTableDesc u >>= cellNamed spTableDesc "LifeCycleState"
        label c = case c of
          Just (EnumCell l _) -> Just l
          _                   -> Nothing
    label (lifeOf defaultSimParams uSP_Admin) `shouldBe` Just "Manufactured"
    label (lifeOf defaultSimParams uSP_Locking) `shouldBe` Just "Manufactured-Inactive"
    label (lifeOf defaultSimParams { spLockingOFS = Manufactured } uSP_Locking) `shouldBe` Just "Manufactured"
  it "give Anybody the Operation None of Opal 2.30 Table 24, not Core Table 209's Sign" $
    (rowWith defaultSimParams authorityTableDesc uAuthority_Anybody >>= cellNamed authorityTableDesc "Operation")
      `shouldBe` Just (EnumCell "None" (enumValue Core_auth_method_None))
