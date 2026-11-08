{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.ViewSpec
Description : Test specifications for Hsed.Sim.View: each query's answer the reader it replaces
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Since M5c the jig reads the simulator's state through typed queries alone
(design 9.2, 11 M5c, Option A: "Hsed.Jig.Script".@peekSim@), answered in
process by 'answer' and over the LAN leg by the hub's peek op. Each query's
answer, as the jig now reads it, equals what the reader it replaces read
of the state at M5b: each of those readers restated here as it stood at
be239b5, with its source line there, over the states the laws quantify
over ("Hsed.Sim.Gen".@genState@: 30 cases each, every unit, one to three
ComIDs, sessions open, transactions, the reset columns varied) and at both
of the Locking SP's Original Factory States ("Hsed.Sim.Gen".@foldedAt@):
the ComIDs' synchronous states and lives (@establish@'s SyncIdle and
NoSessions, SPF-02's extension), the sessions open (ETC-20, ETC-21), the
Locking SP's life cycle (LockingManufactured and
LockingManufacturedInactive), an authority's credential (CredentialsKnown,
@asAuthority@, ETC-21), a committed cell (SPF-11), the DataStore (SPF-02
case 2, ETC-14), the MSID (UCT-03) and the Locking table's ranges (SPF-12).
-}
module Hsed.Sim.ViewSpec (spec) where

import qualified RIO.List              as L
import qualified RIO.Map               as Map
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck       (Property, counterexample, forAllBlind)

import           Hsed.Core.Table       (Cell (..), TableDesc (..), columnOf)
import           Hsed.Core.UID         (UID)
import           Hsed.Gen.Core.TableDescs (authorityTableDesc, c_PINTableDesc, lockingTableDesc)
import           Hsed.Gen.Core.UIDs    (uAuthority_Admins, uAuthority_Anybody, uAuthority_SID, uLocking_GlobalRange, uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Script       (dataStoreTable)
import           Hsed.Sim              (syncOf)
import           Hsed.Sim.Gen          (foldedAt, genState)
import           Hsed.Sim.State        (cellNamedIn, cellOf, lifeOf, lockingRows, rowWithUID, spLifeCycle)
import           Hsed.Sim.Types        (ByteTable, ComIdLife (Associated), ComIdState (..), LifeCycle (..), SPState (..), SimParams (..),
                                        SyncState (..), TPerState (..))
import           Hsed.Sim.View         (ComIdView (..), Credential (..), SimQuery (..), answer)


-- * The readers of be239b5, restated

-- | @establish@'s SyncIdle, the ComIDs it drains (be239b5
--   hsed/src/Hsed/Jig/Run.hs:892).
busyAt :: TPerState -> [Word16]
busyAt st = [ comid | (comid, s) <- Map.toList (tsComIds st), cSync s /= S1 ]

-- | @establish@'s NoSessions, the ComIDs sessions hold, read before the
--   STACK_RESETs and again after them (be239b5 Run.hs:937, read at :904).
holdingAt :: TPerState -> [Word16]
holdingAt s = [ comid | comid <- Map.keys (tsComIds s), lifeOf comid s == Just Associated ]

-- | SPF-02's extension, the base ComID's synchronous state (be239b5
--   hsed/src/Hsed/Jig/Cases/SPF02.hs:131, "Hsed.Sim".'syncOf').
syncAt :: Word16 -> TPerState -> Maybe SyncState
syncAt = syncOf

-- | ETC-20's and ETC-21's sessions left open: whether any is, and how many
--   (be239b5 ETC20.hs:64-65, ETC21.hs:151-152).
sessionsAt :: TPerState -> (Bool, Int)
sessionsAt s = (Map.null (tsSessions s), Map.size (tsSessions s))

-- | LockingManufactured's and LockingManufacturedInactive's life cycle
--   (be239b5 Run.hs:908 and :912).
lifeCycleAt :: UID -> TPerState -> Maybe Int
lifeCycleAt = spLifeCycle

-- | An authority's credential (be239b5 hsed/src/Hsed/Jig/Script.hs:1495-1506,
--   @simCredential@, which CredentialsKnown at Run.hs:925, @asAuthority@
--   and ETC-21 read).
credentialAt :: UID -> UID -> TPerState -> Maybe Credential
credentialAt sp authority st = do
  authorityRow <- rowWithUID sp (tdTableUID authorityTableDesc) authority st
  RefCell _ credential <- cellNamed authorityTableDesc "Credential" authorityRow
  pinRow <- rowWithUID sp (tdTableUID c_PINTableDesc) credential st
  BytesCell pin <- cellNamed c_PINTableDesc "PIN" pinRow
  pure (Credential pin (count "TryLimit" pinRow) (count "Tries" pinRow))
  where
    cellNamed desc name row = columnOf desc name >>= (`cellOf` row)
    count name row = case cellNamed c_PINTableDesc name row of
      Just (UIntCell n) | n >= 0 -> fromInteger n
      _                          -> 0

-- | A committed cell by SP, table, row and column name (be239b5
--   Script.hs:1187-1191, @simCell@, SPF-11's User1 Enabled).
cellAt :: UID -> UID -> UID -> Text -> TPerState -> Maybe Cell
cellAt sp table row name st = do
  sps <- Map.lookup sp (tsSPs st)
  r   <- rowWithUID sp table row st
  cellNamedIn sps table name r

-- | The Locking SP's DataStore (be239b5 SPF02.hs:237, ETC14.hs:171).
dataStoreAt :: TPerState -> Maybe ByteTable
dataStoreAt st = Map.lookup uSP_Locking (tsSPs st) >>= Map.lookup dataStoreTable . spsBytes

-- | UCT-03's extension, the MSID the simulator was given (be239b5
--   UCT03.hs:199).
msidAt :: TPerState -> ByteString
msidAt st = spMsid (tsParams st)

-- | SPF-12's ranges, the Locking table's rows' UIDs (be239b5 SPF12.hs:316).
rangesAt :: TPerState -> [UID]
rangesAt st = [ u | r <- lockingRows st, Just (UIDCell u) <- [cellOf 0 r] ]


-- * Each query as the jig reads its answer

-- | The SPs, authorities and cells the jig's queries name, and some no
--   query of the jig names: SID, Anybody and Admins of the Admin SP;
--   Admin1, User1 and Anybody of the Locking SP; an SP the TPer lacks.
credentials :: [(UID, UID)]
credentials =
  [ (uSP_Admin, uAuthority_SID), (uSP_Admin, uAuthority_Anybody), (uSP_Admin, uAuthority_Admins)
  , (uSP_Locking, LockingSP.uAuthority_Admin1), (uSP_Locking, LockingSP.uAuthority_User1), (uSP_Locking, uAuthority_Anybody)
  , (uLocking_GlobalRange, uAuthority_SID) ]

-- | Cells by SP, table, row and column name: SPF-11's User1 Enabled, the
--   global range's ReadLocked, a column the description lacks, a row and
--   an SP the TPer lacks.
cells :: [(UID, UID, UID, Text)]
cells =
  [ (uSP_Locking, tdTableUID authorityTableDesc, LockingSP.uAuthority_User1, "Enabled")
  , (uSP_Locking, tdTableUID lockingTableDesc, uLocking_GlobalRange, "ReadLocked")
  , (uSP_Locking, tdTableUID authorityTableDesc, LockingSP.uAuthority_User1, "NoSuchColumn")
  , (uSP_Admin, tdTableUID authorityTableDesc, LockingSP.uAuthority_User1, "Enabled")
  , (uLocking_GlobalRange, tdTableUID authorityTableDesc, LockingSP.uAuthority_User1, "Enabled") ]

-- | Each query, named with the readers it replaces, and whether its answer
--   in a state, read as the jig reads it now, is what those readers read.
queries :: [(String, TPerState -> Bool)]
queries =
  [ ( "ComIds: establish's SyncIdle (Run.hs:892), its NoSessions (:937, :904) and SPF-02's extension (SPF02.hs:131)"
    , \st -> let comIds = answer ComIds st
             in [ cvComId v | v <- comIds, cvSync v /= S1 ] == busyAt st
                && [ cvComId v | v <- comIds, cvLife v == Just Associated ] == holdingAt st
                && and [ (cvSync <$> L.find ((== c) . cvComId) comIds) == syncAt c st | c <- 0x0FFF : Map.keys (tsComIds st) ] )
  , ( "Sessions: ETC-20's and ETC-21's sessions left open (ETC20.hs:64-65, ETC21.hs:151-152)"
    , \st -> let sessions = answer Sessions st in (null sessions, length sessions) == sessionsAt st )
  , ( "LifeCycleOf: LockingManufactured and LockingManufacturedInactive (Run.hs:908, :912)"
    , \st -> all (\sp -> answer (LifeCycleOf sp) st == lifeCycleAt sp st) [uSP_Locking, uSP_Admin, uLocking_GlobalRange] )
  , ( "CredentialOf: simCredential, read by CredentialsKnown, asAuthority and ETC-21 (Script.hs:1495-1506)"
    , \st -> all (\(sp, u) -> answer (CredentialOf sp u) st == credentialAt sp u st) credentials )
  , ( "CellOf: simCell, SPF-11's User1 Enabled (Script.hs:1187-1191)"
    , \st -> all (\(sp, t, r, n) -> answer (CellOf sp t r n) st == cellAt sp t r n st) cells )
  , ( "ByteTableOf: the DataStore of SPF-02 case 2 and ETC-14 (SPF02.hs:237, ETC14.hs:171)"
    , \st -> answer (ByteTableOf uSP_Locking dataStoreTable) st == dataStoreAt st )
  , ( "Msid: UCT-03's extension (UCT03.hs:199)"
    , \st -> answer Msid st == msidAt st )
  , ( "LockingRanges: SPF-12's ranges (SPF12.hs:316)"
    , \st -> answer LockingRanges st == rangesAt st )
  ]

-- | A state's parameters and sessions, for a counterexample: the whole state
--   is too long to print.
said :: TPerState -> String
said st = show (spUnits p, spNComIds p, spLockingOFS p, Map.keys (tsSessions st)) where p = tsParams st

-- | A query's readers over a generated state.
overGenerated :: (TPerState -> Bool) -> Property
overGenerated same = forAllBlind genState $ \st -> counterexample (said st) (same st)


spec :: Spec
spec = describe "Hsed.Sim.View.answer: each query, as the jig reads its answer, what the be239b5 reader it replaces read" $ do
  describe "over the states the laws quantify over (Hsed.Sim.Gen.genState)" $ modifyMaxSuccess (const 30) $
    for_ queries $ \(name, same) -> it name (overGenerated same)
  it "at both Original Factory States of the Locking SP (Hsed.Sim.Gen.foldedAt), every query" $
    [ (name, ofs) | ofs <- [ManufacturedInactive, Manufactured], (name, same) <- queries, not (same (foldedAt ofs)) ] `shouldBe` []
