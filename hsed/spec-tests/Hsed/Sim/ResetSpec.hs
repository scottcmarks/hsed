{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.ResetSpec
Description : Test specifications for Hsed.Sim.Reset: the interface events, TPER_RESET and STACK_RESET on the simulated TPer
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Milestone M4a's group G2 (design 8.4, 11): a power cycle, a hardware reset
and a hot plug (Opal 2.30 3.3.5.1 and Table 16, PDF 31-32), injected as
events through "Hsed.Sim".@step@, each abort every session, return every
ComID to Awaiting IF-SEND and empty its buffers; the host's properties are
the initial assumptions again after a power cycle and a hardware reset, and
kept after a hot plug (Core 5.2.2.4.4, PDF 148); C_PIN_SID's Tries is reset
by a power cycle alone, and only where Persistence is False (Core
5.3.4.1.1.2, PDF 210). TPER_RESET ("Hsed.Sim.Reset".@resetTPer@) resets the
host's properties and keeps Tries (Opal 3.2.3 g, PDF 29), and STACK_RESET
does one ComID's share, its management buffer kept (Core 3.3.4.7.5, PDF
65-66).
-}
module Hsed.Sim.ResetSpec (spec) where

import qualified RIO.Map                  as Map
import           RIO
import           Test.Hspec

import           Hsed.Core.Table          (Cell (..))
import           Hsed.Core.Types          (enumValue)
import           Hsed.Gen.Core.Enums      (Core_boolean (..))
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs  (uC_PINTable)
import           Hsed.Gen.Core.UIDs       (uAuthority_Anybody, uC_PIN_SID, uSP_Admin)
import           Hsed.OpalUIDs            (uSP_Locking)
import           Hsed.Sim                 (step)
import           Hsed.Sim.Reset           (ResetKind (..), resetTPer, stackReset)
import           Hsed.Sim.State           (cellOf, columnOf, openSession, rowWithUID, sessionsOn, updateRow)
import           Hsed.Sim.Types           (ComIdState (..), Input (..), LifeCycle (..), MgmtBuffer (..), Output (..),
                                           SPState (..), SimParams (..), SyncState (..), TPerState (..), defaultSimParams,
                                           initialHostProperties, initialState, maxPacketsName)
import           Hsed.Wire                (Event (..), IfStatus (..))


base :: Word16
base = spBaseComId defaultSimParams

-- | A TPer of two static ComIDs, busy on both: a session open on each (the
--   Locking SP Manufactured so that two SPs take one), responses staged, a
--   host property set on each, a management buffer filled, C_PIN_SID's Tries
--   2 with Persistence False.
busy :: TPerState
busy = withTries 2 st2 { tsComIds = Map.adjust onBase base (Map.adjust onNext (base + 1) (tsComIds st2)) }
  where
    st0 = initialState defaultSimParams { spNComIds = 2, spLockingOFS = Manufactured }
    (_, st1) = openSession base 1 uSP_Admin True [uAuthority_Anybody] st0
    (_, st2) = openSession (base + 1) 2 uSP_Locking True [uAuthority_Anybody] st1
    onBase cs = cs { cSync = S3 ["staged"], cHostProps = Map.insert maxPacketsName 4 (cHostProps cs)
                   , cMgmt = MgmtBuffer True (Just (2, "a stack reset response")) }
    onNext cs = cs { cSync = S2 ["prepared"], cHostProps = Map.insert maxPacketsName 2 (cHostProps cs)
                   , cMgmt = MgmtBuffer True Nothing }

-- | The TPer with C_PIN_SID's Tries set.
withTries :: Integer -> TPerState -> TPerState
withTries n = updateRow uSP_Admin uC_PINTable uC_PIN_SID (map (\(col, cell) -> (col, if Just col == columnOf c_PINTableDesc "Tries" then UIntCell n else cell)))

-- | The TPer with C_PIN_SID's Persistence True.
persistent :: TPerState -> TPerState
persistent = updateRow uSP_Admin uC_PINTable uC_PIN_SID
  (map (\(col, cell) -> (col, if Just col == columnOf c_PINTableDesc "Persistence" then EnumCell "True" (enumValue Core_boolean_True) else cell)))

triesOf :: TPerState -> Maybe Cell
triesOf st = rowWithUID uSP_Admin uC_PINTable uC_PIN_SID st >>= \row -> columnOf c_PINTableDesc "Tries" >>= (`cellOf` row)

-- | Every ComID's synchronous state, host properties and management buffer.
comIds :: TPerState -> [(SyncState, Map Text Natural, MgmtBuffer)]
comIds st = [ (cSync cs, cHostProps cs, cMgmt cs) | cs <- Map.elems (tsComIds st) ]

-- | The TPer after an event, Good.
afterEvent :: Event -> TPerState -> TPerState
afterEvent e st = case step (Ev e) st of
  (Evented Good, st') -> st'
  (other, _)          -> error ("the event was not Good: " <> show other)

spec :: Spec
spec = describe "the resets of the simulated TPer" $ do
  describe "every interface event (Opal 3.3.5.1), and TPER_RESET" $
    it "aborts every session, returns every ComID to Awaiting IF-SEND and empties its buffers, the parameters and the SPs' rows otherwise kept" $
      forM_ [afterEvent PowerCycle, afterEvent HardwareReset, afterEvent HotPlug, resetTPer OnTPerReset] $ \reset -> do
        let st' = reset busy
        tsSessions st' `shouldBe` mempty
        map (\(s, _, m) -> (s, m)) (comIds st') `shouldBe` replicate 2 (S1, MgmtBuffer False Nothing)
        tsParams st' `shouldBe` tsParams busy
        tsNextTSN st' `shouldBe` tsNextTSN busy
        Map.keys (tsSPs st') `shouldBe` Map.keys (tsSPs busy)

  describe "the host's properties (Core 5.2.2.4.4; Opal 3.2.3 g)" $ do
    it "are the initial assumptions again after a power cycle, a hardware reset and TPER_RESET" $
      forM_ [afterEvent PowerCycle, afterEvent HardwareReset, resetTPer OnTPerReset] $ \reset ->
        map (\(_, h, _) -> h) (comIds (reset busy)) `shouldBe` replicate 2 initialHostProperties
    it "are kept after a hot plug, another TCG reset event" $
      map (\(_, h, _) -> Map.lookup maxPacketsName h) (comIds (afterEvent HotPlug busy)) `shouldBe` [Just 4, Just 2]

  describe "C_PIN_SID's Tries (Core 5.3.4.1.1.2)" $ do
    it "is reset to 0 by a power cycle where Persistence is False" $
      triesOf (afterEvent PowerCycle busy) `shouldBe` Just (UIntCell 0)
    it "is kept by a power cycle where Persistence is True" $
      triesOf (afterEvent PowerCycle (persistent busy)) `shouldBe` Just (UIntCell 2)
    it "is kept by a hardware reset, a hot plug and TPER_RESET" $
      forM_ [afterEvent HardwareReset, afterEvent HotPlug, resetTPer OnTPerReset] $ \reset ->
        triesOf (reset busy) `shouldBe` Just (UIntCell 2)
    it "a row with no Tries cell, C_PIN_MSID's, is as it was: the table after a power cycle is the table with Tries 0" $
      (Map.lookup uC_PINTable . spsTables =<< Map.lookup uSP_Admin (tsSPs (afterEvent PowerCycle busy)))
        `shouldBe` (Map.lookup uC_PINTable . spsTables =<< Map.lookup uSP_Admin (tsSPs (withTries 0 busy)))

  describe "STACK_RESET on a ComID (Core 3.3.4.7.5 items 1-8)" $ do
    let st' = stackReset base busy
    it "aborts that ComID's sessions, returns it to S1 with the host's initial properties, and keeps its management buffer" $ do
      sessionsOn base st' `shouldBe` []
      fmap (\cs -> (cSync cs, cHostProps cs, cMgmt cs)) (Map.lookup base (tsComIds st'))
        `shouldBe` Just (S1, initialHostProperties, MgmtBuffer True (Just (2, "a stack reset response")))
    it "leaves the other ComID, its sessions and Tries as they were" $ do
      length (sessionsOn (base + 1) st') `shouldBe` 1
      Map.lookup (base + 1) (tsComIds st') `shouldBe` Map.lookup (base + 1) (tsComIds busy)
      triesOf st' `shouldBe` Just (UIntCell 2)
    it "on a ComID the TPer lacks changes nothing" $
      stackReset 0x2000 busy `shouldBe` busy
