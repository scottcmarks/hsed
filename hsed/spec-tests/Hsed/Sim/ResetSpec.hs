{-# LANGUAGE LambdaCase        #-}
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

M5a's group G4: every reset of the TPer applies the Locking SP's reset
columns, at either life of the SP (the plan's D3). A Locking row whose
LockOnReset lists the reset's type (Opal Table 16, PDF 32: Power Cycle 0,
Hardware 1, HotPlug 2, Programmatic 3, TPER_RESET's) is ReadLocked and
WriteLocked True after it, whatever its ReadLockEnabled and
WriteLockEnabled, and an empty set changes neither (Core 5.7.2.2.10, PDF
275; Opal 3.2.3 h, PDF 29); MBRControl's Done is False where its
MBRDoneOnReset lists the type (Core 5.7.2.5.4, PDF 278; Opal 3.2.3 i);
STACK_RESET changes neither table; a power cycle resets the Locking SP's
C_PIN Tries too. And M5a's reset clause on the wire, at
@--locking-ofs manufactured@: a session to the Locking SP as Admin1 (its PIN
the MSID, Opal Table 41 note 1, PDF 83-84) reads Locking_GlobalRange's
ReadLocked and WriteLocked False, a power cycle is injected, and a new
session reads them True, its LockOnReset {Power Cycle} (Opal Table 45, PDF
86): what SPF-12 case 1 does with TPER_RESET (Test Cases PDF 65-66), the
jig's own commands through a transport over "Hsed.Sim".@step@.
-}
module Hsed.Sim.ResetSpec (spec) where

import qualified RIO.ByteString           as B
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO
import           Test.Hspec

import           Hsed.Core.CellBlock      (columnRange)
import           Hsed.Core.Table          (Cell (..), Row)
import           Hsed.Core.Types          (enumValue)
import           Hsed.Core.UID            (UID)
import           Hsed.Core.Value          (List (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes      (StatusCode (..))
import           Hsed.Gen.Core.Enums      (Core_boolean (..))
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs  (uC_PINTable, uLockingTable, uMBRControlTable)
import           Hsed.Gen.Core.UIDs       (uAuthority_Anybody, uC_PIN_SID, uLocking_GlobalRange, uMBRControl_Row, uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Options         (defaultRunOptions)
import           Hsed.Jig.Script          (MethodOutcome (..), RunCtx (..), RunEnv (..), StartArgs (..), SyncAnswer (..),
                                           emptyRunCtx, endSession, getCells, startSession)
import           Hsed.Jig.Target          (Target (..))
import           Hsed.Jig.Transcript      (newTranscript)
import           Hsed.Sim                 (step, syncOf)
import           Hsed.Sim.Reset           (ResetKind (..), resetTPer, resetType, stackReset)
import           Hsed.Sim.State           (cellOf, columnOf, descOf, openSession, rowWithUID, sessionsOn, tableRows, updateRow)
import           Hsed.Sim.Types           (ComIdState (..), Input (..), LifeCycle (..), MgmtBuffer (..), Output (..),
                                           SPState (..), SimParams (..), SyncState (..), TPerState (..), defaultSimParams,
                                           initialHostProperties, initialState, maxPacketsName)
import           Hsed.Transport.InProc    (SimControl (..))
import           Hsed.Wire                (Event (..), IfStatus (..), newChannel)
import           Hsed.Wire.Internal       (Describe (..), Dir (..), Request (..), Response (..), Transport (..), Unit (..))


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

-- | A state with cells of a row of an SP's table put, each column by name
--   in the SP's description of the table.
seeded :: UID -> UID -> UID -> [(Text, Cell)] -> TPerState -> TPerState
seeded sp table row cells st = updateRow sp table row put st
  where
    put r = L.foldl' (\r' (name, cell) -> maybe r' (\c -> L.sortOn fst ((c, cell) : filter ((/= c) . fst) r')) (column name)) r cells
    column name = descOf sp table st >>= (`columnOf` name)

-- | A row's cell in a column of a name, by the SP's description.
named :: UID -> UID -> Text -> TPerState -> Row -> Maybe Cell
named sp table name st r = descOf sp table st >>= (`columnOf` name) >>= (`cellOf` r)

-- | A row's cell, by the row's UID.
cellAt :: UID -> UID -> UID -> Text -> TPerState -> Maybe Cell
cellAt sp table row name st = rowWithUID sp table row st >>= named sp table name st

-- | Boolean cells (Core Table 59, PDF 103).
true, false :: Cell
true  = EnumCell "True" (enumValue Core_boolean_True)
false = EnumCell "False" (enumValue Core_boolean_False)

-- | Whether a cell holds True.
isTrue :: Maybe Cell -> Bool
isTrue = \case
  Just (EnumCell _ v) -> v == enumValue Core_boolean_True
  _                   -> False

-- | A reset_types set of types (Opal Table 16, PDF 32).
resetSet :: [Int] -> Cell
resetSet = ListCell . map (\v -> EnumCell (fromMaybe "?" (L.lookup v [(0, "Power Cycle"), (1, "Hardware"), (2, "HotPlug"), (3, "Programmatic")])) v)

-- | The Locking SP's lock columns of Locking_GlobalRange: ReadLocked and
--   WriteLocked, as True or not.
globalLocks :: TPerState -> (Bool, Bool)
globalLocks st = (isTrue (cellAt uSP_Locking uLockingTable uLocking_GlobalRange "ReadLocked" st)
                 , isTrue (cellAt uSP_Locking uLockingTable uLocking_GlobalRange "WriteLocked" st))

-- | MBRControl's Done, as True or not.
mbrDone :: TPerState -> Bool
mbrDone = isTrue . cellAt uSP_Locking uMBRControlTable uMBRControl_Row "Done"

-- | Every reset of the TPer, by kind: the events through the step, TPER_RESET by the function.
everyReset :: [(ResetKind, TPerState -> TPerState)]
everyReset =
  [ (OnPowerCycle, afterEvent PowerCycle), (OnHardwareReset, afterEvent HardwareReset), (OnHotPlug, afterEvent HotPlug)
  , (OnTPerReset, resetTPer OnTPerReset) ]

-- | A TPer of the default parameters at a Locking SP life, Locking_GlobalRange
--   neither lock enabled nor locked and its LockOnReset a set, MBRControl
--   Done True and its MBRDoneOnReset a set.
armed :: LifeCycle -> [Int] -> [Int] -> TPerState
armed life lor dor =
    seeded uSP_Locking uMBRControlTable uMBRControl_Row [("Done", true), ("MBRDoneOnReset", resetSet dor)]
  . seeded uSP_Locking uLockingTable uLocking_GlobalRange
      [("ReadLockEnabled", false), ("WriteLockEnabled", false), ("ReadLocked", false), ("WriteLocked", false), ("LockOnReset", resetSet lor)]
  $ initialState defaultSimParams { spLockingOFS = life }

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

  describe "LockOnReset (Core 5.7.2.2.10, PDF 275; Opal 3.2.3 h, PDF 29)" $ do
    it "locks Locking_GlobalRange on each reset its set lists, though neither lock is enabled, at either life (D3)" $
      forM_ [ManufacturedInactive, Manufactured] $ \life ->
        forM_ everyReset $ \(kind, reset) -> do
          globalLocks (reset (armed life [fromIntegral (resetType kind)] [])) `shouldBe` (True, True)
          globalLocks (reset (armed life [0, 3] [])) `shouldBe` (kind `elem` [OnPowerCycle, OnTPerReset], kind `elem` [OnPowerCycle, OnTPerReset])
    it "changes neither lock where the set is empty" $
      forM_ everyReset $ \(_, reset) -> globalLocks (reset (armed Manufactured [] [])) `shouldBe` (False, False)
    it "leaves every other cell of the row, and a row whose set does not list the type, as they were" $ do
      let st    = seeded uSP_Locking uLockingTable LockingSP.uLocking_Range1 [("LockOnReset", resetSet [1])] (armed Manufactured [0] [])
          cycled = afterEvent PowerCycle st
          other = [ (c, cell) | (c, cell) <- fromMaybe [] (rowWithUID uSP_Locking uLockingTable uLocking_GlobalRange cycled), c `notElem` [7, 8] ]
      other `shouldBe` [ (c, cell) | (c, cell) <- fromMaybe [] (rowWithUID uSP_Locking uLockingTable uLocking_GlobalRange st), c `notElem` [7, 8] ]
      rowWithUID uSP_Locking uLockingTable LockingSP.uLocking_Range1 cycled `shouldBe` rowWithUID uSP_Locking uLockingTable LockingSP.uLocking_Range1 st
    it "is not applied by STACK_RESET (Core 3.3.4.7.5 items 1-8, PDF 65-66)" $ do
      let st = armed Manufactured [0, 1, 2, 3] [0, 1, 2, 3]
      tableRows uSP_Locking uLockingTable (stackReset base st) `shouldBe` tableRows uSP_Locking uLockingTable st
      tableRows uSP_Locking uMBRControlTable (stackReset base st) `shouldBe` tableRows uSP_Locking uMBRControlTable st

  describe "MBRDoneOnReset (Core 5.7.2.5.4, PDF 278; Opal 3.2.3 i, PDF 29)" $ do
    it "clears Done on each reset its set lists, at either life, and keeps it where the set does not list it" $
      forM_ [ManufacturedInactive, Manufactured] $ \life ->
        forM_ everyReset $ \(kind, reset) -> do
          mbrDone (reset (armed life [] [fromIntegral (resetType kind)])) `shouldBe` False
          mbrDone (reset (armed life [] [ t | t <- [0 .. 3], t /= fromIntegral (resetType kind) ])) `shouldBe` True
          mbrDone (reset (armed life [] [])) `shouldBe` True
    it "keeps Enable" $
      cellAt uSP_Locking uMBRControlTable uMBRControl_Row "Enable" (afterEvent PowerCycle (armed Manufactured [] [0]))
        `shouldBe` cellAt uSP_Locking uMBRControlTable uMBRControl_Row "Enable" (armed Manufactured [] [0])

  describe "the Locking SP's C_PIN Tries (Core 5.3.4.1.1.2, PDF 210)" $
    it "is reset by a power cycle where Persistence is False, and kept by the other resets" $ do
      let st = seeded uSP_Locking uC_PINTable LockingSP.uC_PIN_Admin1 [("Tries", UIntCell 3), ("Persistence", false)]
                 (initialState defaultSimParams { spLockingOFS = Manufactured })
          admin1Tries = cellAt uSP_Locking uC_PINTable LockingSP.uC_PIN_Admin1 "Tries"
      admin1Tries (afterEvent PowerCycle st) `shouldBe` Just (UIntCell 0)
      forM_ (drop 1 everyReset) $ \(_, reset) -> admin1Tries (reset st) `shouldBe` Just (UIntCell 3)

  describe "the reset clause on the wire, at --locking-ofs manufactured (M5a acceptance item 8)" $
    it "reads Locking_GlobalRange's ReadLocked and WriteLocked False as Admin1, then after a power cycle True, its LockOnReset {Power Cycle}" $ do
      let st0 = seeded uSP_Locking uLockingTable uLocking_GlobalRange [("LockOnReset", resetSet [0])]
                  (initialState defaultSimParams { spLockingOFS = Manufactured })
      ref <- newIORef st0
      unlocked <- readLocks ref
      cycled   <- tInject (stepped ref) PowerCycle
      locked   <- readLocks ref
      (unlocked, cycled, locked) `shouldBe` (Right (locks False), Good, Right (locks True))
      globalLocks <$> readIORef ref `shouldReturn` (True, True)

-- | Get's results of ReadLocked (column 7) and WriteLocked (8), both of a
--   value (Core 5.3.3.6, PDF 201; a boolean as 0 or 1, 5.1.2, PDF 99), and
--   SUCCESS.
locks :: Bool -> MethodOutcome
locks b = MethodOutcome [L (List [N ((7 :: Natural) .: v), N ((8 :: Natural) .: v)])] (StatusCode 0)
  where v = if b then 1 else 0 :: Natural

-- | A session to the Locking SP as Admin1, its PIN the MSID, through the
--   jig's own commands over a transport on the state: Get on
--   Locking_GlobalRange's ReadLocked and WriteLocked, then End of Session;
--   the Get's outcome, or what went otherwise.
readLocks :: IORef TPerState -> IO (Either Text MethodOutcome)
readLocks ref = do
  transcript <- newTranscript
  channel    <- newChannel
  ctx        <- newIORef emptyRunCtx { rcBaseComId = Just base }
  stop       <- newIORef Nothing
  let target = SimTarget (stepped ref) (SimControl (\_ -> pure ()) (readIORef ref))
      env    = RunEnv target transcript channel ctx defaultRunOptions stop (mkLogFunc (\_ _ _ _ -> pure ()))
  runRIO env $ startSession (StartArgs uSP_Locking (Just LockingSP.uAuthority_Admin1) (Just (spMsid defaultSimParams)) True) >>= \case
    Right (Synced h) -> do
      got <- getCells h uLocking_GlobalRange (columnRange 7 8)
      _   <- endSession h
      pure (first tshow got)
    other -> pure (Left (tshow other))

-- | A transport over a state through "Hsed.Sim".@step@, as the in-process
--   one with no hold: an IF-SEND that leaves its ComID processing has its
--   response released at once ('Tick'); an event resets the TPer.
stepped :: IORef TPerState -> Transport c
stepped ref = MkTransport
  { tExchange = \r -> atomicModifyIORef' ref (exchange r)
  , tInject   = \e -> atomicModifyIORef' ref (\st -> case step (Ev e) st of
                                                  (Evented s, st') -> (st', s)
                                                  (_, st')         -> (st', TransportFailure "no status"))
  , tUnit     = UnitBytes
  , tDescribe = Describe "tper" "hsed-sim, stepped" UnitBytes (Just "opal-2.30") (Just base) (Just 1) Nothing
  , tClose    = pure ()
  }
  where
    exchange r st = case step (Cmd r) st of
      (Out resp, st') -> (released r st', resp)
      (_, st')        -> (st', Response (TransportFailure "no response") B.empty)
    released r st = case (dir r, syncOf (comId r) st) of
      (IfSend, Just (S2 _)) -> snd (step (Tick (comId r)) st)
      _                     -> st
