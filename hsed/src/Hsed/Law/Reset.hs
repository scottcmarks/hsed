{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Law.Reset
Description : The resets' laws (Opal 2.30 3.2.3, 3.3.5.1 and 4.3.5; Core 5.2.2.4.4 and 5.3.4.1.1.2): what a reset leaves of the Locking SP's tables, the host's properties, Tries and the ComIDs
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The laws of the simulator's resets ("Hsed.Sim.Reset"), as "Hsed.Law.Sync"
has the synchronous protocol's: each names the paragraph it implements by
the first 12 hex digits of its IR hash (@spec/ir/core-2.01.json@ and
@spec/ir/opal-2.30.json@, the sha256 of the paragraph's normalised words)
and the Test Cases' cases that observe it, and says what it holds as a
QuickCheck property over the simulator: over the states of
"Hsed.Sim.Gen".'genState', whose Locking and MBRControl rows, LockOnReset
and MBRDoneOnReset sets and C_PIN Tries and Persistence are varied, at
either life of the Locking SP (its reset columns applied only while it is
Manufactured, Opal 2.30 5.2.2.3.1, PDF 97, since the M5a review), and
over every reset as the TPer meets it: a power cycle, a hardware reset and
a hot plug as events ("Hsed.Sim".@step@), TPER_RESET as the IF-SEND of
Opal's Table 13 on ComID 0x0004 with ProgrammaticResetEnable True (Opal
3.2.3, PDF 28-29), and STACK_RESET on a ComID.

No comment binds them yet: each paragraph is unlabelled prose, which no
anchor names (design 13.29 (j), the per-clause anchoring; the plan's D7:
write and cite now, anchor later), so "Hsed.Gen.Core.Laws" lists none of
them and @LawsSpec@ holds them by name and hash to the paragraphs cited
here.

Each law states what it expects in the spec's own terms, not through the
simulator's: a reset's type is Opal Table 16's value written here
('printedType'), not "Hsed.Sim.Reset"'s @resetType@ or @resetListed@, so
that a wrong mapping there fails the laws (since the M5a review).
-}
module Hsed.Law.Reset
  ( lockOnReset
  , mbrDoneOnReset
  , properties
  , tries
  , comIdIssued
  , laws
    -- * The resets the laws quantify over
  , Reset (..)
  , resets
  , afterReset
  ) where

import qualified RIO.ByteString           as B
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO
import           Test.QuickCheck          (Property, counterexample, elements, forAll, (===))

import           Hsed.Core.Table          (Cell (..), Row, columnOf)
import           Hsed.Core.UID            (UID)
import           Hsed.Gen.Core.TableUIDs  (uC_PINTable, uLockingTable, uMBRControlTable, uTPerInfoTable)
import           Hsed.Gen.Core.UIDs       (uSP_Admin)
import           Hsed.Jig.Catalogue       (CaseKey (..), TestId (..))
import           Hsed.Law                 (Law (..))
import           Hsed.Sim                 (step)
import           Hsed.Sim.Gen             (genState)
import           Hsed.Sim.Cells           (booleanCell, putCell)
import           Hsed.Sim.Reset           (ResetKind (..), stackReset)
import           Hsed.Sim.State           (booleanNamedIn, cellNamedIn, lifeOf, spLifeCycle)
import           Hsed.Sim.Types           (ComIdLife (..), ComIdState (..), Input (..), Output (..), SPState (..),
                                           SyncState (..), TPerState (..), initialHostProperties)
import           Hsed.Wire                (Dir (..), Event (..), IfStatus (..), Request (..), Response (..), comIdManagementProtocol,
                                           tperResetComId)


-- * The resets

-- | A reset as the TPer meets it: one of the TPer's, of a kind, or
--   STACK_RESET on a ComID.
data Reset
  = Whole !ResetKind
  | Stack !Word16
  deriving (Eq, Show)

-- | Every reset of a state: the TPer's four kinds and STACK_RESET on each of
--   its static ComIDs.
resets :: TPerState -> [Reset]
resets st = map Whole [minBound .. maxBound] <> map Stack (Map.keys (tsComIds st))

-- | A state before a reset, as the reset meets it, and after it: an
--   interface event through "Hsed.Sim".@step@ (Opal 2.30 3.3.5.1, PDF
--   31-32); TPER_RESET as a 512-byte IF-SEND to ComID 0x0004 under protocol
--   0x02 (Opal 3.2.3 and Table 13, PDF 28-29), on the state with
--   ProgrammaticResetEnable True (Table 26, PDF 49), the command otherwise
--   aborted; STACK_RESET on a ComID ("Hsed.Sim.Reset".'stackReset', Core
--   3.3.4.7.5, PDF 65-66). The state before is the one the reset meets, the
--   TPER_RESET's with the command enabled. A step that does not answer as a
--   reset does is an error naming it.
afterReset :: Reset -> TPerState -> (TPerState, TPerState)
afterReset reset st = case reset of
  Whole OnPowerCycle    -> evented PowerCycle
  Whole OnHardwareReset -> evented HardwareReset
  Whole OnHotPlug       -> evented HotPlug
  Whole OnTPerReset     ->
    let enabled = tperResetEnabled st
    in case step (Cmd (Request IfSend comIdManagementProtocol tperResetComId 512 (B.replicate 512 0))) enabled of
         (Out (Response Good _), st') -> (enabled, st')
         (other, _)                   -> error ("Hsed.Law.Reset: TPER_RESET was not accepted: " <> show other)
  Stack c               -> (st, stackReset c st)
  where
    evented e = case step (Ev e) st of
      (Evented Good, st') -> (st, st')
      (other, _)          -> error ("Hsed.Law.Reset: the event " <> show e <> " was not Good: " <> show other)

-- | A state with TPER_RESET enabled: the Admin SP's TPerInfo row's
--   ProgrammaticResetEnable True (Opal 2.30 Table 26, PDF 49), the column
--   found by name in the SP's description of the table.
tperResetEnabled :: TPerState -> TPerState
tperResetEnabled st = st { tsSPs = Map.adjust enable uSP_Admin (tsSPs st) }
  where
    enable sps = case Map.lookup uTPerInfoTable (spsDescs sps) >>= (`columnOf` "ProgrammaticResetEnable") of
      Just c  -> sps { spsTables = Map.adjust (map (putCell c (booleanCell True))) uTPerInfoTable (spsTables sps) }
      Nothing -> sps


-- * The laws

-- | Core 5.7.2.2.10 (PDF 275, its first paragraph): LockOnReset "identif[ies]
--   the reset types that cause the values of the ReadLocked and WriteLocked
--   columns of the Locking table to be set to True"; an empty set changes
--   neither (its third). Opal 3.2.3 h (PDF 29) makes TPER_RESET's type
--   Programmatic (3, Opal Table 16, PDF 32), and 4.3.5.2.2 (PDF 88) makes
--   {0} and {0, 3} supported. SPF-12 case 1 (TPER_RESET; its steps 1 to 5,
--   step 4's ReadLocked and WriteLocked TRUE, Test Cases PDF 66, the jig's
--   variant SPF-12:1/no-media since M5b, Scott's answer Q2) and UCT-10 (a
--   power cycle, step 9, Test Cases PDF 39-40) observe it. The property: for any
--   state and any of the TPer's resets, each Locking row of each SP not
--   Manufactured-Inactive whose LockOnReset lists the reset's type is
--   ReadLocked and WriteLocked True after it, whatever its ReadLockEnabled
--   and WriteLockEnabled, and else as it was (Opal 2.30 5.2.2.3.1, PDF 97:
--   locking disabled while the SP is Manufactured-Inactive); every other
--   cell of every Locking row as it was; and
--   STACK_RESET on any ComID leaves every Locking row as it was (Core
--   3.3.4.7.5 items 1-8, PDF 65-66, name none).
lockOnReset :: Law
lockOnReset = Law
  { lawName  = "Reset.lockOnReset"
  , lawHash  = "9594d522ccb0"
  , lawProp  = Just (resetColumnLaw uLockingTable "LockOnReset" [("ReadLocked", True), ("WriteLocked", True)])
  , lawCases = [caseOf (SPF 12) 1, noMedia, whole (UCT 10)]
  }

-- | Core 5.7.2.5.4 (PDF 278, its second paragraph): "The MBRDoneOnReset
--   column identifies the types of resets that cause Done to be
--   automatically set to False. If the set is empty, the device SHALL NOT
--   change the value of the Done column on any reset." Opal 3.2.3 i (PDF 29)
--   for TPER_RESET's Programmatic. SPF-12 case 2 (TPER_RESET, Test Cases PDF
--   66) and UCT-11 (PDF 41) observe it. The property: for any state and any
--   of the TPer's resets, each MBRControl row of an SP not
--   Manufactured-Inactive whose MBRDoneOnReset lists the reset's type is
--   Done False after it, and else as it was; its Enable and
--   every other cell as they were; STACK_RESET changes no MBRControl row.
mbrDoneOnReset :: Law
mbrDoneOnReset = Law
  { lawName  = "Reset.mbrDoneOnReset"
  , lawHash  = "7e253a3ed3e0"
  , lawProp  = Just (resetColumnLaw uMBRControlTable "MBRDoneOnReset" [("Done", False)])
  , lawCases = [caseOf (SPF 12) 2, whole (UCT 11)]
  }

-- | Core 5.2.2.4.4 (PDF 148): "TCG Hardware Resets and TCG Power Cycle
--   Resets SHALL cause the TPer's knowledge of the host's communications
--   capabilities, on all ComIDs, to be reset to the initial minimum
--   assumptions ... Other TCG reset events SHALL NOT"; Opal 3.2.3 g (PDF 29)
--   adds TPER_RESET; STACK_RESET resets its ComID's alone (Core 5.2.2.4.5,
--   PDF 148; 3.3.4.7.5 item 7, PDF 65-66). No case the jig catalogues
--   observes it. The property: after a power cycle, a hardware reset or
--   TPER_RESET every ComID keeps the initial assumptions; after a hot plug
--   every ComID keeps what it had; after STACK_RESET on a ComID that ComID
--   keeps the initial assumptions and every other what it had.
properties :: Law
properties = Law
  { lawName  = "Reset.properties"
  , lawHash  = "759d1a8f5208"
  , lawProp  = Just $
      forAll genState $ \st ->
      forAll (elements (resets st)) $ \reset ->
        let (before, after) = afterReset reset st
            expected c cs = case reset of
              Whole OnHotPlug -> cHostProps cs
              Whole _         -> initialHostProperties
              Stack c'        -> if c == c' then initialHostProperties else cHostProps cs
        in counterexample (show reset) $
             fmap cHostProps (tsComIds after) === Map.mapWithKey expected (tsComIds before)
  , lawCases = []
  }

-- | Core 5.3.4.1.1.2 (PDF 210, its sixth paragraph): "the value of the
--   Tries column SHALL be reset to 0 after a power cycle if the value of
--   the Persistence column is False. Otherwise, the value of the Tries
--   column SHALL persist across power cycles. Other TCG resets (i.e.
--   Hardware Reset) SHALL NOT cause the value of the Tries column to be
--   reset", in every SP's C_PIN table (the Locking SP's since M5a). SPF-05
--   (Test Cases PDF 56-57) observes it. The property: after a power cycle
--   each C_PIN row of each SP holding a Tries cell holds 0 there unless its
--   Persistence is True (a row without Persistence its default, False, Core
--   Table 182), every other cell as it was; after any other reset every
--   C_PIN row as it was.
tries :: Law
tries = Law
  { lawName  = "Reset.tries"
  , lawHash  = "5431c0205778"
  , lawProp  = Just $
      forAll genState $ \st ->
      forAll (elements (resets st)) $ \reset ->
        let (before, after) = afterReset reset st
            expected sps r
              | reset == Whole OnPowerCycle && not (booleanNamedIn sps uC_PINTable "Persistence" r) = putNamed sps uC_PINTable "Tries" (UIntCell 0) r
              | otherwise                                                                           = r
        in counterexample (show reset) $ tableIn uC_PINTable after === Map.mapWithKey (\sp -> map (expected (spOf before sp))) (tableIn uC_PINTable before)
  , lawCases = [whole (SPF 5)]
  }
  where
    putNamed sps table name cell r = case Map.lookup table (spsDescs sps) >>= (`columnOf` name) of
      Just c | isJust (L.lookup c r) -> map (\(c', old) -> (c', if c' == c then cell else old)) r
      _                              -> r

-- | Opal 2.30 3.3.5.1 item 1 (PDF 31): an interface reset aborts every
--   open session ("All open sessions SHALL be aborted"; 3.2.3 b, PDF 29, for
--   TPER_RESET), and a statically allocated ComID stays Active (Opal 3.3.3,
--   PDF 30: "keep all statically allocated ComIDs in the Active state"), so
--   that, with no session, it is Issued (Core 3.3.3, PDF 56-57: Issued, no
--   session started on it; Associated, one or more open). SPF-12 case 1's
--   second StartSession (its step 3, Test Cases PDF 66; the jig's variant
--   SPF-12:1/no-media since M5b) observes it. The property: after
--   any of the TPer's resets no session is open and every static ComID is
--   Issued and awaiting IF-SEND; after STACK_RESET on a ComID that ComID is
--   Issued and awaiting IF-SEND with no session, and every other ComID's
--   sessions and life are as they were.
comIdIssued :: Law
comIdIssued = Law
  { lawName  = "Reset.comIdIssued"
  , lawHash  = "6c1ce772d278"
  , lawProp  = Just $
      forAll genState $ \st ->
      forAll (elements (resets st)) $ \reset ->
        let (before, after) = afterReset reset st
            reset'd c = case reset of
              Whole _  -> True
              Stack c' -> c == c'
            ofComId s c = [ ses | ((c', _), ses) <- Map.toList (tsSessions s), c' == c ]
            expected c
              | reset'd c = (Just Issued, True, [])
              | otherwise = (lifeOf c before, awaiting c before, ofComId before c)
            actual c = (lifeOf c after, awaiting c after, ofComId after c)
        in counterexample (show reset) $
             map actual (Map.keys (tsComIds after)) === map expected (Map.keys (tsComIds before))
  , lawCases = [caseOf (SPF 12) 1, noMedia]
  }
  where
    awaiting c s = maybe False ((== S1) . cSync) (Map.lookup c (tsComIds s))

-- | The resets' laws: none is bound by a comment yet (design 13.29 (j)).
laws :: [Law]
laws = [lockOnReset, mbrDoneOnReset, properties, tries, comIdIssued]


-- * Parts

-- | The law of a reset column (LockOnReset, MBRDoneOnReset): for any state
--   and any reset, each row of a table in each SP not Manufactured-Inactive
--   (Opal 2.30 Table 49, PDF 98: its LifeCycleState 8; 5.2.2.3.1, PDF 97)
--   whose set column lists the reset's type ('printedType') holds after a
--   TPer reset the values named, and else is as it was; every other cell as
--   it was; STACK_RESET changes no row.
resetColumnLaw :: UID -> Text -> [(Text, Bool)] -> Property
resetColumnLaw table listing puts =
  forAll genState $ \st ->
  forAll (elements (resets st)) $ \reset ->
    let (before, after) = afterReset reset st
        expected sp sps r = case reset of
          Whole kind | active sp, listedIn sps kind r -> L.foldl' (\r' (name, b) -> putIn sps name (booleanCell b) r') r puts
          _                                           -> r
        active sp = spLifeCycle sp before /= Just manufacturedInactive
        listedIn sps kind r = case cellNamedIn sps table listing r of
          Just (ListCell cells) -> printedType kind `elem` [ v | EnumCell _ v <- cells ] <> [ fromInteger v | UIntCell v <- cells ]
          _                     -> False
    in counterexample (show reset) $
         tableIn table after === Map.mapWithKey (\sp -> map (expected sp (spOf before sp))) (tableIn table before)
  where
    putIn sps name cell r = maybe r (\c -> putCell c cell r) (Map.lookup table (spsDescs sps) >>= (`columnOf` name))
    -- Opal 2.30 Table 49 (PDF 98): Manufactured-Inactive
    manufacturedInactive = 8

-- | A reset kind's type as Opal 2.30 Table 16 prints it (PDF 32: Power
--   Cycle 0, Hardware 1, HotPlug 2, Programmatic 3, TPER_RESET's, Opal 3.2.3
--   h, PDF 29), written here so that the laws do not take it from the
--   simulator they check.
printedType :: ResetKind -> Int
printedType = \case
  OnPowerCycle    -> 0
  OnHardwareReset -> 1
  OnHotPlug       -> 2
  OnTPerReset     -> 3

-- | A table's rows in each SP that holds it, by the SP's UID.
tableIn :: UID -> TPerState -> Map UID [Row]
tableIn table st = Map.mapMaybe (Map.lookup table . spsTables) (tsSPs st)

-- | An SP of a state, by UID; an SP the state lacks is one of no tables.
spOf :: TPerState -> UID -> SPState
spOf st sp = fromMaybe (SPState sp mempty mempty mempty) (Map.lookup sp (tsSPs st))


-- | A whole test.
whole :: TestId -> CaseKey
whole t = CaseKey t Nothing Nothing Nothing

-- | One case of a test.
caseOf :: TestId -> Int -> CaseKey
caseOf t c = CaseKey t (Just c) Nothing Nothing

-- | SPF-12 case 1's steps 1 to 5, the jig's variant @no-media@ (since M5b;
--   steps 6 and 7, media I/O, and so the case whole, M6's: Scott's answer
--   Q2, design 11 M5b; "Hsed.Jig.Catalogue").
noMedia :: CaseKey
noMedia = CaseKey (SPF 12) (Just 1) Nothing (Just "no-media")
