{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Jig.Cases.SPF12
Description : SPF-12 case 1, steps 1 to 5: TPER_RESET locks the global range where LockOnReset lists Programmatic
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SPF-12 case 1 (Test Cases 1.02, PDF 65-66), its steps 1 to 5, the jig's
variant @no-media@: with ProgrammaticResetEnable TRUE, the global range
unlocked with its locks enabled, Programmatic among its LockOnReset and the
MBR not enabled (its own prerequisites 1-4 and 6, PDF 65-66), StartSession
to the Locking SP as Admin1, TPER_RESET (Opal 2.30 3.2.3, PDF 28-29), a new
session as Admin1, whose Get finds the global range's ReadLocked and
WriteLocked TRUE (PDF 66; Opal 3.2.3 h, PDF 29), and CLOSE_SESSION. Steps 6
and 7, media I/O over the range, are not run: no media I/O on the wire
(design 8.5); the case whole is M6's (Scott's answer Q2, design 11 M5b),
which the case observes, with the Test Cases' slip that names them steps
#5 and #6 (PDF 66).

The prerequisites are established in order (Test Cases 3.2.3, PDF 25),
each by Set after a Get of what the cell held: ProgrammaticResetEnable as
SID (ACE_TPerInfo_Set_ProgrammaticResetEnable, Opal 2.30 Table 23, PDF 47;
Table 27, PDF 50: FALSE at the factory), the global range's five columns
as Admin1 (ACE_Locking_GlblRng_Admins_Set, Table 39, PDF 80; LockOnReset
{Power Cycle, Programmatic}, which every TPer supports, 4.3.5.2.2, PDF 88),
and MBRControl's Enable where it is not FALSE (ACE_MBRControl_Admins_Set,
Table 39, PDF 81; FALSE at the factory, Table 46, PDF 88). What the Sets
changed is put back inside the case whatever the verdict, as Admin1 then as
SID, and a restore that fails fails the case (Scott's answer Q4), so that
the cases after it find the TPer as the factory left it (design 13.34
(10)). TPER_RESET aborts the session of step 1 (Opal 3.2.3 b, PDF 29), which
the run's context then forgets; a TPER_RESET that did not happen leaves the
session, which STACK_RESET clears before the step fails.

Its sessions need the Locking SP Manufactured, the common prerequisite of
6.2 (PDF 49); its Sets and TPER_RESET are writes no drive takes before M6,
so the simulator alone runs it (design 9.3; 13.34 (8)), on its Set and the
jig's TPER_RESET (since M5b). The jig's extension is M5's reset clause
through the jig: a power cycle of the simulator sets ReadLocked and
WriteLocked where LockOnReset lists Power Cycle, as the factory's does
(Core 5.7.2.2.10, PDF 275; Opal 2.30 Table 45, PDF 86), and the extension
unlocks the range again.
-}
module Hsed.Jig.Cases.SPF12
  ( spf12case1nomedia
  ) where

import           RIO
import qualified RIO.Text            as T

import           Hsed.Core.CellBlock (columnRange)
import           Hsed.Core.Token     (Token (..))
import           Hsed.Core.Types     (enumValue)
import           Hsed.Core.UID       (UID)
import           Hsed.Core.Value     (Datum (..), List (..), Name (..), NamedValue (..), Value (..))
import           Hsed.Gen.Core.Codes (pattern SUCCESS)
import           Hsed.Gen.Core.TableDescs (lockingTableDesc, mbrControlTableDesc)
import           Hsed.Gen.Core.UIDs  (uAuthority_SID, uLocking_GlobalRange, uMBRControl_Row, uSP_Admin)
import           Hsed.Gen.Opal.Enums (Opal_reset_types (..))
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import qualified Hsed.Gen.Opal.TableDescs as Opal
import           Hsed.Jig.Case       (Case (..), Need (..))
import           Hsed.Jig.Catalogue  (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq     (Section (..))
import           Hsed.Jig.Script     (MethodOutcome (..), Script, SessionHandle (..), StartArgs, Verdict (..), asAuthority, attempt,
                                      describeStatus, exchanged, expectMethodStatus, getCells, namedColumn, observe, opened,
                                      powerCycle, sessionGone, setCells, stackReset, startSession, tperInfoObject, tperReset,
                                      unmet, withSession)
import           Hsed.Wire           (IfStatus (..), hex)


-- | The case.
spf12case1nomedia :: Case
spf12case1nomedia = Case
  { cKey        = CaseKey (SPF 12) (Just 1) Nothing (Just "no-media")
  , cSection    = SpecificFunctionality
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedWrite, NeedSimOnly]
  , cRun        = run
  , cExtensions = [("a power cycle sets ReadLocked and WriteLocked where LockOnReset lists Power Cycle (Core 5.7.2.2.10, PDF 275; Opal 2.30 Table 45, PDF 86: M5's reset clause through the jig)", powerCycleLocks)]
  }

-- | An object of an SP and the cells it held before the case set them, as
--   Get read them, which the restore puts back (Scott's answer Q4).
data Kept = Kept
  { kName   :: !Text
  , kObject :: !UID
  , kCells  :: ![(Natural, Value)]
  }

-- | The case's own prerequisites established, then its steps 1 to 5, under
--   'attempt'; then the restore of what the prerequisites changed, the
--   Locking SP's objects as Admin1 and the Admin SP's as SID, whatever the
--   verdict; the verdict every failure of either.
run :: Script Verdict
run = do
  admin1 <- asAuthority "Admin1" uSP_Locking LockingSP.uAuthority_Admin1
  sid    <- asAuthority "SID" uSP_Admin uAuthority_SID
  adminKept   <- newIORef []
  lockingKept <- newIORef []
  v <- attempt (Pass <$ (establishOwn sid admin1 adminKept lockingKept >> steps admin1))
  restored <- sequence [restore "Admin1" admin1 lockingKept, restore "SID" sid adminKept]
  pure $ case concatMap reasons (v : restored) of
    [] -> Pass
    rs -> Fail rs
  where
    reasons = \case
      Fail rs        -> rs
      Skipped r      -> [r]
      Unobservable r -> [r]
      Pass           -> []

-- | The case's own prerequisites 1 to 4 and 6 (Test Cases PDF 65-66), in
--   order, each observed: 1, as SID, TPerInfo's ProgrammaticResetEnable
--   read and set TRUE; 2 to 4, as Admin1, the global range's
--   ReadLockEnabled, WriteLockEnabled, ReadLocked, WriteLocked and
--   LockOnReset read, then set TRUE, TRUE, FALSE, FALSE and {Power Cycle,
--   Programmatic}; 6, MBRControl's Enable read, and set FALSE where it is
--   not. What each Set changed is kept for the restore, as it was read.
establishOwn :: StartArgs -> StartArgs -> IORef [Kept] -> IORef [Kept] -> Script ()
establishOwn sid admin1 adminKept lockingKept = do
  tperInfo <- maybe (unmet "the generated Admin SP preconfiguration gives TPerInfo no row (Opal 2.30 Table 27, PDF 50)") pure tperInfoObject
  pre <- namedColumn Opal.tPerInfoTableDesc "ProgrammaticResetEnable"
  settled =<< withSession sid (\h -> do
    was <- cellRead "prerequisite 1: Get TPerInfo's ProgrammaticResetEnable" h tperInfo pre
    modifyIORef' adminKept (Kept "TPerInfo's ProgrammaticResetEnable" tperInfo [(pre, was)] :)
    setSuccess "prerequisite 1: Set TPerInfo's ProgrammaticResetEnable TRUE" h tperInfo [(pre, true)]
    observe "prerequisite 1" ("ProgrammaticResetEnable read " <> boolean was <> ", set TRUE as SID (Test Cases PDF 65; Opal 2.30 Table 23, PDF 47: "
                              <> "ACE_TPerInfo_Set_ProgrammaticResetEnable)")
    pure Pass)
  settled =<< withSession admin1 (\h -> do
    rc <- rangeColumns
    let name = "prerequisites 2-4: Get Locking_GlobalRange's ReadLockEnabled to LockOnReset"
    outcome <- getCells h uLocking_GlobalRange (columnRange (rReadLockEnabled rc) (rLockOnReset rc)) >>= exchanged name
    expectMethodStatus name SUCCESS outcome
    was <- traverse (\c -> (,) c <$> columnIn name c (moResults outcome)) (rangeList rc)
    modifyIORef' lockingKept (Kept "Locking_GlobalRange's ReadLockEnabled, WriteLockEnabled, ReadLocked, WriteLocked and LockOnReset" uLocking_GlobalRange was :)
    setSuccess "prerequisites 2-4: Set Locking_GlobalRange's locks enabled, unlocked, LockOnReset {Power Cycle, Programmatic}" h uLocking_GlobalRange
               [(rReadLockEnabled rc, true), (rWriteLockEnabled rc, true), (rReadLocked rc, false), (rWriteLocked rc, false), (rLockOnReset rc, resetTypes)]
    observe "prerequisites 2-4" ("Locking_GlobalRange read " <> T.intercalate ", " (map (tshow . snd) was) <> " in columns "
                                 <> T.intercalate ", " (map (tshow . fst) was) <> "; set ReadLockEnabled and WriteLockEnabled TRUE, ReadLocked and "
                                 <> "WriteLocked FALSE, LockOnReset {Power Cycle, Programmatic} as Admin1 (Test Cases PDF 65; Opal 2.30 Table 39, PDF "
                                 <> "80: ACE_Locking_GlblRng_Admins_Set; 4.3.5.2.2, PDF 88: {0, 3} supported)")
    enable <- namedColumn mbrControlTableDesc "Enable"
    mbr <- cellRead "prerequisite 6: Get MBRControl's Enable" h uMBRControl_Row enable
    if mbr == false
      then observe "prerequisite 6" "MBRControl's Enable is FALSE, as at the factory (Test Cases PDF 66; Opal 2.30 Table 46, PDF 88)"
      else do
        modifyIORef' lockingKept (Kept "MBRControl's Enable" uMBRControl_Row [(enable, mbr)] :)
        setSuccess "prerequisite 6: Set MBRControl's Enable FALSE" h uMBRControl_Row [(enable, false)]
        observe "prerequisite 6" ("MBRControl's Enable read " <> boolean mbr <> ", set FALSE as Admin1 (Test Cases PDF 66; Opal 2.30 Table 39, PDF 81: "
                                  <> "ACE_MBRControl_Admins_Set)")
    pure Pass)

-- | Step 1 StartSession to the Locking SP as Admin1; step 2 TPER_RESET,
--   which aborts it; steps 3 to 5 in a session of their own
--   ('withSession'): the Get of the global range's ReadLocked and
--   WriteLocked, TRUE TRUE (Test Cases PDF 66; Opal 2.30 3.2.3 h, PDF 29),
--   and what steps 6 and 7 would be, observed.
steps :: StartArgs -> Script ()
steps admin1 = do
  h <- startSession admin1 >>= exchanged step1 >>= opened step1
  step2 h
  settled =<< withSession admin1 (\h' -> do
    rc <- rangeColumns
    (r, w) <- locks step4 h' rc
    unless (r == true && w == true) $
      unmet (step4 <> ": ReadLocked " <> boolean r <> ", WriteLocked " <> boolean w <> ", not TRUE TRUE (Test Cases PDF 66; Opal 2.30 3.2.3 h, PDF 29)")
    observe "step 4" "Get Locking_GlobalRange's ReadLocked and WriteLocked after TPER_RESET: TRUE TRUE (Opal 2.30 3.2.3 h, PDF 29: LockOnReset lists Programmatic)"
    observe "steps 6-7" ("not run: no media I/O on the wire (design 8.5); SPF-12 case 1 whole is M6's (design 13.34 (10)). The Test Cases' expected "
                         <> "results 3 and 4 name steps #5 and #6 for the write and the read, which are steps 6 and 7 (PDF 66)")
    pure Pass)
  where
    step1 = "step 1: StartSession to the Locking SP as Admin1"
    step4 = "step 4: Get Locking_GlobalRange's ReadLocked and WriteLocked"

-- | Step 2: TPER_RESET, its IF-SEND Good, every session aborted with it
--   (Opal 2.30 3.2.3 a-b, PDF 28-29), the session of step 1 forgotten
--   ('sessionGone'). A TPER_RESET that did not happen (refused, or
--   answered another status) leaves that session open: STACK_RESET on its
--   ComID clears it, noted, before the step fails.
step2 :: SessionHandle -> Script ()
step2 h = tperReset >>= \case
  Right Good -> do
    observe "step 2" ("TPER_RESET: the IF-SEND of protocol 0x02 to ComID 0x0004 answered " <> describeStatus Good
                      <> "; every session aborted, the host's properties reset (Opal 2.30 3.2.3 a-g, PDF 28-29)")
    sessionGone h "TPER_RESET aborted every session, the case's with its own (Opal 2.30 3.2.3 b, PDF 29)"
  answered -> do
    cleared <- stackReset (shComId h)
    when (cleared == Right 0) $
      sessionGone h ("STACK_RESET on " <> hex (shComId h) <> " after a TPER_RESET that did not happen: every session on the ComID aborted "
                     <> "(Core 3.3.4.7.5 item 1, PDF 65)")
    unmet (name <> ": " <> either id (("the IF-SEND answered " <>) . describeStatus) answered <> ", not " <> describeStatus Good
           <> "; STACK_RESET on " <> hex (shComId h) <> ": " <> either id (("Success/Failure " <>) . tshow) cleared)
  where name = "step 2: TPER_RESET"

-- | What the prerequisites changed put back, the step named for its
--   authority: a session as it ('withSession'), each object's cells Set to
--   what the case read first, SUCCESS expected of each; nothing kept,
--   nothing to do. A failure here fails the case (Scott's answer Q4).
restore :: Text -> StartArgs -> IORef [Kept] -> Script Verdict
restore who args kept = readIORef kept >>= \case
  [] -> pure Pass
  ks -> attempt . withSession args $ \h -> do
    for_ ks $ \k -> do
      setSuccess ("restore: Set " <> kName k <> " back, as " <> who) h (kObject k) (kCells k)
      observe "restore" (kName k <> " set back to what the case read first, as " <> who <> " (Scott's answer Q4)")
    pure Pass

-- | The extension: a power cycle of the simulator ('powerCycle'), after
--   which, as Admin1, the global range's ReadLocked and WriteLocked read
--   TRUE TRUE, its LockOnReset listing Power Cycle as the factory's does
--   (Opal 2.30 Table 45, PDF 86; Core 5.7.2.2.10, PDF 275: set True whatever
--   ReadLockEnabled and WriteLockEnabled), and are Set FALSE again, as the
--   factory left them.
powerCycleLocks :: Script Verdict
powerCycleLocks = do
  admin1 <- asAuthority "Admin1" uSP_Locking LockingSP.uAuthority_Admin1
  powerCycle >>= \case
    Right Good -> observe "power cycle" ("the simulator's power cycle answered " <> describeStatus Good <> ": every session aborted (Opal 2.30 3.3.5.1 item 1, PDF 31)")
    answered   -> unmet ("the power cycle " <> either id (("answered " <>) . describeStatus) answered <> ", not " <> describeStatus Good)
  withSession admin1 $ \h -> do
    rc <- rangeColumns
    (r, w) <- locks name h rc
    unless (r == true && w == true) $
      unmet (name <> ": ReadLocked " <> boolean r <> ", WriteLocked " <> boolean w <> ", not TRUE TRUE (Core 5.7.2.2.10, PDF 275)")
    observe "after the power cycle" "ReadLocked TRUE, WriteLocked TRUE: LockOnReset lists Power Cycle at the factory (Opal 2.30 Table 45, PDF 86; Core 5.7.2.2.10, PDF 275)"
    setSuccess "Set Locking_GlobalRange's ReadLocked and WriteLocked FALSE, as the factory left them" h uLocking_GlobalRange
               [(rReadLocked rc, false), (rWriteLocked rc, false)]
    pure Pass
  where name = "Get Locking_GlobalRange's ReadLocked and WriteLocked after the power cycle"


-- * What the steps share

-- | The Locking table's columns the case sets and reads, found by name in
--   the generated description (Core Table 226, PDF 273).
data Range = Range
  { rReadLockEnabled  :: !Natural
  , rWriteLockEnabled :: !Natural
  , rReadLocked       :: !Natural
  , rWriteLocked      :: !Natural
  , rLockOnReset      :: !Natural
  }

rangeColumns :: Script Range
rangeColumns = Range <$> col "ReadLockEnabled" <*> col "WriteLockEnabled" <*> col "ReadLocked" <*> col "WriteLocked" <*> col "LockOnReset"
  where col = namedColumn lockingTableDesc

-- | The five, in column order.
rangeList :: Range -> [Natural]
rangeList rc = [rReadLockEnabled rc, rWriteLockEnabled rc, rReadLocked rc, rWriteLocked rc, rLockOnReset rc]

-- | Get the global range's ReadLocked and WriteLocked in a session, the
--   step named: SUCCESS, and the two values.
locks :: Text -> SessionHandle -> Range -> Script (Value, Value)
locks name h rc = do
  outcome <- getCells h uLocking_GlobalRange (columnRange (rReadLocked rc) (rWriteLocked rc)) >>= exchanged name
  expectMethodStatus name SUCCESS outcome
  (,) <$> columnIn name (rReadLocked rc) (moResults outcome) <*> columnIn name (rWriteLocked rc) (moResults outcome)

-- | Get one column of an object in a session, the step named: SUCCESS, and
--   the value the result holds in the column.
cellRead :: Text -> SessionHandle -> UID -> Natural -> Script Value
cellRead name h object c = do
  outcome <- getCells h object (columnRange c c) >>= exchanged name
  expectMethodStatus name SUCCESS outcome
  columnIn name c (moResults outcome)

-- | Set cells of an object in a session, the step named, SUCCESS expected.
setSuccess :: Text -> SessionHandle -> UID -> [(Natural, Value)] -> Script ()
setSuccess name h object cells = setCells h object cells >>= exchanged name >>= expectMethodStatus name SUCCESS

-- | The value a Get's result holds in a column: @[[c = v]]@, one list of
--   the row's values (Core 5.3.3.6.2.2, PDF 201), the step named; the
--   script stopped where the result is not of that form.
columnIn :: Text -> Natural -> [Value] -> Script Value
columnIn name c = \case
  [L (List cells)] -> case [ v | N (NamedValue (Name (Datum (Unsigned col))) v) <- cells, col == c ] of
    v : _ -> pure v
    []    -> unmet (name <> ": no column " <> tshow c <> " among the result's " <> tshow (length cells) <> " cell(s) (Core 5.3.3.6.2.2, PDF 201)")
  results -> unmet (name <> ": the result is not one list of the row's values: " <> tshow (length results) <> " result(s)")

-- | A session's steps settled: a verdict other than 'Pass' stops the
--   script with its reasons, for the case's 'attempt' to take.
settled :: Verdict -> Script ()
settled = \case
  Pass           -> pure ()
  Fail rs        -> unmet (T.intercalate "; " rs)
  Skipped r      -> unmet r
  Unobservable r -> unmet r

-- | A boolean as a method carries it: 1 True, 0 False (Core Table 59, PDF
--   103; 5.1.2 b, PDF 99: an enumeration as its value).
true, false :: Value
true  = D (Datum (Unsigned 1))
false = D (Datum (Unsigned 0))

-- | A boolean value in the Test Cases' words, any other value as it is.
boolean :: Value -> Text
boolean v
  | v == true  = "TRUE"
  | v == false = "FALSE"
  | otherwise  = tshow v

-- | LockOnReset {Power Cycle, Programmatic}, a reset_types set as a method
--   carries it (Core 5.1.2 i, PDF 100: a set as a list of its elements'
--   values), the values Opal 2.30 Table 16's, 0 and 3 (PDF 32).
resetTypes :: Value
resetTypes = L (List [ D (Datum (Unsigned (fromIntegral (enumValue t)))) | t <- [Opal_reset_types_Power_Cycle, Opal_reset_types_Programmatic] ])
