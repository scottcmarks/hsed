{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Run
Description : A jig run: open the target, run the selected cases in order, check the transcript, write the reports
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsed-jig run@ (design 9.2): refuse what it cannot run before anything
opens, read the profile, check what is mounted on a drive the run may send
to ("Hsed.Jig.Preflight"), open the target, establish or check each case's
prerequisites and run it, its waivers printed, and its extensions on the
simulator; on a drive, capture its Level 0 response when asked, and decide
again every command it was issued under the policy it ended with; write
the reports. With @--recover@ the target's one end runs "Hsed.Jig.Recover"
in place of the cases.

What is refused first, in order ('runJig'): a key the catalogue lacks;
@--recover@ with @--tests@, with a pair or with @--interrupt-in-session@,
@--recover@ running alone on one end and never stopping itself; a drive
target without the serial it must have; a Level 0 transfer length a drive
or a replay cannot carry: each a usage error; a drive target without root,
hsed's own gate (design 9.3); a profile that cannot be read; then, for a
drive the run may send to (its allowed list not empty, 'driveAllowedFor'),
a slice of it mounted, unless @--allow-mounted@, whose mounts are noted in
the reports (design 9.3 item 4). Through hsed-jig's command line
("Hsed.Jig.Cli") a drive target meets two of these first, before anything
else is read: without @--expect-serial@ (2), then without root (3); so a
drive run as yourself with a key the catalogue lacks exits 3. Then SIGINT,
SIGTERM and SIGHUP are each caught once, for the run's length: the signal
stops the run ('Hsed.Jig.Script.reStop'), the command in flight completing,
and a second of it kills the process (the kernel then closes the kext's
user client; nothing more reaches the drive); after the run the handlers
it found are put back, whatever they were.

The exit code ('runExit'): 0 every selected case passed (waived
prerequisites printed), no extension failed, a pair found only allowed
differences (or unchecked ones) and no Properties difference that fails,
the capture asked for was written and the Guard refused nothing; 1 a
failure: a case failed, or was skipped or unobservable without a waiver, an
extension failed, the pair found a code missing, a difference not allowed
or a Properties difference that fails, the capture failed, the Guard
refused a command (each Blocked in the transcript, a problem of the run),
@--recover@ did not recover, or a profile or target named could not be
read or opened; 2 a usage error (above); 3 a safety violation: a drive
target without root, a drive mounted where the run may send to it without
@--allow-mounted@, or a command a drive was issued that its policy, as the
run ended, does not admit ('outsidePolicy'); 130 the run was stopped (a
signal, @--interrupt-in-session@): the command in flight completed, nothing
more was issued, no End of Session was sent, the reports were written. A
safety violation outranks a stop, and a stop a failure: 3, then 130, then
1, then 0.

The cases run in the catalogue's order, whatever the order of @--tests@ (the
Test Cases run section 5's in sequence, 3.1, PDF 25); a key naming a whole
test selects every case of it the catalogue has; a run with no @--tests@
selects every case of the catalogue but those its default leaves out, each
noted with why before the first case ('leftOutByDefault': since M5b, on a
target with the simulator's end, the cases whose Locking SP prerequisite its
Original Factory State, @--locking-ofs@, does not meet, Scott's answer Q1;
on a drive or a replay alone, none). Every command of the run
goes into one transcript, created before the target is opened: the
simulator's transport records what it exchanges, and a drive comes behind
the Guard ("Hsed.Jig.Target", "Hsed.Jig.Guard".'GuardedDrive'), every
command it is given checked first under its policy, Level0Only until its
base ComID is bound with what the run admits on it ('driveAllowedFor':
@--recover@'s methods, or, where a case selected from design 9.3's drive
list needs more than Level 0 Discovery, the read-only methods and, with
@roStackResetBeforeCases@, STACK_RESET), and recorded in a transcript of
its own too. The run's end reports each drive's policy, the commands its
own transcript holds that the policy does not admit ('outsidePolicy'), the
commands the Guard refused, why the run was stopped, a drive's pre-flight
notes, each end's Properties exchange with the host's properties and, for
a pair, the two compared ("Hsed.Jig.PairProperties"). A pair's ends run each
case in turn, the simulator first, each case's verdicts joined, each with
its end's name, its transport's role (@tper@, @drive@, @drive-replay@) made
unique by position since M5c (a pair of two simulators' second, @tper-2@;
design 13.33 (28)): the one name the transcript's entries carry
("Hsed.Jig.Transcript"), the reports recording beside it what was opened
for a @ws://@ end ("Hsed.Jig.Report"); then the
two Level 0 responses are compared
("Hsed.Jig.Pair"), discovered after the cases when UCT-01 did not. A drive
is never asked for Level 0 again once it was asked (a failed kext command
leaks a SCSI task in the kext), for the pair or for the capture. A case
whose TCG verdict is Skipped or Unobservable does not run its extensions:
each is reported Skipped with the case's reason (design 9.1, 13.29 (b)),
as are those of a case its target cannot run or whose prerequisite is
unmet. What a case observes ("Hsed.Jig.Script".@observe@) goes into its
result, what an extension observes under the extension's name, and the
secrets the ends read (UCT-03's MSID, a PIN the run reads from the
simulator's state, an answer a step could not read) into the run's, masked
in the reports unless @--show-msid@ (design 9.3 item 5).

A drive runs design 9.3's cases alone ('driveCases': UCT-01, UCT-02,
UCT-03:1a-1c, SPF-02:1; M4c), each of the others skipped, saying so, a case
that writes to the TPer first among them (since M5b: drives stay read-only
until M6, design 9.3; 13.34 (8)); and a case that needs more than Level 0
Discovery is skipped on a replay, which answers Level 0 Discovery alone, and
on a drive the run binds nothing on (no case it selected from the list needs
more).

A case's prerequisites are its section's list, in order, then its own (Test
Cases 3.2.3, PDF 25; since M5b, ETC-20's Locking SP Manufactured-Inactive):
one it waives is printed with its reason; on the simulator the run
establishes the synchronous protocol idle (draining every static ComID not
in Awaiting IF-SEND), establishes that no session is open (STACK_RESET on
every ComID whose sessions a case before left open, Core 3.3.4.7.5, PDF 65,
noted in the transcript), takes the credentials known (vacuous for a case
that authenticates no authority; for one that does, the authority's PIN read
from the simulator's state, which the case may then send or vary), each
HostChallenge the current PIN likewise, and every session read-write (every
StartSession the jig sends has Write True, Test Cases 4.1 e, PDF 26). The
simulator's state is read through its view alone, in process as over the
LAN leg ("Hsed.Sim.View"'s queries; M5c, Option A), a view that does not
answer leaving the prerequisite that reads it unmet; and before each case,
and before the closing Level 0, the run hears what the simulator says
happened to it (a @ws://@ hub that restarted, broadcasts missed, another
client's reset), each noted and the run's session forgotten as a power
cycle forgets it ("Hsed.Jig.Script".'Hsed.Jig.Script.heedNotices'). On a
drive, for a case that needs Level 0 alone, SyncIdle and NoSessions are
waived as not needed, Level 0 being outside the synchronous protocol (Core
3.3.10.3 b, PDF 84) and answered whatever the sessions (3.3.6.2, PDF 67);
for any other, where the run admits STACK_RESET (@roStackResetBeforeCases@,
Scott's decision of 2026-11-02, the default), one STACK_RESET on the base
ComID before the case establishes both, noted: every session on it aborted,
its stack in its initial state, its host properties back to their defaults
(Core 3.3.4.7.5 items 1, 4, 6 and 7, PDF 65); where it does not, SyncIdle is
checked by one IF-RECV on the base, which must find nothing to return (Table
45's (0, 0, 0), 3.3.10.5 S1 b, PDF 84), and NoSessions is unmet where this
run left a session open, and otherwise waived as unobservable
("Hsed.Jig.Prereq".'Hsed.Jig.Prereq.sessionsUnobservable'). On a drive
CredentialsKnown and HostChallengeIsCurrentPin are vacuous for a case that
authenticates no authority and unmet for one that does, every session is
read-write, and LockingManufactured, which every drive case waives, is not
observable. A prerequisite neither waived nor met skips the case.

A stopped run ('Hsed.Jig.Script.Interrupted'): the case in flight is
skipped as interrupted (on the simulator, a case stopped in one of its
extensions keeps its TCG verdict, the extension skipped as interrupted and
those after it not run), each case after it skipped, not run; no Level 0
Discovery after the cases, no pair and no capture follow; the reports say
why it stopped, and name a session it left open, for @--recover@. A session
a StartSession may have opened whose SyncSession the host could not read,
and which no STACK_RESET has cleared since ("Hsed.Jig.Script".@rcSessionUnread@),
is a problem of the run, named with @--recover@'s hint, stopped or not; so
is a session a run that was not stopped left open: its End of Session
unanswered and no STACK_RESET answering Success/Failure 0 since (none
sent, or the closing one answering otherwise, "Hsed.Jig.Script".@closingStep@),
or, on a drive the run latched, not sent.
-}
module Hsed.Jig.Run
  ( RunOptions (..)
  , runJig
  , runOpened
  , runOpenedStopping
  , cases
  , selectedCases
  , chosenCases
  , leftOutByDefault
    -- * A drive's run
  , driveCases
  , driveAllowedFor
  , outsidePolicy
  , runExit
  , withStopSignals
  ) where

import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO
import qualified RIO.Text               as T
import           RIO.Time               (UTCTime, getCurrentTime)

import           Hsed.Core.UID          (UID)
import           Hsed.Gen.Core.UIDs     (uAuthority_SID, uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Capture       (captureLevel0)
import           Hsed.Jig.Case          (Case (..), Need (..))
import           Hsed.Jig.Cases.ETC02   (etc02)
import           Hsed.Jig.Cases.ETC03   (etc03)
import           Hsed.Jig.Cases.ETC04   (etc04)
import           Hsed.Jig.Cases.ETC07   (etc07)
import           Hsed.Jig.Cases.ETC08   (etc08)
import           Hsed.Jig.Cases.ETC09   (etc09)
import           Hsed.Jig.Cases.ETC14   (etc14)
import           Hsed.Jig.Cases.ETC15   (etc15)
import           Hsed.Jig.Cases.ETC20   (etc20)
import           Hsed.Jig.Cases.ETC21   (etc21, etc21adminsp)
import           Hsed.Jig.Cases.SPF02   (spf02case1, spf02case2)
import           Hsed.Jig.Cases.SPF11   (spf11)
import           Hsed.Jig.Cases.SPF12   (spf12case1nomedia)
import           Hsed.Jig.Cases.UCT01   (uct01)
import           Hsed.Jig.Cases.UCT02   (uct02)
import           Hsed.Jig.Cases.UCT03   (uct03steps1a1c)
import           Hsed.Jig.Catalogue     (CaseKey (..), CatalogueEntry (..), catalogue, renderCaseKey)
import           Hsed.Jig.Files         (runningAsRoot)
import           Hsed.Jig.Guard         (Allowed (..), GuardedDrive, Policy, decide, driveAllowed, driveInfo, driveIssued, drivePolicy,
                                         driveTransport, readOnlyMethods, recoverMethods)
import           Hsed.Jig.Options       (RunOptions (..), TargetSpec (..), lockingOFSName, renderWsUrl)
import           Hsed.Jig.Pair          (pairFails, pairLevel0)
import           Hsed.Jig.PairProperties (pairProperties, propertiesFail)
import           Hsed.Jig.Preflight     (preflightDrive)
import           Hsed.Jig.Prereq        (Prereq (..), Waiver (..), common, level0OutsideSync, listAt, sessionsUnobservable)
import           Hsed.Jig.Recover       (recoverScript)
import           Hsed.Jig.Report        (CaseResult (..), EndRecord (..), RunResult (..), renderTarget, summary, writeReports)
import           Hsed.Jig.Script        (Credential (..), Interrupted (..), RunCtx (..), RunEnv (..), Script, SessionHandle (..),
                                         Verdict (..), admitsStackReset, attempt, baseComId, describeStatus, discover, drain,
                                         emptyRunCtx, heedNotices, ifRecvOn, maskAnswersSince, spName, stackReset, t45Of, t45On,
                                         transcriptMark, verdictText)
import           Hsed.Jig.Simulator     (Opened (..), Simulator (..))
import           Hsed.Jig.Target        (Target (..), closeTarget, openTarget)
import           Hsed.Jig.Transcript    (TEntry (..), Transcript, entries, ifSendsIssued, newTranscript, record, recordingAs)
import           Hsed.Level0            (Level0)
import           Hsed.Sim.Profile       (loadProfile)
import           Hsed.Sim.Types         (ComIdLife (Associated), LifeCycle (..), SimParams (..), SyncState (..), lifeCycleValue,
                                         simParamsProblems)
import           Hsed.Sim.View          (ComIdView (..), SimQuery (..), SimView (..))
import           Hsed.Transport.Cdb     (level0Cdb)
import           Hsed.Transport.Drive   (DriveInfo (..))
import           Hsed.Transport.Kext    (diskName)
import           Hsed.Version           (hsedVersion)
import           Hsed.Wire              (Describe (..), Dir (..), IfStatus (..), Request (..), Response (..), Unit (..),
                                         comPacketProtocol, describeTransport, hex, level0ComId, mostCarried, newChannel,
                                         roundUp, transferLength)
#if !defined(mingw32_HOST_OS)
import           Data.Dynamic           (fromDynamic, toDyn)
import qualified GHC.Conc.Signal        as Conc
import           System.Posix.Signals   (Handler (..), installHandler, sigHUP, sigINT, sigTERM)
#endif


-- | The cases the jig runs, one for each entry of the catalogue, in its
--   order.
cases :: [Case]
cases =
  [ uct01, uct02, uct03steps1a1c, spf02case1, spf02case2, spf11, spf12case1nomedia, etc02, etc03, etc04, etc07, etc08, etc09
  , etc14, etc15, etc20, etc21, etc21adminsp ]

-- | The authorities a case authenticates at StartSession, by the case's
--   key, in order, each with its name, the SP it authenticates to and its
--   UID: ETC-21/adminsp's SID to the Admin SP (Opal 2.30 Table 24, PDF 48);
--   since M5b, Admin1 to the Locking SP (Table 40, PDF 82) for ETC-21 as
--   printed, SPF-02 case 2, SPF-11, ETC-09 and ETC-14, and for SPF-12 case
--   1 SID to the Admin SP, then Admin1 to the Locking SP (Test Cases PDF
--   65); every other case opens its sessions as Anybody, with no authority,
--   or none at all. What CredentialsKnown and HostChallengeIsCurrentPin are
--   about (Test Cases 5.2 items 3 and 4, 7.2 item 3; PDF 29 and 76).
authenticating :: [(CaseKey, [(Text, UID, UID)])]
authenticating =
  [ (cKey etc21adminsp, [sid])
  , (cKey etc21, [admin1]), (cKey spf02case2, [admin1]), (cKey spf11, [admin1]), (cKey etc09, [admin1]), (cKey etc14, [admin1])
  , (cKey spf12case1nomedia, [sid, admin1]) ]
  where
    sid    = ("SID", uSP_Admin, uAuthority_SID)
    admin1 = ("Admin1", uSP_Locking, LockingSP.uAuthority_Admin1)

-- | Run the jig: its gates in order (a key the catalogue lacks, @--recover@
--   with cases or a pair, a drive without @--expect-serial@, @--expect-serial@
--   or @--allow-mounted@ on a target with no drive, a Level 0 length
--   a drive cannot carry, simulator parameters out of range: exit 2; a drive without root: exit 3; a profile
--   that cannot be read: exit 1; a drive the run may send to with a slice
--   mounted, unless @--allow-mounted@: exit 3), then the run's transcript
--   and stop flag, made before the target is opened, the stop signals
--   caught for the run's length ('withStopSignals'), the drive's allowed
--   list the selected cases' ('driveAllowedFor').
runJig :: RunOptions -> IO ExitCode
runJig opts0 = do
  started <- getCurrentTime
  root <- runningAsRoot
  case chosenCases opts0 of
    Left e -> complain e $> ExitFailure 2
    Right (chosen, leftOut)
      | Just why <- recoverProblem opts0 -> complain why $> ExitFailure 2
      | devs@(_ : _) <- devices (roTarget opts0), isNothing (roExpectSerial opts0) -> do
          traverse_ (\d -> complain ("hsed-jig: --target " <> d <> " needs --expect-serial SERIAL: a drive is opened only as "
                                      <> "the one named")) devs
          pure (ExitFailure 2)
      | Just why <- driveFlagsProblem opts0 -> complain why $> ExitFailure 2
      | Just why <- level0LenProblem opts0 -> complain why $> ExitFailure 2
      | Just why <- simParamsProblem opts0 -> complain why $> ExitFailure 2
      | not root, devs@(_ : _) <- devices (roTarget opts0) -> do
          traverse_ (\d -> complain ("hsed-jig: --target " <> d <> " needs root (sudo), hsed's own gate: the kext checks no "
                                      <> "privilege (design 9.3)")) devs
          pure (ExitFailure 3)
      | otherwise -> withProfile opts0 >>= \case
          Left e     -> complain e $> ExitFailure 1
          Right opts -> do
            let admits = driveAllowedFor opts chosen
            preflight opts admits >>= \case
              Left (code, why) -> complain why $> code
              Right notes      -> do
                t <- newTranscript
                stop <- newIORef Nothing
                withStopSignals stop $ tryAny (openTarget opts admits t) >>= \case
                  Left e           -> complain ("hsed-jig: --target " <> renderTarget (roTarget opts) <> ": " <> tshow e) $> ExitFailure 1
                  Right (Left e)   -> complain ("hsed-jig: --target " <> renderTarget (roTarget opts) <> ": " <> e) $> ExitFailure 1
                  Right (Right tg) -> runOn opts started chosen leftOut notes t stop tg `finally` closeTarget tg

-- | Run the selected cases on a target the caller opened and closes: what
--   'runJig' does once its target is open, without its gates (root, the
--   profile), which the caller answers for; a test's drive that is a replay
--   with what the IORegistry would say of a drive, or the simulator as a
--   drive. The transcript is the run's: the one a drive of the target
--   records into ("Hsed.Jig.Guard".'Hsed.Jig.Guard.guardDrive'). A key the
--   catalogue lacks is still exit 2.
runOpened :: RunOptions -> Transcript -> Target -> IO ExitCode
runOpened opts t tg = newIORef Nothing >>= \stop -> runOpenedStopping opts t stop tg

-- | 'runOpened' with the run's stop flag given ("Hsed.Jig.Script".@reStop@),
--   so that a test stops the run as a signal would, at a moment it
--   chooses.
runOpenedStopping :: RunOptions -> Transcript -> IORef (Maybe Text) -> Target -> IO ExitCode
runOpenedStopping opts t stop tg = case chosenCases opts of
  Left e                  -> complain e $> ExitFailure 2
  Right (chosen, leftOut) -> do
    started <- getCurrentTime
    runOn opts started chosen leftOut [] t stop tg

-- | Why @--recover@ cannot run as asked, if it cannot: it runs alone (no
--   case with it), on one end (no pair), and to its end (no
--   @--interrupt-in-session@, which would leave its own probe's session
--   open), design 9.3. A usage error.
recoverProblem :: RunOptions -> Maybe Text
recoverProblem opts
  | not (roRecover opts)      = Nothing
  | not (null (roTests opts)) = Just "hsed-jig: --recover runs alone: no --tests with it (design 9.3)"
  | paired (roTarget opts)    = Just "hsed-jig: --recover runs on one end, not on a pair (design 9.3)"
  | roInterruptInSession opts = Just ("hsed-jig: --recover runs to its end: no --interrupt-in-session with it, which would leave "
                                      <> "its own session open (design 9.3)")
  | otherwise                 = Nothing
  where
    paired = \case
      TPair _ _ -> True
      _         -> False

-- | The pre-flight of each drive the run may send to (its allowed list not
--   empty): what is mounted on it ("Hsed.Jig.Preflight".'preflightDrive',
--   by the whole disk's BSD name), the notes for the reports, or why the
--   run does not go ahead and its exit code: 3 for a drive mounted without
--   @--allow-mounted@ (design 9.3 item 4), 2 for a target that names no
--   whole disk. A run that sends Level 0 Discovery alone needs none.
preflight :: RunOptions -> [Allowed] -> IO (Either (ExitCode, Text) [Text])
preflight opts admits
  | null admits = pure (Right [])
  | otherwise   = fmap concat . sequenceA <$> traverse one (devices (roTarget opts))
  where
    one dev = case diskName dev of
      Left why   -> pure (Left (ExitFailure 2, "hsed-jig: --target " <> why))
      Right disk -> preflightDrive True (roAllowMounted opts) disk
                      <&> first (\why -> (ExitFailure 3, "hsed-jig: --target " <> dev <> ": " <> why))

-- | An action with SIGINT, SIGTERM and SIGHUP each caught once (design 9.3,
--   M4c): the signal's name written into the run's stop flag, unless one is
--   there, and said, the command in flight left to complete; a second of
--   the same signal takes its default action, which ends the process. The
--   handlers before them are put back after, whatever they were: base's
--   signal table's own entry for each (GHC's handler of SIGINT among them,
--   which System.Posix.Signals cannot give back, it being no 'Handler'),
--   swapped for the run's and swapped back ("GHC.Conc.Signal"), and the
--   signal's action as it was (the default, ignored, or handled: once, as
--   GHC's SIGINT handler is, or every time, as a 'Handler' says). On
--   Windows, none.
withStopSignals :: IORef (Maybe Text) -> IO a -> IO a
#if defined(mingw32_HOST_OS)
withStopSignals _ act = act
#else
withStopSignals stop act = bracket (traverse install signals) (traverse_ restore) (const act)
  where
    signals = [(sigINT, "SIGINT"), (sigTERM, "SIGTERM"), (sigHUP, "SIGHUP")]
    -- the table's entry before, whatever its type, swapped for the run's at once; then the signal's action set,
    -- installHandler saying whether it was the default or ignored (else it gives back the run's own, now in the table)
    install (sig, name) = do
      let ours = CatchOnce (caught name)
      table <- Conc.setHandler sig (Just (const (caught name), toDyn ours))
      before <- installHandler sig ours Nothing
      pure (sig, table, before)
    -- the action as it was, then the table's very entry
    restore (sig, table, before) = do
      void (installHandler sig (actionOf before table) Nothing)
      void (Conc.setHandler sig table)
    actionOf before table = case before of
      Default -> Default
      Ignore  -> Ignore
      _       -> case table of
        Just (_, dyn)
          | Just h <- fromDynamic dyn  -> h
          | Just io <- fromDynamic dyn -> CatchOnce io   -- base's own (GHC.TopHandler's SIGINT), handled once
        _ -> Default
    caught name = do
      atomicModifyIORef' stop (\was -> (was <|> Just name, ()))
      complain ("hsed-jig: " <> name <> ": the command in flight completes; nothing more is issued; a second " <> name <> " kills")
#endif

-- | The drives a target names.
devices :: TargetSpec -> [Text]
devices = \case
  TDevice d -> [d]
  TPair a b -> devices a <> devices b
  TWs _     -> []
  _         -> []

-- | Why the flags that name a drive cannot be taken, if they cannot:
--   @--expect-serial@ and @--allow-mounted@ given to a target with no drive
--   (no @\/dev@ end: the simulator, a @ws:\/\/@ hub, a replay), where they
--   would say nothing and a mistyped target would be quietly taken for a
--   run with no drive (the M5c review). @--capture-level0@ fails at the
--   open, where a replay's end has no drive to capture.
driveFlagsProblem :: RunOptions -> Maybe Text
driveFlagsProblem opts
  | not (null (devices (roTarget opts))) = Nothing
  | otherwise = listToMaybe
      [ "hsed-jig: " <> flag <> " names a drive, and --target " <> renderTarget (roTarget opts) <> " has none (a /dev target)"
      | (flag, given) <- [("--expect-serial", isJust (roExpectSerial opts)), ("--allow-mounted", roAllowMounted opts)], given ]

-- | Whether a target has a drive's end, its own or a replay.
hasDriveEnd :: TargetSpec -> Bool
hasDriveEnd = \case
  TSim      -> False
  TDevice _ -> True
  TReplay _ -> True
  TPair a b -> hasDriveEnd a || hasDriveEnd b
  TWs _     -> False

-- | Whether a target has the simulator's end: the simulator, in process or
--   over the LAN leg (written out: the wildcard's False would drop a ws://
--   run's Locking SP rule of the default selection and its parameters'
--   gate), or a pair with it.
hasSimEnd :: TargetSpec -> Bool
hasSimEnd = \case
  TSim      -> True
  TWs _     -> True
  TPair a b -> hasSimEnd a || hasSimEnd b
  _         -> False

-- | Why the run's Level 0 transfer length (@--level0-len@) is one a drive
--   cannot carry, if it is: more than 255 blocks of 512 bytes once rounded
--   up to blocks, the count byte of the kext's CDB ("Hsed.Transport.Cdb",
--   design 6.2), on a target with a drive's end (a replay stands in for the
--   drive command for command). A usage error, found before anything opens:
--   the transport would otherwise refuse the IF-RECV at run time and the
--   case fail on it.
level0LenProblem :: RunOptions -> Maybe Text
level0LenProblem opts
  | hasDriveEnd (roTarget opts), roundUp UnitBlocks512 len > most =
      Just ("hsed-jig: --level0-len " <> tshow len <> ": a drive or a replay carries at most " <> tshow most
            <> " bytes, 255 blocks of 512 (the count byte of the kext's CDB, design 6.2)")
  | otherwise = Nothing
  where
    len  = roLevel0Len opts
    most = mostCarried UnitBlocks512

-- | Why the simulator cannot run with the run's parameters, if the target
--   has a simulator's end and they are out of range
--   ("Hsed.Sim.Types".'simParamsProblems': the Locking SP's bounds among
--   them since M5a, which its fold refuses too, "Hsed.Sim.Preconfig"). A
--   usage error, found before anything opens; "Hsed.Jig.Target".@openTarget@
--   refuses them again for a caller that opens the target itself.
simParamsProblem :: RunOptions -> Maybe Text
simParamsProblem opts = case simParamsProblems (roSim opts) of
  problems@(_ : _) | hasSimEnd (roTarget opts) ->
    Just ("hsed-jig: --target " <> renderTarget (roTarget opts) <> ": the simulator's parameters: " <> T.intercalate "; " problems)
  _ -> Nothing

-- | The cases @--tests@ selects, in the catalogue's order: all of them for
--   none; a key naming a whole test, every case of it.
selectedCases :: [CaseKey] -> Either Text [Case]
selectedCases keys = do
  wanted <- if null keys then Right (map ceKey catalogue) else concat <$> traverse matching keys
  traverse caseOf [ ceKey e | e <- catalogue, ceKey e `elem` wanted ]
  where
    matching k = case [ ceKey e | e <- catalogue, k == ceKey e || whole k && ckTest k == ckTest (ceKey e) ] of
      [] -> Left ("hsed-jig: --tests " <> renderCaseKey k <> ": not a case of the catalogue ("
                  <> T.intercalate ", " (map (renderCaseKey . ceKey) catalogue) <> ")")
      ks -> Right ks
    whole k = isNothing (ckCase k) && isNothing (ckSteps k) && isNothing (ckVariant k)
    caseOf k = maybe (Left ("hsed-jig: the catalogue's " <> renderCaseKey k <> " has no case")) Right (L.find ((== k) . cKey) cases)

-- | The cases a run selects, in the catalogue's order ('selectedCases' of
--   its @--tests@), less, where it gives none, those the default leaves
--   out, each with why ('leftOutByDefault'), which the run notes before its
--   first case; a key the catalogue lacks, why.
chosenCases :: RunOptions -> Either Text ([Case], [(CaseKey, Text)])
chosenCases opts = do
  chosen <- selectedCases (roTests opts)
  let out = [ (cKey c, why) | null (roTests opts), c <- chosen, Just why <- [leftOutByDefault opts c] ]
  pure ([ c | c <- chosen, cKey c `notElem` map fst out ], out)

-- | Why a run with no @--tests@ leaves a case of the catalogue out, if it
--   does (Scott's answer Q1, design 11 M5b): on a target with the
--   simulator's end ('hasSimEnd'), a case whose Locking SP prerequisite the
--   simulator's Original Factory State (@--locking-ofs@, "Hsed.Sim.Params".@spLockingOFS@,
--   which no profile changes) does not meet: one that owns
--   LockingManufacturedInactive (ETC-20, Test Cases PDF 91) needing it
--   Manufactured-Inactive, and one that does not waive its section's
--   LockingManufactured (5.2, 6.2 and 7.2 item 2; PDF 29, 49 and 76) needing
--   it Manufactured; on a drive or a replay alone, none (a drive's life
--   cycle is not observable, and its cases skip on what they need). An
--   explicit @--tests@ runs what it names.
leftOutByDefault :: RunOptions -> Case -> Maybe Text
leftOutByDefault opts c
  | not (hasSimEnd (roTarget opts)) = Nothing
  | LockingManufacturedInactive `elem` cOwnPrereqs c, ofs /= ManufacturedInactive =
      Just ("its own prerequisite, the Locking SP Manufactured-Inactive (Test Cases ETC-20 prerequisite 1, PDF 91), which the simulator's "
            <> "Original Factory State does not meet (--locking-ofs " <> lockingOFSName ofs <> ")")
  | LockingManufactured `notElem` map wPrereq (cWaives c), ofs /= Manufactured =
      Just ("its section's common prerequisite, the Locking SP Manufactured (Test Cases " <> section <> " item " <> tshow item <> ", PDF "
            <> tshow page <> "), which the simulator's Original Factory State does not meet (--locking-ofs " <> lockingOFSName ofs <> ")")
  | otherwise = Nothing
  where
    ofs             = spLockingOFS (roSim opts)
    (section, page) = listAt (cSection c)
    item            = fromMaybe 2 (L.lookup LockingManufactured (common (cSection c)))

-- | The options with the profile read into the simulator's parameters.
withProfile :: RunOptions -> IO (Either Text RunOptions)
withProfile opts = case roProfile opts of
  Nothing   -> pure (Right opts)
  Just file -> loadProfile file <&> bimap (("hsed-jig: --profile " <> T.pack file <> ": ") <>)
                                          (\p -> opts { roSim = (roSim opts) { spProfile = Just p } })


-- * A drive's run

-- | The cases a drive runs at M4c, design 9.3's list: UCT-01, UCT-02,
--   UCT-03:1a-1c and SPF-02:1 (Level 0 Discovery, Properties, the MSID
--   read in a session to the Admin SP as Anybody, an IF-RECV with nothing
--   to return).
driveCases :: [CaseKey]
driveCases = map cKey [uct01, uct02, uct03steps1a1c, spf02case1]

-- | What a drive's base ComID, once bound, admits for a run of these cases
--   ("Hsed.Jig.Guard".'Hsed.Jig.Guard.driveBind'): with @--recover@, the
--   recovery's methods; where a case selected from 'driveCases' needs more
--   than Level 0 Discovery, the read-only methods (design 13.7), and
--   STACK_RESET where each case's SyncIdle and NoSessions are established by
--   one ("Hsed.Jig.Options".@roStackResetBeforeCases@, Scott's answer of
--   2026-11-02); otherwise nothing, the drive Level0Only for the run.
driveAllowedFor :: RunOptions -> [Case] -> [Allowed]
driveAllowedFor opts chosen
  | roRecover opts = recoverMethods
  | any needsMore [ c | c <- chosen, cKey c `elem` driveCases ] = readOnlyMethods <> [ AllowStackReset | roStackResetBeforeCases opts ]
  | otherwise = []
  where needsMore c = any (`elem` cNeeds c) [NeedIfRecv, NeedIfSend]

-- | The commands a drive's own transcript holds that a policy does not
--   admit, each decided again by the Guard ("Hsed.Jig.Guard".'decide'):
--   under the policy the drive ended with, what makes a run's exit 3, a
--   safety violation (design 9.3). Every command the Guard let through was
--   decided under the policy as it then stood, Level0Only before the bind
--   admitting nothing the allow list after it does not, so that a run's own
--   commands are never outside it; one is only if something reached the
--   drive past the Guard.
outsidePolicy :: Policy -> [TEntry] -> [Request]
outsidePolicy p es = [ q | Issued _ _ q _ <- es, isLeft (decide p q) ]

-- | A run's exit code from what it found (design 9.1, as M4c has it): 3, a
--   safety violation, where a drive was issued a command outside its policy
--   ('outsidePolicy', by the end's name); else 130 where the run was stopped
--   (why given: a signal, @--interrupt-in-session@), its reports written;
--   else 1 where anything failed; else 0.
runExit :: [(Text, Request)] -> Maybe Text -> Bool -> ExitCode
runExit outside stopped failed
  | not (null outside) = ExitFailure 3
  | isJust stopped     = ExitFailure 130
  | failed             = ExitFailure 1
  | otherwise          = ExitSuccess


-- * The ends of a run

-- | One end of a run: its name (its transport's role, @tper@, @drive@,
--   @drive-replay@, made unique by position, 'endNames': the name the
--   transcript's entries carry), what was opened for it where that says
--   more than the role (a @ws://@ simulator's URL), its environment, and
--   for a drive the drive behind the Guard, which holds the transcript of
--   what was issued to it and what the IORegistry says of it.
data End = End
  { endName   :: !Text
  , endOpened :: !(Maybe Text)
  , endEnv    :: !RunEnv
  , endDrive  :: !(Maybe GuardedDrive)
  }

-- | A target's ends, in order, each named by its role made unique by
--   position ('endNames'; design 13.33 (28)): the simulator's transport
--   recording into the run's transcript under the end's name, whether the
--   simulator was opened in process or over the LAN leg (its URL kept for
--   the reports); a drive as the target opened it, behind the Guard and
--   recording into the run's transcript and its own already, under its
--   role.
endsOf :: RunOptions -> Transcript -> LogFunc -> IORef (Maybe Text) -> Target -> IO [End]
endsOf opts t lf stop tg = traverse end (zip (endNames (map fst leaves)) (map snd leaves))
  where
    leaves = leavesOf tg
    leavesOf = \case
      SimTarget tr sim -> [(roleOf tr, Left (tr, sim))]
      DriveTarget g    -> [(roleOf (driveTransport g), Right g)]
      PairTarget a b   -> leavesOf a <> leavesOf b
    roleOf tr = dRole (describeTransport tr)
    end (name, leaf) = do
      channel <- newChannel
      ctx <- newIORef emptyRunCtx
      let env target = RunEnv target t channel ctx opts stop lf
      pure $ case leaf of
        Left (tr, sim) -> End name (openedOf sim) (env (SimTarget (recordingAs name t tr) sim)) Nothing
        Right g        -> End name Nothing (env (DriveTarget g)) (Just g)
    -- a simulator's URL where it was opened over the LAN leg; nothing more where the role says it all
    openedOf sim = case simOpened sim of
      InProcess -> Nothing
      OverWs u  -> Just (renderWsUrl u)

-- | Ends' names from their roles, in order, made unique by position
--   (design 13.33 (28)): an end the first of its role is named by the role;
--   each one after it by the role and its place among that role's ends,
--   @tper@, @tper-2@.
endNames :: [Text] -> [Text]
endNames roles = [ if k == 1 then r else r <> "-" <> tshow k | (i, r) <- zip [1 ..] roles, let k = length (filter (== r) (take i roles)) ]

-- | An end as the reports record it among the targets: what its transport
--   says it is, its name, and what was opened for it.
endRecord :: End -> Maybe EndRecord
endRecord e = (\d -> EndRecord d (endName e) (endOpened e)) <$> case reTarget (endEnv e) of
  SimTarget tr _ -> Just (describeTransport tr)
  DriveTarget g  -> Just (describeTransport (driveTransport g))
  PairTarget _ _ -> Nothing

-- | Run the cases on the ends (with @--recover@, the recovery on the one
--   end in their place; the cases the default left out noted first), then,
--   unless the run was stopped, hear what each end's simulator says
--   happened to it during the last case ("Hsed.Jig.Script".'heedNotices',
--   as before each case), compare and
--   capture Level 0; count the IF-SENDs each drive was issued, gather each
--   drive's policy, the commands it was issued outside it and those the
--   Guard refused (a problem of the run), why the run was stopped and any
--   session it left open, the pre-flight's notes, each end's Properties with
--   the host's and, for a pair, the two compared, and the secrets the ends
--   read; write the reports; the exit code ('runExit').
runOn :: RunOptions -> UTCTime -> [Case] -> [(CaseKey, Text)] -> [Text] -> Transcript -> IORef (Maybe Text) -> Target -> IO ExitCode
runOn opts started chosen leftOut preflightNotes t stop tg = do
  logOptions <- logOptionsHandle stderr False
  withLogFunc (setLogMinLevel LevelInfo logOptions) $ \lf -> do
    ends <- endsOf opts t lf stop tg
    unless (null leftOut || roRecover opts) $
      record t (Note 0 ("left out of the cases a run with no --tests selects, each on its Locking SP prerequisite (Scott's answer Q1, design 11 "
                        <> "M5b): " <> T.intercalate "; " [ renderCaseKey k <> ": " <> why | (k, why) <- leftOut ]))
    let named = length ends > 1
    (results, recovered) <-
      if roRecover opts
        then (,) [] . Just <$> recoverOn ends
        else (\rs -> (rs, Nothing)) <$> for chosen (\c -> joined named c <$> for ends (\e -> (,) (endName e) <$> runCase e c))
    -- what the simulator says happened to it during the last case, heard before the closing Level 0 as before each case
    unless (roRecover opts) $ void (whileRunning stop (for_ ends (\e -> runRIO (endEnv e) heedNotices)))
    pairing <- fmap join . whileRunning stop $ case ends of
      a : b : _ | not (roRecover opts) -> Just <$> (liftA2 pairLevel0 <$> endLevel0 a <*> endLevel0 b)
      _                                -> pure Nothing
    problems <- fromMaybe [] <$> whileRunning stop (capture opts ends)
    contexts <- for ends (readIORef . reCtx . endEnv)
    stopped <- readIORef stop
    let level0s = [ (endName e, l0) | (e, ctx) <- zip ends contexts, Just l0 <- [rcLevel0 ctx] ]
        secrets = L.nub (filter (not . B.null) (concatMap rcSecrets contexts))
        drives  = [ (endName e, g) | e <- ends, Just g <- [endDrive e] ]
    policies <- for drives (traverse drivePolicy)
    issued <- for drives (traverse driveIssued)
    transcript <- entries t
    let ifSends    = sum (map (ifSendsIssued . snd) issued)
        outside    = concat [ map ((,) n) (outsidePolicy p es) | ((n, p), (_, es)) <- zip policies issued ]
        unread     = [ "the " <> endName e <> " may hold a session on ComID " <> hex c <> " that a StartSession opened and the host "
                       <> "could not read (" <> why <> "), which no STACK_RESET has cleared: hsed-jig --recover"
                     | (e, ctx) <- zip ends contexts, Just (c, why) <- [rcSessionUnread ctx] ]
                     <> [ "the " <> endName e <> " may hold session TSN " <> tshow (shTSN h) <> " on ComID " <> hex (shComId h)
                          <> ", its End of Session unanswered or not sent, and no STACK_RESET answering Success since: "
                          <> "hsed-jig --recover"
                        | isNothing stopped, (e, ctx) <- zip ends contexts, Just h <- [rcSession ctx] ]
        negotiated = [ (endName e, sent, answer) | (e, ctx) <- zip ends contexts, Just (sent, answer) <- [rcNegotiated ctx] ]
        pairProps  = case contexts of
          a : b : _ | not (roRecover opts), isNothing stopped -> Just (pairProperties (rcNegotiated a) (rcNegotiated b))
          _                                                   -> Nothing
        blocked      = length [ () | Blocked {} <- transcript ]
        pairProblems = [ "pair: " <> e | Just (Left e) <- [pairing] ]
        allProblems  = pairProblems <> problems
                       <> [ "the Guard refused " <> tshow blocked <> " command(s) (each Blocked in the transcript)" | blocked > 0 ]
                       <> unread
        interrupted  = (<> mconcat (map leftOpen (mapMaybe rcSession contexts))) <$> stopped
        failed = any (not . passed) results || any pairFails [ r | Just (Right r) <- [pairing] ]
                 || any propertiesFail pairProps || any isLeft recovered || not (null allProblems)
        exit = runExit outside interrupted failed
        rr = RunResult
          { rrOptions        = opts
          , rrVersion        = hsedVersion
          , rrStarted        = started
          , rrTargets        = mapMaybe endRecord ends
          , rrCases          = results
          , rrTranscript     = transcript
          , rrLevel0         = level0s
          , rrPair           = either (const Nothing) Just =<< pairing
          , rrIfSends        = ifSends
          , rrSecrets        = secrets
          , rrProblems       = allProblems
          , rrExit           = exit
          , rrPolicies       = policies
          , rrOutsidePolicy  = outside
          , rrBlocked        = blocked
          , rrInterrupted    = interrupted
          , rrPreflight      = preflightNotes
          , rrRecover        = recovered
          , rrProperties     = negotiated
          , rrPairProperties = pairProps
          }
    traverse_ complain allProblems
    unless (null outside) $
      complain ("hsed-jig: " <> tshow (length outside) <> " command(s) issued on a drive outside its policy: a safety violation "
                <> "(design 9.3)")
    for_ interrupted $ \why -> complain ("hsed-jig: the run was stopped: " <> why)
    tryAny (writeReports (roReportDir opts) rr) >>= \case
      Right paths -> exit <$ traverse_ say (summary rr paths)
      Left e      -> do
        traverse_ say (summary rr [])
        complain ("hsed-jig: the reports were not written: " <> tshow e)
        pure (if exit == ExitSuccess then ExitFailure 1 else exit)
  where
    leftOpen h = "; session TSN " <> tshow (shTSN h) <> " on ComID " <> hex (shComId h) <> " left open (End of Session not sent): "
                 <> "hsed-jig --recover"

-- | What the run does after its cases that may issue commands (Level 0 for
--   the pair, the capture), unless it was stopped: nothing once its stop
--   flag holds why, and nothing more where a stop comes in the middle
--   ('Interrupted' before a command).
whileRunning :: IORef (Maybe Text) -> IO a -> IO (Maybe a)
whileRunning stop act = readIORef stop >>= \case
  Just _  -> pure Nothing
  Nothing -> (Just <$> act) `catch` \(Interrupted _) -> pure Nothing

-- | Whether a case passed: its verdict, and no extension failed.
passed :: CaseResult -> Bool
passed cr = crVerdict cr == Pass && null [ () | (_, Fail _) <- crExtensions cr ]

-- | @--recover@ on a target's one end ("Hsed.Jig.Recover"): what it did, or
--   why it did not recover, a script stopped short (a bind refused, the run
--   stopped) saying why; a target of two ends is not recovered.
recoverOn :: [End] -> IO (Either Text Text)
recoverOn = \case
  [e] -> runRIO (endEnv e) $ do
    outcome <- newIORef (Left "--recover: no outcome")
    (attempt (Pass <$ (recoverScript >>= writeIORef outcome)) >>= \case
       Pass -> readIORef outcome
       v    -> pure (Left ("--recover: " <> verdictText v)))
      `catch` \(Interrupted why) -> pure (Left ("--recover: interrupted: " <> why))
  ends -> pure (Left ("--recover runs on a target of one end, not " <> tshow (length ends) <> " (design 9.3)"))

-- | A case's results on a run's ends as one: alone, as it is; on a pair, a
--   pass where every end passed, else the failures (or the first other
--   verdict) each with its end's name; the waivers every end made, and
--   those of some ends with their names; the extensions and the observations
--   with their end's; each end's own verdict beside the joined one; and the
--   prerequisites established where every end established them.
joined :: Bool -> Case -> [(Text, CaseResult)] -> CaseResult
joined named c rs
  | not named, [(_, r)] <- rs = r
  | otherwise = CaseResult (cKey c) verdict waivers extensions observations [ (n, crVerdict r) | (n, r) <- rs ] (all (crEstablished . snd) rs)
  where
    verdict = case [ (n, crVerdict r) | (n, r) <- rs, crVerdict r /= Pass ] of
      []  -> Pass
      bad | any (isFail . snd) bad -> Fail (concat [ map ((n <> ": ") <>) (reasons v) | (n, v) <- bad ])
      (n, v) : _ -> case v of
        Skipped r      -> Skipped (n <> ": " <> r)
        Unobservable r -> Unobservable (n <> ": " <> r)
        _              -> v
    waivers = [ if length holders == length rs then w else w { wReason = T.intercalate ", " holders <> ": " <> wReason w }
              | w <- L.nub (concatMap (crWaivers . snd) rs), let holders = [ n | (n, r) <- rs, w `elem` crWaivers r ] ]
    extensions = [ (n <> ": " <> x, v) | (n, r) <- rs, (x, v) <- crExtensions r ]
    observations = [ (n <> ": " <> k, v) | (n, r) <- rs, (k, v) <- crObservations r ]
    isFail = \case
      Fail _ -> True
      _      -> False
    reasons = \case
      Fail ws        -> ws
      Skipped w      -> [w]
      Unobservable w -> [w]
      Pass           -> []


-- * A case on an end

-- | A case on one end, what the end's simulator says happened to it since
--   it was last asked heard first (a hub that restarted, broadcasts
--   missed, another client's reset: each noted, the run's session, an
--   unread StartSession and the host's properties forgotten as a power
--   cycle forgets them, "Hsed.Jig.Script".'heedNotices'; the restart hook,
--   M5c), so that the case runs on what the TPer now holds: skipped if the
--   end lacks what it needs or a
--   prerequisite is unmet, its extensions skipped with that reason and its
--   prerequisites not reached; else run to its verdict, then its extensions
--   on the simulator alone, unless the verdict is Skipped or Unobservable,
--   when each extension is Skipped with the case's reason (design 9.1,
--   13.29 (b)); what the case observed goes into its result, what each
--   extension observed under the extension's name, and the next case starts
--   with none.
--   A run stopped before the case skips it, not run; one stopped while the
--   case runs ('Interrupted', thrown before a command) skips it as
--   interrupted, its extensions not run either. One stopped in an extension
--   keeps the case's verdict and what it observed: that extension is
--   skipped as interrupted and each after it not run, and the run stops
--   after the case.
runCase :: End -> Case -> IO CaseResult
runCase e c = runRIO (endEnv e) $ asks reStop >>= readIORef >>= \case
  Just why -> pure (notRun (Skipped ("not run: the run was interrupted (" <> why <> ")")))
  Nothing  -> ran `catch` \(Interrupted why) -> pure (notRun (Skipped ("interrupted: " <> why)))
  where
    ran = do
      void heedNotices
      let heading = renderCaseKey (cKey c) <> " on the " <> endName e
      logInfo (display heading)
      note heading
      tg <- asks reTarget
      _ <- observed
      case lacking tg c of
        Just why -> pure (notRun (Skipped why))
        Nothing  -> prerequisites c >>= \case
          Left (Unmet why)  -> pure (notRun (Skipped ("prerequisite unmet: " <> why)))
          Left (Unread why) -> pure (notRun (Fail ["prerequisite not established: " <> why]))
          Right ws -> do
            v <- attempt (cRun c)
            own <- observed
            xs <- extensionsOn v tg Nothing (cExtensions c)
            pure (CaseResult (cKey c) v ws (map fst xs) (own <> concatMap snd xs) [] True)
    -- the extensions in order, each given what changed under the case, if anything did: on the simulator, what it says
    -- happened to it is heard before each (heedNotices), and once something is heard (a hub restarted, broadcasts
    -- missed, another client's reset) this extension and those after it are not run, since they would observe a TPer
    -- the case did not run on (the M5c review)
    extensionsOn v tg changed = \case
      [] -> pure []
      (name, s) : rest -> case (v, tg) of
        (Skipped _, _)      -> (((name, unrun v), []) :) <$> extensionsOn v tg changed rest
        (Unobservable _, _) -> (((name, unrun v), []) :) <$> extensionsOn v tg changed rest
        (_, SimTarget _ _)  -> asks reStop >>= readIORef >>= \case
          Just why -> (((name, Skipped ("not run: the run was interrupted (" <> why <> ")")), []) :) <$> extensionsOn v tg changed rest
          Nothing  -> maybe heedNotices (pure . pure) changed >>= \case
            notice : _ -> (((name, underCase notice), []) :) <$> extensionsOn v tg (Just notice) rest
            []         -> (:) <$> (extension name s `catch` \(Interrupted why) -> ((name, Skipped ("interrupted: " <> why)), []) <$ observed)
                              <*> extensionsOn v tg Nothing rest
        _                   -> (((name, Skipped "the simulator's alone"), []) :) <$> extensionsOn v tg changed rest
    underCase notice = Skipped ("not run: the simulator changed under the case (" <> notice <> ")")
    -- an extension run, its verdict and what it observed, under its name
    extension name s = do
      note (renderCaseKey (cKey c) <> " extension on the " <> endName e <> ", " <> name)
      xv <- attempt s
      obs <- observed
      pure ((name, xv), map (first ((name <> ": ") <>)) obs)
    -- an extension not run, the case's verdict its reason
    unrun v = Skipped ("not run, the case " <> verdictText v)
    -- a case not run at all: its extensions not run either, its prerequisites not reached
    notRun v = CaseResult (cKey c) v [] [ (name, unrun v) | (name, _) <- cExtensions c ] [] [] False
    -- what the case or the extension running has observed, taken; the next starts with none
    observed = asks reCtx >>= \ref -> atomicModifyIORef' ref (\x -> (x { rcObservations = [] }, rcObservations x))

-- | Why a target cannot run a case, if it cannot. The simulator runs
--   every case. A drive answers Level 0 Discovery; IF-RECV beyond it and the
--   allow list's IF-SENDs where the run binds its base ComID with methods to
--   admit ('driveAllowedFor': a case it selected from design 9.3's drive
--   list needs them), a replay never (it answers Level 0 Discovery alone,
--   "Hsed.Transport.Replay", whatever the run admits); never a write to the
--   TPer's state (since M5b: drives stay read-only until M6), an error path
--   nor what the simulator alone can run; and, its needs met, design 9.3's
--   drive cases alone ('driveCases'). A write is the first a drive or a
--   replay is found to lack.
lacking :: Target -> Case -> Maybe Text
lacking tg c = listToMaybe ([ why | n <- [NeedWrite, NeedSimOnly, NeedErrorPath, NeedIfSend, NeedSimControl, NeedIfRecv, NeedLevel0]
                                  , n `elem` cNeeds c, Just why <- [lack n] ]
                            <> offTheList)
  where
    lack n = case tg of
      SimTarget _ _   -> Nothing
      DriveTarget g   -> case n of
        NeedWrite      -> Just "writes: drives stay read-only until M6 (design 9.3; 13.34 (8))"
        NeedLevel0     -> Nothing
        NeedIfRecv     -> beyondLevel0 g "needs IF-RECV beyond Level 0 Discovery"
        NeedIfSend     -> beyondLevel0 g "needs IF-SEND"
        NeedErrorPath  -> Just "needs an IF-SEND off the allow list, an error path: hardware error paths stay off a drive (design 9.3 item 6 and 13.8, Scott's)"
        NeedSimOnly    -> Just "the simulator's alone: no drive policy admits it (design 9.3)"
        NeedSimControl -> Just "needs the simulator's control"
      PairTarget _ _  -> Just "a pair's ends run a case one at a time"
    -- a replay first: it answers Level 0 alone whatever the run admits, its allowed list the run's
    beyondLevel0 g need
      | isNothing (driveInfo g) = Just (need <> ": a replay answers Level 0 Discovery alone (Hsed.Transport.Replay)")
      | null (driveAllowed g)   = Just (need <> ": this run admits Level 0 Discovery alone on the drive: no case it selected "
                                        <> "from design 9.3's drive list needs more")
      | otherwise               = Nothing
    offTheList = case tg of
      DriveTarget _ | cKey c `notElem` driveCases ->
        ["not among the cases a drive runs (design 9.3, its list unchanged since M4c: " <> T.intercalate ", " (map renderCaseKey driveCases) <> ")"]
      _ -> []

-- | Why a prerequisite was not established: unmet, the state it reads read
--   and not meeting it (the case skipped); or unread, the simulator's state
--   it reads not read at all (the case failed: its view refused or its
--   connection lost, a transport's failure as in a case's body, never a
--   claim about the TPer's state; the M5c review).
data Unestablished
  = Unmet !Text
  | Unread !Text

-- | A case's prerequisites in its section's order, then its own (Test
--   Cases 3.2.3, PDF 25: in sequential order): each waived, or established
--   or checked on this end; the waivers, or why one is not established.
prerequisites :: Case -> Script (Either Unestablished [Waiver])
prerequisites c = go (map fst (common (cSection c)) <> cOwnPrereqs c)
  where
    go [] = pure (Right [])
    go (p : rest) = case L.find ((== p) . wPrereq) (cWaives c) of
      Just w  -> fmap (w :) <$> go rest
      Nothing -> establish c p >>= \case
        Left why -> pure (Left why)
        Right mw -> fmap (maybe id (:) mw) <$> go rest

-- | A prerequisite on this end: met (established if need be), waived as
--   unobservable or not needed, or why not. On the simulator, its state
--   read through its view, in process or over the LAN leg alike
--   ("Hsed.Sim.View"'s queries; M5c, Option A), a view that does not answer
--   leaving the prerequisite unread (its name, @the simulator's state could
--   not be read:@ and why; never in process), which fails the case rather
--   than skip it: SyncIdle drains every ComID not in Awaiting IF-SEND
--   ('ComIds'); NoSessions sends STACK_RESET on every ComID a session holds
--   ('ComIds', Associated; Core 3.3.4.7.5 item 1, PDF 65: every session on
--   it aborted, no CloseSession prepared), noted, and reads the ComIDs
--   again; LockingManufactured reads the Locking SP's life cycle, its row
--   of the Admin SP's SP table ('LifeCycleOf'), which its Original Factory
--   State seeds, and LockingManufacturedInactive, a case's own (since M5b,
--   ETC-20's), the same, met where it is Manufactured-Inactive;
--   CredentialsKnown is vacuous for a case that authenticates no authority
--   ('authenticating'), and for one that does reads each authority's PIN
--   from the state, in order ('CredentialOf'), a secret of the run;
--   HostChallengeIsCurrentPin likewise (a case's challenge is that PIN, or a
--   variation it makes on purpose); ReadWriteSessions is met, every session
--   the jig opens being read-write (Test Cases 4.1 e, PDF 26).
--
--   On a drive (M4c), for a case that needs Level 0 Discovery alone,
--   SyncIdle and NoSessions are waived as not needed
--   ("Hsed.Jig.Prereq".'level0OutsideSync'). For any other, under 'attempt'
--   (a script stopped short, a bind refused, is unmet; a stopped run goes
--   on), where the drive takes STACK_RESET ('admitsStackReset':
--   @roStackResetBeforeCases@, Scott's decision of 2026-11-02): the first of
--   SyncIdle and NoSessions in the case's own set binds the base ComID
--   ('baseComId') and sends one STACK_RESET on it, which establishes both
--   (every session on the ComID aborted, its stack in its initial state, so
--   in Awaiting IF-SEND, its properties back to their defaults: Core
--   3.3.4.7.5 items 1, 4, 6 and 7, PDF 65; 3.3.10.6, PDF 85-86), noted, the
--   other met by it, noted; a reset that fails is unmet. Where the drive
--   takes none: SyncIdle is one IF-RECV on the base, which must be Table
--   45's (0, 0, 0), nothing to return (3.3.10.5 S1 b, PDF 84: a ComPacket
--   naming another ComID still idle, noted, SPF-02:1 judging the ComID), any
--   other answer masked as a secret and unmet ('maskAnswersSince');
--   NoSessions is unmet where this run left a session open, and otherwise
--   waived as unobservable ("Hsed.Jig.Prereq".'sessionsUnobservable').
--   CredentialsKnown and HostChallengeIsCurrentPin are vacuous for a case
--   that authenticates no authority, as on the simulator, and unmet for one
--   that does; ReadWriteSessions is met; LockingManufactured, which every
--   drive case waives, and LockingManufacturedInactive are not observable.
establish :: Case -> Prereq -> Script (Either Unestablished (Maybe Waiver))
establish c p = asks reTarget >>= \case
  SimTarget _ sim -> onSim sim
  DriveTarget _
    | p `elem` [SyncIdle, NoSessions], all (== NeedLevel0) (cNeeds c) -> pure (Right (Just (Waiver p level0OutsideSync)))
    | otherwise -> first Unmet <$> settled onDrive
  PairTarget _ _ -> pure (Left (Unmet "a pair's ends run a case one at a time"))
  where
    authorities = fromMaybe [] (L.lookup (cKey c) authenticating)
    met = pure (Right Nothing)
    credentialsVacuous = "CredentialsKnown: vacuous, the case authenticates no authority (a session as Anybody needs no credential, Core 5.3.4.1.2.1, PDF 211)"
    challengeVacuous   = "HostChallengeIsCurrentPin: vacuous, the case sends no HostChallenge"
    readWrite          = "ReadWriteSessions: every session the jig opens is read-write (StartSession's Write True, Test Cases 4.1 e, PDF 26)"
    onSim sim = case p of
      SyncIdle -> viewing sim ComIds $ \comIds -> do
        let busy = [ cvComId v | v <- comIds, cvSync v /= S1 ]
        unless (null busy) (note ("SyncIdle: draining " <> T.unwords (map hex busy)))
        attempt (Pass <$ traverse_ drain busy) <&> \case
          Pass -> Right Nothing
          v    -> Left (Unmet ("SyncIdle: " <> verdictText v))
      NoSessions -> viewing sim ComIds $ \comIds -> case holding comIds of
        []   -> met
        held -> do
          note ("NoSessions: sessions hold ComIDs " <> T.unwords (map hex held) <> ": STACK_RESET on each (Core 3.3.4.7.5, PDF 65)")
          for_ held $ \comid -> stackReset comid >>= \outcome ->
            note ("NoSessions: STACK_RESET on " <> hex comid <> ": " <> either id (("Success/Failure " <>) . tshow) outcome)
          asks reCtx >>= \ref -> modifyIORef' ref (\x -> x { rcSession = Nothing, rcSessionUnread = Nothing })
          viewing sim ComIds $ \again -> pure $ case holding again of
            []    -> Right Nothing
            still -> Left (Unmet ("NoSessions: sessions still hold ComIDs " <> T.unwords (map hex still) <> " after STACK_RESET"))
      LockingManufactured -> viewing sim (LifeCycleOf uSP_Locking) $ \life -> if life == Just (lifeCycleValue Manufactured)
        then met
        else pure (Left (Unmet ("LockingManufactured: the simulator's Locking SP is Manufactured-Inactive, its Original Factory State; "
                                <> "--locking-ofs manufactured folds it Manufactured (design 8.3, 9.1; M6's Activate)")))
      LockingManufacturedInactive -> viewing sim (LifeCycleOf uSP_Locking) $ \life -> if life == Just (lifeCycleValue ManufacturedInactive)
        then met
        else pure (Left (Unmet "LockingManufacturedInactive: the simulator's Locking SP is Manufactured (--locking-ofs manufactured)"))
      CredentialsKnown
        | null authorities -> Right Nothing <$ note credentialsVacuous
        | otherwise        -> firstUnmet (map (credentialKnown sim) authorities)
      HostChallengeIsCurrentPin
        | null authorities -> Right Nothing <$ note challengeVacuous
        | otherwise        -> Right Nothing <$ for_ authorities (\(name, sp, _) ->
            note ("HostChallengeIsCurrentPin: " <> name <> "'s current PIN is read from the simulator's state; a challenge other than it "
                  <> "is the case's own doing" <> ofSP sp))
      ReadWriteSessions -> Right Nothing <$ note readWrite
    -- the prerequisite on a query of the simulator's state through its view ("Hsed.Sim.View"), unread where the view does
    -- not answer (a hub that refused the query, its connection lost; never in process), saying why
    viewing :: Simulator -> SimQuery a -> (a -> Script (Either Unestablished (Maybe Waiver))) -> Script (Either Unestablished (Maybe Waiver))
    viewing sim q k = liftIO (peekView (simView sim) q) >>= either (pure . Left . Unread . unread) k
    unread why = tshow p <> ": the simulator's state could not be read: " <> why
    -- an authority's credential read from the simulator's state, a secret of the run, noted; or why not: the SP named, the
    -- Admin SP's notes as at M5a, another SP's saying whose C_PIN (since M5b, the Locking SP's Admin1); the view's failure
    credentialKnown sim (name, sp, u) = liftIO (peekView (simView sim) (CredentialOf sp u)) >>= \case
      Left why        -> pure (Just (Unread (unread why)))
      Right Nothing   -> pure (Just (Unmet ("CredentialsKnown: the simulator's " <> spName sp <> " holds no credential for " <> name)))
      Right (Just cr) -> do
        asks reCtx >>= \ref -> modifyIORef' ref (\x -> x { rcSecrets = L.nub (rcSecrets x <> [crPIN cr]) })
        Nothing <$ note ("CredentialsKnown: " <> name <> "'s PIN read from the simulator's state, " <> tshow (B.length (crPIN cr))
                         <> " bytes, TryLimit " <> tshow (crTryLimit cr) <> ", Tries " <> tshow (crTries cr) <> " (masked in the reports unless --show-msid)"
                         <> ofSP sp)
    -- whose C_PIN, for an SP other than the Admin SP (whose notes M5a's runs hold)
    ofSP sp = if sp == uSP_Admin then "" else " (the " <> spName sp <> "'s C_PIN)"
    -- each in order, the first unmet ending them
    firstUnmet = foldr (\s rest -> s >>= maybe rest (pure . Left)) met
    -- the ComIDs sessions hold
    holding comIds = [ cvComId v | v <- comIds, cvLife v == Just Associated ]
    -- a drive's prerequisite under 'attempt': a script stopped short is unmet, saying why; Interrupted goes on
    settled s = do
      out <- newIORef (Left (tshow p <> ": no outcome"))
      attempt (Pass <$ (s >>= writeIORef out)) >>= \case
        Pass           -> readIORef out
        Fail rs        -> pure (Left (tshow p <> ": " <> T.intercalate "; " rs))
        Skipped r      -> pure (Left (tshow p <> ": " <> r))
        Unobservable r -> pure (Left (tshow p <> ": " <> r))
    onDrive = case p of
      SyncIdle   -> admitsStackReset >>= \admits -> if admits then reset else probe
      NoSessions -> admitsStackReset >>= \admits -> if admits then reset else sessionsNow
      CredentialsKnown
        | null authorities -> Right Nothing <$ note credentialsVacuous
        | otherwise        -> pure (Left "CredentialsKnown: no authority is authenticated on a drive (design 9.3)")
      HostChallengeIsCurrentPin
        | null authorities -> Right Nothing <$ note challengeVacuous
        | otherwise        -> pure (Left "HostChallengeIsCurrentPin: no authority is authenticated on a drive (design 9.3)")
      ReadWriteSessions -> Right Nothing <$ note readWrite
      LockingManufactured -> pure (Left "LockingManufactured: not observable on a drive (design 9.1): every case a drive runs waives it")
      LockingManufacturedInactive -> pure (Left "LockingManufacturedInactive: not observable on a drive (design 9.1)")
    -- SyncIdle and NoSessions as the case's own set has them, in order: what one STACK_RESET establishes
    resettable = [ q | (q, _) <- common (cSection c), q `elem` [SyncIdle, NoSessions], q `notElem` map wPrereq (cWaives c) ]
    reset
      | listToMaybe resettable == Just p = do
          base <- baseComId
          let named = T.intercalate " and " (map tshow resettable) <> ": STACK_RESET on " <> hex base <> ": "
          stackReset base >>= \case
            Right 0 -> do
              asks reCtx >>= \ref -> modifyIORef' ref (\x -> x { rcSession = Nothing, rcSessionUnread = Nothing })
              Right Nothing <$ note (named <> "Success/Failure 0: every session on it aborted, its stack in its initial state, its host "
                                     <> "properties back to their defaults (Core 3.3.4.7.5 items 1, 4, 6 and 7, PDF 65)")
            Right n  -> pure (Left (named <> "Success/Failure " <> tshow n <> ": the stack not reset (Core Table 36, PDF 66)"))
            Left why -> pure (Left (named <> why))
      | otherwise = do
          base <- baseComId
          Right Nothing <$ note (tshow p <> ": met by the same STACK_RESET on " <> hex base <> ": " <> case p of
                                   NoSessions -> "every session on it aborted (Core 3.3.4.7.5 item 1, PDF 65)"
                                   _          -> "the ComID in Awaiting IF-SEND (Core 3.3.10.6, PDF 85-86)")
    probe = do
      base <- baseComId
      mark <- transcriptMark
      r <- ifRecvOn base
      let busy answer = do
            maskAnswersSince mark "SyncIdle"
            pure (Left ("SyncIdle: ComID " <> hex base <> " answered " <> answer <> ", not idle: an exchange left from before this run; "
                        <> "hsed-jig --recover resets it (STACK_RESET, Core 3.3.4.7.5, PDF 65)"))
      case (status r, t45Of r) of
        (Good, Right (0, 0, 0)) -> case t45On base r of
          Right _  -> met
          Left why -> Right Nothing <$ note ("SyncIdle: ComID " <> hex base <> " answered (0, 0, 0), nothing to return, in " <> why
                                             <> ": idle; SPF-02:1 judges the ComID (Core 3.3.10.5 S1 b, PDF 84)")
        (Good, Right (l, o, m)) -> busy ("(" <> tshow l <> ", " <> tshow o <> ", " <> tshow m <> ")")
        (Good, Left why)        -> busy why
        (st, _)                 -> pure (Left ("SyncIdle: the IF-RECV on " <> hex base <> " answered " <> describeStatus st))
    sessionsNow = asks reCtx >>= readIORef <&> \ctx -> case (rcSession ctx, rcSessionUnread ctx) of
      (Just h, _)          -> Left ("NoSessions: this run left session TSN " <> tshow (shTSN h) <> " open on " <> hex (shComId h)
                                    <> " (its End of Session unanswered): hsed-jig --recover")
      (_, Just (cid, why)) -> Left ("NoSessions: this run may have left a session open on " <> hex cid <> ", a StartSession's answer "
                                    <> "unread (" <> why <> "): hsed-jig --recover")
      _                    -> Right (Just (Waiver NoSessions sessionsUnobservable))

-- | Add a note to the run's transcript.
note :: Text -> Script ()
note text = asks reTranscript >>= \t -> liftIO (record t (Note 0 text))


-- * Level 0 after the cases

-- | An end's Level 0 response: the one discovered, else discovered now,
--   unless a drive was already asked for it (a failed kext command is never
--   repeated: it leaks a SCSI task in the kext).
endLevel0 :: End -> IO (Either Text Level0)
endLevel0 e = readIORef (reCtx (endEnv e)) >>= \ctx -> case rcLevel0 ctx of
  Just l0 -> pure (Right l0)
  Nothing -> do
    asked <- maybe (pure False) (fmap (any level0Issued) . driveIssued) (endDrive e)
    if asked
      then pure (Left (endName e <> ": its Level 0 Discovery did not succeed, and a drive is not asked again"))
      else runRIO (endEnv e) $ do
        note ("Level 0 Discovery on the " <> endName e <> ", after the cases")
        attempt (Pass <$ discover) >>= \case
          Pass -> maybe (Left (endName e <> ": no Level 0 response")) Right . rcLevel0 <$> (asks reCtx >>= readIORef)
          v    -> pure (Left (endName e <> "'s Level 0 Discovery: " <> verdictText v))
  where
    level0Issued = \case
      Issued _ _ q _ -> isLevel0 q
      _              -> False

-- | The capture @--capture-level0@ asks for: the drive's last Level 0
--   response that succeeded (discovered now if it was never asked for
--   one), its CDB and what the IORegistry says of the drive; what went
--   wrong.
capture :: RunOptions -> [End] -> IO [Text]
capture opts ends = case roCapture opts of
  Nothing   -> pure []
  Just file -> case [ (e, g, di) | e <- ends, Just g <- [endDrive e], Just di <- [driveInfo g] ] of
    [] -> pure ["--capture-level0 " <> T.pack file <> ": no drive to capture (the simulator and a replay have none)"]
    (e, g, di) : _ -> do
      found <- lastLevel0 g >>= \case
        Just x  -> pure (Just x)
        Nothing -> endLevel0 e >> lastLevel0 g
      case found of
        Nothing     -> pure ["--capture-level0 " <> T.pack file <> ": the drive answered no Level 0 Discovery that succeeded"]
        Just (q, r) -> case level0Cdb (diInterface di) (lenBytes q `div` transferLength UnitBlocks512 1) of
          Left why  -> pure ["--capture-level0 " <> T.pack file <> ": no CDB for the IF-RECV: " <> why]
          Right cdb -> tryAny (captureLevel0 file di cdb r) <&> either (\ex -> ["--capture-level0 " <> T.pack file <> ": " <> tshow ex]) (const [])
  where
    lastLevel0 g = driveIssued g <&> \es -> L.lastMaybe [ (q, r) | Issued _ _ q r <- es, isLevel0 q, status r == Good ]

-- | Whether a command is Level 0 Discovery's IF-RECV.
isLevel0 :: Request -> Bool
isLevel0 q = dir q == IfRecv && proto q == comPacketProtocol && comId q == level0ComId


-- * Printing

say :: Text -> IO ()
say t = hPutBuilder stdout (getUtf8Builder (display t <> "\n"))

complain :: Text -> IO ()
complain t = hPutBuilder stderr (getUtf8Builder (display t <> "\n"))
