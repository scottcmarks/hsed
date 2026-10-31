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

@hsed-jig run@ (design 9.2): refuse a drive target without the serial it
must have (a usage error) or without root (hsed's own gate, design 9.3),
read the profile, open the target, establish or check
each case's prerequisites and run it, its waivers printed, and its
extensions on the simulator; on a drive, capture its Level 0 response when
asked, and check that no IF-SEND was issued; write the reports. The exit
code: 0 every selected case passed (waived prerequisites printed), no
extension failed, a pair found only allowed differences (or unchecked ones)
and the capture asked for was written; 1 a failure: a case failed, or was
skipped or unobservable without a waiver, an extension failed, the pair
found a code missing or a difference not allowed, the capture failed, or a
profile or target named could not be read or opened; 2 a usage error (a key
the catalogue lacks, a drive target without @--expect-serial@, a Level 0
transfer length a drive or a replay cannot carry); 3 a safety violation (a
drive target without root, or an IF-SEND issued on a drive).

The cases run in the catalogue's order, whatever the order of @--tests@ (the
Test Cases run section 5's in sequence, 3.1, PDF 25); a key naming a whole
test selects every case of it the catalogue has. Every command of the run
goes into one transcript: each end's transport records what it exchanges,
and a drive's is put behind the Guard (Level0Only), every command it is
given checked first, and also records into a transcript of its own, from
which the IF-SENDs issued on the drive are counted. A pair's ends run each
case in turn, the simulator first, each case's verdicts joined, each with
its end's name, its transport's role (@tper@, @drive@, @drive-replay@: the
one name the transcript's entries carry, "Hsed.Jig.Transcript"); then the
two Level 0 responses are compared
("Hsed.Jig.Pair"), discovered after the cases when UCT-01 did not. A drive
is never asked for Level 0 again once it was asked (a failed kext command
leaks a SCSI task in the kext), for the pair or for the capture. What a
case observes ("Hsed.Jig.Script".@observe@) goes into its result, and the
secrets the ends read (UCT-03's MSID, a PIN the run reads from the
simulator's state) into the run's, masked in the reports unless
@--show-msid@ (design 9.3 item 5).

A case's prerequisites are its section's list, in order (Test Cases 3.2.3,
PDF 25): one it waives is printed with its reason; on the simulator the run
establishes the synchronous protocol idle (draining every static ComID not
in Awaiting IF-SEND), establishes that no session is open (STACK_RESET on
every ComID whose sessions a case before left open, Core 3.3.4.7.5, PDF 65,
noted in the transcript), takes the credentials known (vacuous for a case
that authenticates no authority; for one that does, the authority's PIN
read from the simulator's state, which the case may then send or vary), each
HostChallenge the current PIN likewise, and every session read-write (every
StartSession the jig sends has Write True, Test Cases 4.1 e, PDF 26); on a
drive, for a case that needs Level 0 alone, SyncIdle and NoSessions are
unobservable under Level0Only and waived as such, Level 0 being outside the
synchronous protocol (Core 3.3.10.3 b, PDF 84) and answered whatever the
sessions (3.3.6.2, PDF 67). A prerequisite neither waived nor met skips the
case.
-}
module Hsed.Jig.Run
  ( RunOptions (..)
  , runJig
  , runOpened
  , cases
  ) where

import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import qualified RIO.Map                as Map
import           RIO
import qualified RIO.Text               as T
import           RIO.Time               (UTCTime, getCurrentTime)

import           Hsed.Core.UID          (UID)
import           Hsed.Gen.Core.UIDs     (uAuthority_SID)
import           Hsed.Jig.Capture       (captureLevel0)
import           Hsed.Jig.Case          (Case (..), Need (..))
import           Hsed.Jig.Cases.ETC02   (etc02)
import           Hsed.Jig.Cases.ETC03   (etc03)
import           Hsed.Jig.Cases.ETC04   (etc04)
import           Hsed.Jig.Cases.ETC08   (etc08)
import           Hsed.Jig.Cases.ETC15   (etc15)
import           Hsed.Jig.Cases.ETC21   (etc21adminsp)
import           Hsed.Jig.Cases.SPF02   (spf02case1)
import           Hsed.Jig.Cases.UCT01   (uct01)
import           Hsed.Jig.Cases.UCT02   (uct02)
import           Hsed.Jig.Cases.UCT03   (uct03steps1a1c)
import           Hsed.Jig.Catalogue     (CaseKey (..), CatalogueEntry (..), catalogue, renderCaseKey)
import           Hsed.Jig.Files         (runningAsRoot)
import           Hsed.Jig.Guard         (guarding)
import           Hsed.Jig.Options       (RunOptions (..), TargetSpec (..))
import           Hsed.Jig.Pair          (pairFails, pairLevel0)
import           Hsed.Jig.Prereq        (Prereq (..), Waiver (..), common)
import           Hsed.Jig.Report        (CaseResult (..), RunResult (..), renderTarget, summary, writeReports)
import           Hsed.Jig.Script        (Credential (..), RunCtx (..), RunEnv (..), Script, Verdict (..), attempt, discover,
                                         drain, drivePolicy, emptyRunCtx, simCredential, stackReset, verdictText)
import           Hsed.Jig.Target        (Target (..), closeTarget, openTarget)
import           Hsed.Jig.Transcript    (TEntry (..), Transcript, entries, ifSendsIssued, newTranscript, record, recording)
import           Hsed.Level0            (Level0)
import           Hsed.Sim.Profile       (loadProfile)
import           Hsed.Sim.State         (lifeOf)
import           Hsed.Sim.Types         (ComIdLife (Associated), ComIdState (..), LifeCycle (..), SimParams (..),
                                         SyncState (..), TPerState (..))
import           Hsed.Transport.Cdb     (level0Cdb)
import           Hsed.Transport.Drive   (DriveInfo (..))
import           Hsed.Transport.InProc  (SimControl (..))
import           Hsed.Version           (hsedVersion)
import           Hsed.Wire              (Describe (..), Dir (..), IfStatus (..), Request (..), Response (..), Unit (..),
                                         comPacketProtocol, describeTransport, hex, level0ComId, mostCarried, newChannel,
                                         roundUp, transferLength)


-- | The cases the jig runs, one for each entry of the catalogue, in its
--   order.
cases :: [Case]
cases = [uct01, uct02, uct03steps1a1c, spf02case1, etc02, etc03, etc04, etc08, etc15, etc21adminsp]

-- | The authority a case authenticates at StartSession, by the case's key,
--   with its name: ETC-21/adminsp alone, SID (Opal 2.30 Table 24, PDF 48);
--   every other case opens its sessions as Anybody, with no authority, or
--   none at all. What CredentialsKnown and HostChallengeIsCurrentPin are
--   about (Test Cases 5.2 items 3 and 4, 7.2 item 3; PDF 29 and 76).
authenticating :: [(CaseKey, (Text, UID))]
authenticating = [(cKey etc21adminsp, ("SID", uAuthority_SID))]

-- | Run the jig.
runJig :: RunOptions -> IO ExitCode
runJig opts0 = do
  started <- getCurrentTime
  root <- runningAsRoot
  case selected (roTests opts0) of
    Left e -> complain e $> ExitFailure 2
    Right chosen
      | devs@(_ : _) <- devices (roTarget opts0), isNothing (roExpectSerial opts0) -> do
          traverse_ (\d -> complain ("hsed-jig: --target " <> d <> " needs --expect-serial SERIAL: a drive is opened only as "
                                      <> "the one named")) devs
          pure (ExitFailure 2)
      | Just why <- level0LenProblem opts0 -> complain why $> ExitFailure 2
      | not root, devs@(_ : _) <- devices (roTarget opts0) -> do
          traverse_ (\d -> complain ("hsed-jig: --target " <> d <> " needs root (sudo), hsed's own gate: the kext checks no "
                                      <> "privilege (design 9.3)")) devs
          pure (ExitFailure 3)
      | otherwise -> withProfile opts0 >>= \case
          Left e     -> complain e $> ExitFailure 1
          Right opts -> tryAny (openTarget opts) >>= \case
            Left e           -> complain ("hsed-jig: --target " <> renderTarget (roTarget opts) <> ": " <> tshow e) $> ExitFailure 1
            Right (Left e)   -> complain ("hsed-jig: --target " <> renderTarget (roTarget opts) <> ": " <> e) $> ExitFailure 1
            Right (Right tg) -> runOn opts started chosen tg `finally` closeTarget tg

-- | Run the selected cases on a target the caller opened and closes: what
--   'runJig' does once its target is open, without its gates (root, the
--   profile), which the caller answers for; a test's drive that is a replay
--   with what the IORegistry would say of a drive. A key the catalogue lacks
--   is still exit 2.
runOpened :: RunOptions -> Target -> IO ExitCode
runOpened opts tg = case selected (roTests opts) of
  Left e       -> complain e $> ExitFailure 2
  Right chosen -> getCurrentTime >>= \started -> runOn opts started chosen tg

-- | The drives a target names.
devices :: TargetSpec -> [Text]
devices = \case
  TDevice d -> [d]
  TPair a b -> devices a <> devices b
  _         -> []

-- | Whether a target has a drive's end, its own or a replay.
hasDriveEnd :: TargetSpec -> Bool
hasDriveEnd = \case
  TSim      -> False
  TDevice _ -> True
  TReplay _ -> True
  TPair a b -> hasDriveEnd a || hasDriveEnd b

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

-- | The cases @--tests@ selects, in the catalogue's order: all of them for
--   none; a key naming a whole test, every case of it.
selected :: [CaseKey] -> Either Text [Case]
selected keys = do
  wanted <- if null keys then Right (map ceKey catalogue) else concat <$> traverse matching keys
  traverse caseOf [ ceKey e | e <- catalogue, ceKey e `elem` wanted ]
  where
    matching k = case [ ceKey e | e <- catalogue, k == ceKey e || whole k && ckTest k == ckTest (ceKey e) ] of
      [] -> Left ("hsed-jig: --tests " <> renderCaseKey k <> ": not a case of the catalogue ("
                  <> T.intercalate ", " (map (renderCaseKey . ceKey) catalogue) <> ")")
      ks -> Right ks
    whole k = isNothing (ckCase k) && isNothing (ckSteps k) && isNothing (ckVariant k)
    caseOf k = maybe (Left ("hsed-jig: the catalogue's " <> renderCaseKey k <> " has no case")) Right (L.find ((== k) . cKey) cases)

-- | The options with the profile read into the simulator's parameters.
withProfile :: RunOptions -> IO (Either Text RunOptions)
withProfile opts = case roProfile opts of
  Nothing   -> pure (Right opts)
  Just file -> loadProfile file <&> bimap (("hsed-jig: --profile " <> T.pack file <> ": ") <>)
                                          (\p -> opts { roSim = (roSim opts) { spProfile = Just p } })


-- * The ends of a run

-- | One end of a run: its name (its transport's role, the name the
--   transcript's entries carry: @tper@, @drive@, @drive-replay@), its
--   environment, and for a drive the transcript of what was issued to it
--   and what the IORegistry says of it.
data End = End
  { endName  :: !Text
  , endEnv   :: !RunEnv
  , endDrive :: !(Maybe (Transcript, Maybe DriveInfo))
  }

-- | A target's ends, each transport recording into the run's transcript; a
--   drive's also into its own, behind the Guard.
endsOf :: RunOptions -> Transcript -> LogFunc -> Target -> IO [End]
endsOf opts t lf = \case
  SimTarget tr control -> pure <$> end (roleOf tr) (SimTarget (recording t tr) control) Nothing
  DriveTarget tr di    -> do
    own <- newTranscript
    pure <$> end (roleOf tr) (DriveTarget (guarding t drivePolicy (recording t (recording own tr))) di) (Just (own, di))
  PairTarget a b       -> (<>) <$> endsOf opts t lf a <*> endsOf opts t lf b
  where
    roleOf tr = dRole (describeTransport tr)
    end name tg drive = do
      channel <- newChannel
      ctx <- newIORef emptyRunCtx
      pure (End name (RunEnv tg t channel ctx opts lf) drive)

-- | What an end's transport says it is.
endDescribe :: End -> Maybe Describe
endDescribe e = case reTarget (endEnv e) of
  SimTarget tr _   -> Just (describeTransport tr)
  DriveTarget tr _ -> Just (describeTransport tr)
  PairTarget _ _   -> Nothing

-- | Run the cases on the ends, then compare and capture Level 0, count the
--   IF-SENDs each drive was issued, gather the secrets the ends read, write
--   the reports.
runOn :: RunOptions -> UTCTime -> [Case] -> Target -> IO ExitCode
runOn opts started chosen tg = do
  t <- newTranscript
  logOptions <- logOptionsHandle stderr False
  withLogFunc (setLogMinLevel LevelInfo logOptions) $ \lf -> do
    ends <- endsOf opts t lf tg
    let named = length ends > 1
    results <- for chosen $ \c -> joined named c <$> for ends (\e -> (,) (endName e) <$> runCase e c)
    pairing <- case ends of
      a : b : _ -> Just <$> (liftA2 pairLevel0 <$> endLevel0 a <*> endLevel0 b)
      _         -> pure Nothing
    problems <- capture opts ends
    contexts <- for ends (readIORef . reCtx . endEnv)
    let level0s = [ (endName e, l0) | (e, ctx) <- zip ends contexts, Just l0 <- [rcLevel0 ctx] ]
        secrets = L.nub (filter (not . B.null) (concatMap rcSecrets contexts))
    ifSends <- sum <$> for [ own | e <- ends, Just (own, _) <- [endDrive e] ] (fmap ifSendsIssued . entries)
    transcript <- entries t
    let pairProblems = [ "pair: " <> e | Just (Left e) <- [pairing] ]
        allProblems  = pairProblems <> problems
        failed = any (not . passed) results || any pairFails [ r | Just (Right r) <- [pairing] ]
                 || not (null allProblems)
        exit | ifSends > 0 = ExitFailure 3
             | failed      = ExitFailure 1
             | otherwise   = ExitSuccess
        rr = RunResult
          { rrOptions    = opts
          , rrVersion    = hsedVersion
          , rrStarted    = started
          , rrTargets    = mapMaybe endDescribe ends
          , rrCases      = results
          , rrTranscript = transcript
          , rrLevel0     = level0s
          , rrPair       = either (const Nothing) Just =<< pairing
          , rrIfSends    = ifSends
          , rrSecrets    = secrets
          , rrProblems   = allProblems
          , rrExit       = exit
          }
    traverse_ complain allProblems
    when (ifSends > 0) $
      complain ("hsed-jig: " <> tshow ifSends <> " IF-SENDs issued on a drive: a safety violation (design 9.3)")
    tryAny (writeReports (roReportDir opts) rr) >>= \case
      Right paths -> exit <$ traverse_ say (summary rr paths)
      Left e      -> do
        traverse_ say (summary rr [])
        complain ("hsed-jig: the reports were not written: " <> tshow e)
        pure (if exit == ExitSuccess then ExitFailure 1 else exit)

-- | Whether a case passed: its verdict, and no extension failed.
passed :: CaseResult -> Bool
passed cr = crVerdict cr == Pass && null [ () | (_, Fail _) <- crExtensions cr ]

-- | A case's results on a run's ends as one: alone, as it is; on a pair, a
--   pass where every end passed, else the failures (or the first other
--   verdict) each with its end's name; the waivers every end made, and
--   those of some ends with their names; the extensions and the observations
--   with their end's; and each end's own verdict beside the joined one.
joined :: Bool -> Case -> [(Text, CaseResult)] -> CaseResult
joined named c rs
  | not named, [(_, r)] <- rs = r
  | otherwise = CaseResult (cKey c) verdict waivers extensions observations [ (n, crVerdict r) | (n, r) <- rs ]
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

-- | A case on one end: skipped if the end lacks what it needs or a
--   prerequisite is unmet; else run to its verdict, then its extensions,
--   on the simulator alone; what the case and its extensions observed goes
--   into its result, and the next case starts with none.
runCase :: End -> Case -> IO CaseResult
runCase e c = runRIO (endEnv e) $ do
  let heading = renderCaseKey (cKey c) <> " on the " <> endName e
  logInfo (display heading)
  note heading
  tg <- asks reTarget
  ref <- asks reCtx
  modifyIORef' ref (\x -> x { rcObservations = [] })
  case lacking tg (cNeeds c) of
    Just why -> pure (CaseResult (cKey c) (Skipped why) [] [] [] [])
    Nothing  -> prerequisites c >>= \case
      Left why -> pure (CaseResult (cKey c) (Skipped ("prerequisite unmet: " <> why)) [] [] [] [])
      Right ws -> do
        v <- attempt (cRun c)
        xs <- for (cExtensions c) $ \(name, s) -> (,) name <$> case tg of
          SimTarget _ _ -> note (renderCaseKey (cKey c) <> " extension on the " <> endName e <> ", " <> name) >> attempt s
          _             -> pure (Skipped "the simulator's alone")
        observations <- rcObservations <$> readIORef ref
        pure (CaseResult (cKey c) v ws xs observations [])

-- | Why a target cannot run a case that needs these, if it cannot: a drive
--   before M4c answers Level 0 Discovery alone, and after it the allow
--   list's IF-SENDs, never an error path without opting in nor what the
--   simulator alone can run.
lacking :: Target -> [Need] -> Maybe Text
lacking tg needs = listToMaybe [ why | n <- [NeedSimOnly, NeedErrorPath, NeedIfSend, NeedSimControl, NeedIfRecv, NeedLevel0], n `elem` needs, Just why <- [lack n] ]
  where
    lack n = case tg of
      SimTarget _ _   -> Nothing
      DriveTarget _ _ -> case n of
        NeedLevel0     -> Nothing
        NeedIfRecv     -> Just "needs IF-RECV beyond Level 0 Discovery, outside Level0Only, a drive's policy before M4c (design 9.3)"
        NeedIfSend     -> Just "needs IF-SEND; the drive is 'ReadOnly before M4c (design 9.3)"
        NeedErrorPath  -> Just "needs an IF-SEND off the allow list, an error path: the simulator's until a drive run opts in (design 9.3 item 6; M4c)"
        NeedSimOnly    -> Just "the simulator's alone: no drive policy admits it (design 9.3)"
        NeedSimControl -> Just "needs the simulator's control"
      PairTarget _ _  -> Just "a pair's ends run a case one at a time"

-- | A case's prerequisites in its section's order: each waived, or
--   established or checked on this end; the waivers, or why one is unmet.
prerequisites :: Case -> Script (Either Text [Waiver])
prerequisites c = go (common (cSection c))
  where
    go [] = pure (Right [])
    go ((p, _) : rest) = case L.find ((== p) . wPrereq) (cWaives c) of
      Just w  -> fmap (w :) <$> go rest
      Nothing -> establish c p >>= \case
        Left why -> pure (Left why)
        Right mw -> fmap (maybe id (:) mw) <$> go rest

-- | A prerequisite on this end: met (established if need be), waived as
--   unobservable, or why not. On the simulator: SyncIdle drains every
--   ComID not in Awaiting IF-SEND; NoSessions sends STACK_RESET on every
--   ComID a session holds (Core 3.3.4.7.5 item 1, PDF 65: every session on
--   it aborted, no CloseSession prepared), noted, and reads the state again;
--   LockingManufactured reads the Locking SP's Original Factory State;
--   CredentialsKnown is vacuous for a case that authenticates no authority
--   ('authenticating'), and for one that does reads the authority's PIN from
--   the state ("Hsed.Jig.Script".'simCredential'), a secret of the run;
--   HostChallengeIsCurrentPin likewise (a case's challenge is that PIN, or a
--   variation it makes on purpose); ReadWriteSessions is met, every session
--   the jig opens being read-write (Test Cases 4.1 e, PDF 26).
establish :: Case -> Prereq -> Script (Either Text (Maybe Waiver))
establish c p = asks reTarget >>= \case
  SimTarget _ control -> liftIO (scPeek control) >>= onSim control
  DriveTarget _ _
    | p `elem` [SyncIdle, NoSessions], all (== NeedLevel0) (cNeeds c) -> pure (Right (Just (Waiver p unobservable)))
    | otherwise -> pure (Left (tshow p <> ": not observable on a drive under Level0Only at M3"))
  PairTarget _ _ -> pure (Left "a pair's ends run a case one at a time")
  where
    unobservable = "unobservable under Level0Only, which lets Level 0 Discovery alone reach the drive, and not needed: Level 0 "
                   <> "is outside the synchronous protocol (Core 3.3.10.3 b, PDF 84) and answered whatever the sessions "
                   <> "(3.3.6.2, PDF 67)"
    authority = L.lookup (cKey c) authenticating
    met = pure (Right Nothing)
    onSim control st = case p of
      SyncIdle -> do
        let busy = [ comid | (comid, s) <- Map.toList (tsComIds st), cSync s /= S1 ]
        unless (null busy) (note ("SyncIdle: draining " <> T.unwords (map hex busy)))
        attempt (Pass <$ traverse_ drain busy) <&> \case
          Pass -> Right Nothing
          v    -> Left ("SyncIdle: " <> verdictText v)
      NoSessions -> case holding st of
        []   -> met
        held -> do
          note ("NoSessions: sessions hold ComIDs " <> T.unwords (map hex held) <> ": STACK_RESET on each (Core 3.3.4.7.5, PDF 65)")
          for_ held $ \comid -> stackReset comid >>= \outcome ->
            note ("NoSessions: STACK_RESET on " <> hex comid <> ": " <> either id (("Success/Failure " <>) . tshow) outcome)
          asks reCtx >>= \ref -> modifyIORef' ref (\x -> x { rcSession = Nothing })
          liftIO (scPeek control) <&> holding <&> \case
            []    -> Right Nothing
            still -> Left ("NoSessions: sessions still hold ComIDs " <> T.unwords (map hex still) <> " after STACK_RESET")
      LockingManufactured
        | spLockingOFS (tsParams st) == Manufactured -> met
        | otherwise -> pure (Left "LockingManufactured: the simulator's Locking SP is Manufactured-Inactive, its Original Factory State (design 9.1; M5)")
      CredentialsKnown -> case authority of
        Nothing -> Right Nothing <$ note "CredentialsKnown: vacuous, the case authenticates no authority (a session as Anybody needs no credential, Core 5.3.4.1.2.1, PDF 211)"
        Just (name, u) -> case simCredential u st of
          Nothing -> pure (Left ("CredentialsKnown: the simulator's Admin SP holds no credential for " <> name))
          Just cr -> do
            asks reCtx >>= \ref -> modifyIORef' ref (\x -> x { rcSecrets = L.nub (rcSecrets x <> [crPIN cr]) })
            note ("CredentialsKnown: " <> name <> "'s PIN read from the simulator's state, " <> tshow (B.length (crPIN cr))
                  <> " bytes, TryLimit " <> tshow (crTryLimit cr) <> ", Tries " <> tshow (crTries cr) <> " (masked in the reports unless --show-msid)")
            met
      HostChallengeIsCurrentPin -> case authority of
        Nothing        -> Right Nothing <$ note "HostChallengeIsCurrentPin: vacuous, the case sends no HostChallenge"
        Just (name, _) -> Right Nothing <$ note ("HostChallengeIsCurrentPin: " <> name <> "'s current PIN is read from the simulator's state; a "
                                                 <> "challenge other than it is the case's own doing")
      ReadWriteSessions -> Right Nothing <$ note "ReadWriteSessions: every session the jig opens is read-write (StartSession's Write True, Test Cases 4.1 e, PDF 26)"
    -- the ComIDs sessions hold
    holding s = [ comid | comid <- Map.keys (tsComIds s), lifeOf comid s == Just Associated ]

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
    asked <- maybe (pure False) (fmap (any level0Issued) . entries . fst) (endDrive e)
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
  Just file -> case [ (e, own, di) | e <- ends, Just (own, Just di) <- [endDrive e] ] of
    [] -> pure ["--capture-level0 " <> T.pack file <> ": no drive to capture (the simulator and a replay have none)"]
    (e, own, di) : _ -> do
      found <- lastLevel0 own >>= \case
        Just x  -> pure (Just x)
        Nothing -> endLevel0 e >> lastLevel0 own
      case found of
        Nothing     -> pure ["--capture-level0 " <> T.pack file <> ": the drive answered no Level 0 Discovery that succeeded"]
        Just (q, r) -> case level0Cdb (diInterface di) (lenBytes q `div` transferLength UnitBlocks512 1) of
          Left why  -> pure ["--capture-level0 " <> T.pack file <> ": no CDB for the IF-RECV: " <> why]
          Right cdb -> tryAny (captureLevel0 file di cdb r) <&> either (\ex -> ["--capture-level0 " <> T.pack file <> ": " <> tshow ex]) (const [])
  where
    lastLevel0 own = entries own <&> \es -> L.lastMaybe [ (q, r) | Issued _ _ q r <- es, isLevel0 q, status r == Good ]

-- | Whether a command is Level 0 Discovery's IF-RECV.
isLevel0 :: Request -> Bool
isLevel0 q = dir q == IfRecv && proto q == comPacketProtocol && comId q == level0ComId


-- * Printing

say :: Text -> IO ()
say t = hPutBuilder stdout (getUtf8Builder (display t <> "\n"))

complain :: Text -> IO ()
complain t = hPutBuilder stderr (getUtf8Builder (display t <> "\n"))
