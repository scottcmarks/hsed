{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.ReportSpec
Description : Test specifications for Hsed.Jig.Report
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A run's reports (design 9.2): three files, @jig-<target>-<UTC stamp>@ as
JSON, Markdown and JUnit XML, the target's part @sim@, @dev-disk6@ or
@sim+replay-<file>@, a second run of the same second suffixed @-2@, runs
started together each with a name of their own; each
case with its verdict and waivers, a pair's each end's verdict beside it, a
waiver with its item and page (LockingManufactured, Test Cases 5.2 item 2,
PDF 29), each prerequisite established or not reached; the simulator's
options only where a simulator ran; the run's problems; the JUnit report
failing the pair, each problem and the exit code wherever the exit code
fails; the transcript with each IF-SEND's methods
decoded and each command's end, a subpacket of several items decoded as
each in order; the JUnit counts. The secrets a run read are masked in every
report, in the transcript's bytes and in every text a case came to, each
given by its length and the first 12 hex digits of its SHA-256, in clear
with @--show-msid@ alone (design 9.3 item 5), and the simulator's SID PIN
is given the same way among the options. M4c's keys (each drive's policy,
the commands outside it, those the Guard blocked, why a run stopped, a
pre-flight's notes, @--recover@'s outcome, each end's Properties with the
host's, the pair's comparison of them) are empty or null for a run without
them and given as they are otherwise, a secret their texts or a command
outside the policy quote masked; each end's Properties read back by name.

M4c's renderings (design 9.3): a drive's policy, the commands outside it
and those the Guard refused in the Markdown's Policies, in JUnit (a drive's
two test cases, renamed and added: no drive command outside its policy, the
Guard refused no command; the exit code's 3 naming the commands) and in the
summary; a run stopped (its section, its failed test case, exit 130 and why
in the summary); the pre-flight's notes; @--recover@'s outcome, recovered
or not (its section, its test case, the exit code's reason); the pair's
Properties not compared, with why, or compared, each judgement counted and
its test case failed where a difference is not allowed; a secret under four
bytes marked short in the JSON and warned of in the Markdown, the JUnit
properties and the summary.

@verify-transcript@ fails a report whose drive was issued an IF-SEND only
when asked to, the simulator's IF-SENDs counting for nothing, and with
@--level0-only@ one whose drive was issued anything but Level 0 Discovery's
IF-RECV; an entry of no end counts as a drive's, and one that does not read
against both; a file that is no report fails. With @--allow-list@: the
report of the M4c acceptance's cases on the simulator as a drive
(Support's fakeDrive), bound to 0x1004 by its own Level 0, passes in both
modes (STACK_RESET before each case, or an idle IF-RECV), the base read from
the report's Level 0; the same report with one drive command made a Set, a
STACK_RESET on TPER_RESET's ComID 0x0004, a Get on C_PIN_SID, or an entry
that does not read, or with a Blocked entry added, fails, each edit one
the Guard refuses whether or not it reads the base ComID (DrivePathSpec
edits an IF-RECV onto another base, which only M4c's Guard refuses); so
does one whose Level 0 names another base than its policy is bound to, or
none; an M3 report of Level 0 alone passes. @diff@ compares two reports'
Level 0 responses as the Pair runner does, and their last ends' Properties
exchanges as the Pair's Properties are, where both carry them.
-}
module Hsed.Jig.ReportSpec (spec) where

import qualified Data.Aeson            as A
import qualified Data.Aeson.KeyMap     as KM
import           GHC.IO.Handle         (hDuplicate, hDuplicateTo)
import           Numeric               (showHex)
import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import           RIO.FilePath          ((</>))
import qualified RIO.Text              as T
import           RIO.Time              (UTCTime (..), fromGregorian)
import           Test.Hspec

import           Hsed.Core.Call        (MethodResponse (..))
import           Hsed.Core.CellBlock   (cellBlockValue, columnRange)
import           Hsed.Core.Method      (invocation)
import           Hsed.Core.Record      (encodeRecord)
import           Hsed.Core.StreamItem  (StreamItem (..))
import           Hsed.Core.Token       (Token (..))
import           Hsed.Core.Value       (List (..), Value (..), (.:))
import           Hsed.Gen.Core.Layouts (STACK_RESETCommandRequest (..))
import qualified Hsed.Gen.Core.Methods as Methods
import           Hsed.Gen.Core.UIDs    (uC_PIN_SID, uSMUID)
import           Hsed.Jig.Case         (Case (..))
import           Hsed.Jig.Catalogue    (CaseKey (..), TestId (..), parseCaseKeys)
import           Hsed.Jig.Guard        (Allowed (..), Policy (..), drivePolicy, driveIssued, driveTransport, guardDrive, readOnlyMethods)
import           Hsed.Jig.Options      (RunOptions (..), TargetSpec (..), defaultRunOptions)
import           Hsed.Jig.Pair         (Judgement (..), PairReport (..))
import           Hsed.Jig.PairProperties (PropDiff (..), PropertiesPair (..), Side (..), pairProperties, propertiesLines)
import           Hsed.Jig.Prereq       (Waiver (..), lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Report
import           Hsed.Jig.Run          (driveAllowedFor, selectedCases)
import           Hsed.Jig.Script       (PropertiesAnswer (..), RunCtx (..), RunEnv (..), SessionHandle (..), Verdict (..), attempt, baseComId,
                                        controlComPacket, emptyRunCtx, ifRecvOn, sessionComPacket, stackReset)
import qualified Hsed.Jig.Script       as Script (note)
import           Hsed.Jig.Support      (at, atPath, fakeDrive, silently)
import           Hsed.Jig.Target       (Target (..))
import           Hsed.Jig.Transcript   (TEntry (..), entries, ifSendsIssued, newTranscript)
import           Hsed.Level0           (Descriptor (..), Level0 (..))
import           Hsed.Sim.Types        (SimParams (..), defaultSimParams)
import           Hsed.Transport.Drive  (DriveInfo (..))
import           Hsed.Wire             (Describe (..), Dir (..), IfStatus (..), Request (Request), Response (Response), Unit (..),
                                        describeTransport, newChannel)


-- | 2026-10-30 12:00:00 UTC.
noon :: UTCTime
noon = UTCTime (fromGregorian 2026 10 30) (12 * 3600)

-- | An IF-SEND of Properties on 0x1004.
propertiesSend :: Request
propertiesSend = Request IfSend 1 0x1004 512 (bytes <> B.replicate (512 - B.length bytes) 0)
  where bytes = either (error . show) id (controlComPacket 0x1004 (generate (invocation uSMUID (Methods.Properties Nothing))))

-- | A run on the simulator: UCT-01 passed, SPF-02:1 failed with an
--   extension passed, ETC-02 skipped; a note, an IF-SEND issued, one blocked.
sample :: RunResult
sample = RunResult
  { rrOptions        = defaultRunOptions
  , rrVersion        = "Version 0.1.0.1 [test]"
  , rrStarted        = noon
  , rrTargets        = [Describe "tper" "hsed-sim test" UnitBytes (Just "opal-2.30") (Just 0x1004) (Just 1) Nothing]
  , rrCases          =
      [ CaseResult (CaseKey (UCT 1) Nothing Nothing Nothing) Pass (lockingSPUntouched : noSessionOpened) [] [] [] True
      , CaseResult (CaseKey (SPF 2) (Just 1) Nothing Nothing) (Fail ["step 1: not so"]) [lockingSPUntouched] [("an extension", Pass)]
          [("MaxComPacketSize", "2048")] [] True
      , CaseResult (CaseKey (ETC 2) Nothing Nothing Nothing) (Skipped "needs IF-SEND") [] [] [] [] False
      ]
  , rrTranscript     =
      [ Note 1 "UCT-01 on the tper"
      , Issued 2 "tper" propertiesSend (Response Good mempty)
      , Blocked 3 "drive" propertiesSend "Level0Only: no IF-SEND reaches a drive at M3 (design 9.3)"
      ]
  , rrLevel0         = []
  , rrPair           = Nothing
  , rrIfSends        = 0
  , rrSecrets        = []
  , rrProblems       = []
  , rrExit           = ExitFailure 1
  , rrPolicies       = []
  , rrOutsidePolicy  = []
  , rrBlocked        = 0
  , rrInterrupted    = Nothing
  , rrPreflight      = []
  , rrRecover        = Nothing
  , rrProperties     = []
  , rrPairProperties = Nothing
  }

-- | A pair of the simulator and a replay: UCT-01 passed on the simulator,
--   skipped on the replay; the pair failing on a missing code; a capture
--   problem; an IF-SEND issued on the drive outside its policy.
paired :: RunResult
paired = sample
  { rrOptions       = defaultRunOptions { roTarget = TPair TSim (TReplay "disk6.bin") }
  , rrTargets       = rrTargets sample <> [Describe "drive-replay" "hsed replay test" UnitBlocks512 Nothing Nothing Nothing (Just "disk6.bin")]
  , rrCases         = [ CaseResult (CaseKey (UCT 1) Nothing Nothing Nothing) (Skipped "drive-replay: no Level 0") [] [] []
                          [("tper", Pass), ("drive-replay", Skipped "no Level 0")] False ]
  , rrPair          = Just (PairReport [0x0005] [0x0403] [] [] [])
  , rrIfSends       = 1
  , rrProblems      = ["--capture-level0 x.bin: no drive to capture"]
  , rrExit          = ExitFailure 3
  , rrPolicies      = [("drive-replay", Level0Only)]
  , rrOutsidePolicy = [("drive-replay", propertiesSend)]
  }

-- | The same with the transcript's IF-SEND not issued.
quiet :: RunResult
quiet = sample { rrTranscript = [ e | e <- rrTranscript sample, not (isIssued e) ] }
  where isIssued e = case e of
          Issued {} -> True
          _         -> False

-- | The same with the IF-SEND issued on a drive.
onDrive :: RunResult
onDrive = sample { rrTranscript = map toDrive (rrTranscript sample) }
  where toDrive e = case e of
          Issued n _ q r -> Issued n "drive" q r
          _              -> e

-- | A drive's transcript of IF-RECVs alone: Level 0 Discovery's, and others.
recvs :: [(Word8, Word16)] -> RunResult
recvs cs = quiet { rrTranscript = [ Issued n "drive-replay" (Request IfRecv p c 512 mempty) (Response Good mempty) | (n, (p, c)) <- zip [1 ..] cs ] }

-- | The simulator's default MSID, and its hex, as a transcript would print
--   it unmasked.
msid :: ByteString
msid = spMsid defaultSimParams

hexOf :: ByteString -> Text
hexOf = T.pack . concatMap (\w -> (if w < 16 then "0" else "") <> showHex w "") . B.unpack

-- | A run that read the MSID (UCT-03 step 1b): the Get's result in an
--   IF-RECV's answer, a challenge holding it in an IF-SEND, the MSID among
--   the secrets, the simulator's SID PIN given; the secrets shown or not.
secretive :: Bool -> RunResult
secretive showing = sample
  { rrOptions    = defaultRunOptions { roShowMsid = showing, roSim = defaultSimParams { spSidPin = Just "SID!" } }
  , rrSecrets    = [msid]
  , rrTranscript =
      [ Issued 1 "tper" (Request IfSend 1 0x1004 2048 (padded challenge)) (Response Good mempty)
      , Issued 2 "tper" (Request IfRecv 1 0x1004 2048 mempty) (Response Good (padded result))
      ]
  }
  where
    h         = SessionHandle 0x1004 4096 1
    result    = either (error . show) id (sessionComPacket h (generate (MethodResponse [L (List [N ((3 :: Natural) .: msid)])] 0)))
    challenge = either (error . show) id (sessionComPacket h (msid <> "!"))
    padded b  = b <> B.replicate (2048 - B.length b) 0

-- | The sample with M4c's fields given: a drive bound under the read-only
--   list, a command outside it, one blocked, a stopped run, a pre-flight's
--   note, a recovery, the drive's Properties with the host's, the pair's
--   not compared.
m4cFields :: RunResult
m4cFields = sample
  { rrPolicies       = [("drive", AllowList 0x1004 readOnlyMethods)]
  , rrOutsidePolicy  = [("drive", propertiesSend)]
  , rrBlocked        = 1
  , rrInterrupted    = Just "SIGINT"
  , rrPreflight      = ["/dev/disk22s1 on /Volumes/SED NVME (msdos), allowed"]
  , rrRecover        = Just (Right "STACK_RESET on 0x1004: Success")
  , rrProperties     = [("drive", [("MaxComPacketSize", 4096)], PropertiesAnswer [("MaxComPacketSize", 2048), ("MaxSessions", 1)]
                                                                                 (Just [("MaxComPacketSize", 4096)]))]
  , rrPairProperties = Just (NotCompared "one end ran no UCT-02")
  }

-- | A JSON array's elements.
arr :: Maybe A.Value -> [A.Value]
arr = \v -> case v of
  Just (A.Array xs) -> toList xs
  _                 -> []

-- | A report written, read back: its files, its JSON and its Markdown.
written :: RunResult -> IO ([FilePath], A.Value, Text)
written rr = withSystemTempDirectory "hsed-report" $ \dir -> do
  paths <- writeReports dir rr
  json <- readFileBinary (dir </> "jig-sim-20261030T120000Z.json") >>= either throwString pure . A.eitherDecodeStrict'
  md <- readFileUtf8 (dir </> "jig-sim-20261030T120000Z.md")
  pure (map (L.drop (length dir + 1)) paths, json, md)

-- | The three reports as text.
texts :: RunResult -> IO (Text, Text, Text)
texts rr = withSystemTempDirectory "hsed-report" $ \dir -> do
  _ <- writeReports dir rr
  (,,) <$> readFileUtf8 (dir </> "jig-sim-20261030T120000Z.json")
       <*> readFileUtf8 (dir </> "jig-sim-20261030T120000Z.md")
       <*> readFileUtf8 (dir </> "jig-sim-20261030T120000Z.xml")

-- | A run's three reports written and read back, whatever its target: its
--   JSON, its Markdown and its JUnit.
reportsOf :: RunResult -> IO (A.Value, Text, Text)
reportsOf rr = withSystemTempDirectory "hsed-report" $ \dir -> do
  paths <- writeReports dir rr
  let named ext = maybe (throwString ("no " <> ext <> " report")) pure (L.find (ext `L.isSuffixOf`) paths)
  json <- named ".json" >>= readFileBinary >>= either throwString pure . A.eitherDecodeStrict'
  (,,) json <$> (named ".md" >>= readFileUtf8) <*> (named ".xml" >>= readFileUtf8)

-- | What an action prints to stdout, with what it prints to stderr thrown
--   away; and what it came to.
capturing :: IO a -> IO (a, Text)
capturing act = withSystemTempFile "hsed-stdout" $ \path h ->
  withFile "/dev/null" WriteMode $ \devNull -> do
    hFlush stdout >> hFlush stderr
    r <- bracket ((,) <$> hDuplicate stdout <*> hDuplicate stderr)
                 (\(out, err) -> do
                    hFlush stdout >> hFlush stderr
                    hDuplicateTo out stdout >> hDuplicateTo err stderr
                    hClose out >> hClose err)
                 (\_ -> hDuplicateTo h stdout >> hDuplicateTo devNull stderr >> act)
    hClose h
    (,) r <$> readFileUtf8 path

-- | A JSON array's elements.
elements :: Maybe A.Value -> [A.Value]
elements = \v -> case v of
  Just (A.Array xs) -> toList xs
  _                 -> []

-- | A Set of C_PIN_SID's PIN, the command the Guard is there to stop, and
--   Get on C_PIN_SID's PIN, a read it does not admit: each an IF-SEND on
--   0x1004 of one control packet, padded to a block.
setSend, getSidSend :: Request
setSend    = blockOf (generate (invocation uC_PIN_SID (Methods.Set Nothing (Just (L (List [N ((3 :: Natural) .: ("not the PIN" :: ByteString))]))))))
getSidSend = blockOf (generate (invocation uC_PIN_SID (Methods.Get (cellBlockValue (columnRange 3 3)))))

-- | STACK_RESET on TPER_RESET's ComID, 0x0004, which Core Table 26
--   reserves (PDF 55): an IF-SEND of protocol 2 carrying Table 35's request
--   for its Extended ComID.
resetOnTperResetComId :: Request
resetOnTperResetComId = Request IfSend 2 0x0004 512 (bytes <> B.replicate (512 - B.length bytes) 0)
  where bytes = either (error . show) id (encodeRecord (STACK_RESETCommandRequest 0x00040000))

-- | Tokens in one control packet on 0x1004, as an IF-SEND of a block.
blockOf :: ByteString -> Request
blockOf tokens = Request IfSend 1 0x1004 512 (bytes <> B.replicate (512 - B.length bytes) 0)
  where bytes = either (error . show) id (controlComPacket 0x1004 tokens)

-- | The sample as a drive's run under the read-only list and STACK_RESET,
--   bound to 0x1004: a Set issued outside it, a command the Guard refused.
onDriveM4c :: RunResult
onDriveM4c = sample
  { rrOptions       = defaultRunOptions { roTarget = TDevice "/dev/disk22" }
  , rrTargets       = [Describe "drive" "kext test" UnitBlocks512 Nothing Nothing Nothing (Just "/dev/disk22")]
  , rrPolicies      = [("drive", AllowList 0x1004 (readOnlyMethods <> [AllowStackReset]))]
  , rrOutsidePolicy = [("drive", setSend)]
  , rrBlocked       = 1
  , rrExit          = ExitFailure 3
  }

-- | The keys of the M4c acceptance's drive cases.
acceptanceKeys :: [CaseKey]
acceptanceKeys = either (error . T.unpack) id (parseCaseKeys "UCT-02,UCT-03:1a-1c,SPF-02:1")

-- | The M4c acceptance's cases on the simulator as a drive (Support's
--   fakeDrive), as a run of either mode issues them: each case's script on
--   the drive behind the Guard as a run opens one, bound to 0x1004 from its
--   own Level 0, and before each case STACK_RESET on the base (the shipped
--   default, @roStackResetBeforeCases@) or an IF-RECV that finds it idle
--   (the other mode's probe); the run's reports written into a directory:
--   the JSON's path, and each case's verdict. Built by hand while the
--   runner still skipped these cases on a drive (M4b's way); since M4c the
--   runner runs them (RunSpec), and DrivePathSpec repeats this check on
--   whole runs over the fake kext.
acceptanceReport :: Bool -> FilePath -> IO (FilePath, [Verdict])
acceptanceReport resets dir = do
  (raw, di, _) <- fakeDrive defaultSimParams
  t <- newTranscript
  let opts = defaultRunOptions { roTarget = TDevice ("/dev/" <> diBsd di), roTests = acceptanceKeys, roStackResetBeforeCases = resets
                               , roReportDir = dir }
  chosen <- either (throwString . T.unpack) pure (selectedCases acceptanceKeys)
  g <- guardDrive t (driveAllowedFor opts chosen) (Just di) raw
  channel <- newChannel
  ctx <- newIORef emptyRunCtx
  stop <- newIORef Nothing
  verdicts <- runRIO (RunEnv (DriveTarget g) t channel ctx opts stop (mkLogFunc (\_ _ _ _ -> pure ()))) $ do
    base <- baseComId
    for chosen $ \c -> do
      if resets
        then stackReset base >>= Script.note . ("SyncIdle and NoSessions: STACK_RESET on 0x1004: " <>) . either id (("Success/Failure " <>) . tshow)
        else void (ifRecvOn base)
      attempt (cRun c)
  final <- readIORef ctx
  policy <- drivePolicy g
  issued <- driveIssued g
  transcript <- entries t
  paths <- writeReports dir RunResult
    { rrOptions        = opts
    , rrVersion        = "Version 0.1.0.1 [test]"
    , rrStarted        = noon
    , rrTargets        = [describeTransport (driveTransport g)]
    , rrCases          = [ CaseResult (cKey c) v [] [] [] [] True | (c, v) <- zip chosen verdicts ]
    , rrTranscript     = transcript
    , rrLevel0         = [ ("drive", l0) | Just l0 <- [rcLevel0 final] ]
    , rrPair           = Nothing
    , rrIfSends        = ifSendsIssued issued
    , rrSecrets        = rcSecrets final
    , rrProblems       = []
    , rrExit           = if all (== Pass) verdicts then ExitSuccess else ExitFailure 1
    , rrPolicies       = [("drive", policy)]
    , rrOutsidePolicy  = []
    , rrBlocked        = length [ () | Blocked {} <- transcript ]
    , rrInterrupted    = Nothing
    , rrPreflight      = []
    , rrRecover        = Nothing
    , rrProperties     = [ ("drive", sent, answer) | Just (sent, answer) <- [rcNegotiated final] ]
    , rrPairProperties = Nothing
    }
  json <- maybe (throwString "no JSON report") pure (L.find (".json" `L.isSuffixOf`) paths)
  pure (json, verdicts)

-- | A report's JSON, edited and written to a file of its own: its path.
edited :: FilePath -> (A.Value -> A.Value) -> FilePath -> IO FilePath
edited out f json = do
  v <- readFileBinary json >>= either throwString pure . A.eitherDecodeStrict'
  out <$ writeFileBinary out (toStrictBytes (A.encode (f v)))

-- | A report's key edited.
editKey :: Text -> (A.Value -> A.Value) -> A.Value -> A.Value
editKey k f = \v -> case v of
  A.Object o -> A.Object (KM.insert (fromString (T.unpack k)) (f (fromMaybe A.Null (KM.lookup (fromString (T.unpack k)) o))) o)
  _          -> v

-- | Each element of a JSON array edited.
editEach :: ([A.Value] -> [A.Value]) -> A.Value -> A.Value
editEach f = A.toJSON . f . elements . Just

-- | The first element that holds edited.
editFirst :: (A.Value -> Bool) -> (A.Value -> A.Value) -> [A.Value] -> [A.Value]
editFirst p f vs = case break p vs of
  (earlier, v : later) -> earlier <> (f v : later)
  (earlier, [])        -> earlier

-- | Whether a transcript entry is a command issued on the drive's end, the
--   first IF-SEND of protocol 1 (UCT-02's Properties) among them.
issuedSend :: A.Value -> Bool
issuedSend e = at "entry" e == Just "issued" && at "end" e == Just "drive" && atPath ["request", "op"] e == Just "if_send"
               && atPath ["request", "proto"] e == Just (A.Number 1)

-- | A transcript entry's command replaced by another, as the report prints
--   one.
withRequest :: Request -> A.Value -> A.Value
withRequest q = editKey "request" (const (A.toJSON q))

-- | A report's Level 0 descriptors edited, each end's.
editDescriptors :: ([A.Value] -> [A.Value]) -> A.Value -> A.Value
editDescriptors f = editKey "level0" (editEach (map (editKey "response" (editKey "descriptors" (editEach f)))))

-- | The Opal SSC V2 descriptor (0x0203) with its Base ComID (bytes 4 to 5,
--   Opal 2.30 Table 6, PDF 23) another.
rebased :: Text -> [A.Value] -> [A.Value]
rebased base = map $ \d -> if at "code" d == Just (A.Number 0x0203) then editKey "bytes" rebase d else d
  where
    rebase = \v -> case v of
      A.String hex' -> A.String (T.take 8 hex' <> base <> T.drop 12 hex')
      _             -> v

spec :: Spec
spec = do
  describe "writeReports" $ do
    it "writes jig-<target>-<UTC stamp> as json, md and xml" $ do
      (paths, _, _) <- written sample
      paths `shouldBe` ["jig-sim-20261030T120000Z.json", "jig-sim-20261030T120000Z.md", "jig-sim-20261030T120000Z.xml"]
    it "names a drive's report dev-disk6, a pair's with a replay sim+replay-<file>" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        let named t = map (L.drop (length dir + 1)) <$> writeReports dir sample { rrOptions = defaultRunOptions { roTarget = t } }
        named (TDevice "/dev/disk6") `shouldReturn`
          ["jig-dev-disk6-20261030T120000Z.json", "jig-dev-disk6-20261030T120000Z.md", "jig-dev-disk6-20261030T120000Z.xml"]
        take 1 <$> named (TPair TSim (TReplay "build/disk6-like-level0.bin")) `shouldReturn`
          ["jig-sim+replay-disk6-like-level0-20261030T120000Z.json"]
    it "prints each waiver in Markdown with its item and page" $ do
      (_, _, md) <- written sample
      md `shouldSatisfy` T.isInfixOf "LockingManufactured (Test Cases 5.2 item 2, PDF 29) waived: never touches the Locking SP (design 9.1)"
      md `shouldSatisfy` T.isInfixOf "LockingManufactured (Test Cases 6.2 item 2, PDF 49) waived"
      md `shouldSatisfy` T.isInfixOf "CredentialsKnown (Test Cases 5.2 item 3, PDF 29) waived: vacuous: the case opens no session"
    it "gives each case's verdict, waivers, extensions and observations in JSON and Markdown" $ do
      (_, json, md) <- written sample
      let cases' = maybe [] (\v -> case v of A.Array xs -> toList xs; _ -> []) (at "cases" json)
      map (at "verdict") cases' `shouldBe` map (Just . A.String) ["pass", "fail", "skipped"]
      map (atPath ["key"]) cases' `shouldBe` map (Just . A.String) ["UCT-01", "SPF-02:1", "ETC-02"]
      [ atPath ["pdf"] w | Just (A.Array ws) <- [at "waivers" =<< listToMaybe cases'], w <- toList ws ]
        `shouldBe` replicate 4 (Just (A.Number 29))
      [ (at "name" o, at "value" o) | c <- cases', Just (A.Array os) <- [at "observations" c], o <- toList os ]
        `shouldBe` [(Just "MaxComPacketSize", Just "2048")]
      md `shouldSatisfy` T.isInfixOf "Observations:\n\n- MaxComPacketSize: 2048"
      (at "if_send_issued" json, at "exit" json) `shouldBe` (Just (A.Number 0), Just (A.Number 1))
    it "decodes each IF-SEND's ComPacket in the transcript to its method" $ do
      (_, json, _) <- written sample
      let entries' = maybe [] (\v -> case v of A.Array xs -> toList xs; _ -> []) (at "transcript" json)
          methods  = [ atPath ["method_call", "method"] sp
                     | e <- entries', Just (A.Array ps) <- [atPath ["decode", "request", "packets"] e]
                     , p <- toList ps, Just (A.Array sps) <- [at "subpackets" p], sp <- toList sps ]
      methods `shouldBe` replicate 2 (Just (A.String "Properties"))
    it "decodes a subpacket of a method response followed by the TPer's End of Session as both, in order, in the JSON's items and the Markdown (Hsed.Core.Stream); Empty atoms alone are counted" $ do
      let h       = SessionHandle 0x1004 4096 1
          result  = MethodResponse [L (List [N ((3 :: Natural) .: ("abc" :: ByteString))])] 0
          answer  = either (error . show) id (sessionComPacket h (generate result <> generate EndOfSession))
          empties = either (error . show) id (controlComPacket 0x1004 (B.replicate 4 0xFF))
          recv    = Request IfRecv 1 0x1004 2048 mempty
          rr      = sample { rrTranscript = [ Issued 1 "tper" recv (Response Good (answer <> B.replicate (2048 - B.length answer) 0))
                                            , Issued 2 "tper" (Request IfSend 1 0x1004 512 (empties <> B.replicate (512 - B.length empties) 0)) (Response Good mempty) ] }
      (_, json, md) <- written rr
      let subpackets side = [ sp | e <- arr (at "transcript" json), p <- arr (atPath ["decode", side, "packets"] e), sp <- arr (at "subpackets" p) ]
      [ (at "method_response" i, at "end_of_session" i) | sp <- subpackets "response", i <- arr (at "items" sp) ]
        `shouldBe` [(Just (A.String (tshow result)), Nothing), (Nothing, Just (A.Bool True))]
      [ (at "items" sp, at "empty_atoms" sp) | sp <- subpackets "request" ] `shouldBe` [(Just (A.Array mempty), Just (A.Number 4))]
      md `shouldSatisfy` T.isInfixOf ("results " <> tshow (_results result) <> ", status 0; EndOfSession")
      md `shouldSatisfy` T.isInfixOf ": 4 Empty atoms"
    it "prints a prerequisite as established or checked where the case's own script skipped it, and as not reached where the target or an unmet prerequisite stopped the case first" $ do
      let skippedBy established = sample { rrCases = [CaseResult (CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing) (Skipped "why") [] [] [] [] established] }
      (_, json, own) <- written (skippedBy True)
      own `shouldSatisfy` T.isInfixOf "SyncIdle (Test Cases 5.2 item 1, PDF 29): established or checked by the run"
      map (at "prerequisites") (arr (at "cases" json)) `shouldBe` [Just (A.String "established")]
      (_, json', earlier) <- written (skippedBy False)
      earlier `shouldSatisfy` T.isInfixOf "SyncIdle (Test Cases 5.2 item 1, PDF 29): not reached"
      map (at "prerequisites") (arr (at "cases" json')) `shouldBe` [Just (A.String "not reached")]
    it "counts JUnit's tests, failures and skips, an extension a test, the exit code one" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        _ <- writeReports dir sample
        xml <- readFileUtf8 (dir </> "jig-sim-20261030T120000Z.xml")
        xml `shouldSatisfy` T.isInfixOf "<testsuites name=\"hsed-jig\" tests=\"5\" failures=\"2\" skipped=\"1\">"
        xml `shouldSatisfy` T.isInfixOf "<failure message=\"step 1: not so\"/>"
        xml `shouldSatisfy` T.isInfixOf "<failure message=\"exit 1: SPF-02:1 fail; ETC-02 skipped\"/>"
    it "fails in JUnit the pair, each problem, a drive command outside its policy and the exit code, as the exit code does" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        paths <- writeReports dir paired
        xml <- readFileUtf8 (fromMaybe "" (L.find (".xml" `L.isSuffixOf`) paths))
        xml `shouldSatisfy` T.isInfixOf "tests=\"6\" failures=\"4\" skipped=\"1\""
        for_ [ "<failure message=\"missing 0x0403\"/>", "<failure message=\"--capture-level0 x.bin: no drive to capture\"/>"
             , "<failure message=\"drive commands outside their policy (design 9.3): 1: on the drive-replay, if_send protocol 0x01 ComID 0x1004, 512 bytes: Properties" ]
          $ \f -> xml `shouldSatisfy` T.isInfixOf f
    it "gives a pair's each end's verdict, and the problems, in JSON and Markdown; the simulator's options only where it ran" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        paths <- writeReports dir paired
        json <- readFileBinary (fromMaybe "" (L.find (".json" `L.isSuffixOf`) paths)) >>= either throwString pure . A.eitherDecodeStrict'
        md <- readFileUtf8 (fromMaybe "" (L.find (".md" `L.isSuffixOf`) paths))
        [ (at "end" e, at "verdict" e) | Just (A.Array cs) <- [at "cases" json], c <- toList cs, Just (A.Array es) <- [at "ends" c], e <- toList es ]
          `shouldBe` [(Just "tper", Just "pass"), (Just "drive-replay", Just "skipped")]
        at "problems" json `shouldBe` Just (A.toJSON ["--capture-level0 x.bin: no drive to capture" :: Text])
        md `shouldSatisfy` T.isInfixOf "- drive-replay: skipped: no Level 0"
        md `shouldSatisfy` T.isInfixOf "## Problems"
        md `shouldSatisfy` T.isInfixOf "- simulator: units"
        (_, _, replayOnly) <- written sample { rrTargets = drop 1 (rrTargets paired) }
        replayOnly `shouldNotSatisfy` T.isInfixOf "- simulator: units"
    it "suffixes a second run's reports of the same second, overwriting none" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        one <- writeReports dir sample
        two <- writeReports dir quiet
        three <- writeReports dir quiet
        map (L.drop (length dir + 1)) (one <> two <> three) `shouldBe`
          [ "jig-sim-20261030T120000Z" <> s <> e | s <- ["", "-2", "-3"], e <- [".json", ".md", ".xml"] ]
    it "claims each name exclusively: runs started together write different reports, each its own, none over another's" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        written' <- mapConcurrently (\n -> writeReports dir sample { rrVersion = tshow n }) [1 .. 6 :: Int]
        L.sort (map (L.drop (length dir + 1)) (concat written')) `shouldBe`
          L.sort [ "jig-sim-20261030T120000Z" <> s <> e | s <- "" : [ "-" <> show n | n <- [2 .. 6 :: Int] ], e <- [".json", ".md", ".xml"] ]
        versions <- for (filter (".json" `L.isSuffixOf`) (concat written')) $ \f ->
          readFileBinary f >>= either throwString (pure . at "hsed") . A.eitherDecodeStrict'
        L.sort versions `shouldBe` map (Just . A.String . tshow) [1 .. 6 :: Int]

  describe "M4c's keys in the JSON" $ do
    it "are empty or null where a run had none of them" $ do
      (_, json, _) <- written sample
      [ at k json | k <- ["policies", "outside_policy", "blocked", "interrupted", "preflight", "recover", "properties", "pair_properties"] ]
        `shouldBe` [Just (A.Array mempty), Just (A.Array mempty), Just (A.Number 0), Just A.Null, Just (A.Array mempty), Just A.Null
                   , Just (A.Array mempty), Just A.Null]
    it "give each drive's policy, the commands outside it, those blocked, why the run stopped, the pre-flight's notes, --recover's outcome, each end's Properties with the host's, and the pair's" $ do
      (_, json, _) <- written m4cFields
      at "policies" json `shouldBe` Just (A.toJSON [A.object
        [ "end" A..= ("drive" :: Text), "policy" A..= ("allow_list" :: Text), "base_comid" A..= (4100 :: Int)
        , "allowed" A..= (["properties", "start_session_admin_sp", "get_c_pin_msid", "end_of_session"] :: [Text]) ]])
      [ (at "end" o, atPath ["request", "op"] o, atPath ["request", "comid"] o) | o <- arr (at "outside_policy" json) ]
        `shouldBe` [(Just "drive", Just "if_send", Just (A.Number 4100))]
      (at "blocked" json, at "interrupted" json, at "preflight" json)
        `shouldBe` (Just (A.Number 1), Just "SIGINT", Just (A.toJSON ["/dev/disk22s1 on /Volumes/SED NVME (msdos), allowed" :: Text]))
      at "recover" json `shouldBe` Just (A.object ["ok" A..= True, "text" A..= ("STACK_RESET on 0x1004: Success" :: Text)])
      at "properties" json `shouldBe` Just (A.toJSON [A.object
        [ "end" A..= ("drive" :: Text), "host_sent" A..= A.object ["MaxComPacketSize" A..= (4096 :: Int)]
        , "tper" A..= A.object ["MaxComPacketSize" A..= (2048 :: Int), "MaxSessions" A..= (1 :: Int)]
        , "host" A..= A.object ["MaxComPacketSize" A..= (4096 :: Int)] ]])
      (atPath ["pair_properties", "compared"] json, atPath ["pair_properties", "reason"] json)
        `shouldBe` (Just (A.Bool False), Just "one end ran no UCT-02")
    it "mask a secret the new texts quote, and one in a command outside the policy" $ do
      let quoting = m4cFields { rrSecrets = [msid], rrInterrupted = Just ("stopped reading " <> decodeUtf8Lenient msid)
                              , rrOutsidePolicy = [("drive", Request IfSend 1 0x1004 512 (msid <> B.replicate (512 - B.length msid) 0))] }
      (json, _, _) <- texts quoting
      json `shouldNotSatisfy` T.isInfixOf (decodeUtf8Lenient msid)
      json `shouldNotSatisfy` T.isInfixOf (hexOf msid)

  describe "the secrets a run read (design 9.3 item 5)" $ do
    it "gives a secret by its length and the first 12 hex digits of its SHA-256, never its bytes" $
      describeSecret msid `shouldBe` "32 bytes, sha256 a6c4ca3911e4"
    it "masks the MSID in the JSON transcript's bytes and decodes, in the Markdown and in the JUnit properties, each report giving its length and digest" $ do
      (json, md, xml) <- texts (secretive False)
      for_ [json, md, xml] $ \report -> do
        report `shouldNotSatisfy` T.isInfixOf (hexOf msid)
        report `shouldNotSatisfy` T.isInfixOf (decodeUtf8Lenient msid)
        report `shouldSatisfy` T.isInfixOf "a6c4ca3911e4"
      json `shouldSatisfy` T.isInfixOf "\"secrets\":[{\"len\":32,\"sha256\":\"a6c4ca3911e4\",\"shown\":false}]"
      json `shouldSatisfy` T.isInfixOf (T.replicate 32 "*")
      json `shouldSatisfy` T.isInfixOf (hexOf (B.replicate 32 0x2A))
      md `shouldSatisfy` T.isInfixOf "## Secrets"
      md `shouldSatisfy` T.isInfixOf "- secret 1: 32 bytes, sha256 a6c4ca3911e4"
      md `shouldSatisfy` T.isInfixOf "- secrets read: 1, masked as 0x2A in the transcript (--show-msid shows them)"
      xml `shouldSatisfy` T.isInfixOf "<property name=\"secrets\" value=\"1\"/>"
      xml `shouldSatisfy` T.isInfixOf "<property name=\"secret_1\" value=\"32 bytes, sha256 a6c4ca3911e4, masked as 0x2A in the transcript (--show-msid shows them)\"/>"
    it "shows the secrets in clear with --show-msid, and says so" $ do
      (json, md, xml) <- texts (secretive True)
      json `shouldSatisfy` T.isInfixOf (hexOf msid)
      json `shouldSatisfy` T.isInfixOf "\"shown\":true"
      md `shouldSatisfy` T.isInfixOf "shown in clear (--show-msid)"
      xml `shouldSatisfy` T.isInfixOf "32 bytes, sha256 a6c4ca3911e4, shown in clear (--show-msid)"
    it "gives the simulator's SID PIN among the options by its length and digest alone, in JSON and Markdown" $ do
      (json, md, _) <- texts (secretive False)
      json `shouldSatisfy` T.isInfixOf "\"sim_sid_pin\":\"4 bytes, sha256 "
      json `shouldNotSatisfy` T.isInfixOf "SID!"
      json `shouldNotSatisfy` T.isInfixOf (hexOf "SID!")
      md `shouldSatisfy` T.isInfixOf ", SID PIN given: 4 bytes, sha256 "
      (defaultJson, _, _) <- texts sample
      defaultJson `shouldSatisfy` T.isInfixOf "\"sim_sid_pin\":null"
    it "says in the summary how many secrets were read and how they are printed" $ do
      summary (secretive False) [] `shouldSatisfy` any (T.isPrefixOf "secrets read: 1, masked as 0x2A in the transcript")
      summary sample [] `shouldNotSatisfy` any (T.isPrefixOf "secrets read")
    it "masks a secret quoted in a verdict's reason, an extension's, an observation, a waiver's reason, a note or a problem, as text, hex or as shown, in all three reports and the summary; in clear with --show-msid" $ do
      let quoting shown = (secretive shown)
            { rrCases = [ CaseResult (CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing)
                            (Fail ["an answer that does not read: Bytes " <> tshow msid <> " where End of Data is expected"])
                            [lockingSPUntouched { wReason = "waived over " <> hexOf msid }]
                            [("an extension", Skipped ("skipped over " <> decodeUtf8Lenient msid))]
                            [("seen", "the MSID " <> decodeUtf8Lenient msid)] [] True ]
            , rrTranscript = rrTranscript (secretive shown) <> [Note 3 ("a note quoting " <> decodeUtf8Lenient msid)]
            , rrProblems   = ["a problem quoting " <> hexOf msid] }
      (json, md, xml) <- texts (quoting False)
      for_ [json, md, xml] $ \report -> do
        report `shouldNotSatisfy` T.isInfixOf (hexOf msid)
        report `shouldNotSatisfy` T.isInfixOf (decodeUtf8Lenient msid)
      md `shouldSatisfy` T.isInfixOf ("- an answer that does not read: Bytes \"" <> T.replicate 32 "*" <> "\" where End of Data is expected")
      md `shouldSatisfy` T.isInfixOf ("- a problem quoting " <> T.replicate 32 "*")
      summary (quoting False) [] `shouldNotSatisfy` any (T.isInfixOf (decodeUtf8Lenient msid))
      (shownJson, shownMd, _) <- texts (quoting True)
      shownJson `shouldSatisfy` T.isInfixOf (decodeUtf8Lenient msid)
      shownMd `shouldSatisfy` T.isInfixOf ("a note quoting " <> decodeUtf8Lenient msid)

  describe "verifyTranscript" $ do
    let report dir rr = writeReports dir rr <&> fromMaybe "" . L.find (".json" `L.isSuffixOf`)
    it "fails a transcript whose drive was issued an IF-SEND when asked to, not otherwise" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        r <- report dir onDrive
        silently (verifyTranscript r True False) `shouldReturn` ExitFailure 1
        silently (verifyTranscript r False False) `shouldReturn` ExitSuccess
    it "passes one whose IF-SENDs are the simulator's, a pair's, or that issued none, a blocked IF-SEND being none" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        r1 <- report dir sample
        silently (verifyTranscript r1 True False) `shouldReturn` ExitSuccess
        r2 <- report dir quiet
        silently (verifyTranscript r2 True False) `shouldReturn` ExitSuccess
    it "with --level0-only, fails a drive issued anything but Level 0 Discovery's IF-RECV" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        r1 <- report dir (recvs [(0x01, 0x0001)])
        silently (verifyTranscript r1 True True) `shouldReturn` ExitSuccess
        r2 <- report dir (recvs [(0x01, 0x0001), (0x01, 0x1004)])
        silently (verifyTranscript r2 True True) `shouldReturn` ExitFailure 1
        silently (verifyTranscript r2 True False) `shouldReturn` ExitSuccess
        r3 <- report dir (recvs [(0x02, 0x0001)])
        silently (verifyTranscript r3 False True) `shouldReturn` ExitFailure 1
        r4 <- report dir onDrive
        silently (verifyTranscript r4 False True) `shouldReturn` ExitFailure 1
    it "counts an issued entry that names no end as a drive's" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        let entry = A.object [ "n" A..= (1 :: Int), "entry" A..= ("issued" :: Text)
                             , "request" A..= A.object ["op" A..= ("if_send" :: Text), "proto" A..= (1 :: Int), "comid" A..= (4100 :: Int), "len" A..= (512 :: Int)]
                             , "response" A..= A.object ["status" A..= ("good" :: Text), "len" A..= (0 :: Int)] ]
        writeFileBinary (dir </> "old.json") (toStrictBytes (A.encode (A.object ["transcript" A..= [entry]])))
        silently (verifyTranscript (dir </> "old.json") True False) `shouldReturn` ExitFailure 1
        silently (verifyTranscript (dir </> "old.json") False False) `shouldReturn` ExitSuccess
    it "counts an entry that does not read against both checks, and a note or a blocked command against neither" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        let transcriptOf name es = (dir </> name) <$ writeFileBinary (dir </> name) (toStrictBytes (A.encode (A.object ["transcript" A..= es])))
            note    = A.object ["n" A..= (1 :: Int), "entry" A..= ("note" :: Text), "text" A..= ("a note" :: Text)]
            blocked = A.object [ "n" A..= (2 :: Int), "entry" A..= ("blocked" :: Text), "end" A..= ("drive" :: Text)
                               , "request" A..= A.object ["op" A..= ("if_send" :: Text), "proto" A..= (1 :: Int), "comid" A..= (4100 :: Int), "len" A..= (512 :: Int)]
                               , "reason" A..= ("a Set" :: Text) ]
        quietOnes <- transcriptOf "quiet.json" [note, blocked]
        silently (verifyTranscript quietOnes True True) `shouldReturn` ExitSuccess
        forM_ [ A.object ["n" A..= (3 :: Int), "entry" A..= ("issued" :: Text), "end" A..= ("drive" :: Text)]
              , A.object ["n" A..= (3 :: Int), "entry" A..= ("sent" :: Text)]
              , A.String "an entry" ] $ \unreadable -> do
          r <- transcriptOf "unreadable.json" [note, unreadable]
          silently (verifyTranscript r True False) `shouldReturn` ExitFailure 1
          silently (verifyTranscript r False True) `shouldReturn` ExitFailure 1
          silently (verifyTranscript r False False) `shouldReturn` ExitSuccess
    it "fails a file that is no report" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        writeFileUtf8 (dir </> "not.json") "not JSON"
        silently (verifyTranscript (dir </> "not.json") False False) `shouldReturn` ExitFailure 1
        silently (verifyTranscript (dir </> "absent.json") False False) `shouldReturn` ExitFailure 1

  describe "diffReports" $
    it "compares two reports' Level 0 responses as the Pair runner does" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        let level0 ds = Level0 mempty 0 [ Descriptor c 1 (B.replicate 12 (fromIntegral c)) | c <- ds ] mempty []
            report sub ds = writeReports (dir </> sub) quiet { rrLevel0 = [("tper", level0 ds)] } >>= \paths -> case paths of
              json : _ -> pure json
              []       -> throwString "no report written"
        sim <- report "a" [0x0001, 0x0005]
        drive <- report "b" [0x0001, 0x0403]
        fewer <- report "c" [0x0001]
        silently (diffReports sim sim) `shouldReturn` ExitSuccess
        silently (diffReports sim drive) `shouldReturn` ExitFailure 1
        silently (diffReports sim fewer) `shouldReturn` ExitSuccess

  describe "M4c's renderings: a drive's policy, the commands outside it and those the Guard refused (design 9.3)" $ do
    it "prints each drive's policy, each command outside it and the Guard's refusals in the Markdown's Policies" $ do
      (_, md, _) <- reportsOf onDriveM4c
      for_ [ "## Policies"
           , "- drive: AllowList on base ComID 0x1004: Properties, StartSession to the Admin SP as Anybody, Get on C_PIN_MSID's UID and PIN, End of Session, STACK_RESET and its GET_COMID_RESPONSE"
           , "- outside its policy, on the drive: if_send protocol 0x01 ComID 0x1004, 512 bytes: Set"
           , "- commands the Guard refused: 1" ] $ \l -> md `shouldSatisfy` T.isInfixOf l
    it "fails JUnit's drive cases, renamed and added, and names the commands outside the policy for the exit code's 3" $ do
      (_, _, xml) <- reportsOf onDriveM4c
      xml `shouldNotSatisfy` T.isInfixOf "no IF-SEND issued on a drive"
      for_ [ "name=\"no drive command outside its policy (design 9.3)\">"
           , "<failure message=\"drive commands outside their policy (design 9.3): 1: on the drive, if_send protocol 0x01 ComID 0x1004, 512 bytes: Set"
           , "name=\"the Guard refused no command (design 9.3)\">"
           , "<failure message=\"commands the Guard refused, each blocked in the transcript (design 9.3): 1\"/>"
           , "<failure message=\"exit 3: drive commands outside their policy (design 9.3): 1: on the drive, if_send"
           , "<property name=\"outside_policy\" value=\"1\"/>", "<property name=\"blocked\" value=\"1\"/>" ]
        $ \l -> xml `shouldSatisfy` T.isInfixOf l
    it "passes JUnit's drive cases where nothing was outside the policy or refused, and prints none outside it" $ do
      (_, md, xml) <- reportsOf onDriveM4c { rrOutsidePolicy = [], rrBlocked = 0, rrExit = ExitSuccess }
      for_ ["name=\"no drive command outside its policy (design 9.3)\"/>", "name=\"the Guard refused no command (design 9.3)\"/>"] $ \l ->
        xml `shouldSatisfy` T.isInfixOf l
      md `shouldSatisfy` T.isInfixOf "- outside a drive's policy: none\n- commands the Guard refused: 0\n"
    it "prints no Policies and no drive case where no drive was an end" $ do
      (_, md, xml) <- reportsOf sample
      md `shouldNotSatisfy` T.isInfixOf "## Policies"
      xml `shouldNotSatisfy` T.isInfixOf "outside its policy"
    it "says in the summary each drive's policy, the IF-SENDs, the commands outside it, each named, and the refusals" $ do
      let printed = summary onDriveM4c []
      for_ [ "policy on the drive: AllowList on base ComID 0x1004: Properties, StartSession to the Admin SP as Anybody, Get on C_PIN_MSID's UID and PIN, End of Session, STACK_RESET and its GET_COMID_RESPONSE"
           , "IF-SENDs issued on a drive: 0; drive commands outside their policy: 1; refused by the Guard: 1" ] $ \l ->
        printed `shouldSatisfy` elem l
      printed `shouldSatisfy` any (T.isPrefixOf "  outside its policy, on the drive: if_send protocol 0x01 ComID 0x1004, 512 bytes: Set")

  describe "M4c's renderings: a run stopped, the pre-flight's notes, --recover" $ do
    let why     = "SIGINT: session TSN 4097 on ComID 0x1004 left open (End of Session not sent): hsed-jig --recover"
        stopped = onDriveM4c { rrOutsidePolicy = [], rrBlocked = 0, rrInterrupted = Just why, rrExit = ExitFailure 130 }
    it "prints why a run was stopped: its Markdown section, a failed JUnit case, the exit code's 130, the summary" $ do
      (json, md, xml) <- reportsOf stopped
      at "interrupted" json `shouldBe` Just (A.String why)
      md `shouldSatisfy` T.isInfixOf ("- exit: 130\n\n## Interrupted\n\n- " <> why <> "\n\n## Targets")
      xml `shouldSatisfy` T.isInfixOf ("name=\"interrupted\">\n      <failure message=\"" <> why <> "\"/>")
      xml `shouldSatisfy` T.isInfixOf ("<failure message=\"exit 130: interrupted: " <> why <> "; SPF-02:1 fail")
      summary stopped [] `shouldSatisfy` elem ("interrupted: " <> why)
      (_, _, unstopped) <- reportsOf onDriveM4c
      unstopped `shouldNotSatisfy` T.isInfixOf "name=\"interrupted\""
    it "prints the pre-flight's notes, and what --recover did: recovered, or not, its JUnit case failed and the exit code's reason" $ do
      let note'     = "/dev/disk22s1 mounted on /Volumes/SED NVME (msdos): allowed by --allow-mounted (design 9.3 item 4)"
          recovered = "STACK_RESET on 0x1004: Success; the ComID idle; the Admin SP took a read-write session (TSN 4097) and ended it"
          notReset  = "STACK_RESET on 0x1004: Success/Failure 1, the stack not reset (Core Table 36, PDF 66)"
          ran o     = onDriveM4c { rrOutsidePolicy = [], rrBlocked = 0, rrCases = [], rrPreflight = [note'], rrRecover = Just o
                                 , rrExit = either (const (ExitFailure 1)) (const ExitSuccess) o }
      (_, md, xml) <- reportsOf (ran (Right recovered))
      md `shouldSatisfy` T.isInfixOf ("## Pre-flight\n\n- " <> note' <> "\n\n## Targets")
      md `shouldSatisfy` T.isInfixOf ("## Recover: STACK_RESET on the base ComID (design 9.3)\n\n- recovered: " <> recovered <> "\n\n## Cases")
      xml `shouldSatisfy` T.isInfixOf "name=\"recover: STACK_RESET\"/>"
      summary (ran (Right recovered)) [] `shouldSatisfy` \ls -> ("pre-flight: " <> note') `elem` ls && ("recover: " <> recovered) `elem` ls
      (_, mdFailed, xmlFailed) <- reportsOf (ran (Left notReset))
      mdFailed `shouldSatisfy` T.isInfixOf ("- not recovered: " <> notReset)
      xmlFailed `shouldSatisfy` T.isInfixOf ("name=\"recover: STACK_RESET\">\n      <failure message=\"" <> notReset <> "\"/>")
      xmlFailed `shouldSatisfy` T.isInfixOf ("<failure message=\"exit 1: --recover did not recover: " <> notReset <> "\"/>")
      summary (ran (Left notReset)) [] `shouldSatisfy` elem ("recover: not recovered: " <> notReset)

  describe "M4c's renderings: the pair's Properties (design 13.32 (6))" $ do
    let allowed  = PropDiff "MaxComPacketSize" TPerSide (Just 2048) (Just 65536) "TPer minimum 2048 (Opal 2.30 Table 17, PDF 33)" Allowed
        compared = Compared [allowed, PropDiff "MaxPacketSize" TPerSide (Just 2028) (Just 2000) "TPer minimum 2028 (Opal 2.30 Table 17, PDF 33)" NotAllowed]
                            [] ["MaxReadSessions"]
        title = "## Pair: Properties, the simulator's against the drive's (Opal 2.30 Table 17)"
    it "prints a comparison not made with why, in the Markdown and the summary, with no JUnit case" $ do
      let rr = paired { rrPairProperties = Just (NotCompared "one end ran no UCT-02") }
      (_, md, xml) <- reportsOf rr
      md `shouldSatisfy` T.isInfixOf (title <> "\n\n- not compared: one end ran no UCT-02\n\n")
      xml `shouldNotSatisfy` T.isInfixOf "Properties against"
      summary rr [] `shouldSatisfy` elem "pair properties: not compared: one end ran no UCT-02"
    it "prints a comparison made: each judgement counted, the verdict; a JUnit case failed by a difference not allowed, and the exit code's reason" $ do
      let rr = paired { rrPairProperties = Just compared, rrOutsidePolicy = [], rrExit = ExitFailure 1 }
      (json, md, xml) <- reportsOf rr
      atPath ["pair_properties", "compared"] json `shouldBe` Just (A.Bool True)
      for_ [title, "- differences: 1 allowed, 0 unchecked (outside Table 17), 1 not allowed", "- fail\n"] $ \l -> md `shouldSatisfy` T.isInfixOf l
      for_ [ "name=\"Pair: the simulator&apos;s Properties against the drive&apos;s (Opal 2.30 Table 17)\">"
           , "<failure message=\"MaxPacketSize (tper): the drive&apos;s 2000, not allowed by TPer minimum 2028 (Opal 2.30 Table 17, PDF 33)\"/>"
           , "the pair&apos;s Properties fail (Opal 2.30 Table 17)" ] $ \l -> xml `shouldSatisfy` T.isInfixOf l
      summary rr [] `shouldSatisfy` any (T.isPrefixOf "pair properties: differences: 1 allowed, 0 unchecked (outside Table 17), 1 not allowed")
    it "passes the JUnit case where every difference is allowed" $ do
      (_, _, xml) <- reportsOf paired { rrPairProperties = Just (Compared [allowed] [] ["MaxReadSessions"]) }
      xml `shouldSatisfy` T.isInfixOf "name=\"Pair: the simulator&apos;s Properties against the drive&apos;s (Opal 2.30 Table 17)\"/>"

  describe "M4c's renderings: a secret under four bytes (design 9.3 item 5)" $
    it "is marked short in the JSON and warned of in the Markdown, the JUnit properties and the summary; a longer one is not" $ do
      let short = "SID" :: ByteString
          warning = "short: masked wherever its bytes stand, the reports' own words and numbers among them (design 9.3 item 5)"
          rr = sample { rrSecrets = [msid, short] }
      (json, md, xml) <- reportsOf rr
      [ at "short" e | e <- elements (at "secrets" json) ] `shouldBe` [Nothing, Just (A.Bool True)]
      md `shouldSatisfy` T.isInfixOf ("- secret 2: " <> describeSecret short <> "; " <> warning)
      md `shouldNotSatisfy` T.isInfixOf ("- secret 1: " <> describeSecret msid <> ";")
      xml `shouldSatisfy` T.isInfixOf "; short: masked wherever its bytes stand, the reports&apos; own words and numbers among them (design 9.3 item 5)\"/>"
      summary rr [] `shouldSatisfy` any (\l -> "secrets read: 2" `T.isPrefixOf` l && (describeSecret short <> "; " <> warning) `T.isSuffixOf` l)

  describe "reportProperties" $
    it "reads each end's Properties exchange back from a report, each table by name; none from a report without them" $ do
      (json, _, _) <- reportsOf m4cFields
      reportProperties json `shouldBe` Right (Just [ ("drive", [("MaxComPacketSize", 4096)]
                                                     , PropertiesAnswer [("MaxComPacketSize", 2048), ("MaxSessions", 1)] (Just [("MaxComPacketSize", 4096)])) ])
      reportProperties (A.object []) `shouldBe` Right Nothing

  describe "verifyTranscript with --allow-list (verifyTranscriptWith)" $ do
    for_ [(True, "STACK_RESET before each case (the shipped default)"), (False, "an idle IF-RECV before each case")] $ \(resets, mode) ->
      it ("passes the M4c acceptance's cases on the simulator as a drive, " <> mode <> ", the base read from the report's Level 0") $
        withSystemTempDirectory "hsed-report" $ \dir -> do
          (report, verdicts) <- acceptanceReport resets dir
          verdicts `shouldBe` [Pass, Pass, Pass]
          (code, out) <- capturing (verifyTranscriptWith report False False True)
          code `shouldBe` ExitSuccess
          out `shouldSatisfy` T.isInfixOf "--allow-list: the drive's base ComID, as its Level 0 names it: 0x1004"
          out `shouldSatisfy` T.isInfixOf (if resets then " 16 admitted, 0 refused, 0 that do not read; blocked by the run's Guard: 0"
                                                    else " 13 admitted, 0 refused, 0 that do not read; blocked by the run's Guard: 0")
    it "fails the same report with one drive command a Set, a STACK_RESET on 0x0004, a Get on C_PIN_SID, an entry that does not read, or a Blocked entry added" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        (report, _) <- acceptanceReport True dir
        silently (verifyTranscriptWith report False False True) `shouldReturn` ExitSuccess
        let unreadable = A.object ["n" A..= (5 :: Int), "entry" A..= ("issued" :: Text), "end" A..= ("drive" :: Text)]
            blocked    = A.object [ "n" A..= (99 :: Int), "entry" A..= ("blocked" :: Text), "end" A..= ("drive" :: Text)
                                  , "request" A..= setSend, "reason" A..= ("AllowList: the method Set is not allowed" :: Text) ]
            refused    = "on the drive refused: "
            edits      = [ ("set", editFirst issuedSend (withRequest setSend), refused)
                         , ("tper-reset", editFirst issuedSend (withRequest resetOnTperResetComId), refused)
                         , ("get-sid", editFirst issuedSend (withRequest getSidSend), refused)
                         , ("unreadable", editFirst issuedSend (const unreadable), "#5 does not read as a command a report prints: refused")
                         , ("blocked", (<> [blocked]), "0 refused, 0 that do not read; blocked by the run's Guard: 1") ]
        for_ edits $ \(name, f, said) -> do
          path <- edited (dir </> (name <> ".json")) (editKey "transcript" (editEach f)) report
          (code, out) <- capturing (verifyTranscriptWith path False False True)
          (name, code, said `T.isInfixOf` out) `shouldBe` (name, ExitFailure 1, True)
    it "fails a report whose Level 0 names another base than the drive's policy is bound to, or none" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        (report, _) <- acceptanceReport True dir
        other <- edited (dir </> "other-base.json") (editDescriptors (rebased "1005")) report
        (code, out) <- capturing (verifyTranscriptWith other False False True)
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` T.isInfixOf "--allow-list: the drive's policy is bound to 0x1004, not the 0x1005 its Level 0 names"
        none <- edited (dir </> "no-base.json") (editDescriptors (filter ((/= Just (A.Number 0x0203)) . at "code"))) report
        (code', out') <- capturing (verifyTranscriptWith none False False True)
        code' `shouldBe` ExitFailure 1
        out' `shouldSatisfy` T.isInfixOf "--allow-list: the drive's base ComID, as its Level 0 names it: none"
        out' `shouldSatisfy` T.isInfixOf "refused: no base ComID in its Level 0, so Level 0 Discovery alone: "
    it "passes an M3 report of Level 0 Discovery alone, and fails one whose drive was issued an IF-SEND with no base ComID to hold it to" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        level0Only <- writeReports dir (recvs [(0x01, 0x0001)]) <&> fromMaybe "" . L.find (".json" `L.isSuffixOf`)
        silently (verifyTranscriptWith level0Only True True True) `shouldReturn` ExitSuccess
        sending <- writeReports dir onDrive <&> fromMaybe "" . L.find (".json" `L.isSuffixOf`)
        silently (verifyTranscriptWith sending False False True) `shouldReturn` ExitFailure 1
        silently (verifyTranscriptWith sending False False False) `shouldReturn` ExitSuccess

  describe "diffReports with M4c's Properties exchanges" $
    it "compares the reports' last ends' Properties as the Pair's Properties are, where both carry them, and prints nothing of them where one does not" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        let level0 = Level0 mempty 0 [Descriptor 0x0001 1 (B.replicate 12 1)] mempty []
            exchange tperMax = ( [("MaxComPacketSize", 4096)]
                               , PropertiesAnswer [("MaxComPacketSize", tperMax), ("MaxPacketSize", 2028)] (Just [("MaxComPacketSize", 4096)]) )
            report sub end ex = writeReports (dir </> sub) quiet
              { rrTargets    = [Describe end "a test" UnitBlocks512 Nothing Nothing Nothing Nothing]
              , rrLevel0     = [(end, level0)]
              , rrProperties = [(end, fst ex, snd ex)] }
              <&> fromMaybe "" . L.find (".json" `L.isSuffixOf`)
            title = "## Pair: Properties, A's simulator's against B's drive's (Opal 2.30 Table 17)"
        sim <- report "a" "tper" (exchange 2048)
        drive <- report "b" "drive" (exchange 65536)
        (code, out) <- capturing (diffReports sim drive)
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` T.isInfixOf title
        let expected = case pairProperties (Just (exchange 2048)) (Just (exchange 65536)) of
              NotCompared why -> ["not compared: " <> why]
              pp              -> propertiesLines pp
        for_ expected $ \l -> out `shouldSatisfy` T.isInfixOf l
        older <- edited (dir </> "older.json") (\v -> case v of A.Object o -> A.Object (KM.delete "properties" o); _ -> v) sim
        (code', out') <- capturing (diffReports older drive)
        code' `shouldBe` ExitSuccess
        out' `shouldNotSatisfy` T.isInfixOf "Properties"
