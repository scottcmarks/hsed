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
PDF 29); the simulator's options only where a simulator ran; the run's
problems; the JUnit report failing the pair, each problem, an IF-SEND on a
drive and the exit code wherever the exit code fails; the
transcript with each IF-SEND's methods decoded and each command's end; the
JUnit counts. @verify-transcript@ fails a report whose drive was issued an
IF-SEND only when asked to, the simulator's IF-SENDs counting for nothing,
and with @--level0-only@ one whose drive was issued anything but Level 0
Discovery's IF-RECV; an entry of no end counts as a drive's, and one that
does not read against both; a file that is no report fails; @diff@ compares two reports' Level 0 responses as the Pair
runner does.
-}
module Hsed.Jig.ReportSpec (spec) where

import qualified Data.Aeson            as A
import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import           RIO.FilePath          ((</>))
import qualified RIO.Text              as T
import           RIO.Time              (UTCTime (..), fromGregorian)
import           Test.Hspec

import           Hsed.Core.Method      (invocation)
import           Hsed.Core.StreamItem  (StreamItem (..))
import qualified Hsed.Gen.Core.Methods as Methods
import           Hsed.Gen.Core.UIDs    (uSMUID)
import           Hsed.Jig.Catalogue    (CaseKey (..), TestId (..))
import           Hsed.Jig.Options      (RunOptions (..), TargetSpec (..), defaultRunOptions)
import           Hsed.Jig.Pair         (PairReport (..))
import           Hsed.Jig.Prereq       (lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Report
import           Hsed.Jig.Script       (Verdict (..), controlComPacket)
import           Hsed.Jig.Support      (at, atPath, silently)
import           Hsed.Jig.Transcript   (TEntry (..))
import           Hsed.Level0           (Descriptor (..), Level0 (..))
import           Hsed.Wire             (Describe (..), Dir (..), IfStatus (..), Request (Request), Response (Response), Unit (..))


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
  { rrOptions    = defaultRunOptions
  , rrVersion    = "Version 0.1.0.1 [test]"
  , rrStarted    = noon
  , rrTargets    = [Describe "tper" "hsed-sim test" UnitBytes (Just "opal-2.30") (Just 0x1004) (Just 1) Nothing]
  , rrCases      =
      [ CaseResult (CaseKey (UCT 1) Nothing Nothing Nothing) Pass (lockingSPUntouched : noSessionOpened) [] []
      , CaseResult (CaseKey (SPF 2) (Just 1) Nothing Nothing) (Fail ["step 1: not so"]) [lockingSPUntouched] [("an extension", Pass)] []
      , CaseResult (CaseKey (ETC 2) Nothing Nothing Nothing) (Skipped "needs IF-SEND") [] [] []
      ]
  , rrTranscript =
      [ Note 1 "UCT-01 on the sim"
      , Issued 2 "tper" propertiesSend (Response Good mempty)
      , Blocked 3 "drive" propertiesSend "Level0Only: no IF-SEND reaches a drive at M3 (design 9.3)"
      ]
  , rrLevel0     = []
  , rrPair       = Nothing
  , rrIfSends    = 0
  , rrProblems   = []
  , rrExit       = ExitFailure 1
  }

-- | A pair of the simulator and a replay: UCT-01 passed on the simulator,
--   skipped on the replay; the pair failing on a missing code; a capture
--   problem; an IF-SEND issued on the drive.
paired :: RunResult
paired = sample
  { rrOptions  = defaultRunOptions { roTarget = TPair TSim (TReplay "disk6.bin") }
  , rrTargets  = rrTargets sample <> [Describe "drive-replay" "hsed replay test" UnitBlocks512 Nothing Nothing Nothing (Just "disk6.bin")]
  , rrCases    = [ CaseResult (CaseKey (UCT 1) Nothing Nothing Nothing) (Skipped "replay: no Level 0") [] []
                     [("sim", Pass), ("replay", Skipped "no Level 0")] ]
  , rrPair     = Just (PairReport [0x0005] [0x0403] [] [] [])
  , rrIfSends  = 1
  , rrProblems = ["--capture-level0 x.bin: no drive to capture"]
  , rrExit     = ExitFailure 3
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

-- | A report written, read back: its files, its JSON and its Markdown.
written :: RunResult -> IO ([FilePath], A.Value, Text)
written rr = withSystemTempDirectory "hsed-report" $ \dir -> do
  paths <- writeReports dir rr
  json <- readFileBinary (dir </> "jig-sim-20261030T120000Z.json") >>= either throwString pure . A.eitherDecodeStrict'
  md <- readFileUtf8 (dir </> "jig-sim-20261030T120000Z.md")
  pure (map (L.drop (length dir + 1)) paths, json, md)

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
    it "gives each case's verdict, waivers and extensions in JSON" $ do
      (_, json, _) <- written sample
      let cases' = maybe [] (\v -> case v of A.Array xs -> toList xs; _ -> []) (at "cases" json)
      map (at "verdict") cases' `shouldBe` map (Just . A.String) ["pass", "fail", "skipped"]
      map (atPath ["key"]) cases' `shouldBe` map (Just . A.String) ["UCT-01", "SPF-02:1", "ETC-02"]
      [ atPath ["pdf"] w | Just (A.Array ws) <- [at "waivers" =<< listToMaybe cases'], w <- toList ws ]
        `shouldBe` replicate 4 (Just (A.Number 29))
      (at "if_send_issued" json, at "exit" json) `shouldBe` (Just (A.Number 0), Just (A.Number 1))
    it "decodes each IF-SEND's ComPacket in the transcript to its method" $ do
      (_, json, _) <- written sample
      let entries' = maybe [] (\v -> case v of A.Array xs -> toList xs; _ -> []) (at "transcript" json)
          methods  = [ atPath ["method_call", "method"] sp
                     | e <- entries', Just (A.Array ps) <- [atPath ["decode", "request", "packets"] e]
                     , p <- toList ps, Just (A.Array sps) <- [at "subpackets" p], sp <- toList sps ]
      methods `shouldBe` replicate 2 (Just (A.String "Properties"))
    it "counts JUnit's tests, failures and skips, an extension a test, the exit code one" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        _ <- writeReports dir sample
        xml <- readFileUtf8 (dir </> "jig-sim-20261030T120000Z.xml")
        xml `shouldSatisfy` T.isInfixOf "<testsuites name=\"hsed-jig\" tests=\"5\" failures=\"2\" skipped=\"1\">"
        xml `shouldSatisfy` T.isInfixOf "<failure message=\"step 1: not so\"/>"
        xml `shouldSatisfy` T.isInfixOf "<failure message=\"exit 1: SPF-02:1 fail; ETC-02 skipped\"/>"
    it "fails in JUnit the pair, each problem, an IF-SEND on a drive and the exit code, as the exit code does" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        paths <- writeReports dir paired
        xml <- readFileUtf8 (fromMaybe "" (L.find (".xml" `L.isSuffixOf`) paths))
        xml `shouldSatisfy` T.isInfixOf "tests=\"5\" failures=\"4\" skipped=\"1\""
        for_ [ "<failure message=\"missing 0x0403\"/>", "<failure message=\"--capture-level0 x.bin: no drive to capture\"/>"
             , "<failure message=\"1 issued\"/>" ] $ \f -> xml `shouldSatisfy` T.isInfixOf f
    it "gives a pair's each end's verdict, and the problems, in JSON and Markdown; the simulator's options only where it ran" $
      withSystemTempDirectory "hsed-report" $ \dir -> do
        paths <- writeReports dir paired
        json <- readFileBinary (fromMaybe "" (L.find (".json" `L.isSuffixOf`) paths)) >>= either throwString pure . A.eitherDecodeStrict'
        md <- readFileUtf8 (fromMaybe "" (L.find (".md" `L.isSuffixOf`) paths))
        [ (at "end" e, at "verdict" e) | Just (A.Array cs) <- [at "cases" json], c <- toList cs, Just (A.Array es) <- [at "ends" c], e <- toList es ]
          `shouldBe` [(Just "sim", Just "pass"), (Just "replay", Just "skipped")]
        at "problems" json `shouldBe` Just (A.toJSON ["--capture-level0 x.bin: no drive to capture" :: Text])
        md `shouldSatisfy` T.isInfixOf "- replay: skipped: no Level 0"
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
            report sub ds = writeReports (dir </> sub) quiet { rrLevel0 = [("sim", level0 ds)] } >>= \paths -> case paths of
              json : _ -> pure json
              []       -> throwString "no report written"
        sim <- report "a" [0x0001, 0x0005]
        drive <- report "b" [0x0001, 0x0403]
        fewer <- report "c" [0x0001]
        silently (diffReports sim sim) `shouldReturn` ExitSuccess
        silently (diffReports sim drive) `shouldReturn` ExitFailure 1
        silently (diffReports sim fewer) `shouldReturn` ExitSuccess
