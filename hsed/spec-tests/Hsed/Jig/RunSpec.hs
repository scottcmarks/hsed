{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.RunSpec
Description : Test specifications for Hsed.Jig.Run: the jig run against the simulator, a replay and both
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

M3's acceptance on the simulator (design 11; the plan's integration step 3)
as @hsed-jig run@ makes it: @--tests UCT-01,ETC-02,ETC-03,SPF-02:1@ passes
in bytes and in 512-byte blocks, each with and without a response held for
an IF-RECV; ETC-02's second IF-SEND is answered Synchronous Protocol
Violation; SPF-02:1's IF-RECV in S1 (0, 0, 0); a held run shows an IF-RECV
answered (0, 1, 0); the jig's extensions pass; the Markdown report prints
the LockingManufactured waiver with PDF 29. Then a drive-like response
synthesized from disk6's sedutil query, replayed as a drive: UCT-01 passes,
the cases that need more than Level 0 are skipped with their reasons, no
IF-SEND is issued, and the waivers of SyncIdle, NoSessions and
LockingManufactured are printed; paired with the simulator, 0x0403 is
missing without a profile and the comparison passes with it. These need the
other groups' code and pend until it is merged ("Hsed.Jig.Support"). What
the run refuses before it opens anything needs none: a key the catalogue
lacks (exit 2), a drive without root (exit 3).
-}
module Hsed.Jig.RunSpec (spec) where

import qualified Data.Aeson             as A
import qualified RIO.List               as L
import           RIO
import           RIO.Directory          (listDirectory)
import           RIO.FilePath           (takeExtension, (</>))
import qualified RIO.Text               as T
import           Test.Hspec

import           Hsed.Core.Feature      (Allowance (..), Feature (..))
import           Hsed.Gen.Opal.Features (Level0DiscoveryOpalSSCV2Feature)
import           Hsed.Jig.Catalogue     (CaseKey, parseCaseKeys)
import           Hsed.Jig.Files         (runningAsRoot)
import           Hsed.Jig.Options       (TargetSpec (..), defaultRunOptions)
import           Hsed.Jig.Report        (verifyTranscript)
import           Hsed.Jig.Run
import           Hsed.Jig.Support       (at, atPath, disk6Like, g1, g2, g4, needing, silently)
import           Hsed.Sim.FeatureSets   (DataStoreFeature)
import           Hsed.Sim.Types         (SimParams (..), defaultSimParams)
import           Hsed.Wire              (Unit (..))


keys :: Text -> [CaseKey]
keys = either (error . T.unpack) id . parseCaseKeys

-- | The four cases of M3 on the simulator, in a unit, holding a response
--   for so many IF-RECVs.
onSim :: Unit -> Int -> RunOptions
onSim u hold = defaultRunOptions
  { roTarget = TSim
  , roTests  = keys "UCT-01,ETC-02,ETC-03,SPF-02:1"
  , roSim    = defaultSimParams { spUnits = u }
  , roHold   = hold
  }

-- | A run into a temporary directory, quietly: its exit code, its JSON
--   report and its Markdown, and what else the test does in the directory.
jigIn :: FilePath -> RunOptions -> IO (ExitCode, A.Value, Text)
jigIn dir opts = do
  let reports = dir </> "report"
  code <- silently (runJig opts { roReportDir = reports })
  files <- listDirectory reports
  case ([ f | f <- files, takeExtension f == ".json" ], [ f | f <- files, takeExtension f == ".md" ]) of
    ([json], [md]) -> do
      value <- readFileBinary (reports </> json) >>= either throwString pure . A.eitherDecodeStrict'
      text <- readFileUtf8 (reports </> md)
      pure (code, value, text)
    _ -> throwString ("not one report in " <> reports <> ": " <> show files)

jig :: RunOptions -> IO (ExitCode, A.Value, Text)
jig opts = withSystemTempDirectory "hsed-jig" (`jigIn` opts)

-- | A JSON array's elements.
elements :: Maybe A.Value -> [A.Value]
elements = \v -> case v of
  Just (A.Array xs) -> toList xs
  _                 -> []

-- | Each case's key and verdict.
verdicts :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
verdicts json = [ (at "key" c, at "verdict" c) | c <- elements (at "cases" json) ]

-- | Each extension's verdict.
extensionVerdicts :: A.Value -> [Maybe A.Value]
extensionVerdicts json = [ at "verdict" x | c <- elements (at "cases" json), x <- elements (at "extensions" c) ]

-- | The rows of Table 45 the IF-RECVs of the transcript were answered with.
t45s :: A.Value -> [Text]
t45s json = [ row | e <- elements (at "transcript" json), Just (A.String row) <- [atPath ["decode", "response", "t45"] e] ]

-- | The statuses the transcript's IF-SENDs were answered with.
sendStatuses :: A.Value -> [Text]
sendStatuses json =
  [ s | e <- elements (at "transcript" json), atPath ["request", "op"] e == Just "if_send"
      , Just (A.String s) <- [atPath ["response", "status"] e] ]

str :: Text -> Maybe A.Value
str = Just . A.String

spec :: Spec
spec = do
  describe "what a run refuses before it opens anything" $ do
    it "a key the catalogue lacks: exit 2" $
      silently (runJig defaultRunOptions { roTests = keys "UCT-02" }) `shouldReturn` ExitFailure 2
    it "a drive without root, alone or paired: exit 3" $ do
      root <- runningAsRoot
      if root
        then pendingWith "running as root: the gate lets a drive through"
        else do
          silently (runJig defaultRunOptions { roTarget = TDevice "/dev/disk99" }) `shouldReturn` ExitFailure 3
          silently (runJig defaultRunOptions { roTarget = TPair TSim (TDevice "/dev/disk99") }) `shouldReturn` ExitFailure 3

  describe "UCT-01's minima" $
    it "are the ones Opal and the DataStore feature set allow, the Test Cases' (PDF 30): 1, 4, 8; 1, 0xA0000, 1" $ do
      [ L.lookup f (featureAllowances (Proxy :: Proxy Level0DiscoveryOpalSSCV2Feature))
        | f <- ["Number of ComIDs", "Number of Locking SP Admin Authorities Supported", "Number of Locking SP User Authorities Supported"] ]
        `shouldBe` map (Just . Minimum) [1, 4, 8]
      [ L.lookup f (featureAllowances (Proxy :: Proxy DataStoreFeature))
        | f <- ["Maximum number of DataStore Tables", "Maximum total size of DataStore Tables", "DataStore Table size alignment"] ]
        `shouldBe` map (Just . Minimum) [1, 0xA0000, 1]

  describe "--target sim --tests UCT-01,ETC-02,ETC-03,SPF-02:1" $ do
    for_ [(UnitBytes, 0), (UnitBlocks512, 0), (UnitBytes, 1), (UnitBlocks512, 1)] $ \(u, hold) ->
      it ("passes in " <> show u <> (if hold > 0 then ", a response held for an IF-RECV" else "")) $ needing [g1, g2] $ do
        (code, json, md) <- jig (onSim u hold)
        verdicts json `shouldBe` [ (str k, str "pass") | k <- ["UCT-01", "SPF-02:1", "ETC-02", "ETC-03"] ]
        code `shouldBe` ExitSuccess
        sendStatuses json `shouldSatisfy` elem "synchronous_protocol_violation"
        t45s json `shouldSatisfy` elem "all_returned_no_data"
        md `shouldSatisfy` T.isInfixOf "LockingManufactured (Test Cases 5.2 item 2, PDF 29) waived"
    it "held, answers an IF-RECV (0, 1, 0), and passes both extensions" $ needing [g1, g2] $ do
      (_, json, _) <- jig (onSim UnitBytes 1)
      t45s json `shouldSatisfy` elem "responses_to_come"
      extensionVerdicts json `shouldBe` [str "pass", str "pass"]
    it "not held, passes the Empty-atom extension and skips the one that needs a response held" $ needing [g1, g2] $ do
      (_, json, _) <- jig (onSim UnitBytes 0)
      extensionVerdicts json `shouldBe` [str "pass", str "skipped"]
      t45s json `shouldSatisfy` notElem "responses_to_come"

  describe "a drive-like response replayed as the drive" $ do
    it "passes UCT-01, skips the others with their reasons, issues no IF-SEND, prints the waivers" $ needing [g2, g4] $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, json, md) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01,ETC-02,ETC-03,SPF-02:1" }
        verdicts json `shouldBe` [ (str "UCT-01", str "pass"), (str "SPF-02:1", str "skipped"), (str "ETC-02", str "skipped"), (str "ETC-03", str "skipped") ]
        [ r | c <- elements (at "cases" json), Just (A.String r) <- [at "reason" c] ] `shouldSatisfy` \rs ->
          length rs == 3 && all (\r -> "Level0Only" `T.isInfixOf` r || "IF-SEND" `T.isInfixOf` r) rs
        code `shouldBe` ExitFailure 1
        at "if_send_issued" json `shouldBe` Just (A.Number 0)
        sendStatuses json `shouldBe` []
        for_ ["SyncIdle (Test Cases 5.2 item 1, PDF 29) waived: unobservable under Level0Only"
             , "NoSessions (Test Cases 5.2 item 6, PDF 29) waived: unobservable under Level0Only"
             , "LockingManufactured (Test Cases 5.2 item 2, PDF 29) waived"] $ \w ->
          md `shouldSatisfy` T.isInfixOf w
        reports <- listDirectory (dir </> "report")
        for_ [ f | f <- reports, takeExtension f == ".json" ] $ \f ->
          silently (verifyTranscript (dir </> "report" </> f) True) `shouldReturn` ExitSuccess
    it "passes UCT-01 alone: exit 0" $ needing [g2, g4] $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, _, _) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01" }
        code `shouldBe` ExitSuccess

  describe "the simulator paired with the replay (the Pair runner)" $ do
    it "finds 0x0403 missing without a profile: exit 1" $ needing [g1, g2, g4] $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, json, _) <- jigIn dir defaultRunOptions { roTarget = TPair TSim (TReplay file), roTests = keys "UCT-01" }
        atPath ["pair", "missing"] json `shouldBe` Just (A.toJSON [0x0403 :: Int])
        code `shouldBe` ExitFailure 1
    it "with it as the profile, finds extra 0x0005 and 0x0404, only differences allowed: exit 0" $ needing [g1, g2, g4] $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, json, _) <- jigIn dir defaultRunOptions
          { roTarget = TPair TSim (TReplay file), roTests = keys "UCT-01", roProfile = Just file }
        (atPath ["pair", "extra"] json, atPath ["pair", "missing"] json, atPath ["pair", "differences"] json)
          `shouldBe` (Just (A.toJSON [0x0005, 0x0404 :: Int]), Just (A.toJSON ([] :: [Int])), Just (A.toJSON ([] :: [Int])))
        code `shouldBe` ExitSuccess
        L.length (elements (atPath ["pair", "allowed"] json)) `shouldSatisfy` (> 0)
