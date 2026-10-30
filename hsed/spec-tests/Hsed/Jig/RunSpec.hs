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
missing without a profile and the comparison passes with it; standing in
for the kext's drive, the replay's transcript notes the hardware run's CDB
and the capture is written; a drive whose Level 0 Discovery fails is asked
once and never again; a header revision Opal allows is noted as allowed.
On a pair each end's verdicts and Level 0 are named by its transport's
role, as the transcript's entries are. And what the run refuses before it
opens anything: a key the catalogue lacks (exit 2), a drive without the
serial it must have (exit 2), a Level 0 length a drive or a replay cannot
carry (exit 2), a drive without root (exit 3).
-}
module Hsed.Jig.RunSpec (spec) where

import qualified Data.Aeson             as A
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO
import           RIO.Directory          (doesFileExist, listDirectory)
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
import           Hsed.Jig.Support       (at, atPath, disk6Like, silently)
import           Hsed.Jig.Target        (Target (..))
import           Hsed.Level0.Sedutil    (SedutilFeature (..), SedutilQuery (..), parseQuery, synthesize)
import           Hsed.Sim.FeatureSets   (DataStoreFeature)
import           Hsed.Sim.Types         (SimParams (..), defaultSimParams)
import           Hsed.Transport.Drive   (DriveInfo (..), InterfaceType (..))
import           Hsed.Transport.InProc  (InProcOpts (..), newInProc)
import           Hsed.Transport.Replay  (openReplay)
import           Hsed.Wire.Internal     (Describe (..), IfStatus (..), Response (..), Transport (..), Unit (..))


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
      silently (runJig defaultRunOptions { roTests = keys "SPF-11" }) `shouldReturn` ExitFailure 2
    it "a drive without --expect-serial, alone or paired, root or not: exit 2" $ do
      silently (runJig defaultRunOptions { roTarget = TDevice "/dev/disk99" }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roTarget = TPair TSim (TDevice "/dev/disk99") }) `shouldReturn` ExitFailure 2
    it "a Level 0 length a drive or a replay cannot carry (255 blocks of 512), alone or paired: exit 2; one it can, opened" $ do
      silently (runJig defaultRunOptions { roTarget = TReplay "nowhere.bin", roLevel0Len = 1048576 }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roTarget = TReplay "nowhere.bin", roLevel0Len = 130561 }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roTarget = TPair TSim (TReplay "nowhere.bin"), roLevel0Len = 1048576 }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roTarget = TReplay "nowhere.bin", roLevel0Len = 130560 }) `shouldReturn` ExitFailure 1
    it "a drive without root, alone or paired: exit 3" $ do
      root <- runningAsRoot
      let named = defaultRunOptions { roExpectSerial = Just "S0000000000000" }
      if root
        then pendingWith "running as root: the gate lets a drive through"
        else do
          silently (runJig named { roTarget = TDevice "/dev/disk99" }) `shouldReturn` ExitFailure 3
          silently (runJig named { roTarget = TPair TSim (TDevice "/dev/disk99") }) `shouldReturn` ExitFailure 3

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
      it ("passes in " <> show u <> (if hold > 0 then ", a response held for an IF-RECV" else "")) $ do
        (code, json, md) <- jig (onSim u hold)
        verdicts json `shouldBe` [ (str k, str "pass") | k <- ["UCT-01", "SPF-02:1", "ETC-02", "ETC-03"] ]
        code `shouldBe` ExitSuccess
        sendStatuses json `shouldSatisfy` elem "synchronous_protocol_violation"
        t45s json `shouldSatisfy` elem "all_returned_no_data"
        md `shouldSatisfy` T.isInfixOf "LockingManufactured (Test Cases 5.2 item 2, PDF 29) waived"
    it "held, answers an IF-RECV (0, 1, 0), and passes both extensions" $ do
      (_, json, _) <- jig (onSim UnitBytes 1)
      t45s json `shouldSatisfy` elem "responses_to_come"
      extensionVerdicts json `shouldBe` [str "pass", str "pass"]
    it "asks for Level 0 once more, at its whole length, when the length asked for cut it short" $ do
      (code, json, _) <- jig (onSim UnitBytes 0) { roTests = keys "UCT-01", roLevel0Len = 100 }
      code `shouldBe` ExitSuccess
      [ n | e <- elements (at "transcript" json), atPath ["request", "comid"] e == Just (A.Number 1)
          , Just (A.Number n) <- [atPath ["request", "len"] e] ]
        `shouldBe` [100, 216]
    it "not held, passes the Empty-atom extension and skips the one that needs a response held" $ do
      (_, json, _) <- jig (onSim UnitBytes 0)
      extensionVerdicts json `shouldBe` [str "pass", str "skipped"]
      t45s json `shouldSatisfy` notElem "responses_to_come"

  describe "a drive-like response replayed as the drive" $ do
    it "passes UCT-01, skips the others with their reasons, issues no IF-SEND, prints the waivers" $
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
          silently (verifyTranscript (dir </> "report" </> f) True True) `shouldReturn` ExitSuccess
    it "passes UCT-01 alone: exit 0" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, _, _) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01" }
        code `shouldBe` ExitSuccess
    it "notes a header revision Opal 2.30 Table 2 allows as allowed, in the transcript and the Markdown, and passes UCT-01" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "minor-2.bin"
        bytes <- disk6Like
        writeFileBinary file (B.take 6 bytes <> B.pack [0x00, 0x02] <> B.drop 8 bytes)
        (code, json, md) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01" }
        code `shouldBe` ExitSuccess
        [ t | e <- elements (at "transcript" json), Just (A.String t) <- [at "text" e], "Level 0 Discovery: " `T.isPrefixOf` t ]
          `shouldSatisfy` any (T.isPrefixOf "Level 0 Discovery: allowed (Opal 2.30 Table 2, PDF 19): HeaderDiffers")
        md `shouldSatisfy` T.isInfixOf "- allowed (Opal 2.30 Table 2, PDF 19): HeaderDiffers"
    it "fails UCT-01's step 1c on a response whose Length of Parameter Data asks for more than 255 blocks" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "huge.bin"
        bytes <- disk6Like
        writeFileBinary file (B.pack [0x00, 0x03, 0x0D, 0x40] <> B.drop 4 bytes)
        (code, json, _) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01" }
        code `shouldBe` ExitFailure 1
        [ r | c <- elements (at "cases" json), A.String r <- elements (at "reasons" c) ] `shouldSatisfy` any ("1c:" `T.isPrefixOf`)
    it "says UCT-01's step 4c is not evaluated on a response without the Block SID descriptor, with Table 7's version" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "no-block-sid.bin"
        query <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
        q <- either (throwString . T.unpack) pure (parseQuery query)
        writeFileBinary file (synthesize q { sqFeatures = filter ((/= 0x0402) . sfCode) (sqFeatures q) })
        (code, json, _) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01" }
        code `shouldBe` ExitSuccess
        [ t | e <- elements (at "transcript" json), Just (A.String t) <- [at "text" e], "UCT-01 4c not evaluated" `T.isPrefixOf` t ]
          `shouldSatisfy` any ("TCG Opal SSC Specification v2.00 (Opal 2.30 Table 7, PDF 24)" `T.isInfixOf`)

  describe "a replay standing in for the kext's drive, with what the IORegistry would say of disk6" $
    it "notes the CDB the hardware run expects, issues no IF-SEND, and captures the response with its sidecar" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        bytes <- disk6Like
        let file    = dir </> "disk6-like-level0.bin"
            capture = dir </> "capture" </> "disk6-level0.bin"
            reports = dir </> "report"
        writeFileBinary file bytes
        tr <- openReplay file >>= either (throwString . T.unpack) pure
        code <- silently $ runOpened
          defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01", roCapture = Just capture, roReportDir = reports }
          (DriveTarget tr (Just (DriveInfo "disk6" "TESTSERIAL0001" "625" IfSAT 0)))
        code `shouldBe` ExitSuccess
        readFileBinary capture `shouldReturn` bytes
        doesFileExist (dir </> "capture" </> "disk6-level0.json") `shouldReturn` True
        json <- listDirectory reports >>= \fs -> case [ f | f <- fs, takeExtension f == ".json" ] of
          [f] -> readFileBinary (reports </> f) >>= either throwString pure . A.eitherDecodeStrict'
          _   -> throwString "not one report"
        [ t | e <- elements (at "transcript" json), Just (A.String t) <- [at "text" e] ]
          `shouldSatisfy` any ("a1 08 0e 01 04 00 01 00 00 5c 00 00" `T.isInfixOf`)
        at "if_send_issued" json `shouldBe` Just (A.Number 0)

  describe "a drive whose Level 0 Discovery fails" $
    it "is asked once: UCT-01 fails, and neither the pair nor the capture asks it again" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        asked <- newIORef (0 :: Int)
        let failing = MkTransport
              { tExchange = \_ -> Response (TransportFailure "0xe00002bc") mempty <$ modifyIORef' asked (+ 1)
              , tInject   = \_ -> pure (Unsupported "a drive")
              , tUnit     = UnitBlocks512
              , tDescribe = Describe "drive" "a failing test drive" UnitBlocks512 Nothing Nothing Nothing Nothing
              , tClose    = pure ()
              }
            capture = dir </> "capture" </> "disk6-level0.bin"
        (sim, control) <- newInProc defaultSimParams (InProcOpts UnitBytes 0)
        code <- silently $ runOpened
          defaultRunOptions { roTarget = TPair TSim (TReplay "a failing drive"), roTests = keys "UCT-01"
                            , roCapture = Just capture, roReportDir = dir </> "report" }
          (PairTarget (SimTarget sim control) (DriveTarget failing (Just (DriveInfo "disk6" "TESTSERIAL0001" "625" IfSAT 0))))
        code `shouldBe` ExitFailure 1
        readIORef asked `shouldReturn` 1
        doesFileExist capture `shouldReturn` False

  describe "the simulator paired with the replay (the Pair runner)" $ do
    it "finds 0x0403 missing without a profile: exit 1" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, json, _) <- jigIn dir defaultRunOptions { roTarget = TPair TSim (TReplay file), roTests = keys "UCT-01" }
        atPath ["pair", "missing"] json `shouldBe` Just (A.toJSON [0x0403 :: Int])
        code `shouldBe` ExitFailure 1
        [ at "end" e | c <- elements (at "cases" json), e <- elements (at "ends" c) ] `shouldBe` [str "tper", str "drive-replay"]
        [ at "end" l | l <- elements (at "level0" json) ] `shouldBe` [str "tper", str "drive-replay"]
        L.nub [ at "end" e | e <- elements (at "transcript" json), at "entry" e == str "issued" ] `shouldBe` [str "tper", str "drive-replay"]
    it "with it as the profile, finds extra 0x0005 and 0x0404, only differences allowed: exit 0" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, json, _) <- jigIn dir defaultRunOptions
          { roTarget = TPair TSim (TReplay file), roTests = keys "UCT-01", roProfile = Just file }
        (atPath ["pair", "extra"] json, atPath ["pair", "missing"] json, atPath ["pair", "differences"] json)
          `shouldBe` (Just (A.toJSON [0x0005, 0x0404 :: Int]), Just (A.toJSON ([] :: [Int])), Just (A.toJSON ([] :: [Int])))
        code `shouldBe` ExitSuccess
        L.length (elements (atPath ["pair", "allowed"] json)) `shouldSatisfy` (> 0)
