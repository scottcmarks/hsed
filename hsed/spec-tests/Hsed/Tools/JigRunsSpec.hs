{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Tools.JigRunsSpec
Description : Test specifications for tools/jig-runs.sh: it parses, and -w's usage errors
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

tools/jig-runs.sh parses (bash -n), and its runs over the LAN leg (@-w@, design 11 M5c, group
G3's acceptance) are runs 1-6 and 1m-6m alone: runs 7, 8 and 9, alone or among others and
whichever option comes first, are a usage error (exit 2) found before anything is made, no
hub started, no jig run, no OUTDIR. Nothing here starts a hub: the runs themselves are
Stage 2's (tools/jig-runs.sh -w against the simulator's twelve, compare_reports.py
--allow-target), and the script is run with no stack under it, the refusal coming before its
first stack command.
-}
module Hsed.Tools.JigRunsSpec (spec) where

import           RIO
import           RIO.Directory (doesPathExist)
import           RIO.FilePath  ((</>))
import           RIO.Process   (proc, readProcess, withProcessContextNoLogging)
import qualified RIO.Text      as T
import           Test.Hspec


spec :: Spec
spec = describe "tools/jig-runs.sh" $ do
  it "parses (bash -n)" $ do
    (code, _, err) <- withProcessContextNoLogging (proc "bash" ["-n", script] readProcess)
    (code, err) `shouldBe` (ExitSuccess, "")
  it "with -w, refuses runs 7, 8 and 9, alone or among others, -w before -r or after: a usage error (exit 2) before anything is made, no OUTDIR, no hub" $
    withSystemTempDirectory "jig-runs" $ \dir -> do
      let out = dir </> "out"
      outcomes <- traverse (\(args, run) -> (,) run <$> jigRuns (args <> [out]))
        [ (["-w", "-r", "7"], "7"), (["-w", "-r", "8"], "8"), (["-r", "1m,2,9", "-w"], "9") ]
      for_ outcomes $ \(run, (code, err)) -> do
        code `shouldBe` ExitFailure 2
        err `shouldSatisfy` T.isInfixOf ("jig-runs.sh: -w: run " <> run <> "'s target is a replay")
      doesPathExist out `shouldReturn` False

-- | The script, as the suite (run in the package's directory) finds it.
script :: FilePath
script = "../tools/jig-runs.sh"

-- | The script run with arguments: its exit code and what it wrote to stderr.
jigRuns :: [String] -> IO (ExitCode, Text)
jigRuns args = do
  (code, _, err) <- withProcessContextNoLogging (proc "bash" (script : args) readProcess)
  pure (code, decodeUtf8Lenient (toStrictBytes err))
