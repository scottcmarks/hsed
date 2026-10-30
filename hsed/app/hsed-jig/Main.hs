{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : hsed-jig, the host test jig
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The command line of the jig (docs/spec-as-source.md section 9.2); what each
command does is "Hsed.Jig.Cli". @run@ is the default, so that @hsed-jig
--target sim --tests UCT-01@ runs. The options of later milestones are read
and refused, each naming its milestone, as is a @ws://@ target (M5's). A
usage error exits 2.
-}
module Main (main) where

import           Options.Applicative
import           RIO
import qualified RIO.Text            as T

import           Hsed.Jig.Catalogue  (parseCaseKeys)
import           Hsed.Jig.Cli        (Command (..), runCommand)
import           Hsed.Jig.Options    (RunOptions (..), defaultRunOptions, parseTarget)
import           Hsed.Sim.Types      (SimParams (..))
import           Hsed.Version        (hsedVersion)
import           Hsed.Wire           (Unit (..), mostCarried)


main :: IO ()
main = execParser hsedJig >>= runCommand >>= exitWith

hsedJig :: ParserInfo Command
hsedJig = info (commands <**> helper <**> versionOption)
  ( fullDesc
 <> header "hsed-jig - the hsed host test jig"
 <> progDesc "Runs Opal Family Test Cases against the simulator, a drive or a replay (docs/spec-as-source.md 9); RUN is the default command."
 <> footer exitCodes
 <> failureCode 2 )
  where
    versionOption = infoOption (T.unpack hsedVersion) (long "version" <> help "Show the version")

exitCodes :: String
exitCodes = "Exit codes: 0 every selected case passed (waived prerequisites printed), no simulator extension failed, a pair found only allowed or unchecked differences, and a capture asked for was written; 1 a failure: a case failed or was skipped or unobservable without a waiver, an extension failed, the pair found a code missing or a difference not allowed, the capture failed, or a profile or target named could not be read or opened; 2 a usage error (a key the catalogue lacks, a drive target without --expect-serial, a --level0-len a drive or a replay cannot carry); 3 a safety violation (a drive target without root, or an IF-SEND issued on a drive)."

-- | A subcommand: its parser and its description (hsubparser gives it
--   --help), a usage error exiting 2.
subcommand :: String -> String -> Parser Command -> Mod CommandFields Command
subcommand name desc p = command name (info p (progDesc desc <> failureCode 2))

commands :: Parser Command
commands = hsubparser
  ( subcommand "run" "Run the selected cases against a target (the default command)" (Run <$> runOptions)
 <> subcommand "level0" "Decode or synthesize a Level 0 Discovery response" level0Commands
 <> subcommand "verify-transcript" "Print the commands a report's transcript issued" verifyTranscript
 <> subcommand "diff" "Compare two reports' Level 0 decodes as the Pair runner does" diffReports )
  <|> Run <$> runOptions
  where
    verifyTranscript = VerifyTranscript
      <$> strArgument (metavar "REPORT.json" <> help "a run's JSON report")
      <*> switch (long "no-if-send" <> help "fail (exit 1) if a drive's end was issued an IF-SEND (the simulator's are its own)")
      <*> switch (long "level0-only"
            <> help "fail (exit 1) if a drive's end was issued anything but Level 0 Discovery's IF-RECV (protocol 0x01, ComID 0x0001)")
    diffReports = Diff
      <$> strArgument (metavar "A.json" <> help "a run's JSON report")
      <*> strArgument (metavar "B.json" <> help "another")

level0Commands :: Parser Command
level0Commands = hsubparser
  ( subcommand "decode" "Split and decode a Level 0 response (a capture's .bin)"
      (Level0Decode
         <$> strArgument (metavar "FILE" <> help "the response, as the IF-RECV returned it")
         <*> optional (strOption (long "against" <> metavar "QUERY.txt"
               <> help ("compare with every value sedutil-cli 1.49.1 --query printed, and, as disk6's kext reports one, "
                        <> "ask for the Configurable Namespace Locking descriptor (0x0403) sedutil does not print"))))
 <> subcommand "synthesize" ("Render a drive-like Level 0 response from sedutil's printed query, adding disk6's Configurable "
                              <> "Namespace Locking descriptor (0x0403), which sedutil does not print (a test aid)")
      (Level0Synthesize
         <$> strArgument (metavar "QUERY.txt" <> help "what sedutil-cli --query printed")
         <*> strArgument (metavar "OUT.bin" <> help "the response to write")) )

runOptions :: Parser RunOptions
runOptions = options <**> laterMilestones
  where
    sim0 = roSim defaultRunOptions
    options = mk
      <$> option (eitherReader (first T.unpack . parseTarget . T.pack))
            (long "target" <> metavar "TARGET" <> value (roTarget defaultRunOptions)
             <> help "sim (the default), /dev/diskN, replay:FILE, sim+/dev/diskN or sim+replay:FILE")
      <*> option (eitherReader (first T.unpack . parseCaseKeys . T.pack))
            (long "tests" <> metavar "KEYS" <> value (roTests defaultRunOptions)
             <> help "comma-separated case keys, UCT-01,ETC-02,ETC-03,SPF-02:1 (default: every case of the catalogue)")
      <*> option (eitherReader units)
            (long "units" <> metavar "UNITS" <> value (spUnits sim0) <> showDefaultWith unitName
             <> help "simulator: what transfer lengths count, bytes or blocks512")
      <*> switch (long "hold-processing" <> help "simulator: hold a prepared response in S2 for one IF-RECV")
      <*> optional (option auto (long "hold-ifrecvs" <> metavar "N"
            <> help "simulator: hold a prepared response for N IF-RECVs (with or without --hold-processing)"))
      <*> optional (strOption (long "profile" <> metavar "FILE.bin"
            <> help "simulator: add a Level 0 response's descriptors the simulator lacks"))
      <*> switch (long "inactive-comid-discard"
            <> help "simulator: discard a command to an inactive ComID rather than refuse it")
      <*> optional (strOption (long "expect-serial" <> metavar "SERIAL"
            <> help "drive: the serial (trimmed IDENTIFY) the drive must have; required for a /dev target"))
      <*> optional (strOption (long "capture-level0" <> metavar "FILE"
            <> help "drive: write its Level 0 response to FILE and its sidecar to FILE.json"))
      <*> option (eitherReader level0Len) (long "level0-len" <> metavar "BYTES" <> value (roLevel0Len defaultRunOptions) <> showDefault
            <> help ("the Level 0 IF-RECV's transfer length, on the simulator or a drive: 1 to " <> show mostLevel0
                     <> " bytes, rounded up to the target's unit (a drive or a replay carries at most 255 blocks of 512, "
                     <> show (mostCarried UnitBlocks512) <> " bytes: more is a usage error)"))
      <*> strOption (long "report-dir" <> metavar "DIR" <> value (roReportDir defaultRunOptions) <> showDefault
            <> help "where the reports go")
    mk target tests u holding holdN profile discard serial capture len dir = defaultRunOptions
      { roTarget       = target
      , roTests        = tests
      , roSim          = sim0 { spUnits = u, spInactiveComIdDiscard = discard }
      , roHold         = fromMaybe (if holding then 1 else 0) holdN
      , roProfile      = profile
      , roReportDir    = dir
      , roCapture      = capture
      , roExpectSerial = T.pack <$> serial
      , roLevel0Len    = len
      }
    -- a length some transport carries: none carries more than 1 MiB (Hsed.Wire.mostCarried)
    mostLevel0 = mostCarried UnitBytes
    level0Len s = case readMaybe s of
      Just n | n >= 1 && n <= toInteger mostLevel0 -> Right (fromInteger n)
      _ -> Left ("--level0-len " <> s <> ": a length of 1 to " <> show mostLevel0 <> " bytes")
    units = \case
      "bytes"     -> Right UnitBytes
      "blocks512" -> Right UnitBlocks512
      u           -> Left ("--units " <> u <> ": bytes or blocks512")
    unitName = \case
      UnitBytes     -> "bytes"
      UnitBlocks512 -> "blocks512"

-- | The options of later milestones, read and refused (a usage error), each
--   naming its milestone (design 9.3, 11).
laterMilestones :: Parser (a -> a)
laterMilestones = foldr (\r rest -> (.) <$> refused r <*> rest) (pure id)
  [ ("allow-write", "SERIAL", "M6: writes to a drive, with HSED_DRIVE_WRITE=1 (design 9.3)")
  , ("recover", "", "M4c: STACK_RESET on the base ComID as a pre-flight (design 9.3)")
  , ("allow-mounted", "", "M4c: a mounted drive's pre-flight (design 9.3)")
  , ("allow-error-paths", "", "M4c: hardware error paths, opt-in (design 9.3, 13.8)")
  , ("sessions", "MODE", "M4c: read-only sessions on a drive (design 9.3, 13.7)")
  , ("show-msid", "", "M4c: the MSID in clear (design 9.3)")
  ]
  where
    refused (name, var, milestone) = abortOption
      (ErrorMsg ("--" <> name <> " is " <> milestone <> "; not at M3"))
      (long name <> metavar var <> help ("refused at M3: " <> milestone))
