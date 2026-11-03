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
--target sim --tests UCT-01@ runs. M4c's options for a drive the jig sends
to are read (design 9.3, 11): @--recover@, @--allow-mounted@ and
@--interrupt-in-session@, and @verify-transcript@'s @--allow-list@. The
options no milestone builds yet are read and refused, each saying why
(writes to a drive, M6's; the hardware error paths and read-only sessions,
Scott's decisions, design 13.7 and 13.8), as is a @ws://@ target (M5's). A
usage error exits 2.
-}
module Main (main) where

import           Options.Applicative
import           RIO
import qualified RIO.Text            as T

import           Hsed.Jig.Catalogue  (parseCaseKeys)
import           Hsed.Jig.Cli        (Command (..), runCommand)
import           Hsed.Jig.Options    (RunOptions (..), defaultRunOptions, lockingOFSName, parseLockingOFS, parseSidPin, parseTarget)
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
exitCodes = "Exit codes: 0 every selected case passed (waived prerequisites printed), no simulator extension failed, a pair found only allowed or unchecked differences in Level 0 and in Properties, and a capture asked for was written; with --recover, the base ComID reset, found idle, and a session to the Admin SP opened and ended on it; 1 a failure: a case failed or was skipped or unobservable without a waiver, an extension failed, the pair found a code missing or a difference not allowed (its Properties judged by Opal 2.30 Table 17), the capture failed, the Guard refused a command, --recover did not recover, or a profile or target named could not be read or opened; 2 a usage error (a key the catalogue lacks, a drive target without --expect-serial, a --level0-len a drive or a replay cannot carry, --recover with --tests, a pair or --interrupt-in-session, an option refused); 3 a safety violation (a drive target without root, a drive command its policy does not admit, re-decided from the drive's own transcript, or a mounted drive without --allow-mounted where the run may send to it); 130 interrupted (SIGINT, SIGTERM, SIGHUP, or --interrupt-in-session): nothing issued after the command in flight, the reports written, naming any session left open. Where more than one applies, 3 outranks 130, 130 outranks 1, and 1 outranks 0; and for a /dev target, once the command line parses (an option refused or malformed exits 2 first), --expect-serial (2) and then root (3) are checked before the run's own checks, so that a drive run as yourself exits 3 whatever else the run would refuse."

-- | A subcommand: its parser and its description (hsubparser gives it
--   --help), a usage error exiting 2.
subcommand :: String -> String -> Parser Command -> Mod CommandFields Command
subcommand name desc p = command name (info p (progDesc desc <> failureCode 2))

commands :: Parser Command
commands = hsubparser
  ( subcommand "run" "Run the selected cases against a target (the default command)" (Run <$> runOptions)
 <> subcommand "level0" "Decode or synthesize a Level 0 Discovery response" level0Commands
 <> subcommand "verify-transcript" "Print the commands a report's transcript issued" verifyTranscript
 <> subcommand "diff" ("Compare two reports' Level 0 decodes and, where both carry them, their Properties exchanges, judged by "
                        <> "Opal 2.30 Table 17, as the Pair runner does: A's as the simulator's, B's as the drive's") diffReports )
  <|> Run <$> runOptions
  where
    verifyTranscript = VerifyTranscript
      <$> strArgument (metavar "REPORT.json" <> help "a run's JSON report")
      <*> switch (long "no-if-send" <> help "fail (exit 1) if a drive's end was issued an IF-SEND (the simulator's are its own)")
      <*> switch (long "level0-only"
            <> help "fail (exit 1) if a drive's end was issued anything but Level 0 Discovery's IF-RECV (protocol 0x01, ComID 0x0001)")
      <*> switch (long "allow-list"
            <> help ("fail (exit 1) unless the Guard admits each command a drive's end was issued under that end's own policy as the "
                     <> "report gives it (an allow list's methods, of Level 0 Discovery, Properties, StartSession to the Admin SP as "
                     <> "Anybody, Get on C_PIN_MSID's UID and PIN, End of Session and STACK_RESET; Level 0 Discovery alone where it "
                     <> "gives level0_only or none), on the base ComID the drive's Level 0 in the report names, an allow list bound "
                     <> "to that base, and its Guard blocked nothing (design 9.3)"))
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
             <> help "comma-separated case keys, UCT-01,ETC-02,ETC-03,SPF-02:1,UCT-02,UCT-03:1a-1c,ETC-04,ETC-08,ETC-15,ETC-21/adminsp (default: every case of the catalogue)")
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
      <*> optional (option (eitherReader parseSidPin) (long "sim-sid-pin" <> metavar "HEX"
            <> help ("simulator: C_PIN_SID's PIN as hex digits, two a byte (an optional 0x before them), 4 to 32 bytes, other than "
                     <> "the MSID's, so that Level 0's Initial C_PIN_SID PIN Indicator is 0xFF and SID Value State 1: UCT-03 step 1 "
                     <> "does not apply and UCT-01 step 4c.ii fails, the drive being past its factory state (Test Cases PDF 30, 33)")))
      <*> option (eitherReader parseLockingOFS) (long "locking-ofs" <> metavar "OFS" <> value (spLockingOFS sim0)
            <> showDefaultWith (T.unpack . lockingOFSName)
            <> help ("simulator: the Locking SP's Original Factory State, manufactured-inactive (what Opal 2.30 5.2.2 says it "
                     <> "SHALL be, PDF 96) or manufactured (outside that SHALL, which its informative comment allows; the reports "
                     <> "say so)"))
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
      <*> switch (long "show-msid"
            <> help ("show the secrets the run read (UCT-03's MSID) in clear in the reports; without it each is masked as 0x2A "
                     <> "and given by its length and SHA-256 (design 9.3 item 5)"))
      <*> switch (long "recover"
            <> help ("STACK_RESET on the target's base ComID (Core 3.3.4.7.5, PDF 65), then an IF-RECV that finds it idle and a "
                     <> "StartSession to the Admin SP as Anybody with its End of Session, alone: no case runs, and it is refused with "
                     <> "--tests, a pair or --interrupt-in-session (design 9.3); on a drive, under the Guard's recovery list"))
      <*> switch (long "allow-mounted"
            <> help ("drive: a run that may send to the drive goes ahead with a slice of it mounted, each mount noted in the "
                     <> "reports; check the volume unchanged afterwards, tools/m4c-hw.sh snapshot and compare (design 9.3 item 4)"))
      <*> switch (long "interrupt-in-session"
            <> help ("stop the run once StartSession has opened a session, the session left open, nothing issued after it, the "
                     <> "reports written, exit 130: the deliberately interrupted run --recover clears (design 11)"))
    mk target tests u holding holdN profile discard sidPin ofs serial capture len dir showMsid recover allowMounted interrupt = defaultRunOptions
      { roTarget             = target
      , roTests              = tests
      , roSim                = sim0 { spUnits = u, spInactiveComIdDiscard = discard, spSidPin = sidPin, spLockingOFS = ofs }
      , roHold               = fromMaybe (if holding then 1 else 0) holdN
      , roProfile            = profile
      , roReportDir          = dir
      , roCapture            = capture
      , roExpectSerial       = T.pack <$> serial
      , roLevel0Len          = len
      , roShowMsid           = showMsid
      , roRecover            = recover
      , roAllowMounted       = allowMounted
      , roInterruptInSession = interrupt
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

-- | The options no milestone builds yet, read and refused (a usage error),
--   each saying why (design 9.3, 11): writes to a drive are M6's; the
--   hardware error paths stay off a drive, Scott's to decide (design
--   13.8); read-only sessions are not built, a drive's sessions being
--   read-write under the method-level allow list (Scott's answer of
--   2026-11-02, design 13.7). @--show-msid@ is read since M4a, and
--   @--recover@, @--allow-mounted@ and @--interrupt-in-session@ since M4c.
laterMilestones :: Parser (a -> a)
laterMilestones = foldr (\r rest -> (.) <$> refused r <*> rest) (pure id)
  [ ("allow-write", "SERIAL", "M6: writes to a drive, with HSED_DRIVE_WRITE=1 (design 9.3)")
  , ("allow-error-paths", "", "hardware error paths stay off a drive: design 13.8 is Scott's")
  , ("sessions", "MODE", "not built: drive sessions are read-write under the method-level allow list (design 9.3 item 3, 13.7; Scott, 2026-11-02)")
  ]
  where
    -- optparse-applicative's abortOption, with one message for the option given an argument (which it prints after
    -- "option --NAME: ") and one for it given none (which it prints alone)
    refused (name, var, why) = option (readerAbort (ErrorMsg ("refused: " <> why)))
      (long name <> metavar var <> help ("refused: " <> why) <> value id <> noArgError (ErrorMsg ("--" <> name <> ": refused: " <> why)))
