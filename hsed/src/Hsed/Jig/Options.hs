{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Options
Description : What a jig run is asked to do: its target, its cases, the simulator's parameters and the drive's
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The options of @hsed-jig run@ (design 9.2), apart from the command line
that reads them (@app/hsed-jig@): the target ('TargetSpec': the simulator,
a drive, a replay of a captured response, or the simulator paired with a
drive), the cases, the simulator's parameters and options (among them, since
M4a, the SID's PIN, @--sim-sid-pin HEX@, which 'parseSidPin' reads into
"Hsed.Sim.Params".@spSidPin@), the drive's (the serial it must have, where to
capture its Level 0 response, the Level 0 transfer length), whether the
reports show the secrets a run read in clear (@--show-msid@, design 9.3 item
5), and where the reports go; and M4c's, for a drive the jig sends to
(design 9.3, 11): @--recover@, a mounted drive allowed (@--allow-mounted@),
the deliberately interrupted run (@--interrupt-in-session@), and how a
drive's SyncIdle and NoSessions are established, which no flag sets. Here,
below "Hsed.Jig.Target" and "Hsed.Jig.Run", so that both can take them.
-}
module Hsed.Jig.Options
  ( -- * Targets
    TargetSpec (..)
  , parseTarget
    -- * Options
  , RunOptions (..)
  , defaultRunOptions
  , parseSidPin
  ) where

import qualified Data.ByteString.Base16 as B16
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO
import qualified RIO.Text               as T

import           Hsed.Jig.Catalogue     (CaseKey)
import           Hsed.Sim.Types         (SimParams, defaultSimParams)
import           Hsed.Wire              (ExchangeOpts, defaultExchangeOpts)


-- $setup
-- >>> :set -XOverloadedStrings

-- * Targets

-- | What a run talks to.
data TargetSpec
  = TSim                         -- ^ the simulator, in process
  | TDevice !Text                -- ^ a drive, @/dev/diskN@, through the kext
  | TReplay !FilePath            -- ^ a captured Level 0 response, as a drive
  | TPair !TargetSpec !TargetSpec  -- ^ the simulator and a drive, step by step (the Pair runner)
  deriving (Eq, Show)

-- | @sim@, @/dev/diskN@, @replay:FILE@, @sim+/dev/diskN@ or @sim+replay:FILE@;
--   a @ws://@ target, M5's (the mh_transport client, design 7.4), is
--   refused.
--
-- >>> parseTarget "sim+replay:build/disk6-like-level0.bin"
-- Right (TPair TSim (TReplay "build/disk6-like-level0.bin"))
parseTarget :: Text -> Either Text TargetSpec
parseTarget t
  | t == "sim"                         = Right TSim
  | Just drive <- T.stripPrefix "sim+" t = TPair TSim <$> driveTarget drive
  | otherwise                          = driveTarget t
  where
    driveTarget d
      | any (`T.isPrefixOf` d) ["ws://", "wss://"] =
          Left ("--target " <> d <> ": a ws:// target is M5's (the mh_transport client, design 7.4), not M3's")
      | Just disk <- T.stripPrefix "/dev/" d, not (T.null disk) = Right (TDevice d)
      | Just file <- T.stripPrefix "replay:" d, not (T.null file) = Right (TReplay (T.unpack file))
      | otherwise =
          Left ("--target " <> t <> ": not sim, /dev/diskN, replay:FILE, sim+/dev/diskN or sim+replay:FILE")


-- * Options

-- | What a run is asked to do.
data RunOptions = RunOptions
  { roTarget                :: !TargetSpec
  , roTests                 :: ![CaseKey]        -- ^ the cases to run; none, every case of the catalogue
  , roSim                   :: !SimParams        -- ^ the simulator's parameters (@--units@, @--inactive-comid-discard@,
                                                 --   @--sim-sid-pin@)
  , roHold                  :: !Int              -- ^ the IF-RECVs a prepared response is held for (@--hold-processing@)
  , roProfile               :: !(Maybe FilePath) -- ^ a Level 0 response whose descriptors the simulator adds (@--profile@),
                                                 --   read when the run starts
  , roReportDir             :: !FilePath
  , roCapture               :: !(Maybe FilePath) -- ^ where to write the drive's Level 0 response (@--capture-level0@)
  , roExpectSerial          :: !(Maybe Text)     -- ^ the serial the drive must have (@--expect-serial@)
  , roLevel0Len             :: !Word32           -- ^ the Level 0 IF-RECV's transfer length (@--level0-len@)
  , roShowMsid              :: !Bool             -- ^ the secrets a run read (the MSID) shown in clear in the reports
                                                 --   (@--show-msid@); masked otherwise (design 9.3 item 5)
  , roRecover               :: !Bool             -- ^ @--recover@: STACK_RESET on the drive's base ComID and the checks after it,
                                                 --   alone, no case run ("Hsed.Jig.Recover"; refused with @--tests@ or a pair,
                                                 --   design 9.3)
  , roAllowMounted          :: !Bool             -- ^ @--allow-mounted@: a run that may send to a drive goes ahead with a slice of
                                                 --   it mounted, the mounts noted (design 9.3 item 4)
  , roInterruptInSession    :: !Bool             -- ^ @--interrupt-in-session@: the run stops once StartSession has opened a
                                                 --   session on the drive, the session left open and the reports written, for
                                                 --   @--recover@ to clear (design 11, M4c's interrupted run)
  , roStackResetBeforeCases :: !Bool             -- ^ no flag: on a drive, a case's SyncIdle and NoSessions established by one
                                                 --   STACK_RESET on the base ComID, logged (Core 3.3.4.7.5, PDF 65); false,
                                                 --   SyncIdle probed by an IF-RECV and NoSessions waived as unobservable (design
                                                 --   13.7)
  , roExchange              :: !ExchangeOpts     -- ^ no flag: how the host polls and how long it waits (the host's exchange,
                                                 --   a drain's and STACK_RESET's polls; "Hsed.Wire".'defaultExchangeOpts'),
                                                 --   which only a test shortens
  , roDriveTimeoutMicros    :: !Int              -- ^ no flag: how long the host's exchange waits for each of a drive's
                                                 --   commands, past the kext's own limit ("Hsed.Jig.Script".@driveTimeoutMicros@),
                                                 --   which only a test shortens
  }
  deriving (Eq, Show)

-- | The simulator, every case, the default parameters, no hold, reports in
--   @build/report@, a Level 0 transfer of 2048 bytes, what the kext
--   itself asks for when it attaches (@DISCOVERY0_RESPONSE_SIZE@,
--   UserKernelShared.h; design 7.2), the secrets masked, neither
--   @--recover@, @--allow-mounted@ nor @--interrupt-in-session@; and on a
--   drive, STACK_RESET on its base ComID before each case that needs no
--   session open, automatic and logged: Scott's decision of 2026-11-02
--   (design 13.7), which no flag changes. The other branch, SyncIdle probed
--   by one IF-RECV and NoSessions waived as unobservable, is kept built and
--   tested in both modes, so that a flip is this one field. The host polls
--   and waits as "Hsed.Wire".'defaultExchangeOpts' says, and gives each of
--   a drive's commands 15 s, past the kext's own 10 s on its SCSI task
--   (@SED_TIMEOUT@, macos-driver UserKernelShared.h:71, which
--   TPerDriver.cpp:318 gives each command), which makes the host's timeout
--   unlikely to end a wait; it never cuts a command short (the run gives
--   each under its recording's mask), and a host that gives up all the same
--   latches the drive ("Hsed.Jig.Script".@exchangeOn@).
defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions
  { roTarget                = TSim
  , roTests                 = []
  , roSim                   = defaultSimParams
  , roHold                  = 0
  , roProfile               = Nothing
  , roReportDir             = "build/report"
  , roCapture               = Nothing
  , roExpectSerial          = Nothing
  , roLevel0Len             = 2048
  , roShowMsid              = False
  , roRecover               = False
  , roAllowMounted          = False
  , roInterruptInSession    = False
  , roStackResetBeforeCases = True
  , roExchange              = defaultExchangeOpts
  , roDriveTimeoutMicros    = 15000000
  }

-- | @--sim-sid-pin HEX@: the simulator's C_PIN_SID PIN as hex digits, two a
--   byte, an optional @0x@ before them, 4 to 32 bytes, or why not: at most
--   the PIN column's (Core Table 122, password, PDF 118), and at least
--   four, since the reports mask the PIN in their every text as well as in
--   the transcript's bytes ("Hsed.Jig.Transcript".'maskText', design 9.3
--   item 5) and a shorter one would be masked out of their own words (a
--   3-byte @SID@ masks @C_PIN_SID@ and @MSID@, a 1-byte @1@ every 1 of a
--   page number). The bytes go to "Hsed.Sim.Params".@spSidPin@, so that
--   the simulated TPer's Initial C_PIN_SID PIN Indicator is other than the
--   MSID's and UCT-03's step 1 does not apply (Test Cases PDF 33); a set
--   SID PIN also fails UCT-01's step 4c.ii, SID Value State 1 (PDF 30),
--   the Test Cases taking the drive at its original factory state (4.2,
--   PDF 28). A PIN of four bytes or more that is itself a word of the
--   reports' prose (@Core@, @Opal@, @MSID@) is still masked out of it
--   wherever it stands, the user's choice; the floor is the jig's own rule,
--   reversible (Core Table 122 allows 0 to 32 bytes), and the MSID a drive
--   reports passes through the same masking with no floor (M4c).
--
-- >>> parseSidPin "53494421"
-- Right "SID!"
-- >>> parseSidPin "0x53494421"
-- Right "SID!"
-- >>> parseSidPin "534944"
-- Left "--sim-sid-pin 534944: 3 bytes; at least 4, so that masking the PIN in the reports' texts rewrites none of their short words or page numbers (design 9.3 item 5)"
-- >>> parseSidPin "31"
-- Left "--sim-sid-pin 31: 1 byte; at least 4, so that masking the PIN in the reports' texts rewrites none of their short words or page numbers (design 9.3 item 5)"
-- >>> parseSidPin "5349442"
-- Left "--sim-sid-pin 5349442: hex digits, two a byte (an even count)"
-- >>> parseSidPin "zz"
-- Left "--sim-sid-pin zz: hex digits, two a byte (an even count)"
parseSidPin :: String -> Either String ByteString
parseSidPin s = case B16.decode (fromString digits) of
  Right bytes
    | B.null bytes           -> Left (prefix <> "no bytes")
    | B.length bytes < least -> Left (prefix <> bytesWord (B.length bytes) <> "; at least " <> show least
                                      <> ", so that masking the PIN in the reports' texts rewrites none of their short words or page numbers (design 9.3 item 5)")
    | B.length bytes > most  -> Left (prefix <> "more than " <> show most <> " bytes, the PIN column's most (Core Table 122, PDF 118)")
    | otherwise              -> Right bytes
  Left _ -> Left (prefix <> "hex digits, two a byte (an even count)")
  where
    digits = fromMaybe s (L.stripPrefix "0x" s <|> L.stripPrefix "0X" s)
    prefix = "--sim-sid-pin " <> s <> ": "
    -- the size limitation of the password type (Core 5.1.3.63, Table 122, PDF 118: max_bytes, 32)
    most = 32 :: Int
    -- the jig's own floor: the PIN is masked wherever the reports' texts have it (design 9.3 item 5), and one under
    -- four bytes is a word or a number of theirs
    least = 4 :: Int
    bytesWord n = show n <> (if n == 1 then " byte" else " bytes")
