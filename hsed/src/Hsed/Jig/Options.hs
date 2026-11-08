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
"Hsed.Sim.Params".@spSidPin@, and since M5a the Locking SP's Original
Factory State, @--locking-ofs@, 'parseLockingOFS'), the drive's (the serial it must have, where to
capture its Level 0 response, the Level 0 transfer length), whether the
reports show the secrets a run read in clear (@--show-msid@, design 9.3 item
5), and where the reports go; and M4c's, for a drive the jig sends to
(design 9.3, 11): @--recover@, a mounted drive allowed (@--allow-mounted@),
the deliberately interrupted run (@--interrupt-in-session@), and how a
drive's SyncIdle and NoSessions are established, which no flag sets. Here,
below "Hsed.Jig.Target" and "Hsed.Jig.Run", so that both can take them. The
simulator flags' readers ('parseSidPin', 'parseLockingOFS',
'lockingOFSName') are "Hsed.Sim.Flags"' since M5c, re-exported here, so that
hsed-sim reads them as the jig does and imports no module of the jig.
-}
module Hsed.Jig.Options
  ( -- * Targets
    TargetSpec (..)
  , WsUrl (..)
  , renderWsUrl
  , parseTarget
    -- * Options
  , RunOptions (..)
  , defaultRunOptions
  , parseSidPin
  , parseLockingOFS
  , lockingOFSName
  ) where

import           RIO
import qualified RIO.Text               as T

import           Hsed.Jig.Catalogue     (CaseKey)
import           Hsed.Sim.Flags         (lockingOFSName, parseLockingOFS, parseSidPin)
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
  | TWs !WsUrl                   -- ^ the simulator served by hsed-sim over mh_transport frames (M5c's LAN leg); 'parseTarget'
                                 --   refuses it until group G3 builds its client
  deriving (Eq, Show)

-- | A @ws://@ target's URL: its port and its path, the host being 127.0.0.1
--   alone (the cross-host run is M6's, D4; agents listen on loopback alone,
--   D6; Scott's answer Q1).
data WsUrl = WsUrl
  { wsPort :: !Word16
  , wsPath :: !Text   -- ^ beginning @/@
  }
  deriving (Eq, Show)

-- | A @ws://@ target's URL as @--target@ takes it.
--
-- >>> renderWsUrl (WsUrl 41234 "/ws")
-- "ws://127.0.0.1:41234/ws"
renderWsUrl :: WsUrl -> Text
renderWsUrl u = "ws://127.0.0.1:" <> tshow (wsPort u) <> wsPath u

-- | @sim@, @/dev/diskN@, @replay:FILE@, @sim+/dev/diskN@ or @sim+replay:FILE@;
--   a @ws://@ target, M5c's (the LAN leg: the mh_transport client, design
--   7.4, 11 M5c), is refused.
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
          Left ("--target " <> d <> ": a ws:// target is M5c's (the mh_transport client, design 7.4), not built yet")
      | Just disk <- T.stripPrefix "/dev/" d, not (T.null disk) = Right (TDevice d)
      | Just file <- T.stripPrefix "replay:" d, not (T.null file) = Right (TReplay (T.unpack file))
      | otherwise =
          Left ("--target " <> t <> ": not sim, /dev/diskN, replay:FILE, sim+/dev/diskN or sim+replay:FILE")


-- * Options

-- | What a run is asked to do.
data RunOptions = RunOptions
  { roTarget                :: !TargetSpec
  , roTests                 :: ![CaseKey]        -- ^ the cases to run; none, every case of the catalogue but those the
                                                 --   default leaves out on the simulator, each noted ("Hsed.Jig.Run".@leftOutByDefault@:
                                                 --   since M5b, a case whose Locking SP prerequisite @--locking-ofs@ does not meet)
  , roSim                   :: !SimParams        -- ^ the simulator's parameters (@--units@, @--inactive-comid-discard@,
                                                 --   @--sim-sid-pin@, @--locking-ofs@)
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

-- | The simulator, every case its default selects, the default parameters, no hold, reports in
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
