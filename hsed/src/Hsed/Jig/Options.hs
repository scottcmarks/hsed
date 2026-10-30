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
drive), the cases, the simulator's parameters and options, the drive's
(the serial it must have, where to capture its Level 0 response, the
Level 0 transfer length), and where the reports go. Here, below
"Hsed.Jig.Target" and "Hsed.Jig.Run", so that both can take them.
-}
module Hsed.Jig.Options
  ( -- * Targets
    TargetSpec (..)
  , parseTarget
    -- * Options
  , RunOptions (..)
  , defaultRunOptions
  ) where

import           RIO
import qualified RIO.Text          as T

import           Hsed.Jig.Catalogue (CaseKey)
import           Hsed.Sim.Types     (SimParams, defaultSimParams)


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
  { roTarget       :: !TargetSpec
  , roTests        :: ![CaseKey]          -- ^ the cases to run; none, every case of the catalogue
  , roSim          :: !SimParams          -- ^ the simulator's parameters (@--units@, @--inactive-comid-discard@)
  , roHold         :: !Int                -- ^ the IF-RECVs a prepared response is held for (@--hold-processing@)
  , roProfile      :: !(Maybe FilePath)   -- ^ a Level 0 response whose descriptors the simulator adds (@--profile@),
                                          --   read when the run starts
  , roReportDir    :: !FilePath
  , roCapture      :: !(Maybe FilePath)   -- ^ where to write the drive's Level 0 response (@--capture-level0@)
  , roExpectSerial :: !(Maybe Text)       -- ^ the serial the drive must have (@--expect-serial@)
  , roLevel0Len    :: !Word32             -- ^ the Level 0 IF-RECV's transfer length (@--level0-len@)
  }
  deriving (Eq, Show)

-- | The simulator, every case, the default parameters, no hold, reports in
--   @build/report@, and a Level 0 transfer of 2048 bytes, what the kext
--   itself asks for when it attaches (@DISCOVERY0_RESPONSE_SIZE@,
--   UserKernelShared.h; design 7.2).
defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions
  { roTarget       = TSim
  , roTests        = []
  , roSim          = defaultSimParams
  , roHold         = 0
  , roProfile      = Nothing
  , roReportDir    = "build/report"
  , roCapture      = Nothing
  , roExpectSerial = Nothing
  , roLevel0Len    = 2048
  }
