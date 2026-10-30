{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Report
Description : A run's reports (json, md, JUnit xml), and what reads them again: verify-transcript, diff
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What a run writes (design 9.2): @build/report/jig-<target>-<UTC stamp>@ as
JSON (the wire types' own, "Hsed.Wire.Internal"), as Markdown (the
verdicts, each waiver with its reason and page, the decodes, the pair
comparison) and as JUnit XML, through "Hsed.Jig.Files"; and the commands
that read a report back without privilege: @verify-transcript@ (the
commands issued; a failure if an IF-SEND was) and @diff@ (two reports' Level
0 decodes compared as the Pair runner compares them).

'RunResult' is what a run hands its reports; it and the bodies are
milestone M3's group G3's, stubs until it lands.
-}
module Hsed.Jig.Report
  ( CaseResult (..)
  , RunResult (..)
  , writeReports
  , verifyTranscript
  , diffReports
  ) where

import           Data.Time           (UTCTime)
import           RIO

import           Hsed.Jig.Catalogue  (CaseKey)
import           Hsed.Jig.Options    (RunOptions)
import           Hsed.Jig.Pair       (PairReport)
import           Hsed.Jig.Prereq     (Waiver)
import           Hsed.Jig.Script     (Verdict)
import           Hsed.Jig.Transcript (TEntry)
import           Hsed.Level0         (Level0)
import           Hsed.Wire           (Describe)


-- | What one case came to: its verdict, what it waived, and its extensions'.
data CaseResult = CaseResult
  { crKey        :: !CaseKey
  , crVerdict    :: !Verdict
  , crWaivers    :: ![Waiver]
  , crExtensions :: ![(Text, Verdict)]
  }
  deriving (Eq, Show)

-- | What a run came to.
data RunResult = RunResult
  { rrOptions    :: !RunOptions
  , rrVersion    :: !Text               -- ^ "Hsed.Version"'s
  , rrStarted    :: !UTCTime
  , rrTargets    :: ![Describe]         -- ^ what each transport said it is (two for a pair)
  , rrCases      :: ![CaseResult]
  , rrTranscript :: ![TEntry]
  , rrLevel0     :: ![(Text, Level0)]   -- ^ each end's Level 0 response, by the end's name
  , rrPair       :: !(Maybe PairReport)
  , rrIfSends    :: !Int                -- ^ the IF-SENDs issued on a drive (none at M3)
  , rrExit       :: !ExitCode
  }
  deriving (Eq, Show)

-- | Write a run's reports into a directory: the files written.
writeReports :: FilePath -> RunResult -> IO [FilePath]
writeReports = error "G3: writeReports"

-- | @verify-transcript REPORT.json [--no-if-send]@: print the commands a
--   report's transcript issued; with the flag, fail if one was an IF-SEND.
verifyTranscript :: FilePath -> Bool -> IO ExitCode
verifyTranscript = error "G3: verifyTranscript"

-- | @diff A.json B.json@: two reports' Level 0 decodes compared as the Pair
--   runner compares them.
diffReports :: FilePath -> FilePath -> IO ExitCode
diffReports = error "G3: diffReports"
