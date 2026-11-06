{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Jig.Cases.ETC20
Description : ETC-20, StartSession on an inactive Locking SP: INVALID_PARAMETER
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-20 (Test Cases 1.02, PDF 91-92): StartSession to the Locking SP while
it is Manufactured-Inactive, which SyncSession answers INVALID_PARAMETER
(PDF 92; Core 5.1.5.11, PDF 139: the SPID names an SP no session can be
opened to, Opal 2.30 5.2.2.3.1, PDF 97), a session abort accepted in its
place (Test Cases 7.1, PDF 76), and no session left open. Its own
prerequisite 1, the Locking SP Manufactured-Inactive (PDF 91), contradicts
the common prerequisite of 7.2 that it be Manufactured (PDF 76): the case
waives that one and owns this one, which the runner establishes after its
section's ("Hsed.Jig.Run"; Test Cases 3.2.3, PDF 25), so that at
@--locking-ofs manufactured@ the case is skipped on it. The simulator alone
runs it, a drive's life cycle being unobservable (design 9.1, 9.3).
-}
module Hsed.Jig.Cases.ETC20
  ( etc20
  ) where

import qualified RIO.Map            as Map
import           RIO

import           Hsed.Gen.Core.Codes (pattern INVALID_PARAMETER)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Prereq (..), Section (..), Waiver (..))
import           Hsed.Jig.Script    (Script, StartArgs (..), Verdict (..), exchanged, expectSyncStatus, observe, peekSim, startSession,
                                     statusName, unmet)
import           Hsed.Sim.Types     (TPerState (..))


-- | The case.
etc20 :: Case
etc20 = Case
  { cKey        = CaseKey (ETC 20) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = [ Waiver LockingManufactured
                      "contradicted by the case's own prerequisite 1, the Locking SP Manufactured-Inactive (Test Cases ETC-20, PDF 91)" ]
  , cOwnPrereqs = [LockingManufacturedInactive]
  , cNeeds      = [NeedIfRecv, NeedSimOnly]
  , cRun        = run
  , cExtensions = []
  }

-- | Step 1: StartSession to the Locking SP, as Anybody (the Test Cases name
--   no authority, PDF 91), read-write: SyncSession INVALID_PARAMETER (PDF
--   92), an abort accepted (7.1, PDF 76), and no session left open on the
--   simulator.
run :: Script Verdict
run = do
  answer <- startSession (StartArgs uSP_Locking Nothing Nothing True) >>= exchanged step1
  expectSyncStatus step1 INVALID_PARAMETER answer
  after <- peekSim
  for_ after $ \s -> unless (Map.null (tsSessions s)) $
    unmet (tshow (Map.size (tsSessions s)) <> " session(s) open after the refused StartSession, where none should be (Core 5.3.4.1.5, PDF 215)")
  observe "step 1" ("StartSession to the Locking SP, Manufactured-Inactive: SyncSession " <> statusName INVALID_PARAMETER
                    <> ", no session open (Test Cases ETC-20, PDF 92; Opal 2.30 5.2.2.3.1, PDF 97)")
  pure Pass
  where step1 = "step 1: StartSession to the Locking SP while it is Manufactured-Inactive"
