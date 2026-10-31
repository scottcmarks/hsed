{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Jig.Cases.ETC21
Description : ETC-21, the Admin SP variant: StartSession as SID with a wrong HostChallenge
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-21 (Test Cases 1.02, PDF 92) authenticates Admin1 to a Manufactured
Locking SP with a wrong HostChallenge, which waits for M5; its variant of
the jig's own, @ETC-21/adminsp@, does the same to the Admin SP as SID: the
SID's PIN plus a byte, which SyncSession answers NOT_AUTHORIZED (Core
5.1.5.2, PDF 138; 5.3.4.1.10, PDF 217), an abort accepted (Test Cases 7.1,
PDF 76), and no session is left open. The simulator alone runs it: a drive's
policy never admits SID (design 9.3), and the PIN comes from the simulator's
state ("Hsed.Jig.Script".@simCredential@; the run's CredentialsKnown reads
the same and keeps it among the secrets). Its own prerequisite is
C_PIN_SID's TryLimit 0 or Tries below it (the case's prerequisite 1, PDF
92), read from the state, the case skipped where it does not hold; the
jig's extension is the Tries rule (Core 5.3.4.1.1.2, PDF 210: a failure
counts while TryLimit is not 0, never past it).
-}
module Hsed.Jig.Cases.ETC21
  ( etc21adminsp
  ) where

import qualified RIO.Map            as Map
import           RIO

import           Hsed.Gen.Core.Codes (pattern NOT_AUTHORIZED)
import           Hsed.Gen.Core.UIDs (uAuthority_SID, uSP_Admin)
import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched)
import           Hsed.Jig.Script    (Credential (..), Script, StartArgs (..), Verdict (..), exchanged, expectSyncStatus, observe,
                                     peekSim, simCredential, startSession, unmet)
import           Hsed.Sim.Types     (TPerState (..))


-- | The case.
etc21adminsp :: Case
etc21adminsp = Case
  { cKey        = CaseKey (ETC 21) Nothing Nothing (Just "adminsp")
  , cSection    = ErrorCases
  , cWaives     = [lockingSPUntouched]
  , cNeeds      = [NeedIfRecv, NeedSimOnly]
  , cRun        = run
  , cExtensions = [("the Tries rule: a failure counts while TryLimit is not 0, never past it (Core 5.3.4.1.1.2, PDF 210)", tries)]
  }

-- | The case's own prerequisite checked, then step 1: StartSession to the
--   Admin SP as SID with the PIN plus a byte, NOT_AUTHORIZED (an abort
--   accepted), and no session left open.
run :: Script Verdict
run = peekSim >>= \case
  Nothing -> pure (Skipped "the simulator's alone: SID's PIN is read from its state (design 9.3)")
  Just st -> do
    cr <- credential st
    case locked cr of
      Just why -> pure (Skipped ("prerequisite unmet: " <> why))
      Nothing  -> do
        observe "prerequisite" ("C_PIN_SID TryLimit " <> tshow (crTryLimit cr) <> ", Tries " <> tshow (crTries cr)
                                <> " (Test Cases ETC-21 prerequisite 1, PDF 92)")
        answer <- startSession (wrongChallenge cr) >>= exchanged step1
        expectSyncStatus step1 NOT_AUTHORIZED answer
        after <- peekSim
        for_ after $ \s -> unless (Map.null (tsSessions s)) $
          unmet (tshow (Map.size (tsSessions s)) <> " session(s) open after the failed StartSession, where none should be (Core 5.3.4.1.5, PDF 215)")
        pure Pass
  where step1 = "step 1: StartSession to the Admin SP as SID with the PIN plus '!'"

-- | SID's credential in the simulator's Admin SP.
credential :: TPerState -> Script Credential
credential = maybe (unmet "the simulator's Admin SP holds no credential for SID (Opal 2.30 Table 24, PDF 48)") pure . simCredential uAuthority_SID

-- | Why the case's own prerequisite does not hold, if it does not: a
--   TryLimit other than 0 that Tries has reached (Test Cases PDF 92; Core
--   5.3.4.1.1.2, PDF 210).
locked :: Credential -> Maybe Text
locked cr
  | crTryLimit cr /= 0 && crTries cr >= crTryLimit cr =
      Just ("C_PIN_SID's Tries " <> tshow (crTries cr) <> " is not below its TryLimit " <> tshow (crTryLimit cr))
  | otherwise = Nothing

-- | StartSession to the Admin SP as SID with a HostChallenge other than its
--   PIN: the PIN plus a byte.
wrongChallenge :: Credential -> StartArgs
wrongChallenge cr = StartArgs uSP_Admin (Just uAuthority_SID) (Just (crPIN cr <> "!")) True

-- | The extension: Tries before and after a failed StartSession (Core
--   5.3.4.1.1.2, PDF 210): unchanged while TryLimit is 0, one more
--   otherwise, never past TryLimit.
tries :: Script Verdict
tries = peekSim >>= \case
  Nothing -> pure (Skipped "the simulator's alone")
  Just before -> do
    cr0 <- credential before
    case locked cr0 of
      Just why -> pure (Skipped why)
      Nothing  -> do
        answer <- startSession (wrongChallenge cr0) >>= exchanged name
        expectSyncStatus name NOT_AUTHORIZED answer
        cr1 <- peekSim >>= maybe (unmet "no simulator state to read Tries from") credential
        let expected
              | crTryLimit cr0 == 0 = crTries cr0
              | otherwise           = min (crTryLimit cr0) (crTries cr0 + 1)
        observe "Tries" (tshow (crTries cr0) <> " before, " <> tshow (crTries cr1) <> " after, TryLimit " <> tshow (crTryLimit cr0))
        unless (crTries cr1 == expected) $
          unmet ("Tries " <> tshow (crTries cr1) <> " after the failure, not " <> tshow expected
                 <> " (Core 5.3.4.1.1.2, PDF 210: a failure counts while TryLimit is not 0, never past it)")
        pure Pass
  where name = "StartSession to the Admin SP as SID with a wrong HostChallenge"
