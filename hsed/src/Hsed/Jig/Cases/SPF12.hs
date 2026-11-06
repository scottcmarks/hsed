{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.SPF12
Description : SPF-12 case 1, steps 1 to 5: TPER_RESET locks the global range where LockOnReset lists Programmatic
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SPF-12 case 1 (Test Cases 1.02, PDF 65-66), its steps 1 to 5, the jig's
variant @no-media@: with ProgrammaticResetEnable TRUE, the global range
unlocked with its locks enabled, Programmatic among its LockOnReset and the
MBR not enabled (its own prerequisites 1-4 and 6, PDF 65-66), StartSession
to the Locking SP as Admin1, TPER_RESET (Opal 2.30 3.2.3, PDF 28-29), a new
session as Admin1, whose Get finds the global range's ReadLocked and
WriteLocked TRUE (PDF 66), and CLOSE_SESSION. Steps 6 and 7, media I/O over
the range, are not run: no media I/O on the wire (design 8.5); the case
whole is M6's (Scott's answer Q2, design 11 M5b). The prerequisites are set
by Set and restored after the steps, whatever the verdict (Scott's answer
Q4). Its sessions need the Locking SP Manufactured, the common prerequisite
of 6.2 (PDF 49); its Sets and TPER_RESET are writes no drive takes before
M6, so the simulator alone runs it (design 9.3; 13.34 (8)). The steps are
group G4's (M5b), on the simulator's Set (group G1's) and the jig's
TPER_RESET (group G3's).
-}
module Hsed.Jig.Cases.SPF12
  ( spf12case1nomedia
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..))
import           Hsed.Jig.Script    (Verdict (..))


-- | The case.
spf12case1nomedia :: Case
spf12case1nomedia = Case
  { cKey        = CaseKey (SPF 12) (Just 1) Nothing (Just "no-media")
  , cSection    = SpecificFunctionality
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedWrite, NeedSimOnly]
  , cRun        = pure (Fail ["not yet written: group G4's (M5b)"])
  , cExtensions = []
  }
