{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.SPF11
Description : SPF-11, STACK_RESET rolls back a transaction: User1 enabled inside one, then a reset, then not
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SPF-11 (Test Cases 1.02, PDF 64-65): StartSession to the Locking SP as
Admin1, a Start Transaction, Set of User1's Enabled column TRUE, STACK_RESET,
which aborts the session and its open transaction (Core 3.3.4.7.5 items 1-2,
PDF 65); then a new session as Admin1, whose Get finds User1's Enabled
FALSE (PDF 65), and CLOSE_SESSION. Its own prerequisite is User1 not
enabled (PDF 64). Its sessions need the Locking SP Manufactured, the common
prerequisite of 6.2 (PDF 49); its Set is a write no drive takes before M6,
so the simulator alone runs it (design 9.3; 13.34 (8)). The steps are group
G4's (M5b), on the simulator's Set (group G1's) and its one level of
transactions (group G2's).
-}
module Hsed.Jig.Cases.SPF11
  ( spf11
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..))
import           Hsed.Jig.Script    (Verdict (..))


-- | The case.
spf11 :: Case
spf11 = Case
  { cKey        = CaseKey (SPF 11) Nothing Nothing Nothing
  , cSection    = SpecificFunctionality
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedWrite, NeedSimOnly]
  , cRun        = pure (Fail ["not yet written: group G4's (M5b)"])
  , cExtensions = []
  }
