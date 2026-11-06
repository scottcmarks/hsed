{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC20
Description : ETC-20, StartSession on an inactive Locking SP: INVALID_PARAMETER
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-20 (Test Cases 1.02, PDF 91-92): StartSession to the Locking SP while
it is Manufactured-Inactive, which SyncSession answers INVALID_PARAMETER
(PDF 92). Its own prerequisite 1, the Locking SP Manufactured-Inactive (PDF
91), contradicts the common prerequisite of 7.2 that it be Manufactured
(PDF 76): the case waives that one and owns this one, which the runner
establishes after its section's ("Hsed.Jig.Run"; Test Cases 3.2.3, PDF 25).
The simulator alone runs it, a drive's life cycle being unobservable (design
9.1, 9.3). The step is group G3's (M5b).
-}
module Hsed.Jig.Cases.ETC20
  ( etc20
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Prereq (..), Section (..), Waiver (..))
import           Hsed.Jig.Script    (Verdict (..))


-- | The case.
etc20 :: Case
etc20 = Case
  { cKey        = CaseKey (ETC 20) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = [ Waiver LockingManufactured
                      "contradicted by the case's own prerequisite 1, the Locking SP Manufactured-Inactive (Test Cases ETC-20, PDF 91)" ]
  , cOwnPrereqs = [LockingManufacturedInactive]
  , cNeeds      = [NeedIfRecv, NeedSimOnly]
  , cRun        = pure (Fail ["not yet written: group G3's (M5b)"])
  , cExtensions = []
  }
