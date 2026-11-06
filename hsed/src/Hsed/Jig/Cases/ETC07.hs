{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC07
Description : ETC-07, an unexpected token outside of a method on the control session: an EndList before StartSession's Call
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-07 (Test Cases 1.02, PDF 80): StartSession to the Locking SP with an
EndList token before the Call token, which the TPer discards, the next
IF-RECV answering "All Response(s) returned - no further data" (Opal 2.30
3.3.4.1.3, PDF 31; Core Table 45, PDF 83); then StartSession to the Locking
SP, which succeeds. Its session to the Locking SP needs it Manufactured, the
common prerequisite of 7.2 (PDF 76), and an IF-SEND off a drive's allow
list, so the simulator alone runs it (design 9.3, 11 M5b). The steps are
group G3's (M5b).
-}
module Hsed.Jig.Cases.ETC07
  ( etc07
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..))
import           Hsed.Jig.Script    (Verdict (..))


-- | The case.
etc07 :: Case
etc07 = Case
  { cKey        = CaseKey (ETC 7) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedErrorPath, NeedSimOnly]
  , cRun        = pure (Fail ["not yet written: group G3's (M5b)"])
  , cExtensions = []
  }
