{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC14
Description : ETC-14, TPer properties exceeded in a regular session: a packet of MaxSubpackets + 1 subpackets
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-14 (Test Cases 1.02, PDF 87): Properties for MaxSubpackets,
StartSession to the Locking SP as Admin1, a packet of the session with
MaxSubpackets + 1 subpackets, each a Set on the DataStore table, which
aborts the session (Core 5.2.2.4.1.1, PDF 145), the IF-RECV answering "All
Response(s) returned - no further data" or a ComPacket with a CloseSession
(PDF 87; Test Cases 7.1, PDF 76); then a new session to the Locking SP as
Admin1, and CLOSE_SESSION. Its sessions need the Locking SP Manufactured,
the common prerequisite of 7.2 (PDF 76); its packet is an IF-SEND off a
drive's allow list carrying Sets, writes no drive takes before M6, so the
simulator alone runs it (design 9.3; 13.34 (8)). The steps are group G4's
(M5b).
-}
module Hsed.Jig.Cases.ETC14
  ( etc14
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..))
import           Hsed.Jig.Script    (Verdict (..))


-- | The case.
etc14 :: Case
etc14 = Case
  { cKey        = CaseKey (ETC 14) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedWrite, NeedErrorPath, NeedSimOnly]
  , cRun        = pure (Fail ["not yet written: group G4's (M5b)"])
  , cExtensions = []
  }
