{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC09
Description : ETC-09, exceeding the transaction limit: MaxTransactionLimit + 1 Start Transaction tokens in one subpacket
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-09 (Test Cases 1.02, PDF 81-82): Properties for MaxTransactionLimit,
StartSession to the Locking SP as Admin1, a subpacket of MaxTransactionLimit
+ 1 Start Transaction tokens, which aborts the session (Core 3.2.2.3.3.4,
PDF 37: a Start Transaction past the nesting limit), the IF-RECV answering
"All Response(s) returned - no further data" or a ComPacket with a
CloseSession (PDF 82; Test Cases 7.1, PDF 76); then a new session to the
Locking SP as Admin1, and CLOSE_SESSION. Its sessions need the Locking SP
Manufactured, the common prerequisite of 7.2 (PDF 76), and the over-limit
subpacket is an IF-SEND off a drive's allow list, so the simulator alone
runs it (design 9.3, 11 M5b). The steps are group G4's (M5b), on the
simulator's one level of transactions (group G2's).
-}
module Hsed.Jig.Cases.ETC09
  ( etc09
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..))
import           Hsed.Jig.Script    (Verdict (..))


-- | The case.
etc09 :: Case
etc09 = Case
  { cKey        = CaseKey (ETC 9) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedErrorPath, NeedSimOnly]
  , cRun        = pure (Fail ["not yet written: group G4's (M5b)"])
  , cExtensions = []
  }
