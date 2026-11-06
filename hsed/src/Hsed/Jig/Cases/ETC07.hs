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
EndList token before the Call token, a token outside a method on the
control session, which the TPer discards with the payload (Opal 2.30
3.3.4.1.3, PDF 31; Core 3.2.2.4.2 item 2, PDF 39), the next IF-RECV
answering "All Response(s) returned - no further data", (0, 0, 0) (Core
Table 45, PDF 83); then StartSession to the Locking SP, which succeeds.
The Test Cases print no CLOSE_SESSION step, so the jig ends step 2's
session itself, noted. Its session to the Locking SP needs it
Manufactured, the common prerequisite of 7.2 (PDF 76), and its step 1 an
IF-SEND off a drive's allow list, so the simulator alone runs it (design
9.3, 11 M5b).
-}
module Hsed.Jig.Cases.ETC07
  ( etc07
  ) where

import           RIO

import           Hsed.Core.StreamItem (StreamItem (..))
import           Hsed.Core.Token      (Token (..))
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import           Hsed.Jig.Case        (Case (..), Need (..))
import           Hsed.Jig.Catalogue   (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq      (Section (..))
import           Hsed.Jig.Script      (Script, StartArgs (..), Verdict (..), baseComId, controlComPacket, exchangeOn, expectNoResponse,
                                       note, observe, startSessionCall, unmet, withSession)


-- | The case.
etc07 :: Case
etc07 = Case
  { cKey        = CaseKey (ETC 7) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedErrorPath, NeedSimOnly]
  , cRun        = run
  , cExtensions = []
  }

-- | StartSession to the Locking SP as Anybody (the Test Cases name no
--   authority, PDF 80), read-write: step 1's call, and step 2's session.
toLockingSP :: StartArgs
toLockingSP = StartArgs uSP_Locking Nothing Nothing True

-- | Step 1, then step 2: a session to the Locking SP opened, which
--   succeeds, and ended by the jig's own End of Session, the Test Cases
--   printing no CLOSE_SESSION step (PDF 80), noted.
run :: Script Verdict
run = do
  step1
  withSession toLockingSP $ \_ -> do
    observe "step 2" "StartSession to the Locking SP: SyncSession SUCCESS (0), a session opened (Test Cases ETC-07, PDF 80)"
    note "ETC-07: step 2's session is ended by the jig's own End of Session: the Test Cases print no CLOSE_SESSION step (PDF 80)"
    pure Pass

-- | Step 1: the StartSession call with an EndList token before its Call
--   token, in one packet of the control session on the base ComID (Test
--   Cases Table 6, PDF 27): no response, the IF-RECV answering (0, 0, 0)
--   (Opal 2.30 3.3.4.1.3, PDF 31; Core Table 45, PDF 83).
step1 :: Script ()
step1 = do
  base <- baseComId
  bytes <- either (unmet . ("step 1: the ComPacket does not encode: " <>) . tshow) pure
                  (controlComPacket base (generate EndList <> generate (startSessionCall toLockingSP)))
  exchangeOn base bytes >>= expectNoResponse name
  observe "step 1" ("the IF-RECV answered (0, 0, 0): all responses returned, no further data (Core Table 45, PDF 83); the payload with its "
                    <> "EndList before the Call token discarded (Opal 2.30 3.3.4.1.3, PDF 31)")
  where name = "step 1: StartSession to the Locking SP with an EndList token before the Call token"
