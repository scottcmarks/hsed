{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.UCT03
Description : UCT-03 steps 1a to 1c, taking ownership: a session to the Admin SP, the MSID read, the session closed
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

UCT-03 (Test Cases 1.02, PDF 32-33), its step 1 as far as the jig takes it
read-only: 1a StartSession to the Admin SP with no authority, 1b Get on
C_PIN_MSID's PIN column, @[3 = 3, 4 = 3]@, which SUCCESS answers with
@[[3 = the MSID]]@, 1c the host's End of Session (CLOSE_SESSION, PDF 21),
answered with the TPer's. Steps 1d to 1f set the SID PIN and are not run,
which the verdict says; step 1 applies where Level 0's Opal SSC V2
descriptor says the SID's PIN is the MSID's (byte 13, 0x00), else the case
is skipped, step 2 needing the vendor's SID PIN. The MSID read goes among
the run's secrets, masked in every report unless @--show-msid@ (design 9.3
item 5). The jig's extensions on the simulator: Get on C_PIN_SID's PIN as
Anybody is NOT_AUTHORIZED with an empty result (no Test Case covers it); a
second StartSession while the first is open is SP_BUSY, or
NO_SESSIONS_AVAILABLE where MaxSessions is 1 (ETC-22's allowance, PDF 93);
and the MSID read is the simulator's parameter.

/At the M4a scaffold/ the case's key, section, waivers and needs are final
and its steps are group G3's (design 11).
-}
module Hsed.Jig.Cases.UCT03
  ( uct03steps1a1c
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched)


-- | The case: steps 1a to 1c of step 1.
uct03steps1a1c :: Case
uct03steps1a1c = Case
  { cKey        = CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing
  , cSection    = UseCases
  , cWaives     = [lockingSPUntouched]
  , cNeeds      = [NeedIfRecv, NeedIfSend]
  , cRun        = error "G3: Cases.UCT03.run"
  , cExtensions =
      [ ("Get C_PIN_SID's PIN as Anybody: NOT_AUTHORIZED, an empty result (Core 5.1.5.2, PDF 137)", error "G3: Cases.UCT03.sidPinRefused")
      , ("a second StartSession while the first is open: SP_BUSY, or NO_SESSIONS_AVAILABLE at MaxSessions 1 (Core 5.1.5.3, 5.1.5.7, PDF 138)", error "G3: Cases.UCT03.secondSession")
      , ("the MSID read is the simulator's", error "G3: Cases.UCT03.msidIsParameter")
      ]
  }
