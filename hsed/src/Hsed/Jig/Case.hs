{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Case
Description : A test case: its key, section, waivers, what it needs of a target, its steps
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

One case of the Test Cases as the jig runs it (design 9.1): its key in the
catalogue, the section whose common prerequisites apply, what it waives of
them and why, what it needs of the target (a target that lacks it skips the
case, saying why: a drive is @'ReadOnly@ until M4c, design 9.3), its steps,
and the jig's own extensions to it, reported apart from the TCG verdict (the
simulator held in S2, an all-Empty-atom IF-SEND).

What a case needs of its target tells the allow-listed IF-SENDs a drive
will take under M4c's policy (UCT-02, UCT-03 step 1, ETC-04: Properties,
StartSession to the Admin SP, Get on C_PIN_MSID, End of Session) from the
error paths off it, which the RTL9210 bridge's behaviour under a malformed
IF-SEND makes opt-in (ETC-02, ETC-03, ETC-08, ETC-15; design 9.3 item 6,
13.8), and from what the simulator alone can run (ETC-21/adminsp, which
authenticates SID; the simulator's control).
-}
module Hsed.Jig.Case
  ( Need (..)
  , Case (..)
  ) where

import           RIO

import           Hsed.Jig.Catalogue (CaseKey)
import           Hsed.Jig.Prereq    (Section, Waiver)
import           Hsed.Jig.Script    (Script, Verdict)


-- | What a case needs of its target.
data Need
  = NeedLevel0      -- ^ Level 0 Discovery
  | NeedIfRecv      -- ^ IF-RECV on a ComID other than Level 0's
  | NeedIfSend      -- ^ IF-SEND of the methods the drive's allow list admits (design 9.3): a @'ReadWrite@ target, or a
                    --   drive under M4c's policy
  | NeedErrorPath   -- ^ IF-SEND off the allow list, an error path: the simulator's until a drive run opts in (design 9.3
                    --   item 6, M4c)
  | NeedSimOnly     -- ^ the simulator alone: SID authenticated, or anything no drive policy admits
  | NeedSimControl  -- ^ the simulator's control (hold, release, state)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A case.
data Case = Case
  { cKey        :: !CaseKey
  , cSection    :: !Section
  , cWaives     :: ![Waiver]
  , cNeeds      :: ![Need]
  , cRun        :: Script Verdict
  , cExtensions :: ![(Text, Script Verdict)]
  }
