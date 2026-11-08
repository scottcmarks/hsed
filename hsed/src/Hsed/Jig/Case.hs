{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Case
Description : A test case: its key, section, waivers, own prerequisites, what it needs of a target, its steps
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

One case of the Test Cases as the jig runs it (design 9.1): its key in the
catalogue, the section whose common prerequisites apply, what it waives of
them and why, the prerequisites of its own that the runner establishes or
checks after its section's, in order (since M5b: ETC-20's Locking SP
Manufactured-Inactive; Test Cases 3.2.3, PDF 25), what it needs of the
target (a target that lacks it skips the case, saying why, "Hsed.Jig.Run"),
its steps, and the jig's own extensions to it, reported apart from the TCG
verdict (the simulator held in S2, an all-Empty-atom IF-SEND).

What a case needs of its target tells what a drive can run (design 9.3,
M4c): Level 0 Discovery, which every drive answers (UCT-01); IF-RECV on the
drive's base ComID and the IF-SENDs its allow list admits (Properties,
StartSession to the Admin SP as Anybody, Get on C_PIN_MSID, End of Session:
"Hsed.Jig.Guard"), which a drive takes behind the Guard once its base ComID
is bound, for design 9.3's drive cases alone (UCT-02, UCT-03 steps 1a-1c,
SPF-02 case 1; ETC-04 needs no more, but is not on the list), and a replay
never; the error paths off the allow list, which the RTL9210 bridge's
behaviour under a malformed IF-SEND keeps on the simulator (ETC-02, ETC-03,
ETC-08, ETC-15; design 9.3 item 6, 13.8); what the simulator alone can run
(ETC-21/adminsp, which authenticates SID; the simulator's control); and,
since M5b, a write to the TPer's state (a Set, TPER_RESET: SPF-11, SPF-12
case 1, ETC-14), which no drive takes before M6 (design 9.3; 13.34 (8)).
-}
module Hsed.Jig.Case
  ( Need (..)
  , Case (..)
  ) where

import           RIO

import           Hsed.Jig.Catalogue (CaseKey)
import           Hsed.Jig.Prereq    (Prereq, Section, Waiver)
import           Hsed.Jig.Script    (Script, Verdict)


-- | What a case needs of its target.
data Need
  = NeedLevel0      -- ^ Level 0 Discovery
  | NeedIfRecv      -- ^ IF-RECV on a ComID other than Level 0's
  | NeedIfSend      -- ^ IF-SEND of the methods the drive's allow list admits (design 9.3): a @'ReadWrite@ target, or a
                    --   drive under M4c's policy, its base ComID bound
  | NeedErrorPath   -- ^ IF-SEND off the allow list, an error path: the simulator's, no drive run opting in at M4c (design
                    --   9.3 item 6, 13.8)
  | NeedSimOnly     -- ^ the simulator alone: SID authenticated, or anything no drive policy admits
  | NeedSimControl  -- ^ the simulator's control ("Hsed.Sim.View".'Hsed.Sim.View.Control': a ComID's prepared response released),
                    --   which no case declares (M5c): the jig releases nothing, release being the hub's control op behind
                    --   hsed-sim's @--allow-remote-control@, and it reads the simulator's state through its view alone
                    --   ("Hsed.Jig.Script".@peekSim@)
  | NeedWrite       -- ^ a write to the TPer's state (since M5b): drives stay read-only until M6 (design 9.3; 13.34 (8))
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A case.
data Case = Case
  { cKey        :: !CaseKey
  , cSection    :: !Section
  , cWaives     :: ![Waiver]
  , cNeeds      :: ![Need]
  , cRun        :: Script Verdict
  , cExtensions :: ![(Text, Script Verdict)]
  , cOwnPrereqs :: ![Prereq]   -- ^ the case's own prerequisites, established or checked after its section's, in order
                               --   (Test Cases 3.2.3, PDF 25); none for most
  }
