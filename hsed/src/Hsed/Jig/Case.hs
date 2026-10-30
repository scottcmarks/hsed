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
case, saying why: a drive is @'ReadOnly@ at M3, design 9.3), its steps, and
the jig's own extensions to it, reported apart from the TCG verdict (the
simulator held in S2, an all-Empty-atom IF-SEND).
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
  | NeedIfSend      -- ^ IF-SEND: a @'ReadWrite@ target
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
