{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC21
Description : ETC-21, the Admin SP variant: StartSession as SID with a wrong HostChallenge
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-21 (Test Cases 1.02, PDF 92) authenticates Admin1 to a Manufactured
Locking SP with a wrong HostChallenge, which waits for M5; its variant of
the jig's own, @ETC-21/adminsp@, does the same to the Admin SP as SID: the
SID's PIN plus a byte, which SyncSession answers NOT_AUTHORIZED (Core
5.1.5.2, PDF 138; 5.3.4.1.10, PDF 217), an abort accepted (Test Cases 7.1,
PDF 76), and no session is left open. The simulator alone runs it: a drive's
policy never admits SID (design 9.3). Its own prerequisite is C_PIN_SID's
TryLimit 0 or Tries below it, read from the simulator's state; the jig's
extension is the Tries rule (Core 5.3.4.1.1.2, PDF 210: Tries counts the
failure while TryLimit is not 0).

/At the M4a scaffold/ the case's key, section, waivers and needs are final
and its steps are group G3's (design 11).
-}
module Hsed.Jig.Cases.ETC21
  ( etc21adminsp
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched)


-- | The case.
etc21adminsp :: Case
etc21adminsp = Case
  { cKey        = CaseKey (ETC 21) Nothing Nothing (Just "adminsp")
  , cSection    = ErrorCases
  , cWaives     = [lockingSPUntouched]
  , cNeeds      = [NeedIfRecv, NeedSimOnly]
  , cRun        = error "G3: Cases.ETC21.run"
  , cExtensions = [("the Tries rule: a failure counts while TryLimit is not 0, never past it (Core 5.3.4.1.1.2, PDF 210)", error "G3: Cases.ETC21.tries")]
  }
