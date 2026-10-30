{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC15
Description : ETC-15, more subpackets in a control session packet than the TPer's MaxSubpackets
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-15 (Test Cases 1.02, PDF 88): Properties for the TPer's MaxSubpackets,
n, then a control session packet of n + 1 Properties subpackets, which the
TPer discards and ignores (Core 5.2.2.4.1.1, PDF 145; 3.3.10.7 a, PDF 86:
the law @Sync.ignorePacket@'s second clause), the next IF-RECV answering
(0, 0, 0). A MaxSubpackets of 0, no limit (Core Table 167), leaves the case
not applicable, skipped (design 13.29 (b); the lead's M4a decision).

/At the M4a scaffold/ the case's key, section, waivers and needs are final
and its steps are group G3's (design 11).
-}
module Hsed.Jig.Cases.ETC15
  ( etc15
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched, noSessionOpened)


-- | The case.
etc15 :: Case
etc15 = Case
  { cKey        = CaseKey (ETC 15) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedIfRecv, NeedErrorPath]
  , cRun        = error "G3: Cases.ETC15.run"
  , cExtensions = []
  }
