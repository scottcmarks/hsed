{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC08
Description : ETC-08, an unexpected token in a control session method's parameter list
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-08 (Test Cases 1.02, PDF 81): Properties with a Start List where its
parameter list's values go (the bytes @F8 A8 00 00 00 00 00 00 00 FF A8 00
00 00 00 00 00 FF 01 F0 F0 F1 F1 F9 F0 00 00 00 F1@), which the TPer may
ignore, the next IF-RECV answering (0, 0, 0), or answer INVALID_PARAMETER;
SUCCESS fails the case. The simulator ignores it (Core 3.2.4.2 MAY; Opal
2.30 3.3.4.1.3, PDF 31; design 13.29 (h)).

/At the M4a scaffold/ the case's key, section, waivers and needs are final
and its steps are group G3's (design 11).
-}
module Hsed.Jig.Cases.ETC08
  ( etc08
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched, noSessionOpened)


-- | The case.
etc08 :: Case
etc08 = Case
  { cKey        = CaseKey (ETC 8) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedIfRecv, NeedErrorPath]
  , cRun        = error "G3: Cases.ETC08.run"
  , cExtensions = []
  }
