{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.UCT02
Description : UCT-02, Properties
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

UCT-02 (Test Cases 1.02, PDF 31-32): Properties with the host's own
(MaxComPacketSize 4096, MaxPacketSize 4076, MaxIndTokenSize 4040), the
TPer's eleven properties each at least the Test Cases' minimum (step 2,
MaxTranactionLimit read as Core Table 167's MaxTransactionLimit,
DefSessionTimeout at least 0; a TPer property of 0 where Table 167 says 0
means no limit meets the minimum, said in an observation: the lead's M4a
decision, design 11), and
the host's three answered within their ranges (step 3). Both tables go into
the report's observations. The jig's extension on the simulator: the host's
values echoed exactly (Opal 2.30 Table 17's maxima being VU, the simulator
clamps none by default).

/At the M4a scaffold/ the case's key, section, waivers and needs are final
and its steps are group G3's (design 11).
-}
module Hsed.Jig.Cases.UCT02
  ( uct02
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched, noSessionOpened)


-- | The case.
uct02 :: Case
uct02 = Case
  { cKey        = CaseKey (UCT 2) Nothing Nothing Nothing
  , cSection    = UseCases
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedIfRecv, NeedIfSend]
  , cRun        = error "G3: Cases.UCT02.run"
  , cExtensions = [("the host's properties echoed exactly: 4096, 4076, 4040 (Opal 2.30 Table 17's maxima are VU)", error "G3: Cases.UCT02.echoed")]
  }
