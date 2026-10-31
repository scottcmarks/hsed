{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC04
Description : ETC-04, a packet on a session the TPer does not have
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-04 (Test Cases 1.02, PDF 78-79): a session opened to the Admin SP, then
Get on C_PIN_MSID in a packet whose Session names the session's TSN and the
HSN plus one, which the TPer cannot resolve and ignores (Core 3.3.10.7 a,
PDF 86: the law @Sync.ignorePacket@'s first clause), the next IF-RECV
answering (0, 0, 0); then the session's End of Session. The session is left
open through the bad packet, so the case opens one and closes it.

/At the M4a scaffold/ the case's key, section, waivers and needs are final
and its steps are group G3's (design 11).
-}
module Hsed.Jig.Cases.ETC04
  ( etc04
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched)


-- | The case.
etc04 :: Case
etc04 = Case
  { cKey        = CaseKey (ETC 4) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = [lockingSPUntouched]
  , cNeeds      = [NeedIfRecv, NeedIfSend]
  , cRun        = error "G3: Cases.ETC04.run"
  , cExtensions = []
  }
