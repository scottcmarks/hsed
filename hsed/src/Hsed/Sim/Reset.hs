{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim.Reset
Description : What a reset does to the simulated TPer: an interface event, TPER_RESET, STACK_RESET on a ComID
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The resets of design 8.4 as M4a has them: an interface reset (a power
cycle, a hardware reset, a hot plug: Opal 2.30 3.3.5.1 and Table 16, PDF
31-32) and TPER_RESET (Opal 3.2.3, PDF 28-29) abort every session, drop
every staged response and return every ComID to Awaiting IF-SEND (Core
3.3.7.1.5, PDF 76; 3.3.10.6 S2:S1 a and S3:S1 a, PDF 85-86); only a power
cycle, a hardware reset and TPER_RESET reset the host's properties to the
initial assumptions, a hot plug keeps them (Core 5.2.2.4.4, PDF 148; Opal
3.2.3 g); a power cycle alone resets Tries where Persistence is False (Core
5.3.4.1.1.2, PDF 210). STACK_RESET does a ComID's share (Core 3.3.4.7.5,
PDF 65-66; 5.2.2.4.5, PDF 148): its sessions gone with no CloseSession, its
staged response dropped, S1, its host properties initial.

/At the M4a scaffold/ 'event' does what M3 did, every ComID back to Awaiting
IF-SEND, and 'resetTPer' and 'stackReset' are stubs; group G2 writes them
(design 11).
-}
module Hsed.Sim.Reset
  ( ResetKind (..)
  , event
  , resetTPer
  , stackReset
  ) where

import           RIO

import           Hsed.Sim.Types (ComIdState (..), SyncState (..), TPerState (..))
import           Hsed.Wire      (Event)


-- | What reset the TPer: an interface event of each kind (Opal 2.30 Table
--   16, PDF 32), or TPER_RESET (Opal 3.2.3, PDF 28-29).
data ResetKind
  = OnPowerCycle
  | OnHardwareReset
  | OnHotPlug
  | OnTPerReset
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | An interface event: every ComID back to Awaiting IF-SEND, its prepared
--   response dropped (Core 3.3.10.6 S2:S1 a and S3:S1 a, PDF 85-86). The
--   rest of the reset rules are 'resetTPer''s.
event :: Event -> TPerState -> TPerState
event _ st = st { tsComIds = fmap (\cs -> cs { cSync = S1 }) (tsComIds st) }

-- | A reset of the whole TPer, of a kind: sessions, synchronous states,
--   buffers, host properties and Tries as the module's description says.
resetTPer :: ResetKind -> TPerState -> TPerState
resetTPer = error "G2: Reset.resetTPer"

-- | STACK_RESET on a ComID (Core 3.3.4.7.5 items 1-8, PDF 65-66).
stackReset :: Word16 -> TPerState -> TPerState
stackReset = error "G2: Reset.stackReset"
