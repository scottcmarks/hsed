{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim
Description : A step of the simulated TPer: routing by protocol and ComID, the units, the synchronous protocol
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The simulator's one function (design 8.2): an input and a TPer to an output
and the TPer after it. A command is first held to its unit (SIIS 1.20
4.3.1.1-4.3.2.2, PDF 30-31), then routed: protocol 1, ComID 1 to Level 0
("Hsed.Sim.Level0"), outside the synchronous protocol; protocol 1 on a
static ComID to its synchronous protocol ("Hsed.Sim.Sync"), the Session
Manager answering the control session ("Hsed.Sim.SessionManager"); an
inactive or unsupported ComID as Core 3.3.3.2-3 says (PDF 58-59); protocol 2
at M4a; any other protocol refused. An event resets the ComIDs; a 'Tick'
releases a ComID's prepared response (S2 to S3).

'step' is milestone M3's group G1's; until it lands it is a stub.
-}
module Hsed.Sim
  ( step
  , syncOf
  ) where

import qualified RIO.Map        as Map
import           RIO

import           Hsed.Sim.Types (ComIdState (..), Input, Output, SyncState, TPerState (..))


-- | One step of the TPer.
step :: Input -> TPerState -> (Output, TPerState)
step = error "G1: step"

-- | A static ComID's synchronous protocol state, if the TPer has the ComID.
syncOf :: Word16 -> TPerState -> Maybe SyncState
syncOf c = fmap cSync . Map.lookup c . tsComIds
