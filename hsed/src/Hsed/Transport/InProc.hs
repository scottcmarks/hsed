{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.InProc
Description : The simulated TPer as a 'ReadWrite transport in the jig's own process
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The in-process transport (design 7.1): the TPer's state in a variable, each
command a step of "Hsed.Sim", the unit the run chooses, and the release of a
ComID's prepared response ('Hsed.Sim.Types.Tick') after every IF-SEND,
unless the run holds it for a number of IF-RECVs (@--hold-processing@), so
that the host's loop sees the TPer still processing (Core Table 45 row 1,
PDF 83). A @'ReadWrite@ transport, with a 'SimControl' the jig's simulator
extensions use: release a ComID now, look at the state.

'newInProc' is milestone M3's group G1's; until it lands it is a stub.
-}
module Hsed.Transport.InProc
  ( InProcOpts (..)
  , SimControl (..)
  , newInProc
  ) where

import           RIO

import           Hsed.Sim.Types (SimParams, TPerState)
import           Hsed.Wire      (Cap (..), Transport, Unit)


-- | The transport's unit, and how many IF-RECVs a ComID's prepared response
--   is held in S2 for (0: released at once).
data InProcOpts = InProcOpts
  { ioUnits       :: !Unit
  , ioHoldIfRecvs :: !Int
  }
  deriving (Eq, Show)

-- | What the jig may do to the simulator besides commands: release a ComID's
--   prepared response (S2 to S3), and read the state.
data SimControl = SimControl
  { scRelease :: Word16 -> IO ()
  , scPeek    :: IO TPerState
  }

-- | A simulated TPer from its parameters, as a transport and its control.
newInProc :: SimParams -> InProcOpts -> IO (Transport 'ReadWrite, SimControl)
newInProc = error "G1: newInProc"
