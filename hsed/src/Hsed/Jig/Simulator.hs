{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Simulator
Description : The simulator a run's SimTarget carries: how it was opened, its view, its control, its notices
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The simulator's end of a run (design 9.2, 11 M5c, Option A): besides its
transport, what the jig reads of it and asks of it outside the protocol,
the same whether it was opened in the jig's own process or over the LAN
leg ('Opened'): its state through typed queries ("Hsed.Sim.View".'SimView'),
its control ("Hsed.Sim.View".'Control': a ComID's prepared response
released), and what happened to it meanwhile that the run must hear of
('simNotices': a hub that restarted, broadcasts missed, another client's
reset; none in process). 'inProcess' is the in-process simulator's, over
its "Hsed.Transport.InProc".'SimControl'.

'simStateInProc' is transitional: the in-process state itself, which the
jig's reads of the simulator still take at M5c's scaffold (four of them:
"Hsed.Jig.Run"'s @establish@ twice, "Hsed.Jig.Script".@peekSim@, SPF-02's
extension); group G4 turns each into a query of 'simView' and deletes it, so
that a @ws://@ simulator, which has no such state in the jig's process, is
read the same way.
-}
module Hsed.Jig.Simulator
  ( Opened (..)
  , Simulator (..)
  , inProcess
  ) where

import           RIO

import           Hsed.Jig.Options      (WsUrl)
import           Hsed.Sim.Types        (TPerState)
import           Hsed.Sim.View         (Control, SimView)
import           Hsed.Transport.InProc (SimControl (..), inProcControl, inProcView)


-- | How a run's simulator was opened.
data Opened
  = InProcess      -- ^ in the jig's own process (@--target sim@)
  | OverWs !WsUrl  -- ^ an hsed-sim hub's, over mh_transport frames on 127.0.0.1 (@--target ws://127.0.0.1:PORT/PATH@)
  deriving (Eq, Show)

-- | The simulator a run's 'Hsed.Jig.Target.SimTarget' carries beside its
--   transport.
data Simulator = Simulator
  { simOpened      :: !Opened
  , simView        :: !SimView
  , simControl     :: !Control
  , simNotices     :: IO [Text]                -- ^ what happened to the simulator since last asked, each once
  , simStateInProc :: !(Maybe (IO TPerState))  -- ^ transitional: the in-process state; group G4 deletes it
  }

-- | The in-process simulator, over its control: its view and its control
--   from it ("Hsed.Transport.InProc".'inProcView', 'inProcControl'), no
--   notices, its state read as the control reads it.
inProcess :: SimControl -> Simulator
inProcess c = Simulator
  { simOpened      = InProcess
  , simView        = inProcView c
  , simControl     = inProcControl c
  , simNotices     = pure []
  , simStateInProc = Just (scPeek c)
  }
