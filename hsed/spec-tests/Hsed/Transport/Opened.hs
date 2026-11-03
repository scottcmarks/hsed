{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Opened
Description : The in-process transport opened for a test, its parameters' SPs folding
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Since M5a's scaffold "Hsed.Transport.InProc".'newInProc' refuses parameters
the SPs' preconfiguration does not fold with, returning what keeps them
from folding ("Hsed.Sim.Types".'Hsed.Sim.Types.initialStateProblems'). The
suites open it with parameters that fold: 'openedInProc' is the transport
and its control, or a test failure naming the problems.
-}
module Hsed.Transport.Opened
  ( openedInProc
  ) where

import           RIO
import qualified RIO.Text              as T

import           Hsed.Sim.Types        (SimParams, foldProblemText)
import           Hsed.Transport.InProc (InProcOpts, SimControl, newInProc)
import           Hsed.Wire             (Cap (..), Transport)


-- | The simulator as a transport and its control, its parameters' SPs
--   folding; a failure naming the problems where they do not.
openedInProc :: SimParams -> InProcOpts -> IO (Transport 'ReadWrite, SimControl)
openedInProc p opts = newInProc p opts >>= either refused pure
  where
    refused problems = throwString ("the simulator's SPs do not fold: " <> T.unpack (T.intercalate "; " (map foldProblemText problems)))
