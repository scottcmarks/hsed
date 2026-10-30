{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim.SessionManager
Description : The simulated TPer's Session Manager: SMUID.Properties at M3
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Session Manager's answers on the control session (Core 5.2): at M3
the TPer's half of Properties (Core 5.2.2.1, PDF 140), its
properties the parameters' (Opal 2.30 Table 17's minima by default,
"Hsed.Sim.Types".'Hsed.Sim.Types.defaultSimParams'), which the jig's ETC-02
and ETC-03 need; the host's half and the rest of the Session Manager at
M4a. A Session Manager response is formatted as a method invocation (Core
3.2.4.2), so the answer is a 'MethodCall'.

Until group G2 writes it, 'answer' answers nothing: every control session
payload is discarded.
-}
module Hsed.Sim.SessionManager
  ( answer
  ) where

import           RIO

import           Hsed.Core.Call (MethodCall)
import           Hsed.Sim.Types (SimParams)


-- | The Session Manager's answer to a method call on the control session,
--   if it has one: at M3, SMUID.Properties alone.
answer :: SimParams -> MethodCall -> Maybe MethodCall
answer _ _ = Nothing
