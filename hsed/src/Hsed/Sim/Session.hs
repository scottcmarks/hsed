{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim.Session
Description : A regular session's packets processed: each invocation dispatched through the access control and the behaviours, End of Session, an abort
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The regular-session half of the synchronous protocol (design 8.2, M4a): a
packet whose Session resolves to an open session ("Hsed.Sim.State") is read
as a session reads it ("Hsed.Core.Stream", 'SessionStream'), each invocation
dispatched (the access control gate first, "Hsed.Sim.Access", then the
behaviour of the method's record, "Hsed.Sim.Base"), the TPer's End of
Session echoed and the session closed (Core 3.2.2.3.3.3, PDF 37; 3.3.7.1.5,
PDF 75), and a fault aborting the session with the results completed before
it and the TPer's CloseSession where the run prepares one (3.2.2.4.2, PDF
39; 3.3.10.7 b, PDF 86; Opal 2.30 3.3.4.1.3, PDF 31).

/At the M4a scaffold/ both are stubs that group G1 writes (design 11): no
session can open before G2 answers StartSession, so no packet reaches them.
-}
module Hsed.Sim.Session
  ( dispatch
  , sessionPayload
  ) where

import           RIO

import           Hsed.Core.Call   (MethodCall, MethodResponse)
import           Hsed.Core.Stream (Fault, Item)
import           Hsed.Sim.Types   (Outcome, Session, TPerState)


-- | One invocation in a session: a host status other than 0 echoed with an
--   empty result (Core 3.2.4.2, PDF 49); no AccessControl row for the
--   InvokingID and MethodID, or an ACL unsatisfied, NOT_AUTHORIZED with an
--   empty result (5.1.5.2, PDF 137); else the behaviour of the method's
--   record run, a ParamError INVALID_PARAMETER (5.1.5.11, PDF 139).
dispatch :: Session -> MethodCall -> TPerState -> (MethodResponse, TPerState)
dispatch = error "G1: Session.dispatch"

-- | A session packet's items, as "Hsed.Core.Stream".@items@ read them with
--   the fault that stopped the reading: the outcome (the responses prepared
--   in a packet of the session, or the session aborted) and the TPer after
--   it.
sessionPayload :: Session -> [Item] -> Maybe Fault -> TPerState -> (Outcome, TPerState)
sessionPayload = error "G1: Session.sessionPayload"
