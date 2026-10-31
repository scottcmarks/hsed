{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Sim.SessionManager
Description : The simulated TPer's Session Manager: Properties, StartSession and CloseSession as behaviours, and its answer on the control session
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Session Manager's answers on the control session (Core 5.2), each a
'Behaviour' of its method's generated record ("Hsed.Sim.Behaviour"), bound by
a @behaviour@ comment on the method's heading (design 13.16): 'properties'
(5.2.2.1, PDF 140), 'startSession' (5.2.3.1, PDF 149, answered by
SyncSession, 5.2.3.2) and 'closeSession' (5.2.3.5, PDF 153). A Session
Manager response is formatted as a method invocation (Core 3.2.4.2;
5.2.2.1.2, PDF 140), so a reply is a 'MethodCall'; 'answer' is what the
synchronous protocol ("Hsed.Sim.Sync") calls for each invocation of a control
session packet, threading the TPer's state.

/At the M4a scaffold/ (group G2 fills the rest, design 11): Properties is
M3's, stateless, answered from the parameters; StartSession goes unanswered
as M3 left it; a host's CloseSession is ignored, as it always is (only the
TPer transmits the method, 5.2.3.5), and 'closeSessionCall' is the TPer's
own, which a session abort stages (3.3.7.1.5, PDF 75; 3.3.10.7 b, PDF 86).

* Properties, the TPer's half: the parameters' properties
  ("Hsed.Sim.Params".'SimParams': Opal 2.30 Table 17's minima by default),
  each a Named value, its name the property's as Core Table 167 prints it
  ("Hsed.Gen.Core.Properties"), its value a uinteger; a parameter Table 167
  does not give the TPer, or one no uinteger holds, is not reported.
* The host's half, as far as the answer goes: each host property Opal 2.30
  Table 17 has a TPer accept, the host's value where it sent one, raised to
  the least Table 17 allows (Core 5.2.2.3, PDF 143), its initial assumption
  where it sent none. Keeping them per ComID ("Hsed.Sim.Types".@cHostProps@)
  is G2's.

A Properties invocation the host aborts, its status other than SUCCESS, is
answered in the normal form with that status and its processing aborted, an
empty list of the TPer's properties and no HostProperties: a Session Manager
method recognized but failing SHALL have its normal response format, and a
host's abort SHALL be returned as the status (Core 3.2.4.2 item 1, PDF 49).
The status list's reserved values are ignored where it is read (item 2,
"Hsed.Sim.Sync").

Anything else is no answer, its payload discarded: a method invoked on
another than SMUID, a Session Manager method the TPer does not answer (the
host's SyncSession, CloseSession, the trusted-session pair), or a Properties
invocation whose parameters are not Properties' (a required parameter, an
optional one other than HostProperties, a HostProperties other than Named
byte-string names with uinteger values), which a TPer may ignore rather than
answer with INVALID_PARAMETER (Test Cases ETC-08, PDF 81; Core 3.2.4.2 MAY).
-}
module Hsed.Sim.SessionManager
  ( -- * The answer on the control session
    answer
    -- * The behaviours
  , properties
  , startSession
  , closeSession
  , behaviours
  , closeSessionCall
  ) where

import qualified RIO.List                 as L
import           RIO

import           Hsed.Core.Call           (MethodCall (..), MethodHeader (..), Parameters (..), optionalParameter)
import           Hsed.Core.Method         (MethodSignature (..), Param (..), SigResponse (..), Signature (..),
                                           invocation)
import           Hsed.Core.Properties     (Communicator (..), Property (..))
import           Hsed.Core.Token          (Token (..))
import           Hsed.Core.Value          (Datum (..), IsValue (..), List (..), Name (..), NamedValue (..), Value (..),
                                           (.:))
import           Hsed.Gen.Core.Codes      (StatusCode (..), pattern SUCCESS)
import qualified Hsed.Gen.Core.Methods    as Methods
import qualified Hsed.Gen.Core.Properties as P
import           Hsed.Gen.Core.UIDs       (uProperties, uSMUID)
import           Hsed.Jig.Catalogue       (CaseKey (..), TestId (..))
import           Hsed.Sim.Behaviour       (Behaviour (..), Invoked (..), Reply (..), SomeBehaviour, forMethod, runSome)
import           Hsed.Sim.Types           (Session (..), SimParams (..), TPerState (..), hostRequirements)


-- * The answer on the control session

-- | The Session Manager's answer to a method invocation on a ComID's
--   control session, and the TPer after it: an invocation of SMUID whose
--   method a behaviour runs (its parameters read against the record's
--   signature, a ParamError leaving it unanswered, 3.2.4.2 MAY), or a
--   Properties invocation the host aborted answered in its normal form with
--   the host's status (3.2.4.2 item 1); 'Nothing', the TPer unchanged, for
--   anything else.
answer :: Word16 -> MethodCall -> TPerState -> (Maybe MethodCall, TPerState)
answer c mc@(MethodCall (MethodHeader invoker method) _ status) st
  | invoker /= uSMUID                          = (Nothing, st)
  | method == uProperties, status /= success  = (Just aborted, st)
  | otherwise = case mapMaybe (\b -> runSome b invoked mc st) behaviours of
      [Right (ReplyCall reply, st')] -> (Just reply, st')
      _                              -> (Nothing, st)
  where
    invoked = Invoked { ivComId = c, ivSession = Nothing, ivInvoker = invoker, ivStatus = status }
    aborted = MethodCall (MethodHeader uSMUID uProperties) (Parameters [L (List [])] []) status

-- | The status of a method that succeeded (Core Table 166).
success :: Natural
success = statusValue SUCCESS


-- * The behaviours

-- | SMUID.Properties (Core 5.2.2.1, PDF 140; @behaviour
--   "SessionManager.properties"@): the TPer's properties, and the host's it
--   will use where the host sent its own; no answer where HostProperties is
--   not a list of Named byte-string names with uinteger values. UCT-02
--   reads it, ETC-02, ETC-03, ETC-08 and ETC-15 invoke it.
properties :: Behaviour Methods.Properties
properties = Behaviour
  { behName  = "SessionManager.properties"
  , behHash  = "4096cbe61646"
  , behCases = [whole (UCT 2), whole (ETC 2), whole (ETC 3), whole (ETC 8), whole (ETC 15)]
  , behRun   = \_ (Methods.Properties host) st ->
      (maybe NoReply (ReplyCall . respond (tsParams st)) (hostProperties host), st)
  }
  where
    respond p host = MethodCall (MethodHeader uSMUID uProperties)
                                (Parameters [L (List (tperProperties p))] (maybe [] hostResult host))
                                success

-- | SMUID.StartSession (Core 5.2.3.1, PDF 149; @behaviour
--   "SessionManager.startSession"@), answered by SyncSession (5.2.3.2, PDF
--   150). UCT-03 step 1a, ETC-04 and ETC-21/adminsp invoke it. /At the M4a
--   scaffold/ it is not answered, as M3 left it: group G2 opens the session
--   ("Hsed.Sim.State".@openSession@) or fails it with 5.1.5's statuses.
startSession :: Behaviour Methods.StartSession
startSession = Behaviour
  { behName  = "SessionManager.startSession"
  , behHash  = "24fca697f37f"
  , behCases = [CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing, whole (ETC 4), CaseKey (ETC 21) Nothing Nothing (Just "adminsp")]
  , behRun   = \_ _ st -> (NoReply, st)
  }

-- | SMUID.CloseSession (Core 5.2.3.5, PDF 153; @behaviour
--   "SessionManager.closeSession"@): a method only the TPer transmits, so a
--   host's invocation of it is ignored (3.3.7.1.3, PDF 73-74); the TPer's
--   own is 'closeSessionCall'. No Test Case invokes it.
closeSession :: Behaviour Methods.CloseSession
closeSession = Behaviour
  { behName  = "SessionManager.closeSession"
  , behHash  = "993210934add"
  , behCases = []
  , behRun   = \_ _ st -> (NoReply, st)
  }

-- | The Session Manager's behaviours, the ones its dispatch runs.
behaviours :: [SomeBehaviour]
behaviours = [forMethod properties, forMethod startSession, forMethod closeSession]

-- | The CloseSession the TPer transmits when it aborts a session (Core
--   3.3.7.1.5, PDF 75; 3.3.10.7 b, PDF 86; Opal 2.30 4.1.1.4 (O), PDF 35):
--   SMUID.CloseSession with the session's HSN as RemoteSessionNumber and
--   its TSN as LocalSessionNumber (5.2.3.5.1-2, PDF 153), status 0.
closeSessionCall :: Session -> MethodCall
closeSessionCall s = invocation uSMUID Methods.CloseSession
  { Methods.closeSessionRemoteSessionNumber = value (fromIntegral (sesHSN s) :: Natural)
  , Methods.closeSessionLocalSessionNumber  = value (fromIntegral (sesTSN s) :: Natural)
  }

-- | A test, all of it.
whole :: TestId -> CaseKey
whole t = CaseKey t Nothing Nothing Nothing


-- * Properties' parts

-- | The TPer's properties as the response carries them: each a Named value,
--   a property Core Table 167 gives the TPer, a uinteger.
tperProperties :: SimParams -> [Value]
tperProperties p =
  [ N (encodeUtf8 name .: (fromInteger v :: Natural)) | (name, v) <- spTPerProperties p, name `elem` tper, v >= 0 ]
  where tper = [ propertyName pr | pr <- P.properties, TPer `elem` appliesTo pr ]

-- | The host's properties, if the optional parameter is Properties' encoding
--   of them: 'Just' 'Nothing' when the host sent none, 'Nothing' when they
--   are no HostProperties (not a list of Named byte-string names with
--   uinteger values).
hostProperties :: Maybe Value -> Maybe (Maybe [(Text, Natural)])
hostProperties = \case
  Nothing -> Just Nothing
  Just v  -> Just <$> namedUintegers v
  where
    namedUintegers (L (List vs)) = traverse namedUinteger vs
    namedUintegers _             = Nothing
    namedUinteger (N (NamedValue (Name (Datum (Bytes name))) (D (Datum (Unsigned n))))) =
      either (const Nothing) (\t -> Just (t, n)) (decodeUtf8' name)
    namedUinteger _ = Nothing

-- | The HostProperties the TPer will use (5.2.2.1.2.2, PDF 141), as the
--   response's optional parameter: each of Table 17's host properties, the
--   host's value raised to the least allowed (5.2.2.3, PDF 143), or the
--   initial assumption where the host sent none.
hostResult :: [(Text, Natural)] -> [NamedValue]
hostResult sent =
  [ optionalParameter i (List [ N (encodeUtf8 name .: max least (fromMaybe least (L.lookup name sent)))
                              | (name, least) <- hostRequirements ])
  | Just i <- [responseHostIndex] ]

-- | Where HostProperties is among the response's optional parameters (Core
--   5.2.2.1's signature, generated), which names it (3.2.4.1).
responseHostIndex :: Maybe Natural
responseHostIndex = case sigResponse (signature @Methods.Properties) of
  Invocation _ _ optionals -> fromIntegral <$> L.findIndex ((== "HostProperties") . paramName) optionals
  _                        -> Nothing
