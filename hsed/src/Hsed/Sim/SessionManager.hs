{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Sim.SessionManager
Description : The simulated TPer's Session Manager: SMUID.Properties at M3
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Session Manager's answers on the control session (Core 5.2): at M3
SMUID.Properties (Core 5.2.2.1, PDF 140), which the jig's ETC-02 and ETC-03
invoke (Test Cases PDF 77-78). A Session Manager response is formatted as a
method invocation (Core 3.2.4.2; 5.2.2.1.2, PDF 140), so the answer is a
'MethodCall': SMUID.Properties with the TPer's properties, its required
parameter, and, when the host sent its own, the HostProperties the TPer will
use, its optional one (the response's signature, "Hsed.Gen.Core.Methods").

* The TPer's half: the parameters' properties ("Hsed.Sim.Types".'SimParams':
  Opal 2.30 Table 17's minima by default), each a Named value, its name the
  property's as Core Table 167 prints it ("Hsed.Gen.Core.Properties"), its
  value a uinteger; a parameter Table 167 does not give the TPer, or one no
  uinteger holds, is not reported.
* The host's half, as far as the answer goes: each host property Opal 2.30
  Table 17 has a TPer accept, the host's value where it sent one, raised to
  the least Table 17 allows (Core 5.2.2.3, PDF 143), its initial assumption
  where it sent none. Keeping them per ComID for the sessions that follow is
  M4a's, with sessions.

A Properties invocation the host aborts, its status other than SUCCESS, is
answered in the normal form with that status and its processing aborted, an
empty list of the TPer's properties and no HostProperties: a Session Manager
method recognized but failing SHALL have its normal response format, and a
host's abort SHALL be returned as the status (Core 3.2.4.2 item 1, PDF 49).
The status list's reserved values are ignored where it is read (item 2,
"Hsed.Sim.Sync").

Anything else is no answer, its payload discarded: another Session Manager
method (M4a's), or a Properties invocation whose parameters are not
Properties' (a required parameter, an optional one other than
HostProperties, a HostProperties other than Named byte-string names with
uinteger values), which a TPer may ignore rather than answer with
INVALID_PARAMETER (Test Cases ETC-08, PDF 81).
-}
module Hsed.Sim.SessionManager
  ( answer
  ) where

import qualified RIO.List                 as L
import           RIO

import           Hsed.Core.Call           (MethodCall (..), MethodHeader (..), Parameters (..), optionalParameter)
import           Hsed.Core.Method         (MethodSignature (..), Param (..), SigResponse (..), Signature (..),
                                           parameters)
import           Hsed.Core.Properties     (Communicator (..), Property (..))
import           Hsed.Core.Token          (Token (..))
import           Hsed.Core.Value          (Datum (..), List (..), Name (..), NamedValue (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes      (StatusCode (..), pattern SUCCESS)
import qualified Hsed.Gen.Core.Methods    as Methods
import           Hsed.Gen.Core.Properties (properties)
import           Hsed.Gen.Core.UIDs       (uProperties, uSMUID)
import           Hsed.Sim.Types           (SimParams (..), hostRequirements)


-- | The Session Manager's answer to a method call on the control session,
--   if it has one: at M3, SMUID.Properties alone, or, aborted by the host,
--   its normal form carrying the host's status.
answer :: SimParams -> MethodCall -> Maybe MethodCall
answer p (MethodCall (MethodHeader invoker method) (Parameters required named) status)
  | invoker == uSMUID, method == uProperties, status /= success = Just aborted
  | invoker == uSMUID, method == uProperties, null required    = respond <$> hostProperties named
  | otherwise                                                 = Nothing
  where
    respond host = MethodCall (MethodHeader uSMUID uProperties)
                              (Parameters [L (List (tperProperties p))] (maybe [] hostResult host))
                              success
    aborted = MethodCall (MethodHeader uSMUID uProperties) (Parameters [L (List [])] []) status

-- | The status of a method that succeeded (Core Table 166).
success :: Natural
success = statusValue SUCCESS

-- | The TPer's properties as the response carries them: each a Named value,
--   a property Core Table 167 gives the TPer, a uinteger.
tperProperties :: SimParams -> [Value]
tperProperties p =
  [ N (encodeUtf8 name .: (fromInteger v :: Natural)) | (name, v) <- spTPerProperties p, name `elem` tper, v >= 0 ]
  where tper = [ propertyName pr | pr <- properties, TPer `elem` appliesTo pr ]

-- | The host's properties, if the optional parameters are Properties'
--   encoding of them ("Hsed.Core.Method".'parameters'): 'Just' 'Nothing'
--   when it sent none, 'Nothing' when they are no HostProperties.
hostProperties :: [NamedValue] -> Maybe (Maybe [(Text, Natural)])
hostProperties = \case
  []                     -> Just Nothing
  named@[NamedValue _ v]
    | parameters (Methods.Properties (Just v)) == Parameters [] named -> Just <$> namedUintegers v
  _                      -> Nothing
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
