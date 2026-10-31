{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Jig.Cases.ETC08
Description : ETC-08, an unexpected token in a control session method's parameter list
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-08 (Test Cases 1.02, PDF 81): Properties with a Start List where its
parameter list's values go (the bytes @F8 A8 00 00 00 00 00 00 00 FF A8 00
00 00 00 00 00 FF 01 F0 F0 F1 F1 F9 F0 00 00 00 F1@: Call, SMUID,
Properties, a parameter list holding one empty list, End of Data, the status
list, 'etc08Payload'), which the TPer may ignore, the next IF-RECV answering
(0, 0, 0) (1a), or answer INVALID_PARAMETER (1b); SUCCESS fails the case.
The simulator ignores it (Core 3.2.4.2 MAY; Opal 2.30 3.3.4.1.3, PDF 31;
design 13.29 (h)).
-}
module Hsed.Jig.Cases.ETC08
  ( etc08
  , etc08Payload
  ) where

import           RIO

import           Hsed.Core.Call         (MethodCall (..), MethodHeader (..), statusList)
import           Hsed.Core.Stream       (Returned (..))
import           Hsed.Core.StreamItem   (StreamItem (..))
import           Hsed.Core.Token        (Token (..))
import           Hsed.Gen.Core.Codes    (StatusCode (..), pattern INVALID_PARAMETER, pattern SUCCESS)
import           Hsed.Gen.Core.UIDs     (uProperties, uSMUID)
import           Hsed.Jig.Case          (Case (..), Need (..))
import           Hsed.Jig.Catalogue     (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq        (Section (..), lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Script        (Script, Verdict (..), baseComId, controlComPacket, exchangeOn, exchanged, observe,
                                         returnedOf, statusName, unmet)


-- | The case.
etc08 :: Case
etc08 = Case
  { cKey        = CaseKey (ETC 8) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedIfRecv, NeedErrorPath]
  , cRun        = run
  , cExtensions = []
  }

-- | Step 1's tokens (Test Cases PDF 81): Call, the method header of
--   SMUID.Properties (the generated UIDs), a parameter list of one Start
--   List and End List where Properties takes no required parameter (Core
--   5.2.2.1, PDF 140), End of Data and the status list of 0: the bytes
--   @F8 A8 00 00 00 00 00 00 00 FF A8 00 00 00 00 00 00 FF 01 F0 F0 F1 F1 F9
--   F0 00 00 00 F1@.
etc08Payload :: ByteString
etc08Payload = mconcat
  [ generate Call, generate (MethodHeader uSMUID uProperties), generate StartList, generate StartList, generate EndList, generate EndList
  , generate EndOfData, generate (statusList 0) ]

-- | Step 1: the exchange, then one of the expected responses: no response
--   (1a), or Properties answered INVALID_PARAMETER (1b); anything else
--   fails.
run :: Script Verdict
run = do
  base <- baseComId
  bytes <- either (unmet . ("the ComPacket does not encode: " <>) . tshow) pure (controlComPacket base etc08Payload)
  exchangeOn base bytes >>= exchanged "step 1: Properties with a Start List in its parameter list" >>= \case
    []      -> Pass <$ observe "step 1" "1a: the IF-RECV answered (0, 0, 0), all responses returned, no further data (Core Table 45, PDF 83)"
    packets -> do
      items <- either (unmet . ("step 1: " <>)) pure (returnedOf packets)
      case [ StatusCode (_statusCode mc) | ReturnedCall mc@(MethodCall (MethodHeader i m) _ _) <- items, i == uSMUID, m == uProperties ] of
        st : _
          | st == INVALID_PARAMETER -> Pass <$ observe "step 1" ("1b: Properties answered " <> statusName st)
          | st == SUCCESS -> pure (Fail ["step 1: Properties answered " <> statusName st <> " where no response (1a) or INVALID_PARAMETER (1b) is expected"])
          | otherwise     -> pure (Fail ["step 1: Properties answered " <> statusName st <> ", neither no response (1a) nor INVALID_PARAMETER (1b)"])
        [] -> pure (Fail ["step 1: the TPer answered " <> tshow (length packets) <> " packet(s) holding no Properties response"])
