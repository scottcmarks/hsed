{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC03
Description : ETC-03, an IF-SEND longer than MaxComPacketSize
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-03 (Test Cases 1.02, PDF 78): Properties for the TPer's
MaxComPacketSize, then a Properties ComPacket of a correct Length in an
IF-SEND whose transfer length exceeds it: Invalid Transfer Length parameter
on IF-SEND (Core 3.3.10.7 c, PDF 86).

Step 1 is the host's whole exchange ("Hsed.Wire"'s loop, which polls past a
response the simulator holds). Step 2's transfer length is the least the
target's unit allows above MaxComPacketSize: one byte more, or the next
512-byte block; its payload the same ComPacket, zero-padded.
-}
module Hsed.Jig.Cases.ETC03
  ( etc03
  ) where

import qualified RIO.List           as L
import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Script    (Script, Verdict (..), baseComId, controlComPacket, describeExchange, expectStatus,
                                     maxComPacketSizeName, properties, propertiesCall, rawIfSend, transferFor, unmet)
import           Hsed.Wire          (IfStatus (..))


-- | The case.
etc03 :: Case
etc03 = Case
  { cKey        = CaseKey (ETC 3) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedIfRecv, NeedIfSend]
  , cRun        = run
  , cExtensions = []
  }

-- | Steps 1 and 2.
run :: Script Verdict
run = do
  most <- properties >>= \case
    Left e      -> unmet ("step 1: Properties: " <> describeExchange e)
    Right props -> maybe (unmet ("step 1: Properties reported no " <> maxComPacketSizeName)) pure (L.lookup maxComPacketSizeName props)
  base <- baseComId
  bytes <- either (unmet . ("the Properties ComPacket does not encode: " <>) . tshow) pure (controlComPacket base propertiesCall)
  beyond <- transferFor (fromInteger most + 1)
  r <- rawIfSend base beyond bytes
  expectStatus ("step 2: an IF-SEND of " <> tshow beyond <> " bytes, above MaxComPacketSize " <> tshow most) InvalidTransferLengthOnIfSend r
  pure Pass
