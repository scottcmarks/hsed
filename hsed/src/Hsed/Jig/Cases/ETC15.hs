{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC15
Description : ETC-15, more subpackets in a control session packet than the TPer's MaxSubpackets
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-15 (Test Cases 1.02, PDF 88): Properties for the TPer's MaxSubpackets,
n, then a control session packet of n + 1 Properties subpackets, which the
TPer discards and ignores (Core 5.2.2.4.1.1, PDF 145; 3.3.10.7 a, PDF 86:
the law @Sync.ignorePacket@'s second clause), the next IF-RECV answering
(0, 0, 0). A MaxSubpackets of 0, no limit (Core Table 167), leaves the case
not applicable, skipped (design 13.29 (b); the lead's M4a decision), as
does an n so large that the n + 1 subpackets would not fit the TPer's
MaxComPacketSize in one IF-SEND (Core 3.3.10.7 c, PDF 86: the IF-SEND
would fail for its length, not for its subpackets).
-}
module Hsed.Jig.Cases.ETC15
  ( etc15
  ) where

import qualified RIO.ByteString     as B
import qualified RIO.List           as L
import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Script    (Script, Verdict (..), baseComId, controlSubpackets, exchangeOn, exchanged, expectNoResponse,
                                     observe, properties, propertiesCall, unmet)
import           Hsed.Sim.Types     (maxComPacketSizeName, maxSubpacketsName)


-- | The case.
etc15 :: Case
etc15 = Case
  { cKey        = CaseKey (ETC 15) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedIfRecv, NeedErrorPath]
  , cRun        = run
  , cExtensions = []
  }

-- | Step 1 Properties for MaxSubpackets, step 2 a packet of one more
--   Properties subpacket than it, expecting no response.
run :: Script Verdict
run = do
  tper <- properties >>= exchanged "step 1: Properties"
  n <- maybe (unmet ("step 1: Properties reported no " <> maxSubpacketsName)) pure (L.lookup maxSubpacketsName tper)
  observe "step 1" (maxSubpacketsName <> " = " <> tshow n)
  if n <= 0
    then pure (Skipped (maxSubpacketsName <> " = " <> tshow n <> ": no limit (Core Table 167, PDF 141), which step 2 cannot exceed: not "
                        <> "applicable (NA; design 13.29 (b))"))
    else do
      base <- baseComId
      let count = fromInteger n + 1
      bytes <- either (unmet . ("step 2: the ComPacket does not encode: " <>) . tshow) pure
                      (controlSubpackets base (L.replicate count propertiesCall))
      case L.lookup maxComPacketSizeName tper of
        Just most | toInteger (B.length bytes) > most ->
          pure (Skipped ("step 2: " <> tshow count <> " Properties subpackets take " <> tshow (B.length bytes) <> " bytes, more than "
                         <> maxComPacketSizeName <> " " <> tshow most <> ": the IF-SEND would fail for its length (Core 3.3.10.7 c, PDF 86), "
                         <> "not for its subpackets: not applicable"))
        _ -> do
          let name = "step 2: " <> tshow count <> " Properties subpackets in one control session packet, " <> maxSubpacketsName <> " " <> tshow n
          exchangeOn base bytes >>= expectNoResponse name
          observe "step 2" (tshow count <> " subpackets sent; the IF-RECV answered (0, 0, 0): all responses returned, no further data "
                            <> "(Core Table 45, PDF 83)")
          pure Pass
