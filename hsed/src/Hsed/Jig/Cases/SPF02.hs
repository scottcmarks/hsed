{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.SPF02
Description : SPF-02 case 1, IF-RECV with nothing to return
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SPF-02 case 1 (Test Cases 1.02, PDF 52): IF-RECV on the base ComID
in Awaiting IF-SEND: all responses returned, no further data (Core Table 45
row 6, PDF 83). The jig's extension on the simulator: an IF-SEND of one
data subpacket of Empty atoms leaves the ComID in S1 and the next IF-RECV
answers the same (Core 3.3.10.6 S2:S1 d.c, PDF 86), reported apart from the
TCG verdict, Empty atoms being out of the Test Cases' bounds (4.1, PDF 26).
Case 2 is M5's.

The case's own prerequisite, In Awaiting IF-SEND, is the common SyncIdle
the runner establishes. The extension's subpacket is four Empty atoms (the
codec's @Empty@ token, Core Table 13) in a ComPacket of the Test Cases'
form; the simulator decides at the IF-SEND to discard it and keeps the
ComID in S1 (design 8.2), which the extension reads from the simulator's
state before the IF-RECV.
-}
module Hsed.Jig.Cases.SPF02
  ( spf02case1
  ) where

import qualified RIO.List              as L
import           RIO

import           Hsed.Core.StreamItem  (StreamItem (..))
import           Hsed.Core.Token       (Token (..))
import           Hsed.Jig.Case         (Case (..), Need (..))
import           Hsed.Jig.Catalogue    (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq       (Section (..), lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Script       (RunEnv (..), Script, Verdict (..), baseComId, controlComPacket, expectStatus,
                                        expectT45, ifRecvOn, rawIfSend, sendLength, simOnly, unmet)
import           Hsed.Jig.Target       (Target (..))
import           Hsed.Sim              (syncOf)
import           Hsed.Sim.Sync         (T45Row (..))
import           Hsed.Sim.Types        (SyncState (..))
import           Hsed.Transport.InProc (SimControl (..))
import           Hsed.Wire             (IfStatus (..))


-- | The case.
spf02case1 :: Case
spf02case1 = Case
  { cKey        = CaseKey (SPF 2) (Just 1) Nothing Nothing
  , cSection    = SpecificFunctionality
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedIfRecv]
  , cRun        = run
  , cExtensions = [("Empty atoms: an IF-SEND of them leaves the ComID in S1 (Core 3.3.10.6 S2:S1 d.c, PDF 86)", emptyAtoms)]
  }

-- | Step 1: an IF-RECV on the base ComID succeeds, its ComPacket header all
--   responses returned, no further data.
run :: Script Verdict
run = do
  base <- baseComId
  r <- ifRecvOn base
  expectStatus "step 1: the IF-RECV" Good r
  expectT45 "step 1: the IF-RECV's ComPacket header" base AllReturnedNoData r
  pure Pass

-- | The extension: the IF-SEND succeeds, the ComID stays in S1, and the next
--   IF-RECV has nothing to return.
emptyAtoms :: Script Verdict
emptyAtoms = fmap (fromMaybe (Skipped "the simulator's alone")) . simOnly $ do
  base <- baseComId
  bytes <- either (unmet . ("the ComPacket does not encode: " <>) . tshow) pure
                  (controlComPacket base (foldMap generate (L.replicate 4 Empty)))
  len <- sendLength bytes
  rawIfSend base len bytes >>= expectStatus "the IF-SEND of four Empty atoms" Good
  after <- asks reTarget >>= \t -> case t of
    SimTarget _ control -> liftIO (syncOf base <$> scPeek control)
    _                   -> pure Nothing
  unless (after == Just S1) (unmet ("after the IF-SEND the ComID is in " <> maybe "no state" tshow after <> ", not S1"))
  ifRecvOn base >>= expectT45 "the next IF-RECV" base AllReturnedNoData
  pure Pass
