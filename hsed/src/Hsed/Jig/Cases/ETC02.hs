{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC02
Description : ETC-02, a second IF-SEND before the first's response
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-02 (Test Cases 1.02, PDF 77): Properties sent in an IF-SEND
and its response not retrieved, then Properties sent again on that ComID:
the second IF-SEND's status Synchronous Protocol Violation (Core
3.3.10.2.1 3 a, PDF 82). Its IF-SENDs are raw, past the run's channel. The
jig's extension: with the simulator holding its response, the first IF-RECV
of the drain answers responses to come (Core Table 45 row 1, PDF 83).

The case drains the ComID after its two steps, so that the next case, and
the extension, find the synchronous protocol idle: a step of the jig's own,
outside the case's verdict, which is the two steps' alone (Test Cases PDF
77); a drain that does not end is said in a note of the transcript, and the
next case's SyncIdle prerequisite drains again. Its extension runs the same
two IF-SENDs with the simulator holding the first's response in S2
(@--hold-processing@, Core 3.3.10.5 S2 b, PDF 85; the violating IF-SEND
leaves the state as it was, 3.3.10.2.1 3 a), then drains; without a hold
the simulator releases the response at once, and the extension is skipped.
-}
module Hsed.Jig.Cases.ETC02
  ( etc02
  ) where

import           RIO

import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Options   (RunOptions (..))
import           Hsed.Jig.Prereq    (Section (..), lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Script    (RunEnv (..), Script, Verdict (..), attempt, baseComId, controlComPacket, drain,
                                     expectStatus, expectT45, ifRecvOn, note, propertiesCall, rawIfSend, sendLength, unmet,
                                     verdictText)
import           Hsed.Sim.Sync      (T45Row (..))
import           Hsed.Wire          (IfStatus (..), hex)


-- | The case.
etc02 :: Case
etc02 = Case
  { cKey        = CaseKey (ETC 2) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cNeeds      = [NeedIfRecv, NeedErrorPath]
  , cRun        = run
  , cExtensions = [("held: the first IF-RECV answers responses to come (Core Table 45 row 1, PDF 83)", held)]
  }

-- | Steps 1 and 2, the verdict; then the drain, outside it.
run :: Script Verdict
run = do
  base <- twoIfSends
  Pass <$ cleanUp base

-- | The drain after the steps, the jig's own: a failure said in a note, not
--   the case's.
cleanUp :: Word16 -> Script ()
cleanUp base = attempt (Pass <$ drain base) >>= \v -> case v of
  Pass -> pure ()
  _    -> note ("ETC-02's drain of " <> hex base <> " after its steps, outside its verdict: " <> verdictText v)

-- | Properties in an IF-SEND on the base ComID (step 1: it succeeds), and
--   again on that ComID before any IF-RECV (step 2: Synchronous Protocol
--   Violation); the ComID.
twoIfSends :: Script Word16
twoIfSends = do
  base <- baseComId
  bytes <- either (unmet . ("the Properties ComPacket does not encode: " <>) . tshow) pure (controlComPacket base propertiesCall)
  len <- sendLength bytes
  rawIfSend base len bytes >>= expectStatus "step 1: the IF-SEND of Properties" Good
  rawIfSend base len bytes >>= expectStatus "step 2: a second IF-SEND of Properties, its response not retrieved" SynchronousProtocolViolation
  pure base

-- | The extension: with the simulator holding the response, the two
--   IF-SENDs, then the first IF-RECV answers (0, 1, 0); then the drain.
held :: Script Verdict
held = do
  hold <- asks (roHold . reOpts)
  if hold < 1
    then pure (Skipped "needs --hold-processing: without it the simulator releases the response before any IF-RECV")
    else do
      base <- twoIfSends
      ifRecvOn base >>= expectT45 "the first IF-RECV after them" base ResponsesToCome
      Pass <$ drain base
