{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Jig.Cases.UCT03
Description : UCT-03 steps 1a to 1c, taking ownership: a session to the Admin SP, the MSID read, the session closed
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

UCT-03 (Test Cases 1.02, PDF 32-33), its step 1 as far as the jig takes it
read-only: 1a StartSession to the Admin SP with no authority, 1b Get on
C_PIN_MSID's PIN column, @[3 = 3, 4 = 3]@, which SUCCESS answers with
@[[3 = the MSID]]@, 1c the host's End of Session (CLOSE_SESSION, PDF 21),
answered with the TPer's. Steps 1d to 1f set the SID PIN and are not run,
which the case's observation and a note say; step 1 applies where Level 0's
Opal SSC V2 descriptor says the SID's PIN is the MSID's (byte 13, the
Initial C_PIN_SID PIN Indicator, 0x00; Opal 2.30 Table 6, PDF 23), else the
case is skipped, step 2 needing the vendor's SID PIN. The MSID read goes
among the run's secrets, masked in every report unless @--show-msid@
(design 9.3 item 5), and is never written into a verdict or a note. The
jig's extensions on the simulator: Get on C_PIN_SID's PIN as Anybody is
NOT_AUTHORIZED with an empty result (no Test Case covers it); a second
StartSession while the first is open is SP_BUSY, or NO_SESSIONS_AVAILABLE
where MaxSessions is 1 (ETC-22's allowance, PDF 93); and the MSID read is
the simulator's parameter.
-}
module Hsed.Jig.Cases.UCT03
  ( uct03steps1a1c
  , partialVerdict
  ) where

import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO

import           Hsed.Core.CellBlock    (columnRange)
import           Hsed.Core.Token        (Token (..))
import           Hsed.Core.Value        (Datum (..), List (..), Name (..), NamedValue (..), Value (..))
import           Hsed.Gen.Core.Codes    (pattern NOT_AUTHORIZED, pattern NO_SESSIONS_AVAILABLE, pattern SP_BUSY, pattern SUCCESS)
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.UIDs     (uC_PIN_SID)
import           Hsed.Gen.Opal.Features (Level0DiscoveryOpalSSCV2Feature (..))
import           Hsed.Jig.Case          (Case (..), Need (..))
import           Hsed.Jig.Catalogue     (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq        (Section (..), lockingSPUntouched)
import           Hsed.Jig.Script        (MethodOutcome (..), RunCtx (..), RunEnv (..), Script, SessionHandle, SyncAnswer (..),
                                         Verdict (..), adminSPAsAnybody, attempt, baseComId, closingStep, discover, endSession,
                                         exchanged, expectMethodStatus, getCells, namedColumn, note, observe, opalV2Of, opened,
                                         peekSim, properties, startSession, statusName, unmet, withSession)
import           Hsed.OpalUIDs          (uC_PIN_MSID)
import           Hsed.Sim.Types         (SimParams (..), TPerState (..), maxSessionsName)
import           Hsed.Wire              (hex)


-- | The case: steps 1a to 1c of step 1.
uct03steps1a1c :: Case
uct03steps1a1c = Case
  { cKey        = CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing
  , cSection    = UseCases
  , cWaives     = [lockingSPUntouched]
  , cNeeds      = [NeedIfRecv, NeedIfSend]
  , cRun        = run
  , cExtensions =
      [ ("Get C_PIN_SID's PIN as Anybody: NOT_AUTHORIZED, an empty result (Core 5.1.5.2, PDF 137)", sidPinRefused)
      , ("a second StartSession while the first is open: SP_BUSY, or NO_SESSIONS_AVAILABLE at MaxSessions 1 (Core 5.1.5.3, 5.1.5.7, PDF 138)", secondSession)
      , ("the MSID read is the simulator's", msidIsParameter)
      ]
  }

-- | What the verdict covers, said in the case's observation and the
--   transcript: step 1 as far as 1c; 1d to 1f set the SID PIN.
partialVerdict :: Text
partialVerdict = "1a-1c of step 1; 1d-1f set the SID PIN and are not run"

-- | Whether step 1 applies (the Initial C_PIN_SID PIN Indicator 0x00), then
--   its steps 1a to 1c.
run :: Script Verdict
run = do
  _ <- baseComId
  l0 <- maybe discover pure . rcLevel0 =<< (asks reCtx >>= readIORef)
  case opalV2Of l0 of
    Nothing -> unmet "no Opal SSC V2 descriptor in Level 0 Discovery: the Initial C_PIN_SID PIN Indicator cannot be read (Opal 2.30 Table 6, PDF 23)"
    Just v2
      | indicator /= 0 ->
          pure (Skipped ("the Initial C_PIN_SID PIN Indicator is " <> hex indicator <> ", not 0x00: step 1 does not apply, and step 2 needs the "
                         <> "vendor's SID PIN (Test Cases PDF 33)"))
      | otherwise -> step1
      where indicator = level0DiscoveryOpalSSCV2FeatureInitialC_PIN_SIDPINIndicator v2

-- | 1a StartSession, 1b the MSID read (a failure of it still closes the
--   session), 1c End of Session.
step1 :: Script Verdict
step1 = do
  h <- startSession adminSPAsAnybody >>= exchanged "1a: StartSession to the Admin SP" >>= opened "1a: StartSession to the Admin SP"
  v <- attempt (Pass <$ step1b h)
  observe "step 1" partialVerdict
  note ("UCT-03: " <> partialVerdict)
  closingStep "1c: End of Session" h v

-- | 1b: Get on C_PIN_MSID of its PIN column alone, SUCCESS and the PIN's
--   bytes, which go among the run's secrets.
step1b :: SessionHandle -> Script ()
step1b h = do
  msid <- msidRead "1b: Get C_PIN_MSID's PIN" h
  observe "1b" ("the MSID read, " <> tshow (B.length msid) <> " bytes (a secret: masked in the reports unless --show-msid)")

-- | Get on C_PIN_MSID of its PIN column in a session, the step named: the
--   PIN's bytes, kept among the run's secrets.
msidRead :: Text -> SessionHandle -> Script ByteString
msidRead name h = do
  pin <- pinColumn
  outcome <- getCells h uC_PIN_MSID (columnRange pin pin) >>= exchanged name
  expectMethodStatus name SUCCESS outcome
  msid <- either (unmet . ((name <> ": ") <>)) pure (pinRead pin (moResults outcome))
  asks reCtx >>= \ref -> modifyIORef' ref (\c -> c { rcSecrets = L.nub (rcSecrets c <> [msid]) })
  pure msid

-- | The PIN column's number, found by name in the generated C_PIN
--   description.
pinColumn :: Script Natural
pinColumn = namedColumn c_PINTableDesc "PIN"

-- | The PIN a Get's result holds: @[[p = bytes]]@, one list of the row's
--   values, the PIN column's a byte string of some bytes (Core
--   5.3.3.6.2.2, PDF 201; Test Cases UCT-03 1b, PDF 32); or why not, never
--   quoting the values.
pinRead :: Natural -> [Value] -> Either Text ByteString
pinRead pin = \case
  [L (List cells)] -> case [ bytes | N (NamedValue (Name (Datum (Unsigned col))) (D (Datum (Bytes bytes)))) <- cells, col == pin ] of
    bytes : _
      | not (B.null bytes) -> Right bytes
      | otherwise          -> Left ("the PIN column " <> tshow pin <> " holds no bytes")
    [] -> Left ("no PIN (column " <> tshow pin <> ", a byte string) among the result's " <> tshow (length cells) <> " cells")
  results -> Left ("the result is not one list of the row's values: " <> tshow (length results) <> " results")

-- | The extension: Get on C_PIN_SID's PIN column as Anybody, which
--   ACE_C_PIN_SID_Get_NOPIN does not grant (Opal 2.30 Table 23, PDF 47):
--   NOT_AUTHORIZED with an empty result (Core 5.1.5.2, PDF 137; 3.2.4.2,
--   PDF 49).
sidPinRefused :: Script Verdict
sidPinRefused = withSession adminSPAsAnybody $ \h -> do
  pin <- pinColumn
  outcome <- getCells h uC_PIN_SID (columnRange pin pin) >>= exchanged "Get C_PIN_SID's PIN as Anybody"
  expectMethodStatus "Get C_PIN_SID's PIN as Anybody" NOT_AUTHORIZED outcome
  unless (null (moResults outcome)) $
    unmet ("Get C_PIN_SID's PIN as Anybody: " <> tshow (length (moResults outcome)) <> " result(s) where an empty result is expected (Core 3.2.4.2, PDF 49)")
  pure Pass

-- | The extension: a second StartSession to the Admin SP while the first
--   session is open: SP_BUSY (Core 5.1.5.3, PDF 138), or
--   NO_SESSIONS_AVAILABLE where the TPer's MaxSessions is 1 (5.1.5.7;
--   ETC-22's allowance, Test Cases PDF 93); a second session opened is
--   closed and fails the extension.
secondSession :: Script Verdict
secondSession = withSession adminSPAsAnybody $ \_ -> do
  answer <- startSession adminSPAsAnybody >>= exchanged "a second StartSession while the first is open"
  tper <- maybe (properties >>= exchanged "Properties, for MaxSessions") pure . rcTPerProperties =<< (asks reCtx >>= readIORef)
  let maxSessions = L.lookup maxSessionsName tper
  case answer of
    SyncFailed st
      | st == SP_BUSY -> Pass <$ observe "a second StartSession" (statusName st)
      | st == NO_SESSIONS_AVAILABLE, maxSessions == Just 1 ->
          Pass <$ observe "a second StartSession" (statusName st <> " at MaxSessions 1 (ETC-22's allowance, Test Cases PDF 93)")
      | otherwise ->
          unmet ("a second StartSession: SyncSession answered " <> statusName st <> ", not " <> statusName SP_BUSY
                 <> maybe "" (\m -> " (MaxSessions " <> tshow m <> ")") maxSessions)
    Synced another -> do
      _ <- endSession another
      unmet "a second StartSession: a second session opened to the Admin SP while the first is open (Core 5.1.5.3, PDF 138)"
    NoSync why -> unmet ("a second StartSession: " <> why)

-- | The extension: the MSID read is the simulator's parameter, compared
--   and never printed.
msidIsParameter :: Script Verdict
msidIsParameter = peekSim >>= \case
  Nothing -> pure (Skipped "the simulator's alone")
  Just st -> withSession adminSPAsAnybody $ \h -> do
    msid <- msidRead "Get C_PIN_MSID's PIN" h
    unless (msid == spMsid (tsParams st)) (unmet "the MSID read differs from the simulator's spMsid")
    pure Pass
