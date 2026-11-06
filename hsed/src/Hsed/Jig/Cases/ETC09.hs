{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC09
Description : ETC-09, exceeding the transaction limit: MaxTransactionLimit + 1 Start Transaction tokens in one subpacket
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-09 (Test Cases 1.02, PDF 81-82): Properties for MaxTransactionLimit,
StartSession to the Locking SP as Admin1, a subpacket of MaxTransactionLimit
+ 1 Start Transaction tokens, each with status 0x00, which aborts the
session (Core 3.2.2.3.3.4, PDF 37: a Start Transaction past the nesting
limit), the IF-RECV answering "All Response(s) returned - no further data"
or a ComPacket with a CloseSession (PDF 82; Test Cases 7.1, PDF 76), the
session then forgotten by the run; then a new session to the Locking SP as
Admin1, and CLOSE_SESSION. A TPer that reports no MaxTransactionLimit
supports no transactions (Core 3.3.7.3.1, PDF 78), and one that reports 0
has no limit (Table 167, PDF 142): step 3 exceeds nothing on either, and
the case is not applicable, skipped (design 13.29 (b)). Its sessions need
the Locking SP Manufactured, the common prerequisite of 7.2 (PDF 76), and
the over-limit subpacket is an IF-SEND off a drive's allow list, so the
simulator alone runs it (design 9.3, 11 M5b), on its one level of
transactions (since M5b).

The case passes on a TPer that aborts the session at any transaction
token; the jig's extension tells the two apart: one Start Transaction
within the limit is mirrored with status 0 and its End Transaction with
status 1 mirrored 1 (Core 3.2.2.3.3.4-5, PDF 37-38), so that it was the
limit, not the token, that aborted step 3.
-}
module Hsed.Jig.Cases.ETC09
  ( etc09
  ) where

import qualified RIO.List           as L
import           RIO

import           Hsed.Core.StreamItem (StreamItem (..))
import           Hsed.Core.Token    (Token (..))
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..))
import           Hsed.Jig.Script    (Script, SessionHandle (..), StartArgs, Verdict (..), asAuthority, attempt, endTransaction,
                                     exchangeOn, exchanged, expectNoResponseOrCloseSession, note, observe, opened, properties,
                                     sessionComPacket, sessionGone, stackReset, startSession, startTransaction, unmet, withSession)
import           Hsed.Sim.Types     (maxTransactionLimitName)
import           Hsed.Wire          (hex)


-- | The case.
etc09 :: Case
etc09 = Case
  { cKey        = CaseKey (ETC 9) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedErrorPath, NeedSimOnly]
  , cRun        = run
  , cExtensions = [("one Start Transaction within the limit is mirrored 0 and its End Transaction with status 1 mirrored 1 (Core 3.2.2.3.3.4-5, PDF 37-38): the limit, not the token, aborted step 3", withinLimit)]
  }

-- | Step 1 Properties for MaxTransactionLimit, n (the case not applicable
--   where it is not reported or 0); step 2 StartSession; step 3 the n + 1
--   Start Transaction tokens, after which the session is gone; steps 4 and
--   5 a session opened and closed ('withSession'). A step 3 that failed
--   may leave its session open: STACK_RESET clears it ('cleared').
run :: Script Verdict
run = do
  tper <- properties >>= exchanged "step 1: Properties"
  case L.lookup maxTransactionLimitName tper of
    Nothing -> do
      observe "step 1" (maxTransactionLimitName <> " not reported")
      pure (Skipped (maxTransactionLimitName <> " not reported: the TPer supports no transactions (Core 3.3.7.3.1, PDF 78), so step 3 exceeds "
                     <> "no limit: not applicable (NA; design 13.29 (b))"))
    Just n -> do
      observe "step 1" (maxTransactionLimitName <> " = " <> tshow n)
      if n <= 0
        then pure (Skipped (maxTransactionLimitName <> " = " <> tshow n <> ": no limit (Core Table 167, PDF 142), which step 3 cannot exceed: not "
                            <> "applicable (NA; design 13.29 (b))"))
        else do
          a <- admin1
          h <- startSession a >>= exchanged step2 >>= opened step2
          v <- attempt (Pass <$ step3 h n (fromInteger n + 1))
          case v of
            Pass  -> withSession a (\_ -> Pass <$ observe "steps 4-5" "StartSession to the Locking SP as Admin1 after the abort opened a session, closed by CLOSE_SESSION (Test Cases PDF 82)")
            other -> other <$ cleared h
  where step2 = "step 2: StartSession to the Locking SP as Admin1"

-- | StartSession's arguments for Admin1 of the Locking SP (Test Cases PDF
--   81), its PIN read from the simulator's state.
admin1 :: Script StartArgs
admin1 = asAuthority "Admin1" uSP_Locking LockingSP.uAuthority_Admin1

-- | Step 3: one subpacket of the session holding the count of Start
--   Transaction tokens, each followed by its status 0x00 (Core 3.2.2.3.3.4,
--   PDF 37), the whole exchange answering nothing or the TPer's
--   CloseSession ('expectNoResponseOrCloseSession'), the session aborted
--   by the token past the limit and forgotten ('sessionGone'), observed.
step3 :: SessionHandle -> Integer -> Int -> Script ()
step3 h n count = do
  bytes <- either (unmet . ((name <> ": the ComPacket does not encode: ") <>) . tshow) pure
                  (sessionComPacket h (foldMap generate (concat (L.replicate count [StartTransaction, Unsigned 0]))))
  exchangeOn (shComId h) bytes >>= expectNoResponseOrCloseSession name
  sessionGone h ("the session was aborted by Start Transaction number " <> tshow count <> ", past " <> maxTransactionLimitName <> " " <> tshow n
                 <> " (Core 3.2.2.3.3.4, PDF 37)")
  observe "step 3" (tshow count <> " Start Transaction tokens, each with status 0x00, sent in one subpacket; the session was aborted: the IF-RECV "
                    <> "answered (0, 0, 0) or the TPer's CloseSession (Test Cases PDF 82; 7.1, PDF 76)")
  where name = "step 3: " <> tshow count <> " Start Transaction tokens in one subpacket, " <> maxTransactionLimitName <> " " <> tshow n

-- | After a step that failed with its session perhaps still open:
--   STACK_RESET on the session's ComID, which aborts every session on it
--   (Core 3.3.4.7.5 item 1, PDF 65), the session forgotten where the reset
--   answered Success/Failure 0, so that the next case finds none of this
--   run's open; otherwise noted, for the next case's NoSessions.
cleared :: SessionHandle -> Script ()
cleared h = stackReset (shComId h) >>= \case
  Right 0 -> sessionGone h ("STACK_RESET on " <> hex (shComId h) <> " after a step failed: every session on the ComID aborted (Core 3.3.4.7.5 item 1, PDF 65)")
  outcome -> note ("after a step failed, STACK_RESET on " <> hex (shComId h) <> ": " <> either id (("Success/Failure " <>) . tshow) outcome)

-- | The extension: in a session as Admin1, one Start Transaction with
--   status 0x00, mirrored with status 0 (Core 3.2.2.3.3.4, PDF 37: within
--   MaxTransactionLimit, 1 at least, Opal 2.30 Table 17, PDF 34), and its
--   End Transaction with status 0x01, mirrored 1 (3.2.2.3.3.5, PDF 37-38:
--   the transaction aborted), so that step 3's abort was the limit's.
withinLimit :: Script Verdict
withinLimit = admin1 >>= \a -> withSession a $ \h -> do
  st <- startTransaction h >>= exchanged start
  unless (st == 0) (unmet (start <> ": the TPer mirrored it with status " <> tshow st <> ", not 0 (Core 3.2.2.3.3.4, PDF 37)"))
  et <- endTransaction h 1 >>= exchanged end
  unless (et == 1) (unmet (end <> ": the TPer mirrored it with status " <> tshow et <> ", not 1 (Core 3.2.2.3.3.5, PDF 37-38)"))
  observe "within the limit" ("one Start Transaction was mirrored 0 and its End Transaction with status 1 mirrored 1 (Core 3.2.2.3.3.4-5, PDF 37-38): "
                              <> "the limit, not the token, aborted step 3")
  pure Pass
  where
    start = "Start Transaction with status 0x00"
    end   = "End Transaction with status 0x01"
