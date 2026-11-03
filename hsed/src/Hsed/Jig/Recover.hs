{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Recover
Description : --recover: STACK_RESET on a drive's base ComID, and the checks that it took
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsed-jig --recover@ (design 9.3, 11): run alone on its target's one end
("Hsed.Jig.Run" runs it in place of the cases, under the Guard's
'Hsed.Jig.Guard.recoverMethods' on a drive; on the simulator, as every
script there, with no Guard), it clears what a run that was stopped left on
the base ComID, and shows that it did:

1. the base ComID, from Level 0 Discovery's Opal SSC V2 descriptor, and on
   a drive bound ("Hsed.Jig.Script".'baseComId': the drive's own Level 0,
   a ComID Core Table 26 does not reserve);
2. STACK_RESET on it ("Hsed.Jig.Script".'stackReset': HANDLE_COMID_REQUEST
   of Core Table 35, then GET_COMID_RESPONSE, a Pending answer asked again,
   Core 3.3.4.7.5, PDF 65-66), which aborts every session on the ComID and
   returns its stack to its initial state; its Success/Failure must be 0
   (Table 36, PDF 66);
3. an IF-RECV that finds the ComID idle: Table 45's "All Response(s)
   returned – no further data", (0, 0, 0) (Core 3.3.10.5 S1 b, PDF 84; a
   ComPacket that names a ComID other than the one asked, or another
   extension, is noted and taken, the state being what the probe is for);
4. StartSession to the Admin SP as Anybody, read-write, and its End of
   Session: a session the reset left on the Admin SP would answer it
   SP_BUSY, one read-write session to an SP at a time (Core 5.1.5.3, PDF
   138; 3.3.7, PDF 72).

Its outcome is in words, noted in the transcript as well: what it did, or
why it did not recover (the step, and what the TPer answered).
-}
module Hsed.Jig.Recover
  ( recoverScript
  ) where

import           RIO
import qualified RIO.Text        as T

import           Hsed.Jig.Script (Script, SessionHandle (..), SyncAnswer (..), adminSPAsAnybody, baseComId, describeExchange,
                                  describeStatus, endSession, ifRecvOn, note, stackReset, startSession, statusName, t45Of,
                                  t45On)
import           Hsed.Wire       (IfStatus (..), Response (..), hex)


-- | @--recover@ on an end: what it did, or why it did not recover. A step
--   that cannot be taken at all (no base ComID in Level 0, a bind refused)
--   stops the script, as any verb does.
recoverScript :: Script (Either Text Text)
recoverScript = do
  base <- baseComId
  note ("--recover: STACK_RESET on the base ComID " <> hex base <> " (Core 3.3.4.7.5, PDF 65; design 9.3)")
  outcome <- stackReset base >>= \case
    Left why -> pure (Left ("STACK_RESET on " <> hex base <> ": " <> why))
    Right n
      | n /= 0    -> pure (Left ("STACK_RESET on " <> hex base <> ": Success/Failure " <> tshow n <> ", the stack not reset (Core Table 36, PDF 66)"))
      | otherwise -> idle base >>= either (pure . Left) (const (probe base))
  outcome <$ note ("--recover: " <> either ("not recovered: " <>) id outcome)

-- | The ComID idle after the reset: an IF-RECV answered (0, 0, 0), Table
--   45's "All Response(s) returned – no further data" (Core 3.3.10.5 S1 b,
--   PDF 84), a ComPacket naming another ComID or extension noted; or why
--   not.
idle :: Word16 -> Script (Either Text ())
idle base = do
  r <- ifRecvOn base
  case (status r, t45Of r) of
    (Good, Right (0, 0, 0)) -> Right () <$ either (\why -> note ("--recover: the idle answer is " <> why <> "; taken as (0, 0, 0)")) pure
                                                  (void (t45On base r))
    (Good, Right answer)    -> pure (Left (notIdle ("answered " <> triple answer <> ", not Table 45's (0, 0, 0)")))
    (Good, Left why)        -> pure (Left (notIdle why))
    (st, _)                 -> pure (Left (notIdle ("the IF-RECV answered " <> describeStatus st)))
  where
    notIdle why = "STACK_RESET on " <> hex base <> ": Success, but the ComID not idle after the reset (Core 3.3.10.5 S1 b, PDF 84): " <> why
    triple (l, o, m) = "(" <> T.intercalate ", " (map tshow [l, o, m]) <> ")"

-- | StartSession to the Admin SP as Anybody after the reset, and its End
--   of Session: what --recover did, or the session the reset left.
probe :: Word16 -> Script (Either Text Text)
probe base = startSession adminSPAsAnybody >>= \case
  Right (Synced h) -> endSession h <&> \case
    Right True  -> Right (done <> "; the Admin SP took a read-write session (TSN " <> tshow (shTSN h) <> ") and ended it")
    Right False -> Left (done <> ", but the TPer did not answer the End of Session of the session it took (TSN " <> tshow (shTSN h)
                         <> ") with its own (Core 3.2.2.3.3.3, PDF 37)")
    Left e      -> Left (done <> ", but the End of Session of the session it took (TSN " <> tshow (shTSN h) <> "): " <> describeExchange e)
  Right (SyncFailed st) -> pure (Left (done <> ", but StartSession to the Admin SP answered " <> statusName st
                                       <> ": a session survived the reset (Core 3.3.4.7.5 item 1, PDF 65; 5.1.5.3, PDF 138)"))
  Right (NoSync why)    -> pure (Left (done <> ", but StartSession to the Admin SP opened no session: " <> why))
  Left e                -> pure (Left (done <> ", but StartSession to the Admin SP: " <> describeExchange e))
  where done = "STACK_RESET on " <> hex base <> ": Success; the ComID idle"
