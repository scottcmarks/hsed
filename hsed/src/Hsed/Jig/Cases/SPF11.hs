{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Jig.Cases.SPF11
Description : SPF-11, STACK_RESET rolls back a transaction: User1 enabled inside one, then a reset, then not
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SPF-11 (Test Cases 1.02, PDF 64-65): StartSession to the Locking SP as
Admin1, a Start Transaction with status 0x00, which the TPer mirrors with
its own 0 (Core 3.2.2.3.3.4, PDF 37), Set of User1's Enabled column TRUE
(ACE_Authority_Set_Enabled, Opal 2.30 Table 39, PDF 79: Admins may Set
column 5), STACK_RESET, which aborts the session and its open transaction
(Core 3.3.4.7.5 items 1-2, PDF 65), so that the Set is rolled back (3.3.7.3,
PDF 78); then a new session as Admin1, whose Get finds User1's Enabled
FALSE (PDF 65), and CLOSE_SESSION. Its own prerequisite is User1 not
enabled (PDF 64), read from the simulator's state, the case skipped where
it does not hold. STACK_RESET is the one reset the Test Cases trigger
besides SPF-12's (4.1, PDF 26). Its sessions need the Locking SP
Manufactured, the common prerequisite of 6.2 (PDF 49); its Set is a write
no drive takes before M6, so the simulator alone runs it (design 9.3; 13.34
(8)), on its Set and its one level of transactions (since M5b, design 11
M5b).

The jig's extensions are the two rules the steps rest on: a change inside a
transaction is visible there and End Transaction with status 1 rolls it
back (Core 3.3.7.3, PDF 77-78), the committed cell read from the simulator's
state the while; and End of Session with a transaction open rolls it back
too, the session aborted taking its transaction with it (Core 3.3.7.1.5,
PDF 76; 3.3.7.3, PDF 78; Test Cases SPF-01 case 2, PDF 51-52).
-}
module Hsed.Jig.Cases.SPF11
  ( spf11
  ) where

import           RIO

import           Hsed.Core.CellBlock (columnRange)
import           Hsed.Core.Table     (TableDesc (..))
import           Hsed.Core.Value     (Datum (..), List (..), Name (..), NamedValue (..), Value (..))
import           Hsed.Core.Token     (Token (..))
import           Hsed.Gen.Core.Codes (pattern SUCCESS)
import           Hsed.Gen.Core.TableDescs (authorityTableDesc)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Case       (Case (..), Need (..))
import           Hsed.Jig.Catalogue  (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq     (Section (..))
import           Hsed.Jig.Script     (MethodOutcome (..), Script, SessionHandle (..), StartArgs, Verdict (..), asAuthority, attempt,
                                      endTransaction, exchanged, expectMethodStatus, getCells, namedColumn, observe, opened,
                                      sessionGone, setCells, simCell, stackReset, startSession, startTransaction, statusName, unmet,
                                      withSession)
import           Hsed.Sim.Cells      (isTrue)
import           Hsed.Wire           (hex)


-- | The case.
spf11 :: Case
spf11 = Case
  { cKey        = CaseKey (SPF 11) Nothing Nothing Nothing
  , cSection    = SpecificFunctionality
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedWrite, NeedSimOnly]
  , cRun        = run
  , cExtensions =
      [ ("a change inside a transaction is visible there, and End Transaction with status 1 rolls it back (Core 3.2.2.3.3.5, PDF 37; 3.3.7.3, PDF 77-78)", visibleThenRolledBack)
      , ("End of Session with a transaction open rolls it back (Core 3.3.7.1.5, PDF 76; 3.3.7.3, PDF 78; Test Cases SPF-01 case 2, PDF 51-52)", closedRollsBack)
      ]
  }

-- | The case's own prerequisite checked (User1 not enabled, PDF 64), then
--   step 1 StartSession, steps 2 and 3 in the session, step 4 STACK_RESET
--   whatever came of them (so that the session the reset aborts is not
--   left open), and, every step so far passed, steps 5 to 7 in a session
--   of their own ('withSession').
run :: Script Verdict
run = userEnabled >>= \case
  Nothing    -> pure (Skipped "prerequisite unmet: User1's Enabled column is not in the simulator's state (Opal 2.30 Table 38, PDF 66)")
  Just True  -> pure (Skipped "prerequisite unmet: User1 is enabled (Test Cases SPF-11 prerequisite 1, PDF 64)")
  Just False -> do
    observe "prerequisite" "User1 is not enabled: its Enabled column FALSE in the simulator's state (Test Cases SPF-11 prerequisite 1, PDF 64)"
    a <- admin1
    enabled <- enabledColumn
    h <- startSession a >>= exchanged step1 >>= opened step1
    v23 <- attempt (Pass <$ (step2 h >> step3 h enabled))
    v4  <- attempt (Pass <$ step4 h)
    case (v23, v4) of
      (Pass, Pass) -> withSession a (\h' -> Pass <$ step6 h' enabled)
      _            -> pure (Fail (reasons v23 <> reasons v4))
  where
    step1 = "step 1: StartSession to the Locking SP as Admin1"
    reasons = \case
      Fail rs        -> rs
      Skipped r      -> [r]
      Unobservable r -> [r]
      Pass           -> []

-- | StartSession's arguments for Admin1 of the Locking SP (Test Cases PDF
--   64), its PIN read from the simulator's state.
admin1 :: Script StartArgs
admin1 = asAuthority "Admin1" uSP_Locking LockingSP.uAuthority_Admin1

-- | The Enabled column's number, found by name in the generated Authority
--   description (Core Table 179, PDF 166).
enabledColumn :: Script Natural
enabledColumn = namedColumn authorityTableDesc "Enabled"

-- | Whether User1 is enabled, as the simulator's committed state holds its
--   Authority row's Enabled cell ('simCell'; Opal 2.30 Table 38, PDF 66);
--   'Nothing' on a drive, or where the state holds no such cell.
userEnabled :: Script (Maybe Bool)
userEnabled = fmap isTrue <$> simCell uSP_Locking (tdTableUID authorityTableDesc) LockingSP.uAuthority_User1 "Enabled"

-- | Step 2: Start Transaction with status 0x00, mirrored by the TPer with
--   its own status 0 (Core 3.2.2.3.3.4, PDF 37), the step named.
step2 :: SessionHandle -> Script ()
step2 h = do
  mirror <- startTransaction h >>= exchanged name
  unless (mirror == 0) (unmet (name <> ": the TPer mirrored it with status " <> tshow mirror <> ", not 0 (Core 3.2.2.3.3.4, PDF 37)"))
  observe "step 2" "Start Transaction with status 0x00: the TPer mirrored it with status 0 (Core 3.2.2.3.3.4, PDF 37)"
  where name = "step 2: Start Transaction with status 0x00"

-- | Step 3: Set User1's Enabled TRUE in the transaction, SUCCESS.
step3 :: SessionHandle -> Natural -> Script ()
step3 h enabled = do
  outcome <- setCells h LockingSP.uAuthority_User1 [(enabled, true)] >>= exchanged name
  expectMethodStatus name SUCCESS outcome
  observe "step 3" ("Set User1's Enabled TRUE inside the transaction: " <> statusName SUCCESS)
  where name = "step 3: Set User1's Enabled TRUE"

-- | Step 4: STACK_RESET on the session's ComID, Success/Failure 0, which
--   aborts the session and its open transaction (Core 3.3.4.7.5 items 1-2,
--   PDF 65), the session then forgotten ('sessionGone').
step4 :: SessionHandle -> Script ()
step4 h = stackReset c >>= \case
  Right 0 -> do
    observe "step 4" ("STACK_RESET on " <> hex c <> ": Success/Failure 0, every session on the ComID aborted, its transaction with it "
                      <> "(Core 3.3.4.7.5 items 1-2, PDF 65)")
    sessionGone h ("STACK_RESET on " <> hex c <> " answered Success/Failure 0: every session on the ComID aborted, its open transaction "
                   <> "with it (Core 3.3.4.7.5 items 1-2, PDF 65)")
  Right n  -> unmet (name <> ": STACK_RESET on " <> hex c <> " answered Success/Failure " <> tshow n <> ", the stack not reset (Core Table 36, PDF 66)")
  Left why -> unmet (name <> ": " <> why)
  where
    c    = shComId h
    name = "step 4: STACK_RESET"

-- | Step 6: Get User1's Enabled column in the new session, FALSE (PDF 65):
--   the Set of step 3 rolled back with the transaction (Core 3.3.7.3, PDF
--   78).
step6 :: SessionHandle -> Natural -> Script ()
step6 h enabled = do
  v <- userEnabledIn name h enabled
  unless (v == false) (unmet (name <> ": User1's Enabled is " <> tshow v <> ", not FALSE (Test Cases PDF 65)"))
  observe "step 6" "Get User1's Enabled: FALSE (0): the Set inside the transaction STACK_RESET aborted was rolled back (Test Cases PDF 65; Core 3.3.7.3, PDF 78)"
  where name = "step 6: Get User1's Enabled"

-- | Get User1's Enabled column in a session, the step named: SUCCESS, and
--   the value the result holds in the column.
userEnabledIn :: Text -> SessionHandle -> Natural -> Script Value
userEnabledIn name h enabled = do
  outcome <- getCells h LockingSP.uAuthority_User1 (columnRange enabled enabled) >>= exchanged name
  expectMethodStatus name SUCCESS outcome
  columnIn name enabled (moResults outcome)

-- | The value a Get's result holds in a column: @[[c = v]]@, one list of
--   the row's values (Core 5.3.3.6.2.2, PDF 201), the step named; the
--   script stopped where the result is not of that form.
columnIn :: Text -> Natural -> [Value] -> Script Value
columnIn name c = \case
  [L (List cells)] -> case [ v | N (NamedValue (Name (Datum (Unsigned col))) v) <- cells, col == c ] of
    v : _ -> pure v
    []    -> unmet (name <> ": no column " <> tshow c <> " among the result's " <> tshow (length cells) <> " cell(s) (Core 5.3.3.6.2.2, PDF 201)")
  results -> unmet (name <> ": the result is not one list of the row's values: " <> tshow (length results) <> " result(s)")

-- | A boolean as a method carries it: 1 True, 0 False (Core Table 59, PDF
--   103; 5.1.2 b, PDF 99: an enumeration as its value).
true, false :: Value
true  = D (Datum (Unsigned 1))
false = D (Datum (Unsigned 0))

-- | The extension: in one session, Start Transaction, Set User1's Enabled
--   TRUE, which a Get in the transaction then reads as 1 while the
--   simulator's committed state still holds False (Core 3.3.7.3 b, PDF 77:
--   changes made within a transaction are visible within it, persistent
--   when it commits); End Transaction with status 1, mirrored 1 (Core
--   3.2.2.3.3.5, PDF 37: the transaction aborted); and a Get reading 0 again
--   (3.3.7.3, PDF 78: rolled back).
visibleThenRolledBack :: Script Verdict
visibleThenRolledBack = admin1 >>= \a -> withSession a $ \h -> do
  enabled <- enabledColumn
  step2 h
  step3 h enabled
  within <- userEnabledIn "Get User1's Enabled inside the transaction" h enabled
  unless (within == true) (unmet ("Get User1's Enabled inside the transaction: " <> tshow within <> ", not TRUE (Core 3.3.7.3 b, PDF 77)"))
  committed <- userEnabled
  unless (committed == Just False) $
    unmet ("the simulator's committed state holds User1's Enabled " <> maybe "nowhere" tshow committed <> " while the transaction is open, not False (Core 3.3.7.3 b, PDF 77)")
  mirror <- endTransaction h 1 >>= exchanged "End Transaction with status 1"
  unless (mirror == 1) (unmet ("End Transaction with status 1: the TPer mirrored it with status " <> tshow mirror <> ", not 1 (Core 3.2.2.3.3.5, PDF 37)"))
  after <- userEnabledIn "Get User1's Enabled after the End Transaction" h enabled
  unless (after == false) (unmet ("Get User1's Enabled after the End Transaction: " <> tshow after <> ", not FALSE (Core 3.3.7.3, PDF 78)"))
  observe "inside the transaction" "User1's Enabled read 1 in the session while the committed state held False; End Transaction with status 1 was mirrored 1, and it read 0 again"
  pure Pass

-- | The extension: in one session, Start Transaction and Set User1's
--   Enabled TRUE, then the host's End of Session with the transaction open
--   ('withSession' sends it), which aborts the transaction with the session
--   (Core 3.3.7.1.5, PDF 76; 3.3.7.3, PDF 78); the committed state holds
--   False, and a new session's Get reads 0 (Test Cases SPF-01 case 2, PDF
--   51-52: a session closed without committing its transaction).
closedRollsBack :: Script Verdict
closedRollsBack = do
  a <- admin1
  enabled <- enabledColumn
  v <- withSession a $ \h -> do
    step2 h
    step3 h enabled
    within <- userEnabledIn "Get User1's Enabled inside the transaction" h enabled
    unless (within == true) (unmet ("Get User1's Enabled inside the transaction: " <> tshow within <> ", not TRUE (Core 3.3.7.3 b, PDF 77)"))
    pure Pass
  case v of
    Pass -> do
      committed <- userEnabled
      unless (committed == Just False) $
        unmet ("the simulator's committed state holds User1's Enabled " <> maybe "nowhere" tshow committed <> " after the End of Session, not False (Core 3.3.7.3, PDF 78)")
      withSession a $ \h -> do
        after <- userEnabledIn "Get User1's Enabled in a new session" h enabled
        unless (after == false) (unmet ("Get User1's Enabled in a new session: " <> tshow after <> ", not FALSE (Core 3.3.7.3, PDF 78)"))
        observe "after the End of Session" "User1's Enabled read 0 in a new session, and the committed state held False: the transaction the End of Session left open was rolled back"
        pure Pass
    other -> pure other
