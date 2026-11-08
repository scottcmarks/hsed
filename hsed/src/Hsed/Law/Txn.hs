{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Law.Txn
Description : The transactions' laws (Core 3.2.2.3.3.4-5 and 3.3.7.3): what the TPer answers the transaction control tokens with, and what a session's open level holds, commits and rolls back
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The laws of the simulator's transactions (M5b: one level, held as the
session's working copy of its SP, "Hsed.Sim.State"'s transactions and
"Hsed.Sim.Session".@sessionPayload@; design 11 M5b, Scott's answer Q5), as
"Hsed.Law.Reset" has the resets': each names the paragraph it implements
by the first 12 hex digits of its IR hash (@spec/ir/core-2.01.json@, the
sha256 of the paragraph's normalised words) and the Test Cases' cases that
observe it, and says what it holds as a QuickCheck property over the
simulator: over a Locking SP session as Admin1 holding a level or none
("Hsed.Sim.Gen".'genTxnSession', its rows varied), the session given as
it was opened, stale once a level is open, as "Hsed.Sim.Sync" hands
"Hsed.Sim.Session" the session read when the packets were resolved
(design 8.3), so that each law also holds the simulator to looking the
session up; and, for what the TPer's hardware sees, over 'genState' as
well, a session of some states holding a level.

No comment binds them yet: each paragraph is unlabelled prose, which no
anchor names (design 13.29 (j), the per-clause anchoring; the plan's D7:
write and cite now, anchor later), so "Hsed.Gen.Core.Laws" lists none of
them and @LawsSpec@ holds them by name and hash to the paragraphs cited
here.

Each law states what it expects in the spec's own terms: a mirror is the
token then its status as a uinteger, written here as Core 3.2.2.3.3.4-5
encodes it (PDF 37) and read back as a host reads a response
("Hsed.Core.Stream".@returnedItems@), not taken from the simulator; a
change a method makes within a session is put through
"Hsed.Sim.State".'putSPView', which M5b's Set writes through
("Hsed.Sim.Base"), the SP's rows the resets change varied
("Hsed.Sim.Gen".@genResetColumns@), so that the laws hold whether or not
a Set is bound (the Set-driven examples are "Hsed.Sim.SessionSpec"'s and
"Hsed.Sim.BaseSpec"'s).
-}
module Hsed.Law.Txn
  ( mirrorStart
  , nestingLimit
  , endStatus
  , immediate
  , visibleWithin
  , commitTopLevel
  , hardwareOnCommit
  , abortedSession
  , rollback
  , failedMethod
  , laws
  ) where

import qualified RIO.Map                 as Map
import           RIO
import           Test.QuickCheck         (Gen, chooseInt, conjoin, counterexample, elements, forAll, forAllShow, oneof, (===))

import           Hsed.CaseKey            (CaseKey (..), TestId (..))
import           Hsed.Core.Call          (MethodCall (..), MethodResponse (..), Parameters (..))
import           Hsed.Core.CellBlock     (cellBlockValue, columnRange, noCells)
import           Hsed.Core.Method        (invocation)
import           Hsed.Core.Stream        (Atom (..), Fault (..), Item (..), Place (..), Returned (..), atoms, hostPolicy,
                                          returnedItems)
import           Hsed.Core.Token         (Token (..))
import           Hsed.Gen.Core.Codes     (StatusCode (..), pattern SUCCESS)
import           Hsed.Gen.Core.Layouts   (DataSubPacket (..))
import qualified Hsed.Gen.Core.Methods   as Methods
import           Hsed.Gen.Core.UIDs      (uC_PIN_SID, uLocking_GlobalRange)
import           Hsed.Law                (Law (..))
import           Hsed.Packet             (DecodedFrame (..), PacketFrame (..), decodePacket)
import           Hsed.Sim.Gen            (genResetColumns, genState, genTxnSession, genTxnSessionAt)
import           Hsed.Sim.Level0         (level0Response, lockedBit)
import           Hsed.Sim.Reset          (resetTPer, stackReset)
import           Hsed.Sim.Session        (Processed (..), sessionPayload)
import           Hsed.Sim.State          (abortSessions, liveSession, programmaticResetEnabled, putSPView, spView, txnDepth)
import           Hsed.Sim.Types          (Outcome (..), SPState (..), Session (..), TPerState (..), txnLimit)


-- * The laws

-- | Core 3.2.2.3.3.4 (PDF 37, its third paragraph): "When the SP delivers
--   its response to the host application's message, the SP's message SHALL
--   mirror that of the host by including Start Transaction tokens in the
--   corresponding places in the message stream. The TPer SHALL supply the
--   status of the Start Transaction request. If the host sends a non-zero
--   status code with the Start Transaction token, the device SHALL respond
--   with a status code of 0x00, unless the transaction was unable to start"
--   (the paragraph before it, 8ccf503ed359: the host's status SHOULD be
--   0x00 and SHALL be ignored). No case the jig catalogues observes the
--   mirror alone; SPF-11 and ETC-09 send the token. The property: for any
--   Locking SP session holding no level and any status 0 to 255 the host
--   sends, a subpacket of Start Transaction with that status and then a Get
--   is answered, as the host reads it, by Start Transaction with status 0
--   where the host's token was and then the Get's result, SUCCESS; a level
--   is open after it, the committed SP as it was, and the payload's
--   processing goes on.
mirrorStart :: Law
mirrorStart = Law
  { lawName  = "Txn.mirrorStart"
  , lawHash  = "9de1edf4c4c6"
  , lawProp  = Just $
      forAll (genTxnSessionAt 0) $ \(ses, st) ->
      forAll (fromIntegral <$> chooseInt (0, 255)) $ \s ->
        let Processed outcome st' ends = run ses [StartTransactionItem s, Invoke lockGet] st
        in counterexample ("the host's status " <> show s) $ conjoin
             [ counterexample "the mirror, status 0, where the host's token was, then the Get's answer" $
                 fmap (map came) (readBack outcome) === Just [Mirrored StartTransaction 0, Answered (statusValue SUCCESS)]
             , counterexample "a level open, the committed SP as it was, the payload going on" $
                 (depthOf ses st', tsSPs st' == tsSPs st, ends) === (1, True, False)
             ]
  , lawCases = []
  }

-- | Core 3.2.2.3.3.4 (PDF 37, its fourth paragraph): "If the host transmits
--   a Start Transaction token that causes the transaction nesting limit to
--   be exceeded, the TPer SHALL abort the session (see Properties Section
--   for details on the transaction nesting limit). If for any reason the
--   TPer is unable to start a transaction as requested by the host, the
--   TPer SHALL abort the session." The limit is MaxTransactionLimit
--   ("Hsed.Sim.Params".@txnLimit@: 1, Opal 2.30 Table 17's least, PDF 34;
--   Core 3.3.7.3.1, PDF 78). ETC-09 observes it (a subpacket of
--   MaxTransactionLimit + 1 Start Transaction tokens, Test Cases PDF
--   81-82). The property: for any Locking SP session holding a level or
--   none, a subpacket of one Start Transaction more than the limit leaves
--   room for, then a Get, aborts the session: no result and no mirror is
--   sent, the TPer's CloseSession is prepared (the run preparing one), the
--   session is gone with its levels, the committed SP is as it was, and the
--   payload's processing ends.
nestingLimit :: Law
nestingLimit = Law
  { lawName  = "Txn.nestingLimit"
  , lawHash  = "bf0b5ba552ce"
  , lawProp  = Just $
      forAll genTxnSession $ \(ses, st) ->
        let limit  = maybe 0 fromIntegral (txnLimit (tsParams st)) :: Int
            tokens = replicate (limit - depthOf ses st + 1) (StartTransactionItem 0)
            Processed outcome st' ends = run ses (tokens <> [Invoke lockGet]) st
        in counterexample ("the limit " <> show limit <> " at depth " <> show (depthOf ses st)) $ conjoin
             [ counterexample ("the session aborted, nothing sent, CloseSession prepared: " <> show outcome) $
                 case outcome of
                   SessionAborted [] (Just _) -> True
                   _                          -> False
             , counterexample "the session gone with its levels, the committed SP as it was, the payload ended" $
                 (liveSession ses st', tsSPs st' == tsSPs st, ends) === (Nothing, True, True)
             ]
  , lawCases = [whole (ETC 9)]
  }

-- | Core 3.2.2.3.3.5 (PDF 37, its last paragraph on the page): "If the host
--   sends a status code with an End Transaction token that the device does
--   not support, the device SHALL abort the transaction and return a status
--   code of 0x01"; with its neighbours, which the property holds as well:
--   79d343f18ba4 (PDF 37: a status of 0x00 has the device attempt to commit
--   the level, returning 0x00 committed or 0x01 not), ecd7273bc677 (PDF 37:
--   0x00 commits, 0x01 aborts the level, which the TPer SHALL abort) and
--   6c4b41c2216c (PDF 38: a status other than 0x00 aborts that level). No
--   case the jig catalogues sends End Transaction. The property: for any
--   Locking SP session holding a level, with any change in its copy, and
--   any status 0 to 255, End Transaction is mirrored with 0 for 0 and with
--   1 for any other, the level is closed either way, and the committed SP
--   is the level's copy for 0 and as it was for any other.
endStatus :: Law
endStatus = Law
  { lawName  = "Txn.endStatus"
  , lawHash  = "f8d962966136"
  , lawProp  = Just $
      forAll (genTxnSessionAt 1) $ \(ses, st) ->
      forAll (fromIntegral <$> chooseInt (0, 255)) $ \s ->
        let copy     = viewOf ses st
            expected = if s == 0 then Map.insert (sesSP ses) copy (tsSPs st) else tsSPs st
            Processed outcome st' _ = run ses [EndTransactionItem s] st
        in counterexample ("the host's status " <> show s) $ conjoin
             [ counterexample "the mirror's status" $
                 fmap (map came) (readBack outcome) === Just [Mirrored EndTransaction (if s == 0 then 0 else 1)]
             , counterexample "the level closed, the session kept" $ (depthOf ses st', isJust (liveSession ses st')) === (0, True)
             , counterexample "the committed SP: the copy for 0, else as it was" $ tsSPs st' === expected
             ]
  , lawCases = []
  }

-- | Core 3.3.7.3 item a (PDF 77): "When a method is invoked outside of a
--   transaction, and resolves successfully, changes made by that method
--   SHALL be committed and made persistent immediately." No case the jig
--   catalogues observes it before a Set is bound (SPF-12:1/no-media's Sets,
--   the integration's). The property: for any Locking SP session holding
--   no level and any change to the SP as the session sees it, the change
--   put through "Hsed.Sim.State".'putSPView' is the committed SP at once,
--   is what the session then sees, and leaves the session as it was.
immediate :: Law
immediate = Law
  { lawName  = "Txn.immediate"
  , lawHash  = "b63e825173c6"
  , lawProp  = Just $
      forAll (genTxnSessionAt 0) $ \(ses, st) ->
      forAll (varied ses st) $ \sps' ->
        let st' = putSPView ses sps' st
        in conjoin
             [ counterexample "committed at once" $ Map.lookup (sesSP ses) (tsSPs st') === Just sps'
             , counterexample "what the session sees" $ spView ses st' === Just sps'
             , counterexample "the session as it was" $ liveSession ses st' === liveSession ses st
             ]
  , lawCases = []
  }

-- | Core 3.3.7.3 (PDF 77, the paragraph after its two ways): "Changes made
--   within a transaction SHALL be visible within that transaction. For
--   instance, modification of a table value within a transaction would
--   result in the new value being returned by a Get method invoked within
--   that transaction. Those changes SHALL be made persistent when the
--   top-level transaction is committed. If the transaction is aborted,
--   those changes SHALL be rolled back." SPF-11 observes the rollback
--   ('rollback'). The property: for any Locking SP session holding a level
--   and any change to the SP as it sees it, the change put through
--   'putSPView' is what the session sees ('spView'), while the committed
--   SP is as it was and the level stays open. (That a Get within the
--   transaction returns the new value is "Hsed.Sim.SessionSpec"'s example:
--   Get reads 'spView', "Hsed.Sim.Base".)
visibleWithin :: Law
visibleWithin = Law
  { lawName  = "Txn.visibleWithin"
  , lawHash  = "94b5fe4d5b75"
  , lawProp  = Just $
      forAll (genTxnSessionAt 1) $ \(ses, st) ->
      forAll (varied ses st) $ \sps' ->
        let st' = putSPView ses sps' st
        in conjoin
             [ counterexample "what the session sees" $ spView ses st' === Just sps'
             , counterexample "the committed SP as it was" $ tsSPs st' === tsSPs st
             , counterexample "the level still open" $ depthOf ses st' === 1
             ]
  , lawCases = []
  }

-- | Core 3.3.7.3 item b (PDF 77): "When a method is invoked inside of a
--   transaction or set of nested transactions, changes made by that method
--   SHALL be committed and made persistent when the top-level transaction
--   is committed." No case the jig catalogues commits a transaction. The
--   property: for any Locking SP session holding no level and any change
--   to its SP, Start Transaction, the change put, then End Transaction
--   with status 0 leaves the TPer exactly as the change put outside a
--   transaction does: the committed SP the changed one, the session with
--   no level, everything else as it was.
commitTopLevel :: Law
commitTopLevel = Law
  { lawName  = "Txn.commitTopLevel"
  , lawHash  = "fc73da1d9758"
  , lawProp  = Just $
      forAll (genTxnSessionAt 0) $ \(ses, st) ->
      forAll (varied ses st) $ \sps' ->
        let opened    = prState (run ses [StartTransactionItem 0] st)
            committed = prState (run ses [EndTransactionItem 0] (putSPView ses sps' opened))
        in committed === putSPView ses sps' st
  , lawCases = []
  }

-- | Core 3.3.7.3 (PDF 77, the paragraph after 'visibleWithin'): "Changes
--   that affect other aspects of the TPer (i.e. hardware settings) SHALL
--   occur when associated changes are successfully committed. This means
--   that changes made during transactions that affect the state of the
--   device, such as changing media encryption keys or read/write lock
--   state, SHALL NOT occur until the changes are successfully committed."
--   The simulator's hardware is what Level 0 reports of the Locking SP
--   ("Hsed.Sim.Level0": the Locked, MBREnabled and MBRDone bits of the
--   Locking descriptor, Opal 2.30 3.1.1.3, PDF 21, from the committed
--   rows) and what TPER_RESET's enabling reads
--   ("Hsed.Sim.State".'programmaticResetEnabled'). No case the jig
--   catalogues reads Level 0 inside a transaction. The property: for any
--   state with a session holding a level whose copy differs from the
--   committed SP in its lock columns ('genTxnSessionAt' 1), and any state
--   'genState' gives, the Level 0 response, the Locked bit and
--   ProgrammaticResetEnable are those of the state with every level
--   dropped.
hardwareOnCommit :: Law
hardwareOnCommit = Law
  { lawName  = "Txn.hardwareOnCommit"
  , lawHash  = "42e10a6c398f"
  , lawProp  = Just $
      forAll (oneof [snd <$> genTxnSessionAt 1, genState]) $ \st ->
        let dropped = st { tsSessions = fmap (\s -> s { sesTxn = [] }) (tsSessions st) }
        in conjoin
             [ counterexample "the Level 0 response" $ level0Response st 2048 === level0Response dropped 2048
             , counterexample "the Locked bit" $ lockedBit st === lockedBit dropped
             , counterexample "ProgrammaticResetEnable" $ programmaticResetEnabled st === programmaticResetEnabled dropped
             ]
  , lawCases = []
  }

-- | Core 3.3.7.3 (PDF 77, its first normative sentence): "If a session is
--   aborted, any open transactions SHALL be aborted"; 3.3.7.1.5 (PDF 75-76:
--   an aborted session's uncommitted transactions aborted, the host aborting
--   a session with End of Session). SPF-01 case 2 (Test Cases PDF 51-52)
--   and SPF-11's extension observe it. The property: for any Locking SP
--   session holding a level, each way the session ends short of a commit (a
--   fault in its stream, the host's End of Session, a Start Transaction
--   past the limit, a second invocation past the TPer's MaxMethods, and the
--   TPer aborting it outright) leaves the session gone, the committed SP as
--   it was, and no session of the TPer holding a level.
abortedSession :: Law
abortedSession = Law
  { lawName  = "Txn.abortedSession"
  , lawHash  = "7b6ae99fbdd6"
  , lawProp  = Just $
      forAll (genTxnSessionAt 1) $ \(ses, st) ->
      forAllShow (elements aborts) fst $ \(_, abortBy) ->
        let st' = abortBy ses st
        in conjoin
             [ counterexample "the session gone" $ liveSession ses st' === Nothing
             , counterexample "the committed SP as it was" $ tsSPs st' === tsSPs st
             , counterexample "no level held anywhere" $ all (null . sesTxn) (tsSessions st')
             ]
  , lawCases = []
  }

-- | Core 3.3.7.3 (PDF 78, the paragraph after its steps): "If a transaction
--   is aborted all SP state SHALL be reset ("rolled-back") to its value at
--   the time the transaction was opened (e.g., authentication state),
--   unless otherwise noted in this specification (e.g., logging). A
--   transaction SHALL only be committed at the request of the host
--   application. The TPer SHALL only commit or abort a transaction upon
--   receipt of an End Transaction token from the host except in the case
--   when a session is aborted." SPF-11 observes it (a Set inside a
--   transaction, then STACK_RESET, then the Get reads the old value, Test
--   Cases PDF 64-65). The property: for any Locking SP session holding no
--   level and any change to its SP, after Start Transaction and the change
--   put, each of End Transaction with status 1 or with one the TPer does not
--   support, End of Session, a session abort at a fault, STACK_RESET on the
--   session's ComID (Core 3.3.4.7.5 item 2, PDF 65) and each reset of the
--   TPer (a power cycle, a hardware reset, a hot plug, TPER_RESET: Opal
--   2.30 3.3.5.1 item 2, PDF 31; 3.2.3 c, PDF 29) leaves the committed SPs
--   as they were when the level opened, or, for a reset of the TPer, as
--   that reset makes them of the state the level opened in; and no session
--   holds a level.
rollback :: Law
rollback = Law
  { lawName  = "Txn.rollback"
  , lawHash  = "26cb73b09478"
  , lawProp  = Just $
      forAll (genTxnSessionAt 0) $ \(ses, st) ->
      forAll (varied ses st) $ \sps' ->
      forAllShow (elements (undoings (sesComId ses))) fst $ \(_, undo) ->
        let atST = prState (run ses [StartTransactionItem 0] st)
            put  = putSPView ses sps' atST
            (after, expected) = undo ses put atST
        in conjoin
             [ counterexample "the committed SPs" $ tsSPs after === tsSPs expected
             , counterexample "no level held anywhere" $ all (null . sesTxn) (tsSessions after)
             ]
  , lawCases = [whole (SPF 11)]
  }

-- | Core 3.3.7.3 (PDF 78, the paragraph after 'rollback''s): "The failure or
--   success of the methods encapsulated in the transaction SHALL NOT
--   directly affect whether or not the host is able to commit the
--   transaction, but committing a transaction in which method invocations
--   have not succeeded MAY leave the SP in an intermediate (and
--   potentially unrecoverable) state. A failed method within a transaction
--   SHALL NOT affect the state of the transaction or the state of the SP
--   within the transaction, unless otherwise noted (e.g., logging, PIN
--   tries count, etc.)." (The M5b plan filed this sentence under
--   5298d3b33b27, which is 3.3.7.3.1's nested-transactions paragraph, PDF
--   78; StateSpec holds that one.) No case the jig catalogues fails a
--   method inside a transaction. The property: for any Locking SP session
--   holding a level, with any change in its copy, each method that fails (a
--   Get on an object the SP lacks, NOT_AUTHORIZED; a Get whose parameters
--   are not Get's, INVALID_PARAMETER; an invocation whose header or
--   parameter list does not read; a Get the host sent with a status of its
--   own, echoed) is answered with a status other than SUCCESS and leaves the
--   TPer exactly as it was: the level, its copy and the committed SP.
failedMethod :: Law
failedMethod = Law
  { lawName  = "Txn.failedMethod"
  , lawHash  = "de557e6e406b"
  , lawProp  = Just $
      forAll (genTxnSessionAt 1) $ \(ses, st) ->
      forAll (elements failing) $ \(name, item) ->
        let Processed outcome st' _ = run ses [item] st
        in counterexample name $ conjoin
             [ counterexample "the TPer as it was: the level, its copy and the committed SP" $ st' === st
             , counterexample ("the method answered, not SUCCESS: " <> show outcome) $
                 case fmap (map came) (readBack outcome) of
                   Just [Answered status] -> status /= statusValue SUCCESS
                   _                      -> False
             ]
  , lawCases = []
  }

-- | The transactions' laws: none is bound by a comment yet (design 13.29 (j)).
laws :: [Law]
laws = [mirrorStart, nestingLimit, endStatus, immediate, visibleWithin, commitTopLevel, hardwareOnCommit, abortedSession, rollback, failedMethod]


-- * What the laws run

-- | A data subpacket's items processed in a session, with no fault
--   ("Hsed.Sim.Session".'sessionPayload').
run :: Session -> [Item] -> TPerState -> Processed
run ses items = sessionPayload ses items Nothing

-- | A fault that aborts a session: a token outside a method invocation
--   (Core 3.2.2.4.2 item 2, PDF 39).
outOfPlace :: Fault
outOfPlace = Fault 0 OutsideCall "EndList outside a method invocation (Core 3.2.2.4.2 item 2, PDF 39)"

-- | A Get the session may make as Admin1: Locking_GlobalRange's ReadLocked
--   and WriteLocked, columns 7 and 8 (Opal 2.30 Table 45, PDF 86; the ACE
--   of the Locking SP's preconfiguration, Table 38, PDF 66-78).
lockGet :: MethodCall
lockGet = invocation uLocking_GlobalRange (Methods.Get (cellBlockValue (columnRange 7 8)))

-- | Each way a session ends short of a commit, with the TPer after it.
aborts :: [(String, Session -> TPerState -> TPerState)]
aborts =
  [ ("a fault in the session's stream (Core 3.2.2.4.2, PDF 39)", \ses -> prState . sessionPayload ses [] (Just outOfPlace))
  , ("the host's End of Session (3.3.7.1.5, PDF 76)", \ses -> prState . run ses [EndOfSessionItem])
  , ("a Start Transaction past the limit (3.2.2.3.3.4, PDF 37)", \ses -> prState . run ses [StartTransactionItem 0])
  , ("a second invocation past the TPer's MaxMethods (5.2.2.4.1.7, PDF 146)", \ses -> prState . run ses [Invoke lockGet, Invoke lockGet])
  , ("the TPer aborting the session (3.3.7.1.5, PDF 75)", \ses -> abortSessions (\s -> (sesComId s, sesTSN s) == (sesComId ses, sesTSN ses)))
  ]

-- | Each way an open level is undone short of a commit, on a ComID: given
--   the session, the TPer with the change put, and the TPer as the level
--   opened in it, the TPer after the undoing and the TPer whose committed
--   SPs it must equal.
undoings :: Word16 -> [(String, Session -> TPerState -> TPerState -> (TPerState, TPerState))]
undoings c =
  [ ("End Transaction with status 1 (Core 3.2.2.3.3.5, PDF 37-38)", \ses put atST -> (prState (run ses [EndTransactionItem 1] put), atST))
  , ("End Transaction with a status the TPer does not support, 7 (PDF 37)", \ses put atST -> (prState (run ses [EndTransactionItem 7] put), atST))
  , ("End of Session (3.3.7.1.5, PDF 76)", \ses put atST -> (prState (run ses [EndOfSessionItem] put), atST))
  , ("a session abort at a fault (3.2.2.4.2, PDF 39)", \ses put atST -> (prState (sessionPayload ses [] (Just outOfPlace) put), atST))
  , ("STACK_RESET on the session's ComID (3.3.4.7.5 item 2, PDF 65)", \_ put atST -> (stackReset c put, atST))
  ] <>
  [ ("a reset of the TPer, " <> show kind <> " (Opal 2.30 3.3.5.1 item 2, PDF 31; 3.2.3 c, PDF 29)", \_ put atST -> (resetTPer kind put, resetTPer kind atST))
  | kind <- [minBound .. maxBound] ]

-- | Each method that fails inside a session, and why.
failing :: [(String, Item)]
failing =
  [ ("a Get on an object the SP lacks, NOT_AUTHORIZED (Core 5.1.5.2, PDF 137)", Invoke (invocation uC_PIN_SID (Methods.Get (cellBlockValue noCells))))
  , ("a Get whose parameters are not Get's, INVALID_PARAMETER (5.1.5.11, PDF 139)", Invoke lockGet { _parameters = Parameters [] [] })
  , ("a Get the host sent with a status of its own, 7, echoed (3.2.4.2, PDF 49)", Invoke lockGet { _statusCode = 7 })
  , ("an invocation whose header does not read, NOT_AUTHORIZED (3.2.2.4.2 item 3, PDF 39)", BadCall (Fault 1 InHeader "a uinteger where the InvokingID is expected"))
  , ("an invocation whose parameter list does not read, INVALID_PARAMETER (3.2.2.4.2 item 4, PDF 39)", BadCall (Fault 3 (InParameters (_header lockGet)) "a Call in the parameter list"))
  ]


-- * What the laws read

-- | A session's transaction depth as the TPer holds it
--   ("Hsed.Sim.State".'liveSession'), none for a session it no longer
--   holds.
depthOf :: Session -> TPerState -> Int
depthOf ses = maybe 0 txnDepth . liveSession ses

-- | The SP as the session sees it ('spView'), an SP of no tables where the
--   TPer lacks it.
viewOf :: Session -> TPerState -> SPState
viewOf ses st = fromMaybe (SPState (sesSP ses) mempty mempty mempty) (spView ses st)

-- | The SP as the session sees it with the rows the resets change varied
--   ("Hsed.Sim.Gen".'genResetColumns'): a change a method within the
--   session would put.
varied :: Session -> TPerState -> Gen SPState
varied ses = genResetColumns . viewOf ses

-- | What came back for an item, as the host reads it: a transaction
--   control token mirrored with its status, a method answered with its
--   status, or something else.
data Came
  = Mirrored !Token !Natural
  | Answered !Natural
  | Other
  deriving (Eq, Show)

-- | A returned item's shape.
came :: Returned -> Came
came = \case
  ReturnedTransaction t s -> Mirrored t s
  ReturnedResults r       -> Answered (_resultStatus r)
  _                       -> Other

-- | What the host reads of the packets prepared: their data subpackets'
--   payloads in order, read as one response stream
--   ("Hsed.Core.Stream".'returnedItems'); 'Nothing' for an outcome other
--   than packets prepared, a packet that does not decode, or a stream that
--   does not read whole.
readBack :: Outcome -> Maybe [Returned]
readBack = \case
  Prepared ps -> do
    payloads <- traverse subpacketsOf ps
    as       <- either (const Nothing) Just (atoms hostPolicy (mconcat (concat payloads)))
    case returnedItems (map atomToken as) of
      (rs, Nothing) -> Just rs
      _             -> Nothing
  _ -> Nothing
  where
    subpacketsOf p = either (const Nothing) (\(DecodedFrame (PacketFrame _ sps) _ _) -> Just (map dataSubPacketPayload sps)) (decodePacket p)

-- | A test, all of it.
whole :: TestId -> CaseKey
whole t = CaseKey t Nothing Nothing Nothing
