{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Jig.Cases.SPF02
Description : SPF-02, IF-RECV behaviour: case 1, nothing to return; case 2, an insufficient transfer length
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

Case 2 (PDF 52-53, since M5b): a session to the Locking SP as Admin1, a Get
of the DataStore's rows 0 to 1023 sent alone, whose answer an IF-RECV of
one 512-byte unit cannot carry (the Test Cases' transfer length of 1, in
the end's unit: design 11 M5b, D11), Table 45's "Response ready,
insufficient transfer length request" (Core Table 45, PDF 83), the IF-RECV
asked again at that length while the TPer answers responses to come (Test
Cases 4.1 a, PDF 26: a response held, @--hold-processing@), then the answer
at its MinTransfer, the Get's result one byte string of the 1024 rows (Core
5.3.3.6.2.1, PDF 201: Get on a byte table returns Bytes, from the lowest
row to the highest), and CLOSE_SESSION. Its session needs the Locking SP
Manufactured, the common prerequisite of 6.2 (PDF 49), so the simulator
alone runs it (design 9.3, 11 M5b), on its Get of a byte table (since M5b).
The jig's extension reads the same rows in a whole exchange and holds them
to the DataStore the simulator's state keeps.

Case 1's own prerequisite, In Awaiting IF-SEND, is the common SyncIdle
the runner establishes. Its extension's subpacket is four Empty atoms (the
codec's @Empty@ token, Core Table 13) in a ComPacket of the Test Cases'
form; the simulator decides at the IF-SEND to discard it and keeps the
ComID in S1 (design 8.2), which the extension reads from the simulator's
state before the IF-RECV.
-}
module Hsed.Jig.Cases.SPF02
  ( spf02case1
  , spf02case2
  ) where

import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO

import           Hsed.Core.Call        (MethodResponse (..))
import           Hsed.Core.CellBlock   (cellBlockValue)
import           Hsed.Core.Method      (invocation)
import           Hsed.Core.Stream      (Returned (..))
import           Hsed.Core.StreamItem  (StreamItem (..))
import           Hsed.Core.Token       (Token (..))
import           Hsed.Core.Value       (Datum (..), Value (..))
import           Hsed.Gen.Core.Codes   (StatusCode (..), pattern SUCCESS)
import qualified Hsed.Gen.Core.Methods as Methods
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Case         (Case (..), Need (..))
import           Hsed.Jig.Catalogue    (CaseKey (..), TestId (..))
import           Hsed.Jig.Options      (RunOptions (..))
import           Hsed.Jig.Prereq       (Section (..), lockingSPUntouched, noSessionOpened)
import           Hsed.Jig.Script       (MethodOutcome (..), RunEnv (..), Script, SessionHandle (..), StartArgs, Verdict (..),
                                        asAuthority, baseComId, byteRows, controlComPacket, dataStoreTable, exchanged,
                                        expectMethodStatus, expectStatus, expectT45, framesOf, getCells, ifRecvOn, ifRecvWith,
                                        observe, peekSim, rawIfSend, returnedOf, sendInSession, sendLength, simOnly, statusName,
                                        t45On, t45RowOf, unmet, withSession)
import           Hsed.Sim.Bytes        (byteRange)
import           Hsed.Sim.Sync         (T45Row (..))
import           Hsed.Sim.Types        (ByteTable (..), SyncState (..))
import           Hsed.Sim.View         (ComIdView (..), SimQuery (..))
import           Hsed.Wire             (ExchangeOpts (..), IfStatus (..), Response)


-- $setup
-- >>> :set -XOverloadedStrings

-- | The case.
spf02case1 :: Case
spf02case1 = Case
  { cKey        = CaseKey (SPF 2) (Just 1) Nothing Nothing
  , cSection    = SpecificFunctionality
  , cWaives     = lockingSPUntouched : noSessionOpened
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv]
  , cRun        = run
  , cExtensions = [("Empty atoms: an IF-SEND of them leaves the ComID in S1 (Core 3.3.10.6 S2:S1 d.c, PDF 86)", emptyAtoms)]
  }

-- | Case 2 (Test Cases PDF 52-53): the insufficient transfer length, then
--   the answer at its MinTransfer. Its session needs the Locking SP
--   Manufactured (6.2 item 2, PDF 49); the simulator alone runs it.
spf02case2 :: Case
spf02case2 = Case
  { cKey        = CaseKey (SPF 2) (Just 2) Nothing Nothing
  , cSection    = SpecificFunctionality
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedSimOnly]
  , cRun        = case2
  , cExtensions = [("the 1024 bytes read are the DataStore's rows 0 to 1023 as the simulator's state holds them (Core 5.3.3.6.2.1, PDF 201)", dataStoreRows)]
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
  after <- (>>= fmap cvSync . L.find ((== base) . cvComId)) <$> peekSim ComIds
  unless (after == Just S1) (unmet ("after the IF-SEND the ComID is in " <> maybe "no state" tshow after <> ", not S1"))
  ifRecvOn base >>= expectT45 "the next IF-RECV" base AllReturnedNoData
  pure Pass


-- * Case 2

-- | Step 1 StartSession to the Locking SP as Admin1 (its PIN read from the
--   simulator's state, 'asAuthority'), steps 2 and 3 in the session, step 4
--   CLOSE_SESSION ('withSession').
case2 :: Script Verdict
case2 = admin1 >>= \a -> withSession a $ \h -> step2 h >>= step3 h >> pure Pass

-- | StartSession's arguments for Admin1 of the Locking SP (Test Cases PDF
--   53).
admin1 :: Script StartArgs
admin1 = asAuthority "Admin1" uSP_Locking LockingSP.uAuthority_Admin1

-- | The transfer length of step 2's IF-RECV: one 512-byte unit in either
--   unit, for the Test Cases' transfer length of 1 (PDF 53; design 11 M5b,
--   D11).
shortLength :: Word32
shortLength = 512

-- | Step 2: Get on the DataStore of rows 0 to 1023, the IF-SEND alone
--   ('sendInSession'), then an IF-RECV of one 512-byte unit ('shortRecv'),
--   answered with Table 45's "Response ready, insufficient transfer length
--   request" (Core Table 45 row 2, PDF 83): (0, all available, MinTransfer),
--   observed; the MinTransfer, for step 3.
step2 :: SessionHandle -> Script Integer
step2 h = do
  sent <- sendInSession h (invocation dataStoreTable (Methods.Get (cellBlockValue (byteRows 0 1023))))
  expectStatus (name <> ", the IF-SEND") Good sent
  r <- shortRecv (shComId h)
  expectT45 (name <> ", the IF-RECV of " <> tshow shortLength <> " bytes") (shComId h) InsufficientLength r
  (_, total, least) <- either (unmet . ((name <> ": ") <>)) pure (t45On (shComId h) r)
  observe "step 2" ("the IF-RECV of " <> tshow shortLength <> " bytes answered (0, " <> tshow total <> ", " <> tshow least
                    <> "): response ready, insufficient transfer length request (Core Table 45, PDF 83)")
  pure least
  where name = "step 2: Get on the DataStore, rows 0 to 1023"

-- | An IF-RECV of 'shortLength' on the ComID, asked again after the host's
--   poll interval while the TPer answers responses to come, (0, 1, 0) (Test
--   Cases 4.1 a, PDF 26: the Test Suite re-issues an IF-RECV while
--   OutstandingData is 1; a response the simulator holds,
--   @--hold-processing@), as many times as the host's timeout holds polls
--   (the run's @roExchange@); the last answer.
shortRecv :: Word16 -> Script Response
shortRecv c = asks (roExchange . reOpts) >>= \x -> go x (0 :: Int)
  where
    go x n = do
      r <- ifRecvWith c shortLength
      expectStatus ("step 2: the IF-RECV of " <> tshow shortLength <> " bytes") Good r
      let toCome = either (const False) ((== Just ResponsesToCome) . t45RowOf) (t45On c r)
      if toCome && n < xTimeoutMicros x `div` max 1 (xPollMicros x)
        then threadDelay (xPollMicros x) >> go x (n + 1)
        else pure r

-- | Step 3: an IF-RECV of the MinTransfer step 2's answer gave, which
--   returns the Get's result, Table 45's "Response, all returned, no further
--   data" (row 5, PDF 83): one method response, SUCCESS, its one result a
--   byte string of the 1024 rows (Core 5.3.3.6.2.1, PDF 201), observed.
step3 :: SessionHandle -> Integer -> Script ()
step3 h least = do
  r <- ifRecvWith (shComId h) (fromInteger least)
  expectT45 name (shComId h) AllReturnedWithData r
  packets <- either (unmet . ((name <> ": ") <>)) pure (framesOf (shComId h) r)
  items <- either (unmet . ((name <> ": ") <>)) pure (returnedOf packets)
  bytes <- case [ m | ReturnedResults m <- items ] of
    [MethodResponse results st]
      | StatusCode st /= SUCCESS -> unmet (name <> ": the Get answered " <> statusName (StatusCode st) <> ", not " <> statusName SUCCESS)
      | otherwise                -> either (unmet . ((name <> ": ") <>)) pure (byteString results)
    ms -> unmet (name <> ": " <> tshow (length ms) <> " method response(s) in the answer, not one")
  observe "step 3" ("the IF-RECV of " <> tshow least <> " bytes returned the Get's result, " <> tshow (B.length bytes)
                    <> " bytes: rows 0 to 1023 of the DataStore (Core 5.3.3.6.2.1, PDF 201)")
  where name = "step 3: the IF-RECV at step 2's MinTransfer"

-- | The rows a Get on the DataStore returns: one result, a byte string of
--   the 1024 rows asked, the lowest first (Core 5.3.3.6.2.1, PDF 201); or
--   why not.
--
-- >>> fmap B.length (byteString [D (Datum (Bytes (B.replicate 1024 0)))])
-- Right 1024
-- >>> byteString [D (Datum (Bytes "ETC-14"))]
-- Left "the Get's result is a byte string of 6 bytes, not 1024"
-- >>> byteString []
-- Left "the Get's result is not one byte string: 0 result(s)"
byteString :: [Value] -> Either Text ByteString
byteString = \case
  [D (Datum (Bytes bytes))]
    | B.length bytes == rows -> Right bytes
    | otherwise              -> Left ("the Get's result is a byte string of " <> tshow (B.length bytes) <> " bytes, not " <> tshow rows)
  results -> Left ("the Get's result is not one byte string: " <> tshow (length results) <> " result(s)")
  where rows = 1024 :: Int

-- | The extension: the same rows read in a whole exchange ('getCells', the
--   host's loop asking again at the MinTransfer) are the DataStore's as the
--   simulator's state holds it ("Hsed.Sim.Bytes".'byteRange' of its byte
--   table, Opal 2.30 Table 36, PDF 54).
dataStoreRows :: Script Verdict
dataStoreRows = peekSim (ByteTableOf uSP_Locking dataStoreTable) >>= \case
  Nothing -> pure (Skipped "the simulator's alone")
  Just held -> do
    table <- maybe (unmet "the simulator's Locking SP holds no DataStore byte table (Opal 2.30 Table 36, PDF 54)") pure held
    a <- admin1
    withSession a $ \h -> do
      outcome <- getCells h dataStoreTable (byteRows 0 1023) >>= exchanged name
      expectMethodStatus name SUCCESS outcome
      bytes <- either (unmet . ((name <> ": ") <>)) pure (byteString (moResults outcome))
      unless (bytes == byteRange 0 1023 table) $
        unmet (name <> ": the " <> tshow (B.length bytes) <> " bytes read differ from the DataStore's rows 0 to 1023 in the simulator's state")
      observe "DataStore" ("rows 0 to 1023 read in a whole exchange equal the simulator's, " <> tshow (B.length bytes)
                           <> " bytes of a table of " <> tshow (btSize table) <> " rows")
      pure Pass
  where name = "Get on the DataStore, rows 0 to 1023, in a whole exchange"
