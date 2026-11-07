{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cases.ETC14
Description : ETC-14, TPer properties exceeded in a regular session: a packet of MaxSubpackets + 1 subpackets
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-14 (Test Cases 1.02, PDF 87): Properties for MaxSubpackets, n,
StartSession to the Locking SP as Admin1, a packet of the session with n +
1 subpackets, each a Set on the DataStore table, which aborts the session
(Core 5.2.2.4.1.1, PDF 145), the IF-RECV answering "All Response(s)
returned - no further data" or a ComPacket with a CloseSession (PDF 87;
Test Cases 7.1, PDF 76), the session then forgotten by the run; then a new
session to the Locking SP as Admin1, and CLOSE_SESSION. As ETC-15 decides
for the control session (PDF 88), a MaxSubpackets of 0, no limit (Core
Table 167), leaves the case not applicable, skipped (design 13.29 (b)), as
does an n so large that the packet would not fit the TPer's MaxPacketSize
or its ComPacket its MaxComPacketSize (Core 3.3.10.7 c, PDF 86; 5.2.2.4.1.2,
PDF 145: the IF-SEND would fail for its length, not for its subpackets).
Its sessions need the Locking SP Manufactured, the common prerequisite of
7.2 (PDF 76); its packet is an IF-SEND off a drive's allow list carrying
Sets, writes no drive takes before M6, so the simulator alone runs it
(design 9.3; 13.34 (8)).

The Sets never run: the packet past the limit is discarded whole (Core
5.2.2.4.1.1, PDF 145), so a Set of bytes into the DataStore, which the
simulator's Set does not carry out before M6 (Scott's answer Q3), is never
asked of it. The jig's extension reads the simulator's state: the
DataStore holds nothing written (Core 3.3.7.2, PDF 77: a method that cannot
be processed completely has no effect).
-}
module Hsed.Jig.Cases.ETC14
  ( etc14
  ) where

import qualified Data.IntMap.Strict as IM
import qualified RIO.ByteString     as B
import qualified RIO.List           as L
import qualified RIO.Map            as Map
import           RIO

import           Hsed.Core.Method   (invocation)
import           Hsed.Core.StreamItem (StreamItem (..))
import           Hsed.Core.Token    (Token (..))
import           Hsed.Core.Value    (Datum (..), Value (..))
import qualified Hsed.Gen.Core.Methods as Methods
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Case      (Case (..), Need (..))
import           Hsed.Jig.Catalogue (CaseKey (..), TestId (..))
import           Hsed.Jig.Prereq    (Section (..))
import           Hsed.Jig.Script    (Script, SessionHandle (..), StartArgs, Verdict (..), asAuthority, attempt, baseComId, cleared,
                                     dataStoreTable, exchangeOn, exchanged, expectNoResponseOrCloseSession, observe, opened,
                                     peekSim, properties, sessionGone, sessionSubpackets, startSession, unmet,
                                     withSession)
import           Hsed.Sim.Types     (ByteTable (..), SPState (..), TPerState (..), maxComPacketSizeName, maxPacketSizeName,
                                     maxSubpacketsName)
import           Hsed.Wire          (comPacketHeaderSize)


-- $setup
-- >>> :set -XOverloadedStrings

-- | The case.
etc14 :: Case
etc14 = Case
  { cKey        = CaseKey (ETC 14) Nothing Nothing Nothing
  , cSection    = ErrorCases
  , cWaives     = []
  , cOwnPrereqs = []
  , cNeeds      = [NeedIfRecv, NeedWrite, NeedErrorPath, NeedSimOnly]
  , cRun        = run
  , cExtensions = [("the DataStore is unchanged: nothing written in the simulator's state (Core 5.2.2.4.1.1, PDF 145; 3.3.7.2, PDF 77)", dataStoreUnchanged)]
  }

-- | Step 1 Properties for MaxSubpackets, n (the case not applicable where
--   it is 0, or where the packet of n + 1 subpackets would not be sent for
--   its length, 'tooLong', decided before any session opens); step 2
--   StartSession; step 3 the packet, after which the session is gone;
--   steps 4 and 5 a session opened and closed ('withSession'). A step 3
--   that failed may leave its session open: STACK_RESET clears it
--   ('cleared').
run :: Script Verdict
run = do
  tper <- properties >>= exchanged step1
  n <- maybe (unmet (step1 <> ": Properties reported no " <> maxSubpacketsName)) pure (L.lookup maxSubpacketsName tper)
  observe "step 1" (maxSubpacketsName <> " = " <> tshow n)
  if n <= 0
    then pure (Skipped (maxSubpacketsName <> " = " <> tshow n <> ": no limit (Core Table 167, PDF 141), which step 3 cannot exceed: not applicable "
                        <> "(NA; design 13.29 (b))"))
    else do
      base <- baseComId
      let count = fromInteger n + 1
          -- the packet's length alone, which its Session fields, zero here, do not change
          size  = either (const 0) B.length (sessionSubpackets (SessionHandle base 0 0) (L.replicate count setCall))
      case tooLong tper count size of
        Just why -> pure (Skipped why)
        Nothing  -> do
          a <- admin1
          h <- startSession a >>= exchanged step2 >>= opened step2
          v <- attempt (Pass <$ step3 h n count)
          case v of
            Pass  -> withSession a (\_ -> Pass <$ observe "steps 4-5" "StartSession to the Locking SP as Admin1 after the abort opened a session, closed by CLOSE_SESSION (Test Cases PDF 87)")
            other -> other <$ cleared h
  where
    step1 = "step 1: Properties"
    step2 = "step 2: StartSession to the Locking SP as Admin1"

-- | StartSession's arguments for Admin1 of the Locking SP (Test Cases PDF
--   87), its PIN read from the simulator's state.
admin1 :: Script StartArgs
admin1 = asAuthority "Admin1" uSP_Locking LockingSP.uAuthority_Admin1

-- | Set on the DataStore table of the bytes @ETC-14@, the Where omitted
--   (Core 5.3.3.7.1.2, PDF 202: the operation begins at the byte table's
--   first row), the Values Bytes (5.3.3.7.2.1, PDF 202-203), as tokens: the
--   invocation each subpacket of step 3 carries, which the TPer never runs.
setCall :: ByteString
setCall = generate (invocation dataStoreTable (Methods.Set Nothing (Just (D (Datum (Bytes "ETC-14"))))))

-- | Why the packet of the count of subpackets, of a length in bytes, would
--   not be sent, if it would not: its ComPacket longer than the TPer's
--   MaxComPacketSize, or its packet than MaxPacketSize, so that the IF-SEND
--   would fail for its length (Core 3.3.10.7 c, PDF 86; 5.2.2.4.1.2, PDF
--   145), not for its subpackets, as ETC-15 decides (design 13.29 (b)).
--
-- >>> tooLong [("MaxComPacketSize", 2048), ("MaxPacketSize", 2028)] 2 140
-- Nothing
-- >>> tooLong [("MaxComPacketSize", 2048), ("MaxPacketSize", 2028)] 65 2100
-- Just "step 3: 65 Set subpackets take 2100 bytes, more than MaxComPacketSize 2048: the IF-SEND would fail for its length (Core 3.3.10.7 c, PDF 86), not for its subpackets: not applicable"
-- >>> tooLong [("MaxPacketSize", 2028)] 65 2060
-- Just "step 3: the packet of 65 Set subpackets takes 2040 bytes, more than MaxPacketSize 2028: the packet would be discarded for its length (Core 5.2.2.4.1.2, PDF 145), not for its subpackets: not applicable"
tooLong :: [(Text, Integer)] -> Int -> Int -> Maybe Text
tooLong tper count size = listToMaybe $
  [ "step 3: " <> tshow count <> " Set subpackets take " <> tshow size <> " bytes, more than " <> maxComPacketSizeName <> " " <> tshow most
    <> ": the IF-SEND would fail for its length (Core 3.3.10.7 c, PDF 86), not for its subpackets: not applicable"
  | Just most <- [L.lookup maxComPacketSizeName tper], toInteger size > most ]
  <> [ "step 3: the packet of " <> tshow count <> " Set subpackets takes " <> tshow (size - comPacketHeaderSize) <> " bytes, more than "
       <> maxPacketSizeName <> " " <> tshow most <> ": the packet would be discarded for its length (Core 5.2.2.4.1.2, PDF 145), not for its "
       <> "subpackets: not applicable"
     | Just most <- [L.lookup maxPacketSizeName tper], toInteger (size - comPacketHeaderSize) > most ]

-- | Step 3: one packet of the session of the count of Set subpackets
--   ('sessionSubpackets'), the whole exchange answering nothing or the
--   TPer's CloseSession ('expectNoResponseOrCloseSession'), the session
--   aborted and forgotten ('sessionGone'), observed.
step3 :: SessionHandle -> Integer -> Int -> Script ()
step3 h n count = do
  bytes <- either (unmet . ((name <> ": the ComPacket does not encode: ") <>) . tshow) pure (sessionSubpackets h (L.replicate count setCall))
  exchangeOn (shComId h) bytes >>= expectNoResponseOrCloseSession name
  sessionGone h ("the session was aborted by a packet of " <> tshow count <> " subpackets, past " <> maxSubpacketsName <> " " <> tshow n
                 <> " (Core 5.2.2.4.1.1, PDF 145)")
  observe "step 3" (tshow count <> " Set subpackets sent in one packet of the session; the session was aborted: the IF-RECV answered (0, 0, 0) "
                    <> "or the TPer's CloseSession (Test Cases PDF 87; 7.1, PDF 76)")
  where name = "step 3: a packet of " <> tshow count <> " Set subpackets, " <> maxSubpacketsName <> " " <> tshow n

-- | The extension: the DataStore as the simulator's state holds it has
--   nothing written ("Hsed.Sim.Types".'ByteTable': its extents written),
--   none of step 3's Sets having run (Core 5.2.2.4.1.1, PDF 145; 3.3.7.2,
--   PDF 77).
dataStoreUnchanged :: Script Verdict
dataStoreUnchanged = peekSim >>= \case
  Nothing -> pure (Skipped "the simulator's alone")
  Just st -> do
    table <- maybe (unmet "the simulator's Locking SP holds no DataStore byte table (Opal 2.30 Table 36, PDF 54)") pure
                   (Map.lookup uSP_Locking (tsSPs st) >>= Map.lookup dataStoreTable . spsBytes)
    unless (IM.null (btWritten table)) $
      unmet ("the DataStore holds " <> tshow (IM.size (btWritten table)) <> " written extent(s) where none was expected: no Set of step 3's packet "
             <> "ran (Core 5.2.2.4.1.1, PDF 145; 3.3.7.2, PDF 77)")
    observe "DataStore" ("nothing written in a table of " <> tshow (btSize table) <> " rows: the packet past MaxSubpackets was discarded whole, "
                         <> "none of its Sets run (Core 5.2.2.4.1.1, PDF 145; 3.3.7.2, PDF 77)")
    pure Pass
