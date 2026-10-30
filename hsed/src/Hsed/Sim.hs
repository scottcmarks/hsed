{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim
Description : A step of the simulated TPer: routing by protocol and ComID, the units, the synchronous protocol
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The simulator's one function (design 8.2): an input and a TPer to an output
and the TPer after it. A command is first held to its unit (SIIS 1.20
4.3.1.1-4.3.2.2, PDF 30-31), then routed: protocol 1, ComID 1 to Level 0
("Hsed.Sim.Level0"), outside the synchronous protocol; protocol 1 on a
static ComID to its synchronous protocol ("Hsed.Sim.Sync"), the Session
Manager answering the control session ("Hsed.Sim.SessionManager") and the
sessions their packets ("Hsed.Sim.Session"); an inactive or unsupported
ComID as Core 3.3.3.2-3 says (PDF 58-59); protocol 2, ComID management, to
"Hsed.Sim.ComIdMgmt"; any other protocol refused. An event resets the TPer
("Hsed.Sim.Reset"); a 'Tick' releases a ComID's prepared response (S2 to
S3).

Which ComID is Level 0's and which are reserved is Core Table 26's
("Hsed.Gen.Core.Codes".@cComIDAssignments@, PDF 55); a command answered
other than Good carries no bytes.
-}
module Hsed.Sim
  ( step
  , syncOf
  ) where

import qualified RIO.ByteString          as B
import qualified RIO.Map                 as Map
import           RIO

import           Hsed.Core.Code          (CodeRow (..), codeRowsNamed, inSpan)
import           Hsed.Core.Record        (encodeRecord)
import           Hsed.Gen.Core.Codes     (cComIDAssignments)
import           Hsed.Gen.Core.Layouts   (ComPacket (..), ComPacketHeader (..))
import qualified Hsed.Sim.ComIdMgmt      as ComIdMgmt
import           Hsed.Sim.Level0         (level0Response)
import qualified Hsed.Sim.Reset          as Reset
import           Hsed.Sim.Sync           (classifyIfSend, ifRecvStatic)
import           Hsed.Sim.Types          (ComIdLife (..), ComIdState (..), Input (..), Limits (..), Outcome (..), Output (..),
                                          SimParams (..), SyncState (..), TPerState (..), reservedComId, tperLimits)
import           Hsed.Wire               (Dir (..), IfStatus (..), Request (..), Response (..), Unit (..),
                                          comIdManagementProtocol, comPacketProtocol, exactly, inactiveExtension, roundUp)


-- | One step of the TPer:
--
-- * a command, answered as the module's description says;
-- * an event (a power cycle, a hardware reset, a hot plug): what
--   "Hsed.Sim.Reset".'Reset.event' does to the TPer, Good;
-- * a 'Tick' of a ComID: its prepared response released, S2 to S3 (S2:S3,
--   PDF 86); any other state unchanged.
step :: Input -> TPerState -> (Output, TPerState)
step input st = case input of
  Cmd r  -> let (resp, st') = command r st in (Out resp, st')
  Ev e   -> (Evented Good, Reset.event e st)
  Tick c -> (Ticked, st { tsComIds = Map.adjust released c (tsComIds st) })
  where
    released cs = case cSync cs of
      S2 ps -> cs { cSync = S3 ps }
      _     -> cs

-- | A static ComID's synchronous protocol state, if the TPer has the ComID.
syncOf :: Word16 -> TPerState -> Maybe SyncState
syncOf c = fmap cSync . Map.lookup c . tsComIds

-- | A command's answer and the TPer after it. The unit first: in 512-byte
--   blocks, a transfer length of none or of part of a block is Other Invalid
--   Command Parameter (SIIS 1.20 4.3.1.2 and 4.3.2.2, PDF 30-31; the blocks
--   count it, design 6.2). Then the protocol (SIIS 1.20 Tables 12-13, PDF
--   30-31; Core Table 25, PDF 53): 0x01 the ComPackets and Level 0; 0x02,
--   ComID management, outside the synchronous protocol (Core 3.3.10.3 b,
--   PDF 84; "Hsed.Sim.ComIdMgmt"); 0x00, the security protocol information,
--   which IF-RECV alone has (SIIS 1.20 Tables 12 and 23, PDF 30 and 52: it
--   is not defined for IF-SEND, which an Opal TPer takes on 0x01 and 0x02
--   alone, Opal 2.30 3.3.2, PDF 29-30), so an IF-SEND on it an Invalid
--   Security Protocol ID (Core 3.3.2, PDF 54) and an IF-RECV not simulated;
--   any other an Invalid Security Protocol ID.
command :: Request -> TPerState -> (Response, TPerState)
command r st
  | not (whole (spUnits (tsParams st)) (lenBytes r)) = refused OtherInvalidCommandParameter
  | proto r == comPacketProtocol                   = comPacketCommand r st
  | proto r == comIdManagementProtocol             = ComIdMgmt.command r st
  | otherwise = case proto r of
      0x00 | dir r == IfSend -> refused InvalidSecurityProtocolId
           | otherwise       -> refused (Unsupported "protocol 0x00, the security protocol information: not simulated")
      _    -> refused InvalidSecurityProtocolId
  where
    refused s = (Response s mempty, st)
    whole = \case
      UnitBlocks512 -> \len -> len /= 0 && roundUp UnitBlocks512 len == len
      UnitBytes     -> const True

-- | A command of protocol 1, by its ComID:
--
-- * Level 0's (Core Table 26): IF-SEND accepted and its payload discarded
--   (Core 3.3.6.1, PDF 67), IF-RECV the Level 0 response (3.3.6.2, PDF 67),
--   neither touching the synchronous protocol (3.3.10.3 b, PDF 84);
-- * a static ComID of the TPer's, not inactive: its synchronous protocol,
--   an accepted IF-SEND's payload classified with the TPer's state
--   ("Hsed.Sim.Sync".'classifyIfSend'), which the Session Manager and the
--   sessions may change;
-- * any other reserved ComID (Table 26's, PDF 55): Other Invalid Command
--   Parameter, either way (3.3.3.2-3, PDF 58-59);
-- * an inactive non-reserved one: Other Invalid Command Parameter, or, if
--   the run discards what is sent to inactive ComIDs
--   ('spInactiveComIdDiscard'), IF-SEND accepted and its payload discarded,
--   IF-RECV a ComPacket header alone, the ComID's with extension 0xFFFF,
--   OutstandingData, MinTransfer and Length zero (3.3.3.2 b and 3.3.3.3 b,
--   PDF 58-59).
comPacketCommand :: Request -> TPerState -> (Response, TPerState)
comPacketCommand r st
  | level0 c = case dir r of
      IfSend -> (Response Good mempty, st)
      IfRecv -> (Response Good (level0Response st (lenBytes r)), st)
  | Just cs <- Map.lookup c (tsComIds st), cLife cs /= Inactive = static cs
  | reservedComId c = (Response OtherInvalidCommandParameter mempty, st)
  | spInactiveComIdDiscard p = case dir r of
      IfSend -> (Response Good mempty, st)
      IfRecv -> (Response Good (exactly (lenBytes r) (inactiveHeader c)), st)
  | otherwise = (Response OtherInvalidCommandParameter mempty, st)
  where
    c = comId r
    p = tsParams st
    static cs = case dir r of
      IfRecv -> let (resp, cs') = ifRecvStatic p c (lenBytes r) cs
                in (resp, st { tsComIds = Map.insert c cs' (tsComIds st) })
      IfSend -> case cSync cs of
        S1 | fromIntegral (lenBytes r) > lComPacketSize (tperLimits p)
                -> (Response InvalidTransferLengthOnIfSend mempty, st)
           | otherwise
                -> let (outcome, st') = classifyIfSend c (B.take (fromIntegral (lenBytes r)) (payload r)) st
                   in (Response Good mempty, st' { tsComIds = Map.adjust (\cs' -> cs' { cSync = after outcome }) c (tsComIds st') })
        _       -> (Response SynchronousProtocolViolation mempty, st)
    -- Prepared: processing (S1:S2, PDF 85); a session aborted with results completed or its CloseSession prepared
    -- likewise (3.2.2.4.2 item 1, PDF 39; 3.3.10.7 b, PDF 86); otherwise Awaiting IF-SEND kept, decided at the
    -- IF-SEND (design 8.2)
    after = \case
      Prepared ps | not (null ps) -> S2 ps
      SessionAborted results closing
        | ps@(_ : _) <- results <> maybeToList closing -> S2 ps
      _                           -> S1

-- | Whether Core Table 26 assigns a ComID to Level 0 Device Discovery (PDF
--   55).
level0 :: Word16 -> Bool
level0 c = any (\row -> inSpan (codeSpan row) (toInteger c)) (codeRowsNamed cComIDAssignments "Level 0 Device Discovery")

-- | The answer to IF-RECV on an inactive ComID: a ComPacket header alone,
--   the ComID's, its extension 0xFFFF and the rest zero (Core 3.3.3.3 a.i,
--   PDF 58).
inactiveHeader :: Word16 -> ByteString
inactiveHeader c = either (const mempty) id (encodeRecord (ComPacket (ComPacketHeader c inactiveExtension 0 0) mempty))
