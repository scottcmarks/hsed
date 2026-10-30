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
Manager answering the control session ("Hsed.Sim.SessionManager"); an
inactive or unsupported ComID as Core 3.3.3.2-3 says (PDF 58-59); protocol 2
at M4a; any other protocol refused. An event resets the ComIDs; a 'Tick'
releases a ComID's prepared response (S2 to S3).

Which ComID is Level 0's and which are reserved is Core Table 26's
("Hsed.Gen.Core.Codes".@cComIDAssignments@, PDF 55); a command answered
other than Good carries no bytes.
-}
module Hsed.Sim
  ( step
  , syncOf
  ) where

import qualified RIO.ByteString          as B
import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import qualified RIO.Text                as T
import           RIO

import           Hsed.Core.Code          (CodeRow (..), codeRow, codeRowsNamed, inSpan)
import           Hsed.Core.Record        (encodeRecord)
import           Hsed.Gen.Core.Codes     (cComIDAssignments)
import           Hsed.Gen.Core.Layouts   (ComPacket (..), ComPacketHeader (..))
import           Hsed.Sim.Level0         (level0Response)
import           Hsed.Sim.SessionManager (answer)
import           Hsed.Sim.Sync           (classifyIfSend, ifRecvStatic)
import           Hsed.Sim.Types          (ComIdLife (..), ComIdState (..), Input (..), Outcome (..), Output (..), SimParams (..),
                                          SyncState (..), TPerState (..))
import           Hsed.Wire               (Dir (..), IfStatus (..), Request (..), Response (..), Unit (..), comPacketProtocol,
                                          exactly, inactiveExtension, roundUp)


-- | One step of the TPer:
--
-- * a command, answered as the module's description says;
-- * an event (a power cycle, a hardware reset, a hot plug): every ComID
--   back to Awaiting IF-SEND, its prepared response dropped (Core 3.3.10.6
--   S2:S1 a and S3:S1 a, PDF 85-86), Good. The rest of the reset rules
--   (sessions, the Locking SP's) are M5's (design 8.4);
-- * a 'Tick' of a ComID: its prepared response released, S2 to S3 (S2:S3,
--   PDF 86); any other state unchanged.
step :: Input -> TPerState -> (Output, TPerState)
step input st = case input of
  Cmd r  -> let (resp, st') = command r st in (Out resp, st')
  Ev _   -> (Evented Good, st { tsComIds = fmap (\cs -> cs { cSync = S1 }) (tsComIds st) })
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
--   ComID management, not simulated before M4a; 0x00, the security protocol
--   information, not simulated; any other an Invalid Security Protocol ID.
command :: Request -> TPerState -> (Response, TPerState)
command r st
  | not (whole (spUnits (tsParams st)) (lenBytes r)) = refused OtherInvalidCommandParameter
  | proto r == comPacketProtocol = comPacketCommand r st
  | otherwise = case proto r of
      0x02 -> refused (Unsupported "protocol 0x02, ComID management (TPER_RESET, STACK_RESET): M4a")
      0x00 -> refused (Unsupported "protocol 0x00, the security protocol information: not simulated")
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
-- * a static ComID of the TPer's, not inactive: its synchronous protocol;
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
  | Just cs <- Map.lookup c (tsComIds st), cLife cs /= Inactive =
      let (resp, cs') = static cs in (resp, st { tsComIds = Map.insert c cs' (tsComIds st) })
  | reserved c = (Response OtherInvalidCommandParameter mempty, st)
  | spInactiveComIdDiscard p = case dir r of
      IfSend -> (Response Good mempty, st)
      IfRecv -> (Response Good (exactly (lenBytes r) (inactiveHeader c)), st)
  | otherwise = (Response OtherInvalidCommandParameter mempty, st)
  where
    c = comId r
    p = tsParams st
    static cs = case dir r of
      IfRecv -> ifRecvStatic c (lenBytes r) cs
      IfSend -> case cSync cs of
        S1 | maybe False (toInteger (lenBytes r) >) (L.lookup "MaxComPacketSize" (spTPerProperties p))
                -> (Response InvalidTransferLengthOnIfSend mempty, cs)
           | otherwise
                -> (Response Good mempty, cs { cSync = after (classifyIfSend (answer p) c (B.take (fromIntegral (lenBytes r)) (payload r))) })
        _       -> (Response SynchronousProtocolViolation mempty, cs)
    -- Prepared: processing (S1:S2, PDF 85); a session aborted with its CloseSession prepared likewise (3.3.10.7 b,
    -- PDF 86); otherwise Awaiting IF-SEND kept, decided at the IF-SEND (design 8.2)
    after = \case
      Prepared ps | not (null ps)   -> S2 ps
      SessionAborted (Just closing) -> S2 [closing]
      _                             -> S1

-- | Whether Core Table 26 assigns a ComID to Level 0 Device Discovery (PDF
--   55).
level0 :: Word16 -> Bool
level0 c = any (\row -> inSpan (codeSpan row) (toInteger c)) (codeRowsNamed cComIDAssignments "Level 0 Device Discovery")

-- | Whether a ComID is reserved: Core Table 26 (PDF 55) calls the others,
--   for ComID management, non-reserved.
reserved :: Word16 -> Bool
reserved c = not (any ("non-reserved" `T.isInfixOf`) (maybe [] codeCells (codeRow cComIDAssignments (toInteger c))))

-- | The answer to IF-RECV on an inactive ComID: a ComPacket header alone,
--   the ComID's, its extension 0xFFFF and the rest zero (Core 3.3.3.3 a.i,
--   PDF 58).
inactiveHeader :: Word16 -> ByteString
inactiveHeader c = either (const mempty) id (encodeRecord (ComPacket (ComPacketHeader c inactiveExtension 0 0) mempty))
