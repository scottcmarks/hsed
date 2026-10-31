{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Sim.ComIdMgmt
Description : ComID management, protocol 0x02: TPER_RESET, GET_COMID, HANDLE_COMID_REQUEST and GET_COMID_RESPONSE
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The commands of security protocol 0x02 ("Hsed.Wire".'Hsed.Wire.comIdManagementProtocol'),
outside the synchronous protocol (Core 3.3.10.3 b, PDF 84: a ComID in any
synchronous state takes them), routed here by "Hsed.Sim" by their ComID:

* TPER_RESET, an IF-SEND on ComID 0x0004 ("Hsed.OpalUIDs".'tperResetComId';
  Opal 2.30 3.2.3 and Table 13, PDF 28-29; Table 14, PDF 30): with the
  command enabled (TPerInfo's ProgrammaticResetEnable,
  "Hsed.Sim.Params".@spTPerResetEnabled@, FALSE at the factory, Opal Table
  27, PDF 50) and a transfer length other than zero, accepted at the
  interface, Good, its data ignored, and the TPer reset
  ("Hsed.Sim.Reset".'Reset.resetTPer' 'Reset.OnTPerReset'); disabled, or of
  no length, aborted with Other Invalid Command Parameter. There is no
  IF-RECV response to it: an IF-RECV on 0x0004 is Other Invalid Command
  Parameter too (Opal 3.2.3: the ComID is the command's alone; Core 3.3.3.3,
  PDF 59, a reserved ComID the TPer does not support for it).
* GET_COMID, an IF-RECV on ComID 0x0000 (Core 3.3.4.3 and Tables 27-28,
  PDF 60-61): the simulator assigns no ComID dynamically, so the Extended
  ComID is all zeros (3.3.4.3.1 a), cut or padded to the transfer length;
  an IF-SEND on 0x0000, a reserved ComID with no command, is Other Invalid
  Command Parameter (3.3.3.2, PDF 58).
* On a static ComID, HANDLE_COMID_REQUEST (an IF-SEND, 3.3.4.7.1 and Table
  29, PDF 62) and GET_COMID_RESPONSE (an IF-RECV, 3.3.4.7.2 and Table 30,
  PDF 62-63), their payloads the generated layouts of Tables 28-37
  ("Hsed.Gen.Core.Layouts"), the response kept in the ComID's management
  buffer ("Hsed.Sim.Types".'MgmtBuffer') for the next GET_COMID_RESPONSE,
  consecutive ones answering the last request (3.3.4.7, PDF 62):

    * a request of fewer than 8 bytes, Invalid Transfer Length parameter on
      IF-SEND (3.3.4.7.5, PDF 65), nothing changed;
    * a request whose Extended ComID is not the ComID's (the static ComID
      with extension 0x0000, 3.3.3.1, PDF 57), or whose request code is
      neither VERIFY_COMID_VALID's nor STACK_RESET's (an error in the
      payload, PDF 66 b), is answered No Response Available (Table 31, PDF
      63) with the ComID's Extended ComID;
    * VERIFY_COMID_VALID (Table 32, PDF 63-64): the response of Table 33
      (PDF 64) with the ComID's state as "Hsed.Sim.State".'lifeOf' gives it
      (Issued 2, Associated 3) and its three times all zeros, the simulator
      having no clock; kept until the next request;
    * STACK_RESET (Table 35, PDF 65): the ComID's stack reset
      ("Hsed.Sim.Reset".'Reset.stackReset') and the response of Table 36
      (PDF 66), Success, staged; cleared once the host retrieves it entire
      (PDF 66 a), or at the next request (c), or by a hard reset or power
      cycle (b, "Hsed.Sim.Reset");
    * GET_COMID_RESPONSE: the response staged, or No Response Available, its
      Extended ComID zeros when no request was ever made on the ComID (PDF
      66), cut or padded to the transfer length (3.3.4.7.2, PDF 63).

* Any other ComID as Core 3.3.3.2-3 says (PDF 58-59), as "Hsed.Sim" answers
  protocol 1: a reserved ComID the TPer does not support, Other Invalid
  Command Parameter; an inactive one the same, or, if the run discards what
  is sent to inactive ComIDs ('spInactiveComIdDiscard'), an IF-SEND accepted
  and discarded and an IF-RECV answered No Response Available with the
  inactive extension 0xFFFF (3.3.3.3 a.i's indication, in this protocol's
  payload).

A STACK_RESET is complete before its command returns, so the Pending
response (Table 37) is never staged, and a STACK_RESET never fails.
-}
module Hsed.Sim.ComIdMgmt
  ( command
  ) where

import           Data.Bits             (shiftL, (.|.))
import qualified RIO.ByteString        as B
import qualified RIO.Map               as Map
import           RIO

import           Hsed.Core.Record      (DecodedRecord (..), LayoutRecord, decodeRecordStrict, encodeRecord)
import           Hsed.Gen.Core.Layouts (GET_COMIDPayload (..), NoResponseAvailable (..), STACK_RESETCommandRequest (..),
                                        STACK_RESETCommandResponse (..), VERIFY_COMID_VALIDCommandResponse (..),
                                        VERIFY_COMID_VALIDRequest (..))
import           Hsed.OpalUIDs         (tperResetComId)
import qualified Hsed.Sim.Reset        as Reset
import           Hsed.Sim.State        (lifeOf)
import           Hsed.Sim.Types        (ComIdLife (..), ComIdState (..), MgmtBuffer (..), SimParams (..), TPerState (..),
                                        reservedComId)
import           Hsed.Wire             (Dir (..), IfStatus (..), Request (..), Response (..), exactly, inactiveExtension)


-- | A command of protocol 0x02 answered, and the TPer after it, as the
--   module's description says.
command :: Request -> TPerState -> (Response, TPerState)
command r st
  | c == tperResetComId = tperReset
  | c == getComIdComId  = getComId
  | Just cs <- Map.lookup c (tsComIds st), cLife cs /= Inactive = static cs
  | reservedComId c     = refused OtherInvalidCommandParameter
  | spInactiveComIdDiscard p = case dir r of
      IfSend -> (Response Good mempty, st)
      IfRecv -> (Response Good (exactly (lenBytes r) (noResponse (extended c inactiveExtension))), st)
  | otherwise = refused OtherInvalidCommandParameter
  where
    c = comId r
    p = tsParams st
    refused s = (Response s mempty, st)
    -- TPER_RESET: enabled and of a length, Good and the TPer reset; else aborted at the interface (Opal 3.2.3)
    tperReset = case dir r of
      IfSend | spTPerResetEnabled p && lenBytes r /= 0 -> (Response Good mempty, Reset.resetTPer Reset.OnTPerReset st)
      _                                                -> refused OtherInvalidCommandParameter
    -- GET_COMID: no ComID to assign, all zeros (3.3.4.3.1 a)
    getComId = case dir r of
      IfRecv -> (Response Good (exactly (lenBytes r) (encoded (GET_COMIDPayload 0))), st)
      IfSend -> refused OtherInvalidCommandParameter
    -- a static ComID: HANDLE_COMID_REQUEST or GET_COMID_RESPONSE
    static cs = case dir r of
      IfSend -> handleRequest cs (B.take (fromIntegral (lenBytes r)) (payload r))
      IfRecv -> let (bytes, cs') = response cs in (Response Good (exactly (lenBytes r) bytes), kept cs')
    kept cs' = st { tsComIds = Map.insert c cs' (tsComIds st) }
    own cs   = extended c (cExt cs)
    -- HANDLE_COMID_REQUEST: the request's code by the pins of its layout (Tables 32 and 35), its Extended ComID
    -- the ComID's; a wrong one or an unknown code leaves No Response Available (an error in the payload, PDF 66 b)
    handleRequest cs bytes
      | B.length bytes < 8 = refused InvalidTransferLengthOnIfSend
      | Just ext <- requested @VERIFY_COMID_VALIDRequest verify_COMID_VALIDRequestExtendedComID bytes, ext == own cs =
          (Response Good mempty, kept cs { cMgmt = MgmtBuffer True (Just (verifyCode, verifyResponse cs)) })
      | Just ext <- requested @STACK_RESETCommandRequest stack_RESETCommandRequestExtendedComID bytes, ext == own cs =
          let st' = Reset.stackReset c st
          in (Response Good mempty, st' { tsComIds = Map.adjust (\cs' -> cs' { cMgmt = MgmtBuffer True (Just (stackResetCode, stackResetResponse cs')) }) c (tsComIds st') })
      | otherwise = (Response Good mempty, kept cs { cMgmt = MgmtBuffer True Nothing })
    -- the Extended ComID a request of a layout carries, if the payload holds that layout's pins (its request code)
    requested :: forall a. LayoutRecord a => (a -> Word32) -> ByteString -> Maybe Word32
    requested ext bytes = either (const Nothing) (Just . ext . drRecord) (decodeRecordStrict @a bytes)
    -- VERIFY_COMID_VALID's response (Table 33): the state as lifeOf gives it, the times zero (no clock, PDF 64)
    verifyResponse cs = encoded VERIFY_COMID_VALIDCommandResponse
      { verify_COMID_VALIDCommandResponseExtendedComID                                     = own cs
      , verify_COMID_VALIDCommandResponseCurrentStateOfExtendedComID                       = stateCode (lifeOf c st)
      , verify_COMID_VALIDCommandResponseAbsoluteTimeOfAllocationRelativeToLastResetOfTPer = noTime
      , verify_COMID_VALIDCommandResponseAbsoluteTimeOfExpiryRelativeToLastResetOfTPer     = noTime
      , verify_COMID_VALIDCommandResponseTimeSinceLastResetOfTPer                          = noTime
      }
    -- STACK_RESET's response (Table 36): Success
    stackResetResponse cs = encoded (STACK_RESETCommandResponse (own cs) stackResetSuccess)
    -- GET_COMID_RESPONSE: the response staged, a STACK_RESET's cleared once returned entire (PDF 66 a), a
    -- VERIFY_COMID_VALID's kept (3.3.4.7: consecutive commands return the last request's); else No Response
    -- Available, its Extended ComID the ComID's once a request was made, zeros before any (PDF 66)
    response cs = case mbResponse (cMgmt cs) of
      Just (code, bytes)
        | code == stackResetCode && fromIntegral (lenBytes r) >= B.length bytes -> (bytes, cs { cMgmt = (cMgmt cs) { mbResponse = Nothing } })
        | otherwise                                                              -> (bytes, cs)
      Nothing -> (noResponse (if mbRequested (cMgmt cs) then own cs else 0), cs)

-- | GET_COMID's ComID: 0x0000, as its command block gives it (Core Table 27,
--   PDF 61), the ComID Table 26 reserves (PDF 55).
getComIdComId :: Word16
getComIdComId = 0x0000

-- | An Extended ComID: the ComID, then its extension (Core 3.3.3.1, PDF 57).
extended :: Word16 -> Word16 -> Word32
extended c ext = (fromIntegral c `shiftL` 16) .|. fromIntegral ext

-- | The request codes of Core Table 32 and Table 35, as their generated
--   layouts pin them (bytes 4 to 7 of a request): what the management buffer
--   records a response as answering.
verifyCode, stackResetCode :: Word32
verifyCode     = pinnedRequestCode (VERIFY_COMID_VALIDRequest 0)
stackResetCode = pinnedRequestCode (STACK_RESETCommandRequest 0)

-- | The Request Code a request layout pins, read back from its encoding
--   (bytes 4 to 7, Tables 32 and 35).
pinnedRequestCode :: LayoutRecord a => a -> Word32
pinnedRequestCode = B.foldl' (\n b -> (n `shiftL` 8) .|. fromIntegral b) 0 . B.take 4 . B.drop 4 . encoded

-- | Success in a STACK_RESET response (Core Table 36, PDF 66: 00 00 00 00;
--   Failure 00 00 00 01, which the simulator never reports).
stackResetSuccess :: Word32
stackResetSuccess = 0

-- | A ComID's state as Core Table 33 codes it (PDF 64): 0 Invalid, 1
--   Inactive, 2 Issued, 3 Associated.
stateCode :: Maybe ComIdLife -> Word32
stateCode = maybe 0 $ \life -> case life of
  Inactive   -> 1
  Issued     -> 2
  Associated -> 3

-- | A time of Core Table 33 with no clock: ten bytes of zeros (PDF 64).
noTime :: ByteString
noTime = B.replicate 10 0

-- | No Response Available (Core Table 31, PDF 63) for an Extended ComID.
noResponse :: Word32 -> ByteString
noResponse = encoded . NoResponseAvailable

-- | A management payload's bytes: its layout's fixed part (the reserved run
--   to the end of the transfer is the padding 'exactly' adds). Every layout
--   of Tables 28-37 tiles, so a record always encodes; were one not to, the
--   response would be empty, and the test over them says so.
encoded :: LayoutRecord a => a -> ByteString
encoded = either (const mempty) id . encodeRecord
