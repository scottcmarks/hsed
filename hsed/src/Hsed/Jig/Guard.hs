{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Jig.Guard
Description : What may reach a drive: every command checked against a policy before its transport sees it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The run-time half of the jig's safety against the attached drive (design
9.3.2): before a command reaches a drive's transport, the Guard decides by
the drive's policy, and a refused command is recorded ('Hsed.Jig.Transcript')
and never issued.

A drive reaches a run only as a 'GuardedDrive' (design 13.29 (l), closed at
M4c): "Hsed.Jig.Target" wraps the opener's 'RawDrive' the moment it opens it
('guardDrive'), and the constructor is not exported, so that no module
reaches the raw transport past it. Every command is then decided under the
drive's policy, which the drive holds; one refused is recorded Blocked in
the run's transcript and never issued, and one issued is recorded in the
run's transcript and in the drive's own ('driveIssued'), in the order and
with the entries M4b's run recorded, so that the run, at its end, decides
again every command the drive was issued under the policy it ended with
("Hsed.Jig.Run".'Hsed.Jig.Run.outsidePolicy').

A drive's policy is 'Level0Only' until its base ComID is bound: the IF-RECV
of Level 0 Discovery (protocol 1, ComID 1) and nothing else. 'driveBind'
binds the base ComID the drive's own Level 0 Discovery names (the Opal SSC
V2 descriptor's, Opal 2.30 Table 6, PDF 23), once, never one Core Table 26
reserves (PDF 55-56), and only where the run admits more than Level 0 on the
drive; the policy is then 'AllowList' of that literal base (design 13.31
(c)) and the methods the run admits ('readOnlyMethods', 'recoverMethods'),
read-only at the method level (design 13.7, Scott's answer of 2026-11-02).
Under it ('decide') a drive is given:

* Level 0 Discovery's IF-RECV still, and an IF-RECV of protocol 1 on the
  base: on no other ComID;
* an IF-SEND of protocol 1 on the base alone, its payload exactly its
  transfer length, one ComPacket on the base with a static ComID's
  extension (Core 3.3.3.1, PDF 57) and the host's OutstandingData and
  MinTransfer, zero (3.2.3.2.1.4-5, PDF 41), read without a difference,
  nothing after it but zeros, every subpacket Data and holding one allowed
  method invocation in its canonical encoding or a bare EndOfSession, its
  parameters read against the method's generated signature
  ("Hsed.Core.Method".'Hsed.Core.Method.decodeParameters', Core 3.2.4.1, PDF
  47-48: the required ones positional, exactly as many as it has, the
  optional ones Named by their index): SMUID.Properties with no
  HostProperties or with a list of host properties, each Named by a name
  Core Table 167 gives a host property and a uinteger (PDF 140-143);
  SMUID.StartSession to the Admin SP, its HostSessionID a uinteger and its
  Write 1, read-write (design 13.7), with no optional parameter but
  HostSigningAuthority = Anybody (Core 5.2.3.1, PDF 149: no HostChallenge,
  so that no credential is tried and no Tries counter moves); Get on
  C_PIN_MSID of one cell_block within its UID and PIN columns (Opal 2.30
  Table 23, PDF 47); the UIDs those of
  "Hsed.Gen.Core.UIDs" and, for C_PIN_MSID, Opal's generated Admin SP
  ("Hsed.Gen.Opal.Preconfig.AdminSP", Table 25, PDF 48; since M4b), the
  columns found by name in the generated C_PIN description
  ("Hsed.Gen.Core.TableDescs");
* ComID management (protocol 2) under 'AllowStackReset' alone (@--recover@,
  and the STACK_RESET before each drive case, design 9.3, 13.7), on the base
  alone: its IF-RECV (GET_COMID_RESPONSE), and an IF-SEND whose payload,
  exactly its transfer length, is Core Table 35's STACK_RESET request (PDF
  65), read strictly through the generated layout, for the base's Extended
  ComID (a static ComID's extension), nothing after it but zeros;
  VERIFY_COMID_VALID and every other request code read as a pin other than
  Table 35's and are refused, as is anything that does not parse. TPER_RESET
  is on ComID 0x0004 (Opal 2.30 Table 13, PDF 29), which Core Table 26
  reserves and so no base is: no policy admits it;
* nothing else: no other protocol, ComID or method.

The Guard sends on a @'ReadOnly@ transport, which no other module can:
'guardedSend' and 'guardedRecv' decide one command each; 'guarding' puts
the Guard in front of every command a transport is given, so that a drive's
transport refuses what its policy does whoever asks (the host's exchange of
a ComPacket among them), each refusal answered 'blockedResponse'. The
drive's transport ('driveTransport') and its two verbs ('driveSend',
'driveRecv') decide each command under the policy as it stands when the
command is given, unless the run latched the drive ('driveLatch': it gave
up on an answer, so that what the drive did with its command is no longer
known), when nothing more reaches it: each command after is answered a
'TransportFailure' saying why, noted in the run's transcript, never
issued.

Level 0 Discovery's ComID and the Data subpacket's Kind are the generated
code tables' (Core Tables 26 and 19) and the security protocol of ComPackets
is written by hand, Core Table 25 naming protocols 1 and 2 alike, each
defined once, in "Hsed.Wire". Were Table 26 ever to lose Level 0's row, its
ComID would read 0x0000, the reserved ComID, which every policy refuses.
-}
module Hsed.Jig.Guard
  ( -- * Policies
    Policy (..)
  , Allowed (..)
  , decide
  , readOnlyMethods
  , recoverMethods
    -- * A drive behind the Guard
  , GuardedDrive
  , guardDrive
  , driveTransport
  , driveSend
  , driveRecv
  , drivePolicy
  , driveAllowed
  , driveBind
  , driveIssued
  , driveInfo
  , driveLatch
  , driveLatched
  , driveClose
    -- * One command, one transport
  , guardedSend
  , guardedRecv
  , guarding
  , blockedResponse
  ) where

import qualified Data.Aeson            as A
import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import qualified RIO.Text              as T

import           Hsed.Core.Call        (MethodCall (..), MethodHeader (..), Parameters (..))
import           Hsed.Core.CellBlock   (CellBlock (..), cellBlock)
import           Hsed.Core.Layout      (LayoutError)
import           Hsed.Core.Method      (MethodSignature (..), Param (..), Signature (..), decodeParameters, paramErrorText)
import           Hsed.Core.Properties  (Communicator (..), Property (..))
import           Hsed.Core.TypeName    (TypeName (..))
import           Hsed.Core.Record      (DecodedRecord (..), decodeRecordStrict)
import           Hsed.Core.StreamItem  (StreamItem (..))
import           Hsed.Core.Table       (Col (..), TableDesc (..), columnNamed)
import           Hsed.Core.Token       (IsToken (..), Token (..))
import           Hsed.Core.UID         (UID)
import           Hsed.Core.Value       (Datum (..), List (..), Name (..), NamedValue (..), Value (..))
import           Hsed.Gen.Core.Layouts (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..),
                                        STACK_RESETCommandRequest (..))
import qualified Hsed.Gen.Core.Methods as Methods
import           Hsed.Gen.Core.Properties (properties)
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.UIDs    (methodIDUIDs, sessionManagerMethodUIDs, uAuthority_Anybody, uMethodID_Get, uProperties,
                                        uSMUID, uSP_Admin, uStartSession)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uC_PIN_MSID)
import           Hsed.Jig.Transcript   (TEntry (..), Transcript, entries, newTranscript, record, recording)
import           Hsed.Packet           (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket)
import           Hsed.Sim.Params       (reservedComId)
import           Hsed.Transport.Drive  (DriveInfo, RawDrive (..))
import           Hsed.Wire             (Cap (..), Describe (..), Dir (..), IfStatus (..), Request (..), Response (..),
                                        comIdManagementProtocol, comPacketProtocol, dataKind, extendedComId, hex, level0ComId,
                                        staticExtension)
import           Hsed.Wire.Internal    (Transport (..))


-- * Policies

-- | What a drive's commands may be.
data Policy
  = Level0Only                    -- ^ IF-RECV of Level 0 Discovery alone: a drive until its base ComID is bound
  | AllowList !Word16 ![Allowed]  -- ^ on the drive's base ComID, bound from its own Level 0, IF-SENDs of these
                                  --   methods and IF-RECV; besides, Level 0 Discovery's IF-RECV (M4c)
  deriving (Eq, Show)

-- | A method an 'AllowList' admits.
data Allowed
  = AllowProperties           -- ^ SMUID.Properties
  | AllowStartSessionAdminSP  -- ^ SMUID.StartSession to the Admin SP, as Anybody or no one
  | AllowGetCPinMSID          -- ^ Get on C_PIN_MSID, of its UID and PIN columns alone (UCT-03 step 1b)
  | AllowEndOfSession         -- ^ a bare EndOfSession
  | AllowStackReset           -- ^ STACK_RESET on the base ComID and its GET_COMID_RESPONSE, protocol 2 (@--recover@, and
                              --   before each drive case, "Hsed.Jig.Options".@roStackResetBeforeCases@)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A policy as the reports print it: @{"policy": "level0_only"}@, or
--   @{"policy": "allow_list", "base_comid": 4100, "allowed": [...]}@.
instance A.ToJSON Policy where
  toJSON = \case
    Level0Only            -> A.object ["policy" A..= ("level0_only" :: Text)]
    AllowList base admits -> A.object ["policy" A..= ("allow_list" :: Text), "base_comid" A..= base, "allowed" A..= admits]

-- | A method admitted, by name.
instance A.ToJSON Allowed where
  toJSON = A.String . \case
    AllowProperties          -> "properties"
    AllowStartSessionAdminSP -> "start_session_admin_sp"
    AllowGetCPinMSID         -> "get_c_pin_msid"
    AllowEndOfSession        -> "end_of_session"
    AllowStackReset          -> "stack_reset"

-- | What a drive run of design 9.3's drive cases admits once its base
--   ComID is bound, read-only at the method level (design 13.7, Scott's
--   answer of 2026-11-02): SMUID.Properties, StartSession to the Admin SP
--   as Anybody, Get on C_PIN_MSID of its UID and PIN columns, and End of
--   Session, so that no method that writes can reach the drive.
readOnlyMethods :: [Allowed]
readOnlyMethods = [AllowProperties, AllowStartSessionAdminSP, AllowGetCPinMSID, AllowEndOfSession]

-- | What @--recover@ admits once the base ComID is bound: STACK_RESET and
--   its GET_COMID_RESPONSE (Core 3.3.4.7.5, PDF 65-66), then StartSession
--   to the Admin SP as Anybody and its End of Session, the probe that no
--   session survived the reset (Core 5.1.5.3, PDF 138).
recoverMethods :: [Allowed]
recoverMethods = [AllowStartSessionAdminSP, AllowEndOfSession, AllowStackReset]


-- * A drive behind the Guard

-- | A drive as a run has it (design 13.29 (l)): the opener's 'RawDrive'
--   behind the Guard, what binding its base ComID admits, its policy
--   ('Level0Only' until bound), whether the run latched it, the run's
--   transcript and the drive's own, and what the IORegistry says of it
--   (none for a replay). Abstract: its constructor is not exported, so that
--   the raw transport is reached through the Guard alone.
data GuardedDrive = GuardedDrive
  { gdTranscript :: !Transcript              -- ^ the run's
  , gdOwn        :: !Transcript              -- ^ the drive's own: what reached its raw transport
  , gdAllowed    :: ![Allowed]               -- ^ what binding admits; none keeps the drive Level0Only for the run
  , gdInfo       :: !(Maybe DriveInfo)
  , gdRaw        :: !RawDrive
  , gdRecorded   :: !(Transport 'ReadOnly)    -- ^ the raw transport, recording into the drive's own transcript, then
                                             --   the run's
  , gdPolicy     :: !(IORef Policy)
  , gdLatched    :: !(IORef (Maybe Text))    -- ^ why nothing more reaches the drive, once the run latched it
  }

-- | A drive put behind the Guard, as "Hsed.Jig.Target" opens one: the
--   Guard's decision first, under the drive's policy ('Level0Only' until
--   'driveBind'), over a transport recording into the run's transcript,
--   over one recording into the drive's own, over the raw transport; so
--   that a command refused is recorded Blocked in the run's transcript and
--   never issued, and one issued is recorded in the drive's own transcript,
--   then in the run's, as M4b's run recorded a drive's (each entry the same).
guardDrive :: Transcript -> [Allowed] -> Maybe DriveInfo -> RawDrive -> IO GuardedDrive
guardDrive t admits di raw = do
  own <- newTranscript
  policy <- newIORef Level0Only
  latch <- newIORef Nothing
  pure GuardedDrive
    { gdTranscript = t
    , gdOwn        = own
    , gdAllowed    = admits
    , gdInfo       = di
    , gdRaw        = raw
    , gdRecorded   = recording t (recording own (rdTransport raw))
    , gdPolicy     = policy
    , gdLatched    = latch
    }

-- | The drive as a transport: every command decided under the drive's
--   policy as it stands when the command is given, one refused recorded,
--   never issued, and answered 'blockedResponse'; one admitted issued and
--   recorded in the run's transcript and the drive's own; on a drive the
--   run latched, none ('driveLatch'). The host's exchange of a ComPacket
--   takes its IF-RECVs here.
driveTransport :: GuardedDrive -> Transport 'ReadOnly
driveTransport g = (gdRecorded g) { tExchange = \q -> either blockedResponse id <$> current g q }

-- | An IF-SEND on the drive, if its policy admits it ('guardedSend') and
--   the run has not latched it.
driveSend :: GuardedDrive -> Request -> IO (Either Text Response)
driveSend g q = unlatched g q (drivePolicy g >>= \p -> guardedSend (gdTranscript g) p (gdRecorded g) q)

-- | An IF-RECV on the drive, if its policy admits it ('guardedRecv') and
--   the run has not latched it.
driveRecv :: GuardedDrive -> Request -> IO (Either Text Response)
driveRecv g q = unlatched g q (drivePolicy g >>= \p -> guardedRecv (gdTranscript g) p (gdRecorded g) q)

-- | A command under the drive's policy as it stands, unless the run latched
--   it.
current :: GuardedDrive -> Request -> IO (Either Text Response)
current g q = unlatched g q (drivePolicy g >>= \p -> pass (gdTranscript g) p (gdRecorded g) q)

-- | A command given to the drive: on a drive the run latched, not issued,
--   answered a 'TransportFailure' saying why and noted in the run's
--   transcript; otherwise as the action decides.
unlatched :: GuardedDrive -> Request -> IO (Either Text Response) -> IO (Either Text Response)
unlatched g q act = readIORef (gdLatched g) >>= \case
  Nothing  -> act
  Just why -> do
    record (gdTranscript g) (Note 0 ("not issued on the " <> dRole (tDescribe (gdRecorded g)) <> ", " <> command <> ": " <> why))
    pure (Right (Response (TransportFailure ("not issued: " <> why)) mempty))
  where
    command = (if dir q == IfSend then "an IF-SEND" else "an IF-RECV") <> " of protocol " <> hex (proto q) <> " on ComID "
              <> hex (comId q)

-- | Latch the drive for the rest of the run, saying why (the run gave up on
--   an answer: what the drive did with its command is no longer known), so
--   that nothing more reaches it; a drive latched already keeps its first
--   reason.
driveLatch :: GuardedDrive -> Text -> IO ()
driveLatch g why = atomicModifyIORef' (gdLatched g) (\was -> (was <|> Just why, ()))

-- | Why the run latched the drive, if it did.
driveLatched :: GuardedDrive -> IO (Maybe Text)
driveLatched = readIORef . gdLatched

-- | The drive's policy: 'Level0Only' until its base ComID is bound, then
--   'AllowList' of the base and what binding admits.
drivePolicy :: GuardedDrive -> IO Policy
drivePolicy = readIORef . gdPolicy

-- | What binding the drive's base ComID admits; none keeps the drive
--   'Level0Only' for the run (no case selected needs more of it).
driveAllowed :: GuardedDrive -> [Allowed]
driveAllowed = gdAllowed

-- | Bind the drive's base ComID, the one its own Level 0 Discovery names
--   (the Opal SSC V2 descriptor's, Opal 2.30 Table 6, PDF 23), or why not:
--   refused where binding admits nothing ('driveAllowed' empty), where Core
--   Table 26 reserves the ComID ("Hsed.Sim.Params".'reservedComId', read
--   from the generated table: 0x0000 to 0x0FFF, PDF 55-56, so never Level
--   0's 0x0001 nor TPER_RESET's 0x0004), and where another base is bound
--   already, a drive binding one base once; the same base again is bound
--   already. Otherwise the opener's bind first (the kext shim's
--   @hsed_kext_bind@, which checks again; a replay's refuses), then the
--   policy 'AllowList' of the base and what binding admits.
driveBind :: GuardedDrive -> Word16 -> IO (Either Text ())
driveBind g base
  | null (gdAllowed g) =
      pure (Left ("base ComID " <> hex base <> " not bound: this run admits Level 0 Discovery alone on the drive (design 9.3)"))
  | reservedComId base =
      pure (Left ("base ComID " <> hex base <> " not bound: Core Table 26 reserves it (PDF 55-56); a drive's base ComID is "
                  <> "a non-reserved one, 0x1000 or above"))
  | otherwise = drivePolicy g >>= \case
      AllowList bound _
        | bound == base -> pure (Right ())
        | otherwise     -> pure (Left ("base ComID " <> hex base <> " not bound: the drive is bound to " <> hex bound
                                       <> " already, and a drive binds one base ComID"))
      Level0Only -> rdBind (gdRaw g) base >>= \case
        Left why -> pure (Left ("base ComID " <> hex base <> " not bound: " <> why))
        Right () -> Right () <$ writeIORef (gdPolicy g) (AllowList base (gdAllowed g))

-- | The commands that reached the drive's raw transport, with their
--   answers, in order: its own transcript.
driveIssued :: GuardedDrive -> IO [TEntry]
driveIssued = entries . gdOwn

-- | What the IORegistry says of the drive; none for a replay.
driveInfo :: GuardedDrive -> Maybe DriveInfo
driveInfo = gdInfo

-- | Close the drive's raw transport.
driveClose :: GuardedDrive -> IO ()
driveClose = tClose . rdTransport . gdRaw


-- * One command, one transport

-- | An IF-SEND on a drive, if the policy allows it; otherwise the reason,
--   recorded, and the transport never called.
guardedSend :: Transcript -> Policy -> Transport 'ReadOnly -> Request -> IO (Either Text Response)
guardedSend t policy tr q
  | dir q /= IfSend = refuse t tr q "guardedSend takes an IF-SEND"
  | otherwise       = pass t policy tr q

-- | An IF-RECV, if the policy allows it; otherwise the reason, recorded.
guardedRecv :: Transcript -> Policy -> Transport c -> Request -> IO (Either Text Response)
guardedRecv t policy tr q
  | dir q /= IfRecv = refuse t tr q "guardedRecv takes an IF-RECV"
  | otherwise       = pass t policy tr q

-- | A transport whose every command passes the Guard first: one the policy
--   refuses is recorded, never issued, and answered 'blockedResponse'.
guarding :: Transcript -> Policy -> Transport c -> Transport c
guarding t policy tr = tr { tExchange = fmap (either blockedResponse id) . pass t policy tr }

-- | A command the Guard refused, as its transport's answer: 'Unsupported',
--   with the reason.
blockedResponse :: Text -> Response
blockedResponse why = Response (Unsupported ("blocked by the Guard: " <> why)) mempty

-- | A command through the policy: issued, or refused and recorded.
pass :: Transcript -> Policy -> Transport c -> Request -> IO (Either Text Response)
pass t policy tr q = either (refuse t tr q) (const (Right <$> tExchange tr q)) (decide policy q)

-- | A command refused: recorded as blocked, with the end it was for and why.
refuse :: Transcript -> Transport c -> Request -> Text -> IO (Either Text Response)
refuse t tr q why = Left why <$ record t (Blocked 0 (dRole (tDescribe tr)) q why)

-- | What a policy says of a command. Level0Only admits Level 0's IF-RECV on
--   the ComID Core Table 26 gives it, and nothing on the reserved ComID
--   0x0000, which "Hsed.Wire".'level0ComId' would read were the row lost.
--   An allow list admits that IF-RECV too, and, on its base ComID alone, a
--   ComID Core Table 26 does not reserve: IF-RECV of protocol 1, an IF-SEND
--   of protocol 1 whose payload is exactly its transfer length and whose
--   every method it lists ('allowedSend'), and, with 'AllowStackReset'
--   alone, ComID management's STACK_RESET and GET_COMID_RESPONSE
--   ('stackResetAllowed'); nothing else (design 9.3, 13.31 (c)).
decide :: Policy -> Request -> Either Text ()
decide policy q = case policy of
  Level0Only
    | level0ComId == 0x0000 -> Left "Level0Only: Core Table 26 gives Level 0 Discovery no ComID: nothing reaches a drive"
    | level0Recv            -> Right ()
    | dir q == IfSend       -> Left "Level0Only: no IF-SEND reaches a drive before its base ComID is bound (design 9.3)"
    | otherwise             -> Left ("Level0Only: IF-RECV of Level 0 Discovery alone (protocol " <> hex comPacketProtocol
                                     <> ", ComID " <> hex level0ComId <> "), not protocol " <> hex (proto q) <> ", ComID " <> hex (comId q))
  AllowList base allowed
    | level0ComId /= 0x0000, level0Recv -> Right ()
    | reservedComId base ->
        Left ("AllowList: its base ComID " <> hex base <> " is one Core Table 26 reserves (PDF 55-56), so no drive's: "
              <> "Level 0 Discovery's IF-RECV alone")
    | comId q /= base ->
        Left ("AllowList: " <> command <> " on ComID " <> hex (comId q) <> ", not the base ComID " <> hex base
              <> " the drive's Level 0 named (design 13.31 (c))")
    | proto q == comIdManagementProtocol, AllowStackReset `notElem` allowed ->
        Left ("AllowList: ComID management (protocol " <> hex comIdManagementProtocol <> ") refused: the allow list holds no STACK_RESET")
    | proto q == comIdManagementProtocol -> first ("AllowList: " <>) (stackResetAllowed q)
    | proto q /= comPacketProtocol ->
        Left ("AllowList: protocol " <> hex (proto q) <> " refused: protocols " <> hex comPacketProtocol <> " and "
              <> hex comIdManagementProtocol <> " alone")
    | dir q == IfRecv -> Right ()
    | otherwise -> first ("AllowList: " <>) (exactTransfer q >> allowedSend allowed q)
  where
    level0Recv = dir q == IfRecv && proto q == comPacketProtocol && comId q == level0ComId
    command    = (if dir q == IfSend then "an IF-SEND" else "an IF-RECV") <> " of protocol " <> hex (proto q)

-- | An IF-SEND's payload exactly its transfer length, as the drive is given
--   it: the kext's shim carries the one buffer and checks it whole
--   ("Hsed.Transport.Cdb".'Hsed.Transport.Cdb.shapeOf'; cbits/hsed_cdb.c), so
--   that no byte past what the Guard read, nor one short of it, reaches the
--   drive.
exactTransfer :: Request -> Either Text ()
exactTransfer q
  | B.length (payload q) == fromIntegral (lenBytes q) = Right ()
  | otherwise = Left ("an IF-SEND of " <> tshow (B.length (payload q)) <> " bytes on a transfer of " <> tshow (lenBytes q)
                      <> ": the payload is the transfer, exactly")

-- | A ComID management command 'AllowStackReset' admits on the base ComID
--   (Core 3.3.4.7.5, PDF 65-66; design 9.3's @--recover@, and 13.7's
--   STACK_RESET before each drive case): an IF-RECV (GET_COMID_RESPONSE,
--   which reads what the TPer holds and changes nothing but its buffer), or
--   an IF-SEND whose payload, exactly its transfer length, is Table 35's
--   STACK_RESET request and nothing else: read strictly through the
--   generated layout (its Request Code a pin, 0x00000002, so
--   VERIFY_COMID_VALID's 0x00000001 and every other code are refused), its
--   Extended ComID the command's ComID with a static ComID's extension, its
--   reserved bytes zeros to the end of the transfer.
stackResetAllowed :: Request -> Either Text ()
stackResetAllowed q
  | dir q == IfRecv = Right ()
  | otherwise = exactTransfer q >> case decodeRecordStrict (payload q) :: Either LayoutError (DecodedRecord STACK_RESETCommandRequest) of
      Left e -> Left ("the payload is no STACK_RESET request (Core Table 35, PDF 65): " <> tshow e)
      Right (DecodedRecord request ds rest)
        | not (null ds) -> Left ("a STACK_RESET request read with differences: " <> tshow ds)
        | not (B.all (== 0) rest) -> Left "bytes other than zeros after the STACK_RESET request"
        | stack_RESETCommandRequestExtendedComID request /= extended ->
            Left ("a STACK_RESET request for Extended ComID " <> hex (stack_RESETCommandRequestExtendedComID request) <> ", not the command's "
                  <> hex extended <> " (a static ComID's extension, Core 3.3.3.1, PDF 57)")
        | otherwise -> Right ()
  where
    extended = extendedComId (comId q)

-- | An IF-SEND's payload: one ComPacket on the command's ComID, with a
--   static ComID's extension (Core 3.3.3.1, PDF 57) and the OutstandingData
--   and MinTransfer a host sends, zero (3.2.3.2.1.4-5, PDF 41: reserved in a
--   host's ComPacket), read without a difference (so its reserved bytes 0-3
--   zero too, 3.2.3.2.1.1, PDF 40), then zeros, whose every subpacket is
--   Data and holds one allowed method invocation (in its canonical
--   encoding) or a bare EndOfSession.
allowedSend :: [Allowed] -> Request -> Either Text ()
allowedSend allowed q = do
  DecodedFrame (ComPacketFrame h packets) ds rest <- first (("the payload is no ComPacket: " <>) . tshow) (decodeComPacket (payload q))
  unless (null ds) (Left ("the ComPacket reads with differences: " <> tshow ds))
  unless (B.all (== 0) rest) (Left "bytes other than zeros after the ComPacket")
  unless (comPacketComID h == comId q) (Left ("the ComPacket's ComID " <> hex (comPacketComID h) <> " is not the command's"))
  unless (comPacketComIDExtension h == staticExtension) $
    Left ("the ComPacket's ComID extension " <> hex (comPacketComIDExtension h) <> ", not a static ComID's " <> hex staticExtension
          <> " (Core 3.3.3.1, PDF 57)")
  unless (comPacketOutstandingData h == 0) $
    Left ("the ComPacket's OutstandingData " <> tshow (comPacketOutstandingData h) <> ", not the host's 0 (Core 3.2.3.2.1.4, PDF 41)")
  unless (comPacketMinTransfer h == 0) $
    Left ("the ComPacket's MinTransfer " <> tshow (comPacketMinTransfer h) <> ", not the host's 0 (Core 3.2.3.2.1.5, PDF 41)")
  case concatMap pfSubPackets packets of
    []  -> Left "a ComPacket with no subpacket"
    sps -> traverse_ (allowedSubPacket allowed) sps

allowedSubPacket :: [Allowed] -> DataSubPacket -> Either Text ()
allowedSubPacket allowed sp
  | kind /= dataKind                = Left ("a subpacket of Kind " <> hex kind <> ", not Data")
  | bytes == generate EndOfSession = need allowed AllowEndOfSession "a bare EndOfSession"
  | otherwise                      = case parseByteString bytes of
      Right call | generate call == bytes -> allowedCall allowed call
      _                                   -> Left "a subpacket that is no single method invocation"
  where
    kind  = dataSubPacketKind (dataSubPacketHeader sp)
    bytes = dataSubPacketPayload sp

allowedCall :: [Allowed] -> MethodCall -> Either Text ()
allowedCall allowed (MethodCall (MethodHeader invoker method) ps _)
  | invoker == uSMUID && method == uProperties   = need allowed AllowProperties "SMUID.Properties" >> propertiesAllowed ps
  | invoker == uSMUID && method == uStartSession =
      need allowed AllowStartSessionAdminSP "SMUID.StartSession" >> startSessionAllowed ps
  | invoker == uC_PIN_MSID && method == uMethodID_Get =
      need allowed AllowGetCPinMSID "Get on C_PIN_MSID" >> getCPinMSIDAllowed ps
  | otherwise = Left ("the method " <> named method <> " on " <> tshow invoker <> " is not allowed")
  where named u = maybe (tshow u) (\n -> T.pack n <> " (" <> tshow u <> ")") (L.lookup u (sessionManagerMethodUIDs <> methodIDUIDs))

-- | Properties' parameters read against its generated signature (Core
--   5.2.2.1, PDF 140: no required parameter, one optional, HostProperties):
--   none, or HostProperties a list of host properties, each a Named value
--   whose name Core Table 167 gives a host property (the generated
--   'properties', PDF 141-143) and whose value is a uinteger (a boolean
--   property's 0 or 1), so that nothing but the host's communication
--   properties reaches the drive (each of which STACK_RESET puts back to its
--   default, Core 3.3.4.7.5 item 7, PDF 65).
propertiesAllowed :: Parameters -> Either Text ()
propertiesAllowed ps = do
  call <- first (("Properties' parameters are not its signature's: " <>) . paramErrorText) (decodeParameters @Methods.Properties ps)
  for_ (Methods.propertiesHostProperties call) $ \case
    L (List items) -> traverse_ hostProperty items
    other          -> Left ("Properties' HostProperties is no list but " <> tshow other <> " (Core 5.2.2.1, PDF 140)")
  where
    hostProperty = \case
      N (NamedValue (Name (Datum (Bytes n))) (D (Datum (Unsigned v))))
        | Just p <- L.find ((== decodeUtf8Lenient n) . propertyName) properties, Host `elem` appliesTo p ->
            unless (not (boolean p) || v <= 1) (Left ("Properties with the boolean host property " <> propertyName p <> " = " <> tshow v
                                                       <> ", not 0 or 1 (Core Table 167, PDF 143)"))
        | otherwise -> Left ("Properties with a host property " <> tshow n <> " that Core Table 167 gives no host (PDF 141-143)")
      other -> Left ("Properties' HostProperties holds " <> tshow other <> ", no name = uinteger (Core 5.2.2.1, PDF 140)")
    boolean p = typeNameText (propertyType p) == "boolean"

-- | StartSession's parameters read against its generated signature (Core
--   5.2.3.1, PDF 149; 3.2.4.1, PDF 47-48: exactly its three required ones,
--   positional, the optional ones Named by their 0-based index among them):
--   HostSessionID a uinteger; SPID the Admin SP's; Write 1, a read-write
--   session, the jig's every session (Test Cases 4.1 e, PDF 26; design 13.7,
--   Scott's answer of 2026-11-02); and no optional parameter but its
--   HostSigningAuthority, Anybody, if that: no HostChallenge,
--   HostExchangeAuthority, HostExchangeCert, HostSigningCert,
--   SessionTimeout, TransTimeout, InitialCredit nor SignedHash, so that a
--   session to the Admin SP opens as Anybody, who authenticates without a
--   credential (5.3.4.1.2.1, PDF 211), and no credential is tried.
startSessionAllowed :: Parameters -> Either Text ()
startSessionAllowed ps = do
  call <- first (("StartSession's parameters are not its signature's: " <>) . paramErrorText) (decodeParameters @Methods.StartSession ps)
  unless (isJust (uintegerOf (Methods.startSessionHostSessionID call))) $
    Left "StartSession whose HostSessionID is no uinteger (Core 5.2.3.1, PDF 149)"
  unless (isUid uSP_Admin (Methods.startSessionSPID call)) (Left "StartSession to an SP other than the Admin SP")
  unless (uintegerOf (Methods.startSessionWrite call) == Just 1) $
    Left "StartSession whose Write is not 1: a drive's sessions are read-write under the allow list (design 13.7)"
  case [ (paramName p, v) | (p, Just v) <- zip (sigOptional (signature @Methods.StartSession)) (optionalValues call) ] of
    [] -> Right ()
    [("HostSigningAuthority", v)] ->
      unless (isUid uAuthority_Anybody v) (Left "StartSession with a HostSigningAuthority other than Anybody")
    given -> Left ("StartSession with " <> T.intercalate ", " (map fst given) <> ": no optional parameter but "
                   <> "HostSigningAuthority = Anybody (Core 5.2.3.1, PDF 149)")

-- | A value that is a uinteger.
uintegerOf :: Value -> Maybe Natural
uintegerOf = \case
  D (Datum (Unsigned n)) -> Just n
  _                      -> Nothing

-- | Get on C_PIN_MSID of its UID and PIN columns alone (UCT-03 step 1b,
--   Test Cases PDF 32; the columns Opal 2.30's ACE_C_PIN_MSID_Get_PIN
--   grants Anybody, Table 23, PDF 47): one cell_block
--   ("Hsed.Core.CellBlock", Core 5.1.4.2.3, PDF 129-130) and no optional
--   parameter, the cell_block giving neither Table, startRow nor endRow (on
--   an object they are refused anyway, 5.1.4.2.3 a.b, b.c, c.b), and the
--   columns from its startColumn (the first, omitted) to its endColumn (the
--   last, omitted) each the UID or the PIN column, found by name in the
--   generated C_PIN description.
getCPinMSIDAllowed :: Parameters -> Either Text ()
getCPinMSIDAllowed (Parameters positional nameds) = do
  unless (null nameds) (Left "Get with an optional parameter, which its signature has none of")
  cb <- case positional of
    [v] -> first ("Get's cell_block: " <>) (cellBlock v)
    _   -> Left ("Get with " <> tshow (length positional) <> " required parameters, not its one cell_block")
  traverse_ (\(component, given) -> when given (Left ("Get on C_PIN_MSID with a cell_block naming " <> component <> ": its UID and PIN columns alone")))
            [("Table", isJust (cbTable cb)), ("startRow", isJust (cbStartRow cb)), ("endRow", isJust (cbEndRow cb))]
  let start = fromMaybe 0 (cbStartColumn cb)
      end   = fromMaybe lastColumn (cbEndColumn cb)
  unless (start <= end && all (`elem` allowedColumns) [start .. end]) $
    Left ("Get on C_PIN_MSID of columns " <> tshow start <> " to " <> tshow end <> ": its UID and PIN columns alone ("
          <> T.intercalate ", " (map tshow allowedColumns) <> ")")
  where
    allowedColumns = [ fromIntegral (colNumber col) | name <- ["UID", "PIN"], Just col <- [columnNamed name c_PINTableDesc] ] :: [Natural]
    lastColumn     = fromIntegral (maybe 0 colNumber (L.lastMaybe (tdColumns c_PINTableDesc)))

-- | Whether a parameter is a UID's bytes.
isUid :: UID -> Value -> Bool
isUid u v = mtoken v == Just (token u)

need :: [Allowed] -> Allowed -> Text -> Either Text ()
need allowed a what
  | a `elem` allowed = Right ()
  | otherwise        = Left (what <> " is not in the allow list")
