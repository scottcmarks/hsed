{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations   #-}

{-|
Module      : Hsed.Wire.Internal
Description : The wire contract's types, and the Transport constructor only the transports, the Guard and the Transcript see
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What a host and a TPer exchange (design docs/spec-as-source.md section 6.1):
an interface command ('Request': IF-SEND or IF-RECV, a security protocol, a
ComID, a transfer length and, for IF-SEND, its payload) and its answer
('Response': an interface status and, for IF-RECV, exactly the bytes asked
for), over a 'Transport' that says what it is ('Describe') and counts its
transfer lengths in a 'Unit'.

A transport is indexed by its capability ('Cap'). Every transport answers
IF-RECV; only a @'ReadWrite@ one is given IF-SEND and events
("Hsed.Wire".'Hsed.Wire.ifSend', 'Hsed.Wire.inject'), and the kext's
transport to the attached drive is @'ReadOnly@ (design 9.3). The index is a
phantom, so two things make it hold: 'Transport' is abstract everywhere but
here and in the modules that build or decorate transports (the transports
@Hsed.Transport.*@, the Guard, the Transcript: "Hsed.CapabilitySpec" holds
the list), so no other module can build one or change its index by a record
update; and its role is nominal, so no 'coerce' turns a @'ReadOnly@ transport
into a @'ReadWrite@ one.

The types' JSON (aeson's 'A.ToJSON') is what the jig's reports and
transcripts print: statuses and events in snake case (the words design 6.3's
frames use), a status's text as its @detail@, and bytes as hex with their
trailing zeros left out and their length kept (@len@), which a transfer's
zero padding makes the larger part of.
-}
module Hsed.Wire.Internal
  ( -- * Requests and responses
    Dir (..)
  , Request (..)
  , IfStatus (..)
  , ifStatusName
  , ifStatusDetail
  , Response (..)
    -- * Events, units and capabilities
  , Event (..)
  , Unit (..)
  , Cap (..)
    -- * What a transport says it is
  , Describe (..)
    -- * Transports
  , Transport (..)
    -- * Bytes in JSON
  , trimmedHex
  ) where

import qualified Data.Aeson             as A
import qualified Data.ByteString.Base16 as B16
import qualified RIO.ByteString         as B
import           RIO


-- * Requests and responses

-- | The interface command: IF-SEND carries a payload to the TPer, IF-RECV
--   asks it for one (Core 3.3.2, the interface's Trusted Send and Trusted
--   Receive).
data Dir
  = IfSend
  | IfRecv
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One interface command.
data Request = Request
  { dir      :: !Dir
  , proto    :: !Word8       -- ^ the security protocol (0x01 the ComPacket protocol, 0x02 ComID management)
  , comId    :: !Word16      -- ^ the protocol-specific field: the ComID
  , lenBytes :: !Word32      -- ^ the transfer length in bytes: IF-SEND's payload, IF-RECV's allocation
  , payload  :: !ByteString  -- ^ IF-SEND's bytes (zero-padded to 'lenBytes'); empty for IF-RECV
  }
  deriving (Eq, Show)

-- | The interface status of a command (SIIS 1.20's mapping of TCG's
--   statuses to each interface), and two of hsed's own: a transport that
--   failed (the kext's @kern_return_t@, a lost socket) and a command a
--   transport does not carry.
data IfStatus
  = Good
  | InvalidSecurityProtocolId
  | InvalidTransferLengthOnIfSend
  | OtherInvalidCommandParameter
  | SynchronousProtocolViolation
  | DataProtectionError
  | TransportFailure !Text  -- ^ what failed, as the transport names it
  | Unsupported !Text       -- ^ why the transport does not carry the command
  deriving (Eq, Show)

-- | A status's name in snake case, the word design 6.3's frames carry.
ifStatusName :: IfStatus -> Text
ifStatusName = \case
  Good                          -> "good"
  InvalidSecurityProtocolId     -> "invalid_security_protocol_id"
  InvalidTransferLengthOnIfSend -> "invalid_transfer_length_on_if_send"
  OtherInvalidCommandParameter  -> "other_invalid_command_parameter"
  SynchronousProtocolViolation  -> "synchronous_protocol_violation"
  DataProtectionError           -> "data_protection_error"
  TransportFailure _            -> "transport_failure"
  Unsupported _                 -> "unsupported"

-- | The text a status carries, if it carries one.
ifStatusDetail :: IfStatus -> Maybe Text
ifStatusDetail = \case
  TransportFailure t -> Just t
  Unsupported t      -> Just t
  _                  -> Nothing

-- | A command's answer: its status and, for IF-RECV, exactly the bytes the
--   request's 'lenBytes' asked for, zero-padded (Core 3.3.6.2 for Level 0,
--   PDF 67; the interface's padding otherwise, design 6.2); empty for IF-SEND.
data Response = Response
  { status   :: !IfStatus
  , rpayload :: !ByteString
  }
  deriving (Eq, Show)


-- * Events, units and capabilities

-- | What resets a TPer from outside the protocol: Opal 2.30 Table 16's reset
--   types 0 to 2 (PDF 31; type 3, Programmatic, is TPER_RESET, an IF-SEND).
data Event
  = PowerCycle
  | HardwareReset
  | HotPlug
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What a transport's transfer lengths count (design 6.2): 512-byte blocks
--   (SCSI's INC_512, ATA's Trusted Send and Receive) or bytes (NVMe, the
--   simulator in process and over the LAN). Not @Bytes@: "Hsed.Core.Layout"
--   and "Hsed.Core.Token" name constructors so.
data Unit
  = UnitBlocks512
  | UnitBytes
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A transport's capability, the index of 'Transport' (promoted).
data Cap
  = ReadOnly   -- ^ IF-RECV only: the attached drive at M3
  | ReadWrite  -- ^ IF-SEND, IF-RECV and events: the simulator
  deriving (Eq, Ord, Show, Enum, Bounded)


-- * What a transport says it is

-- | A transport's description: what M5's hub puts in its hello frame's
--   @app.sed@ (design 6.3) and the reports print.
data Describe = Describe
  { dRole      :: !Text            -- ^ @tper@, @drive@, @drive-replay@
  , dImpl      :: !Text            -- ^ what answers: @hsed-sim@ and the version, the kext and its build
  , dUnits     :: !Unit
  , dSsc       :: !(Maybe Text)    -- ^ the SSC it claims, @opal-2.30@
  , dBaseComId :: !(Maybe Word16)
  , dNComIds   :: !(Maybe Word16)
  , dDevice    :: !(Maybe Text)    -- ^ the device, @/dev/disk6@, or the file a replay reads
  }
  deriving (Eq, Show)


-- * Transports

-- | A transport at a capability (design 6.1, 9.3): how a command is
--   exchanged, how an event is caused, the unit of its lengths, what it says
--   it is, how it is closed. Built by the transports alone (@Hsed.Transport.*@);
--   everywhere else it is abstract ("Hsed.Wire").
type role Transport nominal
data Transport (c :: Cap) = MkTransport
  { tExchange :: Request -> IO Response  -- ^ one command and its answer
  , tInject   :: Event -> IO IfStatus    -- ^ an event the transport causes; a drive's answers 'Unsupported'
  , tUnit     :: !Unit
  , tDescribe :: !Describe
  , tClose    :: IO ()
  }


-- * Bytes in JSON

-- | Bytes as lower-case hex, the trailing zeros left out (the length is
--   printed beside them).
--
-- >>> trimmedHex "\x01\x00\xab\x00\x00"
-- "0100ab"
trimmedHex :: ByteString -> Text
trimmedHex = decodeUtf8Lenient . B16.encode . B.dropWhileEnd (== 0)

instance A.ToJSON Dir where
  toJSON = A.String . \case
    IfSend -> "if_send"
    IfRecv -> "if_recv"

instance A.ToJSON Request where
  toJSON r = A.object $
    [ "op"    A..= dir r
    , "proto" A..= proto r
    , "comid" A..= comId r
    , "len"   A..= lenBytes r
    ]
    <> [ "payload" A..= trimmedHex (payload r) | dir r == IfSend ]

instance A.ToJSON IfStatus where
  toJSON s = A.object (statusPairs s)

instance A.ToJSON Response where
  toJSON r = A.object $
    statusPairs (status r)
    <> [ "len"     A..= B.length (rpayload r)
       , "payload" A..= trimmedHex (rpayload r)
       ]

statusPairs :: IfStatus -> [(A.Key, A.Value)]
statusPairs s = ("status" A..= ifStatusName s) : [ "detail" A..= d | Just d <- [ifStatusDetail s] ]

instance A.ToJSON Event where
  toJSON = A.String . \case
    PowerCycle    -> "power_cycle"
    HardwareReset -> "hardware_reset"
    HotPlug       -> "hot_plug"

instance A.ToJSON Unit where
  toJSON = A.String . \case
    UnitBlocks512 -> "blocks512"
    UnitBytes     -> "bytes"

instance A.ToJSON Cap where
  toJSON = A.String . \case
    ReadOnly  -> "read_only"
    ReadWrite -> "read_write"

instance A.ToJSON Describe where
  toJSON d = A.object $
    [ "role"  A..= dRole d
    , "impl"  A..= dImpl d
    , "units" A..= dUnits d
    ]
    <> [ "ssc"        A..= v | Just v <- [dSsc d] ]
    <> [ "base_comid" A..= v | Just v <- [dBaseComId d] ]
    <> [ "n_comids"   A..= v | Just v <- [dNComIds d] ]
    <> [ "device"     A..= v | Just v <- [dDevice d] ]
