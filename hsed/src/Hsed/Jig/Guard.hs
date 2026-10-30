{-# LANGUAGE DataKinds         #-}
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
the run's policy, and a refused command is recorded ('Hsed.Jig.Transcript')
and never issued. At M3 a drive's policy is 'Level0Only': IF-RECV of Level
0 Discovery (protocol 1, ComID 1) and nothing else. 'AllowList' is M4c's,
written now for its test: an IF-SEND whose ComPacket parses to methods every
one of which is allowed (SMUID.Properties; SMUID.StartSession to the Admin
SP as Anybody or no one; a bare EndOfSession), the UIDs those of
"Hsed.Gen.Core.UIDs", and IF-RECV of protocol 1; ComID management
(protocol 2) and anything that does not parse are refused.

The Guard sends on a @'ReadOnly@ transport, which no other module can:
'guardedSend' and 'guardedRecv' decide one command each; 'guarding' puts
the Guard in front of every command a transport is given, so that a drive's
transport refuses what its policy does whoever asks (the host's exchange of
a ComPacket among them).

Level 0 Discovery's ComID and the Data subpacket's Kind are the generated
code tables' (Core Tables 26 and 19); the security protocol of ComPackets is
written here ('comPacketProtocol'), Core Table 25 naming protocols 1 and 2
alike.
-}
module Hsed.Jig.Guard
  ( Policy (..)
  , Allowed (..)
  , guardedSend
  , guardedRecv
  , guarding
    -- * Where Level 0 Discovery and ComPackets go
  , comPacketProtocol
  , level0ComId
  , dataKind
  ) where

import           Data.Bits             (FiniteBits (..))
import           Numeric               (showHex)
import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import qualified RIO.Text              as T

import           Hsed.Core.Call        (MethodCall (..), MethodHeader (..), Parameters (..))
import           Hsed.Core.Code        (CodeRow (..), Span (..), codeRowsNamed)
import           Hsed.Core.Method      (MethodSignature (..), Param (..), Signature (..))
import           Hsed.Core.StreamItem  (StreamItem (..))
import           Hsed.Core.Token       (IsToken (..), Token (..))
import           Hsed.Core.UID         (UID)
import           Hsed.Core.Value       (Datum (..), Name (..), NamedValue (..), Value)
import           Hsed.Gen.Core.Codes   (cComIDAssignments, cSubpacketTypes)
import           Hsed.Gen.Core.Layouts (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..))
import qualified Hsed.Gen.Core.Methods as Methods
import           Hsed.Gen.Core.UIDs    (methodIDUIDs, sessionManagerMethodUIDs, uAuthority_Anybody, uProperties, uSMUID,
                                        uSP_Admin, uStartSession)
import           Hsed.Jig.Transcript   (TEntry (..), Transcript, record)
import           Hsed.Packet           (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket)
import           Hsed.Wire             (Cap (..), Dir (..), IfStatus (..), Request (..), Response (..))
import           Hsed.Wire.Internal    (Transport (..))


-- | What a drive's commands may be.
data Policy
  = Level0Only           -- ^ IF-RECV of Level 0 Discovery alone (M3)
  | AllowList ![Allowed]  -- ^ IF-SENDs of these methods, and IF-RECV (M4c)
  deriving (Eq, Show)

-- | A method an 'AllowList' admits.
data Allowed
  = AllowProperties           -- ^ SMUID.Properties
  | AllowStartSessionAdminSP  -- ^ SMUID.StartSession to the Admin SP, as Anybody or no one
  | AllowEndOfSession         -- ^ a bare EndOfSession
  deriving (Eq, Ord, Show, Enum, Bounded)


-- * Deciding

-- | An IF-SEND on a drive, if the policy allows it; otherwise the reason,
--   recorded, and the transport never called.
guardedSend :: Transcript -> Policy -> Transport 'ReadOnly -> Request -> IO (Either Text Response)
guardedSend t policy tr q
  | dir q /= IfSend = refuse t q "guardedSend takes an IF-SEND"
  | otherwise       = pass t policy tr q

-- | An IF-RECV, if the policy allows it; otherwise the reason, recorded.
guardedRecv :: Transcript -> Policy -> Transport c -> Request -> IO (Either Text Response)
guardedRecv t policy tr q
  | dir q /= IfRecv = refuse t q "guardedRecv takes an IF-RECV"
  | otherwise       = pass t policy tr q

-- | A transport whose every command passes the Guard first: one the policy
--   refuses is recorded, never issued, and answered 'Unsupported' with the
--   reason.
guarding :: Transcript -> Policy -> Transport c -> Transport c
guarding t policy tr = tr { tExchange = fmap (either blocked id) . pass t policy tr }
  where blocked why = Response (Unsupported ("blocked by the Guard: " <> why)) mempty

-- | A command through the policy: issued, or refused and recorded.
pass :: Transcript -> Policy -> Transport c -> Request -> IO (Either Text Response)
pass t policy tr q = either (refuse t q) (const (Right <$> tExchange tr q)) (decide policy q)

-- | A command refused: recorded as blocked, with why.
refuse :: Transcript -> Request -> Text -> IO (Either Text Response)
refuse t q why = Left why <$ record t (Blocked 0 q why)

-- | What a policy says of a command.
decide :: Policy -> Request -> Either Text ()
decide policy q = case policy of
  Level0Only
    | dir q == IfRecv && proto q == comPacketProtocol && comId q == level0ComId -> Right ()
    | dir q == IfSend -> Left "Level0Only: no IF-SEND reaches a drive at M3 (design 9.3)"
    | otherwise       -> Left ("Level0Only: IF-RECV of Level 0 Discovery alone (protocol " <> hex comPacketProtocol
                               <> ", ComID " <> hex level0ComId <> "), not protocol " <> hex (proto q) <> ", ComID " <> hex (comId q))
  AllowList allowed
    | proto q /= comPacketProtocol -> Left ("AllowList: protocol " <> hex (proto q) <> " refused, ComID management among them")
    | dir q == IfRecv              -> Right ()
    | otherwise                    -> first ("AllowList: " <>) (allowedSend allowed q)

-- | An IF-SEND's payload: one ComPacket on the command's ComID, read without
--   a difference, then zeros, whose every subpacket is Data and holds one
--   allowed method invocation (in its canonical encoding) or a bare
--   EndOfSession.
allowedSend :: [Allowed] -> Request -> Either Text ()
allowedSend allowed q = do
  DecodedFrame (ComPacketFrame h packets) ds rest <- first (("the payload is no ComPacket: " <>) . tshow) (decodeComPacket (payload q))
  unless (null ds) (Left ("the ComPacket reads with differences: " <> tshow ds))
  unless (B.all (== 0) rest) (Left "bytes other than zeros after the ComPacket")
  unless (comPacketComID h == comId q) (Left ("the ComPacket's ComID " <> hex (comPacketComID h) <> " is not the command's"))
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
  | invoker == uSMUID && method == uProperties   = need allowed AllowProperties "SMUID.Properties"
  | invoker == uSMUID && method == uStartSession =
      need allowed AllowStartSessionAdminSP "SMUID.StartSession" >> startSessionAllowed ps
  | otherwise = Left ("the method " <> named method <> " on " <> tshow invoker <> " is not allowed")
  where named u = maybe (tshow u) (\n -> T.pack n <> " (" <> tshow u <> ")") (L.lookup u (sessionManagerMethodUIDs <> methodIDUIDs))

-- | StartSession's SPID the Admin SP's, and its HostSigningAuthority none or
--   Anybody: each parameter found by its name in the generated signature
--   (Core 5.2.3.1), a required one by position, an optional one by its
--   0-based index among the optional ones (3.2.4.1).
startSessionAllowed :: Parameters -> Either Text ()
startSessionAllowed (Parameters positional nameds) = do
  spid <- maybe (Left "StartSession without its SPID") Right (listToMaybe . (`drop` positional) =<< L.findIndex (named "SPID") (sigRequired sig))
  unless (isUid uSP_Admin spid) (Left "StartSession to an SP other than the Admin SP")
  case L.findIndex (named "HostSigningAuthority") (sigOptional sig) of
    Nothing -> Left "StartSession's signature names no HostSigningAuthority"
    Just i  -> case [ v | NamedValue (Name (Datum n)) v <- nameds, n == Unsigned (fromIntegral i) ] of
      []                                -> Right ()
      [v] | isUid uAuthority_Anybody v -> Right ()
      _                                 -> Left "StartSession with a HostSigningAuthority other than Anybody"
  where
    sig = signature @Methods.StartSession
    named n = (== n) . paramName

-- | Whether a parameter is a UID's bytes.
isUid :: UID -> Value -> Bool
isUid u v = mtoken v == Just (token u)

need :: [Allowed] -> Allowed -> Text -> Either Text ()
need allowed a what
  | a `elem` allowed = Right ()
  | otherwise        = Left (what <> " is not in the allow list")


-- * Where Level 0 Discovery and ComPackets go

-- | The security protocol of Level 0 Discovery and of ComPackets on the
--   static ComIDs: 0x01 (Core 3.3.6.1, PDF 67; Test Cases Table 5, PDF 26).
--   Core Table 25 (PDF 53) prints 0x01 and 0x02 alike, "Defined in this
--   document", so the value is written here.
comPacketProtocol :: Word8
comPacketProtocol = 0x01

-- | Level 0 Discovery's ComID: Core Table 26's row "Level 0 Device
--   Discovery" (PDF 55), 0x0001. Were the row ever missing, ComID 0x0000
--   (Reserved) would stand in, which no Level 0 response comes from.
level0ComId :: Word16
level0ComId = maybe 0 fromInteger (assigned cComIDAssignments "Level 0 Device Discovery")

-- | The Kind of a Data subpacket: Core Table 19's row "Data" (PDF 43),
--   0x0000.
dataKind :: Word16
dataKind = maybe 0 fromInteger (assigned cSubpacketTypes "Data")

-- | The one value a code table's row of a name assigns.
assigned :: [CodeRow] -> Text -> Maybe Integer
assigned rows name = case [ v | CodeRow (One v) _ <- codeRowsNamed rows name ] of
  [v] -> Just v
  _   -> Nothing

-- | A number in hex, as many digits as its type has.
hex :: (Integral a, FiniteBits a) => a -> Text
hex n = "0x" <> T.justifyRight (finiteBitSize n `div` 4) '0' (T.pack (showHex n ""))
