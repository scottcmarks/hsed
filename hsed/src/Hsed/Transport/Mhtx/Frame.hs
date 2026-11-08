{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-|
Module      : Hsed.Transport.Mhtx.Frame
Description : The LAN leg's frames: mh_transport's hello, ping and pong, and hsed's sed.req, sed.rsp and sed.evt, as JSON text
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The frames of the LAN leg (design 6.3, 11 M5c; docs/wire.md, the client
contract): each one websocket text message holding a JSON object with a
string @type@ (mh_transport SPEC.md L18-19). mh_transport's own: @hello@,
the server's first frame, with its @server_id@, its broadcasts' @seq@, its
heartbeat and an @app@ object, whose @sed@ member says what TPer answers
('SedApp'); @ping@ and @pong@ (SPEC.md L23-29, L37-62). hsed's: a request
('SedReq', an id and an 'Op': IF-SEND, IF-RECV, a reset event, describe,
and the control ops peek and release), its response on the same socket with
no @seq@ ('SedRsp': an interface status with its bytes, the TPer described,
a view, or a protocol error), and the seq-stamped broadcast of a reset
('SedEvt'). What arrives is sorted ('fromClient', 'fromServer'): what is not
UTF-8 JSON, is no object, has no string @type@ or a type not known is
ignored, with no reply (SPEC.md L19-21); a @sed.req@ that does not read is
refused ('ClientBadReq', which the hub answers @protocol_error@), its id
given back where it was an integer. The view's queries and answers are coded
by functions ('queryValue', 'answerValue' and their parsers), no orphan
instances.

What M5c decides where design 6.3 is silent (docs/wire.md): payloads are
standard padded canonical base64 (RFC 4648 4) of exactly @len@ bytes, @len@
always bytes and at most 1 MiB ("Hsed.Wire".'mostCarried'); an id is an
integer from 1 to 'mostId'; numbers are read as aeson reads them, which
takes @2.0@ and @1e3@ for integers, each held to its range here and never
turned into an integer past 64 bits; members not named are ignored; a
member written twice is read as aeson reads it (the first), hsed writing
none twice. hsed's own frames are written @type@ first, the rest in the
order the docs give them, integers as JSON integers (protocol.dart:94-98
needs them so in hello); a broadcast's members are its emitter's.
-}
module Hsed.Transport.Mhtx.Frame
  ( -- * hello
    Hello (..)
  , Allow (..)
  , SedApp (..)
  , sedAppOf
  , sedAppValue
    -- * Requests, responses, events
  , Op (..)
  , SedReq (..)
  , Reply (..)
  , SedRsp (..)
  , SedEvt (..)
  , mostId
    -- * What arrives, sorted
  , FromClient (..)
  , FromServer (..)
  , fromClient
  , fromServer
    -- * What is sent
  , helloText
  , pingText
  , pongText
  , reqText
  , rspText
  , evtText
  , broadcastText
    -- * The view's queries and answers
  , queryValue
  , parseQuery
  , answerValue
  , parseAnswer
  ) where

import           Data.Aeson               ((.=))
import qualified Data.Aeson               as A
import qualified Data.Aeson.Encoding      as AE
import qualified Data.Aeson.Key           as K
import qualified Data.Aeson.KeyMap        as KM
import           Data.ByteArray.Encoding  (Base (Base64), convertFromBase, convertToBase)
import qualified Data.ByteString.Base16   as B16
import qualified GHC.Exts                 as Exts
import qualified RIO.ByteString           as B
import qualified RIO.ByteString.Lazy      as BL
import qualified Data.IntMap.Strict        as IntMap
import qualified RIO.List                 as L
import           RIO
import qualified RIO.Text                 as T

import           Hsed.Core.Table          (Cell (..))
import           Hsed.Core.UID            (UID, uid)
import           Hsed.Sim.Flags           (lockingOFSName, unitName)
import           Hsed.Sim.Types           (ByteTable (..), ComIdLife (..), SyncState (..))
import           Hsed.Sim.View            (ComIdView (..), Credential (..), SimQuery (..), SimSetup (..), SomeQuery (..))
import           Hsed.Transport.Mhtx.Conn (Incoming (..))
import           Hsed.Wire                (Event (..), IfStatus (..), Unit (..), ifStatusDetail, ifStatusName, mostCarried)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hsed.Transport.Mhtx.Conn (Incoming (..))

-- * hello

-- | mh_transport's hello (SPEC.md L37-53): its version (1), the server's id
--   (12 characters of [a-z0-9]), the broadcasts so far, the heartbeat's
--   interval and timeout, and the application's object.
data Hello = Hello
  { helloV          :: !Int
  , helloServerId   :: !Text
  , helloSeq        :: !Integer
  , helloIntervalMs :: !Int
  , helloTimeoutMs  :: !Int
  , helloApp        :: !A.Object
  }
  deriving (Eq, Show)

-- | What a hub allows a client besides commands and @describe@: peek and
--   release with @--allow-remote-control@, reset with @--allow-remote-reset@.
data Allow
  = AllowPeek
  | AllowRelease
  | AllowReset
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | hello's @app.sed@: the TPer that answers (its role, @tper@; its
--   implementation; its unit; its SSC and errata; its base ComID and how
--   many), what the hub allows, and the simulator's setup.
data SedApp = SedApp
  { saRole      :: !Text
  , saImpl      :: !Text
  , saUnits     :: !Unit
  , saSsc       :: !(Maybe Text)
  , saErrata    :: !(Maybe Text)
  , saBaseComId :: !(Maybe Word16)
  , saNComIds   :: !(Maybe Word16)
  , saAllows    :: ![Allow]
  , saSetup     :: !(Maybe SimSetup)
  }
  deriving (Eq, Show)

-- | A hello's @app.sed@, or why it does not read: an object whose @v@ is
--   1, with a string @role@ and @impl@, @units@ @bytes@ or @blocks512@;
--   @ssc@, @errata@, @base_comid@, @n_comids@, @allows@ (each word once,
--   in any order) and @setup@ absent or null where there are none.
--
-- >>> sedAppOf (Hello 1 "q7d2m0s8x1kz" 0 20000 60000 mempty)
-- Left "app.sed: missing"
sedAppOf :: Hello -> Either Text SedApp
sedAppOf h = first ("app." <>) (required "sed" (asObject >=> sedAppFrom) (helloApp h))

-- | @app.sed@ as JSON: what 'sedAppOf' reads, members with nothing to say
--   left out (design 6.3's hello has neither @allows@ nor @setup@).
sedAppValue :: SedApp -> A.Value
sedAppValue a = A.object $
  [ "v"     .= sedVersion
  , "role"  .= saRole a
  , "impl"  .= saImpl a
  , "units" .= unitName (saUnits a)
  ]
  <> [ "ssc"        .= v | Just v <- [saSsc a] ]
  <> [ "errata"     .= v | Just v <- [saErrata a] ]
  <> [ "base_comid" .= v | Just v <- [saBaseComId a] ]
  <> [ "n_comids"   .= v | Just v <- [saNComIds a] ]
  <> [ "allows"     .= map allowName (saAllows a) | not (null (saAllows a)) ]
  <> [ "setup"      .= setupValue s | Just s <- [saSetup a] ]

-- | The version of @app.sed@ (design 6.3).
sedVersion :: Int
sedVersion = 1

-- | @app.sed@ read from its object.
sedAppFrom :: A.Object -> Either Text SedApp
sedAppFrom o = do
  v <- required "v" bounded o
  unless (v == sedVersion) (Left ("v: " <> tshow v <> ", not " <> tshow sedVersion))
  SedApp
    <$> required "role" asText o
    <*> required "impl" asText o
    <*> required "units" (asText >=> named unitName) o
    <*> optionalMember "ssc" asText o
    <*> optionalMember "errata" asText o
    <*> optionalMember "base_comid" bounded o
    <*> optionalMember "n_comids" bounded o
    <*> (fromMaybe [] <$> optionalMember "allows" (asList (asText >=> named allowName) >=> once) o)
    <*> optionalMember "setup" (asObject >=> setupFrom) o
  where
    once as = if length (L.nub as) == length as then Right as else Left "a word twice"

-- | A word of @app.sed.allows@: the op it allows.
allowName :: Allow -> Text
allowName = \case
  AllowPeek    -> "peek"
  AllowRelease -> "release"
  AllowReset   -> "reset"

-- | A simulator's setup as JSON, its members named after hsed-sim's flags.
setupValue :: SimSetup -> A.Value
setupValue s = A.object $
  [ "units"                  .= unitName (ssUnits s)
  , "hold_ifrecvs"           .= ssHold s
  , "locking_ofs"            .= lockingOFSName (ssLockingOFS s)
  , "inactive_comid_discard" .= ssDiscard s
  ]
  <> [ "sim_sid_pin" .= v | Just v <- [ssSidPin s] ]
  <> [ "msid"        .= v | Just v <- [ssMsid s] ]
  <> [ "profile"     .= v | Just v <- [ssProfile s] ]
  <> [ "params"      .= ssParams s ]

-- | A simulator's setup read from its object.
setupFrom :: A.Object -> Either Text SimSetup
setupFrom o = SimSetup
  <$> required "units" (asText >=> named unitName) o
  <*> required "hold_ifrecvs" (bounded >=> atLeast 0) o
  <*> required "locking_ofs" (asText >=> named lockingOFSName) o
  <*> required "inactive_comid_discard" asBool o
  <*> optionalMember "sim_sid_pin" asText o
  <*> optionalMember "msid" asText o
  <*> optionalMember "profile" asText o
  <*> required "params" asText o


-- * Requests, responses, events

-- | What a request asks: IF-SEND (protocol, ComID, payload), IF-RECV
--   (protocol, ComID, transfer length), a reset event, the TPer described,
--   a query of its state, a ComID's prepared response released.
data Op
  = OpIfSend !Word8 !Word16 !ByteString
  | OpIfRecv !Word8 !Word16 !Word32
  | OpReset !Event
  | OpDescribe
  | OpPeek !SomeQuery
  | OpRelease !Word16
  deriving (Eq, Show)

-- | A request: its id (positive, at most 'mostId', increasing per socket)
--   and its op.
data SedReq = SedReq
  { reqId :: !Integer
  , reqOp :: !Op
  }
  deriving (Eq, Show)

-- | A response's substance: an interface status and the bytes an IF-RECV
--   returns, the TPer described, a query's answer, or a protocol error.
data Reply
  = Answered !IfStatus !ByteString
  | Described !SedApp
  | Viewed !A.Value
  | ProtocolError !Text
  deriving (Eq, Show)

-- | A response, on the requesting socket alone, with no @seq@: the request's
--   id (none where a refused request's id was not an integer) and its reply.
data SedRsp = SedRsp
  { rspId    :: !(Maybe Integer)
  , rspReply :: !Reply
  }
  deriving (Eq, Show)

-- | A reset's broadcast: its @seq@, the event, the ComIDs reset and the
--   sessions aborted (ComID, TSN).
data SedEvt = SedEvt
  { evtSeq      :: !Integer
  , evtEvent    :: !Event
  , evtComIds   :: ![Word16]
  , evtSessions :: ![(Word16, Word32)]
  }
  deriving (Eq, Show)

-- | The largest id: 2^53 - 1, the largest integer a JSON number carries
--   exactly to a reader that holds numbers as IEEE 754 doubles (Dart on the
--   web, JavaScript), the bound the protocol rules give ids (design 6.3 as
--   M5c decides it).
mostId :: Integer
mostId = 9007199254740991

-- | The most bytes a request or a response carries, @len@'s bound: 1 MiB,
--   whatever the unit (design 6.3; "Hsed.Wire".'mostCarried').
mostLen :: Integer
mostLen = toInteger (mostCarried UnitBytes)

-- | The most a @seq@ is read as: a signed 64-bit integer's, a Dart @int@'s.
mostSeq :: Integer
mostSeq = toInteger (maxBound :: Int64)


-- * What arrives, sorted

-- | What a hub receives: a ping, a request, a request refused (its id where
--   it was an integer, why), or anything else, ignored.
data FromClient
  = ClientPing
  | ClientReq !SedReq
  | ClientBadReq !(Maybe Integer) !Text
  | ClientIgnored
  deriving (Eq, Show)

-- | What a client receives: a hello, a ping, a pong, a response, a reset's
--   broadcast, another broadcast (its @seq@), or anything else, ignored.
data FromServer
  = ServerHello !Hello
  | ServerPing
  | ServerPong
  | ServerRsp !SedRsp
  | ServerEvt !SedEvt
  | ServerBroadcast !Integer
  | ServerIgnored
  deriving (Eq, Show)

-- | What a hub received, sorted: @ping@; a @sed.req@ that reads, or one
--   refused, with its id where it was an integer (within 64 bits) and why;
--   anything else ignored, a client's @hello@ and @pong@ included, and
--   binary messages (which the hub counts as liveness all the same, SPEC.md
--   L21 and L58-59).
--
-- >>> fromClient (InText "{\"type\":\"ping\"}")
-- ClientPing
-- >>> fromClient (InText "{\"type\":\"sed.req\",\"id\":7,\"op\":\"describe\"}")
-- ClientReq (SedReq {reqId = 7, reqOp = OpDescribe})
-- >>> fromClient (InText "{\"type\":\"sed.req\",\"id\":0,\"op\":\"describe\"}")
-- ClientBadReq (Just 0) "id: an integer from 1 to 9007199254740991, not 0"
-- >>> fromClient (InText "{\"type\":\"sed.req\",\"id\":8,\"op\":\"if_send\",\"proto\":1,\"comid\":4100,\"len\":1,\"payload\":\"AB==\"}")
-- ClientBadReq (Just 8) "payload: not canonical base64"
-- >>> map fromClient [InText "this is not json", InText "[1]", InText "{\"type\":\"conformance_bogus\"}", InBinary "\0"]
-- [ClientIgnored,ClientIgnored,ClientIgnored,ClientIgnored]
fromClient :: Incoming -> FromClient
fromClient incoming = case frameOf incoming of
  Just ("ping", _)    -> ClientPing
  Just ("sed.req", o) -> either (uncurry ClientBadReq) ClientReq (requestFrom o)
  _                   -> ClientIgnored

-- | What a client received, sorted: a @hello@ that reads (the @app@ object
--   as it came, an empty one where it is not an object, as protocol.dart:99
--   takes it); @ping@ and @pong@; a @sed.rsp@ that reads and carries no
--   @seq@ (a SPEC client takes any integer @seq@ for the broadcasts' count,
--   client.dart:390-393, so a response carrying one is ignored); a
--   @sed.evt@ that reads; any other frame with an integer @seq@ a broadcast,
--   a @sed.evt@ that does not read included; anything else ignored.
--
-- >>> fromServer (InText "{\"type\":\"sed.rsp\",\"id\":2,\"status\":\"good\",\"len\":0}")
-- ServerRsp (SedRsp {rspId = Just 2, rspReply = Answered Good ""})
-- >>> fromServer (InText "{\"type\":\"sed.rsp\",\"id\":2,\"status\":\"good\",\"len\":0,\"seq\":3}")
-- ServerIgnored
-- >>> fromServer (InText "{\"type\":\"tick\",\"seq\":3}")
-- ServerBroadcast 3
fromServer :: Incoming -> FromServer
fromServer incoming = case frameOf incoming of
  Nothing             -> ServerIgnored
  Just ("hello", o)   -> either (const ServerIgnored) ServerHello (helloFrom o)
  Just ("ping", _)    -> ServerPing
  Just ("pong", _)    -> ServerPong
  Just ("sed.rsp", o)
    | KM.member "seq" o -> ServerIgnored
    | otherwise         -> either (const ServerIgnored) ServerRsp (responseFrom o)
  Just ("sed.evt", o) -> either (const (broadcast o)) ServerEvt (eventFrom o)
  Just (_, o)         -> broadcast o
  where
    broadcast = either (const ServerIgnored) ServerBroadcast . required "seq" (ranged 0 mostSeq)

-- | A text message's JSON object and its type: valid UTF-8, one JSON value,
--   an object, a string @type@ (SPEC.md L18-21).
frameOf :: Incoming -> Maybe (Text, A.Object)
frameOf = \case
  InText bytes
    | Right _ <- decodeUtf8' bytes
    , Just (A.Object o) <- A.decodeStrict' bytes
    , Just (A.String t) <- KM.lookup "type" o
    -> Just (t, o)
  _ -> Nothing

-- | A @sed.req@ read, or refused: its id where it is an integer, and why.
requestFrom :: A.Object -> Either (Maybe Integer, Text) SedReq
requestFrom o = do
  i  <- first (echoed,) (required "id" (ranged 1 mostId) o)
  op <- first (Just i,) (opFrom o)
  pure (SedReq i op)
  where
    echoed = toInteger <$> (KM.lookup "id" o >>= integral :: Maybe Int64)

-- | A request's op and its members: IF-SEND's @len@ at most 1 MiB and its
--   payload exactly @len@ bytes, IF-RECV's @len@ at most 1 MiB.
opFrom :: A.Object -> Either Text Op
opFrom o = required "op" asText o >>= \case
  "if_send"  -> do
    p     <- required "proto" bounded o
    c     <- required "comid" bounded o
    len   <- required "len" (ranged 0 mostLen) o
    bytes <- required "payload" (asText >=> base64) o
    unless (toInteger (B.length bytes) == len) $
      Left ("payload: " <> tshow (B.length bytes) <> " bytes, not len " <> tshow len)
    pure (OpIfSend p c bytes)
  "if_recv"  -> OpIfRecv <$> required "proto" bounded o <*> required "comid" bounded o
                         <*> (fromInteger <$> required "len" (ranged 0 mostLen) o)
  "reset"    -> OpReset <$> required "event" (asText >=> named eventName) o
  "describe" -> pure OpDescribe
  "peek"     -> OpPeek <$> required "query" parseQuery o
  "release"  -> OpRelease <$> required "comid" bounded o
  other      -> Left ("op: " <> notOneOf other ["if_send", "if_recv", "reset", "describe", "peek", "release"])

-- | A @hello@ read: @v@, @server_id@, @seq@ and the heartbeat's
--   @interval_ms@ and @timeout_ms@ as protocol.dart:88-108 reads them, each
--   an integer, @seq@ not negative.
helloFrom :: A.Object -> Either Text Hello
helloFrom o = Hello
  <$> required "v" bounded o
  <*> required "server_id" asText o
  <*> required "seq" (ranged 0 mostSeq) o
  <*> required "heartbeat" (asObject >=> required "interval_ms" bounded) o
  <*> required "heartbeat" (asObject >=> required "timeout_ms" bounded) o
  <*> pure (case KM.lookup "app" o of
              Just (A.Object app) -> app
              _                   -> mempty)

-- | A @sed.rsp@ read.
responseFrom :: A.Object -> Either Text SedRsp
responseFrom o = SedRsp <$> required "id" idOrNull o <*> replyFrom
  where
    idOrNull = \case
      A.Null -> Right Nothing
      v      -> Just <$> ranged (toInteger (minBound :: Int64)) mostSeq v
    replyFrom
      | Just d <- KM.lookup "describe" o = Described <$> first ("describe: " <>) (asObject d >>= sedAppFrom)
      | Just v <- KM.lookup "view" o     = Right (Viewed v)
      | otherwise                        = required "status" asText o >>= \case
          "protocol_error" -> ProtocolError . fromMaybe "" <$> optionalMember "detail" asText o
          name             -> do
            detail <- fromMaybe "" <$> optionalMember "detail" asText o
            s      <- maybe (Left ("status: " <> notOneOf name ("protocol_error" : map ifStatusName (statuses detail)))) Right
                        (L.find ((== name) . ifStatusName) (statuses detail))
            len    <- required "len" (ranged 0 mostLen) o
            bytes  <- if len == 0 then fromMaybe mempty <$> optionalMember "payload" (asText >=> base64) o
                                  else required "payload" (asText >=> base64) o
            unless (toInteger (B.length bytes) == len) $
              Left ("payload: " <> tshow (B.length bytes) <> " bytes, not len " <> tshow len)
            pure (Answered s bytes)
    -- every status, those that carry a detail with the one the frame gave
    statuses detail = [ Good, InvalidSecurityProtocolId, InvalidTransferLengthOnIfSend, OtherInvalidCommandParameter
                      , SynchronousProtocolViolation, DataProtectionError, TransportFailure detail, Unsupported detail ]

-- | A @sed.evt@ read.
eventFrom :: A.Object -> Either Text SedEvt
eventFrom o = SedEvt
  <$> required "seq" (ranged 0 mostSeq) o
  <*> required "event" (asText >=> named eventName) o
  <*> required "comids_reset" (asList bounded) o
  <*> required "sessions_aborted" (asList pairOf) o


-- * What is sent

-- | A hello as its text frame, its members in SPEC.md L45-53's order, the
--   @app@ object as given.
--
-- >>> helloText (Hello 1 "q7d2m0s8x1kz" 0 20000 60000 mempty)
-- "{\"type\":\"hello\",\"v\":1,\"server_id\":\"q7d2m0s8x1kz\",\"seq\":0,\"heartbeat\":{\"interval_ms\":20000,\"timeout_ms\":60000},\"app\":{}}"
helloText :: Hello -> ByteString
helloText h = frame "hello" $
  "v"           .= helloV h
  <> "server_id" .= helloServerId h
  <> "seq"       .= helloSeq h
  <> "heartbeat" .= A.object [ "interval_ms" .= helloIntervalMs h, "timeout_ms" .= helloTimeoutMs h ]
  <> "app"       .= helloApp h

-- | @{"type":"ping"}@.
--
-- >>> pingText
-- "{\"type\":\"ping\"}"
pingText :: ByteString
pingText = frame "ping" mempty

-- | @{"type":"pong"}@.
pongText :: ByteString
pongText = frame "pong" mempty

-- | A request as its text frame: @id@, @op@, then the op's members; an
--   IF-SEND's @len@ is its payload's length.
--
-- >>> reqText (SedReq 3 (OpIfRecv 1 4100 2048))
-- "{\"type\":\"sed.req\",\"id\":3,\"op\":\"if_recv\",\"proto\":1,\"comid\":4100,\"len\":2048}"
reqText :: SedReq -> ByteString
reqText r = frame "sed.req" $ "id" .= reqId r <> case reqOp r of
  OpIfSend p c bytes -> "op" .= ("if_send" :: Text) <> "proto" .= p <> "comid" .= c <> "len" .= B.length bytes <> "payload" .= base64Text bytes
  OpIfRecv p c len   -> "op" .= ("if_recv" :: Text) <> "proto" .= p <> "comid" .= c <> "len" .= len
  OpReset e          -> "op" .= ("reset" :: Text) <> "event" .= eventName e
  OpDescribe         -> "op" .= ("describe" :: Text)
  OpPeek (SomeQuery q) -> "op" .= ("peek" :: Text) <> "query" .= queryValue q
  OpRelease c        -> "op" .= ("release" :: Text) <> "comid" .= c

-- | A response as its text frame, with no @seq@: @id@ (null where the
--   request's was not an integer), then the status, its detail where it
--   has one, @len@ and the payload where @len@ is not 0; or @describe@; or
--   @view@; or the status @protocol_error@ and its detail.
--
-- >>> rspText (SedRsp (Just 4) (Answered SynchronousProtocolViolation ""))
-- "{\"type\":\"sed.rsp\",\"id\":4,\"status\":\"synchronous_protocol_violation\",\"len\":0}"
-- >>> rspText (SedRsp Nothing (ProtocolError "id: missing"))
-- "{\"type\":\"sed.rsp\",\"id\":null,\"status\":\"protocol_error\",\"detail\":\"id: missing\"}"
rspText :: SedRsp -> ByteString
rspText r = frame "sed.rsp" $ "id" .= rspId r <> case rspReply r of
  Answered s bytes -> "status" .= ifStatusName s <> foldMap ("detail" .=) (ifStatusDetail s) <> "len" .= B.length bytes
                      <> (if B.null bytes then mempty else "payload" .= base64Text bytes)
  Described app    -> "describe" .= sedAppValue app
  Viewed v         -> "view" .= v
  ProtocolError t  -> "status" .= ("protocol_error" :: Text) <> "detail" .= t

-- | A reset's broadcast as its text frame (design 6.3), each aborted
--   session a pair @[comid, tsn]@.
--
-- >>> evtText (SedEvt 7 PowerCycle [4100] [(4100, 4096)])
-- "{\"type\":\"sed.evt\",\"seq\":7,\"event\":\"power_cycle\",\"comids_reset\":[4100],\"sessions_aborted\":[[4100,4096]]}"
evtText :: SedEvt -> ByteString
evtText e = frame "sed.evt" $
  "seq"                 .= evtSeq e
  <> "event"            .= eventName (evtEvent e)
  <> "comids_reset"     .= evtComIds e
  <> "sessions_aborted" .= evtSessions e

-- | A broadcast (the hooks' @/emit@) as its text frame, its @seq@ the hub's,
--   which replaces any the object carried: the object's other members as
--   they came.
broadcastText :: Integer -> A.Object -> ByteString
broadcastText s = BL.toStrict . A.encode . A.Object . KM.insert "seq" (A.toJSON s)

-- | A frame of a type: @type@ first, then the members given.
frame :: Text -> A.Series -> ByteString
frame t members = BL.toStrict (AE.encodingToLazyByteString (A.pairs ("type" .= t <> members)))


-- * The view's queries and answers

-- | A query as JSON: its @name@, and the UIDs (16 lower-case hex digits) and
--   the column name it is of.
--
-- >>> A.encode (queryValue Msid)
-- "{\"name\":\"msid\"}"
queryValue :: SimQuery a -> A.Value
queryValue = \case
  ComIds                   -> query "comids" []
  Sessions                 -> query "sessions" []
  LifeCycleOf sp           -> query "life_cycle" [ "sp" .= uidText sp ]
  CredentialOf sp a        -> query "credential" [ "sp" .= uidText sp, "authority" .= uidText a ]
  CellOf sp table row name -> query "cell" [ "sp" .= uidText sp, "table" .= uidText table, "row" .= uidText row, "column" .= name ]
  ByteTableOf sp table     -> query "byte_table" [ "sp" .= uidText sp, "table" .= uidText table ]
  Msid                     -> query "msid" []
  LockingRanges            -> query "locking_ranges" []
  where
    query name members = A.object (("name" .= (name :: Text)) : members)

-- | A query read from JSON, or why not.
--
-- >>> parseQuery (queryValue LockingRanges)
-- Right (SomeQuery LockingRanges)
parseQuery :: A.Value -> Either Text SomeQuery
parseQuery = asObject >=> \o -> required "name" asText o >>= \case
  "comids"         -> pure (SomeQuery ComIds)
  "sessions"       -> pure (SomeQuery Sessions)
  "life_cycle"     -> SomeQuery . LifeCycleOf <$> uidAt "sp" o
  "credential"     -> (\sp a -> SomeQuery (CredentialOf sp a)) <$> uidAt "sp" o <*> uidAt "authority" o
  "cell"           -> (\sp t r n -> SomeQuery (CellOf sp t r n)) <$> uidAt "sp" o <*> uidAt "table" o <*> uidAt "row" o
                                                                 <*> required "column" asText o
  "byte_table"     -> (\sp t -> SomeQuery (ByteTableOf sp t)) <$> uidAt "sp" o <*> uidAt "table" o
  "msid"           -> pure (SomeQuery Msid)
  "locking_ranges" -> pure (SomeQuery LockingRanges)
  other            -> Left ("name: " <> notOneOf other
                               ["comids", "sessions", "life_cycle", "credential", "cell", "byte_table", "msid", "locking_ranges"])
  where
    uidAt name = required name (asText >=> uidFrom)

-- | A query's answer as JSON: a ComID's view an object (its @comid@, its
--   @life@, null for none, its @sync@, a @state@ and the packets it holds);
--   a session a pair @[comid, tsn]@; a life cycle an integer; a credential
--   its @pin@, @try_limit@ and @tries@; a cell an object of its kind
--   ('cellValue'); a byte table its @size@, @fill@ and every extent
--   @written@, as a pair @[offset, bytes]@; the MSID its bytes; UIDs 16
--   lower-case hex digits; bytes base64; none null.
--
-- >>> A.encode (answerValue Sessions [(4100, 4096)])
-- "[[4100,4096]]"
answerValue :: SimQuery a -> a -> A.Value
answerValue q a = case q of
  ComIds          -> A.toJSON (map comIdViewValue a)
  Sessions        -> A.toJSON a
  LifeCycleOf _   -> A.toJSON a
  CredentialOf {} -> maybe A.Null credentialValue a
  CellOf {}       -> maybe A.Null cellValue a
  ByteTableOf {}  -> maybe A.Null byteTableValue a
  Msid            -> base64Value a
  LockingRanges   -> A.toJSON (map uidText a)

-- | A query's answer read from JSON, or why not.
parseAnswer :: SimQuery a -> A.Value -> Either Text a
parseAnswer q v = case q of
  ComIds          -> asList (asObject >=> comIdViewFrom) v
  Sessions        -> asList pairOf v
  LifeCycleOf _   -> nullable bounded v
  CredentialOf {} -> nullable (asObject >=> credentialFrom) v
  CellOf {}       -> nullable cellFrom v
  ByteTableOf {}  -> nullable (asObject >=> byteTableFrom) v
  Msid            -> asText v >>= base64
  LockingRanges   -> asList (asText >=> uidFrom) v

comIdViewValue :: ComIdView -> A.Value
comIdViewValue cv = A.object
  [ "comid" .= cvComId cv
  , "life"  .= fmap lifeName (cvLife cv)
  , "sync"  .= case cvSync cv of
      S1    -> A.object [ "state" .= ("s1" :: Text) ]
      S2 ps -> A.object [ "state" .= ("s2" :: Text), "packets" .= map base64Text ps ]
      S3 ps -> A.object [ "state" .= ("s3" :: Text), "packets" .= map base64Text ps ]
  ]

comIdViewFrom :: A.Object -> Either Text ComIdView
comIdViewFrom o = ComIdView
  <$> required "comid" bounded o
  <*> optionalMember "life" (asText >=> named lifeName) o
  <*> required "sync" (asObject >=> syncFrom) o
  where
    syncFrom s = required "state" asText s >>= \case
      "s1"  -> pure S1
      "s2"  -> S2 <$> packets s
      "s3"  -> S3 <$> packets s
      other -> Left ("state: " <> notOneOf other ["s1", "s2", "s3"])
    packets = required "packets" (asList (asText >=> base64))

-- | A ComID's life as the answers name it (Core 3.3.3, PDF 56).
lifeName :: ComIdLife -> Text
lifeName = \case
  Inactive   -> "inactive"
  Issued     -> "issued"
  Associated -> "associated"

credentialValue :: Credential -> A.Value
credentialValue cr = A.object [ "pin" .= base64Text (crPIN cr), "try_limit" .= crTryLimit cr, "tries" .= crTries cr ]

credentialFrom :: A.Object -> Either Text Credential
credentialFrom o = Credential
  <$> required "pin" (asText >=> base64) o
  <*> required "try_limit" natural o
  <*> required "tries" natural o

-- | A cell as JSON: an object whose @cell@ names its kind (Hsed.Core.Table's
--   eight), with its members: @uid@; @text@; @enum@'s @label@ and @value@;
--   @ref@'s @name@ and @uid@; @uint@'s and @int@'s @value@; @bytes@'
--   @bytes@; @list@'s @cells@.
cellValue :: Cell -> A.Value
cellValue = \case
  UIDCell u     -> kind "uid" [ "uid" .= uidText u ]
  TextCell t    -> kind "text" [ "text" .= t ]
  EnumCell l n  -> kind "enum" [ "label" .= l, "value" .= n ]
  RefCell n u   -> kind "ref" [ "name" .= n, "uid" .= uidText u ]
  UIntCell n    -> kind "uint" [ "value" .= n ]
  IntCell n     -> kind "int" [ "value" .= n ]
  BytesCell bs  -> kind "bytes" [ "bytes" .= base64Text bs ]
  ListCell cs   -> kind "list" [ "cells" .= map cellValue cs ]
  where
    kind k members = A.object (("cell" .= (k :: Text)) : members)

cellFrom :: A.Value -> Either Text Cell
cellFrom = asObject >=> \o -> required "cell" asText o >>= \case
  "uid"   -> UIDCell <$> required "uid" (asText >=> uidFrom) o
  "text"  -> TextCell <$> required "text" asText o
  "enum"  -> EnumCell <$> required "label" asText o <*> required "value" bounded o
  "ref"   -> RefCell <$> required "name" asText o <*> required "uid" (asText >=> uidFrom) o
  "uint"  -> UIntCell <$> required "value" integer64 o
  "int"   -> IntCell <$> required "value" integer64 o
  "bytes" -> BytesCell <$> required "bytes" (asText >=> base64) o
  "list"  -> ListCell <$> required "cells" (asList cellFrom) o
  other   -> Left ("cell: " <> tshow other <> " is not one of uid, text, enum, ref, uint, int, bytes, list")

byteTableValue :: ByteTable -> A.Value
byteTableValue bt = A.object
  [ "size"    .= btSize bt
  , "fill"    .= btFill bt
  , "written" .= [ (offset, base64Text bytes) | (offset, bytes) <- IntMap.toAscList (btWritten bt) ]
  ]

byteTableFrom :: A.Object -> Either Text ByteTable
byteTableFrom o = ByteTable
  <$> required "size" natural o
  <*> required "fill" bounded o
  <*> required "written" (asList extent >=> ascending) o
  where
    extent = asList Right >=> \case
      [offset, bytes] -> (,) <$> first ("offset: " <>) (bounded offset >>= atLeast 0) <*> first ("bytes: " <>) (asText bytes >>= base64)
      _               -> Left "an [offset, bytes] pair"
    ascending es
      | and (zipWith (<) offsets (drop 1 offsets)) = Right (IntMap.fromDistinctAscList es)
      | otherwise                                  = Left "the extents' offsets not ascending"
      where offsets = map fst es


-- * Reading JSON

-- | A member of an object, read: missing, or why it does not read, each
--   with its name.
required :: Text -> (A.Value -> Either Text a) -> A.Object -> Either Text a
required name reader o = case KM.lookup (K.fromText name) o of
  Nothing -> Left (name <> ": missing")
  Just v  -> first ((name <> ": ") <>) (reader v)

-- | A member of an object that may be absent or null.
optionalMember :: Text -> (A.Value -> Either Text a) -> A.Object -> Either Text (Maybe a)
optionalMember name reader o = case KM.lookup (K.fromText name) o of
  Nothing     -> Right Nothing
  Just A.Null -> Right Nothing
  Just v      -> bimap ((name <> ": ") <>) Just (reader v)

-- | A value that may be null.
nullable :: (A.Value -> Either Text a) -> A.Value -> Either Text (Maybe a)
nullable reader = \case
  A.Null -> Right Nothing
  v      -> Just <$> reader v

asObject :: A.Value -> Either Text A.Object
asObject = \case
  A.Object o -> Right o
  v          -> Left ("an object, not " <> jsonText v)

asText :: A.Value -> Either Text Text
asText = \case
  A.String t -> Right t
  v          -> Left ("a string, not " <> jsonText v)

asBool :: A.Value -> Either Text Bool
asBool = \case
  A.Bool b -> Right b
  v        -> Left ("true or false, not " <> jsonText v)

-- | An array, each element read, the first that does not read named by its
--   index.
asList :: (A.Value -> Either Text a) -> A.Value -> Either Text [a]
asList reader = \case
  A.Array vs -> zipWithM (\i v -> first (\why -> "[" <> tshow i <> "]: " <> why) (reader v)) [0 :: Int ..] (toList vs)
  v          -> Left ("an array, not " <> jsonText v)

-- | A pair @[a, b]@ of two integers: a session's ComID and TSN.
pairOf :: (A.FromJSON a, A.ToJSON a, Bounded a, Integral a, A.FromJSON b, A.ToJSON b, Bounded b, Integral b)
       => A.Value -> Either Text (a, b)
pairOf = asList Right >=> \case
  [a, b] -> (,) <$> bounded a <*> bounded b
  _      -> Left "a pair of integers"

-- | A JSON number as an integral type it fits, if it is an integer within
--   the type's range: aeson's reading, which refuses a fraction and a value
--   out of range and turns no huge exponent into a huge integer, held to
--   the number it read (so that no reading rounds).
integral :: (A.FromJSON i, A.ToJSON i) => A.Value -> Maybe i
integral v = case v of
  A.Number _ | A.Success i <- A.fromJSON v, A.toJSON i == v -> Just i
  _                                                         -> Nothing

-- | An integer within a bounded type's range.
bounded :: forall i. (A.FromJSON i, A.ToJSON i, Bounded i, Integral i) => A.Value -> Either Text i
bounded v = maybe (Left (between (toInteger (minBound :: i)) (toInteger (maxBound :: i)) v)) Right (integral v)

-- | An integer within a range (inside a signed 64-bit integer's).
ranged :: Integer -> Integer -> A.Value -> Either Text Integer
ranged lo hi v = case toInteger <$> (integral v :: Maybe Int64) of
  Just n | n >= lo && n <= hi -> Right n
  _                           -> Left (between lo hi v)

-- | An integer a 64-bit word or a signed 64-bit integer holds: a cell's
--   (uinteger_8 and integer_8 at the most).
integer64 :: A.Value -> Either Text Integer
integer64 v = maybe (Left (between (toInteger (minBound :: Int64)) (toInteger (maxBound :: Word64)) v)) Right
  ((toInteger <$> (integral v :: Maybe Word64)) <|> (toInteger <$> (integral v :: Maybe Int64)))

-- | A count: an integer a 64-bit word holds.
natural :: A.Value -> Either Text Natural
natural = fmap fromIntegral . (bounded :: A.Value -> Either Text Word64)

atLeast :: (Ord a, Show a) => a -> a -> Either Text a
atLeast least n = if n >= least then Right n else Left (tshow n <> ", below " <> tshow least)

between :: Integer -> Integer -> A.Value -> Text
between lo hi v = "an integer from " <> tshow lo <> " to " <> tshow hi <> ", not " <> jsonText v

-- | An enumeration's element by its name.
named :: (Bounded a, Enum a) => (a -> Text) -> Text -> Either Text a
named name t = maybe (Left (notOneOf t (map name [minBound .. maxBound]))) Right
  (L.lookup t [ (name a, a) | a <- [minBound .. maxBound] ])

-- | Why a word is none of those a member takes.
notOneOf :: Text -> [Text] -> Text
notOneOf t ws = tshow t <> " is not one of " <> T.intercalate ", " ws

-- | A value as compact JSON, cut to 40 characters, for a reason.
jsonText :: A.Value -> Text
jsonText v = let t = decodeUtf8Lenient (BL.toStrict (A.encode v)) in if T.length t > 40 then T.take 40 t <> "..." else t


-- * Bytes, UIDs, events

-- | Bytes as standard padded base64 (RFC 4648 4).
base64Text :: ByteString -> Text
base64Text = decodeUtf8Lenient . convertToBase Base64

base64Value :: ByteString -> A.Value
base64Value = A.String . base64Text

-- | Bytes from standard padded base64, canonical alone: the text the bytes
--   encode to (memory's decoder takes @AB==@, whose unused bits are not
--   zero, for the byte @AA==@ encodes).
base64 :: Text -> Either Text ByteString
base64 t = case convertFromBase Base64 (encodeUtf8 t) of
  Left _ -> Left "not standard padded base64"
  Right bytes
    | base64Text bytes == t -> Right bytes
    | otherwise             -> Left "not canonical base64"

-- | A UID as 16 lower-case hex digits.
uidText :: UID -> Text
uidText = decodeUtf8Lenient . B16.encode . B.pack . Exts.toList

-- | A UID from 16 lower-case hex digits.
uidFrom :: Text -> Either Text UID
uidFrom t
  | T.length t == 16, T.all (`elem` ("0123456789abcdef" :: String)) t
  , Right [b7, b6, b5, b4, b3, b2, b1, b0] <- B.unpack <$> B16.decode (encodeUtf8 t)
              = Right (uid b7 b6 b5 b4 b3 b2 b1 b0)
  | otherwise = Left (tshow t <> " is not 16 lower-case hex digits")

-- | An event as design 6.3's frames name it (Hsed.Wire.Internal's words).
eventName :: Event -> Text
eventName = \case
  PowerCycle    -> "power_cycle"
  HardwareReset -> "hardware_reset"
  HotPlug       -> "hot_plug"
