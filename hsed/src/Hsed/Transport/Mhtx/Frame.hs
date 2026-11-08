{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.Frame
Description : The LAN leg's frames: mh_transport's hello, ping and pong, and hsed's sed.req, sed.rsp and sed.evt, as JSON text
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The frames of the LAN leg (design 6.3, 11 M5c; docs/wire.md): each one
websocket text message holding a JSON object with a string @type@
(mh_transport SPEC.md L18-19). mh_transport's own: @hello@, the server's
first frame, with its @server_id@, its broadcasts' @seq@, its heartbeat and
an @app@ object, whose @sed@ member says what TPer answers ('SedApp');
@ping@ and @pong@ (SPEC.md L23-29, L37-62). hsed's: a request ('SedReq', an
id and an 'Op': IF-SEND, IF-RECV, a reset event, describe, and the control
ops peek and release), its response on the same socket with no @seq@
('SedRsp': an interface status with its bytes, the TPer described, a view,
or a protocol error), and the seq-stamped broadcast of a reset ('SedEvt').
Payloads are standard padded canonical base64 of exactly @len@ bytes. What
arrives is sorted ('fromClient', 'fromServer'): what cannot be decoded, has
no string @type@ or a type not known is ignored, with no reply (SPEC.md
L19-21). The view's queries and answers are coded by functions ('queryValue',
'answerValue' and their parsers), no orphan instances.

Not yet written: group G1 writes the bodies; the types and the signatures
are the scaffold's (M5c's S1.2).
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

import qualified Data.Aeson               as A
import           RIO

import           Hsed.Sim.View            (SimQuery, SimSetup, SomeQuery)
import           Hsed.Transport.Mhtx.Conn (Incoming)
import           Hsed.Wire                (Event, IfStatus, Unit)


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

-- | A hello's @app.sed@, or why it does not read.
sedAppOf :: Hello -> Either Text SedApp
sedAppOf _hello = Left "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.sedAppOf)"

-- | @app.sed@ as JSON.
sedAppValue :: SedApp -> A.Value
sedAppValue _app = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.sedAppValue)"


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

-- | What a hub received, sorted.
fromClient :: Incoming -> FromClient
fromClient _incoming = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.fromClient)"

-- | What a client received, sorted.
fromServer :: Incoming -> FromServer
fromServer _incoming = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.fromServer)"


-- * What is sent

-- | A hello as its text frame.
helloText :: Hello -> ByteString
helloText _hello = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.helloText)"

-- | @{"type":"ping"}@.
pingText :: ByteString
pingText = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.pingText)"

-- | @{"type":"pong"}@.
pongText :: ByteString
pongText = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.pongText)"

-- | A request as its text frame.
reqText :: SedReq -> ByteString
reqText _req = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.reqText)"

-- | A response as its text frame.
rspText :: SedRsp -> ByteString
rspText _rsp = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.rspText)"

-- | A reset's broadcast as its text frame.
evtText :: SedEvt -> ByteString
evtText _evt = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.evtText)"

-- | A broadcast (the hooks' @/emit@) as its text frame, its @seq@ the hub's.
broadcastText :: Integer -> A.Object -> ByteString
broadcastText _seq _object = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.broadcastText)"


-- * The view's queries and answers

-- | A query as JSON.
queryValue :: SimQuery a -> A.Value
queryValue _query = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.queryValue)"

-- | A query read from JSON, or why not.
parseQuery :: A.Value -> Either Text SomeQuery
parseQuery _value = Left "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.parseQuery)"

-- | A query's answer as JSON.
answerValue :: SimQuery a -> a -> A.Value
answerValue _query _answer = error "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.answerValue)"

-- | A query's answer read from JSON, or why not.
parseAnswer :: SimQuery a -> A.Value -> Either Text a
parseAnswer _query _value = Left "not yet written: group G1 (Hsed.Transport.Mhtx.Frame.parseAnswer)"
