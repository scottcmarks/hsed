{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.Conn
Description : One websocket of mh_transport frames, abstracted; the one socket hsed binds (127.0.0.1) and the one it dials
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The LAN leg's connections (design 6.3, 7 items 4-5, 11 M5c): a websocket as
the hub and the client see it ('Conn': what arrives, text, binary or the
close with its code and reason; sending text or binary; closing with a
code), over memory for the tests ('memoryPair') or over a websockets
connection ('fromWebSocket', with 'connectionOptions'' explicit size
limits, websockets' own defaults bounding nothing); the close codes the LAN
leg uses; and the only place hsed listens or dials ("Hsed.CapabilitySpec"):
'bindLoopback' binds 127.0.0.1 alone, a port alone in its type, 0 for an
ephemeral one (D6; Scott's answer Q1), and 'dialLoopback' dials
@ws:\/\/127.0.0.1:PORT\/PATH@ alone.

Not yet written: group G1 writes the bodies; the signatures and the close
codes are the scaffold's (M5c's S1.2).
-}
module Hsed.Transport.Mhtx.Conn
  ( -- * A connection
    Incoming (..)
  , Conn (..)
  , memoryPair
  , connectionOptions
  , fromWebSocket
    -- * The one bind, the one dial
  , bindLoopback
  , dialLoopback
    -- * Close codes
  , codeNormal
  , codeGoingAway
  , codePolicy
  , codeTooBig
  , codeHeartbeat
  , codeBadToken
  ) where

import           Network.Socket     (Socket)
import qualified Network.WebSockets as WS
import           RIO


-- * A connection

-- | What arrives on a connection: a text message's bytes (its UTF-8 not
--   checked: a frame that does not decode is ignored, SPEC.md L19-21), a
--   binary message's, or the close, with its code and reason where the
--   peer gave one.
data Incoming
  = InText !ByteString
  | InBinary !ByteString
  | InClosed !(Maybe (Word16, Text))
  deriving (Eq, Show)

-- | One websocket as the hub and the client use it.
data Conn = Conn
  { connReceive    :: IO Incoming
  , connSendText   :: ByteString -> IO ()
  , connSendBinary :: ByteString -> IO ()
  , connClose      :: Word16 -> Text -> IO ()
  , connName       :: Text                    -- ^ the peer as the logs name it
  }

-- | Two connections joined in memory, what one sends the other receives, a
--   close carried with its code: the hub's and the client's tests, no socket
--   opened.
memoryPair :: IO (Conn, Conn)
memoryPair = throwString "not yet written: group G1 (Hsed.Transport.Mhtx.Conn.memoryPair)"

-- | The websockets options of every connection: compression off, unicode
--   lenient, and explicit frame and message size limits (websockets'
--   defaults have none: s1-sources.txt).
connectionOptions :: WS.ConnectionOptions
connectionOptions = error "not yet written: group G1 (Hsed.Transport.Mhtx.Conn.connectionOptions)"

-- | A websockets connection as a 'Conn', named.
fromWebSocket :: Text -> WS.Connection -> Conn
fromWebSocket _name _connection = error "not yet written: group G1 (Hsed.Transport.Mhtx.Conn.fromWebSocket)"


-- * The one bind, the one dial

-- | A listening socket on 127.0.0.1 (AF_INET; never the wildcard address,
--   never IPv6, never a name), the port given or, 0, an ephemeral one,
--   ReuseAddr set; and the port it is bound to.
bindLoopback :: Word16 -> IO (Socket, Word16)
bindLoopback _port = throwString "not yet written: group G1 (Hsed.Transport.Mhtx.Conn.bindLoopback)"

-- | An action over a websocket to @ws:\/\/127.0.0.1:PORT\/PATH@, the
--   connection made within the microseconds given; or why there is none.
dialLoopback :: Word16 -> Text -> Int -> (Conn -> IO a) -> IO (Either Text a)
dialLoopback _port _path _micros _act = pure (Left "not yet written: group G1 (Hsed.Transport.Mhtx.Conn.dialLoopback)")


-- * Close codes

-- | 1000, a normal closure (RFC 6455 7.4.1).
codeNormal :: Word16
codeNormal = 1000

-- | 1001, an endpoint going away (RFC 6455 7.4.1): the hub shutting down.
codeGoingAway :: Word16
codeGoingAway = 1001

-- | 1008, a message that violates the endpoint's policy (RFC 6455 7.4.1).
codePolicy :: Word16
codePolicy = 1008

-- | 1009, a message too big to process (RFC 6455 7.4.1).
codeTooBig :: Word16
codeTooBig = 1009

-- | 4000, @heartbeat timeout@: a peer that sent nothing for the hello's
--   @timeout_ms@ (mh_transport SPEC.md L58-60).
codeHeartbeat :: Word16
codeHeartbeat = 4000

-- | 4001: a wrong token, closed before hello (mh_transport SPEC.md L92-93).
codeBadToken :: Word16
codeBadToken = 4001
