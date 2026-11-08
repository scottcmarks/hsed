{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-|
Module      : Hsed.Transport.Mhtx.Conn
Description : One websocket of mh_transport frames, abstracted; the one socket hsed binds (127.0.0.1) and the one it dials
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The LAN leg's connections (design 6.3, 7 items 4-5, 11 M5c; docs/wire.md): a
websocket as the hub and the client see it ('Conn': what arrives, text,
binary or the close with its code and reason; sending text or binary;
closing with a code), over memory for the tests ('memoryPair') or over a
websockets connection ('fromWebSocket', with 'connectionOptions'' explicit
size limits, websockets' own defaults bounding nothing); the close codes the
LAN leg uses; and the only place hsed listens or dials
("Hsed.CapabilitySpec"): 'bindLoopback' binds 127.0.0.1 alone, a port alone
in its type, 0 for an ephemeral one (D6; Scott's answer Q1), and
'dialLoopback' dials @ws:\/\/127.0.0.1:PORT\/PATH@ alone.

A connection behaves as RFC 6455's closing handshake has a websocket behave
(5.5.1, 7.1.1), in memory as over a socket: a close is sent once, whoever
asks for it ('connClose', or the echo of the peer's close, which the
websockets library sends itself as it reads it, Connection.hs:291-294);
nothing is sent after it, and data that arrives after it is dropped, so that
a reader reads on to the peer's close ('InClosed', its code and reason) and
no request crossing the close is answered; a receive after the close gives
a close again (in memory the same one; over a socket the stream's end, with
no code, once the peer drops the connection). A message over 2 MiB
('messageLimit'), which the websockets library refuses with no close frame
of its own (design 13.34 (28)(b)), is closed here with 1009 ('codeTooBig'), and a
frame the library cannot read with 1002 ('codeProtocolError'). The wait for
the peer's close after one's own (1 s at most, design 6.3 as M5c builds it)
is the reader's, the hub's or the client's: 'connClose' sends and returns.
-}
module Hsed.Transport.Mhtx.Conn
  ( -- * A connection
    Incoming (..)
  , Conn (..)
  , memoryPair
  , messageLimit
  , connectionOptions
  , fromWebSocket
    -- * The one bind, the one dial
  , bindLoopback
  , dialLoopback
    -- * Close codes
  , codeNormal
  , codeGoingAway
  , codeProtocolError
  , codePolicy
  , codeTooBig
  , codeHeartbeat
  , codeBadToken
  ) where

import           Network.Socket                (Socket)
import qualified Network.Socket                as NS
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WSC
import qualified Network.WebSockets.Stream     as WSS
import qualified RIO.ByteString                as B
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.List                      as L
import           RIO
import qualified RIO.Text                      as T


-- $setup
-- >>> :set -XOverloadedStrings

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

-- | One websocket as the hub and the client use it: one reader at a time
--   ('connReceive'), sends and closes from any thread. A send on a
--   connection whose transport is gone throws (the websockets library's
--   'WS.ConnectionClosed', or the socket's 'IOException'); a send after a
--   close was sent is dropped; 'connClose' never throws.
data Conn = Conn
  { connReceive    :: IO Incoming
  , connSendText   :: ByteString -> IO ()
  , connSendBinary :: ByteString -> IO ()
  , connClose      :: Word16 -> Text -> IO ()
  , connName       :: Text                    -- ^ the peer as the logs name it
  }

-- | Two connections joined in memory, what one sends the other receives, a
--   close carried with its code: the hub's and the client's tests, no socket
--   opened. Each end behaves as a websocket does: its close is sent once
--   and echoed by the other end as that end receives it, its reason cut as
--   a close frame cuts it, nothing sent after it, data received after its
--   own close dropped, and every receive after a close the close again.
--   The ends are named @memory:1@ and @memory:2@.
--
-- >>> (a, b) <- memoryPair
-- >>> connSendText a "{\"type\":\"ping\"}" >> connReceive b
-- InText "{\"type\":\"ping\"}"
-- >>> connClose b 4000 "heartbeat timeout" >> connReceive a
-- InClosed (Just (4000,"heartbeat timeout"))
-- >>> connReceive b
-- InClosed (Just (4000,"heartbeat timeout"))
memoryPair :: IO (Conn, Conn)
memoryPair = do
  oneToTwo <- newTQueueIO
  twoToOne <- newTQueueIO
  one      <- memoryEnd "memory:1" twoToOne oneToTwo
  two      <- memoryEnd "memory:2" oneToTwo twoToOne
  pure (one, two)

-- | One end of a 'memoryPair': what it receives, what it sends, whether it
--   has sent its close, and the close it received.
memoryEnd :: Text -> TQueue Incoming -> TQueue Incoming -> IO Conn
memoryEnd name inbox outbox = do
  closeSent <- newTVarIO False
  received  <- newTVarIO Nothing
  let receive = readTVar received >>= \case
        Just closed -> pure closed
        Nothing     -> readTQueue inbox >>= \case
          closed@(InClosed _) -> do
            writeTVar received (Just closed)
            sent <- swapTVar closeSent True
            unless sent (writeTQueue outbox closed)
            pure closed
          message -> readTVar closeSent >>= \sent -> if sent then receive else pure message
      send message = readTVar closeSent >>= \sent -> unless sent (writeTQueue outbox message)
      close code reason = swapTVar closeSent True >>= \sent ->
        unless sent (writeTQueue outbox (InClosed (Just (code, closeReason reason))))
  pure Conn
    { connReceive    = atomically receive
    , connSendText   = atomically . send . InText
    , connSendBinary = atomically . send . InBinary
    , connClose      = \code -> atomically . close code
    , connName       = name
    }

-- | The most a frame and a message carry, 2 MiB, either way: an IF-SEND or
--   an IF-RECV of 1 MiB, the most @len@ allows (design 6.3), is 1,398,104
--   base64 characters, about 1.4 MB of frame with the JSON around it.
messageLimit :: Int64
messageLimit = 2 * 1024 * 1024

-- | The websockets options of every connection: compression off, unicode
--   lenient (a text message's bytes reach the reader as they came, a frame
--   that is not UTF-8 ignored there), and explicit frame and message size
--   limits ('messageLimit'; websockets' defaults have none, its
--   Connection/Options.hs:62-69 and :110-114).
connectionOptions :: WS.ConnectionOptions
connectionOptions = WS.defaultConnectionOptions
  { WS.connectionCompressionOptions    = WS.NoCompression
  , WS.connectionStrictUnicode         = False
  , WS.connectionFramePayloadSizeLimit = WS.SizeLimit messageLimit
  , WS.connectionMessageDataSizeLimit  = WS.SizeLimit messageLimit
  }

-- | A websockets connection as a 'Conn', named. Its state is the
--   connection's own: the close it sent (websockets' @connectionSentClose@,
--   set once, here or by the library's echo) and its stream. What
--   websockets' receive throws becomes the close: the peer's close frame
--   its code and reason (the library has echoed it); the stream's end, or
--   the socket's failure, no code; a message over 'messageLimit' 1009 and
--   any other frame the library cannot read 1002, each sent to the peer
--   here (the library sends none) and given as the close. A reader stops at
--   the close; one that reads on is given what the stream yields after it
--   as a close or not at all, data after a close sent being dropped (the
--   stream past a frame refused for its size is no longer framed).
fromWebSocket :: Text -> WS.Connection -> Conn
fromWebSocket name c = Conn
  { connReceive    = receive
  , connSendText   = sendData (\bytes -> WS.Text (BL.fromStrict bytes) Nothing)
  , connSendBinary = sendData (WS.Binary . BL.fromStrict)
  , connClose      = \code -> closeOnce code . closeReason
  , connName       = name
  }
  where
    receive = tryAny (WS.receiveDataMessage c) >>= \case
      Right message -> closeSent >>= \sent -> if sent then receive else pure (incoming message)
      Left e        -> closedBy e
    incoming = \case
      WS.Text bytes _ -> InText (BL.toStrict bytes)
      WS.Binary bytes -> InBinary (BL.toStrict bytes)
    closedBy e = case fromException e of
      Just (WS.CloseRequest code reason) -> closing code (decodeUtf8Lenient (BL.toStrict reason))
      Just WS.ConnectionClosed           -> pure (InClosed Nothing)
      Just (WS.ParseException why)
        | "exceeded limit" `L.isSuffixOf` why -> closing codeTooBig ("message too big: over " <> tshow messageLimit <> " bytes")
        | otherwise                          -> closing codeProtocolError ("protocol error: " <> T.pack why)
      Just (WS.UnicodeException why)      -> closing codeProtocolError ("protocol error: " <> T.pack why)
      Nothing                             -> pure (InClosed Nothing)
    -- the close given, sent first where none was (a frame the library refused, which it does not answer)
    closing code reason = let cut = closeReason reason in closeOnce code cut $> InClosed (Just (code, cut))
    closeSent = readIORef (WSC.connectionSentClose c)
    closeOnce code reason = do
      sent <- atomicModifyIORef' (WSC.connectionSentClose c) (True,)
      unless sent $ void $ tryAny $
        WSC.connectionWrite c [WS.ControlMessage (WS.Close code (BL.fromStrict (encodeUtf8 reason)))]
    sendData message bytes = closeSent >>= \sent -> unless sent (WS.sendDataMessage c (message bytes))

-- | A close's reason as a close frame carries it: its UTF-8 cut to 123
--   bytes, a control frame's payload being 125 at most with the code's two
--   (RFC 6455 5.5; websockets cuts the frame so, its Hybi13.hs:139), read
--   back leniently.
--
-- >>> closeReason "heartbeat timeout"
-- "heartbeat timeout"
-- >>> T.length (closeReason (T.replicate 200 "x"))
-- 123
closeReason :: Text -> Text
closeReason = decodeUtf8Lenient . B.take 123 . encodeUtf8


-- * The one bind, the one dial

-- | A listening socket on 127.0.0.1 (AF_INET; never the wildcard address,
--   never IPv6, never a name), the port given or, 0, an ephemeral one,
--   ReuseAddr set (so that a hub started again binds the port another
--   stopped on, design 11 M5c's restart; a port in use still fails, loudly)
--   and close-on-exec; and the port it is bound to. The socket is closed if
--   anything fails; else it is the caller's to close.
bindLoopback :: Word16 -> IO (Socket, Word16)
bindLoopback port = bracketOnError (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol) NS.close $ \sock -> do
  NS.withFdSocket sock NS.setCloseOnExecIfNeeded
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.bind sock (loopbackAddress port)
  NS.listen sock NS.maxListenQueue
  NS.getSocketName sock >>= \case
    NS.SockAddrInet bound host | host == loopbackHost -> pure (sock, fromIntegral bound)
    other                                             -> throwString ("bindLoopback: bound to " <> show other <> ", not 127.0.0.1")

-- | An action over a websocket to @ws:\/\/127.0.0.1:PORT\/PATH@, the
--   connection made within the microseconds given; or why there is none.
--   The wait covers the TCP connect and the websocket handshake (websockets'
--   own client has no timeout, its Client.hs:47-64), never the action, whose
--   exceptions are its own; the socket is closed when the action ends (a
--   close frame is the action's to send, 'connClose'). The path is the
--   request's whole path, a @\/@ and printable ASCII, a query included
--   (@\/ws?token=T@); the connection's name and every reason leave the query
--   out, so that no token is written where a log may print it. A refused
--   connection, an upgrade the hub refuses (the HTTP status it answered) and
--   the wait running out are each a reason.
dialLoopback :: Word16 -> Text -> Int -> (Conn -> IO a) -> IO (Either Text a)
dialLoopback port path micros act
  | not ("/" `T.isPrefixOf` path && T.all (\ch -> ch > ' ' && ch < '\DEL') path)
              = pure (Left (url <> ": the path is a / and printable ASCII, no space"))
  | micros <= 0 = pure (Left (url <> ": no time to connect in"))
  | otherwise = bracket (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol) NS.close $ \sock ->
      bracket (WSS.makeSocketStream sock) WSS.close $ \stream -> do
        opened <- tryAny $ timeout micros $ do
          NS.setSocketOption sock NS.NoDelay 1
          NS.connect sock (loopbackAddress port)
          WS.newClientConnection stream ("127.0.0.1:" <> show port) (T.unpack path) connectionOptions []
        case opened of
          Left e         -> pure (Left (url <> ": " <> dialFailure e))
          Right Nothing  -> pure (Left (url <> ": no websocket within " <> tshow (micros `div` 1000) <> " ms"))
          Right (Just c) -> Right <$> act (fromWebSocket url c)
  where
    url = "ws://127.0.0.1:" <> tshow port <> T.takeWhile (/= '?') path

-- | Why a dial failed, with nothing of the request in it (its path may carry
--   a token): the hub's HTTP answer where it refused the upgrade.
dialFailure :: SomeException -> Text
dialFailure e = case fromException e of
  Just (WS.MalformedResponse response why) ->
    "the upgrade refused, HTTP " <> tshow (WS.responseCode response) <> " " <> decodeUtf8Lenient (WS.responseMessage response)
    <> " (" <> T.pack why <> ")"
  Just (WS.OtherHandshakeException why)     -> "the handshake failed: " <> T.pack why
  Just (WS.MalformedRequest _ why)          -> "the handshake failed: " <> T.pack why
  Just (WS.RequestRejected _ why)           -> "the handshake failed: " <> T.pack why
  Just WS.NotSupported                      -> "the handshake failed: no protocol in common"
  Nothing                                   -> T.pack (displayException e)

-- | 127.0.0.1 and a port.
loopbackAddress :: Word16 -> NS.SockAddr
loopbackAddress port = NS.SockAddrInet (fromIntegral port) loopbackHost

-- | 127.0.0.1.
loopbackHost :: NS.HostAddress
loopbackHost = NS.tupleToHostAddress (127, 0, 0, 1)


-- * Close codes

-- | 1000, a normal closure (RFC 6455 7.4.1).
codeNormal :: Word16
codeNormal = 1000

-- | 1001, an endpoint going away (RFC 6455 7.4.1): the hub shutting down.
codeGoingAway :: Word16
codeGoingAway = 1001

-- | 1002, a protocol error (RFC 6455 7.4.1): a frame the websockets library
--   cannot read.
codeProtocolError :: Word16
codeProtocolError = 1002

-- | 1008, a message that violates the endpoint's policy (RFC 6455 7.4.1).
codePolicy :: Word16
codePolicy = 1008

-- | 1009, a message too big to process (RFC 6455 7.4.1): one over
--   'messageLimit'.
codeTooBig :: Word16
codeTooBig = 1009

-- | 4000, @heartbeat timeout@: a peer that sent nothing for the hello's
--   @timeout_ms@ (mh_transport SPEC.md L58-60).
codeHeartbeat :: Word16
codeHeartbeat = 4000

-- | 4001: a wrong token, closed before hello (mh_transport SPEC.md L92-93).
codeBadToken :: Word16
codeBadToken = 4001
