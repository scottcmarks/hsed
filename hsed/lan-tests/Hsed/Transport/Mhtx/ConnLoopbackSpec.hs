{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.ConnLoopbackSpec
Description : Loopback specifications for Hsed.Transport.Mhtx.Conn: a real socket on 127.0.0.1
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

On a socket bindLoopback 0 bound (a fresh one per example, closed after it): its name is
127.0.0.1 and the port it gave, a port in use is refused and a closed one bound again; text,
binary and close 4000 with its reason carried both ways, the close echoed, a receive after it a
close again; a text message of about 1.4 MB (a 1 MiB IF-SEND's frame) passed both ways; a message over
the 2 MiB limit closed with 1009, the code s1-sources.txt predicts once Conn sends it, the
library sending none, while the server serves the next connection; a text frame of invalid UTF-8
given to fromClient as it came, which ignores it, the connection kept; and a dial that fails
(no listener, a listener that never answers, a path refused) a reason within its wait, the
connection named without its query (design 11 M5c, group G1's acceptance).

The server here is an accept loop, as no server library is in hsed-lan-test's dependencies and
"Hsed.CapabilitySpec" forbids websockets' own servers: each connection accepted on the bound
socket, upgraded by websockets' makePendingConnection and acceptRequest with Conn's options,
given to a handler as fromWebSocket's Conn, then closed after a lingering close.
-}
module Hsed.Transport.Mhtx.ConnLoopbackSpec (spec) where

import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.WebSockets        as WS
import qualified RIO.ByteString            as B
import qualified RIO.Text                  as T
import           RIO
import           Test.Hspec

import           Hsed.Transport.Mhtx.Conn  (Conn (..), Incoming (..), bindLoopback, codeHeartbeat, codeTooBig, connectionOptions,
                                            dialLoopback, fromWebSocket, messageLimit)
import           Hsed.Transport.Mhtx.Frame (FromClient (..), Op (..), SedReq (..), fromClient, pingText, reqText)


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Conn" $ do
  describe "bindLoopback" $ do
    it "binds 127.0.0.1 alone, an ephemeral port for 0, the one it returns" $
      bracket (bindLoopback 0) (NS.close . fst) $ \(sock, port) -> do
        port `shouldSatisfy` (/= 0)
        NS.getSocketName sock `shouldReturn` NS.SockAddrInet (fromIntegral port) (NS.tupleToHostAddress (127, 0, 0, 1))

    it "refuses a port in use, loudly, and binds a port again once its socket is closed" $ do
      port <- bracket (bindLoopback 0) (NS.close . fst) $ \(_, port) -> do
        again <- tryIO (bindLoopback port)
        either (const (pure ())) (\(s, _) -> NS.close s *> expectationFailure "bound a port in use") again
        pure port
      bracket (bindLoopback port) (NS.close . fst) (\(_, rebound) -> rebound `shouldBe` port)

  describe "a websocket over 127.0.0.1" $ do
    it "carries text and binary both ways and a close 4000 with its reason, echoed; a receive after it is a close again" $ do
      result <- newEmptyMVar
      let handler c = do
            text   <- connReceive c
            for_ (textOf text) (connSendText c)
            binary <- connReceive c
            for_ (binaryOf binary) (connSendBinary c)
            connClose c codeHeartbeat "heartbeat timeout"
            echo   <- connReceive c
            putMVar result [text, binary, echo]
      serving handler $ \port -> do
        dialLoopback port "/ws" 5000000 (\c -> do
            connSendText c pingText
            text   <- connReceive c
            connSendBinary c "\0\1\2\255"
            binary <- connReceive c
            closed <- connReceive c
            again  <- connReceive c
            pure [text, binary, closed, again])
          `shouldReturn` Right [InText pingText, InBinary "\0\1\2\255", InClosed (Just (4000, "heartbeat timeout")), InClosed Nothing]
        timeout 5000000 (readMVar result)
          `shouldReturn` Just [InText pingText, InBinary "\0\1\2\255", InClosed (Just (4000, "heartbeat timeout"))]

    it "passes a text message of about 1.4 MB both ways, a 1 MiB IF-SEND's frame, which fromClient reads" $ do
      let request = SedReq 1 (OpIfSend 1 4100 (B.replicate 1048576 0x5a))
          frame   = reqText request
      B.length frame `shouldSatisfy` (\n -> n > 1390000 && n < fromIntegral messageLimit)
      seen <- newEmptyMVar
      serving (\c -> connReceive c >>= \m -> putMVar seen (fromClient m) *> for_ (textOf m) (connSendText c) *> void (connReceive c)) $ \port ->
        dialLoopback port "/ws" 5000000 (\c -> connSendText c frame *> connReceive c) `shouldReturn` Right (InText frame)
      timeout 5000000 (readMVar seen) `shouldReturn` Just (ClientReq request)

    it "closes a message over 2 MiB with 1009 (Conn sends it, websockets sending none), and serves the next connection" $ do
      seen <- newIORef []
      let handler c = do
            m <- connReceive c
            atomicModifyIORef' seen (\ms -> (m : ms, ()))
            for_ (textOf m) (\t -> connSendText c t *> void (connReceive c))
      serving handler $ \port -> do
        refused <- dialLoopback port "/ws" 5000000 (\c -> connSendText c (B.replicate (fromIntegral messageLimit + 1) 0x61) *> connReceive c)
        refused `shouldSatisfy` either (const False) tooBig
        dialLoopback port "/ws" 5000000 (\c -> connSendText c "after" *> connReceive c) `shouldReturn` Right (InText "after")
      ms <- readIORef seen
      (length ms, any tooBig ms, InText "after" `elem` ms) `shouldBe` (2, True, True)

    it "gives a text frame of invalid UTF-8 to fromClient as it came, which ignores it, the connection kept" $ do
      let invalid = "{\"type\":\"ping\",\"x\":\"\255\254\"}"
      seen <- newIORef []
      let sorting c = connReceive c >>= \case
            InClosed _ -> pure ()
            m          -> do
              atomicModifyIORef' seen (\ms -> (ms <> [m], ()))
              connSendText c (encodeUtf8 (tshow (fromClient m)))
              sorting c
      serving sorting $ \port ->
        dialLoopback port "/ws" 5000000 (\c -> traverse (\m -> connSendText c m *> connReceive c) [invalid, pingText])
          `shouldReturn` Right [InText "ClientIgnored", InText "ClientPing"]
      readIORef seen `shouldReturn` [InText invalid, InText pingText]

  describe "dialLoopback" $ do
    it "gives a reason within its wait: no listener, a listener that never answers, a path it refuses" $ do
      closed <- bracket (bindLoopback 0) (NS.close . fst) (pure . snd)
      dialLoopback closed "/ws" 2000000 (const (pure ())) >>= (`shouldSatisfy` isLeft)
      bracket (bindLoopback 0) (NS.close . fst) $ \(_, silent) ->
        dialLoopback silent "/ws" 200000 (const (pure ()))
          `shouldReturn` Left ("ws://127.0.0.1:" <> tshow silent <> "/ws: no websocket within 200 ms")
      traverse (\path -> dialLoopback closed path 1000000 (const (pure ()))) ["ws", "/ws x", "/ws\r\nX: y", ""]
        >>= (`shouldSatisfy` all isLeft)

    it "names the connection by its URL without the query: a token is in no name and no reason" $ do
      serving (void . connReceive) $ \port ->
        dialLoopback port "/ws?token=s3cr3t" 5000000 (pure . connName) `shouldReturn` Right ("ws://127.0.0.1:" <> tshow port <> "/ws")
      closed <- bracket (bindLoopback 0) (NS.close . fst) (pure . snd)
      dialLoopback closed "/ws?token=s3cr3t" 2000000 (const (pure ()))
        >>= (`shouldSatisfy` either (not . ("s3cr3t" `T.isInfixOf`)) (const False))


-- | A websocket server on 127.0.0.1 and an ephemeral port for the action's length: each connection
--   accepted in its own thread, upgraded with Conn's options, given to the handler, then closed
--   after a lingering close ('lingering'); the connections' threads stopped, then the socket
--   closed, whatever the action comes to.
serving :: (Conn -> IO ()) -> (Word16 -> IO a) -> IO a
serving handler act = bracket (bindLoopback 0) (NS.close . fst) $ \(sock, port) -> do
  peers <- newIORef []
  withAsync (forever (acceptOne sock peers)) (const (act port)) `finally` (readIORef peers >>= traverse_ cancel)
  where
    acceptOne sock peers = mask_ $ do
      (peer, _) <- NS.accept sock
      a <- asyncWithUnmask (\restore -> restore (serveOne peer) `finally` NS.close peer)
      atomicModifyIORef' peers (\as -> (a : as, ()))
    serveOne peer = do
      NS.setSocketOption peer NS.NoDelay 1
      c <- WS.makePendingConnection peer connectionOptions >>= WS.acceptRequest
      handler (fromWebSocket "loopback peer" c)
      lingering peer

-- | A connection's sending half shut and what the peer still sends read and dropped until it
--   closes, 2 s at most: so that the peer reads all that was sent to it, a close frame included,
--   before its own sending ends, where closing at once with its bytes unread would reset it.
lingering :: NS.Socket -> IO ()
lingering peer = void $ tryAny $ do
  NS.shutdown peer NS.ShutdownSend
  void (timeout 2000000 drain)
  where
    drain = NSB.recv peer 65536 >>= \bytes -> unless (B.null bytes) drain

textOf :: Incoming -> Maybe ByteString
textOf = \case
  InText t -> Just t
  _        -> Nothing

binaryOf :: Incoming -> Maybe ByteString
binaryOf = \case
  InBinary b -> Just b
  _          -> Nothing

-- | A close with 1009, the code Conn sends a message over its limit.
tooBig :: Incoming -> Bool
tooBig = \case
  InClosed (Just (code, _)) -> code == codeTooBig
  _                         -> False
