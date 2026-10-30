{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.TranscriptSpec
Description : Test specifications for Hsed.Jig.Transcript
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A transcript numbers its entries itself, in the order added; a recording
transport adds every command it exchanges with its answer, an event as a
note, and a command whose transport threw as a transport failure before the
exception goes on; the IF-SENDs issued are the issued entries' IF-SENDs,
not a blocked one's.
-}
module Hsed.Jig.TranscriptSpec (spec) where

import qualified RIO.ByteString      as B
import           RIO
import           Test.Hspec

import           Hsed.Jig.Transcript
import           Hsed.Wire.Internal  (Cap (..), Describe (..), Dir (..), Event (..), IfStatus (..), Request (..),
                                      Response (..), Transport (..), Unit (..))


-- | A transport that answers every command Good with as many zeros as asked
--   for, or throws for an IF-SEND when told to.
fake :: Bool -> Transport 'ReadWrite
fake throws = MkTransport
  { tExchange = \q -> if throws && dir q == IfSend then throwString "the wire broke" else pure (answer q)
  , tInject   = \_ -> pure Good
  , tUnit     = UnitBytes
  , tDescribe = Describe "tper" "a test" UnitBytes Nothing Nothing Nothing Nothing
  , tClose    = pure ()
  }
  where answer q = Response Good (if dir q == IfRecv then B.replicate (fromIntegral (lenBytes q)) 0 else mempty)

recv, send :: Request
recv = Request IfRecv 1 0x1004 64 mempty
send = Request IfSend 1 0x1004 512 mempty

spec :: Spec
spec = do
  describe "record and entries" $
    it "number the entries 1, 2, 3 in the order added, whatever number they carry" $ do
      t <- newTranscript
      record t (Note 7 "first")
      record t (Blocked 0 send "refused")
      record t (Note 0 "third")
      entries t `shouldReturn` [Note 1 "first", Blocked 2 send "refused", Note 3 "third"]

  describe "recording" $ do
    it "adds each command with its answer, and an event as a note" $ do
      t <- newTranscript
      let tr = recording t (fake False)
      r1 <- tExchange tr recv
      r2 <- tExchange tr send
      s <- tInject tr PowerCycle
      s `shouldBe` Good
      entries t `shouldReturn` [Issued 1 recv r1, Issued 2 send r2, Note 3 "event PowerCycle: good"]
    it "adds a command whose transport threw as a transport failure, then rethrows" $ do
      t <- newTranscript
      tExchange (recording t (fake True)) send `shouldThrow` anyException
      entries t >>= \es -> case es of
        [Issued 1 q (Response (TransportFailure _) _)] -> q `shouldBe` send
        other                                          -> expectationFailure ("not one failed IF-SEND: " <> show other)

  describe "ifSendsIssued" $
    it "counts the issued IF-SENDs, not an IF-RECV or a blocked IF-SEND" $
      ifSendsIssued [Issued 1 recv (Response Good mempty), Blocked 2 send "no", Issued 3 send (Response Good mempty), Note 4 "x"]
        `shouldBe` 1
