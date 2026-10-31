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
not a blocked one's. A secret is masked in an entry's bytes as so many
asterisks, in a command's payload and its answer's alike, a blocked
command's too, so that a byte-string atom carrying it keeps its length and
the stream still reads; an empty secret and a note are left alone (design
9.3 item 5).
-}
module Hsed.Jig.TranscriptSpec (spec) where

import qualified RIO.ByteString       as B
import           RIO
import           Test.Hspec

import           Hsed.Core.Call       (MethodResponse (..))
import           Hsed.Core.StreamItem (StreamItem (..))
import           Hsed.Core.Value      (List (..), Value (..), (.:))
import           Hsed.Jig.Transcript
import           Hsed.Wire.Internal   (Cap (..), Describe (..), Dir (..), Event (..), IfStatus (..), Request (..),
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

-- | The simulator's default MSID, 32 bytes.
msid :: ByteString
msid = "HSED-SIM-MSID-0123456789ABCDEF01"

-- | So many asterisks.
stars :: Int -> ByteString
stars n = B.replicate n maskByte

spec :: Spec
spec = do
  describe "record and entries" $
    it "number the entries 1, 2, 3 in the order added, whatever number they carry" $ do
      t <- newTranscript
      record t (Note 7 "first")
      record t (Blocked 0 "drive" send "refused")
      record t (Note 0 "third")
      entries t `shouldReturn` [Note 1 "first", Blocked 2 "drive" send "refused", Note 3 "third"]

  describe "recording" $ do
    it "adds each command with its answer and its transport's role, and an event as a note" $ do
      t <- newTranscript
      let tr = recording t (fake False)
      r1 <- tExchange tr recv
      r2 <- tExchange tr send
      s <- tInject tr PowerCycle
      s `shouldBe` Good
      entries t `shouldReturn` [Issued 1 "tper" recv r1, Issued 2 "tper" send r2, Note 3 "event PowerCycle: good"]
    it "adds a command whose transport threw as a transport failure, then rethrows" $ do
      t <- newTranscript
      tExchange (recording t (fake True)) send `shouldThrow` anyException
      entries t >>= \es -> case es of
        [Issued 1 "tper" q (Response (TransportFailure _) _)] -> q `shouldBe` send
        other                                          -> expectationFailure ("not one failed IF-SEND: " <> show other)

  describe "ifSendsIssued" $
    it "counts the issued IF-SENDs, not an IF-RECV or a blocked IF-SEND" $
      ifSendsIssued [ Issued 1 "drive" recv (Response Good mempty), Blocked 2 "drive" send "no"
                    , Issued 3 "drive" send (Response Good mempty), Note 4 "x" ]
        `shouldBe` 1

  describe "maskSecrets (design 9.3 item 5)" $ do
    it "replaces each occurrence of a secret in a command's payload and in its answer with as many 0x2A bytes, and nothing else" $
      maskSecrets [msid] (Issued 1 "tper" send { payload = "abc" <> msid <> "def" } (Response Good (msid <> "--" <> msid)))
        `shouldBe` Issued 1 "tper" send { payload = "abc" <> stars 32 <> "def" } (Response Good (stars 32 <> "--" <> stars 32))
    it "masks a blocked command's payload and its reason, and a note's text as text, hex or as shown; leaves an empty secret and bytes without the secret alone" $ do
      maskSecrets ["x"] (Blocked 2 "drive" send { payload = "axb" } "why x") `shouldBe` Blocked 2 "drive" send { payload = "a*b" } "why *"
      maskSecrets [""] (Issued 1 "tper" send (Response Good "abc")) `shouldBe` Issued 1 "tper" send (Response Good "abc")
      maskSecrets ["zz"] (Issued 1 "tper" send (Response Good "abc")) `shouldBe` Issued 1 "tper" send (Response Good "abc")
      maskSecrets ["abc"] (Note 3 "abc, 616263, Bytes \"abc\"") `shouldBe` Note 3 "***, ***, Bytes \"***\""
      maskSecrets ["abc"] (Note 3 "abd") `shouldBe` Note 3 "abd"
      maskText ["\255!"] ("shown " <> tshow ("\255!" :: ByteString)) `shouldBe` "shown \"**\""
    it "masks several secrets, each where it occurs" $
      maskSecrets ["MSID", "PIN!"] (Issued 1 "tper" send (Response Good "the MSID and the PIN!"))
        `shouldBe` Issued 1 "tper" send (Response Good ("the " <> stars 4 <> " and the " <> stars 4))
    it "keeps a masked byte-string atom's length, so that a Get result carrying the MSID still reads, its PIN asterisks" $ do
      let result = MethodResponse [L (List [N ((3 :: Natural) .: msid)])] 0
      case maskSecrets [msid] (Issued 1 "tper" recv (Response Good (generate result))) of
        Issued _ _ _ r -> parseByteString (rpayload r) `shouldBe` Right (MethodResponse [L (List [N ((3 :: Natural) .: stars 32)])] 0)
        other          -> expectationFailure ("not an issued entry: " <> show other)
