{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.ScriptSpec
Description : Test specifications for Hsed.Jig.Script
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A script's verdict ('attempt'): its own, a failure where an expectation did
not hold or it threw, unobservable where a drive's transport cannot show a
status; a step for the simulator alone; on a drive, an IF-SEND stopped by
the Guard before the transport, recorded. The ComPacket the jig sends is
the Test Cases' (Table 6, PDF 27), its Properties call design 11's M0
encoding; an IF-RECV's header read for Table 45; the matcher's field names
the generated table's, and MaxComPacketSize a property Core Table 167
names; and each answer named by its row of Table 45, through
"Hsed.Sim.Sync".@t45Rows@, which reads the generated table's rows in order.
-}
module Hsed.Jig.ScriptSpec (spec) where

import qualified RIO.ByteString            as B
import           RIO
import qualified RIO.Text                  as T
import           Test.Hspec

import           Hsed.Core.Properties      (Property (..))
import           Hsed.Core.Record          (encodeRecord)
import           Hsed.Core.Sync            (StateTable (..))
import           Hsed.Gen.Core.Layouts     (ComPacket (..), ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..),
                                            PacketHeader (..))
import qualified Hsed.Gen.Core.Properties  as P
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Jig.Options          (defaultRunOptions)
import           Hsed.Jig.Script
import           Hsed.Jig.Target           (Target (..))
import           Hsed.Jig.Transcript       (TEntry (..), entries, newTranscript)
import           Hsed.Packet               (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket)
import           Hsed.Sim.Sync             (T45Row (..))
import           Hsed.Sim.Types            (defaultSimParams, initialState)
import           Hsed.Transport.InProc     (SimControl (..))
import           Hsed.Wire                 (newChannel)
import           Hsed.Wire.Internal        (Describe (..), IfStatus (..), Response (..), Transport (..), Unit (..))


-- | A transport that counts what reaches it and answers Good.
counted :: IORef Int -> Transport c
counted n = MkTransport
  { tExchange = \_ -> Response Good mempty <$ modifyIORef' n (+ 1)
  , tInject   = \_ -> pure Good
  , tUnit     = UnitBytes
  , tDescribe = Describe "test" "a test" UnitBytes Nothing Nothing Nothing Nothing
  , tClose    = pure ()
  }

-- | A script on the simulator's end or a drive's, over counting transports:
--   what it came to, how many commands reached the transport, the
--   transcript.
scripted :: Bool -> Script a -> IO (a, Int, [TEntry])
scripted sim s = do
  n <- newIORef 0
  t <- newTranscript
  channel <- newChannel
  ctx <- newIORef (RunCtx Nothing Nothing Nothing)
  let target | sim       = SimTarget (counted n) (SimControl (\_ -> pure ()) (pure (initialState defaultSimParams)))
             | otherwise = DriveTarget (counted n) Nothing
  a <- runRIO (RunEnv target t channel ctx defaultRunOptions (mkLogFunc (\_ _ _ _ -> pure ()))) s
  (,,) a <$> readIORef n <*> entries t

-- | A response holding a ComPacket header: Length bytes of payload,
--   OutstandingData, MinTransfer.
answerOf :: Int -> Word32 -> Word32 -> Response
answerOf l o m = Response Good (either (error . show) id (encodeRecord (ComPacket (ComPacketHeader 0x1004 0 o m) (B.replicate l 0))) <> B.replicate 64 0)

spec :: Spec
spec = do
  describe "attempt" $ do
    it "gives a script's own verdict" $ do
      (v, _, _) <- scripted True (attempt (pure (Skipped "why")))
      v `shouldBe` Skipped "why"
    it "fails a script whose expectation did not hold, and one that threw, naming it" $ do
      (v1, _, _) <- scripted True (attempt (unmet "not so"))
      (v2, _, _) <- scripted True (attempt (throwString "boom"))
      v1 `shouldBe` Fail ["not so"]
      v2 `shouldSatisfy` \v -> case v of
        Fail [r] -> "exception: " `T.isPrefixOf` r
        _        -> False

  describe "expectStatus" $ do
    it "passes the status expected, fails another, naming both" $ do
      (v1, _, _) <- scripted True (attempt (Pass <$ expectStatus "step 1" Good (Response Good mempty)))
      (v2, _, _) <- scripted True (attempt (Pass <$ expectStatus "step 2" SynchronousProtocolViolation (Response Good mempty)))
      v1 `shouldBe` Pass
      v2 `shouldBe` Fail ["step 2: good, not synchronous_protocol_violation"]
    it "on a drive, takes a transport failure for an error expected as unobservable (design 6.2)" $ do
      (v, _, _) <- scripted False (attempt (Pass <$ expectStatus "step 2" SynchronousProtocolViolation (Response (TransportFailure "0xff") mempty)))
      v `shouldSatisfy` \x -> case x of
        Unobservable _ -> True
        _              -> False

  describe "simOnly" $
    it "runs a step on the simulator alone" $ do
      (a, _, _) <- scripted True (simOnly (pure ()))
      (b, _, _) <- scripted False (simOnly (pure ()))
      (a, b) `shouldBe` (Just (), Nothing)

  describe "rawIfSend on a drive" $
    it "is stopped by the Guard (Level0Only), recorded, answered unsupported, the transport never called" $ do
      (r, reached, es) <- scripted False (rawIfSend 0x1004 512 (either (error . show) id (controlComPacket 0x1004 propertiesCall)))
      reached `shouldBe` 0
      status r `shouldSatisfy` \s -> case s of
        Unsupported _ -> True
        _             -> False
      length [ () | Blocked {} <- es ] `shouldBe` 1

  describe "the ComPacket the jig sends" $ do
    it "calls SMUID.Properties as design 11's M0 encodes it" $
      B.unpack propertiesCall `shouldBe`
        [0xF8, 0xA8, 0, 0, 0, 0, 0, 0, 0, 0xFF, 0xA8, 0, 0, 0, 0, 0, 0, 0xFF, 0x01, 0xF0, 0xF1, 0xF9, 0xF0, 0, 0, 0, 0xF1]
    it "is the Test Cases' (Table 6, PDF 27): one packet of the control session, one Data subpacket, the rest zeros" $ do
      let bytes = either (error . show) id (controlComPacket 0x1004 propertiesCall)
      B.length bytes `mod` 4 `shouldBe` 0
      decodeComPacket bytes `shouldBe` Right (DecodedFrame
        (ComPacketFrame (ComPacketHeader 0x1004 0 0 0) [PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader 0) propertiesCall]])
        [] mempty)

  describe "Table 45" $ do
    it "reads an IF-RECV's header as its Length, OutstandingData and MinTransfer" $
      map t45Of [answerOf 0 1 0, answerOf 40 0 0, answerOf 0 100 128] `shouldBe` map Right [(0, 1, 0), (40, 0, 0), (0, 100, 128)]
    it "matches the generated table's fields by their names, and MaxComPacketSize is Core Table 167's" $ do
      fields table45 `shouldBe` ["Length", "OutstandingData", "MinTransfer"]
      maxComPacketSizeName `shouldSatisfy` (`elem` map propertyName P.properties)
    it "names each answer's row, in the order the simulator reads the generated rows" $
      map t45RowOf [(0, 1, 0), (0, 100, 128), (40, 100, 128), (40, 1, 0), (40, 0, 0), (0, 0, 0), (0, 0, 7)]
        `shouldBe` [ Just ResponsesToCome, Just InsufficientLength, Just AdditionalAvailable, Just AdditionalToCome
                   , Just AllReturnedWithData, Just AllReturnedNoData, Nothing ]
