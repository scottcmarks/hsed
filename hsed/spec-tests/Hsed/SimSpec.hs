{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.SimSpec
Description : Test specifications for Hsed.Sim: a step of the simulated TPer, routed by unit, protocol and ComID
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The simulator's step (design 8.2): a command held to its unit (SIIS 1.20
4.3, PDF 30-31), then routed by protocol (SIIS 1.20 Tables 12-13; Core Table
25, PDF 53) and by ComID (Core Table 26, PDF 55): Level 0 outside the
synchronous protocol (3.3.6, PDF 67; 3.3.10.3 b, PDF 84), a static ComID to
its synchronous protocol, a reserved or inactive ComID refused or, if the
run says so, discarded (3.3.3.2-3, PDF 58-59); an IF-SEND on protocol 0x00,
which is not defined for IF-SEND, an Invalid Security Protocol ID (SIIS 1.20
Tables 12 and 23). An event and a Tick, on states built directly. And the
parameters a TPer starts from: static ComIDs that neither wrap past 0xFFFF
nor fall among the reserved ones, and the parameters out of range refused.
-}
module Hsed.SimSpec (spec) where

import qualified RIO.ByteString        as B
import qualified RIO.Map               as Map
import           RIO
import           Test.Hspec

import           Hsed.Core.Code        (CodeRow (..), codeRow)
import           Hsed.Gen.Core.Codes   (cComIDAssignments, cProtocolIDs)
import           Hsed.Gen.Core.Layouts (ComPacketHeader (..))
import           Hsed.Packet           (ComPacketFrame (..), DecodedFrame (..), decodeComPacket)
import           Hsed.Sim
import           Hsed.Sim.Types
import           Hsed.Wire


base :: Word16
base = spBaseComId defaultSimParams

-- | A TPer of the default parameters but for these.
tper :: (SimParams -> SimParams) -> TPerState
tper f = initialState (f defaultSimParams)

-- | A command's answer and the TPer after it.
command :: Request -> TPerState -> (Response, TPerState)
command r st = case step (Cmd r) st of
  (Out resp, st') -> (resp, st')
  (other, st')    -> (Response (TransportFailure (tshow other)) mempty, st')

-- | Its answer alone.
answerTo :: TPerState -> Request -> Response
answerTo st r = fst (command r st)

recvOn, sendOn :: Word8 -> Word16 -> Word32 -> Request
recvOn p c len = Request IfRecv p c len mempty
sendOn p c len = Request IfSend p c len (B.replicate (fromIntegral len) 0)

refused :: IfStatus -> Response
refused s = Response s mempty

-- | The base ComID in a state.
withSync :: SyncState -> TPerState -> TPerState
withSync s st = st { tsComIds = Map.adjust (\cs -> cs { cSync = s }) base (tsComIds st) }


spec :: Spec
spec = describe "a step of the simulated TPer" $ do
  describe "the unit" $ do
    it "in blocks, refuses a length of none or of part of a block, whatever the protocol or ComID" $ do
      let st = tper (\p -> p { spUnits = UnitBlocks512 })
      forM_ [recvOn 0x01 base 100, sendOn 0x01 base 100, recvOn 0x01 0x0001 0, recvOn 0x01 base 0, recvOn 0x03 base 513, sendOn 0x02 4 1] $ \r ->
        command r st `shouldBe` (refused OtherInvalidCommandParameter, st)
    it "in blocks, takes whole blocks" $ do
      let st = tper (\p -> p { spUnits = UnitBlocks512 })
      status (answerTo st (recvOn 0x01 base 512)) `shouldBe` Good
      status (answerTo st (recvOn 0x01 0x0001 1024)) `shouldBe` Good
    it "in bytes, takes any length" $
      map (status . answerTo (tper id) . recvOn 0x01 base) [0, 1, 19, 20, 100, 4096] `shouldBe` replicate 6 Good

  describe "the protocol" $ do
    it "is TCG's 0x01 and 0x02 as Core Table 25 assigns them, 0x00 another's" $ do
      codeCells <$> codeRow cProtocolIDs 0x01 `shouldBe` Just ["Defined in this document"]
      codeCells <$> codeRow cProtocolIDs 0x02 `shouldBe` Just ["Defined in this document"]
      codeCells <$> codeRow cProtocolIDs 0x00 `shouldBe` Just ["See [6]"]
    it "0x02, ComID management, is M4a's; IF-RECV on 0x00 is not simulated" $ do
      status (answerTo (tper id) (sendOn 0x02 0x0004 512)) `shouldSatisfy` isUnsupported
      status (answerTo (tper id) (recvOn 0x00 0x0000 512)) `shouldSatisfy` isUnsupported
    it "0x00 is not defined for IF-SEND: an Invalid Security Protocol ID, the TPer unchanged (SIIS 1.20 Tables 12, 23; Opal 3.3.2)" $
      command (sendOn 0x00 0x0000 512) (tper id) `shouldBe` (refused InvalidSecurityProtocolId, tper id)
    it "any other is an Invalid Security Protocol ID, either way, the TPer unchanged" $
      forM_ [0x03, 0x06, 0xEE, 0xFF] $ \p -> do
        command (recvOn p base 2048) (tper id) `shouldBe` (refused InvalidSecurityProtocolId, tper id)
        command (sendOn p base 2048) (tper id) `shouldBe` (refused InvalidSecurityProtocolId, tper id)

  describe "Level 0 Discovery, ComID 0x0001 (Core Table 26)" $ do
    it "is Table 26's" $ codeCells <$> codeRow cComIDAssignments 0x0001 `shouldBe` Just ["Level 0 Device Discovery"]
    it "accepts IF-SEND, discarding it, whatever the synchronous state" $
      forM_ [S1, S2 ["p"], S3 ["p"]] $ \s ->
        command (sendOn 0x01 0x0001 512) (withSync s (tper id)) `shouldBe` (Response Good mempty, withSync s (tper id))
    it "answers IF-RECV with exactly the bytes asked, the synchronous state untouched" $
      forM_ [S1, S2 ["p"], S3 ["p"]] $ \s -> forM_ [1, 48, 100, 4096] $ \len -> do
        let (r, st') = command (recvOn 0x01 0x0001 len) (withSync s (tper id))
        (status r, B.length (rpayload r), st') `shouldBe` (Good, fromIntegral len, withSync s (tper id))

  describe "a static ComID" $ do
    it "takes an IF-SEND of MaxComPacketSize, 2048 bytes, and refuses one longer in S1 (Core 3.3.10.7 c)" $ do
      status (answerTo (tper id) (sendOn 0x01 base 2048)) `shouldBe` Good
      command (sendOn 0x01 base 2049) (tper id) `shouldBe` (refused InvalidTransferLengthOnIfSend, tper id)
    it "refuses any IF-SEND in S2 or S3 with a Synchronous Protocol Violation, the TPer unchanged (3.3.10.5)" $
      forM_ [S2 ["p"], S3 ["p"]] $ \s -> forM_ [1, 2048, 2049] $ \len ->
        command (sendOn 0x01 base len) (withSync s (tper id)) `shouldBe` (refused SynchronousProtocolViolation, withSync s (tper id))
    it "keeps S1 for an IF-SEND it discards" $
      command (sendOn 0x01 base 2048) (tper id) `shouldBe` (Response Good mempty, tper id)
    it "answers IF-RECV with Table 45 for its state" $ do
      let (r, st') = command (recvOn 0x01 base 2048) (withSync (S2 ["p"]) (tper id))
      (dfFrame <$> decodeComPacket (rpayload r), st') `shouldBe` (Right (ComPacketFrame (ComPacketHeader base 0 1 0) []), withSync (S2 ["p"]) (tper id))

  describe "a ComID neither Level 0's nor static" $ do
    it "reserved (Core Table 26: 0x0000-0x0FFF), is refused either way, discarding or not (3.3.3.2-3)" $
      forM_ [False, True] $ \discard -> forM_ [0x0000, 0x0002, 0x07FF, 0x0800, 0x0FFF] $ \c -> do
        let st = tper (\p -> p { spInactiveComIdDiscard = discard })
        command (recvOn 0x01 c 2048) st `shouldBe` (refused OtherInvalidCommandParameter, st)
        command (sendOn 0x01 c 2048) st `shouldBe` (refused OtherInvalidCommandParameter, st)
    it "inactive and not reserved, is refused either way" $
      forM_ [0x1000, 0x1005, 0x2000, 0xFFFF] $ \c -> do
        command (recvOn 0x01 c 2048) (tper id) `shouldBe` (refused OtherInvalidCommandParameter, tper id)
        command (sendOn 0x01 c 2048) (tper id) `shouldBe` (refused OtherInvalidCommandParameter, tper id)
    it "inactive, discarding: IF-SEND accepted; IF-RECV a header of the ComID, extension 0xFFFF, zeros, exactly the length asked" $ do
      let st = tper (\p -> p { spInactiveComIdDiscard = True })
      command (sendOn 0x01 0x2000 2048) st `shouldBe` (Response Good mempty, st)
      let (r, st') = command (recvOn 0x01 0x2000 2048) st
      (status r, B.length (rpayload r), st') `shouldBe` (Good, 2048, st)
      B.take 20 (rpayload r) `shouldBe` (B.replicate 4 0 <> "\x20\x00\xff\xff" <> B.replicate 12 0)
      B.drop 20 (rpayload r) `shouldSatisfy` B.all (== 0)
    it "inactive in the TPer's own list is inactive" $ do
      let st = (tper id) { tsComIds = Map.adjust (\cs -> cs { cLife = Inactive }) base (tsComIds (tper id)) }
      command (recvOn 0x01 base 2048) st `shouldBe` (refused OtherInvalidCommandParameter, st)

  describe "the parameters a TPer starts from" $ do
    it "the defaults pass, the DataStore table Opal 2.30 4.3.8.1's 10 MB" $ do
      simParamsProblems defaultSimParams `shouldBe` []
      spDataStore defaultSimParams `shouldBe` (1, 0xA00000, 1)
    it "give static ComIDs as far as 0xFFFF, none wrapping to the reserved 0x0000" $ do
      Map.keys (tsComIds (tper (\p -> p { spBaseComId = 0xFFFF, spNComIds = 2 }))) `shouldBe` [0xFFFF]
      Map.keys (tsComIds (tper (\p -> p { spBaseComId = 0x0FFF, spNComIds = 2 }))) `shouldBe` [0x1000]
      command (recvOn 0x01 0x0000 64) (tper (\p -> p { spBaseComId = 0xFFFF, spNComIds = 2 }))
        `shouldBe` (refused OtherInvalidCommandParameter, tper (\p -> p { spBaseComId = 0xFFFF, spNComIds = 2 }))
    it "are refused out of range: ComIDs past 0xFFFF or reserved, none, a version past 4 bits, a limit of none" $
      map (length . simParamsProblems . ($ defaultSimParams))
        [ \p -> p { spBaseComId = 0xFFFF, spNComIds = 2 }
        , \p -> p { spBaseComId = 0x0800 }
        , \p -> p { spNComIds = 0 }
        , \p -> p { spSscMinor = 0x10 }
        , \p -> p { spDataRemovalVersion = 0x10 }
        , \p -> p { spTPerProperties = [(maxMethodsName, 0)] } ]
        `shouldSatisfy` all (> 0)

  describe "events and ticks" $ do
    it "an event puts every static ComID back to awaiting IF-SEND, Good" $ do
      let st = (tper (\p -> p { spNComIds = 3 })) { tsComIds = Map.fromList [ (base + i, ComIdState 0 Issued s) | (i, s) <- zip [0 ..] [S1, S2 ["a"], S3 ["b"]] ] }
      forM_ [minBound .. maxBound] $ \e ->
        step (Ev e) st `shouldBe` (Evented Good, tper (\p -> p { spNComIds = 3 }))
    it "a Tick releases a prepared response, S2 to S3, and leaves any other state" $ do
      step (Tick base) (withSync (S2 ["p", "q"]) (tper id)) `shouldBe` (Ticked, withSync (S3 ["p", "q"]) (tper id))
      forM_ [S1, S3 ["p"]] $ \s ->
        step (Tick base) (withSync s (tper id)) `shouldBe` (Ticked, withSync s (tper id))
      step (Tick 0x2000) (tper id) `shouldBe` (Ticked, tper id)
    it "syncOf reads a static ComID's state, and no other's" $ do
      syncOf base (withSync (S3 ["p"]) (tper id)) `shouldBe` Just (S3 ["p"])
      syncOf 0x2000 (tper id) `shouldBe` Nothing
  where
    isUnsupported (Unsupported _) = True
    isUnsupported _               = False
