{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Sim.ComIdMgmtSpec
Description : Test specifications for Hsed.Sim.ComIdMgmt through a step of the simulator: TPER_RESET, STACK_RESET, VERIFY_COMID_VALID, GET_COMID, GET_COMID_RESPONSE
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Milestone M4a's group G2 (design 11): protocol 0x02, outside the synchronous
protocol (Core 3.3.10.3 b, PDF 84), each command sent through "Hsed.Sim".@step@
to a TPer with sessions open, responses staged and host properties set.
TPER_RESET (Opal 2.30 3.2.3 and Table 13, PDF 28-29) is Other Invalid
Command Parameter while disabled, as it is at the factory (Table 27, PDF 50),
or of no length, and enabled resets the whole TPer; STACK_RESET (Core
3.3.4.7.5 and Tables 35-36, PDF 65-66) resets its ComID's stack and stages a
Success response that GET_COMID_RESPONSE returns once (Table 36 a) and then
No Response Available (Table 31, PDF 63; its Extended ComID zeros before any
request, PDF 66); a short request is Invalid Transfer Length on IF-SEND, a
request of another Extended ComID or an unknown code leaves No Response
Available; VERIFY_COMID_VALID (Tables 32-33, PDF 63-64) reports the ComID's
state, Issued or Associated, with zero times; GET_COMID (Tables 27-28, PDF
60-61) assigns no ComID, zeros; and a ComID in any synchronous state takes
the protocol.
-}
module Hsed.Sim.ComIdMgmtSpec (spec) where

import           Data.Bits             (shiftL, (.|.))
import qualified RIO.ByteString        as B
import qualified RIO.Map               as Map
import           RIO
import           Test.Hspec

import           Hsed.Core.Record         (DecodedRecord (..), LayoutRecord, decodeRecordStrict, encodeRecord)
import           Hsed.Core.Table          (Cell (..))
import           Hsed.Gen.Core.Layouts    (NoResponseAvailable (..), STACK_RESETCommandRequest (..),
                                           STACK_RESETCommandResponse (..), VERIFY_COMID_VALIDCommandResponse (..),
                                           VERIFY_COMID_VALIDRequest (..))
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs  (uC_PINTable)
import           Hsed.Gen.Core.UIDs       (uAuthority_Anybody, uC_PIN_SID, uSP_Admin)
import           Hsed.OpalUIDs            (tperResetComId, uSP_Locking)
import           Hsed.Sim                 (step, syncOf)
import           Hsed.Sim.State           (cellOf, columnOf, openSession, rowWithUID, sessionsOn, updateRow)
import           Hsed.Sim.Types           (ComIdLife (..), ComIdState (..), Input (..), LifeCycle (..), MgmtBuffer (..),
                                           Output (..), SimParams (..), SyncState (..), TPerState (..), defaultSimParams,
                                           initialHostProperties, initialState, maxPacketsName)
import           Hsed.Wire                (Dir (..), IfStatus (..), Request (..), Response (..), Unit (..),
                                           comIdManagementProtocol)


base :: Word16
base = spBaseComId defaultSimParams

-- | A TPer of two static ComIDs, busy on both: a session open on each (the
--   Locking SP Manufactured so that two SPs take one), responses staged, a
--   host property set, a management buffer filled, C_PIN_SID's Tries 2.
busy :: TPerState
busy = st3 { tsComIds = Map.adjust onBase base (Map.adjust onNext (base + 1) (tsComIds st3)) }
  where
    p   = defaultSimParams { spNComIds = 2, spLockingOFS = Manufactured, spTPerResetEnabled = True }
    st0 = initialState p
    (_, st1) = openSession base 1 uSP_Admin True [uAuthority_Anybody] st0
    (_, st2) = openSession (base + 1) 2 uSP_Locking True [uAuthority_Anybody] st1
    st3 = updateRow uSP_Admin uC_PINTable uC_PIN_SID (map (\(col, cell) -> (col, if Just col == columnOf c_PINTableDesc "Tries" then UIntCell 2 else cell))) st2
    onBase cs = cs { cSync = S3 ["staged"], cHostProps = Map.insert maxPacketsName 4 (cHostProps cs)
                   , cMgmt = MgmtBuffer True (Just (1, "a verify response")) }
    onNext cs = cs { cSync = S2 ["prepared"], cHostProps = Map.insert maxPacketsName 2 (cHostProps cs) }

-- | A command's answer and the TPer after it.
command :: Request -> TPerState -> (Response, TPerState)
command r st = case step (Cmd r) st of
  (Out resp, st') -> (resp, st')
  (other, st')    -> (Response (TransportFailure (tshow other)) mempty, st')

-- | An IF-SEND and an IF-RECV of protocol 0x02.
send :: Word16 -> ByteString -> Request
send c bytes = Request IfSend comIdManagementProtocol c (fromIntegral (B.length bytes)) bytes

recv :: Word16 -> Word32 -> Request
recv c len = Request IfRecv comIdManagementProtocol c len mempty

-- | A ComID's Extended ComID with extension 0.
extended :: Word16 -> Word32
extended c = fromIntegral c `shiftL` 16 .|. 0

-- | A request's bytes, by its generated layout.
encoded :: LayoutRecord a => a -> ByteString
encoded = either (error . show) id . encodeRecord

-- | A response decoded strictly by a layout, its pins telling it apart.
decoded :: forall a. LayoutRecord a => ByteString -> Maybe a
decoded = either (const Nothing) (Just . drRecord) . decodeRecordStrict @a

-- | A ComID's state, Tries and the like, read back.
comIdOf :: Word16 -> TPerState -> Maybe ComIdState
comIdOf c = Map.lookup c . tsComIds

triesOf :: TPerState -> Maybe Cell
triesOf st = rowWithUID uSP_Admin uC_PINTable uC_PIN_SID st >>= \row -> columnOf c_PINTableDesc "Tries" >>= (`cellOf` row)

spec :: Spec
spec = describe "protocol 0x02, ComID management, through a step of the simulator" $ do
  describe "TPER_RESET, an IF-SEND on ComID 0x0004 (Opal 3.2.3, Table 13)" $ do
    it "disabled, as at the factory (Table 27), is Other Invalid Command Parameter, the TPer unchanged" $ do
      let disabled = busy { tsParams = (tsParams busy) { spTPerResetEnabled = False } }
      command (send tperResetComId "\x00") disabled `shouldBe` (Response OtherInvalidCommandParameter mempty, disabled)
    it "enabled is Good: every session gone, every ComID awaiting IF-SEND with the host's initial properties and an empty buffer, Tries kept" $ do
      let (r, st') = command (send tperResetComId "\x00") busy
      r `shouldBe` Response Good mempty
      tsSessions st' `shouldBe` mempty
      map (\cs -> (cSync cs, cHostProps cs, cMgmt cs)) (Map.elems (tsComIds st'))
        `shouldBe` replicate 2 (S1, initialHostProperties, MgmtBuffer False Nothing)
      tsParams st' `shouldBe` tsParams busy
      triesOf st' `shouldBe` Just (UIntCell 2)
    it "of no length is refused, in bytes as in blocks (Table 13: non-zero)" $ do
      command (send tperResetComId "") busy `shouldBe` (Response OtherInvalidCommandParameter mempty, busy)
      let blocks = busy { tsParams = (tsParams busy) { spUnits = UnitBlocks512 } }
      command (send tperResetComId "") blocks `shouldBe` (Response OtherInvalidCommandParameter mempty, blocks)
      fst (command (Request IfSend comIdManagementProtocol tperResetComId 512 (B.replicate 512 0)) blocks) `shouldBe` Response Good mempty
    it "has no IF-RECV response: an IF-RECV on 0x0004 is Other Invalid Command Parameter" $
      command (recv tperResetComId 16) busy `shouldBe` (Response OtherInvalidCommandParameter mempty, busy)

  describe "STACK_RESET on a static ComID (Core 3.3.4.7.5, Tables 35-36)" $ do
    let request  = encoded (STACK_RESETCommandRequest (extended base))
        (r1, st1) = command (send base request) busy
        (r2, st2) = command (recv base 16) st1
    it "is Good, and GET_COMID_RESPONSE then decodes to Table 36's response, Success" $ do
      r1 `shouldBe` Response Good mempty
      status r2 `shouldBe` Good
      B.length (rpayload r2) `shouldBe` 16
      decoded (rpayload r2) `shouldBe` Just (STACK_RESETCommandResponse (extended base) 0)
    it "aborts that ComID's sessions with no CloseSession staged, puts it in S1 with the host's initial properties, and leaves the other ComID" $ do
      sessionsOn base st1 `shouldBe` []
      fmap (\cs -> (cSync cs, cHostProps cs)) (comIdOf base st1) `shouldBe` Just (S1, initialHostProperties)
      length (sessionsOn (base + 1) st1) `shouldBe` 1
      fmap (\cs -> (cSync cs, Map.lookup maxPacketsName (cHostProps cs))) (comIdOf (base + 1) st1) `shouldBe` Just (S2 ["prepared"], Just 2)
      triesOf st1 `shouldBe` Just (UIntCell 2)
    it "once retrieved entire, the response is cleared: a second read is No Response Available with the ComID's Extended ComID (Table 36 a)" $ do
      let (r3, st3) = command (recv base 12) st2
      decoded (rpayload r3) `shouldBe` Just (NoResponseAvailable (extended base))
      fmap cMgmt (comIdOf base st3) `shouldBe` Just (MgmtBuffer True Nothing)
    it "retrieved short of its 16 bytes, the response is kept for a longer read (3.3.4.7.2: the host may repeat with the available length)" $ do
      let (r, st')  = command (recv base 12) st1
          (r', _)   = command (recv base 64) st'
      B.length (rpayload r) `shouldBe` 12
      fmap (mbResponse . cMgmt) (comIdOf base st') `shouldSatisfy` maybe False isJust
      decoded (rpayload r') `shouldBe` Just (STACK_RESETCommandResponse (extended base) 0)
      B.drop 16 (rpayload r') `shouldBe` B.replicate 48 0
    it "before any request on a ComID, GET_COMID_RESPONSE is No Response Available with an Extended ComID of zeros (PDF 66)" $ do
      let fresh    = initialState defaultSimParams
          (r, st') = command (recv base 32) fresh
      decoded (rpayload r) `shouldBe` Just (NoResponseAvailable 0)
      st' `shouldBe` fresh
    it "of fewer than 8 bytes is Invalid Transfer Length parameter on IF-SEND, nothing changed (3.3.4.7.5)" $
      command (send base (B.take 4 request)) busy `shouldBe` (Response InvalidTransferLengthOnIfSend mempty, busy)
    it "with an Extended ComID other than the ComID's leaves No Response Available, the stack untouched" $ do
      let (r, st') = command (send base (encoded (STACK_RESETCommandRequest (extended (base + 1))))) busy
          (r', _)  = command (recv base 12) st'
      r `shouldBe` Response Good mempty
      decoded (rpayload r') `shouldBe` Just (NoResponseAvailable (extended base))
      fmap cSync (comIdOf base st') `shouldBe` Just (S3 ["staged"])
      length (sessionsOn base st') `shouldBe` 1
    it "with an unknown request code leaves No Response Available too (PDF 66 b)" $ do
      let (_, st') = command (send base (B.pack [0x10, 0x04, 0, 0, 0, 0, 0, 7])) busy
          (r', _)  = command (recv base 12) st'
      decoded (rpayload r') `shouldBe` Just (NoResponseAvailable (extended base))
    it "reserved bytes after the request are ignored (3.3.4.7.5: SHALL be ignored)" $ do
      let (_, st') = command (send base (request <> B.replicate 8 0xAB)) busy
      fmap cSync (comIdOf base st') `shouldBe` Just S1

  describe "VERIFY_COMID_VALID on a static ComID (Core 3.3.4.7.4, Tables 32-33)" $ do
    let request = encoded (VERIFY_COMID_VALIDRequest (extended base))
        verify st = let (_, st') = command (send base request) st
                        (r, st'') = command (recv base 46) st'
                    in (decoded (rpayload r) :: Maybe VERIFY_COMID_VALIDCommandResponse, st'')
    it "reports the ComID Issued (2) with no session open, with zero times, the stack and the sessions untouched" $ do
      let fresh = initialState defaultSimParams { spNComIds = 2 }
          (r, st') = verify fresh
      r `shouldBe` Just (VERIFY_COMID_VALIDCommandResponse (extended base) 2 (B.replicate 10 0) (B.replicate 10 0) (B.replicate 10 0))
      fmap (mbResponse . cMgmt) (comIdOf base st') `shouldSatisfy` maybe False isJust
      st' { tsComIds = Map.adjust (\cs -> cs { cMgmt = MgmtBuffer False Nothing }) base (tsComIds st') } `shouldBe` fresh
    it "reports it Associated (3) with a session open" $ do
      let (r, st') = verify busy
      fmap verify_COMID_VALIDCommandResponseCurrentStateOfExtendedComID r `shouldBe` Just 3
      length (sessionsOn base st') `shouldBe` 1
      fmap cSync (comIdOf base st') `shouldBe` Just (S3 ["staged"])
    it "its response is kept for consecutive reads (3.3.4.7)" $ do
      let (_, st') = verify busy
          (r, _)   = command (recv base 46) st'
      fmap verify_COMID_VALIDCommandResponseCurrentStateOfExtendedComID (decoded (rpayload r)) `shouldBe` Just 3

  describe "GET_COMID, an IF-RECV on ComID 0x0000 (Core 3.3.4.3, Tables 27-28)" $ do
    it "assigns no ComID: all zeros, cut or padded to the transfer length (3.3.4.3.1 a)" $ do
      forM_ [1, 4, 10] $ \len ->
        command (recv 0x0000 len) busy `shouldBe` (Response Good (B.replicate (fromIntegral len) 0), busy)
    it "an IF-SEND on 0x0000, a reserved ComID with no command, is Other Invalid Command Parameter" $
      command (send 0x0000 (B.replicate 8 0)) busy `shouldBe` (Response OtherInvalidCommandParameter mempty, busy)

  describe "the other ComIDs (Core 3.3.3.2-3)" $ do
    it "a reserved ComID the TPer does not support is Other Invalid Command Parameter" $ do
      command (send 0x0002 (B.replicate 8 0)) busy `shouldBe` (Response OtherInvalidCommandParameter mempty, busy)
      command (recv 0x0002 8) busy `shouldBe` (Response OtherInvalidCommandParameter mempty, busy)
    it "an inactive one is refused, or, if the run discards what is sent to inactive ComIDs, discarded, an IF-RECV No Response Available with extension 0xFFFF" $ do
      let inactive = busy { tsComIds = Map.adjust (\cs -> cs { cLife = Inactive }) (base + 1) (tsComIds busy) }
          discards = inactive { tsParams = (tsParams inactive) { spInactiveComIdDiscard = True } }
      command (send (base + 1) (B.replicate 8 0)) inactive `shouldBe` (Response OtherInvalidCommandParameter mempty, inactive)
      command (send (base + 1) (B.replicate 8 0)) discards `shouldBe` (Response Good mempty, discards)
      let (r, st') = command (recv (base + 1) 12) discards
      decoded (rpayload r) `shouldBe` Just (NoResponseAvailable (fromIntegral (base + 1) `shiftL` 16 .|. 0xFFFF))
      st' `shouldBe` discards
    it "a ComID the TPer has not is refused" $
      command (recv 0x2000 8) busy `shouldBe` (Response OtherInvalidCommandParameter mempty, busy)

  describe "outside the synchronous protocol (Core 3.3.10.3 b)" $
    it "is taken while the ComID is in S2 or S3: VERIFY leaves the state, STACK_RESET ends it" $ do
      forM_ [S2 ["p"], S3 ["q"]] $ \s -> do
        let st = busy { tsComIds = Map.adjust (\cs -> cs { cSync = s }) base (tsComIds busy) }
            (r, st') = command (send base (encoded (VERIFY_COMID_VALIDRequest (extended base)))) st
        r `shouldBe` Response Good mempty
        syncOf base st' `shouldBe` Just s
        let (r', st'') = command (send base (encoded (STACK_RESETCommandRequest (extended base)))) st
        r' `shouldBe` Response Good mempty
        syncOf base st'' `shouldBe` Just S1
