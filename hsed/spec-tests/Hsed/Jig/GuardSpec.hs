{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.GuardSpec
Description : Test specifications for Hsed.Jig.Guard
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Guard against the attached drive (design 9.3.2), over a @'ReadOnly@
test transport that counts what reaches it, without root or a kext: a
scripted Set (built with "Hsed.Core.Method".'Hsed.Core.Method.invocation',
framed by "Hsed.Packet" on ComID 0x1004) is refused under both policies,
recorded as blocked, and never reaches the transport; Level0Only admits
the Level 0 IF-RECV alone; the M4c allow list admits Properties,
StartSession to the Admin SP as Anybody or no one, Get on C_PIN_MSID of its
UID and PIN columns, and a bare EndOfSession, and refuses everything else,
what does not parse and ComID management, but for STACK_RESET on a static
ComID and its GET_COMID_RESPONSE under AllowStackReset, which still refuses
TPER_RESET (ComID 0x0004) and VERIFY_COMID_VALID; a transport behind the
Guard answers what it refuses 'Unsupported'. The protocol, ComID and Kind
the Guard reads are Core's (Tables 25, 26 and 19).
-}
module Hsed.Jig.GuardSpec (spec) where

import qualified RIO.ByteString          as B
import           RIO
import           Test.Hspec

import           Hsed.Core.CellBlock     (CellBlock (..), cellBlockValue, columnRange, noCells)
import           Hsed.Core.Method        (invocation)
import           Hsed.Core.Record        (encodeRecord)
import           Hsed.Core.StreamItem    (StreamItem (..))
import           Hsed.Core.Table         (Col (..), columnNamed)
import           Hsed.Core.Token         (IsToken (..), Token (..))
import           Hsed.Core.UID           (UID, uid)
import           Hsed.Core.Value         (IsValue (..), List (..), Value (..), (.:))
import           Hsed.Gen.Core.Layouts   (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..),
                                          STACK_RESETCommandRequest (..), VERIFY_COMID_VALIDRequest (..))
import qualified Hsed.Gen.Core.Methods   as Methods
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc)
import           Hsed.Gen.Core.TableUIDs (uC_PINTable)
import           Hsed.Gen.Core.UIDs      (uAuthority_Anybody, uAuthority_SID, uC_PIN_SID, uSMUID, uSP_Admin)
import           Hsed.Jig.Guard
import           Hsed.Jig.Transcript
import           Hsed.OpalUIDs           (tperResetComId, uC_PIN_MSID)
import           Hsed.Packet             (ComPacketFrame (..), PacketFrame (..), encodeComPacket)
import           Hsed.Wire               (comPacketProtocol, dataKind, level0ComId)
import           Hsed.Wire.Internal      (Cap (..), Describe (..), Dir (..), IfStatus (..), Request (..), Response (..),
                                          Transport (..), Unit (..))


-- | A drive's transport that counts the commands that reach it and answers
--   each Good, an IF-RECV with as many zeros as it asks for.
counting :: IO (IORef Int, Transport 'ReadOnly)
counting = do
  n <- newIORef 0
  pure (n, MkTransport
    { tExchange = \q -> do
        modifyIORef' n (+ 1)
        pure (Response Good (if dir q == IfRecv then B.replicate (fromIntegral (lenBytes q)) 0 else mempty))
    , tInject   = \_ -> pure (Unsupported "a drive")
    , tUnit     = UnitBlocks512
    , tDescribe = Describe "drive" "a test" UnitBlocks512 Nothing Nothing Nothing Nothing
    , tClose    = pure ()
    })

-- | Tokens in one data subpacket of one packet of the control session, in a
--   ComPacket on a ComID.
framed :: Word16 -> ByteString -> ByteString
framed c tokens = either (error . show) id $ encodeComPacket $
  ComPacketFrame (ComPacketHeader c 0 0 0) [PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader 0) tokens]]

-- | An IF-SEND of tokens so framed on 0x1004, padded to a 512-byte block.
sendOf :: ByteString -> Request
sendOf tokens = Request IfSend 1 0x1004 512 (bytes <> B.replicate (512 - B.length bytes) 0)
  where bytes = framed 0x1004 tokens

-- | An IF-SEND of protocol 2 on a ComID carrying bytes, padded to a block.
mgmtSend :: Word16 -> ByteString -> Request
mgmtSend c bytes = Request IfSend 2 c 512 (bytes <> B.replicate (512 - B.length bytes) 0)

-- | A Set of C_PIN_SID's PIN: what the Guard is there to stop.
setPin :: ByteString
setPin = generate (invocation uC_PIN_SID (Methods.Set Nothing (Just (L (List [N ((3 :: Natural) .: ("not the PIN" :: ByteString))])))))

-- | SMUID.Properties, no HostProperties.
props :: ByteString
props = generate (invocation uSMUID (Methods.Properties Nothing))

-- | SMUID.StartSession to an SP, as an authority or no one.
startSession :: UID -> Maybe UID -> ByteString
startSession sp authority = generate $ invocation uSMUID Methods.StartSession
  { Methods.startSessionHostSessionID         = value (1 :: Natural)
  , Methods.startSessionSPID                  = value (token sp)
  , Methods.startSessionWrite                 = value (1 :: Natural)
  , Methods.startSessionHostChallenge         = Nothing
  , Methods.startSessionHostExchangeAuthority = Nothing
  , Methods.startSessionHostExchangeCert      = Nothing
  , Methods.startSessionHostSigningAuthority  = value . token <$> authority
  , Methods.startSessionHostSigningCert       = Nothing
  , Methods.startSessionSessionTimeout        = Nothing
  , Methods.startSessionTransTimeout          = Nothing
  , Methods.startSessionInitialCredit         = Nothing
  , Methods.startSessionSignedHash            = Nothing
  }

-- | Get on an object of a cell_block, as UCT-03 step 1b's verb builds it.
getOf :: UID -> CellBlock -> ByteString
getOf object cb = generate (invocation object (Methods.Get (cellBlockValue cb)))

-- | The PIN column's number in the generated C_PIN description: 3 (Core
--   Table 181, PDF 171).
pinColumn :: Natural
pinColumn = maybe 3 (fromIntegral . colNumber) (columnNamed "PIN" c_PINTableDesc)

-- | Table 35's STACK_RESET request for an Extended ComID, and Table 32's
--   VERIFY_COMID_VALID request (Core PDF 63, 65).
stackResetRequest, verifyRequest :: Word32 -> ByteString
stackResetRequest ext = either (error . show) id (encodeRecord (STACK_RESETCommandRequest ext))
verifyRequest ext     = either (error . show) id (encodeRecord (VERIFY_COMID_VALIDRequest ext))

-- | Opal's Locking SP (design 8.3), an SP other than the Admin SP.
lockingSP :: UID
lockingSP = uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x02

allowAll :: Policy
allowAll = AllowList [minBound .. maxBound]

-- | The list UCT-03's verbs need (design 9.3; the plan's acceptance).
uct03List :: Policy
uct03List = AllowList [AllowProperties, AllowStartSessionAdminSP, AllowGetCPinMSID, AllowEndOfSession]

-- | The recovery list, STACK_RESET alone (@--recover@).
recovery :: Policy
recovery = AllowList [AllowStackReset]

-- | A command through the Guard: its decision, how many commands reached
--   the transport, and how many entries the transcript blocked.
through :: (Transcript -> Policy -> Transport 'ReadOnly -> Request -> IO (Either Text Response))
        -> Policy -> Request -> IO (Either Text Response, Int, Int)
through guarded policy q = do
  t <- newTranscript
  (n, tr) <- counting
  decision <- guarded t policy tr q
  reached <- readIORef n
  es <- entries t
  pure (decision, reached, length [ () | Blocked {} <- es ])

refused :: (Either Text Response, Int, Int) -> Bool
refused (decision, reached, blocked) = isLeft decision && reached == 0 && blocked == 1

passed :: (Either Text Response, Int, Int) -> Bool
passed (decision, reached, blocked) = isRight decision && reached == 1 && blocked == 0

spec :: Spec
spec = do
  describe "a scripted Set" $
    it "is refused, recorded as blocked, the transport never called, under Level0Only and under the allow list" $ do
      for [Level0Only, allowAll] (\p -> through guardedSend p (sendOf setPin)) >>= \rs -> do
        map (\(d, r, b) -> (isLeft d, r, b)) rs `shouldBe` [(True, 0, 1), (True, 0, 1)]

  describe "Level0Only" $ do
    it "admits the IF-RECV of Level 0 Discovery (protocol 1, ComID 1)" $
      through guardedRecv Level0Only (Request IfRecv 1 1 2048 mempty) >>= (`shouldSatisfy` passed)
    it "refuses an IF-RECV on 0x1004, and one of protocol 2" $ do
      through guardedRecv Level0Only (Request IfRecv 1 0x1004 2048 mempty) >>= (`shouldSatisfy` refused)
      through guardedRecv Level0Only (Request IfRecv 2 1 512 mempty) >>= (`shouldSatisfy` refused)
    it "refuses every IF-SEND, Properties too, and STACK_RESET" $ do
      through guardedSend Level0Only (sendOf props) >>= (`shouldSatisfy` refused)
      through guardedSend Level0Only (Request IfSend 1 1 512 (B.replicate 512 0)) >>= (`shouldSatisfy` refused)
      through guardedSend Level0Only (mgmtSend 0x1004 (stackResetRequest 0x10040000)) >>= (`shouldSatisfy` refused)

  describe "the allow list (M4c)" $ do
    it "admits Properties" $
      through guardedSend allowAll (sendOf props) >>= (`shouldSatisfy` passed)
    it "admits StartSession to the Admin SP as Anybody or no one, not to another SP or as SID" $ do
      through guardedSend allowAll (sendOf (startSession uSP_Admin (Just uAuthority_Anybody))) >>= (`shouldSatisfy` passed)
      through guardedSend allowAll (sendOf (startSession uSP_Admin Nothing)) >>= (`shouldSatisfy` passed)
      through guardedSend allowAll (sendOf (startSession lockingSP Nothing)) >>= (`shouldSatisfy` refused)
      through guardedSend allowAll (sendOf (startSession uSP_Admin (Just uAuthority_SID))) >>= (`shouldSatisfy` refused)
    it "admits a bare EndOfSession" $
      through guardedSend allowAll (sendOf (generate EndOfSession)) >>= (`shouldSatisfy` passed)
    it "admits only the methods it lists" $ do
      through guardedSend (AllowList [AllowEndOfSession]) (sendOf props) >>= (`shouldSatisfy` refused)
      through guardedSend (AllowList [AllowProperties]) (sendOf (generate EndOfSession)) >>= (`shouldSatisfy` refused)
      through guardedSend (AllowList [AllowProperties]) (sendOf (startSession uSP_Admin Nothing)) >>= (`shouldSatisfy` refused)
      through guardedSend (AllowList [AllowProperties]) (sendOf (getOf uC_PIN_MSID (columnRange pinColumn pinColumn))) >>= (`shouldSatisfy` refused)
    it "refuses what does not parse: no ComPacket, two methods in a subpacket, a ComPacket for another ComID, a credit subpacket" $ do
      through guardedSend allowAll (Request IfSend 1 0x1004 512 (B.replicate 512 0xFF)) >>= (`shouldSatisfy` refused)
      through guardedSend allowAll (sendOf (props <> props)) >>= (`shouldSatisfy` refused)
      let elsewhere = framed 0x1005 props
      through guardedSend allowAll (Request IfSend 1 0x1004 512 (elsewhere <> B.replicate (512 - B.length elsewhere) 0)) >>= (`shouldSatisfy` refused)
      let credit = either (error . show) id $ encodeComPacket $
            ComPacketFrame (ComPacketHeader 0x1004 0 0 0) [PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader 0x8001) props]]
      through guardedSend allowAll (Request IfSend 1 0x1004 512 (credit <> B.replicate (512 - B.length credit) 0)) >>= (`shouldSatisfy` refused)
    it "refuses ComID management (protocol 2), IF-RECV and IF-SEND, without AllowStackReset; with it, an IF-SEND that is no STACK_RESET request" $ do
      through guardedRecv uct03List (Request IfRecv 2 0x1004 512 mempty) >>= (`shouldSatisfy` refused)
      through guardedSend uct03List ((sendOf props) { proto = 2 }) >>= (`shouldSatisfy` refused)
      through guardedSend allowAll ((sendOf props) { proto = 2 }) >>= (`shouldSatisfy` refused)
    it "admits IF-RECV of protocol 1" $
      through guardedRecv allowAll (Request IfRecv 1 0x1004 2048 mempty) >>= (`shouldSatisfy` passed)

  describe "the allow list UCT-03 needs: Properties, StartSession to the Admin SP, Get on C_PIN_MSID, End of Session" $ do
    it "admits the payloads UCT-03's verbs build: Properties, StartSession as no one, Get on C_PIN_MSID of its PIN column or its UID column, a bare End of Session" $
      for_ [ props, startSession uSP_Admin Nothing, getOf uC_PIN_MSID (columnRange pinColumn pinColumn), getOf uC_PIN_MSID (columnRange 0 0)
           , generate EndOfSession ] $ \tokens ->
        through guardedSend uct03List (sendOf tokens) >>= (`shouldSatisfy` passed)
    it "refuses Get on C_PIN_SID, a cell_block naming Table (its component 0), one covering Name and CommonName, one of every column, a Set, and StartSession as SID: each with no exchange and one Blocked entry" $
      for_ [ getOf uC_PIN_SID (columnRange pinColumn pinColumn), getOf uC_PIN_MSID (columnRange pinColumn pinColumn) { cbTable = Just uC_PINTable }
           , getOf uC_PIN_MSID (columnRange 0 pinColumn), getOf uC_PIN_MSID noCells, setPin, startSession uSP_Admin (Just uAuthority_SID) ] $ \tokens ->
        through guardedSend uct03List (sendOf tokens) >>= (`shouldSatisfy` refused)
    it "reads the UID and PIN columns from the generated C_PIN description: 0 and 3 (Core Table 181, PDF 171)" $
      [ colNumber col | name <- ["UID", "PIN"], Just col <- [columnNamed name c_PINTableDesc] ] `shouldBe` [0, 3]

  describe "AllowStackReset (--recover, design 9.3)" $ do
    it "admits exactly STACK_RESET on the base ComID, Table 35's request for its Extended ComID, and the protocol-2 IF-RECV of its GET_COMID_RESPONSE" $ do
      through guardedSend recovery (mgmtSend 0x1004 (stackResetRequest 0x10040000)) >>= (`shouldSatisfy` passed)
      through guardedRecv recovery (Request IfRecv 2 0x1004 512 mempty) >>= (`shouldSatisfy` passed)
    it "refuses TPER_RESET (ComID 0x0004, Opal Table 13), GET_COMID's ComID 0x0000, and VERIFY_COMID_VALID" $ do
      through guardedSend recovery (mgmtSend tperResetComId (B.replicate 512 0)) >>= (`shouldSatisfy` refused)
      through guardedSend recovery (mgmtSend tperResetComId (stackResetRequest 0x00040000)) >>= (`shouldSatisfy` refused)
      through guardedRecv recovery (Request IfRecv 2 tperResetComId 512 mempty) >>= (`shouldSatisfy` refused)
      through guardedSend recovery (mgmtSend 0x0000 (stackResetRequest 0)) >>= (`shouldSatisfy` refused)
      through guardedRecv recovery (Request IfRecv 2 0x0000 512 mempty) >>= (`shouldSatisfy` refused)
      through guardedSend recovery (mgmtSend 0x1004 (verifyRequest 0x10040000)) >>= (`shouldSatisfy` refused)
    it "refuses a request for another ComID's Extended ComID, or an extension other than a static ComID's, bytes after the request, and what is no request" $ do
      through guardedSend recovery (mgmtSend 0x1004 (stackResetRequest 0x10050000)) >>= (`shouldSatisfy` refused)
      through guardedSend recovery (mgmtSend 0x1004 (stackResetRequest 0x10040001)) >>= (`shouldSatisfy` refused)
      through guardedSend recovery (mgmtSend 0x1004 (stackResetRequest 0x10040000 <> "\x01")) >>= (`shouldSatisfy` refused)
      through guardedSend recovery (Request IfSend 2 0x1004 512 (B.replicate 512 0xFF)) >>= (`shouldSatisfy` refused)
      through guardedSend recovery (Request IfSend 2 0x1004 4 (B.replicate 4 0)) >>= (`shouldSatisfy` refused)
    it "admits nothing of protocol 1 by itself" $ do
      through guardedSend recovery (sendOf props) >>= (`shouldSatisfy` refused)
      through guardedRecv recovery (Request IfRecv 1 0x1004 2048 mempty) >>= (`shouldSatisfy` passed)

  describe "guardedSend and guardedRecv" $
    it "each refuse the other's direction, whatever the policy" $ do
      through guardedRecv allowAll (sendOf props) >>= (`shouldSatisfy` refused)
      through guardedSend allowAll (Request IfRecv 1 1 2048 mempty) >>= (`shouldSatisfy` refused)

  describe "guarding" $
    it "puts the Guard in front of every command: a refused one answered Unsupported, recorded, never issued" $ do
      t <- newTranscript
      (n, tr) <- counting
      let guarded = guarding t Level0Only tr
      blocked <- tExchange guarded (sendOf setPin)
      level0 <- tExchange guarded (Request IfRecv 1 1 512 mempty)
      readIORef n `shouldReturn` 1
      (status blocked, status level0) `shouldSatisfy` \(b, l) -> case b of
        Unsupported _ -> l == Good
        _             -> False
      es <- entries t
      length [ () | Blocked {} <- es ] `shouldBe` 1

  describe "the protocol, ComID and Kind it reads (Hsed.Wire's)" $
    it "are Core's: protocol 0x01 (Table 25), Level 0 Discovery's ComID 0x0001 (Table 26), the Data Kind 0x0000 (Table 19)" $
      (comPacketProtocol, level0ComId, dataKind) `shouldBe` (0x01, 0x0001, 0x0000)
