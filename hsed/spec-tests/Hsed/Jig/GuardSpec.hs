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
StartSession to the Admin SP with no optional parameter but
HostSigningAuthority = Anybody, Get on C_PIN_MSID of its UID and PIN
columns, and a bare EndOfSession, and refuses everything else, what does
not parse and ComID management, but for STACK_RESET and its
GET_COMID_RESPONSE under AllowStackReset, which still refuses TPER_RESET
(ComID 0x0004) and VERIFY_COMID_VALID; and it holds every command to its
literal base ComID, 0x1004 here (design 13.31 (c)): IF-RECV of protocol 1
there or on Level 0's ComID alone, IF-SEND and ComID management there
alone, an IF-SEND's payload exactly its transfer length, its ComPacket's
extension a static ComID's and its OutstandingData and MinTransfer the
host's zero (Core 3.3.3.1, PDF 57; 3.2.3.2.1.4-5, PDF 41); a policy whose
base Core Table 26 reserves admits Level 0 Discovery alone. A transport
behind the Guard answers what it refuses 'Unsupported'. A drive behind the Guard
('guardDrive', M4c) records each command in the run's transcript and the
drive's own exactly as M4b's run did, Level0Only before its base ComID is
bound and under its allow list after, and before a bind refuses every
IF-SEND without its raw transport called; 'driveBind' binds nothing where
binding admits nothing, refuses a base Core Table 26 reserves, binds 0x1004
once (again idempotently) and refuses another base after it, and leaves
the drive Level0Only where the opener's bind refuses (a replay's); a
policy prints with its base and its methods by name. The protocol, ComID
and Kind the Guard reads are Core's (Tables 25, 26 and 19).
-}
module Hsed.Jig.GuardSpec (spec) where

import qualified Data.Aeson              as A
import qualified RIO.ByteString          as B
import           RIO
import qualified RIO.Text                as T
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
import           Hsed.Gen.Opal.Preconfig.AdminSP (uC_PIN_MSID)
import           Hsed.Jig.Guard
import           Hsed.Jig.Transcript
import           Hsed.Packet             (ComPacketFrame (..), PacketFrame (..), encodeComPacket)
import           Hsed.Transport.Drive    (RawDrive (..))
import           Hsed.Wire               (comPacketProtocol, dataKind, level0ComId, tperResetComId)
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
framed c = framedUnder (ComPacketHeader c 0 0 0)

-- | The same under a ComPacket header given: its ComID, extension,
--   OutstandingData and MinTransfer.
framedUnder :: ComPacketHeader -> ByteString -> ByteString
framedUnder h tokens = either (error . show) id $ encodeComPacket $
  ComPacketFrame h [PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader 0) tokens]]

-- | An IF-SEND of tokens so framed on 0x1004, padded to a 512-byte block.
sendOf :: ByteString -> Request
sendOf = sendOn 0x1004

-- | An IF-SEND of tokens so framed on a ComID, on that ComID, padded to a
--   512-byte block.
sendOn :: Word16 -> ByteString -> Request
sendOn c = blockOn c . framed c

-- | An IF-SEND of protocol 1 of bytes on a ComID, padded to a 512-byte
--   block.
blockOn :: Word16 -> ByteString -> Request
blockOn c bytes = Request IfSend 1 c 512 (bytes <> B.replicate (512 - B.length bytes) 0)

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
startSession sp authority = startCall (startArgs sp authority)

-- | SMUID.StartSession of these parameters, as tokens.
startCall :: Methods.StartSession -> ByteString
startCall = generate . invocation uSMUID

-- | StartSession's parameters to an SP, as an authority or no one: HSN 1,
--   read-write, no other optional parameter.
startArgs :: UID -> Maybe UID -> Methods.StartSession
startArgs sp authority = Methods.StartSession
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
allowAll = AllowList 0x1004 [minBound .. maxBound]

-- | The list UCT-03's verbs need (design 9.3; the plan's acceptance).
uct03List :: Policy
uct03List = AllowList 0x1004 [AllowProperties, AllowStartSessionAdminSP, AllowGetCPinMSID, AllowEndOfSession]

-- | A drive run's read-only methods on the base ComID 0x1004, with
--   STACK_RESET or without (design 13.7).
readOnly, readOnlyAndReset :: Policy
readOnly         = AllowList 0x1004 readOnlyMethods
readOnlyAndReset = AllowList 0x1004 (readOnlyMethods <> [AllowStackReset])

-- | The recovery list, STACK_RESET alone (@--recover@).
recovery :: Policy
recovery = AllowList 0x1004 [AllowStackReset]

-- | A raw drive over a counting transport whose bind records each base it
--   is asked for and answers as told: what reached the transport, the binds
--   asked, and the drive.
rawDrive :: Either Text () -> IO (IORef Int, IORef [Word16], RawDrive)
rawDrive answer = do
  (n, tr) <- counting
  binds <- newIORef []
  pure (n, binds, RawDrive tr (\b -> answer <$ modifyIORef' binds (<> [b])))

-- | Commands given to a drive two ways, each on fresh transcripts and a
--   fresh counting transport: as M4b's run gave them (the Guard of
--   "Hsed.Jig.Script" over the Guard of 'guarding', over a transport
--   recording into the run's transcript, over one recording into the
--   drive's own, under one policy), and through a 'GuardedDrive' bound to
--   0x1004 (with these methods) or not: the run's entries, the drive's
--   own, and how many commands reached the transport, each way. Each
--   command is given through the verb of its direction, then again through
--   the drive's transport.
bothWays :: Maybe [Allowed] -> [Request] -> IO (([TEntry], [TEntry], Int), ([TEntry], [TEntry], Int))
bothWays bound qs = do
  m4b <- do
    t <- newTranscript
    own <- newTranscript
    (n, tr) <- counting
    let policy  = maybe Level0Only (AllowList 0x1004) bound
        guarded = guarding t policy (recording t (recording own tr))
    for_ qs $ \q -> do
      _ <- (if dir q == IfSend then guardedSend else guardedRecv) t policy guarded q
      tExchange guarded q
    (,,) <$> entries t <*> entries own <*> readIORef n
  m4c <- do
    t <- newTranscript
    (n, _, raw) <- rawDrive (Right ())
    g <- guardDrive t (fromMaybe readOnlyMethods bound) Nothing raw
    for_ bound $ \_ -> driveBind g 0x1004 >>= either (throwString . show) pure
    for_ qs $ \q -> do
      _ <- (if dir q == IfSend then driveSend else driveRecv) g q
      tExchange (driveTransport g) q
    (,,) <$> entries t <*> driveIssued g <*> readIORef n
  pure (m4b, m4c)

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

  describe "Level0Only (unchanged at M4c)" $ do
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
      through guardedSend (AllowList 0x1004 [AllowEndOfSession]) (sendOf props) >>= (`shouldSatisfy` refused)
      through guardedSend (AllowList 0x1004 [AllowProperties]) (sendOf (generate EndOfSession)) >>= (`shouldSatisfy` refused)
      through guardedSend (AllowList 0x1004 [AllowProperties]) (sendOf (startSession uSP_Admin Nothing)) >>= (`shouldSatisfy` refused)
      through guardedSend (AllowList 0x1004 [AllowProperties]) (sendOf (getOf uC_PIN_MSID (columnRange pinColumn pinColumn))) >>= (`shouldSatisfy` refused)
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

  describe "the allow list holds every command to its base ComID, 0x1004 (M4c, design 13.31 (c))" $ do
    it "admits IF-RECV of protocol 1 on the base ComID and on Level 0 Discovery's alone: 0x1004 and 0x0001, not 0x1005, 0x0800 or 0x0000" $ do
      for_ [0x1004, 0x0001] $ \c -> through guardedRecv readOnly (Request IfRecv 1 c 2048 mempty) >>= (`shouldSatisfy` passed)
      for_ [0x1005, 0x0800, 0x0000] $ \c -> through guardedRecv readOnly (Request IfRecv 1 c 2048 mempty) >>= (`shouldSatisfy` refused)
    it "refuses Properties on 0x1005, its ComPacket on that ComID too" $ do
      through guardedSend readOnly (sendOn 0x1005 props) >>= (`shouldSatisfy` refused)
      through guardedSend readOnly (sendOn 0x1004 props) >>= (`shouldSatisfy` passed)
    it "refuses an IF-SEND whose payload is longer or shorter than its transfer length, of either protocol" $ do
      let q = sendOf props
          reset = mgmtSend 0x1004 (stackResetRequest 0x10040000)
      for_ [q { payload = payload q <> B.replicate 512 0 }, q { lenBytes = 1024 }, q { payload = B.take 511 (payload q) }
           , reset { lenBytes = 1024 }, reset { payload = payload reset <> "\x00" }] $ \bad ->
        through guardedSend readOnlyAndReset bad >>= (`shouldSatisfy` refused)
      for_ [q, reset] $ \good -> through guardedSend readOnlyAndReset good >>= (`shouldSatisfy` passed)
    it "refuses a ComPacket of extension 0x0001, of OutstandingData 1 or of MinTransfer 1, the host's being 0 (Core 3.3.3.1, PDF 57; 3.2.3.2.1.4-5, PDF 41)" $ do
      for_ [ComPacketHeader 0x1004 0x0001 0 0, ComPacketHeader 0x1004 0 1 0, ComPacketHeader 0x1004 0 0 1] $ \h ->
        through guardedSend readOnly (blockOn 0x1004 (framedUnder h props)) >>= (`shouldSatisfy` refused)
      through guardedSend readOnly (blockOn 0x1004 (framedUnder (ComPacketHeader 0x1004 0 0 0) props)) >>= (`shouldSatisfy` passed)
    it "admits StartSession with no optional parameter or with HostSigningAuthority = Anybody alone; refuses one with HostChallenge, SessionTimeout or HostExchangeAuthority (Core 5.2.3.1, PDF 149)" $ do
      for_ [startSession uSP_Admin Nothing, startSession uSP_Admin (Just uAuthority_Anybody)] $ \tokens ->
        through guardedSend readOnly (sendOf tokens) >>= (`shouldSatisfy` passed)
      let anybody = startArgs uSP_Admin (Just uAuthority_Anybody)
          noOne   = startArgs uSP_Admin Nothing
      for_ [ anybody { Methods.startSessionHostChallenge = Just (value ("a password" :: ByteString)) }
           , noOne { Methods.startSessionHostChallenge = Just (value ("a password" :: ByteString)) }
           , anybody { Methods.startSessionSessionTimeout = Just (value (60000 :: Natural)) }
           , noOne { Methods.startSessionHostExchangeAuthority = Just (value (token uAuthority_Anybody)) } ] $ \args ->
        through guardedSend readOnly (sendOf (startCall args)) >>= (`shouldSatisfy` refused)
    it "takes STACK_RESET and GET_COMID_RESPONSE on the base ComID alone, and only with AllowStackReset" $ do
      through guardedSend readOnlyAndReset (mgmtSend 0x1004 (stackResetRequest 0x10040000)) >>= (`shouldSatisfy` passed)
      through guardedRecv readOnlyAndReset (Request IfRecv 2 0x1004 512 mempty) >>= (`shouldSatisfy` passed)
      through guardedSend readOnlyAndReset (mgmtSend 0x1005 (stackResetRequest 0x10050000)) >>= (`shouldSatisfy` refused)
      through guardedRecv readOnlyAndReset (Request IfRecv 2 0x1005 512 mempty) >>= (`shouldSatisfy` refused)
      through guardedSend readOnly (mgmtSend 0x1004 (stackResetRequest 0x10040000)) >>= (`shouldSatisfy` refused)
      through guardedRecv readOnly (Request IfRecv 2 0x1004 512 mempty) >>= (`shouldSatisfy` refused)
    it "admits Level 0 Discovery alone under a policy whose base ComID Core Table 26 reserves (no drive's: driveBind refuses it)" $ do
      let reservedBase = AllowList 0x0004 [minBound .. maxBound]
      through guardedRecv reservedBase (Request IfRecv 1 1 2048 mempty) >>= (`shouldSatisfy` passed)
      through guardedRecv reservedBase (Request IfRecv 1 0x0004 2048 mempty) >>= (`shouldSatisfy` refused)
      through guardedSend reservedBase (sendOn 0x0004 props) >>= (`shouldSatisfy` refused)
      through guardedSend reservedBase (mgmtSend 0x0004 (stackResetRequest 0x00040000)) >>= (`shouldSatisfy` refused)
    it "says why it refuses, naming the ComID and the base, the lengths, the field" $ do
      let reasonOf p q = either id (const "admitted") (decide p q)
      reasonOf readOnly (Request IfRecv 1 0x1005 2048 mempty) `shouldSatisfy` T.isInfixOf "on ComID 0x1005, not the base ComID 0x1004"
      reasonOf readOnly ((sendOf props) { lenBytes = 1024 }) `shouldSatisfy` T.isInfixOf "an IF-SEND of 512 bytes on a transfer of 1024"
      reasonOf readOnly (blockOn 0x1004 (framedUnder (ComPacketHeader 0x1004 0 0 1) props)) `shouldSatisfy` T.isInfixOf "MinTransfer 1"
      reasonOf readOnly (sendOf (startCall (startArgs uSP_Admin Nothing) { Methods.startSessionSessionTimeout = Just (value (1 :: Natural)) }))
        `shouldSatisfy` T.isInfixOf "SessionTimeout: no optional parameter but HostSigningAuthority = Anybody"

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

  describe "a drive behind the Guard (guardDrive, design 13.29 (l))" $ do
    it "records every command in the run's transcript and the drive's own as M4b's run did, Level0Only before a bind" $ do
      let qs = [Request IfRecv 1 1 2048 mempty, sendOf props, Request IfRecv 1 0x1004 2048 mempty, sendOf setPin]
      (m4b, m4c) <- bothWays Nothing qs
      m4c `shouldBe` m4b
      let (runEntries, own, reached) = m4c
      (length [ () | Issued {} <- runEntries ], length [ () | Blocked {} <- runEntries ], length own, reached) `shouldBe` (2, 6, 2, 2)
    it "records as M4b's run did under the allow list, once bound" $ do
      let qs = [Request IfRecv 1 1 2048 mempty, sendOf props, Request IfRecv 1 0x1004 2048 mempty, sendOf setPin]
      (m4b, m4c) <- bothWays (Just readOnlyMethods) qs
      m4c `shouldBe` m4b
      let (_, own, reached) = m4c
      (length own, reached) `shouldBe` (6, 6)
    it "before a bind refuses every IF-SEND, records it Blocked, and never calls the raw transport" $ do
      t <- newTranscript
      (n, _, raw) <- rawDrive (Right ())
      g <- guardDrive t readOnlyMethods Nothing raw
      decisions <- for [sendOf props, sendOf (generate EndOfSession), mgmtSend 0x1004 (stackResetRequest 0x10040000)] (driveSend g)
      map isLeft decisions `shouldBe` [True, True, True]
      readIORef n `shouldReturn` 0
      entries t >>= \es -> length [ () | Blocked {} <- es ] `shouldBe` 3
      drivePolicy g `shouldReturn` Level0Only
    it "driveBind: refuses every base where binding admits nothing, its raw bind never asked" $ do
      t <- newTranscript
      (_, binds, raw) <- rawDrive (Right ())
      g <- guardDrive t [] Nothing raw
      driveBind g 0x1004 >>= (`shouldSatisfy` isLeft)
      readIORef binds `shouldReturn` []
      drivePolicy g `shouldReturn` Level0Only
    it "driveBind: refuses a base Core Table 26 reserves (0x0000, 0x0004, 0x0800, 0x0FFF); binds 0x1004 once, again idempotently, then refuses 0x1005" $ do
      t <- newTranscript
      (_, binds, raw) <- rawDrive (Right ())
      g <- guardDrive t readOnlyMethods Nothing raw
      for [0x0000, 0x0004, 0x0800, 0x0FFF] (driveBind g) >>= (`shouldSatisfy` all isLeft)
      drivePolicy g `shouldReturn` Level0Only
      driveBind g 0x1004 `shouldReturn` Right ()
      drivePolicy g `shouldReturn` AllowList 0x1004 readOnlyMethods
      driveBind g 0x1004 `shouldReturn` Right ()
      driveBind g 0x1005 >>= (`shouldSatisfy` isLeft)
      readIORef binds `shouldReturn` [0x1004]
      drivePolicy g `shouldReturn` AllowList 0x1004 readOnlyMethods
    it "driveBind: a raw bind that refuses leaves the drive Level0Only, its reason given" $ do
      t <- newTranscript
      (_, binds, raw) <- rawDrive (Left "a replay answers Level 0 Discovery alone (Hsed.Transport.Replay)")
      g <- guardDrive t recoverMethods Nothing raw
      driveBind g 0x1004 >>= (`shouldSatisfy` either ("a replay answers Level 0 Discovery alone" `T.isInfixOf`) (const False))
      readIORef binds `shouldReturn` [0x1004]
      drivePolicy g `shouldReturn` Level0Only
    it "prints a policy as {policy, base_comid, allowed}, each method by name" $ do
      A.toJSON Level0Only `shouldBe` A.object ["policy" A..= ("level0_only" :: Text)]
      A.toJSON (AllowList 0x1004 (readOnlyMethods <> [AllowStackReset])) `shouldBe` A.object
        [ "policy" A..= ("allow_list" :: Text), "base_comid" A..= (4100 :: Int)
        , "allowed" A..= (["properties", "start_session_admin_sp", "get_c_pin_msid", "end_of_session", "stack_reset"] :: [Text]) ]
      recoverMethods `shouldBe` [AllowStartSessionAdminSP, AllowEndOfSession, AllowStackReset]

  describe "the protocol, ComID and Kind it reads (Hsed.Wire's)" $
    it "are Core's: protocol 0x01 (Table 25), Level 0 Discovery's ComID 0x0001 (Table 26), the Data Kind 0x0000 (Table 19)" $
      (comPacketProtocol, level0ComId, dataKind) `shouldBe` (0x01, 0x0001, 0x0000)
