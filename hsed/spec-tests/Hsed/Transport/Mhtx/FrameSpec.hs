{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Mhtx.FrameSpec
Description : Test specifications for Hsed.Transport.Mhtx.Frame: the LAN leg's frames coded, decoded and sorted
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The frames of design 6.3 round trip, their payloads a real exchange's with the in-process
simulator (docs/spec-as-source.md:502-509), hello's numbers JSON integers (protocol.dart:94-98);
requests, responses, events and hellos round trip under QuickCheck, payloads standard padded
canonical base64 (checked against a reference encoder written out here), 1 MiB of payload
1,398,104 characters; what a request may not be is refused, its id given back where it is an
integer, and only for type sed.req; what is not a frame the hub or the client reads is ignored;
and the view's codec covers every query and answer, over Hsed.Sim.Gen's states and the folded
states at both Original Factory States, every Cell constructor, byte tables written, credentials
(design 11 M5c, group G1's acceptance). Generators are functions here, no orphan instances; the
QuickCheck runs are capped, hsed-test's 15 s budget being shared (design 13.34).
-}
module Hsed.Transport.Mhtx.FrameSpec (spec) where

import qualified Data.Aeson                        as A
import qualified Data.Aeson.KeyMap                 as KM
import           Data.Bits                         (shiftL, shiftR, (.&.), (.|.))
import qualified Data.IntMap.Strict                as IntMap
import qualified RIO.ByteString                    as B
import qualified RIO.List                          as L
import qualified RIO.Map                           as Map
import           RIO
import qualified RIO.Text                          as T
import           Test.Hspec
import           Test.Hspec.QuickCheck             (modifyMaxSuccess, prop)
import           Test.QuickCheck                   (Gen, arbitrary, chooseInt, chooseInteger, counterexample, elements, forAll, listOf,
                                                    oneof, sublistOf, vectorOf, (===))

import           Hsed.Core.Table                   (Cell (..), Col (..), TableDesc (..))
import           Hsed.Core.UID                     (UID, uid)
import           Hsed.Gen.Core.TableDescs          (authorityTableDesc, lockingTableDesc)
import           Hsed.Gen.Core.UIDs                (uAuthority_SID, uLocking_GlobalRange, uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP   (uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Jig.Script                   (controlComPacket, dataStoreTable, propertiesCall)
import           Hsed.Sim.Gen                      (foldedAt, genState)
import           Hsed.Sim.Types                    (ByteTable (..), SPState (..), SimParams (..), SyncState (..),
                                                    TPerState (..), defaultSimParams)
import           Hsed.Sim.View                     (ComIdView (..), Credential (..), SimQuery (..), SimSetup (..), SomeQuery (..),
                                                    answer, setupOf)
import           Hsed.Transport.InProc             (InProcOpts (..))
import           Hsed.Transport.Mhtx.Conn          (Incoming (..), messageLimit)
import           Hsed.Transport.Mhtx.Frame
import           Hsed.Transport.Opened             (openedInProc)
import           Hsed.Wire                         (Event (..), IfStatus (..), Response (..), Unit (..), ifRecv, ifSend)


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Frame" $ do
  describe "design 6.3's frames" $ do
    it "decode and re-encode to the same JSON values, the payloads a real exchange's, which begin AAAAABAE (ComID 0x1004)" $ do
      (t, _) <- openedInProc defaultSimParams (InProcOpts UnitBytes 0)
      sent   <- either (throwString . show) (pure . padded 2048) (controlComPacket 0x1004 propertiesCall)
      ifSend t 1 0x1004 2048 sent `shouldReturn` Response Good mempty
      received <- ifRecv t 1 0x1004 2048
      status received `shouldBe` Good
      let sentText     = referenceBase64 sent
          receivedText = referenceBase64 (rpayload received)
      [sentText, receivedText] `shouldSatisfy` all ("AAAAABAE" `T.isPrefixOf`)
      let fromHub = [ designHello
                    , "{\"type\":\"sed.rsp\",\"id\":2,\"status\":\"good\",\"len\":0}"
                    , "{\"type\":\"sed.rsp\",\"id\":3,\"status\":\"good\",\"len\":2048,\"payload\":\"" <> encodeUtf8 receivedText <> "\"}"
                    , "{\"type\":\"sed.rsp\",\"id\":4,\"status\":\"synchronous_protocol_violation\",\"len\":0}"
                    , "{\"type\":\"sed.evt\",\"seq\":7,\"event\":\"power_cycle\",\"comids_reset\":[4100],\"sessions_aborted\":[[4100,4096]]}"
                    ]
          fromJig = [ "{\"type\":\"sed.req\",\"id\":2,\"op\":\"if_send\",\"proto\":1,\"comid\":4100,\"len\":2048,\"payload\":\""
                      <> encodeUtf8 sentText <> "\"}"
                    , "{\"type\":\"sed.req\",\"id\":3,\"op\":\"if_recv\",\"proto\":1,\"comid\":4100,\"len\":2048}"
                    ]
      [ (json f, json <$> reencodedByHub f) | f <- fromHub ] `shouldSatisfy` all (\(v, v') -> v' == Just v)
      [ (json f, json <$> reencodedByJig f) | f <- fromJig ] `shouldSatisfy` all (\(v, v') -> v' == Just v)
      map (fromClient . InText) (drop 1 fromJig) `shouldBe` [ClientReq (SedReq 3 (OpIfRecv 1 4100 2048))]
      map (fromServer . InText) (drop 4 fromHub) `shouldBe` [ServerEvt (SedEvt 7 PowerCycle [4100] [(4100, 4096)])]

    it "keep hello's v, seq, interval_ms and timeout_ms JSON integers, type first, the app object as it came" $ do
      hello <- case fromServer (InText designHello) of
        ServerHello h -> pure h
        other         -> throwString ("design 6.3's hello does not read: " <> show other)
      helloText hello `shouldBe` "{\"type\":\"hello\",\"v\":1,\"server_id\":\"q7d2m0s8x1kz\",\"seq\":0,\"heartbeat\":{\"interval_ms\":20000,\"timeout_ms\":60000},\"app\":{\"sed\":{\"base_comid\":4100,\"errata\":\"scott\",\"impl\":\"hsed-sim 0.1.0\",\"n_comids\":1,\"role\":\"tper\",\"ssc\":\"opal-2.30\",\"units\":\"bytes\",\"v\":1}}}"
      sedAppOf hello `shouldBe` Right (SedApp "tper" "hsed-sim 0.1.0" UnitBytes (Just "opal-2.30") (Just "scott") (Just 4100) (Just 1) [] Nothing)

    it "write ping and pong as mh_transport does, each read by the side it is for" $ do
      (pingText, pongText) `shouldBe` ("{\"type\":\"ping\"}", "{\"type\":\"pong\"}")
      map (fromClient . InText) [pingText, pongText] `shouldBe` [ClientPing, ClientIgnored]
      map (fromServer . InText) [pingText, pongText] `shouldBe` [ServerPing, ServerPong]

  describe "round trips (QuickCheck)" $ modifyMaxSuccess (const 100) $ do
    prop "requests of every op, payloads of 0 to 4096 bytes" $ forAll genReq $ \r ->
      fromClient (InText (reqText r)) === ClientReq r
    prop "responses of every IfStatus with detail texts, describe, view and protocol_error, ids null or integers" $
      forAll genRsp $ \r -> fromServer (InText (rspText r)) === ServerRsp r
    prop "events" $ forAll genEvt $ \e -> fromServer (InText (evtText e)) === ServerEvt e
    prop "hellos with an app.sed, its numbers JSON integers" $ forAll genSedApp $ \app -> forAll (genHello app) $ \h ->
      counterexample (show (helloText h)) $
        (fromServer (InText (helloText h)), sedAppOf h, all (integerMember (helloText h)) ["v", "seq", "interval_ms", "timeout_ms"])
          === (ServerHello h, Right app, True)
    it "payloads as the reference encoder writes them, every length to 6 bytes and every byte value" $
      forM_ ([ B.pack (take n [0xfb ..]) | n <- [0 .. 6] ] <> [B.pack [0 .. 255]]) $ \bytes ->
        payloadOf (reqText (SedReq 1 (OpIfSend 1 4100 bytes))) `shouldBe` Just (referenceBase64 bytes)
    it "1 MiB of payload, 1,398,104 base64 characters in a frame under the 2 MiB limit" $ do
      let r    = SedReq 1 (OpIfSend 1 4100 (B.replicate 1048576 0xa5))
          text = reqText r
      T.length <$> payloadOf text `shouldBe` Just 1398104
      B.length text `shouldSatisfy` (< fromIntegral messageLimit)
      fromClient (InText text) `shouldBe` ClientReq r
      fromClient (InText (reqText (SedReq 2 (OpIfRecv 1 4100 1048576)))) `shouldBe` ClientReq (SedReq 2 (OpIfRecv 1 4100 1048576))
    prop "broadcasts, the hub's seq replacing any the object carried" $ forAll (chooseInteger (0, mostSeq)) $ \s ->
      let object = KM.fromList [("type", A.String "tick"), ("seq", A.Number 99), ("n", A.Number 1)]
      in (fromServer (InText (broadcastText s object)), json (broadcastText s object))
           === (ServerBroadcast s, Just (A.Object (KM.insert "seq" (A.toJSON s) object)))

  describe "a sed.req refused (the hub's protocol_error), its id given back where it is an integer" $ do
    it "for payloads that are not standard padded canonical base64 of exactly len bytes" $
      map (fromClient . InText . ifSendWith 1 4100 9)
        [ ("2", "\"AAE\"")              -- unpadded
        , ("2", "\"-_8=\"")             -- URL-safe characters for +/8=
        , ("6", "\"AAAA\\nAAAA\"")      -- an embedded newline
        , ("1", "\"AB==\"")             -- non-canonical: AA== is the byte 0
        , ("4", "\"AAAA\"")             -- 3 bytes, not len's 4
        , ("2", "\"AAAA\"")             -- 3 bytes, not len's 2
        , ("1048577", "\"AAAA\"")       -- len over 1 MiB
        , ("2", "17")                   -- no string
        ] `shouldSatisfy` all (refusedWith 9)
    it "for an unknown op, a field out of its range, a field missing" $ do
      fromClient (InText "{\"type\":\"sed.req\",\"id\":5,\"op\":\"format\"}") `shouldSatisfy` refusedWith 5
      map (fromClient . InText)
        [ "{\"type\":\"sed.req\",\"id\":5,\"op\":\"if_recv\",\"proto\":256,\"comid\":4100,\"len\":16}"
        , "{\"type\":\"sed.req\",\"id\":5,\"op\":\"if_recv\",\"proto\":1,\"comid\":65536,\"len\":16}"
        , "{\"type\":\"sed.req\",\"id\":5,\"op\":\"if_recv\",\"proto\":1,\"comid\":-1,\"len\":16}"
        , "{\"type\":\"sed.req\",\"id\":5,\"op\":\"if_recv\",\"proto\":1,\"comid\":4100,\"len\":1048577}"
        , "{\"type\":\"sed.req\",\"id\":5,\"op\":\"if_recv\",\"proto\":1.5,\"comid\":4100,\"len\":16}"
        , "{\"type\":\"sed.req\",\"id\":5,\"op\":\"if_recv\",\"proto\":1,\"len\":16}"
        , "{\"type\":\"sed.req\",\"id\":5,\"op\":\"reset\",\"event\":\"programmatic\"}"
        , "{\"type\":\"sed.req\",\"id\":5,\"op\":\"peek\",\"query\":{\"name\":\"everything\"}}"
        , "{\"type\":\"sed.req\",\"id\":5,\"op\":\"peek\",\"query\":{\"name\":\"life_cycle\",\"sp\":\"0000020500000002ff\"}}"
        , "{\"type\":\"sed.req\",\"id\":5,\"op\":\"release\",\"comid\":\"4100\"}"
        ] `shouldSatisfy` all (refusedWith 5)
    it "for an id that is 0, negative, fractional, above 2^53-1 or missing" $
      map (fromClient . InText . (\i -> "{\"type\":\"sed.req\"" <> i <> ",\"op\":\"describe\"}"))
        [",\"id\":0", ",\"id\":-1", ",\"id\":2.5", ",\"id\":9007199254740992", ",\"id\":\"7\"", ""]
        `shouldSatisfy` \sorted -> map badId sorted == [Just (Just 0), Just (Just (-1)), Just Nothing, Just (Just 9007199254740992), Just Nothing, Just Nothing]
    it "takes an id up to 2^53-1, and 2.0 and 1e3 as the integers they are" $
      map (fromClient . InText . (\i -> "{\"type\":\"sed.req\",\"id\":" <> i <> ",\"op\":\"describe\"}")) ["9007199254740991", "2.0", "1e3"]
        `shouldBe` map (\i -> ClientReq (SedReq i OpDescribe)) [mostId, 2, 1000]
    it "for type sed.req alone: a bad sed.rsp, sed.evt or other type from a client is ignored" $
      map (fromClient . InText)
        [ "{\"type\":\"sed.rsp\",\"id\":0,\"op\":\"describe\"}"
        , "{\"type\":\"sed.evt\",\"id\":0}"
        , "{\"type\":\"other\",\"id\":0,\"op\":\"format\"}"
        ] `shouldBe` replicate 3 ClientIgnored

  describe "what is ignored, with nothing produced" $ do
    it "by the hub: non-JSON, an array or scalar, no type, a non-string type, an unknown type, binary, invalid UTF-8, a client's hello and pong" $
      map fromClient
        [ InText "this is not json", InText "[1,2,3]", InText "17", InText "\"ping\"", InText "{}", InText "{\"type\":5}"
        , InText "{\"type\":\"conformance_bogus\"}", InBinary pingText, InBinary (reqText (SedReq 1 OpDescribe))
        , InText "{\"type\":\"ping\",\"x\":\"\255\"}", InText designHello, InText pongText, InClosed Nothing
        ] `shouldBe` replicate 13 ClientIgnored
    it "by the client: a sed.rsp carrying seq; a frame with no integer seq that it does not know; a hello that does not read" $
      map (fromServer . InText)
        [ "{\"type\":\"sed.rsp\",\"id\":2,\"status\":\"good\",\"len\":0,\"seq\":3}"
        , "{\"type\":\"tick\",\"seq\":1.5}", "{\"type\":\"tick\"}", "{\"type\":\"hello\",\"v\":1.5}", "[1]", "{}"
        ] `shouldBe` replicate 6 ServerIgnored
    it "but takes any other frame with an integer seq as a broadcast, a sed.evt that does not read included" $
      map (fromServer . InText) ["{\"type\":\"tick\",\"seq\":3}", "{\"type\":\"sed.evt\",\"seq\":4,\"event\":\"reboot\"}"]
        `shouldBe` [ServerBroadcast 3, ServerBroadcast 4]

  describe "app.sed" $
    it "refuses a version other than 1, an unknown or repeated allow, and unknown units" $ do
      let app members = Hello 1 "q7d2m0s8x1kz" 0 200 600 (KM.singleton "sed" (A.object members))
          base        = [("v", A.Number 1), ("role", A.String "tper"), ("impl", A.String "hsed-sim"), ("units", A.String "bytes")]
      map (sedAppOf . app)
        [ ("v", A.Number 2) : drop 1 base
        , base <> [("allows", A.toJSON ["peek", "format" :: Text])]
        , base <> [("allows", A.toJSON ["peek", "peek" :: Text])]
        , take 3 base <> [("units", A.String "words")]
        ] `shouldSatisfy` all isLeft
      sedAppOf (app (base <> [("allows", A.toJSON ["reset", "peek" :: Text]), ("setup", A.Null)]))
        `shouldBe` Right (SedApp "tper" "hsed-sim" UnitBytes Nothing Nothing Nothing Nothing [AllowReset, AllowPeek] Nothing)

  describe "the view's codec" $ do
    it "round trips every query and answer over the folded states at both OFS: every cell, credential and byte table" $
      forM_ [minBound .. maxBound] $ \ofs -> do
        let st       = foldedAt ofs
            answered = [ c | SomeQuery q@CellOf {} <- everyQueryOf st, Just c <- [answer q st] ]
        mapMaybe (answerProblem st) (everyQueryOf st) `shouldBe` []
        -- over 300 cells answered, of seven kinds: no table of either SP holds an integer cell, which the
        -- property below covers
        (length answered > 300, L.sort (L.nub (map cellKind answered))) `shouldBe` (True, ["bytes", "enum", "list", "ref", "text", "uid", "uint"])
    modifyMaxSuccess (const 20) $ prop "round trips the queries the jig reads over Hsed.Sim.Gen's states" $ forAll genState $ \st ->
      mapMaybe (answerProblem st) jigQueries === []
    modifyMaxSuccess (const 100) $ do
      prop "round trips every Cell constructor, byte tables written, credentials, ComIDs held in S2 and S3" $
        forAll ((,,,) <$> genCell 2 <*> genByteTable <*> genCredential <*> upTo 4 genComIdView) $ \(c, bt, cr, cvs) ->
          ( parseAnswer anyCell (answerValue anyCell (Just c)), parseAnswer anyByteTable (answerValue anyByteTable (Just bt))
          , parseAnswer anyCredential (answerValue anyCredential (Just cr)), parseAnswer ComIds (answerValue ComIds cvs) )
            === (Right (Just c), Right (Just bt), Right (Just cr), Right cvs)
      prop "round trips every query, its UIDs 16 lower-case hex digits" $ forAll genQuery $ \(SomeQuery q) ->
        parseQuery (queryValue q) === Right (SomeQuery q)
    it "refuses an answer of another shape, and a query it does not know" $ do
      parseAnswer Msid (A.String "AB==") `shouldSatisfy` isLeft
      parseAnswer LockingRanges (A.toJSON ["000008020000000"  :: Text]) `shouldSatisfy` isLeft
      parseAnswer LockingRanges (A.toJSON ["000008020000000G" :: Text]) `shouldSatisfy` isLeft
      parseAnswer Sessions (A.toJSON [[4100, 4096, 1 :: Int]]) `shouldSatisfy` isLeft
      parseAnswer anyByteTable (A.object [("size", A.Number 4), ("fill", A.Number 0), ("written", A.toJSON [(2 :: Int, "AA==" :: Text), (1, "AA==")])])
        `shouldSatisfy` isLeft
      parseAnswer anyCell (A.object [("cell", A.String "float"), ("value", A.Number 1.5)]) `shouldSatisfy` isLeft
      parseQuery (A.object [("name", A.String "everything")]) `shouldSatisfy` isLeft
  where
    reencodedByHub f = case fromServer (InText f) of
      ServerHello h -> Just (helloText h)
      ServerRsp r   -> Just (rspText r)
      ServerEvt e   -> Just (evtText e)
      _             -> Nothing
    reencodedByJig f = case fromClient (InText f) of
      ClientReq r -> Just (reqText r)
      _           -> Nothing
    anyCell       = CellOf uSP_Admin uSP_Admin uSP_Admin "Name"
    anyByteTable  = ByteTableOf uSP_Locking dataStoreTable
    anyCredential = CredentialOf uSP_Admin uAuthority_SID


-- * Frames

-- | Design 6.3's hello (docs/spec-as-source.md:502-503).
designHello :: ByteString
designHello = "{\"type\":\"hello\",\"v\":1,\"server_id\":\"q7d2m0s8x1kz\",\"seq\":0,\"heartbeat\":{\"interval_ms\":20000,\"timeout_ms\":60000},\"app\":{\"sed\":{\"v\":1,\"role\":\"tper\",\"impl\":\"hsed-sim 0.1.0\",\"units\":\"bytes\",\"ssc\":\"opal-2.30\",\"errata\":\"scott\",\"base_comid\":4100,\"n_comids\":1}}}"

-- | An if_send request with this len and payload member (raw JSON), its id given.
ifSendWith :: Word8 -> Word16 -> Integer -> (ByteString, ByteString) -> ByteString
ifSendWith p c i (len, payload) =
  "{\"type\":\"sed.req\",\"id\":" <> fromString (show i) <> ",\"op\":\"if_send\",\"proto\":" <> fromString (show p) <> ",\"comid\":"
  <> fromString (show c) <> ",\"len\":" <> len <> ",\"payload\":" <> payload <> "}"

-- | A request refused with this id given back.
refusedWith :: Integer -> FromClient -> Bool
refusedWith i = \case
  ClientBadReq (Just i') _ -> i' == i
  _                        -> False

-- | The id a refused request gives back, if it was refused.
badId :: FromClient -> Maybe (Maybe Integer)
badId = \case
  ClientBadReq i _ -> Just i
  _                -> Nothing

-- | A frame's JSON value.
json :: ByteString -> Maybe A.Value
json = A.decodeStrict'

-- | A request frame's payload member.
payloadOf :: ByteString -> Maybe Text
payloadOf bytes = case json bytes of
  Just (A.Object o) | Just (A.String t) <- KM.lookup "payload" o -> Just t
  _                                                               -> Nothing

-- | Whether a frame writes a member as a JSON integer: its name, a colon, an optional minus, digits, then a comma or a brace.
integerMember :: ByteString -> Text -> Bool
integerMember frame name = case B.breakSubstring key frame of
  (_, rest) | not (B.null rest) -> let (digits, next) = B.span isDigit (B.drop (B.length key) rest)
                                   in not (B.null (B.dropWhile (== 45) digits)) && B.take 1 next `elem` [",", "}"]
  _                             -> False
  where
    key     = "\"" <> encodeUtf8 name <> "\":"
    isDigit w = w == 45 || (w >= 48 && w <= 57)

-- | Bytes zero-padded to a length.
padded :: Int -> ByteString -> ByteString
padded n bytes = bytes <> B.replicate (n - B.length bytes) 0

-- | Standard padded base64 (RFC 4648 4), written out here so that the codec's (memory's) is held
--   to another.
referenceBase64 :: ByteString -> Text
referenceBase64 = T.pack . go . B.unpack
  where
    go (a : b : c : rest) = quad a b c <> go rest
    go [a, b]             = take 3 (quad a b 0) <> "="
    go [a]                = take 2 (quad a 0 0) <> "=="
    go []                 = []
    quad :: Word8 -> Word8 -> Word8 -> String
    quad a b c = let n = (fromIntegral a `shiftL` 16) .|. (fromIntegral b `shiftL` 8) .|. fromIntegral c :: Word32
                 in [ alphabet (fromIntegral ((n `shiftR` s) .&. 63)) | s <- [18, 12, 6, 0] ]
    alphabet i = fromMaybe '?' (L.lookup i (zip [0 :: Int ..] (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "+/")))

-- | The most a seq is read as.
mostSeq :: Integer
mostSeq = toInteger (maxBound :: Int64)


-- * The view's codec

-- | Why a query's answer over a state does not round trip through JSON text, if it does not: the
--   answer's value, the query's.
answerProblem :: TPerState -> SomeQuery -> Maybe String
answerProblem st (SomeQuery q) = case q of
  ComIds          -> through q
  Sessions        -> through q
  LifeCycleOf _   -> through q
  CredentialOf {} -> through q
  CellOf {}       -> through q
  ByteTableOf {}  -> through q
  Msid            -> through q
  LockingRanges   -> through q
  where
    through :: (Eq a, Show a) => SimQuery a -> Maybe String
    through q' =
      let a = answer q' st
          v = answerValue q' a
      in if A.decode (A.encode v) == Just v && parseAnswer q' v == Right a && parseQuery (queryValue q') == Right (SomeQuery q')
           then Nothing
           else Just (show q' <> ": " <> show a)

-- | A cell's constructor, as its JSON names it.
cellKind :: Cell -> Text
cellKind = \case
  UIDCell {}   -> "uid"
  TextCell {}  -> "text"
  EnumCell {}  -> "enum"
  RefCell {}   -> "ref"
  UIntCell {}  -> "uint"
  IntCell {}   -> "int"
  BytesCell {} -> "bytes"
  ListCell {}  -> "list"

-- | Every query of a state: ComIds, Sessions, Msid and LockingRanges; each SP's life cycle and every
--   authority's credential; every byte table; every cell of the first two rows of every table; and
--   each of an SP, authority and table it lacks.
everyQueryOf :: TPerState -> [SomeQuery]
everyQueryOf st =
  [SomeQuery ComIds, SomeQuery Sessions, SomeQuery Msid, SomeQuery LockingRanges]
  <> [ SomeQuery (LifeCycleOf sp) | sp <- sps <> [nowhere] ]
  <> [ SomeQuery (CredentialOf sp a) | (sp, s) <- spsOf, a <- rowUIDs s (tdTableUID authorityTableDesc) <> [nowhere] ]
  <> [ SomeQuery (ByteTableOf sp t) | (sp, s) <- spsOf, t <- Map.keys (spsBytes s) <> [nowhere] ]
  <> [ SomeQuery (CellOf sp t r (colName c)) | (sp, s) <- spsOf, (t, desc) <- Map.toList (spsDescs s)
                                             , r <- take 2 (rowUIDs s t), c <- tdColumns desc ]
  where
    spsOf   = Map.toList (tsSPs st)
    sps     = map fst spsOf
    nowhere = uid 0xde 0xad 0xbe 0xef 0x00 0x00 0x00 0x01
    rowUIDs s t = [ u | row <- Map.findWithDefault [] t (spsTables s), Just (UIDCell u) <- [L.lookup 0 row] ]

-- | The queries the jig's cases read (design 9.2 as M5c builds it): the ComIDs, the sessions, the
--   MSID, the Locking ranges, both SPs' life cycles, the SID's and Admin1's credentials, User1's
--   Enabled and the global range's LockOnReset, the DataStore.
jigQueries :: [SomeQuery]
jigQueries =
  [ SomeQuery ComIds, SomeQuery Sessions, SomeQuery Msid, SomeQuery LockingRanges
  , SomeQuery (LifeCycleOf uSP_Admin), SomeQuery (LifeCycleOf uSP_Locking)
  , SomeQuery (CredentialOf uSP_Admin uAuthority_SID), SomeQuery (CredentialOf uSP_Locking LockingSP.uAuthority_Admin1)
  , SomeQuery (CellOf uSP_Locking (tdTableUID authorityTableDesc) LockingSP.uAuthority_User1 "Enabled")
  , SomeQuery (CellOf uSP_Locking (tdTableUID lockingTableDesc) uLocking_GlobalRange "LockOnReset")
  , SomeQuery (ByteTableOf uSP_Locking dataStoreTable)
  ]


-- * Generators (functions, no orphan instances)

genReq :: Gen SedReq
genReq = SedReq <$> chooseInteger (1, mostId) <*> genOp

genOp :: Gen Op
genOp = oneof
  [ OpIfSend <$> arbitrary <*> arbitrary <*> genBytes 4096
  , OpIfRecv <$> arbitrary <*> arbitrary <*> (fromIntegral <$> chooseInt (0, 1048576))
  , OpReset <$> elements [minBound .. maxBound]
  , pure OpDescribe
  , OpPeek <$> genQuery
  , OpRelease <$> arbitrary
  ]

genRsp :: Gen SedRsp
genRsp = SedRsp <$> genMaybe (chooseInteger (toInteger (minBound :: Int64), mostSeq)) <*> oneof
  [ Answered <$> genStatus <*> genBytes 4096
  , Described <$> genSedApp
  , Viewed . answerValue (CellOf uSP_Admin uSP_Admin uSP_Admin "Name") . Just <$> genCell 2
  , ProtocolError <$> genText
  ]

genStatus :: Gen IfStatus
genStatus = oneof
  [ elements [Good, InvalidSecurityProtocolId, InvalidTransferLengthOnIfSend, OtherInvalidCommandParameter, SynchronousProtocolViolation, DataProtectionError]
  , TransportFailure <$> genText
  , Unsupported <$> genText
  ]

genEvt :: Gen SedEvt
genEvt = SedEvt <$> chooseInteger (0, mostSeq) <*> elements [minBound .. maxBound] <*> listOf arbitrary <*> listOf ((,) <$> arbitrary <*> arbitrary)

genSedApp :: Gen SedApp
genSedApp = SedApp <$> genText <*> genText <*> elements [minBound .. maxBound] <*> genMaybe genText <*> genMaybe genText
                   <*> genMaybe arbitrary <*> genMaybe arbitrary <*> sublistOf [minBound .. maxBound] <*> genMaybe genSetup

genSetup :: Gen SimSetup
genSetup = oneof
  [ setupOf <$> ((\u ofs -> defaultSimParams { spUnits = u, spLockingOFS = ofs }) <$> elements [minBound .. maxBound]
                                                                                    <*> elements [minBound .. maxBound])
            <*> chooseInt (0, 4)
  , SimSetup <$> elements [minBound .. maxBound] <*> chooseInt (0, 8) <*> elements [minBound .. maxBound] <*> arbitrary
             <*> genMaybe genText <*> genMaybe genText <*> genMaybe genText <*> genText
  ]

genHello :: SedApp -> Gen Hello
genHello app = Hello <$> arbitrary <*> genText <*> chooseInteger (0, mostSeq) <*> arbitrary <*> arbitrary
                     <*> pure (KM.fromList [("sed", sedAppValue app), ("other", A.toJSON [True, False])])

genQuery :: Gen SomeQuery
genQuery = oneof
  [ pure (SomeQuery ComIds), pure (SomeQuery Sessions), pure (SomeQuery Msid), pure (SomeQuery LockingRanges)
  , SomeQuery . LifeCycleOf <$> genUID
  , (\sp a -> SomeQuery (CredentialOf sp a)) <$> genUID <*> genUID
  , (\sp t r n -> SomeQuery (CellOf sp t r n)) <$> genUID <*> genUID <*> genUID <*> genText
  , (\sp t -> SomeQuery (ByteTableOf sp t)) <$> genUID <*> genUID
  ]

-- | A cell of any of the eight kinds, lists nested to a depth.
genCell :: Int -> Gen Cell
genCell depth = oneof $
  [ UIDCell <$> genUID, TextCell <$> genText, EnumCell <$> genText <*> arbitrary, RefCell <$> genText <*> genUID
  , UIntCell <$> genInteger64, IntCell <$> genInteger64, BytesCell <$> genBytes 64 ]
  <> [ ListCell <$> (chooseInt (0, 3) >>= (`vectorOf` genCell (depth - 1))) | depth > 0 ]

genByteTable :: Gen ByteTable
genByteTable = ByteTable <$> (fromInteger <$> chooseInteger (0, mostWord64)) <*> arbitrary
                         <*> (IntMap.fromList <$> upTo 8 ((,) <$> chooseInt (0, 1048575) <*> genBytes 64))

genCredential :: Gen Credential
genCredential = Credential <$> genBytes 32 <*> (fromInteger <$> chooseInteger (0, mostWord64)) <*> (fromInteger <$> chooseInteger (0, mostWord64))

genComIdView :: Gen ComIdView
genComIdView = ComIdView <$> arbitrary <*> genMaybe (elements [minBound .. maxBound])
                         <*> oneof [pure S1, S2 <$> upTo 3 (genBytes 64), S3 <$> upTo 3 (genBytes 64)]

genUID :: Gen UID
genUID = uid <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

genText :: Gen Text
genText = T.pack <$> listOf arbitrary

-- | None to so many.
upTo :: Int -> Gen a -> Gen [a]
upTo most g = chooseInt (0, most) >>= (`vectorOf` g)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [pure Nothing, Just <$> g]

-- | An integer a cell holds: a signed 64-bit integer's or a 64-bit word's.
genInteger64 :: Gen Integer
genInteger64 = oneof [chooseInteger (toInteger (minBound :: Int64), mostSeq), chooseInteger (0, mostWord64), elements [0, 1, -1]]

mostWord64 :: Integer
mostWord64 = toInteger (maxBound :: Word64)

-- | Up to so many bytes, their values from a seed (quicker than a byte drawn at a time).
genBytes :: Int -> Gen ByteString
genBytes most = do
  n    <- chooseInt (0, most)
  seed <- arbitrary
  pure (fst (B.unfoldrN n (\s -> Just (fromIntegral (s `shiftR` 24), s * 1103515245 + 12345)) (seed :: Word32)))
