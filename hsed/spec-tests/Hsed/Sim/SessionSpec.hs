{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Sim.SessionSpec
Description : Test specifications for Hsed.Sim.Session: a regular session's items dispatched, End of Session, aborts and the TPer's CloseSession, an overflow ending the payload, the framing, and the simulator's answers to the error cases
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The regular-session half of M4a (design 8.2) on sessions opened directly
("Hsed.Sim.State".@openSession@): one data subpacket's items processed, a
Get answered in a packet of the session (TSN then HSN, Core 3.3.7.1.2, PDF
73), [Get, EOS] the results then the TPer's End of Session and the session
closed (3.2.2.3.3.3, PDF 37; 3.3.7.1.5, PDF 75), a bare EOS echoed; an
invocation whose header does not read NOT_AUTHORIZED and one whose
parameters do not INVALID_PARAMETER, the session kept (3.2.2.4.2 items 3-4,
PDF 39); a fault or a second invocation past the TPer's MaxMethods aborting
the session with the results completed before it and the TPer's
CloseSession where the run prepares one (3.3.10.7 b, PDF 86; 5.2.2.4.1.7,
PDF 146); since M5b the transaction control tokens (design 11 M5b): Start
Transaction mirrored with status 0 whatever the host's, a level open after
it, which End Transaction with 0 commits and with any other status rolls
back, each mirrored (3.2.2.3.3.4-5, PDF 37-38), a second Start Transaction
past MaxTransactionLimit, 1, aborting the session with no mirror sent
(ETC-09's shape), an End Transaction with none open or either token on a
TPer reporting no MaxTransactionLimit aborting it (3.2.2.3.4 b and
3.2.2.4.1 item 4, PDF 38), the session taken as the TPer holds it before
each item, not as the stale value given (design 8.3); a response the
host's packet cannot hold RESPONSE_OVERFLOW, which ends the payload's
processing (Opal 2.30 3.3.1, PDF 29); the framing within the host's
MaxSubpackets, MaxPacketSize and
MaxComPacketSize (5.2.2.4.1.1-2 and 4, PDF 145-146), a packet never larger
than the ComPacket that carries it, over any host properties. Then the whole
path through the simulator's step: ETC-05's EndList before Call, answered
by CloseSession at the next IF-RECV or by (0, 0, 0) without one; ETC-06's
Call EndList, NOT_AUTHORIZED with the session kept; ETC-14's shape, a
packet of two subpackets, aborting (Test Cases PDF 79-80, 87).
-}
module Hsed.Sim.SessionSpec (spec) where

import qualified RIO.ByteString          as B
import qualified RIO.Map                 as Map
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck         (arbitrary, chooseInt, counterexample, forAll, vectorOf)

import           Hsed.Core.Call          (MethodCall (..), MethodHeader (..), MethodResponse (..), Parameters (..), statusList)
import           Hsed.Core.CellBlock     (cellBlockValue, columnRange, noCells)
import           Hsed.Core.Table         (Cell (..), columnOf)
import           Hsed.Core.Method        (fromCall, invocation)
import           Hsed.Core.Stream        (Atom (..), Fault (..), Item (..), Place (..), Returned (..), StreamMode (..), atoms,
                                          hostPolicy, items, returnedItems)
import           Hsed.Core.StreamItem    (StreamItem (..))
import           Hsed.Core.Token         (Token (..))
import           Hsed.Core.Value         (IsValue (..), List (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes     (StatusCode (..), pattern INVALID_PARAMETER, pattern NOT_AUTHORIZED,
                                          pattern RESPONSE_OVERFLOW)
import           Hsed.Gen.Core.Layouts   (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..))
import qualified Hsed.Gen.Core.Methods   as Methods
import           Hsed.Gen.Core.TableUIDs (uC_PINTable)
import           Hsed.Gen.Core.UIDs      (uAuthority_Anybody, uC_PIN_SID, uCloseSession, uSMUID, uSP_Admin)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uC_PIN_MSID)
import           Hsed.Packet             (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, decodePacket,
                                          encodeComPacket)
import           Hsed.Sim                (step)
import           Hsed.Sim.Gen            (genHostProperties)
import           Hsed.Sim.Session
import           Hsed.Sim.SessionManager (closeSessionCall)
import           Hsed.Sim.State          (lifeOf, liveSession, openSession, putSPView, resolveSession, spView, txnDepth,
                                          updateRowIn)
import           Hsed.Sim.Types
import           Hsed.Wire               (Dir (..), IfStatus (..), Request (..), Response (..), Unit (..), comPacketHeaderSize,
                                          comPacketProtocol, roundUp)


base :: Word16
base = spBaseComId defaultSimParams

-- | The default TPer, and the TPer that prepares no CloseSession on an
--   abort, each with an Admin SP session as Anybody open on its base ComID,
--   HSN 0x51, TSN 4096.
ses, quietSes :: Session
st1, quietSt1 :: TPerState
(ses, st1)           = openSession base 0x51 uSP_Admin True [uAuthority_Anybody] (initialState defaultSimParams)
(quietSes, quietSt1) = openSession base 0x51 uSP_Admin True [uAuthority_Anybody] (initialState defaultSimParams { spCloseSessionOnAbort = False })

-- | The same TPer with one of its properties set.
withTPer :: Text -> Integer -> TPerState -> TPerState
withTPer name v st = st { tsParams = (tsParams st) { spTPerProperties = map replaced (spTPerProperties (tsParams st)) } }
  where replaced (n, v') = (n, if n == name then v else v')

-- | The session with one of the host's properties it keeps set.
hostOf :: Text -> Natural -> Session -> Session
hostOf name v s = s { sesHostProps = Map.insert name v (sesHostProps s) }

-- | UCT-03 step 1b's Get: C_PIN_MSID's PIN column, and its result.
getCall :: MethodCall
getCall = invocation uC_PIN_MSID (Methods.Get (cellBlockValue (columnRange 3 3)))

getResult :: MethodResponse
getResult = MethodResponse [L (List [N ((3 :: Natural) .: spMsid defaultSimParams)])] 0

-- | A packet decoded: its header and its data subpackets' payloads.
decoded :: ByteString -> Maybe (PacketHeader, [ByteString])
decoded p = either (const Nothing) (\(DecodedFrame (PacketFrame h sps) _ _) -> Just (h, map dataSubPacketPayload sps)) (decodePacket p)

-- | The header of the session's packets.
sessionHdr :: PacketHeader
sessionHdr = PacketHeader 4096 0x51 0 0 0

-- | A status as the stream carries it.
statusOf :: StatusCode -> Natural
statusOf = statusValue

-- | The items of a token stream, as a regular session reads them.
itemsOf :: ByteString -> ([Item], Maybe Fault)
itemsOf bytes = either (\(_, why) -> ([], Just (Fault 0 OutsideCall why))) (items SessionStream . map atomToken) (atoms hostPolicy bytes)

-- | Whether the session is still open in a TPer.
open :: Session -> TPerState -> Bool
open s st = isJust (resolveSession (sesComId s) (sesTSN s) (sesHSN s) st)

-- | The TPer's reading of a packet's one data subpacket.
returned :: ByteString -> Maybe [Returned]
returned p = decoded p >>= \(_, payloads) -> case payloads of
  [bytes] -> either (const Nothing) (whole . returnedItems . map atomToken) (atoms hostPolicy bytes)
  _       -> Nothing
  where
    whole = \case
      (rs, Nothing) -> Just rs
      _             -> Nothing

-- | The TPer's mirror of a transaction control token with a status, as
--   Core 3.2.2.3.3.4-5 encodes it (PDF 37): the token, then a uinteger.
mirror :: Token -> Natural -> ByteString
mirror t s = generate t <> generate (Unsigned s)

-- | The session's transaction depth as the TPer holds it.
depth :: Session -> TPerState -> Int
depth s = maybe 0 txnDepth . liveSession s

-- | The Admin SP with C_PIN_SID's Tries put, a change a method within the
--   session might make, found by its column's name in the SP's description
--   of its C_PIN table.
withSidTries :: Integer -> SPState -> SPState
withSidTries n sps = updateRowIn uC_PINTable uC_PIN_SID (map put) sps
  where
    tries = Map.lookup uC_PINTable (spsDescs sps) >>= (`columnOf` "Tries")
    put (c, cell) = (c, if Just c == tries then UIntCell n else cell)

spec :: Spec
spec = do
  describe "one data subpacket's items (sessionPayload)" $ do
    it "answers a Get in a packet of the session, TSN then HSN, one data subpacket of the result, the session kept" $ do
      let Processed outcome st' _ = sessionPayload ses [Invoke getCall] Nothing st1
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [generate getResult])
        other        -> expectationFailure (show other)
      st' `shouldBe` st1
    it "answers [Get, EOS] with the result then the TPer's End of Session, in one subpacket, and closes the session: the ComID Issued again" $ do
      let Processed outcome st' _ = sessionPayload ses [Invoke getCall, EndOfSessionItem] Nothing st1
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [generate getResult <> generate EndOfSession])
        other        -> expectationFailure (show other)
      open ses st' `shouldBe` False
      lifeOf base st' `shouldBe` Just Issued
      st' `shouldBe` (initialState defaultSimParams) { tsNextTSN = tsNextTSN st1 }
    it "echoes a bare End of Session and closes the session (Core 3.2.2.3.3.3; Test Cases' CLOSE_SESSION)" $ do
      let Processed outcome st' _ = sessionPayload ses [EndOfSessionItem] Nothing st1
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [generate EndOfSession])
        other        -> expectationFailure (show other)
      open ses st' `shouldBe` False
    it "prepares nothing for no items and no fault, the TPer unchanged (no action)" $
      sessionPayload ses [] Nothing st1 `shouldBe` Processed (Prepared []) st1 False
    it "answers an invocation whose header does not read NOT_AUTHORIZED, one whose parameters do not INVALID_PARAMETER, the session kept (3.2.2.4.2 items 3-4)" $ do
      let hdr = _header getCall
      forM_ [ (InHeader, NOT_AUTHORIZED), (InParameters hdr, INVALID_PARAMETER) ] $ \(place, code) -> do
        let Processed outcome st' _ = sessionPayload ses [BadCall (Fault 1 place "out of place")] Nothing st1
        case outcome of
          Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [generate (MethodResponse [] (statusOf code))])
          other        -> expectationFailure (show other)
        open ses st' `shouldBe` True
    it "aborts the session at a fault, with no results, preparing the TPer's CloseSession [HSN, TSN] on the control session (3.3.10.7 b; 5.2.3.5)" $ do
      let Processed outcome st' _ = sessionPayload ses [] (Just (Fault 0 OutsideCall "EndList outside a method invocation")) st1
      case outcome of
        SessionAborted [] (Just closing) -> do
          decoded closing `shouldBe` Just (controlHeader, [generate (closeSessionCall ses)])
          returned closing `shouldBe` Just [ReturnedCall (closeSessionCall ses)]
          _header (closeSessionCall ses) `shouldBe` MethodHeader uSMUID uCloseSession
          fmap (fmap (\c -> (Methods.closeSessionRemoteSessionNumber c, Methods.closeSessionLocalSessionNumber c))) (fromCall @Methods.CloseSession (closeSessionCall ses))
            `shouldBe` Just (Right (value (0x51 :: Natural), value (4096 :: Natural)))
        other -> expectationFailure (show other)
      open ses st' `shouldBe` False
      lifeOf base st' `shouldBe` Just Issued
    it "aborts with no CloseSession where the run prepares none (spCloseSessionOnAbort False), the session gone all the same" $ do
      let Processed outcome st' _ = sessionPayload quietSes [] (Just (Fault 0 OutsideCall "EndList outside a method invocation")) quietSt1
      outcome `shouldBe` SessionAborted [] Nothing
      open quietSes st' `shouldBe` False
    it "mirrors a Start Transaction with status 0, in one packet, a level open after it, the committed SP as it was (Core 3.2.2.3.3.4; M5b)" $ do
      let Processed outcome st' ends = sessionPayload ses [StartTransactionItem 0] Nothing st1
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [mirror StartTransaction 0])
        other        -> expectationFailure (show other)
      (depth ses st', tsSPs st' == tsSPs st1, open ses st', ends) `shouldBe` (1, True, True, False)
    it "mirrors a Start Transaction of status 7 with 0, the host's status ignored (3.2.2.3.3.4)" $ do
      let Processed outcome st' _ = sessionPayload ses [StartTransactionItem 7] Nothing st1
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [mirror StartTransaction 0])
        other        -> expectationFailure (show other)
      depth ses st' `shouldBe` 1
    it "takes the session as the TPer holds it before each item, not the value given (design 8.3): Start Transaction then a Get answers the mirror then the result, in two packets for a host taking one subpacket a packet, the level kept" $ do
      let Processed outcome st' _ = sessionPayload ses [StartTransactionItem 0, Invoke getCall] Nothing st1
      case outcome of
        Prepared [p, q] -> do
          decoded p `shouldBe` Just (sessionHdr, [mirror StartTransaction 0])
          decoded q `shouldBe` Just (sessionHdr, [generate getResult])
        other -> expectationFailure (show other)
      depth ses st' `shouldBe` 1
    it "aborts at a second Start Transaction in the same subpacket, past MaxTransactionLimit 1, with no result and no mirror sent, the TPer's CloseSession prepared, the session gone with its level: ETC-09's shape (3.2.2.3.3.4; Test Cases PDF 81-82)" $ do
      let Processed outcome st' ends = sessionPayload ses [StartTransactionItem 0, StartTransactionItem 0, Invoke getCall] Nothing st1
      outcome `shouldSatisfy` \o -> case o of
        SessionAborted [] (Just _) -> True
        _                          -> False
      (open ses st', liveSession ses st', ends) `shouldBe` (False, Nothing, True)
      let Processed quiet _ _ = sessionPayload quietSes [StartTransactionItem 0, StartTransactionItem 0] Nothing quietSt1
      quiet `shouldBe` SessionAborted [] Nothing
    it "aborts at a Start Transaction in a session already holding a level (3.2.2.3.3.4: the nesting limit exceeded)" $ do
      let opened = prState (sessionPayload ses [StartTransactionItem 0] Nothing st1)
          Processed outcome st' _ = sessionPayload ses [StartTransactionItem 0] Nothing opened
      outcome `shouldSatisfy` \o -> case o of
        SessionAborted [] (Just _) -> True
        _                          -> False
      open ses st' `shouldBe` False
    it "aborts at an End Transaction with no level open (3.2.2.3.4 b: a control token out of order)" $
      forM_ [EndTransactionItem 0, EndTransactionItem 1] $ \t -> do
        let Processed outcome st' _ = sessionPayload ses [t] Nothing st1
        outcome `shouldSatisfy` \o -> case o of
          SessionAborted [] (Just _) -> True
          _                          -> False
        open ses st' `shouldBe` False
    it "aborts at either token on a TPer that reports no MaxTransactionLimit, whose transaction control tokens are invalid (3.2.2.4.1 item 4)" $ do
      let noTxn = st1 { tsParams = (tsParams st1) { spTPerProperties = filter ((/= maxTransactionLimitName) . fst) (spTPerProperties (tsParams st1)) } }
      txnLimit (tsParams noTxn) `shouldBe` Nothing
      forM_ [StartTransactionItem 0, EndTransactionItem 0] $ \t -> do
        let Processed outcome st' _ = sessionPayload ses [t] Nothing noTxn
        outcome `shouldSatisfy` \o -> case o of
          SessionAborted [] (Just _) -> True
          _                          -> False
        open ses st' `shouldBe` False
    it "commits a level at End Transaction 0, mirrored 0: Start Transaction, a change put in the session's view, End Transaction 0 leaves the change in the committed SP, no level open (3.2.2.3.3.5; 3.3.7.3 b)" $ do
      let opened = prState (sessionPayload ses [StartTransactionItem 0] Nothing st1)
          put    = putSPView ses (withSidTries 3 (fromMaybe (error "no view") (spView ses opened))) opened
          Processed outcome st' _ = sessionPayload ses [EndTransactionItem 0] Nothing put
      tsSPs put `shouldBe` tsSPs st1
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [mirror EndTransaction 0])
        other        -> expectationFailure (show other)
      (depth ses st', open ses st') `shouldBe` (0, True)
      Map.lookup uSP_Admin (tsSPs st') `shouldBe` fmap (withSidTries 3) (Map.lookup uSP_Admin (tsSPs st1))
    it "rolls a level back at End Transaction 1, or at a status the TPer does not support, 7, each mirrored 1: the change put is gone, the committed SP as it was, no level open (3.2.2.3.3.5)" $
      forM_ [1, 7] $ \s -> do
        let opened = prState (sessionPayload ses [StartTransactionItem 0] Nothing st1)
            put    = putSPView ses (withSidTries 3 (fromMaybe (error "no view") (spView ses opened))) opened
            Processed outcome st' _ = sessionPayload ses [EndTransactionItem s] Nothing put
        case outcome of
          Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [mirror EndTransaction 1])
          other        -> expectationFailure (show other)
        (depth ses st', open ses st', tsSPs st' == tsSPs st1, spView ses st' == Map.lookup uSP_Admin (tsSPs st1)) `shouldBe` (0, True, True, True)
    it "mirrors Start Transaction then End Transaction of one subpacket in order, each in a packet of its own, the level opened and closed by the live session (design 8.3)" $
      forM_ [(0, 0), (1, 1), (7, 1)] $ \(s, mirrored) -> do
        let Processed outcome st' _ = sessionPayload ses [StartTransactionItem 0, EndTransactionItem s] Nothing st1
        case outcome of
          Prepared [p, q] -> do
            decoded p `shouldBe` Just (sessionHdr, [mirror StartTransaction 0])
            decoded q `shouldBe` Just (sessionHdr, [mirror EndTransaction mirrored])
          other -> expectationFailure (show other)
        (depth ses st', st' == st1) `shouldBe` (0, True)
    it "rolls a level back at End of Session, the TPer's own after the mirror in its subpacket, the session closed (3.3.7.3: the TPer commits only on the host's End Transaction; 3.3.7.1.5)" $ do
      let opened = prState (sessionPayload ses [StartTransactionItem 0] Nothing st1)
          put    = putSPView ses (withSidTries 3 (fromMaybe (error "no view") (spView ses opened))) opened
          Processed outcome st' _ = sessionPayload ses [EndOfSessionItem] Nothing put
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [generate EndOfSession])
        other        -> expectationFailure (show other)
      (open ses st', tsSPs st' == tsSPs st1) `shouldBe` (False, True)
      let Processed both _ _ = sessionPayload ses [StartTransactionItem 0, EndOfSessionItem] Nothing st1
      case both of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [mirror StartTransaction 0 <> generate EndOfSession])
        other        -> expectationFailure (show other)
    it "drops the mirrors where it aborts: Start Transaction, a Get, then a second Get past MaxMethods sends the Get's result alone before the CloseSession (3.2.2.4.1 item 1)" $ do
      let Processed outcome st' _ = sessionPayload ses [StartTransactionItem 0, Invoke getCall, Invoke getCall] Nothing st1
      case outcome of
        SessionAborted [p] (Just _) -> decoded p `shouldBe` Just (sessionHdr, [generate getResult])
        other                       -> expectationFailure (show other)
      open ses st' `shouldBe` False
    it "aborts at a transaction control token after End of Data, where the status list is expected, as at a token out of place (3.2.2.4.2 item 2)" $ do
      let inside = B.take (B.length (generate getCall) - B.length (generate (statusList 0))) (generate getCall) <> generate StartTransaction <> generate (Unsigned 0) <> generate (statusList 0)
          (is, f) = itemsOf inside
      is `shouldBe` []
      f `shouldSatisfy` isJust
      let Processed outcome st' _ = uncurry (sessionPayload ses) (is, f) st1
      outcome `shouldSatisfy` \o -> case o of
        SessionAborted [] (Just _) -> True
        _                          -> False
      open ses st' `shouldBe` False
    it "aborts the session at a transaction control token within a method invocation, in its header or its parameter list (Core 3.3.7.3, PDF 78)" $
      pendingWith "Hsed.Core.Stream reads a transaction control token there as any unexpected token, a BadCall answered NOT_AUTHORIZED or INVALID_PARAMETER with the session kept (3.2.2.4.2 items 3-4, PDF 39), where 3.3.7.3 (PDF 78, 71e3a18f1fb9) has the session aborted: hsed-core's, no group's at M5b"
    it "ends the payload at a behaviour's own RESPONSE_OVERFLOW, as at the framing's (Opal 3.3.1 item 1): a Get on a byte table bounded before it builds its bytes" $
      pendingWith "group G1's Base.get on a byte table answers RESPONSE_OVERFLOW (M5b); until it merges no behaviour answers it, and a host's status echoed is not one"
    it "answers a Get within a transaction with the value put in the session's view (Core 3.3.7.3: visible within the transaction)" $
      pendingWith "group G1's Base.get reads spView (M5b); until it merges Get reads the committed SP alone, and [Start Transaction, Set, Get] in one subpacket is the integration's example"
    it "sends the results completed before the fault: a Get, then an unexpected token" $ do
      let Processed outcome st' _ = sessionPayload ses [Invoke getCall] (Just (Fault 9 OutsideCall "StartName outside a method invocation")) st1
      case outcome of
        SessionAborted [p] (Just _) -> decoded p `shouldBe` Just (sessionHdr, [generate getResult])
        other                       -> expectationFailure (show other)
      open ses st' `shouldBe` False
    it "aborts at a second invocation past the TPer's MaxMethods, 1, after answering the first (Core 5.2.2.4.1.7)" $ do
      let Processed outcome st' _ = sessionPayload ses [Invoke getCall, Invoke getCall] Nothing st1
      case outcome of
        SessionAborted [p] (Just _) -> decoded p `shouldBe` Just (sessionHdr, [generate getResult])
        other                       -> expectationFailure (show other)
      open ses st' `shouldBe` False
    it "answers two invocations within a TPer MaxMethods of 2, each in a packet of its own for a host taking one subpacket a packet" $ do
      let Processed outcome st' _ = sessionPayload ses [Invoke getCall, Invoke getCall] Nothing (withTPer maxMethodsName 2 st1)
      case outcome of
        Prepared [p, q] -> do
          decoded p `shouldBe` Just (sessionHdr, [generate getResult])
          decoded q `shouldBe` Just (sessionHdr, [generate getResult])
        other -> expectationFailure (show other)
      open ses st' `shouldBe` True
    it "packs two responses into one packet of two subpackets for a host whose MaxSubpackets is 2" $ do
      let Processed outcome _ _ = sessionPayload (hostOf maxSubpacketsName 2 ses) [Invoke getCall, Invoke getCall] Nothing (withTPer maxMethodsName 2 st1)
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [generate getResult, generate getResult])
        other        -> expectationFailure (show other)
    it "answers RESPONSE_OVERFLOW, an empty list, for a result the host's packet cannot hold (Opal 3.3.1)" $ do
      let Processed outcome _ _ = sessionPayload (hostOf maxPacketSizeName 60 ses) [Invoke getCall] Nothing st1
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [generate (MethodResponse [] (statusOf RESPONSE_OVERFLOW))])
        other        -> expectationFailure (show other)
    it "ends the payload's processing at an overflow, the items after it not processed (Opal 3.3.1 item 1): a second Get within MaxMethods 2 goes unanswered, the session kept" $ do
      let Processed outcome st' ends = sessionPayload (hostOf maxPacketSizeName 60 ses) [Invoke getCall, Invoke getCall] Nothing (withTPer maxMethodsName 2 st1)
      ends `shouldBe` True
      case outcome of
        Prepared [p] -> decoded p `shouldBe` Just (sessionHdr, [generate (MethodResponse [] (statusOf RESPONSE_OVERFLOW))])
        other        -> expectationFailure (show other)
      open ses st' `shouldBe` True
    it "says the payload goes on after a response that fits, a mirror or an End of Session, and ends at an abort" $ do
      prEnds (sessionPayload ses [Invoke getCall] Nothing st1) `shouldBe` False
      prEnds (sessionPayload ses [Invoke getCall, EndOfSessionItem] Nothing st1) `shouldBe` False
      prEnds (sessionPayload ses [StartTransactionItem 0, EndTransactionItem 0] Nothing st1) `shouldBe` False
      prEnds (sessionPayload ses [] (Just (Fault 0 OutsideCall "EndList outside a method invocation")) st1) `shouldBe` True
    it "echoes a host's status of RESPONSE_OVERFLOW as any other, the payload going on (Core 3.2.4.2): it is no overflow of the TPer's" $ do
      let Processed outcome _ ends = sessionPayload ses [Invoke getCall { _statusCode = statusOf RESPONSE_OVERFLOW }, Invoke getCall] Nothing (withTPer maxMethodsName 2 st1)
      ends `shouldBe` False
      case outcome of
        Prepared [p, q] -> do
          decoded p `shouldBe` Just (sessionHdr, [generate (MethodResponse [] (statusOf RESPONSE_OVERFLOW))])
          decoded q `shouldBe` Just (sessionHdr, [generate getResult])
        other -> expectationFailure (show other)
    it "reads the items a session's stream gives: ETC-06's Call EndList is a header that does not read, EndList before Call a fault" $ do
      let (badHeader, noFault) = itemsOf (generate Call <> generate EndList <> B.drop 1 (generate getCall))
      noFault `shouldBe` Nothing
      badHeader `shouldSatisfy` \is -> case is of
        [BadCall (Fault _ InHeader _)] -> True
        _                              -> False
      itemsOf (generate EndList <> generate getCall) `shouldSatisfy` \(is, f) -> null is && isJust f

  describe "dispatch" $ do
    it "echoes a host status other than 0 with an empty result (Core 3.2.4.2)" $
      dispatch ses getCall { _statusCode = 7 } st1 `shouldBe` (MethodResponse [] 7, st1)
    it "refuses NOT_AUTHORIZED where the AccessControl table has no row for the invocation, or the ACL is not satisfied (5.1.5.2)" $ do
      fst (dispatch ses (invocation uC_PIN_SID (Methods.Get (cellBlockValue noCells))) st1) `shouldBe` MethodResponse [] (statusOf NOT_AUTHORIZED)
      fst (dispatch ses (invocation uC_PIN_MSID (Methods.Properties Nothing)) st1) `shouldBe` MethodResponse [] (statusOf NOT_AUTHORIZED)
    it "runs the behaviour of the method's record: Get's" $
      dispatch ses getCall st1 `shouldBe` (getResult, st1)
    it "answers INVALID_PARAMETER where the parameters are not the method's (5.1.5.11)" $
      fst (dispatch ses getCall { _parameters = Parameters [] [] } st1) `shouldBe` MethodResponse [] (statusOf INVALID_PARAMETER)
    it "gates by the SP as the session sees it (spView, M5b): with the AccessControl rows gone from the session's view alone, the Get is NOT_AUTHORIZED, and granted again once the level is dropped" $ do
      let opened  = prState (sessionPayload ses [StartTransactionItem 0] Nothing st1)
          noAcl   = (\sps -> sps { spsTables = Map.map (const []) (spsTables sps) }) (fromMaybe (error "no view") (spView ses opened))
          blinded = putSPView ses noAcl opened
      fst (dispatch ses getCall blinded) `shouldBe` MethodResponse [] (statusOf NOT_AUTHORIZED)
      tsSPs blinded `shouldBe` tsSPs st1
      fst (dispatch ses getCall (prState (sessionPayload ses [EndTransactionItem 1] Nothing blinded))) `shouldBe` getResult

  describe "the framing within the host's properties (packets, fits)" $ do
    let host = comIdHostLimits defaultSimParams initialHostProperties
        streams = replicate 3 (generate getResult)
    it "headers: the control session's Session 0, a session's TSN then HSN, no sequence number or acknowledgement" $ do
      controlHeader `shouldBe` PacketHeader 0 0 0 0 0
      sessionHeader ses `shouldBe` sessionHdr
    it "puts each stream in a packet of its own for a host taking one subpacket a packet (Opal Table 17's assumption)" $
      fmap (map decoded) (packets host sessionHdr streams) `shouldBe` Right (map (\s -> Just (sessionHdr, [s])) streams)
    it "packs as many subpackets a packet as the host's MaxSubpackets and MaxPacketSize allow" $ do
      let roomy = comIdHostLimits defaultSimParams (Map.insert maxSubpacketsName 2 initialHostProperties)
          tight = comIdHostLimits defaultSimParams (Map.fromList [(maxSubpacketsName, 0), (maxPacketSizeName, 150)])
      fmap (map decoded) (packets roomy sessionHdr streams) `shouldBe` Right [Just (sessionHdr, take 2 streams), Just (sessionHdr, take 1 streams)]
      fmap (map (fmap (length . snd) . decoded)) (packets tight sessionHdr streams) `shouldBe` Right [Just 2, Just 1]
      fmap (map (fmap (length . snd) . decoded)) (packets (comIdHostLimits defaultSimParams (Map.fromList [(maxSubpacketsName, 0)])) sessionHdr streams)
        `shouldBe` Right [Just 3]
    it "fits: a stream within the host's packet and ComPacket, or not" $ do
      fits host sessionHdr (generate getResult) `shouldBe` True
      fits (comIdHostLimits defaultSimParams (Map.fromList [(maxPacketSizeName, 60)])) sessionHdr (generate getResult) `shouldBe` False
      fits (comIdHostLimits defaultSimParams (Map.fromList [(maxComPacketSizeName, 60)])) sessionHdr (generate getResult) `shouldBe` False
      packets (comIdHostLimits defaultSimParams (Map.fromList [(maxPacketSizeName, 60)])) sessionHdr streams `shouldSatisfy` isLeft
      packets host sessionHdr [] `shouldBe` Right []
    it "bounds a packet by the host's ComPacket as well as its packet: six subpackets a packet of any size, within a ComPacket of 2048, make packets of at most 2028 bytes, three subpackets each (Core 5.2.2.4.1.4)" $ do
      let wide = comIdHostLimits defaultSimParams (Map.fromList [(maxSubpacketsName, 6), (maxPacketSizeName, 0)])
          big  = replicate 6 (B.replicate 600 7)
      fmap (map B.length) (packets wide sessionHdr big) `shouldSatisfy` either (const False) (all (<= 2048 - comPacketHeaderSize))
      fmap (map (fmap (length . snd) . decoded)) (packets wide sessionHdr big) `shouldBe` Right [Just 3, Just 3]
    prop "stages no packet larger than the host's packet, nor than what its ComPacket carries, over any host properties the host may have sent (Core 5.2.2.4.1.2 and 4)" $
      forAll genHostProperties $ \props ->
      forAll (chooseInt (1, 8) >>= (`vectorOf` (B.pack <$> (chooseInt (0, 600) >>= (`vectorOf` arbitrary))))) $ \payloads ->
        let host' = comIdHostLimits defaultSimParams props
            most  = min (lPacketSize host') (lComPacketSize host' - comPacketHeaderSize)
        in counterexample (show (props, fmap (map B.length) (packets host' sessionHdr payloads)))
             (either (const False) (all ((<= most) . B.length)) (packets host' sessionHdr payloads))

  describe "through the simulator's step: the error cases on a regular session" $ do
    let packetOf hdr payloads = PacketFrame hdr (map (DataSubPacket (DataSubPacketHeader 0)) payloads)
        comPacketOf ps = either (const mempty) id (encodeComPacket (ComPacketFrame (ComPacketHeader base 0 0 0) ps))
        ifSend bytes st = step (Cmd (Request IfSend comPacketProtocol base (roundUp UnitBytes (fromIntegral (B.length bytes))) bytes)) st
        ifRecv st = step (Cmd (Request IfRecv comPacketProtocol base 2048 mempty)) st
        -- the IF-SEND accepted, the response released, then the IF-RECV's ComPacket
        exchange bytes st = do
          let (sent, st') = ifSend bytes st
          sent `shouldBe` Out (Response Good mempty)
          let (_, st'') = step (Tick base) st'
              (received, st3) = ifRecv st''
          case received of
            Out r -> do
              status r `shouldBe` Good
              either (\e -> expectationFailure (show e) >> throwString "no ComPacket") (\(DecodedFrame f _ _) -> pure (f, st3)) (decodeComPacket (rpayload r))
            other -> expectationFailure (show other) >> throwString "no response"
        closeSessionAnswer f = case cpfPackets f of
          [p] -> (pfHeader p, map dataSubPacketPayload (pfSubPackets p)) == (controlHeader, [generate (closeSessionCall ses)])
          _   -> False
    it "ETC-05: EndList before Call aborts the session, and the next IF-RECV returns CloseSession [HSN, TSN] (Test Cases PDF 79)" $ do
      (f, st') <- exchange (comPacketOf [packetOf sessionHdr [generate EndList <> generate getCall]]) st1
      f `shouldSatisfy` closeSessionAnswer
      lifeOf base st' `shouldBe` Just Issued
      fst (ifRecv st') `shouldSatisfy` \o -> case o of
        Out r -> fmap (dfFrame) (decodeComPacket (rpayload r)) == Right (ComPacketFrame (ComPacketHeader base 0 0 0) [])
        _     -> False
    it "ETC-05 without a CloseSession prepared: the next IF-RECV is (0, 0, 0), the session gone" $ do
      let (sent, st') = ifSend (comPacketOf [packetOf sessionHdr [generate EndList <> generate getCall]]) quietSt1
      sent `shouldBe` Out (Response Good mempty)
      syncOfBase st' `shouldBe` Just S1
      lifeOf base st' `shouldBe` Just Issued
      fst (ifRecv st') `shouldSatisfy` \o -> case o of
        Out r -> fmap dfFrame (decodeComPacket (rpayload r)) == Right (ComPacketFrame (ComPacketHeader base 0 0 0) [])
        _     -> False
    it "ETC-06: Call EndList in the method header is NOT_AUTHORIZED with an empty result, the session kept (Test Cases PDF 80)" $ do
      (f, st') <- exchange (comPacketOf [packetOf sessionHdr [generate Call <> generate EndList <> B.drop 1 (generate getCall)]]) st1
      map (\p -> (pfHeader p, map dataSubPacketPayload (pfSubPackets p))) (cpfPackets f) `shouldBe` [(sessionHdr, [generate (MethodResponse [] (statusOf NOT_AUTHORIZED))])]
      lifeOf base st' `shouldBe` Just Associated
    it "ETC-14's shape: a session packet of two subpackets, more than the TPer's MaxSubpackets, aborts the session (Core 5.2.2.4.1.1; Test Cases PDF 87)" $ do
      (f, st') <- exchange (comPacketOf [packetOf sessionHdr [generate getCall, generate getCall]]) st1
      f `shouldSatisfy` closeSessionAnswer
      lifeOf base st' `shouldBe` Just Issued
    it "two invocations in one subpacket: the first answered, then the abort, in two packets the host takes one at a time" $ do
      (f, st') <- exchange (comPacketOf [packetOf sessionHdr [generate getCall <> generate getCall]]) st1
      map (\p -> (pfHeader p, map dataSubPacketPayload (pfSubPackets p))) (cpfPackets f) `shouldBe` [(sessionHdr, [generate getResult])]
      comPacketOutstandingData (cpfHeader f) `shouldSatisfy` (> 0)
      (f', st'') <- case ifRecv st' of
        (Out r, s) -> either (\e -> expectationFailure (show e) >> throwString "no ComPacket") (\(DecodedFrame fr _ _) -> pure (fr, s)) (decodeComPacket (rpayload r))
        (other, _) -> expectationFailure (show other) >> throwString "no response"
      f' `shouldSatisfy` closeSessionAnswer
      lifeOf base st'' `shouldBe` Just Issued
    it "[Get, EOS] through the step: the result and the TPer's End of Session, the session closed, then (0, 0, 0)" $ do
      (f, st') <- exchange (comPacketOf [packetOf sessionHdr [generate getCall <> generate EndOfSession]]) st1
      map (\p -> (pfHeader p, map dataSubPacketPayload (pfSubPackets p))) (cpfPackets f) `shouldBe` [(sessionHdr, [generate getResult <> generate EndOfSession])]
      lifeOf base st' `shouldBe` Just Issued
      syncOfBase st' `shouldBe` Just S1
  where
    syncOfBase st = cSync <$> Map.lookup base (tsComIds st)
