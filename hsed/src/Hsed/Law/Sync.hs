{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Law.Sync
Description : The synchronous protocol's laws (Core 3.3.10): what the simulator's S1, S2 and S3 hold to
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The laws the comments of Core 3.3.10 bind (design example E10, milestone
M3): each names the paragraph or table it implements by the first 12 hex
digits of its IR hash (@spec/ir/core-2.01.json@), and the Test Cases'
cases that observe it. Each says what it holds as a QuickCheck property
('lawProp') over the simulator's step ("Hsed.Sim") and the generators of
"Hsed.Sim.Gen": states of every unit, one to three static ComIDs in every
synchronous state, each keeping the host's properties as the host may have
sent them, none to two regular sessions open (M4a), requests of every
length a unit allows.
-}
module Hsed.Law.Sync
  ( ifSendWhileBusy
  , discardToS1
  , ignorePacket
  , table45
  , laws
  ) where

import qualified RIO.ByteString            as B
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO
import           Test.QuickCheck           (Gen, Property, arbitrary, conjoin, counterexample, elements, forAll, oneof, suchThat,
                                            (===), (==>))

import           Hsed.Core.Record          (encodeRecord)
import           Hsed.Core.Sync            (Setting (..), StateRow (..), StateTable (..))
import           Hsed.Gen.Core.Layouts     (ComPacket (..), ComPacketHeader (..))
import qualified Hsed.Gen.Core.StateTables as StateTables
import           Hsed.Jig.Catalogue        (CaseKey (..), TestId (..))
import           Hsed.Law                  (Law (..))
import           Hsed.Sim                  (step, syncOf)
import           Hsed.Sim.Gen              (genNoActionPayload, genOverfullPayload, genState, genStaticRequest,
                                            genUnresolvablePayload)
import           Hsed.Sim.Sync             (Delivery (..), T45Row (..), t45Fields, t45Quantity, t45Rows)
import           Hsed.Sim.Types            (ComIdState (..), Input (..), Limits (..), Output (..), SimParams (..), SyncState (..),
                                            TPerState (..), comIdHostLimits)
import           Hsed.Wire                 (Dir (..), IfStatus (..), Request (..), Response (..), comPacketHeaderSize,
                                            comPacketProtocol, roundUp)


-- | Core 3.3.10.2.1 item 3 a (PDF 82, @core-2.01 | 3.3.10.2.1#3a@): an
--   IF-SEND on a ComID whose previous IF-SEND's response the host has not
--   retrieved SHALL be aborted with Synchronous Protocol Violation; the
--   TPer's state is as it was. ETC-02 observes it. The property: for a
--   ComID processing (S2) or awaiting IF-RECV (S3), any IF-SEND on it, of
--   any length its unit allows, is answered Synchronous Protocol Violation
--   with no bytes, the TPer unchanged.
ifSendWhileBusy :: Law
ifSendWhileBusy = Law
  { lawName  = "Sync.ifSendWhileBusy"
  , lawHash  = "37bedfb19343"
  , lawProp  = Just $
      forAll (genState `suchThat` (not . null . busy)) $ \st ->
      forAll (elements (busy st)) $ \c ->
      forAll (genStaticRequest st `suchThat` (\r -> dir r == IfSend && comId r == c)) $ \r ->
        step (Cmd r) st === (Out (Response SynchronousProtocolViolation mempty), st)
  , lawCases = [whole (ETC 2)]
  }
  where
    busy st = [ c | (c, cs) <- Map.toList (tsComIds st), cSync cs /= S1 ]

-- | Core 3.3.10.6, S2:S1 item d.c (PDF 86, @core-2.01 | 3.3.10.6#dc@): an
--   IF-SEND whose packets' subpackets have no payload or Empty atoms alone
--   returns the ComID to Awaiting IF-SEND, and the next IF-RECV has nothing
--   to return (design 8.2 decides it at the IF-SEND). SPF-02 case 1's
--   simulator extension observes it. The property: on a ComID awaiting
--   IF-SEND, such an IF-SEND ("Hsed.Sim.Gen".'genNoActionPayload', with its
--   other cases of item d) is accepted (Good), the TPer unchanged, the ComID
--   still in S1, and an IF-RECV after it is answered with Table 45's last
--   row.
discardToS1 :: Law
discardToS1 = Law
  { lawName  = "Sync.discardToS1"
  , lawHash  = "cb4d5d489fc0"
  , lawProp  = Just (discardedAtIfSend (const genNoActionPayload))
  , lawCases = [caseOf (SPF 2) 1]
  }

-- | Core 3.3.10.7 item a (PDF 86, @core-2.01 | 3.3.10.7#a@): a packet whose
--   session the TPer cannot resolve, or that violates the packet
--   requirements, is ignored, and the TPer awaits an IF-SEND at once. ETC-04
--   observes the first clause (a Get in a packet of an open session's TSN
--   and another HSN, Test Cases PDF 78) and ETC-15 the second (a control
--   session packet of more subpackets than the TPer's MaxSubpackets, which
--   it SHALL discard and ignore, 5.2.2.4.1.1, PDF 145; PDF 88). The
--   property: on a ComID awaiting IF-SEND, of a TPer with sessions open or
--   none, an IF-SEND whose packets name sessions the TPer does not have on
--   that ComID, an open session's TSN with another HSN among them
--   ("Hsed.Sim.Gen".'genUnresolvablePayload'), or whose control session
--   packet holds more subpackets than the TPer's MaxSubpackets or more
--   bytes than its MaxPacketSize ('genOverfullPayload'), each even holding a
--   call the Session Manager answers, is accepted (Good), the TPer
--   unchanged, its sessions among the rest, the ComID still in S1, and an
--   IF-RECV after it is answered with Table 45's last row. (ETC-08 observes
--   Opal 2.30 3.3.4.1.3 with Core 3.2.2.4.2 items 1 and 4, which no anchor
--   can name yet: design 13.29 (j).)
ignorePacket :: Law
ignorePacket = Law
  { lawName  = "Sync.ignorePacket"
  , lawHash  = "cda4e92af591"
  , lawProp  = Just (discardedAtIfSend (\st c -> oneof [genUnresolvablePayload st c, genOverfullPayload st c]))
  , lawCases = [whole (ETC 4), whole (ETC 15)]
  }

-- | Core Table 45 (PDF 83), bound by its comment @stateTable
--   "Sync.table45"@ ("Hsed.Gen.Core.StateTables".@table45@): the ComPacket
--   header fields an IF-RECV is answered with in each state. SPF-02 case 1
--   and ETC-02 observe it. The property:
--
--   * the generated rows are 'T45Row''s, one each, in order, and each is
--     the state it names ('rowState');
--   * every quantity a row names in words is computed ("Hsed.Sim.Sync".'t45Quantity'),
--     and words the table does not use are not;
--   * an IF-RECV of any length the unit allows on any static ComID in any
--     state is answered with exactly that many bytes: a ComPacket of the
--     ComID and its extension whose header is the row that state selects
--     (Core 3.3.10.5, PDF 84-85), the packets it returns as many as fit the
--     transfer, the host's ComPacket and its MaxPackets (5.2.2.4.1.3-4, PDF
--     145-146), and the ComID's state after it as Core 3.3.10.6 (PDF 85-86)
--     says; the header's Length, OutstandingData and MinTransfer are the
--     numbers this law computes itself (the packets returned; the ComPacket
--     header and the packets left, or all of them; the header and the next
--     packet, or the first), which the generated row, evaluated
--     ("Hsed.Sim.Sync".'t45Fields'), must also give.
table45 :: Law
table45 = Law
  { lawName  = "Sync.table45"
  , lawHash  = "498ea00e3c6a"
  , lawProp  = Just $ conjoin
      [ counterexample "Table 45's rows are T45Row's, in order" $
          (t45Rows, map state (states StateTables.table45)) === ([minBound .. maxBound], map rowState [minBound .. maxBound])
      , counterexample "a quantity Table 45 names is computed by no code" $
          conjoin [ counterexample (T.unpack q) (isJust (t45Quantity q)) | q <- quantities ]
      , forAll (T.pack <$> arbitrary) $ \t -> t `notElem` quantities ==> isNothing (t45Quantity t)
      , forAll genState $ \st ->
        forAll (elements (Map.toList (tsComIds st))) $ \(c, cs) ->
        forAll (recvLength st (cSync cs)) $ \len ->
          let (row, d, after) = selected (comIdHostLimits (tsParams st) (cHostProps cs)) len (cSync cs)
              (out, st')      = step (Cmd (Request IfRecv comPacketProtocol c len mempty)) st
          in conjoin
               [ counterexample "the generated row evaluates to the law's own numbers" $ t45Fields row d === Right (expected row d)
               , either (`counterexample` False)
                        (\resp -> conjoin [ out === Out resp, syncOf c st' === Just after ])
                        (answerOf c (cExt cs) len (expected row d) d)
               ]
      ]
  , lawCases = [caseOf (SPF 2) 1, whole (ETC 2)]
  }
  where
    quantities = L.nub [ q | r <- states StateTables.table45, Quantity q <- settings r ]

-- | What a law says of an IF-SEND whose payload, on its ComID, the TPer
--   discards (the payload generated for the ComID of the TPer as it is,
--   its sessions known): on a ComID awaiting IF-SEND, of a TPer in any
--   state otherwise, accepted (Good, no bytes), the TPer as it was, the
--   whole of it (its sessions, the properties its ComIDs keep of the host,
--   its SPs' rows and its ComID management buffers among the rest), and an
--   IF-RECV of any length after it answered with Table 45's last row,
--   (0, 0, 0) (Core 3.3.10.5 S1 b, PDF 84), the TPer still as it was.
discardedAtIfSend :: (TPerState -> Word16 -> Gen ByteString) -> Property
discardedAtIfSend payloadOn =
  forAll genState $ \st0 ->
  forAll (elements (Map.keys (tsComIds st0))) $ \c ->
  let st   = st0 { tsComIds = Map.adjust (\cs -> cs { cSync = S1 }) c (tsComIds st0) } in
  forAll (payloadOn st c) $ \bytes ->
  let len  = roundUp (spUnits (tsParams st)) (fromIntegral (B.length bytes))
      sent = step (Cmd (Request IfSend comPacketProtocol c len (bytes <> B.replicate (fromIntegral len - B.length bytes) 0))) st
  in forAll (lenBytes <$> (genStaticRequest st `suchThat` ((== IfRecv) . dir))) $ \recvLen ->
     let received = step (Cmd (Request IfRecv comPacketProtocol c recvLen mempty)) (snd sent)
     in conjoin
          [ counterexample "the IF-SEND accepted, the TPer unchanged" $ sent === (Out (Response Good mempty), st)
          , counterexample "the IF-RECV after it" $
              either (`counterexample` False) (\resp -> received === (Out resp, st))
                     (answerOf c (maybe 0 cExt (Map.lookup c (tsComIds st))) recvLen (0, 0, 0) (Delivery [] []))
          ]

-- | The length of an IF-RECV on a ComID in a state: any its unit allows, or,
--   two times in three when packets are staged, one that falls short of the
--   ComPacket header and the first packet, or one at the edge of a run of
--   them fitting (the header and the run, a byte either side, rounded up to
--   the unit), so that each row of S3 is often met.
recvLength :: TPerState -> SyncState -> Gen Word32
recvLength st = \case
  S3 ps@(p : _) -> oneof [anyLength, short p, elements (edges ps)]
  _             -> anyLength
  where
    u         = spUnits (tsParams st)
    anyLength = lenBytes <$> (genStaticRequest st `suchThat` ((== IfRecv) . dir))
    short p   = case takeWhile (< fromIntegral (comPacketHeaderSize + B.length p)) (L.iterate (+ least) least) of
      [] -> anyLength
      ls -> elements ls
    least     = roundUp u 1
    edges ps  = [ l | run <- L.scanl1 (+) (map B.length ps), side <- [-1, 0, 1]
                    , let l = roundUp u (fromIntegral (comPacketHeaderSize + run + side)), l > 0 ]

-- | The row an IF-RECV of a length selects in a synchronous state, given
--   what the host takes, what it returns, and the state after it (Core
--   3.3.10.5, PDF 84-85; 3.3.10.6, PDF 85-86): in S1 the last row; in S2 the
--   first; in S3, room short of the ComPacket header and the first packet
--   staged the second (the room the transfer's and the host's ComPacket's,
--   whichever is less); else the packets that fit, as many as the host's
--   MaxPackets, the fifth row if they are all of them (then S1), the third
--   if not (S3 with the rest).
selected :: Limits -> Word32 -> SyncState -> (T45Row, Delivery, SyncState)
selected host len = \case
  S1    -> (AllReturnedNoData, Delivery [] [], S1)
  S2 ps -> (ResponsesToCome, Delivery [] [], S2 ps)
  S3 ps
    | null ps                                  -> (AllReturnedNoData, Delivery [] [], S1)
    | room < maybe 0 B.length (listToMaybe ps) -> (InsufficientLength, Delivery ps [], S3 ps)
    | null rest                                -> (AllReturnedWithData, Delivery ps sent, S1)
    | otherwise                                -> (AdditionalAvailable, Delivery ps sent, S3 rest)
    where
      room         = min (fromIntegral len) (lComPacketSize host) - comPacketHeaderSize
      (sent, rest) = L.splitAt (min (lPackets host) (length (takeWhile (<= room) (L.scanl1 (+) (map B.length ps))))) ps

-- | A row's Length, OutstandingData and MinTransfer over a delivery, as this
--   law computes them from Table 45's words (PDF 83) and 3.2.3.2.1.4-6 (PDF
--   41: the overhead counted): Length the bytes of the packets returned;
--   OutstandingData, where the row names it, the ComPacket header and every
--   packet staged (row 2) or every packet left (row 3); MinTransfer, where
--   the row names it, the header and the first packet staged (row 2) or the
--   next left (row 3); row 1 (0, 1, 0), row 4 (Length, 1, 0), the rest (Length, 0, 0).
expected :: T45Row -> Delivery -> (Word32, Word32, Word32)
expected row d = case row of
  ResponsesToCome     -> (0, 1, 0)
  InsufficientLength  -> (0, carrying staged, carrying (take 1 staged))
  AdditionalAvailable -> (len, carrying left, carrying (take 1 left))
  AdditionalToCome    -> (len, 1, 0)
  AllReturnedWithData -> (len, 0, 0)
  AllReturnedNoData   -> (0, 0, 0)
  where
    staged     = dlStaged d
    left       = drop (length (dlReturned d)) staged
    len        = sizeOf (dlReturned d)
    carrying ps = if null ps then 0 else fromIntegral comPacketHeaderSize + sizeOf ps
    sizeOf     = fromIntegral . sum . map B.length

-- | The answer an IF-RECV of a length on a ComID gets with header fields
--   over a delivery: exactly that many bytes of the ComPacket they head, the
--   packets returned its payload; Left if its Length is not the bytes
--   returned.
answerOf :: Word16 -> Word16 -> Word32 -> (Word32, Word32, Word32) -> Delivery -> Either String Response
answerOf c ext len (l, od, mt) d = do
  let returned = mconcat (dlReturned d)
  unless (fromIntegral l == B.length returned) $
    Left ("Length " <> show l <> " to " <> show (B.length returned) <> " bytes returned")
  bytes <- first show (encodeRecord (ComPacket (ComPacketHeader c ext od mt) returned))
  pure (Response Good (B.take (fromIntegral len) (bytes <> B.replicate (fromIntegral len - B.length bytes) 0)))

-- | The state each of Table 45's rows names, as printed (two hold an en
--   dash, U+2013).
rowState :: T45Row -> Text
rowState = \case
  ResponsesToCome     -> "Response(s) to come, no Response(s) available"
  InsufficientLength  -> "Response ready, insufficient transfer length request"
  AdditionalAvailable -> "Response, additional Response(s) available"
  AdditionalToCome    -> "Response, additional Response(s) to come, no Response(s) available"
  AllReturnedWithData -> "Response, all Response(s) returned \8211 no further data"
  AllReturnedNoData   -> "All Response(s) returned \8211 no further data"

-- | Every law of the module.
laws :: [Law]
laws = [ifSendWhileBusy, discardToS1, ignorePacket, table45]

-- | A test, all of it.
whole :: TestId -> CaseKey
whole t = CaseKey t Nothing Nothing Nothing

-- | One case of a test.
caseOf :: TestId -> Int -> CaseKey
caseOf t c = CaseKey t (Just c) Nothing Nothing
