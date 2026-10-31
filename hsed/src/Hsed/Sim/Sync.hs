{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Sim.Sync
Description : The synchronous protocol of a static ComID: S1, S2, S3, the three outcomes of an IF-SEND, Table 45's answers to IF-RECV
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The TPer's side of Core 3.3.10 (PDF 82-87) on a static ComID: an IF-SEND
accepted in Awaiting IF-SEND (S1) yields an 'Hsed.Sim.Types.Outcome'
('classifyIfSend'): response packets prepared, the ComID processing (S2)
until released, then awaiting IF-RECV (S3); the payload discarded, the
ComID kept in S1 (design 8.2); or a session aborted. An IF-SEND in S2 or S3
is a synchronous protocol violation, the state unchanged. An IF-RECV is
answered in each state with the ComPacket header fields Core Table 45 gives
(PDF 83, 'ifRecvStatic').

Each packet of an IF-SEND names a session (3.3.7.1.2, PDF 73): the control
session, Session 0, whose invocations the Session Manager answers
("Hsed.Sim.SessionManager".@answer@, the TPer's state threaded through
them), or a regular session the TPer has open ("Hsed.Sim.State"), whose
data subpackets "Hsed.Sim.Session" processes; one it has not is ignored
(3.3.10.7 a, PDF 86). The token streams are read by "Hsed.Core.Stream":
the atoms under the TPer's policy (its MaxIndTokenSize; no signed integer,
no continued byte sequence, Opal 2.30 Table 15, PDF 30-31), then the items
as the control session or a regular session reads them.

Table 45 is generated ("Hsed.Gen.Core.StateTables".@table45@) and bound by
its comment to this code (@Sync.table45@, the law "Hsed.Law.Sync"). Two of
its state names hold an en dash, so its rows are matched by position:
'T45Row' names them in the order printed, and 't45Rows' is that order over
the generated rows. An IF-RECV's answer is the row its state selects,
evaluated ('t45Fields'): a field the row prints as a constant is that
constant, and one it names in words is the computation 't45Quantity' gives
those words, over what the ComID has staged and what the answer returns
('Delivery').
-}
module Hsed.Sim.Sync
  ( -- * Core Table 45
    T45Row (..)
  , t45Rows
  , Delivery (..)
  , t45Quantity
  , t45Fields
    -- * The synchronous protocol of a static ComID
  , classifyIfSend
  , ifRecvStatic
  ) where

import qualified RIO.ByteString            as B
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import           RIO

import           Hsed.Core.Call            (MethodCall (..), Parameters (..))
import           Hsed.Core.Code            (CodeRow (..), codeRowsNamed, inSpan)
import           Hsed.Core.Record          (encodeRecord)
import qualified Hsed.Core.Stream          as Stream
import           Hsed.Core.StreamItem      (StreamItem (..))
import           Hsed.Core.Sync            (Setting (..), StateTable (..), settingOf)
import           Hsed.Gen.Core.Codes       (StatusCode (..), cSubpacketTypes, pattern RESPONSE_OVERFLOW)
import           Hsed.Gen.Core.Layouts     (ComPacket (..), ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..),
                                            PacketHeader (..))
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Packet               (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, encodePacket)
import           Hsed.Sim.Session          (Processed (..), controlHeader, fits, packets, sessionPayload)
import           Hsed.Sim.SessionManager   (answer)
import           Hsed.Sim.State            (resolveSession)
import           Hsed.Sim.Types            (ComIdState (..), Limits (..), Outcome (..), Session (..), SimParams, SyncState (..),
                                            TPerState (..), comIdHostLimits, tperIndTokenSize, tperLimits)
import           Hsed.Wire                 (IfStatus (..), Response (..), comPacketHeaderSize, exactly, hex, staticExtension)


-- * Core Table 45

-- | Core Table 45's rows (PDF 83), in the order printed: what the TPer's
--   IF-RECV answer reports of its responses.
data T45Row
  = ResponsesToCome      -- ^ responses to come, none available: (0, 1, 0)
  | InsufficientLength   -- ^ a response ready, the transfer too short for it: (0, all available, the least to transfer it)
  | AdditionalAvailable  -- ^ a response, more available
  | AdditionalToCome     -- ^ a response, more to come, none available
  | AllReturnedWithData  -- ^ a response, all returned, no further data
  | AllReturnedNoData    -- ^ all returned, no further data: (0, 0, 0)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The constructors in the order of the generated table's rows, one for
--   each.
t45Rows :: [T45Row]
t45Rows = zipWith const [minBound .. maxBound] (states table45)

-- | What Table 45's quantities are computed from at an IF-RECV: the response
--   packets the ComID had staged before it, and those its ComPacket returns
--   (the first of them).
data Delivery = Delivery
  { dlStaged   :: ![ByteString]
  , dlReturned :: ![ByteString]
  }
  deriving (Eq, Show)

-- | Table 45's quantities, each the computation its words name; Nothing for
--   words the table does not use. OutstandingData counts what the TPer has
--   for the host apart from what this ComPacket returns, and MinTransfer
--   the least request that transfers a packet, both with their ComPacket,
--   packet and subpacket overhead, and both zero when nothing is left (Core
--   3.2.3.2.1.4-5, PDF 41): the bytes of a ComPacket that would carry them,
--   its header ('ComPacket''s layout) and the packets. Data Length is the
--   bytes of the packets the ComPacket returns, its Length (3.2.3.2.1.6,
--   PDF 41).
t45Quantity :: Text -> Maybe (Delivery -> Word32)
t45Quantity = \case
  "Total bytes currently available"                          -> Just (carrying . dlStaged)
  "The minimum request required to transfer a packet"        -> Just (carrying . take 1 . dlStaged)
  "Data Length"                                              -> Just (bytes . dlReturned)
  "Additional bytes available, not including the data transferred in the current ComPacket."
                                                             -> Just (carrying . remaining)
  "The minimum request required to transfer the next packet" -> Just (carrying . take 1 . remaining)
  _                                                          -> Nothing
  where
    remaining d = drop (length (dlReturned d)) (dlStaged d)
    carrying ps = if null ps then 0 else fromIntegral comPacketHeaderSize + bytes ps
    bytes       = fromIntegral . sum . map B.length

-- | A row's Length, OutstandingData and MinTransfer (the generated table's
--   fields, by name) over a delivery: a constant as printed, a quantity as
--   't45Quantity' computes it; Left names what no code reads.
t45Fields :: T45Row -> Delivery -> Either Text (Word32, Word32, Word32)
t45Fields row d = do
  stateRow <- maybe (Left ("Core Table 45 has no row for " <> tshow row)) Right (L.lookup row (zip t45Rows (states table45)))
  let field name = case settingOf table45 name stateRow of
        Nothing           -> Left ("Core Table 45's row " <> tshow row <> " sets no " <> name)
        Just (Constant n) -> Right (fromIntegral n)
        Just (Quantity q) -> maybe (Left ("Core Table 45's quantity \"" <> q <> "\" is computed by no code")) (Right . ($ d)) (t45Quantity q)
  (,,) <$> field "Length" <*> field "OutstandingData" <*> field "MinTransfer"


-- * The synchronous protocol of a static ComID

-- | What an IF-SEND accepted in S1 on a ComID comes to, and the TPer after
--   it: its response packets prepared, the payload discarded and why (the
--   TPer as it was), or a session aborted. The TPer's limits are its
--   properties' ("Hsed.Sim.Params".'tperLimits') and the host's those the
--   ComID keeps of it ('comIdHostLimits', 0 = no limit).
--
--   The ComPacket must be the ComID's, its extension a static ComID's
--   0x0000, with a packet (Core 3.3.3.1, PDF 57-58; 3.3.10.7 a, PDF 86;
--   3.3.10.6 S2:S1 d, PDF 85-86), else the payload is discarded. The first
--   TPer-MaxPackets packets are considered, the rest ignored, as it MAY
--   (5.2.2.4.1.3, PDF 145-146). Each considered packet names a session
--   (3.3.7.1.2, PDF 73): Session 0 is the control session; an open
--   session's TSN and HSN ("Hsed.Sim.State".'resolveSession') is that
--   session; any other packet discards the whole payload, the TPer
--   unchanged (3.3.10.7 a: the session cannot be resolved; what ETC-04
--   sends, Test Cases PDF 78; the law @Sync.ignorePacket@).
--
--   The control session's packets are read before anything changes, so
--   that a payload the TPer discards changes nothing:
--
--   * a packet of more subpackets than the TPer's MaxSubpackets, or of more
--     bytes than its MaxPacketSize, is discarded and ignored with the whole
--     payload, the TPer unchanged (5.2.2.4.1.1-2, PDF 145: the packet
--     discarded and ignored; 3.3.10.7 a, PDF 86: on a violation of packet
--     requirements the TPer SHALL ignore the entire packet and SHALL
--     immediately transition to awaiting an IF-SEND, which answering the
--     payload's other packets would not do; what ETC-15 sends, PDF 88;
--     design 8.2);
--   * each data subpacket's atoms are read under the TPer's policy
--     ("Hsed.Core.Stream".'Stream.atoms': Empty atoms dropped wherever they
--     are, Table 14, PDF 36), then its items as the control session reads
--     them ('Stream.items' 'Stream.ControlStream': End of Session and the
--     transaction control tokens ignored, each with the status after it,
--     3.3.7.1.3, PDF 73); a token that does not read, is signed or too
--     large (3.2.2.4.1, PDF 38; Opal 2.30 Table 15, PDF 30-31), an
--     invocation whose header or parameter list does not read, a token
--     outside an invocation or in a status list, or a stream ending inside
--     an invocation (3.2.2.4.2, PDF 39; 3.3.10.3 a) is a streaming error on
--     the Session Manager, which discards the whole payload (Opal 2.30
--     3.3.4.1.3, PDF 31; the lead's M4a decision 8);
--   * a subpacket of another Kind than data (Table 19, PDF 43) is not read;
--     the invocations of a subpacket past the TPer's MaxMethods are ignored,
--     as it MAY (5.2.2.4.1.7, PDF 146).
--
--   Then the considered packets are processed in order, the TPer's state
--   threaded. A control packet's calls go to the Session Manager ('answer'),
--   in order; an unrecognized invocation outside a regular session is
--   ignored, no response sent (3.2.4.2, PDF 49), as is one with status 0
--   whose parameters are not its method's: a parameter-shape error read as a
--   streaming violation on the Session Manager (Opal 2.30 3.3.4.1.3, PDF 31;
--   the lead's answer 8, design 13.30 (8); ETC-08 accepts the discard, PDF
--   81). The answers, each in a data subpacket of its own, are packed into
--   control session packets within the host's MaxSubpackets and MaxPacketSize
--   ("Hsed.Sim.Session".'packets'), SeqNumber, AckType and Acknowledgement 0
--   (5.2.2.4.1.9-10, PDF 146-147). An answer that fits no packet of the
--   host's, or no ComPacket, is answered instead with an empty list and
--   RESPONSE_OVERFLOW, and the processing of the payload ends there: the
--   calls after it are not run and no later packet is processed (Opal 2.30
--   3.3.1 item 1, PDF 29, over Core 3.3.10.7 d, PDF 87, which has the TPer
--   continue: the SSC refines Core); where even that answer would not fit (no
--   host within Opal 2.30 Table 17's minima, PDF 33), the payload is
--   discarded with the reason, nothing staged that no IF-RECV could carry. A
--   regular session's packet goes to "Hsed.Sim.Session": one over the TPer's
--   MaxSubpackets or MaxPacketSize aborts the session (5.2.2.4.1.1-2), else
--   each data subpacket's atoms are read as above, a token that does not read
--   aborting the session with the results completed before it (3.2.2.4.1 item
--   1), and its items as a regular session reads them
--   ('Stream.SessionStream') go to 'sessionPayload'; the subpackets after one
--   that closes or aborts the session, or whose response overflowed (which
--   ends the payload, 'Processed'), are not read, and a later packet of a
--   session a packet before it closed is ignored.
--
--   The outcome: 'Prepared', the response packets of every packet
--   processed, in order; 'SessionAborted' where a session's packet aborted
--   it, with the packets prepared before and its CloseSession where the run
--   prepares one, the packets after it not processed; or 'Discarded' where
--   nothing was answered (3.2.4.2; no action, 3.3.10.6 S2:S1 d), the TPer as
--   it was.
classifyIfSend :: Word16 -> ByteString -> TPerState -> (Outcome, TPerState)
classifyIfSend c bytes st = case Map.lookup c (tsComIds st) of
  Nothing -> (Discarded ("no static ComID " <> hex c), st)
  Just cs -> case decodeComPacket bytes of
    Left e -> discard ("not a ComPacket (Core 3.3.10.7 a, PDF 86): " <> tshow e)
    Right (DecodedFrame (ComPacketFrame h ps) _ _)
      | comPacketComID h /= c ->
          discard ("a ComPacket of ComID " <> tshow (comPacketComID h) <> " sent on " <> tshow c <> " (Core 3.3.10.7 a, PDF 86)")
      | comPacketComIDExtension h /= staticExtension ->
          discard ("an invalid Extended ComID: extension " <> tshow (comPacketComIDExtension h)
                   <> ", a static ComID's being 0x0000 (Core 3.3.3.1, PDF 57-58)")
      | null ps -> discard "no packets: no action (Core 3.3.10.6 S2:S1 d, PDF 85-86)"
      | otherwise -> either discard (run (comIdHostLimits p0 (cHostProps cs)) st [] []) $
          traverse resolved (take (lPackets tper) ps) >>= traverse prepare
  where
    p0   = tsParams st
    tper = tperLimits p0
    discard why = (Discarded why, st)
    policy = Stream.tperPolicy (tperIndTokenSize p0)
    -- the session a packet names: the control session (Nothing) or an open regular session; any other discards the payload
    resolved pkt
      | controlSession (pfHeader pkt) = Right (Nothing, pkt)
      | otherwise = maybe (Left "a packet names a session the TPer does not have (Core 3.3.10.7 a, PDF 86)")
                          (\ses -> Right (Just ses, pkt))
                          (resolveSession c (packetTSN (pfHeader pkt)) (packetHSN (pfHeader pkt)) st)
    -- a control packet's calls, all read before any is answered, the packet within the TPer's limits (else the whole
    -- payload is discarded, Core 3.3.10.7 a); a regular session's packet, processed later in order
    prepare = \case
      (Just ses, pkt) -> Right (SessionPacket ses pkt)
      (Nothing, pkt)  -> case overLimits tper pkt of
        Just why -> Left (why <> ": the packet discarded and ignored, with the payload it came in (Core 5.2.2.4.1.1-2, PDF 145; "
                          <> "3.3.10.7 a, PDF 86)")
        Nothing  -> ControlCalls . concat <$> traverse (calls . dataSubPacketPayload) (filter isData (pfSubPackets pkt))
    -- a control session subpacket's invocations, as far as the TPer's MaxMethods; Left, a streaming error on the Session Manager
    calls payload = do
      as <- first (\(off, why) -> "at byte " <> tshow off <> " of a control session subpacket, " <> why <> onSessionManager)
                  (Stream.atoms policy payload)
      let (items, fault) = Stream.items Stream.ControlStream (map Stream.atomToken as)
      traverse_ (\f -> Left ("at token " <> tshow (Stream.faultAt f) <> " of a control session subpacket, " <> Stream.faultWhy f <> onSessionManager)) fault
      take (lMethods tper) <$> traverse invocation items
    invocation = \case
      Stream.Invoke mc -> Right mc
      Stream.BadCall f -> Left ("a method invocation that does not read on the control session, " <> Stream.faultWhy f <> onSessionManager)
      other            -> Left ("the control session does not read " <> tshow other <> onSessionManager)
    onSessionManager = ": a streaming error on the Session Manager, the payload discarded (Opal 2.30 3.3.4.1.3, PDF 31)"
    -- the considered packets processed in order, the state threaded, the response packets gathered, until the payload's
    -- processing ends at an overflow or an abort; nothing gathered is Discarded with the TPer as it was, for the first
    -- reason found
    run host st' acc whys = \case
      []
        | null acc  -> discard (fromMaybe "no action (Core 3.3.10.6 S2:S1 d, PDF 85-86)" (listToMaybe (reverse whys)))
        | otherwise -> (Prepared acc, st')
      ControlCalls mcs : rest -> case answering host st' mcs of
        Left why -> discard why
        Right (st'', [], _) ->
          let why | null mcs  = "no invocation in any subpacket: no action (Core 3.3.10.6 S2:S1 d, PDF 85-86)"
                  | otherwise = "no call the Session Manager answers: ignored, no response sent (Core 3.2.4.2, PDF 49)"
          in run host st'' acc (why : whys) rest
        Right (st'', answers, ends) -> case packets host controlHeader (map generate answers) of
          Left why -> discard why
          Right packets'
            | ends      -> (Prepared (acc <> packets'), st'')
            | otherwise -> run host st'' (acc <> packets') whys rest
      SessionPacket ses pkt : rest
        | isNothing (resolveSession c (sesTSN ses) (sesHSN ses) st') ->
            run host st' acc ("a packet names a session closed by a packet before it in the payload: ignored (Core 3.3.10.7 a, PDF 86)" : whys) rest
        | otherwise -> case regular ses pkt st' of
            Processed (Prepared []) st'' _ -> run host st'' acc ("no action in a regular session's packet (Core 3.3.10.6 S2:S1 d, PDF 85-86)" : whys) rest
            Processed (Prepared packets') st'' ends
              | ends      -> (Prepared (acc <> packets'), st'')
              | otherwise -> run host st'' (acc <> packets') whys rest
            Processed (SessionAborted results closing) st'' _ -> (SessionAborted (acc <> results) closing, st'')
            Processed (Discarded why) st'' _ -> run host st'' acc (why : whys) rest
    -- a control packet's calls answered in order, the state threaded, each answer checked to fit the host as it is
    -- made: one that does not is replaced by its RESPONSE_OVERFLOW answer and ends the payload's processing, the calls
    -- after it not run (Opal 2.30 3.3.1 item 1, PDF 29; the overflowed call's own state change stands, the method having
    -- run); an overflow answer that fits nothing discards the payload (Left)
    answering host = go []
      where
        go acc s = \case
          []        -> Right (s, reverse acc, False)
          mc : more -> case answer c mc s of
            (Nothing, s') -> go acc s' more
            (Just a, s')
              | fits host controlHeader (generate a)              -> go (a : acc) s' more
              | fits host controlHeader (generate (overflowed a)) -> Right (s', reverse (overflowed a : acc), True)
              | otherwise -> Left ("the host's ComPacket of " <> tshow (lComPacketSize host) <> " bytes, or its packet of "
                                   <> tshow (lPacketSize host) <> ", cannot carry even RESPONSE_OVERFLOW's answer "
                                   <> "(Opal 2.30 3.3.1, PDF 29): discarded, nothing staged that no IF-RECV could carry")
    overflowed (MethodCall hdr _ _) = MethodCall hdr (Parameters [] []) (statusValue RESPONSE_OVERFLOW)
    -- a regular session's packet: over the TPer's limits, aborted; else its data subpackets read and processed one by
    -- one until the session closes or aborts, or the payload's processing ends at an overflow
    regular ses pkt st1 = case overLimits tper pkt of
      Just why -> sessionPayload ses [] (Just (Stream.Fault 0 Stream.OutsideCall (why <> ": the session aborted (Core 5.2.2.4.1.1-2, PDF 145)"))) st1
      Nothing  -> subpackets ses st1 [] (map dataSubPacketPayload (filter isData (pfSubPackets pkt)))
    subpackets ses st1 acc = \case
      []             -> Processed (Prepared acc) st1 False
      payload : rest -> case Stream.atoms policy payload of
        Left (off, why) -> continued ses acc rest (sessionPayload ses [] (Just (Stream.Fault 0 Stream.OutsideCall ("at byte " <> tshow off <> ", " <> why))) st1)
        Right as        -> continued ses acc rest (uncurry (sessionPayload ses) (Stream.items Stream.SessionStream (map Stream.atomToken as)) st1)
    continued ses acc rest (Processed outcome st1 ends) = case outcome of
      Prepared ps
        | ends                                                    -> Processed (Prepared (acc <> ps)) st1 True
        | isJust (resolveSession c (sesTSN ses) (sesHSN ses) st1) -> subpackets ses st1 (acc <> ps) rest
        | otherwise                                               -> Processed (Prepared (acc <> ps)) st1 False
      SessionAborted results closing -> Processed (SessionAborted (acc <> results) closing) st1 True
      Discarded why                  -> Processed (Discarded why) st1 False

-- | A considered packet, ready to process: a control packet's calls, all
--   read; a regular session's packet, to read with its session.
data Work
  = ControlCalls ![MethodCall]
  | SessionPacket !Session !PacketFrame

-- | An IF-RECV of a transfer length on a static ComID in a state: the
--   answer, exactly that many bytes, and the state after it, within what
--   the host takes of a ComPacket, the properties the ComID keeps of it
--   ('comIdHostLimits'). The answer is a ComPacket of the ComID and its
--   extension, its header the fields of the Table 45 row the state selects
--   (3.3.10.5, PDF 84-85), its payload the packets returned, zero-padded:
--
--   * S1: All Response(s) returned, no further data; S1 stays;
--   * S2: Response(s) to come, none available; S2 stays;
--   * S3, the transfer, or the host's ComPacket, shorter than the first
--     packet staged and the ComPacket header: Response ready, insufficient
--     transfer length; S3 stays;
--   * S3 otherwise: the packets staged that fit, in order, as many as the
--     host's MaxPackets (Core 5.2.2.4.1.3-4, PDF 145-146: the TPer SHALL NOT
--     send more packets, or a larger ComPacket, than the host takes); if all,
--     Response, all Response(s) returned, then S1 (3.3.10.6 S3:S1 c, PDF
--     86); else Response, additional Response(s) available, S3 with the rest
--     (S3:S3 c).
--
--   Table 45's fourth row, more to come and none available, is no state's:
--   a command's whole response is prepared at once. A row this code cannot
--   evaluate is answered 'Unsupported', the state unchanged.
ifRecvStatic :: SimParams -> Word16 -> Word32 -> ComIdState -> (Response, ComIdState)
ifRecvStatic p c len cs = case cSync cs of
  S1   -> reply AllReturnedNoData (Delivery [] []) cs
  S2 _ -> reply ResponsesToCome (Delivery [] []) cs
  S3 staged
    | null staged                                -> reply AllReturnedNoData (Delivery [] []) cs { cSync = S1 }
    | room < maybe 0 B.length (listToMaybe staged) -> reply InsufficientLength (Delivery staged []) cs
    | null rest                                  -> reply AllReturnedWithData (Delivery staged sent) cs { cSync = S1 }
    | otherwise                                  -> reply AdditionalAvailable (Delivery staged sent) cs { cSync = S3 rest }
    where
      -- the bytes of packets the answer can carry: the transfer's, and the host's ComPacket's, less the header
      room         = min (fromIntegral len) (lComPacketSize host) - comPacketHeaderSize
      fitting      = length (takeWhile (<= room) (L.scanl1 (+) (map B.length staged)))
      (sent, rest) = L.splitAt (min (lPackets host) fitting) staged
  where
    host = comIdHostLimits p (cHostProps cs)
    reply row d after = either (\why -> (Response (Unsupported why) mempty, cs)) (\bytes -> (Response Good (exactly len bytes), after)) $ do
      (l, od, mt) <- t45Fields row d
      let returned = mconcat (dlReturned d)
      unless (fromIntegral l == B.length returned) $
        Left ("Core Table 45's row " <> tshow row <> " sets Length " <> tshow l <> " where " <> tshow (B.length returned) <> " bytes are returned")
      first (("the answer does not frame: " <>) . tshow) (encodeRecord (ComPacket (ComPacketHeader c (cExt cs) od mt) returned))


-- * The parts of an IF-SEND's payload

-- | Whether a packet is the control session's: Session 0, its TSN and HSN
--   zero (Core 3.3.7.1.2, PDF 73).
controlSession :: PacketHeader -> Bool
controlSession h = packetTSN h == 0 && packetHSN h == 0

-- | Why a packet breaks the TPer's limits, if it does: more subpackets than
--   its MaxSubpackets, or more bytes with its header than its MaxPacketSize
--   (Core 5.2.2.4.1.1-2, PDF 145), which discards a control session packet
--   and aborts a regular session.
overLimits :: Limits -> PacketFrame -> Maybe Text
overLimits tper pkt
  | length (pfSubPackets pkt) > lSubpackets tper =
      Just ("a packet of " <> tshow (length (pfSubPackets pkt)) <> " subpackets, more than the TPer's MaxSubpackets " <> tshow (lSubpackets tper))
  | either (const True) ((> lPacketSize tper) . B.length) (encodePacket pkt) =
      Just ("a packet of more bytes than the TPer's MaxPacketSize " <> tshow (lPacketSize tper))
  | otherwise = Nothing

-- | Whether a subpacket is a data subpacket, by its Kind (Core Table 19).
isData :: DataSubPacket -> Bool
isData sp = any (\r -> inSpan (codeSpan r) (toInteger (dataSubPacketKind (dataSubPacketHeader sp)))) (codeRowsNamed cSubpacketTypes "Data")
