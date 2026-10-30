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
until released, then awaiting IF-RECV (S3); or the payload discarded, the
ComID kept in S1 (design 8.2). An IF-SEND in S2 or S3 is a synchronous
protocol violation, the state unchanged. An IF-RECV is answered in each
state with the ComPacket header fields Core Table 45 gives (PDF 83,
'ifRecvStatic').

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
    -- * Reading a control session's data subpacket
  , controlInvocations
  ) where

import qualified RIO.ByteString            as B
import qualified RIO.List                  as L
import           RIO

import           Hsed.Core.Call            (MethodCall (..), Parameters (..), statusList)
import           Hsed.Core.Code            (CodeRow (..), codeRowsNamed, inSpan)
import           Hsed.Core.Record          (encodeRecord)
import           Hsed.Core.StreamItem      (StreamItem (..))
import           Hsed.Core.Sync            (Setting (..), StateTable (..), settingOf)
import           Hsed.Core.Token           (Token (..))
import           Hsed.Gen.Core.Codes       (StatusCode (..), cSubpacketTypes, pattern RESPONSE_OVERFLOW)
import           Hsed.Gen.Core.Layouts     (ComPacket (..), ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..),
                                            PacketHeader (..))
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Packet               (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, encodePacket)
import           Hsed.Sim.Types            (ComIdState (..), Limits (..), Outcome (..), SyncState (..))
import           Hsed.Wire                 (IfStatus (..), Response (..), comPacketHeaderSize, dataKind, exactly,
                                            staticExtension)


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

-- | What an IF-SEND accepted in S1 on a ComID comes to, given what the TPer
--   and the host each take in a ComPacket ("Hsed.Sim.Types".'Limits') and how
--   the Session Manager answers a control session's method call: its response
--   packets prepared, or the payload discarded and why. Discarded:
--
--   * what is no ComPacket of the ComID, or names an extension other than a
--     static ComID's 0x0000 (Core 3.3.3.1, PDF 57-58; 3.3.10.7 a, PDF 86);
--   * a ComPacket with no packets: no action (3.3.10.6 S2:S1 d, PDF 85-86);
--   * a packet that names a session (any but the control session, Session 0,
--     3.3.7.1.2, PDF 73), since the TPer has none before M4a: the session
--     cannot be resolved (3.3.10.7 a, PDF 86);
--   * a control session packet of more subpackets than the TPer's
--     MaxSubpackets, or of more bytes than its MaxPacketSize, which the TPer
--     SHALL discard and ignore (5.2.2.4.1.1-2, PDF 145; 3.3.10.7 a, a
--     violation of packet requirements); packets past the TPer's MaxPackets
--     are ignored, as it MAY (5.2.2.4.1.3, PDF 145-146);
--   * packets whose data subpackets have no payload or Empty atoms alone: no
--     action (3.3.10.6 S2:S1 d); a subpacket of another Kind than data
--     (Table 19, PDF 43) is no data;
--   * tokens that are no method invocations: a token that does not read, one
--     outside an invocation, a Call inside one, an invocation cut short, or a
--     status list of other than three or more uintegers (Core 3.2.2.4, PDF
--     38-39), an error on the Session Manager whose payload the TPer SHALL
--     discard (Opal 2.30 3.3.4.1.3, PDF 31);
--   * method calls none of which the Session Manager answers: an
--     unrecognized invocation outside a regular session is ignored, no
--     response sent (3.2.4.2, PDF 49), as is one whose header or parameters
--     do not read (3.2.2.4.2 items 3-4). At M3 the Session Manager answers
--     Properties alone.
--
--   The tokens are read as a control session reads them (3.3.7.1.3, PDF 73):
--   Empty atoms dropped wherever they are (3.2.2.3.1.5, Table 14, PDF 36),
--   End of Session and the Transaction Control tokens, each with the status
--   after it, ignored; the invocations of a subpacket past the TPer's
--   MaxMethods ignored, as it MAY (5.2.2.4.1.7, PDF 146).
--
--   The answers, in the order called, each in a response packet of its own
--   on the control session, one data subpacket of its tokens: the host's
--   MaxMethods, MaxSubpackets and MaxPackets are one until its properties are
--   kept (M4a). An answer that with its packet does not fit the host's
--   MaxPacketSize or its ComPacket is answered instead with an empty list and
--   RESPONSE_OVERFLOW, and nothing after it (Opal 2.30 3.3.1, PDF 29).
classifyIfSend :: Limits -> Limits -> (MethodCall -> Maybe MethodCall) -> Word16 -> ByteString -> Outcome
classifyIfSend tper host respond c bytes = case decodeComPacket bytes of
  Left e -> Discarded ("not a ComPacket (Core 3.3.10.7 a, PDF 86): " <> tshow e)
  Right (DecodedFrame (ComPacketFrame h ps) _ _)
    | comPacketComID h /= c ->
        Discarded ("a ComPacket of ComID " <> tshow (comPacketComID h) <> " sent on " <> tshow c <> " (Core 3.3.10.7 a, PDF 86)")
    | comPacketComIDExtension h /= staticExtension ->
        Discarded ("an invalid Extended ComID: extension " <> tshow (comPacketComIDExtension h)
                   <> ", a static ComID's being 0x0000 (Core 3.3.3.1, PDF 57-58)")
    | null ps -> Discarded "no packets: no action (Core 3.3.10.6 S2:S1 d, PDF 85-86)"
    | any (not . controlSession . pfHeader) ps ->
        Discarded "a packet names a session the TPer does not have (Core 3.3.10.7 a, PDF 86)"
    | otherwise -> either Discarded Prepared (asked (take (lPackets tper) ps) >>= answered)
  where
    -- the calls of the packets the TPer reads, those it answers answered
    asked considered = do
      let kept = filter (isNothing . violation) considered
      when (null kept) $ Left (fromMaybe "no packet the TPer reads" (listToMaybe (mapMaybe violation considered)))
      case filter (not . noAction) [ dataSubPacketPayload sp | p <- kept, sp <- pfSubPackets p, isData sp ] of
        []       -> Left "no subpacket with a payload of more than Empty atoms: no action (Core 3.3.10.6 S2:S1 d, PDF 85-86)"
        payloads -> do
          calls <- concat <$> traverse (fmap (take (lMethods tper)) . controlInvocations) payloads
          case mapMaybe (>>= respond) calls of
            []      -> Left "no call the Session Manager answers: ignored, no response sent (Core 3.2.4.2, PDF 49)"
            answers -> Right answers
    -- why the TPer discards and ignores a control session packet, if it does
    violation p
      | length (pfSubPackets p) > lSubpackets tper =
          Just ("a control session packet of " <> tshow (length (pfSubPackets p)) <> " subpackets, more than the TPer's MaxSubpackets "
                <> tshow (lSubpackets tper) <> ": discarded and ignored (Core 5.2.2.4.1.1, PDF 145)")
      | either (const True) ((> lPacketSize tper) . B.length) (encodePacket p) =
          Just ("a control session packet of more bytes than the TPer's MaxPacketSize " <> tshow (lPacketSize tper)
                <> ": discarded and ignored (Core 5.2.2.4.1.2, PDF 145)")
      | otherwise = Nothing
    -- each answer a response packet; the first that does not fit replaced by its overflow, nothing after it
    answered answers = traverse framed (fitting answers)
    fitting = \case
      []       -> []
      a : rest
        | fits a    -> a : fitting rest
        | otherwise -> [overflowed a]
    fits a = either (const False) (\p -> B.length p <= lPacketSize host && comPacketHeaderSize + B.length p <= lComPacketSize host) (framed a)
    framed a = first (("the response does not frame: " <>) . tshow)
                     (encodePacket (PacketFrame controlHeader [DataSubPacket (DataSubPacketHeader dataKind) (generate a)]))
    overflowed (MethodCall hdr _ _) = MethodCall hdr (Parameters [] []) responseOverflow

-- | An IF-RECV of a transfer length on a static ComID in a state, given what
--   the host takes in a ComPacket: the answer, exactly that many bytes, and
--   the state after it. The answer is a ComPacket of the ComID and its
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
--   Table 45's fourth row, more to come and none available, is no state's at
--   M3: the Session Manager prepares a command's whole response at once. A
--   row this code cannot evaluate is answered 'Unsupported', the state
--   unchanged.
ifRecvStatic :: Limits -> Word16 -> Word32 -> ComIdState -> (Response, ComIdState)
ifRecvStatic host c len cs = case cSync cs of
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
      fits         = length (takeWhile (<= room) (L.scanl1 (+) (map B.length staged)))
      (sent, rest) = L.splitAt (min (lPackets host) fits) staged
  where
    reply row d after = either (\why -> (Response (Unsupported why) mempty, cs)) (\bytes -> (Response Good (exactly len bytes), after)) $ do
      (l, od, mt) <- t45Fields row d
      let returned = mconcat (dlReturned d)
      unless (fromIntegral l == B.length returned) $
        Left ("Core Table 45's row " <> tshow row <> " sets Length " <> tshow l <> " where " <> tshow (B.length returned) <> " bytes are returned")
      first (("the answer does not frame: " <>) . tshow) (encodeRecord (ComPacket (ComPacketHeader c (cExt cs) od mt) returned))


-- * Reading a control session's data subpacket

-- | The method invocations a control session's data subpacket makes, in
--   order: each a call where its header and parameters read exactly as its
--   tokens are, its status the host's first value (the reserved ones
--   ignored, Core 3.2.4.2 item 2), or 'Nothing', an invocation the control
--   session ignores (3.2.2.4.2 items 3-4); or why the tokens are no
--   invocations at all, which discards the payload ('classifyIfSend').
--
--   Empty atoms are dropped first, then End of Session and the Transaction
--   Control tokens, each with the uinteger status after it, which the TPer
--   SHALL ignore on the control session (3.3.7.1.3, PDF 73). Then the stream
--   is invocations alone, each a Call, the tokens to the first End of Data
--   (no Call among them, 3.2.2.3.4 a), and a status list of three or more
--   uintegers (3.2.4.2 item 5); a token anywhere else stops it (3.2.2.4.2
--   items 2 and 5).
controlInvocations :: ByteString -> Either Text [Maybe MethodCall]
controlInvocations bytes = do
  as <- maybe (Left "a token that does not read (Core 3.2.2.4.1, PDF 38)") Right (atoms bytes)
  invocationsOf (ignored (filter ((/= Empty) . snd) as))
  where
    ignored = \case
      (_, EndOfSession) : rest                                      -> ignored rest
      (_, t) : (_, Unsigned _) : rest | t `elem` transactionControl -> ignored rest
      (_, t) : rest | t `elem` transactionControl                   -> ignored rest
      a : rest                                                      -> a : ignored rest
      []                                                            -> []
    transactionControl = [StartTransaction, EndTransaction]
    invocationsOf = \case
      [] -> Right []
      call@(_, Call) : rest -> do
        let (body, after) = L.break ((== EndOfData) . snd) rest
        when (any ((== Call) . snd) body) $ Left "a Call inside a method invocation (Core 3.2.2.3.4 a, PDF 38)"
        (eod, afterEod) <- maybe (Left "a method invocation with no End of Data (Core 3.2.4.2, PDF 49)") Right (L.uncons after)
        (first', rest') <- statusAfter afterEod
        (readCall (call : body <> [eod]) first' :) <$> invocationsOf rest'
      (_, t) : _ -> Left ("a token outside a method invocation, " <> tshow t <> " (Core 3.2.2.4.2 item 2, PDF 39)")
    -- the status list after End of Data: its first value, and the tokens after it
    statusAfter = \case
      (_, StartList) : rest -> case L.break ((== EndList) . snd) rest of
        (values, _ : rest')
          | length values >= 3, Just (v : _) <- traverse (uinteger . snd) values -> Right (v, rest')
          | otherwise -> Left "a status list of other than three or more uintegers (Core 3.2.2.4.2 item 5, PDF 39)"
        (_, []) -> Left "a status list with no End List (Core 3.2.4.2, PDF 49)"
      _ -> Left "a method invocation with no status list after its End of Data (Core 3.2.4.2, PDF 49)"
    uinteger = \case
      Unsigned n -> Just n
      _          -> Nothing
    -- a call read exactly as its tokens are, to its End of Data
    readCall toks sc = case parseByteString (foldMap fst toks <> generate (statusList sc)) of
      Right mc | fmap (map snd) (atoms (toEndOfData mc)) == Just (map snd toks) -> Just mc
      _                                                                        -> Nothing
    toEndOfData mc = generate Call <> generate (_header mc) <> generate (_parameters mc) <> generate EndOfData


-- * The parts of an IF-SEND's payload

-- | Whether a packet is the control session's: Session 0, its TSN and HSN
--   zero (Core 3.3.7.1.2, PDF 73).
controlSession :: PacketHeader -> Bool
controlSession h = packetTSN h == 0 && packetHSN h == 0

-- | The header of a response packet on the control session: Session 0, no
--   sequence number and no acknowledgement (the synchronous protocol uses
--   neither).
controlHeader :: PacketHeader
controlHeader = PacketHeader 0 0 0 0 0

-- | Whether a subpacket is a data subpacket, by its Kind (Core Table 19).
isData :: DataSubPacket -> Bool
isData sp = any (\r -> inSpan (codeSpan r) (toInteger (dataSubPacketKind (dataSubPacketHeader sp)))) (codeRowsNamed cSubpacketTypes "Data")

-- | Whether a data subpacket asks nothing: no payload, or Empty atoms alone
--   (Core 3.3.10.6 S2:S1 d, PDF 85-86).
noAction :: ByteString -> Bool
noAction = maybe False (all ((== Empty) . snd)) . atoms

-- | The status of a method response that does not fit the response buffer
--   (Core Table 166; Opal 2.30 3.3.1, PDF 29).
responseOverflow :: Natural
responseOverflow = case RESPONSE_OVERFLOW of StatusCode n -> n

-- | The atoms of a token stream in order, each its bytes and its token, or
--   Nothing where an atom does not read (Core 3.2.2.3.1, PDF 33-36).
--   An atom is the shortest start of the stream that reads as a token: its
--   header says how many bytes it has, and the codec reads that many.
atoms :: ByteString -> Maybe [(ByteString, Token)]
atoms stream
  | B.null stream = Just []
  | otherwise     = do
      (raw, t) <- listToMaybe [ (raw, t) | n <- [1 .. B.length stream], let raw = B.take n stream, Right t <- [readToken raw] ]
      ((raw, t) :) <$> atoms (B.drop (B.length raw) stream)
  where
    readToken :: ByteString -> Either String Token
    readToken = parseByteString
