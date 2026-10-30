{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
  ) where

import qualified RIO.ByteString            as B
import qualified RIO.List                  as L
import           RIO

import           Hsed.Core.Call            (MethodCall)
import           Hsed.Core.Code            (CodeRow (..), Span (..), codeRowsNamed, inSpan)
import           Hsed.Core.Layout          (Layout (..))
import           Hsed.Core.Record          (LayoutRecord (..), encodeRecord)
import           Hsed.Core.StreamItem      (StreamItem (..))
import           Hsed.Core.Sync            (Setting (..), StateTable (..), settingOf)
import           Hsed.Core.Token           (Token (..))
import           Hsed.Gen.Core.Codes       (cSubpacketTypes)
import           Hsed.Gen.Core.Layouts     (ComPacket (..), ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..),
                                            PacketHeader (..))
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Packet               (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, encodePacket)
import           Hsed.Sim.Types            (ComIdState (..), Outcome (..), SyncState (..))
import           Hsed.Wire                 (IfStatus (..), Response (..))


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

-- | What an IF-SEND accepted in S1 on a ComID comes to, given how the
--   Session Manager answers a control session's method call: its response
--   packets prepared, or the payload discarded and why. Discarded:
--
--   * what is no ComPacket of the ComID, or names an extension other than a
--     static ComID's 0x0000 (Core 3.3.3.1, PDF 57-58; 3.3.10.7 a, PDF 86);
--   * a ComPacket with no packets, or whose packets' data subpackets have no
--     payload or Empty atoms alone: no action (3.3.10.6 S2:S1 d, PDF 85-86); a
--     subpacket of another Kind than data (Table 19, PDF 43) is no data;
--   * a packet that names a session (any but the control session, Session 0,
--     3.3.7.1.2, PDF 73), since the TPer has none before M4a: the session
--     cannot be resolved (3.3.10.7 a, PDF 86);
--   * method calls none of which the Session Manager answers: an
--     unrecognized invocation outside a regular session is ignored, no
--     response sent (3.2.4.2, PDF 49). At M3 the Session Manager answers
--     Properties alone; a call it does not answer is discarded here, which
--     is M3's gap.
--
--   The answers, in the order called, are one response packet on the control
--   session, one data subpacket of their tokens. Empty atoms are dropped
--   wherever they are (Core 3.2.2.3.1.5, Table 14, PDF 36), and each data
--   subpacket's tokens are read as invocations, each from its Call token.
classifyIfSend :: (MethodCall -> Maybe MethodCall) -> Word16 -> ByteString -> Outcome
classifyIfSend respond c bytes = case decodeComPacket bytes of
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
    | otherwise -> case filter (not . noAction) [ dataSubPacketPayload sp | p <- ps, sp <- pfSubPackets p, isData sp ] of
        []    -> Discarded "no subpacket with a payload of more than Empty atoms: no action (Core 3.3.10.6 S2:S1 d, PDF 85-86)"
        asked -> case mapMaybe respond (concatMap invocations asked) of
          []      -> Discarded "no call the Session Manager answers: ignored, no response sent (Core 3.2.4.2, PDF 49)"
          answers -> either (Discarded . ("the response does not frame: " <>) . tshow) (Prepared . pure)
                            (encodePacket (PacketFrame controlHeader [DataSubPacket (DataSubPacketHeader dataKind) (foldMap generate answers)]))

-- | An IF-RECV of a transfer length on a static ComID in a state: the
--   answer, exactly that many bytes, and the state after it. The answer is
--   a ComPacket of the ComID and its extension, its header the fields of the
--   Table 45 row the state selects (3.3.10.5, PDF 84-85), its payload the
--   packets returned, zero-padded:
--
--   * S1: All Response(s) returned, no further data; S1 stays;
--   * S2: Response(s) to come, none available; S2 stays;
--   * S3, the transfer shorter than the first packet staged and the
--     ComPacket header: Response ready, insufficient transfer length; S3
--     stays;
--   * S3 otherwise: the packets staged that fit, in order; if all, Response,
--     all Response(s) returned, then S1 (3.3.10.6 S3:S1 c, PDF 86); else
--     Response, additional Response(s) available, S3 with the rest (S3:S3
--     c).
--
--   Table 45's fourth row, more to come and none available, is no state's at
--   M3: the Session Manager prepares a command's whole response at once. A
--   row this code cannot evaluate is answered 'Unsupported', the state
--   unchanged.
ifRecvStatic :: Word16 -> Word32 -> ComIdState -> (Response, ComIdState)
ifRecvStatic c len cs = case cSync cs of
  S1   -> reply AllReturnedNoData (Delivery [] []) cs
  S2 _ -> reply ResponsesToCome (Delivery [] []) cs
  S3 staged
    | null staged                                                   -> reply AllReturnedNoData (Delivery [] []) cs { cSync = S1 }
    | n < comPacketHeaderSize + maybe 0 B.length (listToMaybe staged) -> reply InsufficientLength (Delivery staged []) cs
    | null rest                                                     -> reply AllReturnedWithData (Delivery staged sent) cs { cSync = S1 }
    | otherwise                                                     -> reply AdditionalAvailable (Delivery staged sent) cs { cSync = S3 rest }
    where
      n            = fromIntegral len
      (sent, rest) = L.splitAt (length (takeWhile (<= n - comPacketHeaderSize) (L.scanl1 (+) (map B.length staged)))) staged
  where
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

-- | The header of a response packet on the control session: Session 0, no
--   sequence number and no acknowledgement (the synchronous protocol uses
--   neither).
controlHeader :: PacketHeader
controlHeader = PacketHeader 0 0 0 0 0

-- | The extension of a ComID the TPer does not assign dynamically, a static
--   one (Core 3.3.3.1, PDF 57).
staticExtension :: Word16
staticExtension = 0x0000

-- | The Kind a data subpacket has: Core Table 19's @Data@ (PDF 43), which
--   3.2.3.4.1.1.2 (PDF 44) says is zeroes.
dataKind :: Word16
dataKind = fromMaybe 0 (listToMaybe [ fromInteger k | CodeRow (One k) _ <- codeRowsNamed cSubpacketTypes "Data" ])

-- | Whether a subpacket is a data subpacket, by its Kind (Core Table 19).
isData :: DataSubPacket -> Bool
isData sp = any (\r -> inSpan (codeSpan r) (toInteger (dataSubPacketKind (dataSubPacketHeader sp)))) (codeRowsNamed cSubpacketTypes "Data")

-- | Whether a data subpacket asks nothing: no payload, or Empty atoms alone
--   (Core 3.3.10.6 S2:S1 d, PDF 85-86).
noAction :: ByteString -> Bool
noAction = maybe False (all ((== Empty) . snd)) . atoms

-- | The method calls a data subpacket's tokens make, Empty atoms dropped,
--   each from its Call token to the token before the next: those that read
--   as a call, all of their tokens (Core 3.2.4.2, PDF 48-49); a stream that
--   does not read as atoms makes none.
invocations :: ByteString -> [MethodCall]
invocations = maybe [] (mapMaybe call . L.groupBy (\_ a -> snd a /= Call) . filter ((/= Empty) . snd)) . atoms
  where
    call chunk = case parseByteString (foldMap fst chunk) of
      Right mc | fmap (map snd) (atoms (generate mc)) == Just (map snd chunk) -> Just mc
      _                                                                       -> Nothing

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

-- | The size of the ComPacket header, Core Table 17's fixed part (PDF 40).
comPacketHeaderSize :: Int
comPacketHeaderSize = layoutSize (recordLayout (Proxy :: Proxy ComPacket))

-- | Exactly so many bytes: cut to the length, or zero-padded to it.
exactly :: Word32 -> ByteString -> ByteString
exactly len bytes = B.take n (bytes <> B.replicate (n - B.length bytes) 0)
  where n = fromIntegral len
