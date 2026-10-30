{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Packet
Description : ComPacket, Packet and Data SubPacket framing over the generated layouts
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The framing of design section 4.1's @Hsed.Core.Packet@ (milestone M2c): what
an interface command carries is a ComPacket (Core 2.01 Table 17, section
3.2.3.2), whose payload is a sequence of packets (Table 18, 3.2.3.3), whose
payload is a sequence of subpackets (Table 20, 3.2.3.4.1), each a header and
its data padded to a multiple of four bytes. Every byte position, size,
length field and the padding come from the layouts the @byteGrid@ rule
generates from those three tables ("Hsed.Gen.Core.Layouts": 'ComPacket',
'Packet', 'DataSubPacket'), through their "Hsed.Core.Record" instances; this
module only nests them: a ComPacket's payload is its packets encoded one
after another, a packet's its subpackets. Each Length field is computed from
what follows it when encoding, and read to find it when decoding; the
subpacket data stays bytes (the token stream, which the codec reads).

It lives in hsed, not hsed-core, because it imports the generated layouts,
which hsed holds (@gen/@) and hsed-core cannot depend on.

Decoding is lenient, as a host reads what a TPer sent: reserved bits and a
subpacket's pad bytes (which SHALL be zeroes, Core 3.2.3.4.1.2.2) read other
than zero are reported ('dfDifferences') and decoding goes on; a frame that
does not fit its bytes (a length past the end) is an error. There is no
strict path: Tables 17, 18 and 20 pin nothing, so a strict decode (which
refuses only a pin read otherwise, "Hsed.Core.Layout".'decodeStrict') would
equal this one. What follows the ComPacket (the padding of the transfer) is
returned as it is. Every
subpacket is read as a data subpacket (Table 20): Table 20's Kind is no
constant of its layout (3.2.3.4.1.1.2 says it "is set to zeroes", in words
none of the pin schemas reads, design 13.26), so a credit control subpacket
(Table 21, Kind 0x8001) reads as a data subpacket of that Kind, its credit
its data.
-}
module Hsed.Packet
  ( -- * Frames
    ComPacketFrame (..)
  , PacketFrame (..)
  , DecodedFrame (..)
    -- * Encoding
  , encodeComPacket
  , encodePacket
    -- * Decoding
  , decodeComPacket
  , decodePacket
  , decodeSubPacket
  ) where

import qualified RIO.ByteString        as B
import           RIO

import           Hsed.Core.Layout      (Difference, LayoutError)
import           Hsed.Core.Record      (DecodedRecord (..), decodeRecord, encodeRecord)
import           Hsed.Gen.Core.Layouts (ComPacket (..), ComPacketHeader, DataSubPacket (..), Packet (..), PacketHeader)


-- * Frames

-- | A ComPacket and the packets its payload holds.
data ComPacketFrame = ComPacketFrame
  { cpfHeader  :: !ComPacketHeader
  , cpfPackets :: ![PacketFrame]
  }
  deriving (Eq, Show)

-- | A packet and the subpackets its payload holds, each its header (its
--   Kind) and its data.
data PacketFrame = PacketFrame
  { pfHeader     :: !PacketHeader
  , pfSubPackets :: ![DataSubPacket]
  }
  deriving (Eq, Show)

-- | A frame decoded from the start of the input: the frame, what its bytes
--   held other than its layouts' reserved bits and pins ask, at every level,
--   and the input after it.
data DecodedFrame a = DecodedFrame
  { dfFrame       :: !a
  , dfDifferences :: ![Difference]
  , dfRest        :: !ByteString
  }
  deriving (Eq, Show)


-- * Encoding

-- | A ComPacket's bytes: its packets encoded one after another as its
--   payload, its Length their total.
encodeComPacket :: ComPacketFrame -> Either LayoutError ByteString
encodeComPacket (ComPacketFrame h ps) = encodeRecord . ComPacket h . mconcat =<< traverse encodePacket ps

-- | A packet's bytes: its subpackets, each padded, one after another as its
--   payload, its Length their total; each subpacket's Length its data's.
encodePacket :: PacketFrame -> Either LayoutError ByteString
encodePacket (PacketFrame h sps) = encodeRecord . Packet h . mconcat =<< traverse encodeRecord sps


-- * Decoding

-- | A ComPacket from the start of the input, its payload read as packets to
--   its end.
decodeComPacket :: ByteString -> Either LayoutError (DecodedFrame ComPacketFrame)
decodeComPacket input = do
  DecodedRecord (ComPacket h payload) ds rest <- decodeRecord input
  (packets, ds') <- everyOne decodePacket payload
  pure (DecodedFrame (ComPacketFrame h packets) (ds <> ds') rest)

-- | A packet from the start of the input, its payload read as subpackets to
--   its end.
decodePacket :: ByteString -> Either LayoutError (DecodedFrame PacketFrame)
decodePacket input = do
  DecodedRecord (Packet h payload) ds rest <- decodeRecord input
  (subPackets, ds') <- everyOne decodeSubPacket payload
  pure (DecodedFrame (PacketFrame h subPackets) (ds <> ds') rest)

-- | A subpacket from the start of the input, its padding skipped.
decodeSubPacket :: ByteString -> Either LayoutError (DecodedFrame DataSubPacket)
decodeSubPacket input = (\(DecodedRecord sp ds rest) -> DecodedFrame sp ds rest) <$> decodeRecord input

-- | Every frame a payload holds, one after another to its end (each layout
--   has a fixed part, so each step consumes bytes).
everyOne :: (ByteString -> Either LayoutError (DecodedFrame a)) -> ByteString -> Either LayoutError ([a], [Difference])
everyOne dec = go [] []
  where
    go frames ds bytes
      | B.null bytes = Right (reverse frames, ds)
      | otherwise    = dec bytes >>= \(DecodedFrame f ds' rest) -> go (f : frames) (ds <> ds') rest
