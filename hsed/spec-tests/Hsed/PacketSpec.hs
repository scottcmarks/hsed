{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.PacketSpec
Description : Test specifications for Hsed.Packet
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ComPacket, Packet and Data SubPacket framing over the generated layouts of
Core 2.01 Tables 17, 18 and 20: random frames decode to themselves; each
Length field is what follows it (a subpacket's its data's, without the
padding, Table 20's "If Length > 0, 12 to (n+(-n modulo 4)) + 11", PDF 44); a
subpacket is padded to a multiple of four bytes; what follows the ComPacket is
kept; one frame's bytes written out from the three grids, the packet's
Session as its TSN and HSN (Table 18's @Session (0-3 = TSN, 4-7 = HSN)@);
reserved bytes and a subpacket's padding (§3.2.3.4.1.2.2: its pad bytes SHALL
be zeroes) read other than zero reported; a Length past the end refused.
-}
module Hsed.PacketSpec (spec) where

import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Hsed.Core.Layout      (Content (..), Decoded (..), Difference (..), Layout (..), LayoutError (..), decode)
import           Hsed.Core.Record      (LayoutRecord (..), encodeRecord)
import           Hsed.Gen.Core.Layouts
import           Hsed.Packet


-- * Random frames

payload :: Gen ByteString
payload = B.pack <$> (chooseInt (0, 23) >>= vector)

subPacket :: Gen DataSubPacket
subPacket = DataSubPacket <$> (DataSubPacketHeader <$> arbitrary) <*> payload

packetFrame :: Gen PacketFrame
packetFrame = PacketFrame
  <$> (PacketHeader <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
  <*> (chooseInt (0, 3) >>= (`vectorOf` subPacket))

comPacketFrame :: Gen ComPacketFrame
comPacketFrame = ComPacketFrame
  <$> (ComPacketHeader <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
  <*> (chooseInt (0, 3) >>= (`vectorOf` packetFrame))

-- | A frame's bytes; a test failure if it does not encode.
encoded :: (a -> Either LayoutError ByteString) -> a -> IO ByteString
encoded enc = either (throwString . show) pure . enc

-- | The value of a layout's Length field in the bytes.
lengthIn :: Decoded -> Maybe Content
lengthIn = L.lookup "Length" . decodedFields

spec :: Spec
spec = describe "Hsed.Packet" $ do
  it "frames with the generated layouts of Tables 17, 18 and 20 (PDF 40, 41, 44), through their records" $ do
    map layoutName [ recordLayout (Proxy :: Proxy ComPacket), recordLayout (Proxy :: Proxy Packet), recordLayout (Proxy :: Proxy DataSubPacket) ]
      `shouldBe` ["ComPacket Format", "Packet Format", "Data SubPacket Format"]
    [ recordLayout (Proxy :: Proxy ComPacket), recordLayout (Proxy :: Proxy Packet), recordLayout (Proxy :: Proxy DataSubPacket) ]
      `shouldBe` [comPacketLayout, packetLayout, dataSubPacketLayout]
    map layoutSize [comPacketLayout, packetLayout, dataSubPacketLayout] `shouldBe` [20, 24, 12]
  prop "decodes the ComPacket it encoded, every level, nothing left over" $ forAll comPacketFrame $ \f -> do
    bytes <- encoded encodeComPacket f
    decodeComPacket bytes `shouldBe` Right (DecodedFrame f [] "")
  prop "keeps what follows the ComPacket (a transfer's padding)" $ forAll comPacketFrame $ \f -> do
    bytes <- encoded encodeComPacket f
    dfRest <$> decodeComPacket (bytes <> B.replicate 7 0) `shouldBe` Right (B.replicate 7 0)
  prop "computes each Length: the ComPacket's and the packet's their payload's, the subpacket's its data's" $ forAll packetFrame $ \p -> do
    bytes <- encoded encodeComPacket (ComPacketFrame (ComPacketHeader 0 0 0 0) [p])
    packetBytes <- encoded encodePacket p
    subBytes <- traverse (encoded encodeRecord) (pfSubPackets p)
    lengthIn <$> decode comPacketLayout bytes `shouldBe` Right (Just (Number (toInteger (B.length packetBytes))))
    lengthIn <$> decode packetLayout packetBytes `shouldBe` Right (Just (Number (toInteger (sum (map B.length subBytes)))))
    [ lengthIn <$> decode dataSubPacketLayout b | b <- subBytes ]
      `shouldBe` [ Right (Just (Number (toInteger (B.length (dataSubPacketPayload s))))) | s <- pfSubPackets p ]
  prop "pads each subpacket to a multiple of four bytes" $ forAll subPacket $ \s -> do
    bytes <- encoded encodeRecord s
    let n = B.length (dataSubPacketPayload s)
    (B.length bytes, B.drop (12 + n) bytes) `shouldBe` (12 + n + (negate n `mod` 4), B.replicate (negate n `mod` 4) 0)

  it "writes one ComPacket as the three grids place its bytes" $ do
    let frame = ComPacketFrame (ComPacketHeader 0x07fe 0 0 0)
                  [PacketFrame (PacketHeader 0x00000001 0x00000069 1 0 0) [DataSubPacket (DataSubPacketHeader 0) "\xf8\xa8\xf0"]]
        subBytes = B.replicate 6 0 <> "\x00\x00" <> "\x00\x00\x00\x03" <> "\xf8\xa8\xf0" <> "\x00"
        packetBytes = "\x00\x00\x00\x01\x00\x00\x00\x69" <> "\x00\x00\x00\x01" <> "\x00\x00" <> "\x00\x00" <> "\x00\x00\x00\x00"
                      <> "\x00\x00\x00\x10" <> subBytes
        comPacketBytes = B.replicate 4 0 <> "\x07\xfe" <> "\x00\x00" <> B.replicate 8 0 <> "\x00\x00\x00\x28" <> packetBytes
    encodeComPacket frame `shouldBe` Right comPacketBytes
    B.length comPacketBytes `shouldBe` 60
    decodeComPacket comPacketBytes `shouldBe` Right (DecodedFrame frame [] "")
  it "reports a subpacket's reserved bytes read other than zero, and decodes on" $ do
    let frame = ComPacketFrame (ComPacketHeader 1 0 0 0) [PacketFrame (PacketHeader 0 1 1 0 0) [DataSubPacket (DataSubPacketHeader 0) "x"]]
    bytes <- encoded encodeComPacket frame
    let altered = B.take 44 bytes <> "\x01" <> B.drop 45 bytes   -- byte 0 of the subpacket, after 20 + 24 bytes
    decodeComPacket altered `shouldBe` Right (DecodedFrame frame [ReservedNotZero "Reserved" 0 0x010000000000] "")
  it "reports a subpacket's pad bytes read other than zero, and decodes on" $ do
    let frame = ComPacketFrame (ComPacketHeader 1 0 0 0) [PacketFrame (PacketHeader 0 1 1 0 0) [DataSubPacket (DataSubPacketHeader 0) "x"]]
    bytes <- encoded encodeComPacket frame
    let altered = B.take 57 bytes <> "\x01" <> B.drop 58 bytes   -- the first pad byte: the subpacket's byte 13, after its 12 and its one of data
    decodeComPacket altered `shouldBe` Right (DecodedFrame frame [PaddingNotZero "Payload" 104 0x010000] "")
  it "refuses a ComPacket whose Length runs past the bytes" $ do
    bytes <- encoded encodeComPacket (ComPacketFrame (ComPacketHeader 1 0 0 0) [])
    decodeComPacket (B.take 16 bytes <> "\x00\x00\x00\x08") `shouldBe` Left (Truncated 28 20)
