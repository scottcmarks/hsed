{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.CdbSpec
Description : Test specifications for Hsed.Transport.Cdb
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The five commands a drive transport carries (design 9.3) and their CDBs:
the ten CDBs of a drive whose base ComID is 0x1004, SAT's and SCSI's, byte
for byte; every CDB 'cdbFor' builds read back field by field, its
direction, protocol, ComID and count the request's that 'shapeOf' admitted;
the refusals, of no blocks, of two blocks of ComID management, of a ComID
other than the base bound, of an IF-SEND whose payload is not its transfer
length, of anything but Level 0 Discovery while no base is bound; the
shapes numbered as cbits/hsed_cdb.h numbers them.

The Level 0 IF-RECV's CDB held to the kext's own: for 4 blocks, the bytes of
the CDBs the kext sends when it attaches (macos-driver TPerDriver.cpp:1506-1524
for SCSI's SECURITY PROTOCOL IN, :1527-1557 for SAT's ATA PASS-THROUGH of
TRUSTED RECEIVE), byte for byte; for 1 to 255 blocks the same with the count
changed, every one of them 'cdbFor''s; no blocks and more than 255 refused.
Before a base is bound, a replay's one command and the kext transport's
('shapeOf' with no base): Level 0 Discovery's protocol (Core 3.3.6.2, PDF
67) and ComID (Core Table 26, PDF 55), of whole 512-byte blocks, 1 to 255;
every other request refused, saying which.
-}
module Hsed.Transport.CdbSpec (spec) where

import qualified RIO.ByteString        as B
import qualified RIO.Char              as C
import           RIO
import qualified RIO.Text              as T
import qualified RIO.Text.Partial      as T'
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Hsed.Transport.Cdb
import           Hsed.Transport.Drive  (InterfaceType (..))
import           Hsed.Wire             (Dir (..), Request (..), comIdManagementProtocol, comPacketProtocol, level0ComId)


-- | The kext's attach-time CDBs, Level 0 IF-RECV of 4 blocks (2048 bytes).
kextSat, kextScsi :: ByteString
kextSat  = B.pack [0xA1, 0x08, 0x0E, 0x01, 0x04, 0x00, 0x01, 0x00, 0x00, 0x5C, 0x00, 0x00]
kextScsi = B.pack [0xA2, 0x01, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00]

-- | A CDB with one byte changed.
withByte :: Int -> Word8 -> ByteString -> ByteString
withByte i w b = B.take i b <> B.singleton w <> B.drop (i + 1) b

-- | An IF-RECV.
recv :: Word8 -> Word16 -> Word32 -> Request
recv p c n = Request { dir = IfRecv, proto = p, comId = c, lenBytes = n, payload = "" }

-- | An IF-SEND of a payload of its transfer length.
send :: Word8 -> Word16 -> Word32 -> Request
send p c n = Request { dir = IfSend, proto = p, comId = c, lenBytes = n, payload = B.replicate (fromIntegral n) 0 }

-- | The base ComID of design 9.3's drive (its Opal SSC V2 descriptor's).
base :: Word16
base = 0x1004

-- | The ten CDBs of design 9.3's drive on its base ComID, each with the
--   request the jig makes of it: SAT's (the bytes SAT-5's and ACS-5's, SIIS
--   1.20's references [21] and [22], PDF 17; the values its Tables 16-17,
--   PDF 40-41), then SCSI's (SPC-5's, its reference [10], PDF 16; Tables
--   12-13, PDF 30-31).
golden :: [(InterfaceType, Shape, Word16, Word8, Request, Text)]
golden =
  [ (IfSAT,  Level0Recv, 0x0001, 4, recv 0x01 0x0001 2048, "a1 08 0e 01 04 00 01 00 00 5c 00 00")
  , (IfSAT,  PacketSend, base,   1, send 0x01 base 512,    "a1 0a 06 01 01 00 04 10 00 5e 00 00")
  , (IfSAT,  PacketRecv, base,   4, recv 0x01 base 2048,   "a1 08 0e 01 04 00 04 10 00 5c 00 00")
  , (IfSAT,  ResetSend,  base,   1, send 0x02 base 512,    "a1 0a 06 02 01 00 04 10 00 5e 00 00")
  , (IfSAT,  ResetRecv,  base,   1, recv 0x02 base 512,    "a1 08 0e 02 01 00 04 10 00 5c 00 00")
  , (IfSCSI, Level0Recv, 0x0001, 4, recv 0x01 0x0001 2048, "a2 01 00 01 80 00 00 00 00 04 00 00")
  , (IfSCSI, PacketSend, base,   1, send 0x01 base 512,    "b5 01 10 04 80 00 00 00 00 01 00 00")
  , (IfSCSI, PacketRecv, base,   4, recv 0x01 base 2048,   "a2 01 10 04 80 00 00 00 00 04 00 00")
  , (IfSCSI, ResetSend,  base,   1, send 0x02 base 512,    "b5 02 10 04 80 00 00 00 00 01 00 00")
  , (IfSCSI, ResetRecv,  base,   1, recv 0x02 base 512,    "a2 02 10 04 80 00 00 00 00 01 00 00")
  ]

-- | A CDB read back, field by field, as the kext reads it: its direction (SAT's
--   PROTOCOL 4 or 5 and its command 5Ch or 5Eh, which must agree; SCSI's
--   opcode A2h or B5h), its protocol, its ComID, its count of blocks; or
--   Nothing when it is no CDB of the four kinds, a byte off its template.
readCdb :: InterfaceType -> ByteString -> Maybe (Dir, Word8, Word16, Word8)
readCdb iface cdb = case (iface, B.unpack cdb) of
  (IfSAT, [0xA1, 0x08, 0x0E, p, n, 0x00, lo, hi, 0x00, 0x5C, 0x00, 0x00]) -> Just (IfRecv, p, word hi lo, n)
  (IfSAT, [0xA1, 0x0A, 0x06, p, n, 0x00, lo, hi, 0x00, 0x5E, 0x00, 0x00]) -> Just (IfSend, p, word hi lo, n)
  (IfSCSI, [0xA2, p, hi, lo, 0x80, 0x00, 0x00, 0x00, 0x00, n, 0x00, 0x00]) -> Just (IfRecv, p, word hi lo, n)
  (IfSCSI, [0xB5, p, hi, lo, 0x80, 0x00, 0x00, 0x00, 0x00, n, 0x00, 0x00]) -> Just (IfSend, p, word hi lo, n)
  _                                                                         -> Nothing
  where word hi lo = fromIntegral hi * 256 + fromIntegral lo

-- | The requests of a grid: each direction, protocols 0 to 3, ComIDs about
--   the reserved range and the base, lengths of whole blocks and not, and
--   for an IF-SEND a payload of its length, one byte longer and none.
grid :: [Request]
grid =
  [ Request d p c n pl
  | d <- [IfSend, IfRecv], p <- [0x00 .. 0x03], c <- [0x0000, 0x0001, 0x0004, 0x0FFF, 0x1000, base, 0x1005, 0xFFFF]
  , n <- [0, 1, 511, 512, 1024, 2048, 130560, 131072], pl <- payloads d n ]
  where
    payloads d n
      | d == IfSend = [B.replicate (fromIntegral n) 0, B.replicate (fromIntegral n + 1) 0, ""]
      | otherwise   = [""]

-- | The C name of a shape, as cbits/hsed_cdb.h's enum spells it: Level0Recv, HSED_SHAPE_LEVEL0_RECV.
cName :: Shape -> Text
cName s = "HSED_SHAPE_" <> T.toUpper (T.pack (concatMap snake (zip [0 :: Int ..] (show s))))
  where snake (i, ch) = [ '_' | i > 0, C.isUpper ch ] <> [ch]

-- | The names and values of @enum hsed_shape@ in a header's text.
enumValues :: Text -> [(Text, Int)]
enumValues src =
  [ (T.strip name, v)
  | item <- T'.splitOn "," body, [name, value] <- [T'.splitOn "=" item], Just v <- [readMaybe (T.unpack (T.strip value))] ]
  where body = T.takeWhile (/= '}') (T.drop 1 (T.dropWhile (/= '{') (snd (T'.breakOn "enum hsed_shape" src))))

spec :: Spec
spec = describe "Hsed.Transport.Cdb" $ do
  describe "the five commands a drive transport carries" $ do
    it "are numbered as cbits/hsed_cdb.h's enum hsed_shape numbers them, the shim's argument" $ do
      header <- decodeUtf8Lenient <$> readFileBinary "cbits/hsed_cdb.h"
      enumValues header `shouldBe` [ (cName s, shapeCode s) | s <- [minBound .. maxBound] ]
      map shapeCode [minBound .. maxBound] `shouldBe` [0 .. 4]
    it "are two IF-SENDs and three IF-RECVs, STACK_RESET's two of protocol 0x02 (Core Tables 29-30, PDF 62-63), the rest of 0x01" $ do
      [ (s, shapeDir s, shapeProto s) | s <- [minBound .. maxBound] ] `shouldBe`
        [ (Level0Recv, IfRecv, comPacketProtocol), (PacketSend, IfSend, comPacketProtocol), (PacketRecv, IfRecv, comPacketProtocol)
        , (ResetSend, IfSend, comIdManagementProtocol), (ResetRecv, IfRecv, comIdManagementProtocol) ]
      (comPacketProtocol, comIdManagementProtocol) `shouldBe` (0x01, 0x02)

  describe "cdbFor" $ do
    it "builds the ten CDBs of a drive whose base ComID is 0x1004, SAT's and SCSI's, byte for byte" $
      [ cdbHex <$> cdbFor iface s c n | (iface, s, c, n, _, _) <- golden ] `shouldBe` [ Right h | (_, _, _, _, _, h) <- golden ]
    it "builds them of the jig's requests, which shapeOf takes on the base bound" $
      [ shapeOf (Just base) q >>= \(s, n) -> cdbHex <$> cdbFor iface s base n | (iface, _, _, _, q, _) <- golden ]
        `shouldBe` [ Right h | (_, _, _, _, _, h) <- golden ]
    it "refuses no blocks, of every shape on either interface" $
      [ (iface, s) | iface <- [IfSAT, IfSCSI], s <- [minBound .. maxBound], isRight (cdbFor iface s base 0) ] `shouldBe` []
    it "refuses ComID management of other than one block: two, or any more (Core Tables 29-30, PDF 62-63)" $
      [ (iface, s, n) | iface <- [IfSAT, IfSCSI], s <- [ResetSend, ResetRecv], n <- [2 .. 255], isRight (cdbFor iface s base n) ]
        `shouldBe` []
    it "puts a shape on the base only where Core Table 26 does not reserve it, 0x1000 to 0xFFFF (PDF 55-56)" $
      [ (s, c) | s <- [PacketSend .. ResetRecv], c <- [minBound .. maxBound], isRight (cdbFor IfSAT s c 1) /= (c >= 0x1000) ]
        `shouldBe` []
    it "takes no base for Level 0 Discovery: its ComID is 0x0001 whatever base is given" $
      [ c | iface <- [IfSAT, IfSCSI], c <- [0x0000, 0x0004, 0x0FFF, base, 0xFFFF]
          , cdbFor iface Level0Recv c 4 /= cdbFor iface Level0Recv 0x0001 4 ] `shouldBe` []
    it "builds a CDB of the request shapeOf admits: its direction, protocol, ComID and blocks, read back as the kext reads them" $
      [ (iface, b, q)
      | iface <- [IfSAT, IfSCSI], b <- [Nothing, Just base, Just 0x0004], q <- grid, Right (s, n) <- [shapeOf b q]
      , (readCdb iface =<< either (const Nothing) Just (cdbFor iface s (fromMaybe 0 b) n))
          /= Just (dir q, proto q, comId q, n)
        || fromIntegral n * blockBytes /= lenBytes q ]
        `shouldBe` []

  describe "shapeOf" $ do
    it "is each of the five on the base bound, the blocks its transfer length's" $
      [ shapeOf (Just base) q | (IfSAT, _, _, _, q, _) <- golden ]
        `shouldBe` map Right [(Level0Recv, 4), (PacketSend, 1), (PacketRecv, 4), (ResetSend, 1), (ResetRecv, 1)]
    it "takes Level 0 Discovery alone while no base is bound" $ do
      [ isRight (shapeOf Nothing q) | (IfSAT, _, _, _, q, _) <- golden ] `shouldBe` [True, False, False, False, False]
      [ q | q <- grid, Right (s, _) <- [shapeOf Nothing q], s /= Level0Recv ] `shouldBe` []
      either id tshow (shapeOf Nothing (send 0x01 base 512)) `shouldSatisfy` ("no base ComID is bound" `T.isInfixOf`)
    it "refuses a ComID other than the base bound: another, TPER_RESET's, the reserved, a vendor's" $ do
      [ c | c <- [0x1005, 0x0004, 0x0000, 0x0FFF, 0xFFFF], q <- [send 0x01 c 512, recv 0x01 c 2048, send 0x02 c 512, recv 0x02 c 512]
          , isRight (shapeOf (Just base) q) ] `shouldBe` []
      either id tshow (shapeOf (Just base) (recv 0x01 0x1005 2048)) `shouldSatisfy` ("the base ComID bound, 0x1004" `T.isInfixOf`)
    it "carries nothing on a base Core Table 26 reserves (PDF 55-56), nor anything but Level 0's IF-RECV on its ComID" $ do
      [ q | c <- [0x0000, 0x0004], q <- [send 0x01 c 512, recv 0x01 c 2048, send 0x02 c 512, recv 0x02 c 512]
          , isRight (shapeOf (Just c) q) ] `shouldBe` []
      [ q | q <- [send 0x01 0x0001 512, recv 0x02 0x0001 512, send 0x02 0x0001 512], isRight (shapeOf (Just base) q) ] `shouldBe` []
    it "refuses another protocol on the base: 0x00, 0x03, 0x06" $
      [ q | p <- [0x00, 0x03, 0x06], q <- [send p base 512, recv p base 512], isRight (shapeOf (Just base) q) ] `shouldBe` []
    it "refuses no blocks, part of one, more than 255, and ComID management of two (Core Tables 29-30, PDF 62-63)" $ do
      [ (q, n) | n <- [0, 1, 511, 513, 131072], q <- [send 0x01 base n, recv 0x01 base n, recv 0x01 0x0001 n]
               , isRight (shapeOf (Just base) q) ] `shouldBe` []
      [ q | q <- [send 0x02 base 1024, recv 0x02 base 1024], isRight (shapeOf (Just base) q) ] `shouldBe` []
      either id tshow (shapeOf (Just base) (send 0x02 base 1024)) `shouldSatisfy` ("one 512-byte block" `T.isInfixOf`)
    it "refuses an IF-SEND whose payload is not its transfer length, longer or shorter" $ do
      [ n | n <- [0, 20, 511, 513, 1024], isRight (shapeOf (Just base) ((send 0x01 base 512) { payload = B.replicate n 0 })) ]
        `shouldBe` []
      either id tshow (shapeOf (Just base) ((send 0x01 base 512) { payload = B.replicate 20 0 }))
        `shouldSatisfy` ("its payload is 20 bytes" `T.isInfixOf`)
    it "names the request it refuses" $
      either id tshow (shapeOf Nothing (recv 0x02 base 512))
        `shouldSatisfy` ("an IF-RECV of protocol 0x02 on ComID 0x1004, 512 bytes: " `T.isPrefixOf`)

  describe "level0Cdb" $ do
    it "is, for 4 blocks, the kext's own attach-time CDB, byte for byte (TPerDriver.cpp:1506-1557)" $ do
      level0Cdb IfSAT 4 `shouldBe` Right kextSat
      level0Cdb IfSCSI 4 `shouldBe` Right kextScsi
    it "prints as the jig records it" $ do
      cdbHex <$> level0Cdb IfSAT 4 `shouldBe` Right "a1 08 0e 01 04 00 01 00 00 5c 00 00"
      cdbHex <$> level0Cdb IfSCSI 4 `shouldBe` Right "a2 01 00 01 80 00 00 00 00 04 00 00"
    prop "counts 1 to 255 blocks in SAT's COUNT and SCSI's ALLOCATION LENGTH, every other byte the kext's" $
      forAll (choose (1, 255)) $ \n -> do
        level0Cdb IfSAT n `shouldBe` Right (withByte 4 (fromIntegral n) kextSat)
        level0Cdb IfSCSI n `shouldBe` Right (withByte 9 (fromIntegral n) kextScsi)
    it "is cdbFor's Level 0 Discovery, the bytes it was at M3 for every count from 1 to 255" $
      [ (iface, n)
      | (iface, kext, at) <- [(IfSAT, kextSat, 4), (IfSCSI, kextScsi, 9)], n <- [1 .. 255]
      , level0Cdb iface n /= Right (withByte at (fromIntegral n) kext)
        || level0Cdb iface n /= cdbFor iface Level0Recv level0ComId (fromIntegral n) ]
        `shouldBe` []
    it "refuses no blocks, and more than a count byte holds" $
      [ (iface, n) | iface <- [IfSAT, IfSCSI], n <- [0, 256, 4096, maxBound], isRight (level0Cdb iface n) ]
        `shouldBe` []

  describe "Level 0 Discovery before a base is bound: a replay's one command, and the kext transport's (shapeOf with no base)" $ do
    it "is Level 0 Discovery's: protocol 0x01 (Core 3.3.6.2, PDF 67) and ComID 0x0001 (Core Table 26, PDF 55), the shim's" $
      (comPacketProtocol, level0ComId, shimComId) `shouldBe` (0x01, 0x0001, 0x0001)
    it "counts 512-byte blocks, at most 255 of them (SIIS 1.20 Tables 13 and 17, PDF 31 and 41: a length in 512-byte units; the kext's count byte)" $
      (blockBytes, maxLevel0Blocks) `shouldBe` (512, 255)
    it "is IF-RECV on protocol 1, ComID 1, of whole blocks: their count" $
      map (shapeOf Nothing . recv 0x01 0x0001) [512, 2048, 130560] `shouldBe` map Right [(Level0Recv, 1), (Level0Recv, 4), (Level0Recv, 255)]
    prop "takes every whole number of blocks from 1 to 255" $
      forAll (choose (1, 255)) $ \n ->
        shapeOf Nothing (recv 0x01 0x0001 (n * 512)) `shouldBe` Right (Level0Recv, fromIntegral n)
    it "refuses IF-SEND, another protocol, another ComID" $
      filter isRight (map (shapeOf Nothing)
        [ (recv 0x01 0x0001 2048) { dir = IfSend, payload = B.replicate 2048 0 }
        , recv 0x02 0x0001 2048
        , recv 0x01 0x1004 2048
        , recv 0x00 0x0000 2048
        ])
        `shouldBe` []
    it "refuses a length of no blocks, part of one, or more than 255, saying which" $
      [ n | n <- [0, 1, 511, 513, 1000, 2047, 131072, maxBound], not (refusedSaying n (shapeOf Nothing (recv 0x01 0x0001 n))) ]
        `shouldBe` []

-- | A refusal that names the length refused and why: not whole blocks of 512 bytes.
refusedSaying :: Word32 -> Either Text (Shape, Word8) -> Bool
refusedSaying n = either (\t -> (tshow n <> " bytes: not 1 to 255 whole 512-byte blocks") `T.isInfixOf` t) (const False)
