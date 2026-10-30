{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.CdbSpec
Description : Test specifications for Hsed.Transport.Cdb
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Level 0 IF-RECV's CDB held to the kext's own: for 4 blocks, the bytes of
the CDBs the kext sends when it attaches (macos-driver TPerDriver.cpp:1506-1524
for SCSI's SECURITY PROTOCOL IN, :1527-1557 for SAT's ATA PASS-THROUGH of
TRUSTED RECEIVE), byte for byte; for 1 to 255 blocks the same with the count
changed; no blocks and more than 255 refused. The command a drive transport
carries: Level 0 Discovery's protocol (Core 3.3.6.2, PDF 67) and ComID (Core
Table 26, PDF 55), of whole 512-byte blocks; every other request refused.
-}
module Hsed.Transport.CdbSpec (spec) where

import qualified RIO.ByteString        as B
import           RIO
import qualified RIO.Text              as T
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Hsed.Transport.Cdb
import           Hsed.Transport.Drive  (InterfaceType (..))
import           Hsed.Wire             (Dir (..), Request (..))


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

spec :: Spec
spec = describe "Hsed.Transport.Cdb" $ do
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
    it "refuses no blocks, and more than a count byte holds" $
      [ (iface, n) | iface <- [IfSAT, IfSCSI], n <- [0, 256, 4096, maxBound], isRight (level0Cdb iface n) ]
        `shouldBe` []

  describe "the one command a drive transport carries" $ do
    it "is Level 0 Discovery's: protocol 0x01 (Core 3.3.6.2, PDF 67) and ComID 0x0001 (Core Table 26, PDF 55)" $ do
      level0Protocol `shouldBe` 0x01
      level0ComId `shouldBe` 0x0001
    it "counts 512-byte blocks, at most 255 of them (SIIS 1.20 Tables 13 and 17, PDF 31 and 41; the kext's count byte)" $
      (blockBytes, maxLevel0Blocks) `shouldBe` (512, 255)
    it "is IF-RECV on protocol 1, ComID 1, of whole blocks: their count" $
      map (level0Blocks . recv 0x01 0x0001) [512, 2048, 130560] `shouldBe` [Right 1, Right 4, Right 255]
    prop "takes every whole number of blocks from 1 to 255" $
      forAll (choose (1, 255)) $ \n ->
        level0Blocks (recv 0x01 0x0001 (n * 512)) `shouldBe` Right (fromIntegral n)
    it "refuses IF-SEND, another protocol, another ComID: M3: Level 0 only" $
      map level0Blocks
        [ (recv 0x01 0x0001 2048) { dir = IfSend, payload = B.replicate 2048 0 }
        , recv 0x02 0x0001 2048
        , recv 0x01 0x1004 2048
        , recv 0x00 0x0000 2048
        ]
        `shouldBe` replicate 4 (Left "M3: Level 0 only")
    it "refuses a length of no blocks, part of one, or more than 255, saying which" $
      [ n | n <- [0, 1, 511, 513, 1000, 2047, 131072, maxBound], not (refusedSaying n (level0Blocks (recv 0x01 0x0001 n))) ]
        `shouldBe` []

-- | A refusal of the one command's kind that names the length refused.
refusedSaying :: Word32 -> Either Text Word8 -> Bool
refusedSaying n = either (\t -> "M3: Level 0 only" `T.isPrefixOf` t && tshow n `T.isInfixOf` t) (const False)
