{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Cdb
Description : The command descriptor block of a Level 0 IF-RECV, on SAT and on SCSI
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The CDB that carries IF-RECV of Level 0 Discovery (protocol 1, ComID 1) to a
drive (design 7.2): on SAT, ATA PASS-THROUGH (12) wrapping TRUSTED RECEIVE
(A1h ... 5Ch); on SCSI, SECURITY PROTOCOL IN (A2h, INC_512), each its
transfer counted in 512-byte blocks, 1 to 255 (the kext sizes an A1h
transfer from the count byte alone). The bytes are those the kext itself
sends when it attaches (TPerDriver.cpp:1506-1557), and the C shim builds the
same, the only CDB it can (cbits/hsed_kext.c); these are what the jig
records and the tests hold to the kext's.

The drive transports (the kext's, "Hsed.Transport.MacKext", and a replay of
a capture, "Hsed.Transport.Replay") carry that one command at M3 and refuse
every other: 'level0Blocks' says which requests it is. Its ComID is the one
the shim's templates carry ('shimComId'), and a drive transport carries
nothing at all unless Core Table 26 gives Level 0 that ComID ("Hsed.Wire".'level0ComId'):
were the table's row lost, every command is refused before the shim is
called, not sent as ComID 1 and judged after.
-}
module Hsed.Transport.Cdb
  ( level0Cdb
  , cdbHex
    -- * The one command a drive transport carries
  , shimComId
  , blockBytes
  , maxLevel0Blocks
  , level0Blocks
  , level0BlocksOf
  ) where

import           Data.Bits            (shiftR)
import           Numeric              (showHex)
import qualified RIO.ByteString       as B
import           RIO
import qualified RIO.Text             as T

import           Hsed.Transport.Drive (InterfaceType (..))
import           Hsed.Wire            (Dir (..), Request (..), Unit (..), comPacketProtocol, hex, level0ComId, mostCarried)


-- | The 12 bytes of a Level 0 IF-RECV of a number of 512-byte blocks, or why
--   not (no blocks, or more than a count byte holds).
--
-- * SAT (SAT's ATA PASS-THROUGH (12) carrying ACS's TRUSTED RECEIVE; SIIS
--   1.20 Table 17, PDF 41): @A1@; PROTOCOL 4, PIO Data-In, in bits 4 to 1;
--   T_DIR from the device, BYTE_BLOCK, T_LENGTH in COUNT; FEATURES the
--   protocol; COUNT the blocks; LBA 7 to 0 zero (the length's high byte);
--   LBA 15 to 8 and 23 to 16 the ComID, low byte first; DEVICE 0; @5C@;
--   reserved; CONTROL.
-- * SCSI (SPC's SECURITY PROTOCOL IN; SIIS 1.20 Table 13, PDF 31): @A2@; the
--   protocol; the ComID, high byte first; INC_512; reserved; ALLOCATION
--   LENGTH, the blocks in its low byte; reserved; CONTROL.
--
-- >>> cdbHex <$> level0Cdb IfSAT 4
-- Right "a1 08 0e 01 04 00 01 00 00 5c 00 00"
level0Cdb :: InterfaceType -> Word32 -> Either Text ByteString
level0Cdb iface blocks
  | blocks == 0 || blocks > maxLevel0Blocks =
      Left ("a Level 0 IF-RECV of " <> tshow blocks <> " blocks: its length is 1 to 255 blocks of 512 bytes")
  | otherwise = Right (B.pack (cdb iface))
  where
    n = fromIntegral blocks
    (comIdHigh, comIdLow) = (fromIntegral (level0ComId `shiftR` 8), fromIntegral level0ComId)
    cdb = \case
      IfSAT  -> [0xA1, 0x08, 0x0E, comPacketProtocol, n, 0x00, comIdLow, comIdHigh, 0x00, 0x5C, 0x00, 0x00]
      IfSCSI -> [0xA2, comPacketProtocol, comIdHigh, comIdLow, 0x80, 0x00, 0x00, 0x00, 0x00, n, 0x00, 0x00]

-- | A CDB as the jig prints it: lower-case hex, a space between bytes.
cdbHex :: ByteString -> Text
cdbHex = T.unwords . map byte . B.unpack
  where byte w = T.pack ((if w < 0x10 then ('0' :) else id) (showHex w ""))


-- * The one command a drive transport carries

-- | The ComID the shim's two templates carry, 0x0001 (cbits/hsed_kext.c: SAT's
--   LBA 15 to 8, SCSI's SECURITY PROTOCOL SPECIFIC; "Hsed.Transport.KextSourceSpec"
--   holds them to 'level0Cdb', which writes "Hsed.Wire".'level0ComId'): the
--   ComID of the one command a drive transport carries.
shimComId :: Word16
shimComId = 0x0001

-- | The bytes of the block an IF-RECV's transfer length counts: 512 (SIIS
--   1.20 Table 17 for ATA, PDF 41; Table 13 with INC_512 set for SCSI, PDF 31).
blockBytes :: Word32
blockBytes = 512

-- | The most blocks a Level 0 IF-RECV asks for, 255: the kext sizes an ATA
--   PASS-THROUGH transfer from its COUNT byte alone, ignoring the length's
--   high byte (CDBAccess.cpp:46-50; design 6.2). The count is
--   "Hsed.Wire".'mostCarried''s, in blocks.
maxLevel0Blocks :: Word32
maxLevel0Blocks = mostCarried UnitBlocks512 `div` blockBytes

-- | The blocks a request asks for when it is the one command a drive
--   transport carries at M3: IF-RECV of Level 0 Discovery, protocol 1 and
--   ComID 1, in whole 512-byte blocks, 1 to 255 of them. Any other is
--   refused with why, the detail of the transport's 'Hsed.Wire.Unsupported'.
level0Blocks :: Request -> Either Text Word8
level0Blocks = level0BlocksOf level0ComId

-- | 'level0Blocks' given the ComID Core Table 26 gives Level 0 Discovery:
--   every request is refused unless that ComID is the shim's ('shimComId'),
--   so that a table that lost its row (0x0000) fails closed.
level0BlocksOf :: Word16 -> Request -> Either Text Word8
level0BlocksOf level0 r
  | level0 /= shimComId =
      Left ("M3: Level 0 only, and Core Table 26 gives Level 0 the ComID " <> hex level0 <> ", not the "
            <> hex shimComId <> " the kext shim's CDB carries: nothing is carried")
  | dir r /= IfRecv || proto r /= comPacketProtocol || comId r /= level0 = Left "M3: Level 0 only"
  | len == 0 || len `mod` blockBytes /= 0 || len > maxLevel0Blocks * blockBytes =
      Left ("M3: Level 0 only, of 1 to 255 whole 512-byte blocks, not " <> tshow len <> " bytes")
  | otherwise = Right (fromIntegral (len `div` blockBytes))
  where len = lenBytes r
