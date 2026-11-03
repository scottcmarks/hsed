{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Cdb
Description : The five commands a drive transport carries, and the command descriptor block of each, on SAT and on SCSI
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The commands a drive transport carries (design 7.2, 9.3), five 'Shape's and
nothing else: Level 0 Discovery's IF-RECV (protocol 1, ComID 1; Core
3.3.6.1-3.3.6.2, PDF 67) and, on the drive's base ComID once it is bound, a
ComPacket's IF-SEND and IF-RECV (protocol 1) and STACK_RESET's
HANDLE_COMID_REQUEST and GET_COMID_RESPONSE (protocol 2, one block each; Core
Tables 29-30, PDF 62-63). 'shapeOf' says which of them a request is, or why
it is none; 'cdbFor' builds the 12 bytes that carry one to a drive (design
7.2): on SAT, ATA PASS-THROUGH (12) wrapping TRUSTED RECEIVE or TRUSTED SEND
(A1h ... 5Ch or 5Eh); on SCSI, SECURITY PROTOCOL IN or OUT (A2h or B5h,
INC_512); each transfer counted in 512-byte blocks, 1 to 255 (the kext sizes
an A1h transfer from the count byte alone). Level 0's CDB is the kext's own
at attach (TPerDriver.cpp:1506-1557).

The C shim builds the same CDBs in C (cbits/hsed_cdb.c, compiled in every
build), and 'cdbFor' builds them here independently: its protocols and
Level 0's ComID are "Hsed.Wire"'s constants, and the ComIDs a base may be
are those Core Table 26 does not reserve, as the generated code reads it
("Hsed.Sim.Params".'reservedComId'). The tests hold the two to each other,
and the jig judges by these the CDB the shim says it sent.

Level 0 Discovery's IF-RECV is carried only while Core Table 26 gives Level
0 the ComID the shim's Level 0 CDB carries ('shimComId',
"Hsed.Wire".'level0ComId'): were the table's row lost, the command is
refused before the shim is called, not sent as ComID 1 and judged after.
'level0Blocks' is M3's one command, in M3's words; since M4c a replay of a
captured Level 0 response ("Hsed.Transport.Replay") refuses every other
command by 'shapeOf' with no base bound, as the kext's transport does
before its bind, and nothing in the library calls 'level0Blocks'.
-}
module Hsed.Transport.Cdb
  ( -- * The five commands a drive transport carries
    Shape (..)
  , shapeCode
  , shapeDir
  , shapeProto
  , shapeOf
  , cdbFor
    -- * Level 0 Discovery's CDB
  , level0Cdb
  , cdbHex
    -- * The shim's Level 0 ComID, the block, and M3's one command
  , shimComId
  , blockBytes
  , maxLevel0Blocks
  , level0Blocks
  , level0BlocksOf
  ) where

import           Data.Bits            (shiftR)
import           Numeric              (showHex)
import qualified RIO.ByteString       as B
import qualified RIO.List             as L
import           RIO
import qualified RIO.Text             as T

import           Hsed.Sim.Params      (reservedComId)
import           Hsed.Transport.Drive (InterfaceType (..))
import           Hsed.Wire            (Dir (..), Request (..), Unit (..), comIdManagementProtocol, comPacketProtocol, hex,
                                       level0ComId, mostCarried)


-- * The five commands a drive transport carries

-- | The five commands a drive transport carries (design 9.3), in the order
--   of cbits/hsed_cdb.h's @enum hsed_shape@ ('shapeCode').
data Shape
  = Level0Recv  -- ^ IF-RECV of Level 0 Discovery: protocol 1, ComID 0x0001 (Core 3.3.6.1-3.3.6.2, PDF 67)
  | PacketSend  -- ^ IF-SEND of a ComPacket: protocol 1, the base ComID
  | PacketRecv  -- ^ IF-RECV of a ComPacket: protocol 1, the base ComID
  | ResetSend   -- ^ STACK_RESET's HANDLE_COMID_REQUEST: IF-SEND, protocol 2, the base ComID, one block
                --   (Core Table 29, PDF 62; its request Table 35, PDF 65)
  | ResetRecv   -- ^ STACK_RESET's GET_COMID_RESPONSE: IF-RECV, protocol 2, the base ComID, one block (Core Table 30, PDF 63)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A shape's number: @enum hsed_shape@'s value in cbits/hsed_cdb.h, what
--   the shim is told.
--
-- >>> map shapeCode [minBound .. maxBound]
-- [0,1,2,3,4]
shapeCode :: Shape -> Int
shapeCode = \case
  Level0Recv -> 0
  PacketSend -> 1
  PacketRecv -> 2
  ResetSend  -> 3
  ResetRecv  -> 4

-- | A shape's interface command.
shapeDir :: Shape -> Dir
shapeDir = \case
  Level0Recv -> IfRecv
  PacketSend -> IfSend
  PacketRecv -> IfRecv
  ResetSend  -> IfSend
  ResetRecv  -> IfRecv

-- | A shape's security protocol: ComID management's for STACK_RESET's two,
--   the ComPacket protocol, Level 0 Discovery's too, for the others
--   ("Hsed.Wire".'comIdManagementProtocol', 'comPacketProtocol').
shapeProto :: Shape -> Word8
shapeProto = \case
  Level0Recv -> comPacketProtocol
  PacketSend -> comPacketProtocol
  PacketRecv -> comPacketProtocol
  ResetSend  -> comIdManagementProtocol
  ResetRecv  -> comIdManagementProtocol

-- | Which of the five commands a request is, and the 512-byte blocks it
--   moves, given the base ComID bound (none: Level 0 Discovery alone), or
--   why it is none of them; it is one when
--
-- * its direction, protocol and ComID are a shape's: Level 0 Discovery's
--   (while Core Table 26 gives Level 0 the shim's ComID, 'shimComId'), or
--   one on the base bound, which must be a ComID Core Table 26 does not
--   reserve (PDF 55-56);
-- * its transfer is whole 512-byte blocks, 1 to 255 of them (the kext's
--   count byte), and one exactly for ComID management (Core Tables 29-30,
--   PDF 62-63);
-- * and, an IF-SEND, its payload is exactly its transfer length.
--
-- >>> shapeOf Nothing (Request IfRecv 0x01 0x0001 2048 mempty)
-- Right (Level0Recv,4)
-- >>> shapeOf (Just 0x1004) (Request IfSend 0x02 0x1004 512 (B.replicate 512 0))
-- Right (ResetSend,1)
shapeOf :: Maybe Word16 -> Request -> Either Text (Shape, Word8)
shapeOf base q = do
  shape  <- maybe (refused none) Right (L.find carries [minBound .. maxBound])
  blocks <- blocksOf shape
  when (dir q == IfSend && B.length (payload q) /= fromIntegral len) $
    refused ("its payload is " <> tshow (B.length (payload q)) <> " bytes, and an IF-SEND carries exactly its transfer length")
  pure (shape, blocks)
  where
    len = lenBytes q
    refused why = Left (named <> ": " <> why)
    named = (if dir q == IfSend then "an IF-SEND" else "an IF-RECV") <> " of protocol " <> hex (proto q) <> " on ComID "
            <> hex (comId q) <> ", " <> tshow len <> " bytes"

    carries s = shapeDir s == dir q && shapeProto s == proto q && comIdOf s == Just (comId q)
    comIdOf = \case
      Level0Recv -> if level0ComId == shimComId then Just level0ComId else Nothing
      _          -> case base of
        Just b | not (reservedComId b) -> Just b
        _                              -> Nothing

    blocksOf s
      | len == 0 || len `mod` blockBytes /= 0 || len > maxLevel0Blocks * blockBytes =
          refused ("not 1 to 255 whole 512-byte blocks (SIIS 1.20 Tables 12-13 and 16-17, PDF 30-31 and 40-41; "
                   <> "the kext's count byte)")
      | shapeProto s == comIdManagementProtocol && len /= blockBytes =
          refused "not ComID management's one 512-byte block (Core Tables 29-30, PDF 62-63)"
      | otherwise = Right (fromIntegral (len `div` blockBytes))

    none
      | proto q `notElem` [comPacketProtocol, comIdManagementProtocol] =
          "a drive transport carries protocols " <> hex comPacketProtocol <> " and " <> hex comIdManagementProtocol <> " alone"
      | comId q == level0ComId && level0ComId /= shimComId =
          "Core Table 26 gives Level 0 Discovery the ComID " <> hex level0ComId <> ", not the " <> hex shimComId
          <> " the shim's CDB carries, so it is not carried"
      | comId q == level0ComId =
          "Level 0 Discovery's ComID carries its IF-RECV of protocol " <> hex comPacketProtocol <> " alone (Core 3.3.6.2, PDF 67)"
      | otherwise = case base of
          Nothing -> "no base ComID is bound, and until the drive's is, Level 0 Discovery's IF-RECV alone is carried"
          Just b
            | reservedComId b ->
                "the base ComID bound, " <> hex b <> ", is one Core Table 26 reserves (PDF 55-56), so nothing is carried on it"
            | otherwise       -> "neither the base ComID bound, " <> hex b <> ", nor Level 0 Discovery's"

-- | The 12 bytes that carry a shape to a drive of an interface, given the
--   base ComID bound and the 512-byte blocks moved, or why not: no blocks,
--   more than one for ComID management (Core Tables 29-30, PDF 62-63), or
--   a base Core Table 26 reserves (PDF 55-56). Level 0 Discovery's shape
--   takes no base: its ComID is "Hsed.Wire".'level0ComId', 0x0001.
--
-- * SAT (SAT's ATA PASS-THROUGH (12) carrying ACS's TRUSTED RECEIVE or
--   TRUSTED SEND; SIIS 1.20 Tables 17 and 16, PDF 41 and 40): @A1@;
--   PROTOCOL 4, PIO Data-In, or 5, PIO Data-Out, in bits 4 to 1; T_DIR
--   from the device or to it, BYTE_BLOCK, T_LENGTH in COUNT; FEATURES the
--   protocol; COUNT the blocks; LBA 7 to 0 zero (the length's high byte);
--   LBA 15 to 8 and 23 to 16 the ComID, low byte first; DEVICE 0; @5C@ or
--   @5E@; reserved; CONTROL.
-- * SCSI (SPC's SECURITY PROTOCOL IN or OUT; SIIS 1.20 Tables 13 and 12,
--   PDF 31 and 30): @A2@ or @B5@; the protocol; the ComID, high byte first;
--   INC_512; reserved; ALLOCATION or TRANSFER LENGTH, the blocks in its low
--   byte; reserved; CONTROL.
--
-- The five on SAT, then on SCSI, of a drive whose base ComID is 0x1004:
--
-- >>> cdbHex <$> cdbFor IfSAT Level0Recv 0x0001 4
-- Right "a1 08 0e 01 04 00 01 00 00 5c 00 00"
-- >>> cdbHex <$> cdbFor IfSAT PacketSend 0x1004 1
-- Right "a1 0a 06 01 01 00 04 10 00 5e 00 00"
-- >>> cdbHex <$> cdbFor IfSAT PacketRecv 0x1004 4
-- Right "a1 08 0e 01 04 00 04 10 00 5c 00 00"
-- >>> cdbHex <$> cdbFor IfSAT ResetSend 0x1004 1
-- Right "a1 0a 06 02 01 00 04 10 00 5e 00 00"
-- >>> cdbHex <$> cdbFor IfSAT ResetRecv 0x1004 1
-- Right "a1 08 0e 02 01 00 04 10 00 5c 00 00"
-- >>> cdbHex <$> cdbFor IfSCSI Level0Recv 0x0001 4
-- Right "a2 01 00 01 80 00 00 00 00 04 00 00"
-- >>> cdbHex <$> cdbFor IfSCSI PacketSend 0x1004 1
-- Right "b5 01 10 04 80 00 00 00 00 01 00 00"
-- >>> cdbHex <$> cdbFor IfSCSI PacketRecv 0x1004 4
-- Right "a2 01 10 04 80 00 00 00 00 04 00 00"
-- >>> cdbHex <$> cdbFor IfSCSI ResetSend 0x1004 1
-- Right "b5 02 10 04 80 00 00 00 00 01 00 00"
-- >>> cdbHex <$> cdbFor IfSCSI ResetRecv 0x1004 1
-- Right "a2 02 10 04 80 00 00 00 00 01 00 00"
cdbFor :: InterfaceType -> Shape -> Word16 -> Word8 -> Either Text ByteString
cdbFor iface shape base blocks
  | blocks == 0 =
      Left (described <> ": no blocks; a transfer is 1 to 255 blocks of 512 bytes")
  | shapeProto shape == comIdManagementProtocol && blocks /= 1 =
      Left (described <> ": ComID management moves one 512-byte block (Core Tables 29-30, PDF 62-63)")
  | shape /= Level0Recv && reservedComId base =
      Left (described <> ": the ComID " <> hex base <> " is one Core Table 26 reserves (PDF 55-56), never a base")
  | otherwise = Right (B.pack cdb)
  where
    described = tshow shape <> " of " <> tshow blocks <> " blocks"
    c = if shape == Level0Recv then level0ComId else base
    (comIdHigh, comIdLow) = (fromIntegral (c `shiftR` 8), fromIntegral c)
    p = shapeProto shape
    cdb = case (iface, shapeDir shape) of
      (IfSAT,  IfRecv) -> [0xA1, 0x08, 0x0E, p, blocks, 0x00, comIdLow, comIdHigh, 0x00, 0x5C, 0x00, 0x00]
      (IfSAT,  IfSend) -> [0xA1, 0x0A, 0x06, p, blocks, 0x00, comIdLow, comIdHigh, 0x00, 0x5E, 0x00, 0x00]
      (IfSCSI, IfRecv) -> [0xA2, p, comIdHigh, comIdLow, 0x80, 0x00, 0x00, 0x00, 0x00, blocks, 0x00, 0x00]
      (IfSCSI, IfSend) -> [0xB5, p, comIdHigh, comIdLow, 0x80, 0x00, 0x00, 0x00, 0x00, blocks, 0x00, 0x00]


-- * Level 0 Discovery's CDB

-- | The 12 bytes of a Level 0 IF-RECV of a number of 512-byte blocks, or why
--   not (no blocks, or more than a count byte holds): 'cdbFor''s
--   'Level0Recv', the CDB the kext itself sends when it attaches
--   (TPerDriver.cpp:1506-1557) with its count changed.
--
-- >>> cdbHex <$> level0Cdb IfSAT 4
-- Right "a1 08 0e 01 04 00 01 00 00 5c 00 00"
level0Cdb :: InterfaceType -> Word32 -> Either Text ByteString
level0Cdb iface blocks
  | blocks == 0 || blocks > maxLevel0Blocks =
      Left ("a Level 0 IF-RECV of " <> tshow blocks <> " blocks: its length is 1 to 255 blocks of 512 bytes")
  | otherwise = cdbFor iface Level0Recv level0ComId (fromIntegral blocks)

-- | A CDB as the jig prints it: lower-case hex, a space between bytes.
cdbHex :: ByteString -> Text
cdbHex = T.unwords . map byte . B.unpack
  where byte w = T.pack ((if w < 0x10 then ('0' :) else id) (showHex w ""))


-- * The shim's Level 0 ComID, the block, and M3's one command

-- | The ComID of the shim's Level 0 IF-RECV, 0x0001, which its C writes
--   (cbits/hsed_cdb.c's @HSED_LEVEL0_COMID@; "Hsed.Transport.CdbCSpec" holds
--   every CDB @hsed_cdb_build@ makes to 'cdbFor', which writes
--   "Hsed.Wire".'level0ComId', and "Hsed.Transport.KextSourceSpec" its
--   templates): the one ComID on which a drive transport carries a command
--   before a base is bound.
shimComId :: Word16
shimComId = 0x0001

-- | The bytes of the block an IF-SEND's or IF-RECV's transfer length
--   counts: 512 (SIIS 1.20 Tables 16-17 for ATA, PDF 40-41; Tables 12-13
--   with INC_512 set for SCSI, PDF 30-31).
blockBytes :: Word32
blockBytes = 512

-- | The most blocks one command moves on a drive, Level 0 Discovery's
--   IF-RECV among them, 255: the kext sizes an ATA PASS-THROUGH transfer
--   from its COUNT byte alone, ignoring the length's high byte
--   (CDBAccess.cpp:46-50; design 6.2). The count is
--   "Hsed.Wire".'mostCarried''s, in blocks.
maxLevel0Blocks :: Word32
maxLevel0Blocks = mostCarried UnitBlocks512 `div` blockBytes

-- | The blocks a request asks for when it is the one command a drive
--   transport carried at M3: IF-RECV of Level 0 Discovery, protocol 1 and
--   ComID 1, in whole 512-byte blocks, 1 to 255 of them. Any other is
--   refused with why, in M3's words, which a replay's
--   'Hsed.Wire.Unsupported' carried until M4c (it refuses by 'shapeOf'
--   now).
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
