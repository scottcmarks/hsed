\documentstyle{article}
\begin{document}
\chapter{Types}

Define the types used in SEDs, and in the hsed program in particular.


\begin{code}
{-|
Module      : System.SED.MCTP.Common.Token
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for Tokens.

-}

{-# LANGUAGE NoImplicitPrelude #-}

module System.SED.MCTP.Common.Token
  (
    Token(..)
  , IsToken(..)

  , rawTokenSource
  , removeEmpty
  , combineContinued
  , tokenSource
  )

where

import           Data.Array
import           Data.Attoparsec.ByteString hiding (takeWhile)
import           Data.Bits
import           Data.ByteString            hiding (ByteString, take, takeWhile,
                                                    map, unsnoc)
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Combinators   (takeWhile)
import           GHC.Natural
import           RIO                        hiding (foldr, map, length, mask,
                                                    null, reverse, take, takeWhile)
import qualified RIO as R                   (map)
import           Test.QuickCheck hiding(generate,(.&.))
import           Test.QuickCheck.Instances.Natural ()
import           Test.QuickCheck.Instances.ByteString ()
import           Text.Printf

import           Data.ByteString.Integral
import           System.SED.MCTP.Common.StreamItem


\end{code}

Due to the nature of method parameters and results, there are two additional constructs defined for
messaging that serve as grouping mechanisms for the basic types: Named values and List values.

  a. Named values. The name (a byte-string/integer/uinteger value) followed by its value (any
messaging type, i.e. byte-string values, N length signed or unsigned integer values, list values,
or Named values).
  b. List values. Zero or more values of some type, grouped into an ordered list. List tokens are
used to encapsulate method parameters and method results.


Named values and List values serve multiple uses. One use of Named values is to identify optional
method parameters in stream encoding. List tokens are used to encapsulate method parameters or to
separate the InvokingID/MethodID from the method parameters in the stream encoding. For more
information on stream encoding, see 3.2.2.

{-*
3.2.2.3 Tokens

Values of the four basic types are packaged into tokens, each of which is a TLV (tag, length, value)
sequence of bits that specifies a single data value.


                                 Table 04 Token Types
+---------------------------------------------+--------+----------+-------------------+
|    Byte                                     |  Hex   | Acronym  | Meaning           |
+---------------+---------+---------+---------+--------+----------+-------------------+
|       0       |    1    |    2    |    3    |        |          |                   |
+-+-+-----------+---------+---------+---------+--------+----------+-------------------+
|0|S|  d<5..0>  |                             | 00..7F |          | Tiny atom         |
+-+-+-+-+-------+-----------------------------+--------+----------+-------------------+
|1|0|B|S|n<3..0>|                             | 80..BF |          | Short atom        |
+-+-+-+-+-+-----+---------+-------------------+--------+----------+-------------------+
|1|1|0|B|S|   n<10..0>    |                   | C0..DF |          | Medium atom       |
+-+-+-+-+-+-+-+-+---------+---------+---------+--------+----------+-------------------+
|1|1|1|0|0|0|B|S|n<23..16>| n<15..8>| n<7..0> | E0..E3 |          | Long atom         |
+-+-+-+-+-+-+-+-+---------+---------+---------+--------+----------+-------------------+
|                                             | E4..EF |       TCG Reserved           |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|0|0|0|0|                             |   F0   |    SL    | Start List        |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|0|0|0|1|                             |   F1   |    EL    | End List          |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|0|0|1|0|                             |   F2   |    SN    | Start Name        |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|0|0|1|1|                             |   F3   |    EN    | End Name          |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|                                             | F4..F7 |       TCG Reserved           |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|1|0|0|0|                             |   F8   |   CALL   | Call              |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|1|0|0|1|                             |   F9   |   EOD    | End of Data       |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|1|0|1|0|                             |   FA   |   EOS    | End of session    |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|1|0|1|1|                             |   FB   |    ST    | Start transaction |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|1|1|0|0|                             |   FC   |    ET    | End transaction   |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|                                             | FD..FE |       TCG Reserved           |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+
|1|1|1|1|1|1|1|1|                             |   FF   |    MT    | Empty atom        |
+-+-+-+-+-+-+-+-+-----------------------------+--------+----------+-------------------+



The Token Types identified in Table 04 are divided into 3 subgroups:

a. Simple Tokens - Atoms: tiny, short, medium, long, and empty atoms
b. Sequence Tokens: Start List, End List, Start Name, and End Name
c. Control Tokens: Call, End of Data, End of Session, Start Transaction, End Transaction



Tokens 0xE4-0xEF, 0xF4-0xF7 and 0xFD-0xFE are reserved for use by TCG.

An SSC MAY define support for only a subset of the available tokens, as well as
the behavior of the TPer when unsupported tokens are transmitted by the host.
-}
\begin{code}

data Token =
    Unsigned Natural
  | Signed Integer
  | Bytes ByteString
  | ContinuedBytes ByteString
  | StartName
  | EndName
  | StartList
  | EndList
  | Call
  | EndOfData
  | EndOfSession
  | StartTransaction
  | EndTransaction
  | Empty
  deriving (Eq,Show)

class (Show a) => IsToken a where
    fromToken :: Token -> Maybe a

    token :: a -> Token
    token = fromMaybe <$> (error . ("Not a Token" <>) . show) <*> mtoken

    mtoken :: a -> Maybe Token
    mtoken = Just . token


instance IsToken Token where
    token = id
    fromToken = Just

-- * Instances of StreamItem.
--

instance StreamItem Token where
    parser    = tokenParser
    generate  = generateToken



-- | Parse a Token.
--   Dispatch on the first byte, which is the tag, plus some more info.
--   256 thunks continue the parsing to a Parser Token
tokenParser :: Parser Token
tokenParser = pTag >>= (dispatch !)
  where
      pTag = anyWord8
      bnds = (minBound,maxBound)
      dispatch = listArray bnds $ R.map p (range bnds)
      p b
        | inRange (0x00,0x3F) b = pTinyUnsigned   b
        | inRange (0x40,0x7F) b = pTinySigned     b
        |          0x80  ==   b = failLengthZero "Short"
        | inRange (0x81,0x8F) b = pShortUnsigned  b
        |          0x90  ==   b = failLengthZero "Short"
        | inRange (0x91,0x9F) b = pShortSigned    b
        |          0xA0  ==   b = pure mempty <&> Bytes
        | inRange (0xA1,0xAF) b = pShortBytes     b
        |          0xB0  ==   b = failLengthZero "Short"
        | inRange (0xB1,0xBF) b = pShortCBytes    b
        | inRange (0xC0,0xC7) b = pMediumUnsigned b
        | inRange (0xC8,0xCF) b = pMediumSigned   b
        | inRange (0xD0,0xD7) b = pMediumBytes    b
        | inRange (0xD8,0xDF) b = pMediumCBytes   b
        |          0xE0  ==   b = pLongUnsigned
        |          0xE1  ==   b = pLongSigned
        |          0xE2  ==   b = pLongBytes
        |          0xE3  ==   b = pLongCBytes
        |          0xF0  ==   b = pure StartList
        |          0xF1  ==   b = pure EndList
        |          0xF2  ==   b = pure StartName
        |          0xF3  ==   b = pure EndName
        |          0xF8  ==   b = pure Call
        |          0xF9  ==   b = pure EndOfData
        |          0xFA  ==   b = pure EndOfSession
        |          0xFB  ==   b = pure StartTransaction
        |          0xFC  ==   b = pure EndTransaction
        |          0xFF  ==   b = pure Empty
        | otherwise             = failTCGReserved b

      -- parsers and constructors
      pTinyUnsigned   b = pTiny   b <&> tinyUnsigned   <&> Unsigned
      pTinySigned     b = pTiny   b <&> tinySigned     <&> Signed

      pShortUnsigned  b = pShort  b <&> fromByteString <&> Unsigned
      pShortSigned    b = pShort  b <&> fromByteString <&> Signed
      pShortBytes     b = pShort  b                    <&> Bytes
      pShortCBytes    b = pShort  b                    <&> ContinuedBytes

      pMediumUnsigned b = pMedium b <&> fromByteString <&> Unsigned
      pMediumSigned   b = pMedium b <&> fromByteString <&> Signed
      pMediumBytes    b = pMedium b                    <&> Bytes
      pMediumCBytes   b = pMedium b                    <&> ContinuedBytes

      pLongUnsigned     = pLong     <&> fromByteString <&> Unsigned
      pLongSigned       = pLong     <&> fromByteString <&> Signed
      pLongBytes        = pLong                        <&> Bytes
      pLongCBytes       = pLong                        <&> ContinuedBytes

      -- layout parsers
      pTiny   :: Word8 -> Parser Word8
      pTiny     = pure

      pShort  :: Word8 -> Parser ByteString
      pShort  b = pure (0x0F .&. b)
                 <&> int
                 >>= takeFor "Short"

      pMedium :: Word8 -> Parser ByteString
      pMedium b = takeLengthFor "Medium" 1
                 <&> (.|. ((`shiftL` 8) $ int $ 0x07 .&. b))
                 >>= checkLength "Medium"
                 >>= takeFor "Medium"

      pLong   ::          Parser ByteString
      pLong     = takeLengthFor "Long" 3
                 >>= checkLength "Long"
                 >>= takeFor "Long"

      -- conversion to Integral
      tinyUnsigned b = natural (0x3F .&. b)
      tinySigned   b = if 0 == (0x20 .&. b) then integer d else integer d - 0x20
        where d = 0x1F .&. b

      -- zero length checks for Medium and Long lengths
      -- (Short lengths are checked syntacticaly, and Tiny has only the tag)
      failLengthZero      = fail . (<> " Atom with illegal length 0")
      checkLength   sz  l = if 0 < l then pure l else failLengthZero sz
      failTCGReserved     = fail . printf "TCG Reserved Token Type 0x%02X"
      takeLengthFor :: String -> Int -> Parser Int
      takeLengthFor sz l = take' <&> byteStringToNatural  <&> int
        where take' = take l <?> lengthError sz l
      takeFor :: String -> Int -> Parser ByteString
      takeFor       sz  l = take l <?> bytesError sz l
      szerror sz1 l sz2= mconcat
                    [ "Needed "
                    , show l
                    , if l==1 then " byte for " else " bytes for "
                    , sz1
                    , " "
                    , sz2
                    ]
      lengthError sz l = szerror sz l "Token length"
      bytesError sz l  = szerror sz l "Token"

generateToken :: Token -> ByteString
generateToken = go
  where
    go (Unsigned       n ) = unsigned n
    go (Signed         i ) = signed   i
    go (Bytes          bs) = bytes   bs
    go (ContinuedBytes bs) = cbytes  bs
    go Empty               = singleton 0xFF
    go StartList           = singleton 0xF0
    go EndList             = singleton 0xF1
    go StartName           = singleton 0xF2
    go EndName             = singleton 0xF3
    go Call                = singleton 0xF8
    go EndOfData           = singleton 0xF9
    go EndOfSession        = singleton 0xFA
    go StartTransaction    = singleton 0xFB
    go EndTransaction      = singleton 0xFC

    unsigned n
        | inRange(0,63) n   = singleton (0x00 .|. 0x3F .&. byte n)
        | otherwise         = tlv 0 0 $ naturalToByteString n
    signed   i
        | inRange(-32,31) i = singleton (0x40 .|. 0x3F .&. byte i)
        | otherwise         = tlv 0 1 $ integerToByteString i
    bytes                   = tlv 1 0
    cbytes                  = tlv 1 1

    tlv _B _S bs = tl _B _S (length bs) <> bs
    tl _B _S l
        | l <= 0x0F =            -- Small
              singleton $ 0x80 .|. _BS `shiftL` 4 .|. byte l
        | l <= 0x7FF =           -- Medium
              singleton (0xC0 .|. _BS `shiftL` 3 .|. byte (l `shiftR` 8))
           <> byte0 l
        | l <= 0xFFFFFFFFFFFF =  -- Long
              singleton (0xE0 .|. _BS)
           <> byte2 l <> byte1 l <> byte0 l
        | otherwise = error "Generated Atom byte sequence impossibly long!"
      where _BS = shiftL _B 1 .|. _S
            byte0 = singleton . byte
            byte1 = byte0 . (`shiftR` 8)
            byte2 = byte0 . (`shiftR` 16)

-- * Arbitrary instances for testing
--

instance Arbitrary Token where
    arbitrary = frequency
        [ (10, Unsigned       <$> arbitrary)
        , (10, Signed         <$> arbitrary)
        , (15, Bytes          <$> arbitrary)
        , ( 5, ContinuedBytes <$> arbitrary)
        , ( 3, pure StartName)
        , ( 3, pure EndName)
        , ( 3, pure StartList)
        , ( 3, pure EndList)
        , ( 1, pure EndOfData)
        , ( 1, pure EndOfSession)
        , ( 1, pure StartTransaction)
        , ( 1, pure EndTransaction)
        ]

rawTokenSource :: MonadThrow m => ConduitT ByteString (PositionRange, Token) m ()
rawTokenSource = conduitParser parser

removeEmpty :: Monad m => ConduitT (PositionRange, Token) (PositionRange, Token) m ()
removeEmpty = takeWhile ((/= Empty) . snd)

combineContinued :: Monad m => ConduitT (PositionRange, Token) (PositionRange, Token) m ()
combineContinued = loop Nothing
  where
    loop mpacc = await >>= examine mpacc
       where
         examine  Nothing            (Just (pos, ContinuedBytes bs))  =  loop  $ Just (pos, bs)
         examine  (Just (pos, acc))  (Just (_  , ContinuedBytes bs))  =  loop  $ Just (pos, append acc bs)
         examine  Nothing            (Just (pos, t))                  =  yield (pos, t)
         examine  (Just (pos, acc))  (Just (_  , Bytes bs         ))  =  yield (pos, Bytes (append acc bs))
         examine  Nothing            Nothing                          =  pure  ()
         examine  _                  Nothing                          =  error "ContinuedBytes not finished"
         examine  _                  _                                =  error "ContinuedBytes followed by non-Bytes"

tokenSource :: MonadThrow m => ConduitT ByteString (PositionRange, Token) m ()
tokenSource = rawTokenSource .| removeEmpty .| combineContinued



\end{code}

{-*
3.2.2.3.1 Simple Tokens -- Atoms Overview

Atoms are used to encode data of various sizes and types. Atoms MAY be tiny
atoms, which are one byte in length; short atoms which have a 1-byte header and
contain up to 15 bytes of data; medium atoms which have a 2-byte header and
contain up to 2047 bytes of data; or long atoms which have a 4-byte header and
which contain up to 16,777,215 bytes of data.

Tiny atoms only represent integers, whereas short, medium, and long atoms are
used to represent integers or bytes (with the `B' bit set).

A continued value is used to represent a long byte sequence when the total
length is not known in advance. A continued value is represented by a sequence
of two or more atoms.

Each atom in a continued value MAY be a short atom, medium atom, or long. The
BS bits are set to 11b for all atoms except the last atom, for which the BS bits
are set to 10b. All representations of continued are considered equivalent
encodings of the same value.

Integer and uinteger values SHOULD be encoded using the shortest possible atom.


3.2.2.3.1.1 Tiny atoms

Tiny atom header and data are all contained in eight bits.


   Table 05 Tiny Atom Description
    +--------------------------+
    |      Header + Data       |
    +---------+----+-----------+
    |Tiny atom|sign|   data    |
    +---------+----+-+-+-+-+-+-+
    |    0    | S  |d|d|d|d|d|d|
    +---------+----+-+-+-+-+-+-+


The encoding is as follows:

                          Table 06 Tiny Atom Encoding
    +----------+----------------------------------------------------------+
    |Tiny Atom | This bit is set to 0b to indicate the atom is a tiny atom|
    |indicator |                                                          |
    +----------+----------------------------------------------------------+
    |Sign      |Value Interpretation                                      |
    |indicator | 0b   The data is treated as unsigned integer data.       |
    |          | 1b   The data is treated as a signed integer.            |
    +----------+----------------------------------------------------------+
    |Data bits |These represent the data value, an unsigned value in the  |
    |          |range of 0...63 or a signed value in the range of         |
    |          |-32...31. The interpretation is based on the setting of   |
    |          |the sign bit.                                             |
    +----------+----------------------------------------------------------+


3.2.2.3.1.2 Short atoms

Short atoms consist of a one-byte header and between 0 and 15 bytes of data.

                 Table 07 Short Atom Description
    +---------------------------------------+---------------+
    |            Header (1 byte)            |     Data      |
    +-----------+---------+---------+-------+---------------+
    |Short Atom |byte/    |sign/    |length |(0...15 bytes) |
    |           |integer  |continued|       |               |
    +-----+-----+---------+---------+-+-+-+-+---+-------+---+
    |  1  |  0  |  B      |  S      |n|n|n|n| d |  ...  | d |
    +-----+-----+---------+---------+-+-+-+-+---+-------+---+


The encoding is as follows:


                                       Table 08 Short Atom Encoding
    +----------------+--------------------------------------------------------------------------------+
    |Short Atom      |These two bits are set to 10b to indicate the atom is a short atom.             |
    |indicator       |                                                                                |
    +----------------+--------------------------------------------------------------------------------+
    |Byte/integer    | Value Interpretation                                                           |
    |indicator       | 0b    The data bytes represent an integer value and the S bit indicates if that|
    |                |       value is signed.                                                         |
    |                | 1b    The data bytes represent a byte sequence and the S bit indicates whether |
    |                |       or not this value is continued into another atom.                        |
    +----------------+------+-------------------------------------------------------------------------+
    |Sign/continued  | Value Interpretation                                                           |
    |indicator       | 0b    The interpretation of the data depends on the byte/integer indicator bit.|
    |                |        B==0b  The data is treated as unsigned integer data.                    |
    |                |        B==1b  The data is either the complete byte sequence, or the final      |
    |                |               segment of a continued byte sequence.                            |
    |                | 1b    The interpretation of the data depends on the byte/integer indicator bit.|
    |                |        B==0b  The data is treated as signed integer data.                      |
    |                |        B==1b  The data is a non-final segment of a multi-byte continued value. |
    +----------------+--------------------------------------------------------------------------------+
    |Length          |These bits specify the length of the following data byte sequence. The permitted|
    |                |range is from 0 to 15, inclusive.                                               |
    +----------------+--------------------------------------------------------------------------------+

A length of 0 SHALL only be permitted for non-continued bytes tokens The encoding of a 0-length byte
value is displayed in Table 09.

         Table 09 0-Length Byte Encoding
    +---------------------------------------+
    |            Header (1 byte)            |
    +-----------+---------+---------+-------+
    |Short Atom |byte/    |sign/    |length |
    |           |integer  |continued|       |
    +-----+-----+---------+---------+-+-+-+-+
    |  1  |  0  |  1      |  0      |0|0|0|0|
    +-----+-----+---------+---------+-+-+-+-+

A 0-length byte value is encoded using only 1 byte: 1 0 1 0 0 0 0 0. This value would be encoded in
the token stream as 0xA0.


3.2.2.3.1.3 Medium atoms

Medium atoms consist of a two-byte header, and between 1 and 2047 bytes of data.

                        Table 10 Medium Atom Description
    +---------------------------------------------------------+----------------+
    |                    Header (2 bytes)                     |      Data      |
    +-------------------------------------------+-------------+----------------+
    |                    0                      |     1       |      ...       |
    +-----------------+---------+---------+-----+-------------+----------------+
    |  Medium Atom    |byte/    |sign/    |      length       |(1...2047 bytes)|
    |                 |integer  |continued|                   |                |
    +-----+-----+-----+---------+---------+-+-+-+-+-+-+-+-+-+-+---+-------+----+
    |  1  |  1  |  0  |    B    |    S    |n|n|n|n|n|n|n|n|n|n| d |  ...  | d  |
    +-----+-----+-----+---------+---------+-+-+-+-+-+-+-+-+-+-+---+-------+----+


The encoding is as follows:

                                     Table 11 Medium Atom Encoding
    +---------------+--------------------------------------------------------------------------------+
    |Medium Atom    | These three bits are set to 110b to indicate the atom is a medium atom.        |
    |indicator      |                                                                                |
    +---------------+--------------------------------------------------------------------------------+
    |Byte/integer   | Value Interpretation                                                           |
    |indicator      | 0b    The data bytes represent an integer value and the S bit indicates if that|
    |               |       value is signed.                                                         |
    |               | 1b    The data bytes represent a byte sequence and the S bit indicates whether |
    |               |       or not this value is continued into another atom.                        |
    +---------------+------+-------------------------------------------------------------------------+
    |Sign/continued | Value Interpretation                                                           |
    |indicator      | 0b    The interpretation of the data depends on the byte/integer indicator bit.|
    |               |        B==0b  The data is treated as unsigned integer data.                    |
    |               |        B==1b  The data is either the complete byte sequence, or the final      |
    |               |               segment of a continued byte sequence.                            |
    |               | 1b    The interpretation of the data depends on the byte/integer indicator bit.|
    |               |        B==0b  The data is treated as signed integer data.                      |
    |               |        B==1b  The data is a non-final segment of a multi-byte continued value. |
    +---------------+--------------------------------------------------------------------------------+
    |Length         |These bits specify the length of the following data byte sequence. The value 0  |
    |               |is not a legal value. The permitted range is up to 2047.                        |
    +---------------+--------------------------------------------------------------------------------+


3.2.2.3.1.4 Long atoms

Long atoms consist of a four-byte header, and between 1 and 16M-1 bytes of data.

                            Table 12 Long Atom Description
    +-------------------------------------------------------------------------------------------------------+----------------+
    |                                           Header (4 bytes)                                            |      Data      |
    +-------------------------------------------------------+---------------+---------------+---------------+----------------+
    |                           0                           |        1      |       2       |       3       |      ...       |
    +-----------------------+-----------+---------+---------+---------------+---------------+---------------+----------------+
    |            Long       | reserved  |byte/    |sign/    |                    length                     |(1..16,777,215  |
    |            Atom       |           |integer  |continued|                                               |bytes)          |
    +-----+-----+-----+-----+-----+-----+---------+---------+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+---+-------+----+
    |  1  |  1  |  1  |  0  |  0  |  0  |    B    |    S    |n|n|n|n|n|n|n|n|n|n|n|n|n|n|n|n|n|n|n|n|n|n|n|n| d |  ...  | d  |
    +-----+-----+-----+-----+-----+-----+---------+---------+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+---+-------+----+


The encoding is as follows:

                                     Table 13 Long Atom Encoding
    +---------------+--------------------------------------------------------------------------------+
    |Long Atom      | These three bits are set to 110b to indicate the atom is a medium atom.        |
    |indicator      |                                                                                |
    +---------------+--------------------------------------------------------------------------------+
    |Byte/integer   | Value Interpretation                                                           |
    |indicator      | 0b    The data bytes represent an integer value and the S bit indicates if that|
    |               |       value is signed.                                                         |
    |               | 1b    The data bytes represent a byte sequence and the S bit indicates whether |
    |               |       or not this value is continued into another atom.                        |
    +---------------+------+-------------------------------------------------------------------------+
    |Sign/continued | Value Interpretation                                                           |
    |indicator      | 0b    The interpretation of the data depends on the byte/integer indicator bit.|
    |               |        B==0b  The data is treated as unsigned integer data.                    |
    |               |        B==1b  The data is either the complete byte sequence, or the final      |
    |               |               segment of a continued byte sequence.                            |
    |               | 1b    The interpretation of the data depends on the byte/integer indicator bit.|
    |               |        B==0b  The data is treated as signed integer data.                      |
    |               |        B==1b  The data is a non-final segment of a multi-byte continued value. |
    +---------------+--------------------------------------------------------------------------------+
    |Length         |These bits specify the length of the following data byte sequence. The value 0  |
    |               |is not a legal value. The permitted range is up to 16,777,215.                  |
    +---------------+--------------------------------------------------------------------------------+




3.2.2.3.1.5 Empty Atom

The Empty atom is one byte consisting of eight 1 bits.

Table 14 Empty Atom Description
    +---------------+
    |Header (1 byte)|
    +---------------+
    |  Empty Atom   |
    +-+-+-+-+-+-+-+-+
    |1|1|1|1|1|1|1|1|
    +-+-+-+-+-+-+-+-+

The Empty atom MAY appear at any point in the stream encoding where any other atom is able to
appear, including between the atoms of a continued value and after the End of Session, and it SHALL
be ignored.

Begin Informative Content

The Empty atom does not encode values. The Empty atom allows other values in the data subpacket
contained in that part of the stream to be aligned with multi-byte boundaries for efficiency. It also allows
areas of a fixed buffer to be filled with a value that is able to be safely ignored.

End Informative Content

3.2.2.3.2 Sequence Tokens

Composite values, such as Named values and lists, are represented by a sequence of tokens.

3.2.2.3.2.1 Named

Named values have the expositional form name=value and are used to represent a name-value pair. A
Named value is a sequence of tokens: a Start Name token (SN), followed by a non-continued byte-
string/uinteger/integer value that specifies the name, followed by any value (including list or a Named
value), followed by an End Name token (EN).

3.2.2.3.2.2 List

Lists are ordered sequences of elements of the form [e1,e2,...,ei]. List elements MAY be tokens,
lists, or Named values. A list is encoded as a Start List token (SL) followed by a sequence of zero or
more elements followed by an End List token (EL).



3.2.2.3.3 Control Tokens

Control tokens are single byte tokens that are used to specify special actions.

3.2.2.3.3.1 Call (CALL)

This token is used to indicate the start of a method invocation.

3.2.2.3.3.2 End of Data (EOD)

This token is used to signal the end of the parameters, or the result, of a method invocation. This token
is used in message streams by both the host and the SP.

3.2.2.3.3.3 End of Session (EOS)

The host application utilizes this token to signal to the SP that it is ending the session. The SP
responds to this token with an End of Session token of its own in its response stream.

3.2.2.3.3.4 Start Transaction (ST)

The host application utilizes this token to open a transaction.

When the host begins a transaction, the Start Transaction token is sent by the host to the SP along with
the status, a uinteger, required for that transaction control token. The status supplied by the host with
the Start Transaction token SHOULD be a 0x00, and SHALL be ignored by the TPer.

When the SP delivers its response to the host application's message, the SP's message SHALL mirror
that of the host by including Start Transaction tokens in the corresponding places in the message
stream. The TPer SHALL supply the status of the Start Transaction request. If the host sends a non-
zero status code with the Start Transaction token, the device SHALL respond with a status code of
0x00, unless the transaction was unable to start.

If the host transmits a Start Transaction token that causes the transaction nesting limit to be exceeded,
the TPer SHALL abort the session (see Properties Section for details on the transaction nesting limit).
If for any reason the TPer is unable to start a transaction as requested by the host, the TPer SHALL
abort the session.

  Table 15 Start Transaction Status Codes
    +-----------------------------------------+----------+
    |Start Transaction Status Code (uinteger) | Meaning  |
    +-----------------------------------------+----------+
    |                   0x00                  | Success  |
    +-----------------------------------------+----------+
    |                   >0x00                 | Reserved |
    +-----------------------------------------+----------+


3.2.2.3.3.5 End Transaction (ET)

The host application utilizes this token to commit or abort the associated open transaction level.

When the host ends the transaction, the End Transaction token is sent by the host to the SP along with
the uinteger status required by the host for that transaction control token.

When the SP delivers its response to the host application's message, the SP's message SHALL mirror
that of the host by including End Transaction tokens in the equivalent places in the message stream
along with the actual status of the End Transaction request.

The host SHOULD send a status code of 0x00 or 0x01 with an End Transaction token. A status code of
0x00 signals to the device that the host is committing that transaction level. A status code of 0x01
signals to the device that the host is aborting that transaction level and the TPer SHALL abort that
transaction level.



If the host sends a status code of 0x00, the device SHALL attempt to commit that transaction level, and
SHALL return either 0x00 in the case of a successfully committed transaction or 0x01 in the case of an
unsuccessfully committed transaction.



If the host sends a status code with an End Transaction token that the device does not support, the
device SHALL abort the transaction and return a status code of 0x01.

Host delivery of the End Transaction token with a status code other than 0x00 signals that the host is
aborting the transaction. The TPer SHALL abort that transaction level.

SP delivery of the End Transaction token with a status code other than 0x00 signals that the SP
aborted the transaction.


            Table 16 End Transaction Status Codes
    +----------------------------------------+----------+
    | End Transaction Status Code (uinteger) | Meaning  |
    +----------------------------------------+----------+
    |                    0x00                | Commit   |
    +----------------------------------------+----------+
    |                    0x01                | Abort    |
    +----------------------------------------+----------+
    |                    >0x01               | Reserved |
    +----------------------------------------+----------+


3.2.2.3.4 Out of Order Control Tokens

In cases where the host transmits out of order control tokens the TPer SHOULD abort the session.
These cases include (but are not limited to):

  a. Multiple consecutive control tokens of the same type where this repetition is not permitted. This
     includes the Call, End of Data, and End of Session tokens.
  b. Out of order control tokens


Any tokens encoded after an End of Session token SHALL be ignored by the TPer.

3.2.2.4 Invalid and Unexpected Tokens

3.2.2.4.1 Invalid Tokens

An invalid token is a token that is not supported by the TPer's current communications configuration.

The list of invalid tokens is as follows:

  1. A token whose size is greater than the TPer's MaxIndTokenSize property.
  2. A token whose size is greater than the TPer's MaxAggTokenSize property.
  3. A continued token, if the TPer does not support continued tokens (the ContinuedTokens
     property is reported by the TPer as FALSE).
  4. Transaction Control tokens, if the TPer does not support transactions (the MaxTransactionLimit
     property is omitted from the TPer's Properties method response).
  5. Unsupported simple tokens (ie if an SSC does not require support for certain simple tokens).
  6. A TCG Reserved token.


When an invalid token appears in a communication from the host, the TPer SHALL behave as follows:

  1. For regular sessions the TPer SHALL abort the session associated with the Packet that
     contained the violating token. Results for methods that were completed before the violating
     token was encountered SHALL be sent to the host.
  2. For control sessions the TPer SHALL stop processing the packet where the violating token
     occurs, and ignore the remainder of the packet. Results for methods completed before the
     violating token was encountered SHALL be sent to the host.




3.2.2.4.2 Unexpected Tokens

An unexpected token is an otherwise valid and supported token that, based on the construction of a
Subpacket payload, is not a token of the type that is expected, by the construction of the Subpacket
payload, to occupy a particular position in that payload.

Begin Informative Content

For example, in the construction of a method invocation, the beginning of the Subpacket payload
(discounting empty atoms) has the following structure:

  1. Call       -- the Call token
  2. InvokingID -- a byte token
  3. MethodID   -- a byte token
  4. StartList  -- the StartList token


If the beginning of the Subpacket payload was sent by the host as follows: Call token + uinteger token +
byte token + StartList token; then the uinteger token that appears between Call and the byte token
would be unexpected, as the expected token is a byte token.

End Informative Content

Given no other encoding errors in the Subpacket payload, or any other layer of the protocol stack, the
result of inclusion of an unexpected token SHALL be on e of the following, based on the conditions
stated in each item:

  1. For a regular session, abort the session (Note that the TPer is always free to abort the session
     at any time for any reason). For a control session, ignore the remainder of the packet. Results
     for methods that were completed before the violating token was encountered SHALL be sent to
     the host.
  2. If the unexpected token appears outside of a method invocation, at any position in the
     subpacket other than after a Call token and before the next End of Data token after that Call
     token, abort the session for a regular session or, for a control session, ignore the remainder of
     the packet. Results for methods that were completed before the violating token was
     encountered SHALL be sent to the host.
  3. If the unexpected token appears within a method invocation after the Call token but before a
     the first StartList token after that Call token (ie in the method header) for a method invoked
     within a regular session, the method fails and the TPer responds with an empty method result
     list and a method status of NOT_AUTHORIZED. If the unexpected token appears within a
     method invocation after the Call token but before the first StartList token after that Call token for
     a method invoked within a control session, the method SHALL be ignored.
  4. If the unexpected token appears within a method invocation after parameter StartList and
     before the End of Data token (ie in the method parameter list), the method fails and the TPer
     responds with a method result list that MAY be empty, and a method status of
     INVALID_PARAMETER. This includes type mismatches for parameter values.
  5. If the unexpected token appears between the StartList token that marks the beginning of the
     Status List, and the EndList token that marks the end of the Status List, abort the session for a
     regular session or, for a control session, ignore the remainder of the packet. Results for
     methods that were completed before the violating token was encountered SHALL be sent to the
     host. Note that the status list is a list of unsigned integers, so the appearance of a signed
     integer, or any other non-uinteger token, is unexpected.


If empty atoms are supported, then they SHALL NOT be unexpected tokens.



-}

\end{document}
