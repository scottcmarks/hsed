{-|
Module      : Hsed.Core.Token
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for Tokens.
Define the types used in SEDs, and in the hsed program in particular.

The Core Spec text transcribed here in 2019 (the end of 3.2.1.3, then 3.2.2.3
through 3.2.2.4.2) is kept in dev/Token-prose.txt (design section 13.20).
-}

{-# LANGUAGE NoImplicitPrelude #-}

module Hsed.Core.Token
  (
    Token(..)
  , IsToken(..)

  , rawTokenSource
  , removeEmpty
  , combineContinued
  , tokenSource

  , byteStringToNatural
  , naturalToByteString
  , byteStringToInteger
  , integerToByteString
  , ordw
  )

where

import           Data.Array
import           Data.Attoparsec.ByteString           hiding (takeWhile)
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.Combinators             as C (filter)
import           GHC.Natural
import qualified RIO                                  as R (map)
import           Test.QuickCheck                      hiding (generate, (.&.))
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Instances.Natural    ()
import           Text.Printf

import           Control.Monad.Catch                  (MonadThrow (..))
import           Data.Bits                            (Bits, complement, shiftL,
                                                       shiftR, (.&.), (.|.))
import           Data.ByteString                      hiding (map, take, unsnoc)
import qualified Data.ByteString                      as B (map)
import           Data.Char                            (ord)
import           Data.Functor                         ((<&>))
import           Data.Maybe                           (Maybe (..), fromMaybe,
                                                       maybe)
import           GHC.Word                             (Word8)
import           Hsed.Core.StreamItem
import           Prelude                              (Bounded (..), Char,
                                                       Eq (..), Int, Integer,
                                                       Integral (..),
                                                       Monad (..), Show (..),
                                                       String, error, fail,
                                                       fromIntegral, id,
                                                       mconcat, mempty,
                                                       otherwise, pure, snd,
                                                       ($), (-), (.), (<),
                                                       (<$>), (<*>), (<=), (<>))


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
    go (ContinuedBytes bs)
      | null bs            = mempty  -- no atom: Core 3.2.2.3.1.2 permits length 0 only for a
                                     -- non-continued byte sequence, and an empty segment adds
                                     -- nothing to the value it continues
      | otherwise          = cbytes  bs
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
        | otherwise         = tlv 0 0 $ toByteString n
    signed   i
        | inRange(-32,31) i = singleton (0x40 .|. 0x3F .&. byte i)
        | otherwise         = tlv 0 1 $ toByteString i
    bytes                   = tlv 1 0
    cbytes                  = tlv 1 1

    tlv _B _S bs = tl _B _S (length bs) <> bs
    tl _B _S l
        | l <= 0x0F =            -- Small
              singleton $ 0x80 .|. _BS `shiftL` 4 .|. byte l
        | l <= 0x7FF =           -- Medium
              singleton (0xC0 .|. _BS `shiftL` 3 .|. byte (l `shiftR` 8))
           <> byte0 l
        | l <= 0xFFFFFF =        -- Long (3 length bytes, Table 13)
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
        , ( 5, ContinuedBytes . pack . getNonEmpty <$> arbitrary)
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

-- | Drop Empty atoms: they MAY appear anywhere an atom can, including between
--   the atoms of a continued value, and SHALL be ignored (Table 14).
removeEmpty :: Monad m => ConduitT (PositionRange, Token) (PositionRange, Token) m ()
removeEmpty = C.filter ((/= Empty) . snd)

combineContinued :: Monad m => ConduitT (PositionRange, Token) (PositionRange, Token) m ()
combineContinued = loop Nothing
  where
    loop mpacc = await >>= examine mpacc
       where
         examine  Nothing            (Just (pos, ContinuedBytes bs))  =  loop  $ Just (pos, bs)
         examine  (Just (pos, acc))  (Just (_  , ContinuedBytes bs))  =  loop  $ Just (pos, append acc bs)
         examine  Nothing            (Just (pos, t))                  =  yield (pos, t) >> loop Nothing
         examine  (Just (pos, acc))  (Just (_  , Bytes bs         ))  =  yield (pos, Bytes (append acc bs)) >> loop Nothing
         examine  Nothing            Nothing                          =  pure  ()
         examine  _                  Nothing                          =  error "ContinuedBytes not finished"
         examine  _                  _                                =  error "ContinuedBytes followed by non-Bytes"

tokenSource :: MonadThrow m => ConduitT ByteString (PositionRange, Token) m ()
tokenSource = rawTokenSource .| removeEmpty .| combineContinued


rollUp :: Bits a => (Word8 -> a) -> (Word8, ByteString) -> a
rollUp f (d, ds) = foldl roll (f d) ds
  where
    x `roll` d' = shiftL x 8 .|. f d'

byteStringToNatural :: ByteString -> Natural
byteStringToNatural = maybe 0 (rollUp fromIntegral) . uncons

byteStringToInteger :: ByteString -> Integer
byteStringToInteger = maybe 0 rollUp' . uncons
  where rollUp' dds@(d,_) =
            if 0x00 == (0x80 .&. d)
            then rollUp fromIntegral dds
            else complement $ rollUp (fromIntegral.complement) dds

naturalToByteString :: Natural -> ByteString
naturalToByteString n = if n == 0 then singleton 0x00 else reverse $ unfoldr unroll n
  where unroll n' = if n' == 0 then Nothing else Just (fromIntegral n', shiftR n' 8)

integerToByteString :: Integer -> ByteString
integerToByteString i =
    if 0 <= i
    then                    nonnegativeToByteString              i
    else B.map complement $ nonnegativeToByteString $ complement i
  where
    nonnegativeToByteString nn =
        let bs = toByteString $ natural nn
          in if 0x00 == 0x80 .&. head bs
                then bs
                else cons 0x00 bs


ordw :: Char -> Word8
ordw = fromIntegral . ord

byte :: Integral a => a -> Word8
byte = fromIntegral

int :: Integral a => a -> Int
int = fromIntegral

integer :: Integral a => a -> Integer
integer = fromIntegral

natural :: Integral a => a -> Natural
natural = fromIntegral

class IsByteString a where
    fromByteString :: ByteString -> a
    toByteString   :: a -> ByteString

instance IsByteString Integer where
    fromByteString = byteStringToInteger
    toByteString   = integerToByteString

instance IsByteString Natural where
    fromByteString = byteStringToNatural
    toByteString   = naturalToByteString
