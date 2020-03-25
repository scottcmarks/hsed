\documentstyle{article}
\begin{document}
\chapter{Integral}

Conversions between numbers used in tokens and
bytestrings used to represent them.


\begin{code}
{-|
Module      : Extras.Integral
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Conversions for numbers used in tokens.

-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Extras.Integral
where

import           Data.Attoparsec.ByteString   ()
import           GHC.TypeNats
import           RIO.ByteString               hiding (count,length,map)
import           Data.Bits
import           Data.ByteString      hiding (take, map, unsnoc)
import qualified Data.ByteString as B (map)
import           Data.Char            (chr, ord)
import           RIO                  hiding (foldr, map, length, mask,
                                              replicate, reverse, take)

\end{code}

{-*
3.2.1.3 Messaging Data Types

For stream encoding, because of the manner in which data is encoded and transferred across the
interface, the types used in method parameter and result values are described using two basic types:

  a. Byte-string values are a sequence of n bytes that are used to represent strings, blobs, bit
vectors, etc.
  b. N length integer values are whole numbers that are either signed or unsigned.

\begin{code}

rollUp :: Bits a => (Word8 -> a) -> (Word8, ByteString) -> a
rollUp f (d, ds) = foldl roll (f d) ds
  where
    x `roll` d' = shiftL x 8 .|. f d'

byteStringToNatural :: ByteString -> Natural
byteStringToNatural = maybe 0 (rollUp natural) . uncons

byteStringToInteger :: ByteString -> Integer
byteStringToInteger = maybe 0 rollUp' . uncons
  where rollUp' dds@(d,_) =
            if 0x00 == (0x80 .&. d)
            then rollUp integer dds
            else complement $ rollUp (integer.complement) dds

naturalToByteString :: Natural -> ByteString
naturalToByteString n = if n == 0 then singleton 0x00 else reverse $ unfoldr unroll n
  where unroll n' = if n' == 0 then Nothing else Just (fromIntegral n', shiftR n' 8)

-- | Type-level to value-level for all Integrals, from the Natural
intVal :: (Num b, KnownNat n) => Proxy n -> b
intVal p = fromIntegral $ natVal p

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


word8 :: (Integral a) => a -> Word8
word8 = fromIntegral

char :: Word8 -> Char
char = chr . int

ordw :: Char -> Word8
ordw = word8 . ord

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

\end{code}
