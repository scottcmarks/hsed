\documentstyle{article}
\begin{document}
\chapter{Hex}

Define hex input and output functions

\begin{code}
{-|
Module      : Extras.Hex
Description : hex input and output of bytes
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Hex encode/decoding.

-}

{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module Extras.Hex
where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString       as B (pack, unpack)
import qualified Data.ByteString.Char8 as C (unpack)
import           Data.ByteString.Short      (ShortByteString, fromShort, toShort)
import           Data.Foldable              (concatMap)
import           Data.Functor               ((<$>))
import           Data.List                  (elemIndex)
import           Data.SizedText             (create, Static, unwrap)
import           Data.String                (IsString(..))

import           GHC.Base                   (String, fmap, pure,
                                             (<=), (.), ($), (<*>), (=<<))
import           GHC.List                   ((!!))
import           GHC.Maybe                  (Maybe(..))
import           GHC.Num                    ((+), (-), (*))
import           GHC.Real                   (divMod, fromIntegral)
import           GHC.TypeLits               (KnownNat)
import           GHC.Types                  (Char(..))
import           GHC.Word                   (Word8)

import           Data.ByteString.Integral


hexDigit :: Word8 -> Char
hexDigit = (hexDigits !!) . fromIntegral

hexDigits :: String
hexDigits = "0123456789ABCDEF0123456789abcdef"

hexValue :: Char -> Maybe Word8
hexValue d = fromIntegral . (\i -> if 16 <= i then i - 16 else i) <$> elemIndex d hexDigits


class HasHex a where
    hex :: a -> String
    fromHex :: String -> Maybe a

instance HasHex Word8 where
    hex w = case divMod w 16 of
      (q,r) -> hexDigit <$> [q, r]
    fromHex [hi,lo] = do h <- hexValue hi
                         l <- hexValue lo
                         pure $ h * 16 + l
    fromHex _       = Nothing

instance HasHex Char where
    hex = hex . ordw
    fromHex = fmap char . fromHex

instance HasHex [Word8] where
    hex = concatMap hex
    fromHex (hi:lo:bs) = (:) <$> fromHex [hi,lo] <*> fromHex bs
    fromHex []         = Just []
    fromHex [_hi]      = Nothing -- odd number of hex digits

instance HasHex ByteString where
    hex = hex . B.unpack
    fromHex = fmap B.pack . fromHex

instance HasHex String where
    hex = hex . (fromString::String -> ByteString)
    fromHex = fmap C.unpack . fromHex

instance HasHex ShortByteString  where
    hex = hex . fromShort
    fromHex hs = toShort <$> fromHex hs

instance (KnownNat n) => HasHex (Static ShortByteString n) where
    hex = hex . unwrap
    fromHex hs = create =<< fromHex hs

\end{code}
