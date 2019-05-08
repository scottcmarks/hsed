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
{-# LANGUAGE TypeSynonymInstances #-}

module Extras.Hex
where


import Data.List             (elemIndex)
import Prelude               ((!!))
import RIO
-- import qualified RIO             as R (unpack)
import Data.ByteString                 as B (pack, unpack)
import qualified Data.ByteString.Char8 as C (unpack)


import Extras.Integral


hexDigit :: Word8 -> Char
hexDigit = (hexDigits !!) . fromIntegral

hexDigits :: [Char]
hexDigits = "0123456789ABCDEF"

hexValue :: Char -> Maybe Word8
hexValue d = fromIntegral <$> elemIndex d hexDigits

hexCharSyntax :: HasHex a => a -> String
hexCharSyntax n = "\\x" ++ hex n


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
    fromHex []         = Just [] -- could also be Nothing
    fromHex [_hi]      = Nothing -- odd number of hex digits

instance HasHex ByteString where
    hex = hex . B.unpack
    fromHex = fmap pack . fromHex

instance HasHex String where
    hex = hex . (fromString::String -> ByteString)
    fromHex = fmap C.unpack . fromHex


\end{code}
