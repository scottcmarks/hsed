\documentstyle{article}
\begin{document}
\chapter{Stream Items}

Parse and generate stream items.


\begin{code}
{-|
Module      : System.SED.Common.StreamItem
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Parse and generate stream items.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.SED.Common.StreamItem (StreamItem(..), Parser, require)

where

import            Data.Attoparsec.ByteString (Parser, parseOnly)
import            Data.ByteString
import            Data.Either.Combinators
import            RIO


\end{code}

Due to the nature of method parameters and results, there are two additional constructs defined for
messaging that serve as grouping mechanisms for the basic types: Named values and List values.

  a. Named values. The name (a byte-string/integer/uinteger value) followed by its value (any
messaging type, i.e. byte-string values, N length signed or unsigned integer values, list values,
or Named values).

\begin{code}


class StreamItem a where
    parser :: Parser a

    generate :: a -> ByteString

    parseByteString :: ByteString -> Either String a
    parseByteString = parseOnly parser

    parseString :: String -> Either String a
    parseString = parseByteString . fromString

    maybeParse :: ByteString -> Maybe a
    maybeParse = rightToMaybe . parseByteString

    -- | reduction to canonical form; should never fail by returning Nothing
    pg :: a -> Maybe a
    pg = maybeParse . generate

    typeIsa :: a -> a
    typeIsa = id

    typeIsMaybea :: Maybe a -> Maybe a
    typeIsMaybea = id

    maybeGenerate :: Maybe a -> Maybe ByteString
    maybeGenerate = (fmap generate)

--     hex :: a -> String
--     hex     = (hex :: ByteString -> String) . generate

--     fromHex' :: String -> Maybe a
--     fromHex' = (>>= maybeParse) . (fromHex :: String -> Maybe ByteString)

-- _hex' :: (StreamItem a) => a -> String
-- _hex' = hex . generate

-- _fromHex' :: (StreamItem a) => String -> Maybe a
-- _fromHex' =  (>>= maybeParse) . fromHex

require :: (StreamItem a, Show a, Eq a) => a -> Parser a
require t = parser >>= onlyt
  where
    onlyt x
        | x == t = pure t
        | otherwise = fail $ "Looking for " <> show t <> ", but saw " <> show x

\end{code}
\end{document}
