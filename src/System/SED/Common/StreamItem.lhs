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
{-# LANGUAGE TemplateHaskell   #-}

module System.SED.Common.StreamItem where

import           Data.Attoparsec.ByteString (Parser, parseOnly)
import           Data.ByteString
import           Data.Either.Combinators
import           RIO

\end{code}

Due to the nature of method parameters and results, there are two additional constructs defined for
messaging that serve as grouping mechanisms for the basic types: Named values and List values.

  a. Named values. The name (a byte-string/integer/uinteger value) followed by its value (any
messaging type, i.e. byte-string values, N length signed or unsigned integer values, list values,
or Named values).

\begin{code}

class StreamItem a where
    parse :: Parser a

    generate :: a -> ByteString

    maybeGenerate :: Maybe a -> Maybe ByteString
    maybeGenerate = (maybe Nothing (Just . generate))

    parseByteString :: ByteString -> Either String a
    parseByteString = parseOnly parse

    parseString :: String -> Either String a
    parseString = parseByteString . fromString

    maybeParse :: ByteString -> Maybe a
    maybeParse = rightToMaybe . parseByteString

    pg :: a -> Maybe a
    pg = maybeParse . generate



\end{code}
\end{document}
