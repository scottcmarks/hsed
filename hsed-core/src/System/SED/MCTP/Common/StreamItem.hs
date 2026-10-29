{-|
Module      : System.SED.MCTP.Common.StreamItem
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Parse and generate stream items.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.SED.MCTP.Common.StreamItem (StreamItem(..), Parser, require)

where

import           Data.Attoparsec.ByteString (Parser, parseOnly)
import           Data.ByteString
import           Data.Either.Combinators
import           RIO


{-
Due to the nature of method parameters and results, there are two additional constructs defined for
messaging that serve as grouping mechanisms for the basic types: Named values and List values.

  a. Named values. The name (a byte-string/integer/uinteger value) followed by its value (any
messaging type, i.e. byte-string values, N length signed or unsigned integer values, list values,
or Named values).

-}


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


require :: (StreamItem a, Show a, Eq a) => a -> Parser a
require t = parser >>= onlyt
  where
    onlyt x
        | x == t = pure t
        | otherwise = fail $ "Looking for " <> show t <> ", but saw " <> show x

instance (StreamItem a) => StreamItem [a] where  -- TODO: Token should use these
    parser = undefined
    generate = mconcat . fmap generate
instance (StreamItem a) => StreamItem(a,a) where
    parser = undefined
    generate (f, s) = generate f <> generate s
