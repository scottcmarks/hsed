{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Readers.Text
Description : Identifiers and hex bytes read from a table's text, as several rules read them
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The readers of text that several rules share and no comment names
(docs/adding-a-rule.md: a helper never goes under @Rules/@): the two ways a
generated name is made from a caption's title, names compared as headers are,
numbers and bytes printed in hex or decimal, and a number printed in hex.
Every rule that names a declaration after a title uses one of the two, so
that the generated names follow one convention:

* 'titleIdent': a capitalised identifier, the title's words joined
  (@ComID Assignments@ gives @ComIDAssignments@), for a name a rule prefixes
  (@cComIDAssignments@);
* 'lowerIdent': a value's name, the leading capitals lowered as a run
  (@SPTemplates Table UIDs@ gives @spTemplatesTableUIDs@, @MBRControl@ gives
  @mbrControl@).

Every rule reads a number one way ('parseNumber': @0x0001@, @0001h@,
@0x0000_0000@, @1024@, keeping how many hex digits it prints) and prints one
in hex one way ('hexDigits').
-}
module Hsed.Spec.Readers.Text
  ( titleIdent
  , lowerIdent
  , nameKey
    -- * Numbers
  , Number (..)
  , parseNumber
  , readNumber
  , numberExpr
  , hexDigits
  , readHexByte
  ) where

import qualified Data.Char as DC
import qualified RIO.Char  as C
import qualified RIO.Text  as T
import           RIO
import           Numeric   (showHex)


-- | An identifier from a caption's title: its words, each capitalised, joined,
--   every character but a letter, a digit or an underscore dropped:
--   @ComID Assignments@ gives @ComIDAssignments@, @GET_COMID Command Block@
--   gives @GET_COMIDCommandBlock@.
titleIdent :: Text -> Text
titleIdent = T.concat . map capitalise . T.split (\c -> not (C.isAlphaNum c || c == '_'))

-- | An identifier from a title: its words joined, the leading capitals lowered
--   (the last kept when a lower-case letter follows): @SPTemplates Table UIDs@
--   gives @spTemplatesTableUIDs@, @HMACInit@ gives @hmacInit@.
lowerIdent :: Text -> Text
lowerIdent title = lowered <> rest
  where
    joined = T.concat [ capitalise (T.filter (\c -> C.isAlphaNum c || c == '_') w) | w <- T.words title ]
    (caps, after) = T.span C.isUpper joined
    (lowered, rest)
      | T.length caps > 1, maybe False (C.isLower . fst) (T.uncons after) = (T.toLower (T.dropEnd 1 caps), T.takeEnd 1 caps <> after)
      | otherwise = (T.toLower caps, after)

capitalise :: Text -> Text
capitalise w = maybe w (\(c, cs) -> T.cons (C.toUpper c) cs) (T.uncons w)

-- | A name compared as headers are: whitespace-free and case-blind (a pin's
--   @Feature Code@ names the grid's @Feature  Code@).
--
-- >>> nameKey "ACK/NAK  Supported"
-- "ack/naksupported"
nameKey :: Text -> Text
nameKey = T.toLower . T.filter (not . C.isSpace)


-- * Numbers

-- | A number as printed: its value, and how many hex digits it prints
--   (Nothing for a decimal one).
data Number = Number
  { nValue     :: !Integer
  , nHexDigits :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | A number in hex (@0x0001@, @0001h@, either case, an underscore between
--   groups of digits, @0x0000_0000@) or in decimal (@1024@).
--
-- >>> map parseNumber ["0x0C", "0001h", "0x0000_0001", "12", "VU", "0x"]
-- [Just (Number {nValue = 12, nHexDigits = Just 2}),Just (Number {nValue = 1, nHexDigits = Just 4}),Just (Number {nValue = 1, nHexDigits = Just 8}),Just (Number {nValue = 12, nHexDigits = Nothing}),Nothing,Nothing]
parseNumber :: Text -> Maybe Number
parseNumber n
  | Just h <- T.stripPrefix "0x" lower, isHex h = Just (Number (hexValue h) (Just (T.length (digits h))))
  | Just h <- T.stripSuffix "h" lower, isHex h  = Just (Number (hexValue h) (Just (T.length (digits h))))
  | not (T.null n), T.all C.isDigit n           = Number <$> readMaybe (T.unpack n) <*> pure Nothing
  | otherwise                                   = Nothing
  where
    lower = T.toLower n
    digits = T.filter (/= '_')
    isHex h = not (T.null (digits h)) && T.all C.isHexDigit (digits h) && not ("_" `T.isPrefixOf` h || "_" `T.isSuffixOf` h)
    hexValue = T.foldl' (\acc c -> acc * 16 + toInteger (DC.digitToInt c)) 0 . digits

-- | A number printed as a value of its own, a field's name or a cell: as
--   'parseNumber' reads it, its first character a digit (a name such as
--   @Each@ is no hex number @eac@ with an @h@ after it).
--
-- >>> map readNumber ["0x02", "12", "0001h", "Each", " 0x05 "]
-- [Just 2,Just 12,Just 1,Nothing,Just 5]
readNumber :: Text -> Maybe Integer
readNumber t = case T.uncons (T.strip t) of
  Just (c, _) | C.isDigit c -> nValue <$> parseNumber (T.strip t)
  _                         -> Nothing

-- | A number as printed: hex with as many digits as it printed, else decimal.
numberExpr :: Number -> Text
numberExpr (Number v hex) = case hex of
  Just w  -> hexDigits w v
  Nothing -> tshow v

-- | A number in hex, lower case, at least so many digits: @hexDigits 4 1@ is
--   @0x0001@ (a field's width in hex digits gives its constant's).
--
-- >>> map (uncurry hexDigits) [(4, 1), (2, 12), (1, 5), (0, 0), (2, 0x123)]
-- ["0x0001","0x0c","0x5","0x0","0x123"]
hexDigits :: Int -> Integer -> Text
hexDigits w v = "0x" <> T.replicate (w - T.length digits) "0" <> digits
  where digits = T.pack (showHex v "")

-- | A byte printed as exactly two hex digits, either case: @0B@, @ff@.
readHexByte :: Text -> Maybe Word8
readHexByte w = case T.unpack w of
  [a, b] | C.isHexDigit a, C.isHexDigit b -> Just (fromIntegral (DC.digitToInt a * 16 + DC.digitToInt b))
  _                                       -> Nothing
