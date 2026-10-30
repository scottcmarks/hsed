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
generated name is made from a caption's title, and a byte printed in hex.
Every rule that names a declaration after a title uses one of the two, so
that the generated names follow one convention:

* 'titleIdent': a capitalised identifier, the title's words joined
  (@ComID Assignments@ gives @ComIDAssignments@), for a name a rule prefixes
  (@cComIDAssignments@);
* 'lowerIdent': a value's name, the leading capitals lowered as a run
  (@SPTemplates Table UIDs@ gives @spTemplatesTableUIDs@, @MBRControl@ gives
  @mbrControl@).
-}
module Hsed.Spec.Readers.Text
  ( titleIdent
  , lowerIdent
  , readHexByte
  ) where

import qualified Data.Char as DC
import qualified RIO.Char  as C
import qualified RIO.Text  as T
import           RIO


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

-- | A byte printed as exactly two hex digits, either case: @0B@, @ff@.
readHexByte :: Text -> Maybe Word8
readHexByte w = case T.unpack w of
  [a, b] | C.isHexDigit a, C.isHexDigit b -> Just (fromIntegral (DC.digitToInt a * 16 + DC.digitToInt b))
  _                                       -> Nothing
