{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Core.Feature
Description : The runtime form of a feature descriptor: a record of its fields, its code, version, length and layout
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The back end of the @feature@ rule (design docs/spec-as-source.md example E8).
A feature descriptor is a byte grid captioned @… Feature Descriptor@ (Core
Tables 42 and 43): a header in the general format its spec defines (Core Table
41: a code, a version, reserved bits and a length), then the feature's own
fields. The spec compiler makes each a record, its flags 'Bool' fields, its
other numbers 'Natural' ones, and an instance of 'Feature':

* 'featureCode', the code the header's first field holds, which identifies
  the descriptor among the others of a Level 0 Discovery response; and the
  version and the length the header holds, when the spec pins them
  ('featureVersion', 'featureLength': the length counts the bytes after the
  header);
* 'featureLayout', the descriptor as a "Hsed.Core.Layout" 'Layout': its
  header pinned, its reserved bits, its fields at the bits the grid prints
  them over;
* 'featureValues' and 'featureFromValues', the record's fields by the names
  the layout gives them, which 'encodeFeature' and 'decodeFeature' pass to and
  take from 'encode' and 'decode'.

A pinned field (the header's, and any other the spec pins) and reserved bits
are no fields of the record: they are written as the layout has them and, read
as anything else, reported ('decodeFeature') or refused
('decodeFeatureStrict': how a TPer's own descriptor is held to what it SHALL
send).
-}
module Hsed.Core.Feature
  ( -- * Feature descriptors
    Feature (..)
  , DecodedFeature (..)
  , encodeFeature
  , decodeFeature
  , decodeFeatureStrict
    -- * The bit map
  , bitMap
    -- * A record's fields by name, for the generated instances
  , flag
  , flagOf
  , number
  , numberOf
  , octets
  , octetsOf
  ) where

import qualified RIO.List         as L
import           RIO

import           Hsed.Core.Layout (Content (..), Decoded (..), Difference, Field (..), Layout (..), LayoutError (..),
                                   bitOffset, decode, decodeStrict, encode)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hsed.Core.Layout (FieldKind (..), spanning)
-- >>> let small = Layout "Small" 2 [spanning "Code" (0, 7) (0, 4) (Pinned 3), spanning "Reserved" (0, 3) (0, 1) Reserved, spanning "Ready" (0, 0) (0, 0) Flag, spanning "Count" (1, 7) (1, 0) Unsigned] Nothing


-- * Feature descriptors

-- | A feature descriptor's record. Use the class's constants with a type
--   application: @featureCode \@TPerFeature@.
class Feature f where
  -- | The code the header's first field holds: which feature the descriptor describes.
  featureCode :: Integer
  -- | The version the header holds, when the spec pins one.
  featureVersion :: Maybe Integer
  -- | The length the header holds, the bytes of the descriptor after its header, when the spec pins it.
  featureLength :: Maybe Integer
  -- | The descriptor: the header and every pinned field as 'Hsed.Core.Layout.Pinned', reserved bits, the record's fields.
  featureLayout :: Layout
  -- | The record's fields by their names in the layout, as 'encode' takes them.
  featureValues :: f -> [(Text, Content)]
  -- | The record from the values of the layout's fields by name, as 'decode' gives them.
  featureFromValues :: [(Text, Content)] -> Either LayoutError f

-- | A descriptor read from the start of an input: the record, what differs
--   from the layout's pins and reserved bits, and the input after it.
data DecodedFeature f = DecodedFeature
  { decodedFeature     :: !f
  , featureDifferences :: ![Difference]
  , featureRest        :: !ByteString
  }
  deriving (Eq, Show)

-- | A descriptor's bytes: its pins as pinned, its reserved bits zeros, its
--   fields the record's.
encodeFeature :: forall f. Feature f => f -> Either LayoutError ByteString
encodeFeature = encode (featureLayout @f) . featureValues

-- | A descriptor from the start of an input, a pin read with another value
--   and reserved bits read other than zero reported ('decode'): how a host
--   reads what a drive sent.
decodeFeature :: forall f. Feature f => ByteString -> Either LayoutError (DecodedFeature f)
decodeFeature = decodedWith (decode (featureLayout @f))

-- | 'decodeFeature', refusing a pin read with another value ('decodeStrict').
decodeFeatureStrict :: forall f. Feature f => ByteString -> Either LayoutError (DecodedFeature f)
decodeFeatureStrict = decodedWith (decodeStrict (featureLayout @f))

decodedWith :: Feature f => (ByteString -> Either LayoutError Decoded) -> ByteString -> Either LayoutError (DecodedFeature f)
decodedWith reader input = do
  d <- reader input
  f <- featureFromValues (decodedFields d)
  pure (DecodedFeature f (decodedDifferences d) (decodedRest d))


-- * The bit map

-- | Each bit of a layout's fixed part, byte 0 first and bit 7 first in each
--   byte, with the field it lies in (none for a bit no field covers).
--
-- >>> [ (pos, fieldName f) | (pos, Just f) <- bitMap small, fst pos == 0 ]
-- [((0,7),"Code"),((0,6),"Code"),((0,5),"Code"),((0,4),"Code"),((0,3),"Reserved"),((0,2),"Reserved"),((0,1),"Reserved"),((0,0),"Ready")]
bitMap :: Layout -> [((Int, Int), Maybe Field)]
bitMap l =
  [ ((byte, bit), L.find (covers (bitOffset byte bit)) (layoutFields l))
  | byte <- [0 .. layoutSize l - 1], bit <- [7, 6 .. 0] ]
  where covers o f = fieldStart f <= o && o < fieldStart f + fieldWidth f


-- * A record's fields by name

-- | A flag's value, by its field's name: 1 for True.
--
-- >>> map (flag "Ready") [False, True]
-- [("Ready",Number 0),("Ready",Number 1)]
flag :: Text -> Bool -> (Text, Content)
flag name b = (name, Number (if b then 1 else 0))

-- | A flag among a layout's values by name.
--
-- >>> map (flagOf "Ready") [[("Ready", Number 1)], [("Ready", Number 2)], [("Count", Number 1)]]
-- [Right True,Left (OutOfRange "Ready" 2),Left (MissingField "Ready")]
flagOf :: Text -> [(Text, Content)] -> Either LayoutError Bool
flagOf name vs = case L.lookup name vs of
  Just (Number 0) -> Right False
  Just (Number 1) -> Right True
  Just (Number v) -> Left (OutOfRange name v)
  Just (Octets _) -> Left (WrongContent name)
  Nothing         -> Left (MissingField name)

-- | An unsigned number's value, by its field's name.
number :: Text -> Natural -> (Text, Content)
number name n = (name, Number (toInteger n))

-- | An unsigned number among a layout's values by name.
--
-- >>> numberOf "Count" [("Ready", Number 1), ("Count", Number 7)]
-- Right 7
numberOf :: Text -> [(Text, Content)] -> Either LayoutError Natural
numberOf name vs = case L.lookup name vs of
  Just (Number v)
    | v >= 0      -> Right (fromInteger v)
    | otherwise   -> Left (OutOfRange name v)
  Just (Octets _) -> Left (WrongContent name)
  Nothing         -> Left (MissingField name)

-- | A byte string's value, by its field's name.
octets :: Text -> ByteString -> (Text, Content)
octets name b = (name, Octets b)

-- | A byte string among a layout's values by name.
octetsOf :: Text -> [(Text, Content)] -> Either LayoutError ByteString
octetsOf name vs = case L.lookup name vs of
  Just (Octets b) -> Right b
  Just (Number _) -> Left (WrongContent name)
  Nothing         -> Left (MissingField name)
