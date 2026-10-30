{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Core.Record
Description : A Haskell record whose fields are a byte layout's: encoded and decoded through it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The typed side of a byte layout ("Hsed.Core.Layout"). The @byteGrid@ rule
(design docs/spec-as-source.md example E7) generates, beside each byte grid's
'Layout', a record with one field for each field of the layout that carries a
value (an unsigned number as the narrowest of 'Word8' to 'Word64' that holds
it, a flag as 'Bool', a byte string as 'ByteString'), and an instance of
'LayoutRecord' that names the layout and maps the record's fields to the
layout's by name. What the layout fixes is not in the record: reserved bits
(written as zeros), the constants a TPer SHALL send (pins), and the field
that gives a variable tail's length (computed from the tail when encoding,
used to find it when decoding). A layout with a tail gives two records, the
fixed part's (@ComPacketHeader@) and the whole (@ComPacket@: the header and
the tail's bytes), and the whole is the instance.

'encodeRecord' and 'decodeRecord' are 'Hsed.Core.Layout.encode' and
'Hsed.Core.Layout.decode' through the instance; the helpers 'number',
'flag', 'octets' and 'numberField', 'flagField', 'octetsField' are what a
generated instance is written with.
-}
module Hsed.Core.Record
  ( -- * Records over layouts
    LayoutRecord (..)
  , DecodedRecord (..)
  , encodeRecord
  , decodeRecord
  , decodeRecordStrict
    -- * Writing an instance
  , number
  , flag
  , octets
  , numberField
  , flagField
  , octetsField
  ) where

import qualified RIO.List         as L
import           RIO

import           Hsed.Core.Layout (Content (..), Decoded (..), Difference, Layout, LayoutError (..), decode,
                                   decodeStrict, encode)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hsed.Core.Layout (FieldKind (..), Layout (..), byteRange, spanning)
-- >>> :{
-- data Header = Header { hVersion :: Word8, hBusy :: Bool } deriving (Eq, Show)
-- instance LayoutRecord Header where
--   recordLayout _ = Layout "Header" 2 [ spanning "Version" (0, 7) (0, 4) Unsigned, spanning "Busy" (0, 3) (0, 3) Flag
--                                      , spanning "Reserved" (0, 2) (0, 0) Reserved, byteRange "Code" 1 1 (Pinned 0x2a) ] Nothing
--   recordFields r = [("Version", number (hVersion r)), ("Busy", flag (hBusy r))]
--   fromFields vs = pure Header <*> numberField "Version" vs <*> flagField "Busy" vs
-- :}


-- | A record whose fields are those of a byte layout that carry a value.
class LayoutRecord a where
  -- | The layout.
  recordLayout :: proxy a -> Layout
  -- | The record's values by the names of the layout's fields (a tail's by
  --   the tail's name); none for reserved bits, pins, or the length of a tail.
  recordFields :: a -> [(Text, Content)]
  -- | The record from a decoded layout's values.
  fromFields   :: [(Text, Content)] -> Either LayoutError a

-- | A record decoded from the start of the input: the record, what the input
--   held other than the layout's pins and reserved bits ask, and the input
--   after it.
data DecodedRecord a = DecodedRecord
  { drRecord      :: !a
  , drDifferences :: ![Difference]
  , drRest        :: !ByteString
  }
  deriving (Eq, Show)

-- | A record as its layout's bytes.
--
-- >>> encodeRecord (Header 2 True)
-- Right "(*"
-- >>> encodeRecord (Header 16 False)
-- Left (OutOfRange "Version" 16)
encodeRecord :: forall a. LayoutRecord a => a -> Either LayoutError ByteString
encodeRecord a = encode (recordLayout (Proxy :: Proxy a)) (recordFields a)

-- | A record from the start of the input, a pin read with another value or
--   reserved bits not zero reported as a 'Difference' (how a host reads what
--   a drive sent).
--
-- >>> decodeRecord "(*rest" :: Either LayoutError (DecodedRecord Header)
-- Right (DecodedRecord {drRecord = Header {hVersion = 2, hBusy = True}, drDifferences = [], drRest = "rest"})
-- >>> drDifferences <$> (decodeRecord "\x2f\x2b" :: Either LayoutError (DecodedRecord Header))
-- Right [ReservedNotZero "Reserved" 7,PinDiffers "Code" 42 43]
decodeRecord :: forall a. LayoutRecord a => ByteString -> Either LayoutError (DecodedRecord a)
decodeRecord = decodeWith decode

-- | 'decodeRecord', refusing a pin read with another value.
--
-- >>> drRecord <$> (decodeRecordStrict "\x2f\x2b" :: Either LayoutError (DecodedRecord Header))
-- Left (PinViolated "Code" 42 43)
decodeRecordStrict :: forall a. LayoutRecord a => ByteString -> Either LayoutError (DecodedRecord a)
decodeRecordStrict = decodeWith decodeStrict

decodeWith :: forall a. LayoutRecord a => (Layout -> ByteString -> Either LayoutError Decoded) -> ByteString -> Either LayoutError (DecodedRecord a)
decodeWith dec input = do
  d <- dec (recordLayout (Proxy :: Proxy a)) input
  r <- fromFields (decodedFields d)
  pure (DecodedRecord r (decodedDifferences d) (decodedRest d))


-- * Writing an instance

-- | An unsigned field's value.
number :: Integral n => n -> Content
number = Number . toInteger

-- | A flag's value: 1 for True.
flag :: Bool -> Content
flag b = Number (if b then 1 else 0)

-- | A byte string's value (a field of whole bytes, or a tail).
octets :: ByteString -> Content
octets = Octets

-- | An unsigned field's value among a decoded layout's, as the record's type
--   (which the generator chose wide enough for the field).
--
-- >>> numberField "Version" [("Version", Number 2)] :: Either LayoutError Word8
-- Right 2
-- >>> numberField "Length" [("Version", Number 2)] :: Either LayoutError Word8
-- Left (MissingField "Length")
numberField :: Num n => Text -> [(Text, Content)] -> Either LayoutError n
numberField name vs = case L.lookup name vs of
  Just (Number v) -> Right (fromInteger v)
  Just (Octets _) -> Left (WrongContent name)
  Nothing         -> Left (MissingField name)

-- | A flag's value among a decoded layout's.
flagField :: Text -> [(Text, Content)] -> Either LayoutError Bool
flagField name vs = (/= (0 :: Integer)) <$> numberField name vs

-- | A byte string's value among a decoded layout's.
octetsField :: Text -> [(Text, Content)] -> Either LayoutError ByteString
octetsField name vs = case L.lookup name vs of
  Just (Octets b) -> Right b
  Just (Number _) -> Left (WrongContent name)
  Nothing         -> Left (MissingField name)
