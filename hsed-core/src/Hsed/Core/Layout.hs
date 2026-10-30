{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Core.Layout
Description : The runtime form of a byte layout: fields at bit positions, a fixed size, a variable tail
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The back end of the @byteGrid@, @feature@ and @byteFieldValue@ rules (design
docs/spec-as-source.md section 4.1, example E7): a structure the spec prints as
a grid of bytes and bits (a ComPacket header, a Level 0 Discovery header, a
feature descriptor) or as rows of byte ranges (a ComID management payload) is,
at run time, a 'Layout': named 'Field's, each at a bit position and of a bit
width, over a fixed number of bytes, and optionally a variable 'Tail' whose
length a field gives. The generated module (@Hsed.Gen.\<Spec\>.Layouts@) holds
each such table as a 'Layout' value; this module encodes and decodes them and
checks that their fields tile their bytes.

Bits are counted as the spec's grids print them: byte 0 first, bit 7 the most
significant of each byte. A field's position is the offset of its most
significant bit from bit 7 of byte 0 ('bitOffset'), so a multi-byte number is
big-endian and a field may start and end anywhere within a byte (Core Table
212's IEEE COMPANY ID runs from bit 3 of byte 4 to bit 4 of byte 7).

A field is one of five kinds ('FieldKind'): an unsigned number, a flag, a byte
string, reserved bits, or a constant a TPer SHALL send (a pin, design example
E8). Pins are what the drive must send: 'decode', for a host reading a
drive's reply, reports a pin read with another value as a 'Difference' and
goes on; 'decodeStrict' refuses it. Reserved bits are written as zeros and,
read as anything else, are reported too.
-}
module Hsed.Core.Layout
  ( -- * Fields
    Field (..)
  , FieldKind (..)
  , spanning
  , byteRange
  , fieldEnd
  , bitOffset
  , bitPosition
    -- * Layouts
  , Layout (..)
  , Tail (..)
    -- * Tiling
  , TileProblem (..)
  , tiling
  , tiles
    -- * Encoding and decoding
  , Content (..)
  , Difference (..)
  , Decoded (..)
  , LayoutError (..)
  , encode
  , decode
  , decodeStrict
  ) where

import           Data.Bits      (shiftL, shiftR, (.&.), (.|.))
import qualified RIO.ByteString as B
import qualified RIO.List       as L
import qualified RIO.Map        as Map
import           RIO


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let header = Layout "Header" 3 [spanning "Version" (0, 7) (0, 4) Unsigned, spanning "Reserved" (0, 3) (0, 0) Reserved, byteRange "Length" 1 2 Unsigned] Nothing


-- * Fields

-- | What a field holds.
data FieldKind
  = Unsigned          -- ^ a big-endian unsigned number
  | Flag              -- ^ one bit read as a number, 0 or 1
  | Bytes             -- ^ a byte string: whole bytes, on byte boundaries
  | Reserved          -- ^ written as zeros; read as anything else, a 'Difference'
  | Pinned !Integer   -- ^ a constant the TPer SHALL send: written as it is, read and compared
  deriving (Eq, Ord, Show)

-- | One field: its name as the spec prints it, its first (most significant)
--   bit as an offset from bit 7 of byte 0 ('bitOffset'), its width in bits,
--   and what it holds.
data Field = Field
  { fieldName  :: !Text
  , fieldStart :: !Int
  , fieldWidth :: !Int
  , fieldKind  :: !FieldKind
  }
  deriving (Eq, Ord, Show)

-- | The offset of bit @b@ (7 the most significant) of byte @n@ from bit 7 of byte 0.
--
-- >>> map (uncurry bitOffset) [(0, 7), (0, 0), (4, 3), (7, 4)]
-- [0,7,36,59]
bitOffset :: Int -> Int -> Int
bitOffset byte bit = byte * 8 + (7 - bit)

-- | The byte and bit of an offset: the inverse of 'bitOffset'.
--
-- >>> map bitPosition [0, 7, 36, 59]
-- [(0,7),(0,0),(4,3),(7,4)]
bitPosition :: Int -> (Int, Int)
bitPosition o = (o `div` 8, 7 - o `mod` 8)

-- | A field from its most significant bit to its least, each a @(byte, bit)@
--   as a grid prints them.
--
-- >>> spanning "Version" (2, 7) (2, 4) Unsigned
-- Field {fieldName = "Version", fieldStart = 16, fieldWidth = 4, fieldKind = Unsigned}
-- >>> fieldWidth (spanning "Company" (4, 3) (7, 4) Unsigned)
-- 24
spanning :: Text -> (Int, Int) -> (Int, Int) -> FieldKind -> Field
spanning name (b0, hi) (b1, lo) = Field name start (bitOffset b1 lo - start + 1)
  where start = bitOffset b0 hi

-- | A field of whole bytes, from its first byte to its last: @byteRange
--   "Request Code" 4 7@ is a table's row @4 to 7@.
--
-- >>> byteRange "Length" 1 2 Unsigned
-- Field {fieldName = "Length", fieldStart = 8, fieldWidth = 16, fieldKind = Unsigned}
byteRange :: Text -> Int -> Int -> FieldKind -> Field
byteRange name firstByte lastByte = spanning name (firstByte, 7) (lastByte, 0)

-- | One past a field's last bit.
fieldEnd :: Field -> Int
fieldEnd f = fieldStart f + fieldWidth f


-- * Layouts

-- | What follows a layout's fixed part: a byte string, its length given by a
--   field of the fixed part (or the rest of the input when no field does),
--   padded with zeros to a multiple of an alignment (Core Table 20's payload
--   to four bytes; 1 for none).
data Tail = Tail
  { tailName   :: !Text
  , tailLength :: !(Maybe Text)
  , tailAlign  :: !Int
  }
  deriving (Eq, Show)

-- | A structure of bytes: its name, the size of its fixed part in bytes, the
--   fields of the fixed part, and what follows it.
data Layout = Layout
  { layoutName   :: !Text
  , layoutSize   :: !Int
  , layoutFields :: ![Field]
  , layoutTail   :: !(Maybe Tail)
  }
  deriving (Eq, Show)


-- * Tiling

-- | Why a layout's fields do not tile its fixed part (each bit covered by
--   exactly one field), with offsets as 'bitOffset' counts them.
data TileProblem
  = Overlap !Text !Text !Int  -- ^ two fields share bits, the second starting at this offset inside the first
  | Gap !Int !Int             -- ^ the bits from the first offset to the second, inclusive, that no field covers
  | Outside !Text             -- ^ a field of no width, or not wholly within the fixed part
  | Unaligned !Text           -- ^ a byte-string field not of whole bytes on byte boundaries
  | Unfit !Text !Integer      -- ^ a pinned value the field's width cannot hold
  | WideFlag !Text            -- ^ a flag of more than one bit
  | NoLength !Text            -- ^ the tail's length field is no unsigned field of the layout
  | BadAlign !Int             -- ^ a tail alignment below 1
  deriving (Eq, Show)

-- | Every reason a layout does not tile, in order: fields outside the fixed
--   part, misaligned byte strings, pins that do not fit, flags wider than a
--   bit, then the gaps and overlaps from the first bit to the last, then the
--   tail's.
--
-- >>> tiling header
-- []
-- >>> tiling header { layoutFields = take 2 (layoutFields header) }
-- [Gap 8 23]
-- >>> tiling header { layoutFields = layoutFields header <> [spanning "Late" (2, 0) (2, 0) Flag] }
-- [Overlap "Length" "Late" 23]
-- >>> tiling header { layoutFields = [spanning "Version" (0, 7) (0, 4) (Pinned 16), spanning "Rest" (0, 3) (2, 0) Bytes] }
-- [Unaligned "Rest",Unfit "Version" 16]
tiling :: Layout -> [TileProblem]
tiling l = outside <> unaligned <> unfit <> flags <> coverage <> tailProblems
  where
    total     = layoutSize l * 8
    fs        = layoutFields l
    inside f  = fieldWidth f > 0 && fieldStart f >= 0 && fieldEnd f <= total
    outside   = [ Outside (fieldName f) | f <- fs, not (inside f) ]
    unaligned = [ Unaligned (fieldName f) | f <- fs, fieldKind f == Bytes, fieldStart f `mod` 8 /= 0 || fieldWidth f `mod` 8 /= 0 ]
    unfit     = [ Unfit (fieldName f) v | f <- fs, Pinned v <- [fieldKind f], v < 0 || v >= 2 ^ fieldWidth f ]
    flags     = [ WideFlag (fieldName f) | f <- fs, fieldKind f == Flag, fieldWidth f /= 1 ]
    -- walk the fields in order of their first bit, keeping the furthest bit reached and the field reaching it
    (reach, _, found) = foldl' step (0, "", []) (L.sortOn fieldStart (filter inside fs))
    step (r, owner, acc) f
      | fieldStart f > r = (fieldEnd f, fieldName f, Gap r (fieldStart f - 1) : acc)
      | fieldStart f < r = if fieldEnd f > r then (fieldEnd f, fieldName f, overlap : acc) else (r, owner, overlap : acc)
      | otherwise        = (fieldEnd f, fieldName f, acc)
      where overlap = Overlap owner (fieldName f) (fieldStart f)
    coverage  = reverse found <> [ Gap reach (total - 1) | reach < total ]
    tailProblems = case layoutTail l of
      Nothing -> []
      Just t  -> [ BadAlign (tailAlign t) | tailAlign t < 1 ]
                 <> [ NoLength n | Just n <- [tailLength t], not (any (\f -> fieldName f == n && fieldKind f == Unsigned) fs) ]

-- | Whether every bit of a layout's fixed part is covered by exactly one
--   field (and its tail is sound): what a generated layout asserts.
--
-- >>> tiles header
-- True
tiles :: Layout -> Bool
tiles = null . tiling


-- * Encoding and decoding

-- | A field's value: a number (an unsigned field, a flag, a pin) or bytes (a
--   byte string, the tail).
data Content
  = Number !Integer
  | Octets !ByteString
  deriving (Eq, Show)

-- | What a lenient 'decode' reports and goes on from.
data Difference
  = PinDiffers !Text !Integer !Integer  -- ^ a pinned field read with another value: the pin, the value read
  | ReservedNotZero !Text !Integer      -- ^ reserved bits read other than zero
  deriving (Eq, Show)

-- | A decoded layout: every field's value but the reserved ones', in field
--   order, then the tail's; what differs from the layout's pins and reserved
--   bits; and the input after the layout (after the tail's padding).
data Decoded = Decoded
  { decodedFields      :: ![(Text, Content)]
  , decodedDifferences :: ![Difference]
  , decodedRest        :: !ByteString
  }
  deriving (Eq, Show)

-- | Why a layout cannot be encoded or decoded.
data LayoutError
  = NotTiled ![TileProblem]             -- ^ the layout itself does not tile
  | MissingField !Text                  -- ^ no value for a field that needs one
  | UnknownField !Text                  -- ^ a value for a name the layout does not have
  | OutOfRange !Text !Integer           -- ^ a number the field's width cannot hold
  | WrongLength !Text !Int !Int         -- ^ a byte string of the wrong length: the field's, the one given
  | WrongContent !Text                  -- ^ bytes for a number, or a number for bytes
  | ReservedGiven !Text                 -- ^ a value for reserved bits, which are zeros
  | PinViolated !Text !Integer !Integer -- ^ a pinned field given, or read strictly, with another value: the pin, the value
  | LengthDisagrees !Text !Integer !Int -- ^ the tail's length field given another value than the tail's length
  | Truncated !Int !Int                 -- ^ bytes needed, bytes there
  deriving (Eq, Show)

-- | Encode values, by field name, as a layout's bytes: the fixed part, then the
--   tail and its padding. A reserved field takes no value and is zeros; a pin
--   is written as itself (a value given for it must be the pin); the tail's
--   length field, if not given, is the tail's length.
--
-- >>> encode header [("Version", Number 2), ("Length", Number 0x1234)]
-- Right " \DC24"
-- >>> encode header [("Version", Number 16), ("Length", Number 0)]
-- Left (OutOfRange "Version" 16)
-- >>> encode header [("Version", Number 2)]
-- Left (MissingField "Length")
encode :: Layout -> [(Text, Content)] -> Either LayoutError ByteString
encode l vs = do
  unless (tiles l) (Left (NotTiled (tiling l)))
  traverse_ (\(n, _) -> unless (n `elem` names) (Left (UnknownField n))) vs
  tailBytes <- traverse tailOf (layoutTail l)
  let lengthOf = case (layoutTail l, tailBytes) of
        (Just (Tail _ (Just lf) _), Just bs) -> Just (lf, B.length bs)
        _                                    -> Nothing
  fixed <- foldM (\acc f -> (acc .|.) . place f <$> valueOf lengthOf f) 0 (layoutFields l)
  pure (integerBytes (layoutSize l) fixed <> maybe mempty (padded (maybe 1 tailAlign (layoutTail l))) tailBytes)
  where
    names  = map fieldName (layoutFields l) <> maybe [] (pure . tailName) (layoutTail l)
    given  = Map.fromListWith (\_ old -> old) vs
    total  = layoutSize l * 8
    place f v = v `shiftL` (total - fieldEnd f)
    tailOf t = case Map.lookup (tailName t) given of
      Nothing         -> Right mempty
      Just (Octets b) -> Right b
      Just (Number _) -> Left (WrongContent (tailName t))
    valueOf lengthOf f = case (fieldKind f, Map.lookup (fieldName f) given) of
      (Reserved, Nothing)           -> Right 0
      (Reserved, Just _)            -> Left (ReservedGiven (fieldName f))
      (Pinned p, Nothing)           -> Right p
      (Pinned p, Just (Number v))
        | v == p                    -> Right p
        | otherwise                 -> Left (PinViolated (fieldName f) p v)
      (Bytes, Just (Octets b))
        | B.length b * 8 == fieldWidth f -> Right (bytesInteger b)
        | otherwise                 -> Left (WrongLength (fieldName f) (fieldWidth f `div` 8) (B.length b))
      (Bytes, Just (Number _))      -> Left (WrongContent (fieldName f))
      (_, Just (Octets _))          -> Left (WrongContent (fieldName f))
      (_, Just (Number v))          -> case lengthOf of
        Just (lf, n) | lf == fieldName f, v /= fromIntegral n -> Left (LengthDisagrees lf v n)
        _ | v < 0 || v >= 2 ^ fieldWidth f                     -> Left (OutOfRange (fieldName f) v)
          | otherwise                                          -> Right v
      (_, Nothing)                  -> case lengthOf of
        Just (lf, n) | lf == fieldName f, toInteger n < 2 ^ fieldWidth f -> Right (toInteger n)
                     | lf == fieldName f                                 -> Left (OutOfRange lf (toInteger n))
        _                                                                -> Left (MissingField (fieldName f))
    padded align b = b <> B.replicate ((negate (B.length b)) `mod` align) 0

-- | Decode a layout from the start of the input, reporting a pin read with
--   another value and reserved bits read other than zero ('Difference') rather
--   than refusing them: how a host reads what a drive sent.
--
-- >>> decode header "\x2f\x12\x34rest"
-- Right (Decoded {decodedFields = [("Version",Number 2),("Length",Number 4660)], decodedDifferences = [ReservedNotZero "Reserved" 15], decodedRest = "rest"})
-- >>> decode header "\x20"
-- Left (Truncated 3 1)
decode :: Layout -> ByteString -> Either LayoutError Decoded
decode l input = do
  unless (tiles l) (Left (NotTiled (tiling l)))
  when (B.length input < size) (Left (Truncated size (B.length input)))
  (tailField, rest) <- case layoutTail l of
    Nothing -> Right ([], B.drop size input)
    Just t  -> case tailLength t of
      Nothing -> Right ([(tailName t, Octets (B.drop size input))], mempty)
      Just lf -> do
        let n = maybe 0 fromInteger (L.lookup lf [ (name, v) | (name, Number v) <- values ])
            end = size + n + (negate n `mod` tailAlign t)
        when (B.length input < end) (Left (Truncated end (B.length input)))
        Right ([(tailName t, Octets (B.take n (B.drop size input)))], B.drop end input)
  pure (Decoded (values <> tailField) differences rest)
  where
    size  = layoutSize l
    total = size * 8
    fixed = bytesInteger (B.take size input)
    raw f = (fixed `shiftR` (total - fieldEnd f)) .&. (2 ^ fieldWidth f - 1)
    values = mapMaybe valueOf (layoutFields l)
    valueOf f = case fieldKind f of
      Reserved -> Nothing
      Bytes    -> Just (fieldName f, Octets (B.take (fieldWidth f `div` 8) (B.drop (fieldStart f `div` 8) input)))
      _        -> Just (fieldName f, Number (raw f))
    differences = mapMaybe differs (layoutFields l)
    differs f = case fieldKind f of
      Pinned p | raw f /= p -> Just (PinDiffers (fieldName f) p (raw f))
      Reserved | raw f /= 0 -> Just (ReservedNotZero (fieldName f) (raw f))
      _                     -> Nothing

-- | 'decode', refusing a pin read with another value: how a TPer's own
--   output is held to what it SHALL send. Reserved bits read other than zero
--   are still only reported.
--
-- >>> let pinned = header { layoutFields = spanning "Version" (0, 7) (0, 4) (Pinned 1) : drop 1 (layoutFields header) }
-- >>> decodeStrict pinned "\x20\x00\x00"
-- Left (PinViolated "Version" 1 2)
-- >>> decodedFields <$> decodeStrict pinned "\x10\x00\x05"
-- Right [("Version",Number 1),("Length",Number 5)]
decodeStrict :: Layout -> ByteString -> Either LayoutError Decoded
decodeStrict l input = decode l input >>= \d -> case [ PinViolated n p v | PinDiffers n p v <- decodedDifferences d ] of
  e : _ -> Left e
  []    -> Right d


-- * Bytes and integers

-- | A big-endian unsigned number.
bytesInteger :: ByteString -> Integer
bytesInteger = B.foldl' (\acc w -> acc `shiftL` 8 .|. toInteger w) 0

-- | The low @n@ bytes of a number, big-endian.
integerBytes :: Int -> Integer -> ByteString
integerBytes n v = B.pack [ fromInteger ((v `shiftR` (8 * k)) .&. 0xff) | k <- [n - 1, n - 2 .. 0] ]
