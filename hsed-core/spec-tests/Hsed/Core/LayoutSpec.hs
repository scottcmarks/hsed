{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Core.LayoutSpec
Description : Test specifications for Hsed.Core.Layout
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Round trips over random layouts that tile (fields of every kind at any bit
position and width, a tail with or without a length field, or a reserved
tail): decoding what
was encoded gives the values back, encoding what was decoded gives the bytes
back; a pin read with another value is a difference to 'decode' and an error
to 'decodeStrict', each difference at its field's first bit, a reserved
tail's at its first byte other than zero; a field taken away leaves a gap,
one added twice an overlap. Then the tails: padding and a reserved tail
written as zeros and, read otherwise, reported; a reserved tail taking no
value, and a long one (the rest of a large transfer) read in linear time; a
length past the input, however large, refused; a
padded tail without a length field refused; and 'matching' telling layouts
apart by their pins. The layouts are random, not the spec's: the spec's are
generated (@Hsed.Gen.\<Spec\>.Layouts@) and tested in hsed.
-}
module Hsed.Core.LayoutSpec (spec) where

import           Data.Bits             (complementBit, xor)
import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Hsed.Core.Layout


-- | A layout whose fields tile its fixed part, with values for it: each field
--   a random kind and width, in order; and a tail, possibly with its length
--   in a one-byte field of its own at the end of the fixed part, or reserved.
data Tiled = Tiled !Layout ![(Text, Content)] !ByteString
  deriving (Show)

-- | What follows a random layout's fixed part.
data TailChoice = NoTail | RestOfInput | WithLength | ReservedRest
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary Tiled where
  arbitrary = do
    nBytes <- chooseInt (1, 8)
    widths <- partition (nBytes * 8)
    fields <- traverse field (L.zip3 [0 :: Int ..] (L.scanl (+) 0 widths) widths)
    tailChoice <- elements [minBound .. maxBound]
    payload <- B.pack <$> (chooseInt (0, 9) >>= vector)
    align <- elements [1, 4]
    let lengthField = Field "length" (nBytes * 8) 8 Unsigned
        (size, fs, tl) = case tailChoice of
          NoTail       -> (nBytes, fields, Nothing)
          RestOfInput  -> (nBytes, fields, Just (Tail "tail" Nothing 1 Bytes))
          WithLength   -> (nBytes + 1, fields <> [lengthField], Just (Tail "tail" (Just "length") align Bytes))
          ReservedRest -> (nBytes, fields, Just (Tail "tail" Nothing 1 Reserved))
        layout' = Layout "example" size fs tl
    values <- concat <$> traverse (valueFor tl payload) fs
    pure (Tiled layout' (values <> [ ("tail", Octets payload) | Just t <- [tl], tailKind t /= Reserved ]) payload)
    where
      -- widths summing to n, none wider than 24
      partition n
        | n <= 0    = pure []
        | otherwise = do
            w <- chooseInt (1, min n 24)
            (w :) <$> partition (n - w)
      field (i, start, w) = do
        kind <- frequency
          [ (4, pure Unsigned)
          , (if w == 1 then 3 else 0, pure Flag)
          , (if start `mod` 8 == 0 && w `mod` 8 == 0 then 3 else 0, pure Bytes)
          , (2, pure Reserved)
          , (2, Pinned <$> chooseInteger (0, 2 ^ w - 1)) ]
        pure (Field ("f" <> tshow i) start w kind)
      valueFor tl payload f = case fieldKind f of
        Reserved -> pure []
        Pinned p -> pure [(fieldName f, Number p)]
        Bytes    -> (\ws -> [(fieldName f, Octets (B.pack ws))]) <$> vector (fieldWidth f `div` 8)
        _ | Just (Tail _ (Just lf) _ _) <- tl, lf == fieldName f -> pure [(lf, Number (toInteger (B.length payload)))]
          | otherwise -> (\v -> [(fieldName f, Number v)]) <$> chooseInteger (0, 2 ^ fieldWidth f - 1)

spec :: Spec
spec = describe "Hsed.Core.Layout" $ do
  describe "a layout whose fields tile its bytes" $ do
    prop "tiles" $ \(Tiled l _ _) -> tiling l `shouldBe` []
    prop "decodes what it encodes: the values, in field order, then the tail" $ \(Tiled l vs _) ->
      (encode l vs >>= decode l) `shouldBe` Right (Decoded vs [] mempty)
    prop "encodes what it decodes, byte for byte" $ \(Tiled l vs _) ->
      case encode l vs of
        Left e      -> expectationFailure (show e)
        Right bytes -> (decode l bytes >>= encode l . decodedFields) `shouldBe` Right bytes
    prop "fills in the tail's length field when it is not given" $ \(Tiled l vs _) ->
      let lengthField = [ lf | Just (Tail _ (Just lf) _ _) <- [layoutTail l] ]
      in encode l [ v | v@(n, _) <- vs, n `notElem` lengthField ] `shouldBe` encode l vs
    prop "leaves what follows the layout to the caller" $ \(Tiled l vs _) (rest :: [Word8]) ->
      let following = B.pack rest
      in case layoutTail l of
        Just (Tail _ Nothing _ _) -> pure ()     -- a tail without a length field is the rest of the input
        _ -> fmap decodedRest (encode l vs >>= decode l . (<> following)) `shouldBe` Right following
    prop "reports a pin read with another value, and refuses it when strict" $ \(Tiled l vs _) ->
      case ([ (f, p) | f <- layoutFields l, Pinned p <- [fieldKind f] ], encode l vs) of
        ((f, p) : _, Right bytes) -> do
          let (byte, bit) = bitPosition (fieldStart f)
              flipped = B.pack [ if i == byte then complementBit w bit else w | (i, w) <- zip [0 ..] (B.unpack bytes) ]
              read' = p `xor` (2 ^ (fieldWidth f - 1))
          fmap decodedDifferences (decode l flipped) `shouldBe` Right [PinDiffers (fieldName f) (fieldStart f) p read']
          decodeStrict l flipped `shouldBe` Left (PinViolated (fieldName f) p read')
        _ -> pure ()
    prop "reports reserved bits read other than zero, strict or not" $ \(Tiled l vs _) ->
      case ([ f | f <- layoutFields l, fieldKind f == Reserved ], encode l vs) of
        (f : _, Right bytes) -> do
          let (byte, bit) = bitPosition (fieldStart f)
              raised = B.pack [ if i == byte then complementBit w bit else w | (i, w) <- zip [0 ..] (B.unpack bytes) ]
              expected = [ReservedNotZero (fieldName f) (fieldStart f) (2 ^ (fieldWidth f - 1))]
          fmap decodedDifferences (decode l raised) `shouldBe` Right expected
          fmap decodedDifferences (decodeStrict l raised) `shouldBe` Right expected
        _ -> pure ()
    prop "reports a reserved tail's first byte other than zero, at its offset, strict or not" $ \(Tiled l vs _) (rest :: [Word8]) ->
      case (layoutTail l, encode l vs) of
        (Just t, Right bytes) | tailKind t == Reserved -> do
          let expected = [ ReservedNotZero (tailName t) ((layoutSize l + i) * 8) (toInteger w) | (i, w) <- take 1 (filter ((/= 0) . snd) (zip [0 ..] rest)) ]
          fmap decodedDifferences (decode l (bytes <> B.pack rest)) `shouldBe` Right expected
          fmap decodedDifferences (decodeStrict l (bytes <> B.pack rest)) `shouldBe` Right expected
        _ -> pure ()
    prop "leaves a gap where a field is taken away, and an overlap where one is added twice" $ \(Tiled l _ _) ->
      case layoutFields l of
        f : rest -> do
          tiling l { layoutFields = rest } `shouldSatisfy` any (== Gap (fieldStart f) (fieldEnd f - 1))
          tiling l { layoutFields = f : f : rest } `shouldSatisfy` any isOverlap
        [] -> pure ()

  describe "encode" $ do
    let l = Layout "x" 2 [byteRange "A" 0 0 Unsigned, spanning "B" (1, 7) (1, 7) Flag, spanning "R" (1, 6) (1, 0) Reserved]
                   (Just (Tail "T" (Just "A") 4 Bytes))
    it "pads the tail to its alignment and sets its length field" $
      encode l [("B", Number 1), ("T", Octets "abcde")] `shouldBe` Right "\x05\x80\&abcde\0\0\0"
    it "refuses a length field that disagrees with the tail, a value for reserved bits, a name it does not have" $ do
      encode l [("A", Number 4), ("B", Number 1), ("T", Octets "abcde")] `shouldBe` Left (LengthDisagrees "A" 4 5)
      encode l [("A", Number 5), ("B", Number 1), ("R", Number 0), ("T", Octets "abcde")] `shouldBe` Left (ReservedGiven "R")
      encode l [("B", Number 1), ("C", Number 1)] `shouldBe` Left (UnknownField "C")
    it "refuses a layout that does not tile" $
      encode l { layoutSize = 3 } [("B", Number 1)] `shouldBe` Left (NotTiled [Gap 16 23])

  describe "decode" $ do
    let l = Layout "x" 1 [byteRange "A" 0 0 Unsigned] (Just (Tail "T" (Just "A") 4 Bytes))
    it "needs the tail its length field gives, padding included" $ do
      decode l "\x05\&abcde\0\0" `shouldBe` Left (Truncated 9 8)
      fmap decodedFields (decode l "\x05\&abcde\0\0\0") `shouldBe` Right [("A", Number 5), ("T", Octets "abcde")]
    it "reports padding read other than zero (a data subpacket's pad bytes SHALL be zeroes), strict or not" $ do
      fmap decodedDifferences (decode l "\x05\&abcde\0\x09\0") `shouldBe` Right [PaddingNotZero "T" 48 0x0900]
      fmap decodedDifferences (decodeStrict l "\x05\&abcde\0\x09\0") `shouldBe` Right [PaddingNotZero "T" 48 0x0900]
      fmap decodedRest (decode l "\x05\&abcde\0\x09\0next") `shouldBe` Right "next"
    it "refuses a length past the input, however large, rather than wrapping it" $ do
      let wide = Layout "w" 8 [byteRange "N" 0 7 Unsigned] (Just (Tail "T" (Just "N") 1 Bytes))
          huge = B.pack [0x80, 0, 0, 0, 0, 0, 0, 1]
      decode wide (huge <> "abc") `shouldSatisfy` \case
        Left (Truncated _ 11) -> True
        _                     -> False

  describe "a reserved tail (the rest of a transfer)" $ do
    let l = Layout "x" 1 [byteRange "A" 0 0 Unsigned] (Just (Tail "Reserved" Nothing 1 Reserved))
    it "takes no value, and is written as none" $ do
      encode l [("A", Number 1)] `shouldBe` Right "\x01"
      encode l [("A", Number 1), ("Reserved", Octets "\xff")] `shouldBe` Left (ReservedGiven "Reserved")
    it "is no decoded field; its first byte other than zero is reported, alone and at its offset, strict or not" $ do
      decode l "\x01\x00\x00" `shouldBe` Right (Decoded [("A", Number 1)] [] "")
      fmap decodedDifferences (decode l "\x01\x00\xff\x01") `shouldBe` Right [ReservedNotZero "Reserved" 16 0xff]
      fmap decodedDifferences (decodeStrict l "\x01\x00\xff\x01") `shouldBe` Right [ReservedNotZero "Reserved" 16 0xff]
    it "reads a long one (the rest of a large transfer) in linear time, never as one number" $ do
      -- a MiB of zeros, then 7, then more: folding the run into an Integer took seconds per MiB, growing with its square
      let big = "\x01" <> B.replicate (1024 * 1024) 0 <> "\x07" <> B.replicate 4096 0xff
      done <- timeout 5000000 (fmap decodedDifferences (decodeStrict l big) `shouldBe` Right [ReservedNotZero "Reserved" ((1 + 1024 * 1024) * 8) 7])
      done `shouldBe` Just ()

  describe "tiling a tail" $ do
    it "refuses padding without a length field, which could not tell the tail from its padding" $
      tiling (Layout "x" 1 [byteRange "A" 0 0 Unsigned] (Just (Tail "T" Nothing 4 Bytes))) `shouldBe` [PadNoLength "T" 4]
    it "refuses a tail of a kind other than bytes or reserved" $
      tiling (Layout "x" 1 [byteRange "A" 0 0 Unsigned] (Just (Tail "T" Nothing 1 Flag))) `shouldBe` [TailKind "T" Flag]

  describe "differences" $
    it "tell apart two reserved fields of one name by their first bits" $ do
      let l = Layout "x" 2 [spanning "Reserved" (0, 7) (0, 4) Reserved, spanning "A" (0, 3) (0, 0) Unsigned, byteRange "Reserved" 1 1 Reserved] Nothing
      fmap decodedDifferences (decode l "\x10\x00") `shouldBe` Right [ReservedNotZero "Reserved" 0 1]
      fmap decodedDifferences (decode l "\x00\x01") `shouldBe` Right [ReservedNotZero "Reserved" 8 1]

  describe "matching" $
    it "keeps the layouts whose every pin the input holds, a lenient decode's pin difference being no match" $ do
      let reply k = Layout ("reply " <> tshow k) 2 [byteRange "Code" 0 0 (Pinned k), spanning "Reserved" (1, 7) (1, 4) Reserved, spanning "V" (1, 3) (1, 0) Unsigned] Nothing
          input = "\x02\x13"
      fmap decodedDifferences (decode (reply 1) input) `shouldBe` Right [PinDiffers "Code" 0 1 2, ReservedNotZero "Reserved" 8 1]
      [ (layoutName l, decodedDifferences d) | (l, d) <- matching [reply 1, reply 2, reply 3] input ]
        `shouldBe` [("reply 2", [ReservedNotZero "Reserved" 8 1])]
  where
    isOverlap = \case
      Overlap {} -> True
      _          -> False
