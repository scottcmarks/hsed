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
position and width, a tail with or without a length field): decoding what
was encoded gives the values back, encoding what was decoded gives the bytes
back; a pin read with another value is a difference to 'decode' and an error
to 'decodeStrict'; a field taken away leaves a gap, one added twice an
overlap. The layouts are random, not the spec's: the spec's are generated
(@Hsed.Gen.\<Spec\>.Layouts@) and tested in hsed.
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
--   in a one-byte field of its own at the end of the fixed part.
data Tiled = Tiled !Layout ![(Text, Content)] !ByteString
  deriving (Show)

instance Arbitrary Tiled where
  arbitrary = do
    nBytes <- chooseInt (1, 8)
    widths <- partition (nBytes * 8)
    fields <- traverse field (L.zip3 [0 :: Int ..] (L.scanl (+) 0 widths) widths)
    tailKind <- elements [Nothing, Just False, Just True]   -- no tail, a rest-of-input tail, a tail with a length field
    payload <- B.pack <$> (chooseInt (0, 9) >>= vector)
    align <- elements [1, 4]
    let lengthField = Field "length" (nBytes * 8) 8 Unsigned
        (size, fs, tl) = case tailKind of
          Nothing    -> (nBytes, fields, Nothing)
          Just False -> (nBytes, fields, Just (Tail "tail" Nothing 1))
          Just True  -> (nBytes + 1, fields <> [lengthField], Just (Tail "tail" (Just "length") align))
        layout' = Layout "example" size fs tl
    values <- concat <$> traverse (valueFor tl payload) fs
    pure (Tiled layout' (values <> [ ("tail", Octets payload) | isJust tl ]) payload)
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
        _ | Just (Tail _ (Just lf) _) <- tl, lf == fieldName f -> pure [(lf, Number (toInteger (B.length payload)))]
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
      let lengthField = [ lf | Just (Tail _ (Just lf) _) <- [layoutTail l] ]
      in encode l [ v | v@(n, _) <- vs, n `notElem` lengthField ] `shouldBe` encode l vs
    prop "leaves what follows the layout to the caller" $ \(Tiled l vs _) (rest :: [Word8]) ->
      let following = B.pack rest
      in case layoutTail l of
        Just (Tail _ Nothing _) -> pure ()     -- a tail without a length field is the rest of the input
        _ -> fmap decodedRest (encode l vs >>= decode l . (<> following)) `shouldBe` Right following
    prop "reports a pin read with another value, and refuses it when strict" $ \(Tiled l vs _) ->
      case ([ (f, p) | f <- layoutFields l, Pinned p <- [fieldKind f] ], encode l vs) of
        ((f, p) : _, Right bytes) -> do
          let (byte, bit) = bitPosition (fieldStart f)
              flipped = B.pack [ if i == byte then complementBit w bit else w | (i, w) <- zip [0 ..] (B.unpack bytes) ]
              read' = p `xor` (2 ^ (fieldWidth f - 1))
          fmap decodedDifferences (decode l flipped) `shouldBe` Right [PinDiffers (fieldName f) p read']
          decodeStrict l flipped `shouldBe` Left (PinViolated (fieldName f) p read')
        _ -> pure ()
    prop "reports reserved bits read other than zero, strict or not" $ \(Tiled l vs _) ->
      case ([ f | f <- layoutFields l, fieldKind f == Reserved ], encode l vs) of
        (f : _, Right bytes) -> do
          let (byte, bit) = bitPosition (fieldStart f)
              raised = B.pack [ if i == byte then complementBit w bit else w | (i, w) <- zip [0 ..] (B.unpack bytes) ]
              expected = [ReservedNotZero (fieldName f) (2 ^ (fieldWidth f - 1))]
          fmap decodedDifferences (decode l raised) `shouldBe` Right expected
          fmap decodedDifferences (decodeStrict l raised) `shouldBe` Right expected
        _ -> pure ()
    prop "leaves a gap where a field is taken away, and an overlap where one is added twice" $ \(Tiled l _ _) ->
      case layoutFields l of
        f : rest -> do
          tiling l { layoutFields = rest } `shouldSatisfy` any (== Gap (fieldStart f) (fieldEnd f - 1))
          tiling l { layoutFields = f : f : rest } `shouldSatisfy` any isOverlap
        [] -> pure ()

  describe "encode" $ do
    let l = Layout "x" 2 [byteRange "A" 0 0 Unsigned, spanning "B" (1, 7) (1, 7) Flag, spanning "R" (1, 6) (1, 0) Reserved]
                   (Just (Tail "T" (Just "A") 4))
    it "pads the tail to its alignment and sets its length field" $
      encode l [("B", Number 1), ("T", Octets "abcde")] `shouldBe` Right "\x05\x80\&abcde\0\0\0"
    it "refuses a length field that disagrees with the tail, a value for reserved bits, a name it does not have" $ do
      encode l [("A", Number 4), ("B", Number 1), ("T", Octets "abcde")] `shouldBe` Left (LengthDisagrees "A" 4 5)
      encode l [("A", Number 5), ("B", Number 1), ("R", Number 0), ("T", Octets "abcde")] `shouldBe` Left (ReservedGiven "R")
      encode l [("B", Number 1), ("C", Number 1)] `shouldBe` Left (UnknownField "C")
    it "refuses a layout that does not tile" $
      encode l { layoutSize = 3 } [("B", Number 1)] `shouldBe` Left (NotTiled [Gap 16 23])

  describe "decode" $
    it "needs the tail its length field gives, padding included" $ do
      let l = Layout "x" 1 [byteRange "A" 0 0 Unsigned] (Just (Tail "T" (Just "A") 4))
      decode l "\x05\&abcde\0\0" `shouldBe` Left (Truncated 9 8)
      fmap decodedFields (decode l "\x05\&abcde\0\0\0") `shouldBe` Right [("A", Number 5), ("T", Octets "abcde")]
  where
    isOverlap = \case
      Overlap {} -> True
      _          -> False
