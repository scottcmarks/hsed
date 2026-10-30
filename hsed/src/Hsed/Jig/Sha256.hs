{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Sha256
Description : SHA-256 of a capture, for its sidecar
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SHA-256 (FIPS 180-4, section 6.2) of the few kilobytes a Level 0 capture
holds, for the sidecar beside it ("Hsed.Jig.Capture"): the library has no
cryptographic package among its dependencies, and needs no speed. The
constants are section 4.2.2's and 5.3.3's; the tests hold the function to
the FIPS examples.
-}
module Hsed.Jig.Sha256
  ( sha256
  , sha256Hex
  ) where

import           Data.Bits              (complement, rotateR, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString.Base16 as B16
import qualified RIO.ByteString         as B
import           RIO
import qualified RIO.List               as L


-- | The 32-byte digest of a message.
sha256 :: ByteString -> ByteString
sha256 = digest . L.foldl' compress initial . blocks . padded

-- | The digest in lower-case hex, 64 digits.
sha256Hex :: ByteString -> Text
sha256Hex = decodeUtf8Lenient . B16.encode . sha256

-- | The eight words of the hash.
data Hash = Hash !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32

-- | H(0): the first 32 bits of the fractional parts of the square roots of
--   the first 8 primes (5.3.3).
initial :: Hash
initial = Hash 0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a 0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19

-- | K: the first 32 bits of the fractional parts of the cube roots of the
--   first 64 primes (4.2.2).
roundConstants :: [Word32]
roundConstants =
  [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
  , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
  , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
  , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
  , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
  , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
  , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
  , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  ]

-- | The message padded (5.1.1): a one bit, zeros to 56 bytes modulo 64, its
--   length in bits as 64 bits, big-endian.
padded :: ByteString -> ByteString
padded m = m <> B.singleton 0x80 <> B.replicate zeros 0 <> bigEndian 8 (8 * toInteger (B.length m))
  where zeros = (55 - B.length m) `mod` 64

-- | The padded message's 512-bit blocks (5.2.1), each as 16 big-endian words.
blocks :: ByteString -> [[Word32]]
blocks = map (map word . chunks 4) . chunks 64
  where
    chunks n b = if B.null b then [] else B.take n b : chunks n (B.drop n b)
    word = B.foldl' (\w byte -> w `shiftL` 8 .|. fromIntegral byte) 0

-- | One block into the hash (6.2.2): the 64 rounds over the message
--   schedule, then each word added.
compress :: Hash -> [Word32] -> Hash
compress h block = add h (L.foldl' step h (zip roundConstants (schedule block)))
  where
    step (Hash a b c d e f g hh) (k, w) = Hash (t1 + t2) a b c (d + t1) e f g
      where
        t1 = hh + bigSigma1 e + (e .&. f `xor` complement e .&. g) + k + w
        t2 = bigSigma0 a + (a .&. b `xor` a .&. c `xor` b .&. c)
    add (Hash a b c d e f g hh) (Hash a' b' c' d' e' f' g' hh') =
      Hash (a + a') (b + b') (c + c') (d + d') (e + e') (f + f') (g + g') (hh + hh')

-- | The message schedule W(0) to W(63) of a block (6.2.2 step 1).
schedule :: [Word32] -> [Word32]
schedule block = take 64 ws
  where ws = block <> L.zipWith4 (\w2 w7 w15 w16 -> sigma1 w2 + w7 + sigma0 w15 + w16) (drop 14 ws) (drop 9 ws) (drop 1 ws) ws

bigSigma0, bigSigma1, sigma0, sigma1 :: Word32 -> Word32
bigSigma0 x = rotateR x 2 `xor` rotateR x 13 `xor` rotateR x 22
bigSigma1 x = rotateR x 6 `xor` rotateR x 11 `xor` rotateR x 25
sigma0 x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3
sigma1 x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10

-- | The hash's words, big-endian.
digest :: Hash -> ByteString
digest (Hash a b c d e f g h) = mconcat (map (bigEndian 4 . toInteger) [a, b, c, d, e, f, g, h])

-- | A number as so many bytes, big-endian.
bigEndian :: Int -> Integer -> ByteString
bigEndian n x = B.pack [ fromInteger (x `shiftR` (8 * i)) | i <- [n - 1, n - 2 .. 0] ]
