{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Sha256Spec
Description : Test specifications for Hsed.Jig.Sha256
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The capture sidecar's SHA-256 against the examples of FIPS 180-4's
publication (one block, two blocks, a million bytes) and against digests
another implementation (Python's hashlib) gave for the lengths around a
block's padding boundary (55 to 65 bytes, and 119 and 120).
-}
module Hsed.Jig.Sha256Spec (spec) where

import qualified RIO.ByteString  as B
import           RIO
import           Test.Hspec

import           Hsed.Jig.Sha256 (sha256, sha256Hex)


spec :: Spec
spec = describe "Hsed.Jig.Sha256" $ do
  it "gives FIPS 180-4's examples" $ do
    sha256Hex "" `shouldBe` "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    sha256Hex "abc" `shouldBe` "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    sha256Hex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
      `shouldBe` "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    sha256Hex ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopq"
               <> "klmnopqrlmnopqrsmnopqrstnopqrstu")
      `shouldBe` "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"
  it "gives a million 'a's FIPS 180-4's digest" $
    sha256Hex (B.replicate 1000000 0x61) `shouldBe` "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
  it "pads at a block's boundary as another implementation does" $
    map (\n -> (n, sha256Hex (B.pack (map fromIntegral [0 .. n - 1])))) [55, 56, 63, 64, 65, 119, 120]
      `shouldBe`
        [ (55 :: Int, "463eb28e72f82e0a96c0a4cc53690c571281131f672aa229e0d45ae59b598b59")
        , (56, "da2ae4d6b36748f2a318f23e7ab1dfdf45acdc9d049bd80e59de82a60895f562")
        , (63, "29af2686fd53374a36b0846694cc342177e428d1647515f078784d69cdb9e488")
        , (64, "fdeab9acf3710362bd2658cdc9a29e8f9c757fcf9811603a8c447cd1d9151108")
        , (65, "4bfd2c8b6f1eec7a2afeb48b934ee4b2694182027e6d0fc075074f2fabb31781")
        , (119, "da18797ed7c3a777f0847f429724a2d8cd5138e6ed2895c3fa1a6d39d18f7ec6")
        , (120, "f52b23db1fbb6ded89ef42a23ce0c8922c45f25c50b568a93bf1c075420bbb7c")
        ]
  it "is 32 bytes" $
    B.length (sha256 "hsed") `shouldBe` 32
