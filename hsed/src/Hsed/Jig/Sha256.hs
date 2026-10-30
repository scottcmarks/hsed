{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Sha256
Description : SHA-256 of a capture, for its sidecar
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SHA-256 (FIPS 180-4) of the few kilobytes a Level 0 capture holds, for the
sidecar beside it ("Hsed.Jig.Capture"): cryptonite's (@Crypto.Hash@, in the
lts-21.21 snapshot, which hsed-spec hashes the IR's cells with), not one
written by hand (decided at the M3 review by the lead, design 13.29 (e)).
The tests hold it to the FIPS examples and to another implementation's
digests about a block's padding boundary.
-}
module Hsed.Jig.Sha256
  ( sha256
  , sha256Hex
  ) where

import           Crypto.Hash            (SHA256 (..), hashWith)
import qualified Data.ByteArray         as BA
import qualified Data.ByteString.Base16 as B16
import           RIO


-- | The 32-byte digest of a message.
sha256 :: ByteString -> ByteString
sha256 = BA.convert . hashWith SHA256

-- | The digest in lower-case hex, 64 digits.
sha256Hex :: ByteString -> Text
sha256Hex = decodeUtf8Lenient . B16.encode . sha256
