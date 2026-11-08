{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sha256
Description : SHA-256 of a message: a capture's, for its sidecar; a simulator's setup, for a hub's hello
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SHA-256 (FIPS 180-4) of the few kilobytes a Level 0 capture holds, for the
sidecar beside it ("Hsed.Jig.Capture"), and since M5c of what the
simulator's setup is made of, which an mh_transport hub publishes in its
hello ("Hsed.Sim.View".'Hsed.Sim.View.setupOf'): cryptonite's
(@Crypto.Hash@, in the lts-21.21 snapshot, which hsed-spec hashes the IR's
cells with), not one written by hand (decided at the M3 review by the lead,
design 13.29 (e)). The tests hold it to the FIPS examples and to another
implementation's digests about a block's padding boundary. Moved here from
"Hsed.Jig.Sha256", which re-exports it, at M5c, so that hsed-sim imports no
module of the jig (design 11 M5c).
-}
module Hsed.Sha256
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
