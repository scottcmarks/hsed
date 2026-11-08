{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Sha256
Description : SHA-256 of a capture, for its sidecar, and of a secret, for the reports: "Hsed.Sha256"'s, re-exported
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SHA-256 (FIPS 180-4) as the jig uses it: of a Level 0 capture, for the
sidecar beside it ("Hsed.Jig.Capture"), and of a secret the reports give by
its length and digest alone ("Hsed.Jig.Report"). It is "Hsed.Sha256"'s
(cryptonite's, design 13.29 (e)), re-exported here since M5c moved it there,
so that the simulator's setup, which an mh_transport hub publishes by its
digests, is digested without a module of the jig (design 11 M5c).
-}
module Hsed.Jig.Sha256
  ( sha256
  , sha256Hex
  ) where

import           Hsed.Sha256 (sha256, sha256Hex)
