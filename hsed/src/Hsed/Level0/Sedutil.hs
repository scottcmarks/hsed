{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Level0.Sedutil
Description : sedutil's printed Level 0 (its --query text) read, compared with a decoded response, and rendered as one
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What @sedutil-cli --query@ (1.49.1) prints of a drive's Level 0 Discovery
response, the oracle the M3 fixture @spec/fixtures/disk6-query.txt@ holds
(design 10.2; sedutil has no @--json@): a line naming the device, then for
each feature it knows a heading, @TPer function (0x0001)@, and lines of
@name = value@ pairs separated by commas (one by a full stop), each value
@Y@ or @N@, a decimal or hex number, or a number with another in
parentheses (@8 (4096)@).

'compareQuery' holds a decoded response to every value sedutil printed (its
@OPAL 2.15@ heading is sedutil's arithmetic, not the drive's version,
DtaDev.cpp:302); 'synthesize' renders a drive-like response from them, a
test aid until the drive's own is captured.

The bodies are milestone M3's group G2's; until it lands they are stubs.
-}
module Hsed.Level0.Sedutil
  ( SedutilQuery (..)
  , SedutilFeature (..)
  , parseQuery
  , compareQuery
  , synthesize
  ) where

import           RIO

import           Hsed.Level0 (Level0)


-- | sedutil's printed Level 0: the device line as printed, and each
--   feature's heading and values.
data SedutilQuery = SedutilQuery
  { sqDevice   :: !Text
  , sqFeatures :: ![SedutilFeature]
  }
  deriving (Eq, Show)

-- | One feature as sedutil prints it: the code its heading names, the
--   heading's words before it, and each value by its printed name (@Y@ 1,
--   @N@ 0, a number as itself, @8 (4096)@ 8).
data SedutilFeature = SedutilFeature
  { sfCode   :: !Word16
  , sfName   :: !Text
  , sfValues :: ![(Text, Integer)]
  }
  deriving (Eq, Show)

-- | sedutil's @--query@ text read.
parseQuery :: Text -> Either Text SedutilQuery
parseQuery = error "G2: parseQuery"

-- | Each value sedutil printed that a decoded response does not hold, said
--   in words; none when they agree.
compareQuery :: SedutilQuery -> Level0 -> [Text]
compareQuery = error "G2: compareQuery"

-- | A drive-like response from sedutil's values (and a Configurable
--   Namespace Locking descriptor, which sedutil does not print).
synthesize :: SedutilQuery -> ByteString
synthesize = error "G2: synthesize"
