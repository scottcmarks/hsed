{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Level0
Description : The simulated TPer's Level 0 Discovery response
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The simulator's answer to IF-RECV on protocol 1, ComID 1 (Core 3.3.6.2,
PDF 67): its header and its descriptors in increasing code order (3.3.6.3,
PDF 68), exactly the bytes the transfer length asks for, truncated or
zero-padded. Outside the synchronous protocol (3.3.10.3 b, PDF 84): it
changes no state.

Until group G2 writes the descriptors ('simDescriptors'), 'level0Response'
answers with the header alone, its Length of Parameter Data the header's
own bytes after that field, so that the transports and their tests can ask
for Level 0 of any length.
-}
module Hsed.Sim.Level0
  ( level0Response
  , simDescriptors
  ) where

import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO

import           Hsed.Core.Layout      (Field (..), Layout (..), fieldEnd)
import           Hsed.Core.Record      (DecodedRecord (..), LayoutRecord (..), decodeRecord, encodeRecord)
import           Hsed.Gen.Core.Layouts (Level0DiscoveryHeader (..))
import           Hsed.Level0           (Descriptor)
import           Hsed.Sim.Types        (TPerState)


-- | The response to a Level 0 IF-RECV of a transfer length: exactly that
--   many bytes.
level0Response :: TPerState -> Word32 -> ByteString
level0Response _ len = B.take n (header <> B.replicate n 0)
  where
    n      = fromIntegral len
    layout = recordLayout (Proxy :: Proxy Level0DiscoveryHeader)
    -- Length of Parameter Data counts the bytes after itself (Core 3.3.6.2.1, PDF 68): of a header alone, the
    -- header's size less the field's end (Table 39's layout); the rest of the header, zeros, decoded from zeros
    lengthEnd = maybe 0 ((`div` 8) . fieldEnd) (L.find ((== "Length of Parameter Data") . fieldName) (layoutFields layout))
    header = either (const mempty) id $ do
      DecodedRecord zeros _ _ <- decodeRecord (B.replicate (layoutSize layout) 0)
      encodeRecord zeros { level0DiscoveryHeaderLengthOfParameterData = fromIntegral (layoutSize layout - lengthEnd) }

-- | The simulator's descriptors, in increasing code order: Opal's and the
--   hand-written feature sets' ("Hsed.Sim.FeatureSets"), their values from
--   the parameters and the state, and a profile's codes the simulator lacks.
simDescriptors :: TPerState -> [Descriptor]
simDescriptors = error "G2: simDescriptors"
