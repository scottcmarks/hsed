{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim.Gen
Description : QuickCheck generators of TPer states and requests, for the laws
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What the laws of "Hsed.Law.Sync" quantify over: states of the simulated
TPer, requests on their static ComIDs, and IF-SEND payloads that ask
nothing or name no session the TPer has. Generators ('Gen' values), not
'Test.QuickCheck.Arbitrary' instances, so that no type here or in the codec
gets an orphan instance (design 12).

The generators are milestone M3's group G1's; until it lands they are stubs.
-}
module Hsed.Sim.Gen
  ( genState
  , genStaticRequest
  , genNoActionPayload
  , genUnresolvablePayload
  ) where

import           RIO
import           Test.QuickCheck (Gen)

import           Hsed.Sim.Types  (TPerState)
import           Hsed.Wire       (Request)


-- | A TPer of the default parameters' static ComIDs, each awaiting IF-SEND,
--   processing or awaiting IF-RECV, with response packets prepared.
genState :: Gen TPerState
genState = error "G1: genState"

-- | An IF-SEND or IF-RECV on one of a state's static ComIDs, of a length its
--   unit allows.
genStaticRequest :: TPerState -> Gen Request
genStaticRequest = error "G1: genStaticRequest"

-- | A ComPacket on a ComID that asks nothing: no packets, packets without
--   subpackets, or data subpackets empty or all Empty atoms (Core 3.3.10.6
--   S2:S1 d, PDF 85-86).
genNoActionPayload :: Word16 -> Gen ByteString
genNoActionPayload = error "G1: genNoActionPayload"

-- | A ComPacket on a ComID whose packet names a session the TPer does not
--   have (Core 3.3.10.7 a, PDF 86).
genUnresolvablePayload :: Word16 -> Gen ByteString
genUnresolvablePayload = error "G1: genUnresolvablePayload"
