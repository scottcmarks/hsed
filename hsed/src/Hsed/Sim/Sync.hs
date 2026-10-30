{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim.Sync
Description : The synchronous protocol of a static ComID: S1, S2, S3, the three outcomes of an IF-SEND, Table 45's answers to IF-RECV
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The TPer's side of Core 3.3.10 (PDF 82-87) on a static ComID: an IF-SEND
accepted in Awaiting IF-SEND (S1) yields an 'Hsed.Sim.Types.Outcome'
('classifyIfSend'): response packets prepared, the ComID processing (S2)
until released, then awaiting IF-RECV (S3); or the payload discarded, the
ComID kept in S1 (design 8.2). An IF-SEND in S2 or S3 is a synchronous
protocol violation, the state unchanged. An IF-RECV is answered in each
state with the ComPacket header fields Core Table 45 gives (PDF 83,
'ifRecvStatic').

Table 45 is generated ("Hsed.Gen.Core.StateTables".@table45@) and bound by
its comment to this code (@Sync.table45@, the law "Hsed.Law.Sync"). Two of
its state names hold an en dash, so its rows are matched by position:
'T45Row' names them in the order printed, and 't45Rows' is that order over
the generated rows.

The bodies are milestone M3's group G1's; until it lands they are stubs.
-}
module Hsed.Sim.Sync
  ( -- * Core Table 45
    T45Row (..)
  , t45Rows
    -- * The synchronous protocol of a static ComID
  , classifyIfSend
  , ifRecvStatic
  ) where

import           RIO

import           Hsed.Core.Call (MethodCall)
import           Hsed.Sim.Types (ComIdState, Outcome)
import           Hsed.Wire      (Response)


-- | Core Table 45's rows (PDF 83), in the order printed: what the TPer's
--   IF-RECV answer reports of its responses.
data T45Row
  = ResponsesToCome      -- ^ responses to come, none available: (0, 1, 0)
  | InsufficientLength   -- ^ a response ready, the transfer too short for it: (0, all available, the least to transfer it)
  | AdditionalAvailable  -- ^ a response, more available
  | AdditionalToCome     -- ^ a response, more to come, none available
  | AllReturnedWithData  -- ^ a response, all returned, no further data
  | AllReturnedNoData    -- ^ all returned, no further data: (0, 0, 0)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The constructors in the order of the generated table's rows, one for
--   each.
t45Rows :: [T45Row]
t45Rows = error "G1: t45Rows"

-- | What an IF-SEND accepted in S1 on a ComID comes to, given how the
--   Session Manager answers a control session's method call: its response
--   packets prepared, or the payload discarded and why.
classifyIfSend :: (MethodCall -> Maybe MethodCall) -> Word16 -> ByteString -> Outcome
classifyIfSend = error "G1: classifyIfSend"

-- | An IF-RECV of a transfer length on a static ComID in a state: the
--   answer, exactly that many bytes, and the state after it.
ifRecvStatic :: Word16 -> Word32 -> ComIdState -> (Response, ComIdState)
ifRecvStatic = error "G1: ifRecvStatic"
