{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Cases.UCT01
Description : UCT-01, Level 0 Discovery
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

UCT-01 (Test Cases 1.02, PDF 29-31): one IF-RECV of Level 0
Discovery; the TPer and Locking descriptors, and, for Opal 2.00 to 2.30,
the Opal SSC V2, Additional DataStore Table and Block SID descriptors, each
with the values the case requires (steps 2 and 4); the base ComID recorded
for the cases after it. On a drive at M3 it waives SyncIdle and NoSessions,
which Level0Only cannot observe (Core 3.3.10.3 b, PDF 84; 3.3.6.2, PDF 67).

The case is milestone M3's group G3's; until it lands it is a stub.
-}
module Hsed.Jig.Cases.UCT01
  ( uct01
  ) where

import           RIO

import           Hsed.Jig.Case (Case)


-- | The case.
uct01 :: Case
uct01 = error "G3: uct01"
