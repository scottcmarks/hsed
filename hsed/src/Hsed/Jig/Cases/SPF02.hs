{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Cases.SPF02
Description : SPF-02 case 1, IF-RECV with nothing to return
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

SPF-02 case 1 (Test Cases 1.02, PDF 52): IF-RECV on the base ComID
in Awaiting IF-SEND: all responses returned, no further data (Core Table 45
row 6, PDF 83). The jig's extension on the simulator: an IF-SEND of one
data subpacket of Empty atoms leaves the ComID in S1 and the next IF-RECV
answers the same (Core 3.3.10.6 S2:S1 d.c, PDF 86), reported apart from the
TCG verdict, Empty atoms being out of the Test Cases' bounds (4.1, PDF 26).
Case 2 is M5's.

The case is milestone M3's group G3's; until it lands it is a stub.
-}
module Hsed.Jig.Cases.SPF02
  ( spf02case1
  ) where

import           RIO

import           Hsed.Jig.Case (Case)


-- | The case.
spf02case1 :: Case
spf02case1 = error "G3: spf02case1"
