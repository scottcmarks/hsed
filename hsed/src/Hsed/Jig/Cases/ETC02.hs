{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Cases.ETC02
Description : ETC-02, a second IF-SEND before the first's response
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-02 (Test Cases 1.02, PDF 77): Properties sent in an IF-SEND
and its response not retrieved, then Properties sent again on that ComID:
the second IF-SEND's status Synchronous Protocol Violation (Core
3.3.10.2.1 3 a, PDF 82). Its IF-SENDs are raw, past the run's channel. The
jig's extension: with the simulator holding its response, the first IF-RECV
of the drain answers responses to come (Core Table 45 row 1, PDF 83).

The case is milestone M3's group G3's; until it lands it is a stub.
-}
module Hsed.Jig.Cases.ETC02
  ( etc02
  ) where

import           RIO

import           Hsed.Jig.Case (Case)


-- | The case.
etc02 :: Case
etc02 = error "G3: etc02"
