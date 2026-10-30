{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Cases.ETC03
Description : ETC-03, an IF-SEND longer than MaxComPacketSize
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

ETC-03 (Test Cases 1.02, PDF 78): Properties for the TPer's
MaxComPacketSize, then a Properties ComPacket of a correct Length in an
IF-SEND whose transfer length exceeds it: Invalid Transfer Length parameter
on IF-SEND (Core 3.3.10.7 c, PDF 86).

The case is milestone M3's group G3's; until it lands it is a stub.
-}
module Hsed.Jig.Cases.ETC03
  ( etc03
  ) where

import           RIO

import           Hsed.Jig.Case (Case)


-- | The case.
etc03 :: Case
etc03 = error "G3: etc03"
