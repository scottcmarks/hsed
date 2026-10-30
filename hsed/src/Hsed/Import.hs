{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Import
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The 2019 prelude of hsed: RIO with the umbrella "Hsed.Types", whose refined
@drop@, @length@, @map@, @replicate@ and @take@ ("Data.BoundedSize") and
whose @toList@ (the method of class t'GHC.Exts.IsList') it takes in place of
RIO's. Only the Run stubs import it (design docs/spec-as-source.md section
13.13); since the M2d review it hides no @lookup@, the umbrella having none
since M2d.
-}
module Hsed.Import
  ( module RIO
  , module Hsed.Types
  ) where

import           Hsed.Types
import           RIO                          hiding (drop, length, map,
                                               replicate, take, toList)
