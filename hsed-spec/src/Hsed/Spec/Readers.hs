{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Spec.Readers
Description : The finding codes of the readers several rules share
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The modules under @Hsed/Spec/Readers/@ are the helpers several rules share
and no comment names (docs/adding-a-rule.md). A code that several rules give
through one of them is that reader's, not any one rule's, so that rules
written in parallel never both claim it: this module lists those codes for
the catalogue ("Hsed.Spec.Report".'Hsed.Spec.Report.codeCatalogue'), one
line per reader that has any.
-}
module Hsed.Spec.Readers
  ( readerCodes
  ) where

import           RIO

import           Hsed.Spec.Common         (CodeDef)
import           Hsed.Spec.Readers.Grid   (gridCodes)
import           Hsed.Spec.Readers.Layout (layoutCodes)
import           Hsed.Spec.Readers.Pins   (pinsCodes)


-- | The codes the shared readers claim, reader by reader: the byte grid's
--   (C-LAYOUT-EXTENT, C-LAYOUT-ENDIAN, N-LAYOUT-BITS, W-LAYOUT-BITS-UNUSED), the
--   layouts' (C-LAYOUT-TILE), the pins' (W-PINS-NONE).
readerCodes :: [CodeDef]
readerCodes = gridCodes <> layoutCodes <> pinsCodes
