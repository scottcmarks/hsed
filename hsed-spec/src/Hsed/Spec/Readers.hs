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

import           Hsed.Spec.Common (CodeDef)


-- | The codes the shared readers claim, reader by reader.
readerCodes :: [CodeDef]
readerCodes = []
