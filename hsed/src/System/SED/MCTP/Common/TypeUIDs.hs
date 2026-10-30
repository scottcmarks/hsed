{-|
Module      : System.SED.MCTP.Common.TypeUIDs
Description : The Type objects' UIDs, TypeDefs and enumerations, generated from the PDF
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Since M2d a re-export of "Hsed.Gen.Core.Types" and "Hsed.Gen.Core.Enums" (design
docs/spec-as-source.md section 4.1). The hand transcription this module held is
the regenerate-and-compare oracle's test fixture, hsed/fixture/Hand/TypeUIDs.hs
(section 5.5).
-}
module System.SED.MCTP.Common.TypeUIDs
  ( module Hsed.Gen.Core.Enums
  , module Hsed.Gen.Core.Types
  ) where

import           Hsed.Gen.Core.Enums
import           Hsed.Gen.Core.Types
