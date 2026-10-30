{-|
Module      : System.SED.MCTP.Common.TableUIDs
Description : The tables' UIDs and half-UIDs of Core Table 240, generated from the PDF
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Since M2d a re-export of "Hsed.Gen.Core.TableUIDs" (design docs/spec-as-source.md
section 4.1). The hand transcription this module held, with its typed half-UIDs
and TableID class, is the oracle's test fixture, hsed/fixture/Hand/TableUIDs.hs
(section 5.5).
-}
module System.SED.MCTP.Common.TableUIDs
  ( module Hsed.Gen.Core.TableUIDs
  ) where

import           Hsed.Gen.Core.TableUIDs
