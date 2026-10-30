{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Hsed.Version
Description : hsed's version and git state, for what the executables write
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The package version with the git branch, commit and dirty flag of the tree it
was built from ("GitHash.Extras.GitVersion"), once for the library, so that
what the jig writes (its reports, the Level 0 capture's sidecar) and what a
transport says it is ('Hsed.Wire.describeTransport') name the build that wrote
it. Kept from inlining, so that a commit recompiles this module alone.
-}
module Hsed.Version
  ( hsedVersion
  ) where

import           RIO
import qualified RIO.Text                  as T

import           GitHash.Extras.GitVersion (gitVersion)
import qualified Paths_hsed


-- | @Version 0.1.0.1 [feature/types 1234567 (dirty)]@.
hsedVersion :: Text
hsedVersion = T.pack $(gitVersion Paths_hsed.version)
{-# NOINLINE hsedVersion #-}
