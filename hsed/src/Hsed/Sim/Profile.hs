{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim.Profile
Description : A drive's Level 0 response as the simulator's profile (--profile)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The descriptors of a captured Level 0 response (a @.bin@, the IF-RECV
payload as the kext returned it), which the simulator adds, the codes it
lacks as they are, to its own (design 9.2; never dropping one of its own),
so that the jig's Pair runner compares like with like.

'loadProfile' is milestone M3's group G2's; until it lands it is a stub.
-}
module Hsed.Sim.Profile
  ( loadProfile
  ) where

import           RIO

import           Hsed.Sim.Types (Level0Profile)


-- | A Level 0 response's descriptors from a file, or why not.
loadProfile :: FilePath -> IO (Either Text Level0Profile)
loadProfile = error "G2: loadProfile"
