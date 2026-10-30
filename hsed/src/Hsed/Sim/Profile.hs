{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Profile
Description : A drive's Level 0 response as the simulator's profile (--profile)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The descriptors of a captured Level 0 response (a @.bin@, the IF-RECV
payload as the kext returned it, or one "Hsed.Level0.Sedutil" synthesized),
which the simulator adds, the codes it lacks as they are, to its own (design
9.2; never dropping one of its own, "Hsed.Sim.Level0"), so that the jig's
Pair runner compares like with like.

A response cut short, whose Length of Parameter Data asks for more than the
file holds or whose last descriptor runs past it, is refused: its
descriptors would be some of the drive's, or part of one. Its other
problems (its header's versions, its codes' order, its padding) leave its
descriptors as the drive sent them, and are no reason to refuse it.
-}
module Hsed.Sim.Profile
  ( loadProfile
  ) where

import           RIO
import qualified RIO.Text       as T

import           Hsed.Level0    (Level0 (..), cutShort, splitLevel0)
import           Hsed.Sim.Types (Level0Profile (..))


-- | A Level 0 response's descriptors from a file, or why not.
loadProfile :: FilePath -> IO (Either Text Level0Profile)
loadProfile path = do
  contents <- tryIO (readFileBinary path)
  pure $ do
    bytes <- either (\e -> Left (T.pack path <> ": " <> tshow e)) Right contents
    level0 <- either (\e -> Left (T.pack path <> ": not a Level 0 response: " <> tshow e)) Right (splitLevel0 bytes)
    case filter cutShort (l0Problems level0) of
      []       -> Right (Level0Profile path (l0Descriptors level0))
      problems -> Left (T.pack path <> ": the response is cut short: " <> T.intercalate ", " (map tshow problems))
