{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Replay
Description : A captured Level 0 response as a 'ReadOnly drive transport
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A drive without the drive: a @'ReadOnly@ transport that answers IF-RECV of
Level 0 Discovery from a captured response (@spec/fixtures/disk6-level0.bin@,
or one "Hsed.Level0.Sedutil" synthesized), truncated or zero-padded to the
transfer length as a TPer pads it (Core 3.3.6.2, PDF 67), and refuses
every other command. The jig's drive-side runs, its Pair runner and the
fixture's tests run on it without root or hardware.

'openReplay' is milestone M3's group G4's; until it lands it is a stub.
-}
module Hsed.Transport.Replay
  ( openReplay
  ) where

import           RIO

import           Hsed.Wire (Cap (..), Transport)


-- | A replay of a captured response, or why not.
openReplay :: FilePath -> IO (Either Text (Transport 'ReadOnly))
openReplay = error "G4: openReplay"
