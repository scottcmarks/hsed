{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Run
Description : A jig run: open the target, run the selected cases in order, check the transcript, write the reports
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsed-jig run@ (design 9.2): refuse a drive target unless root (hsed's own
gate, design 9.3), read the profile, open the target, establish or check
each case's prerequisites and run it, its waivers printed, and its
extensions on the simulator; on a drive, capture its Level 0 response when
asked, and check that no IF-SEND was issued; write the reports. The exit
code: 0 every selected case passed (waived prerequisites printed); 1 a
failure, or a skip without a waiver; 2 a usage error; 3 a safety violation
(a drive target without root, or an IF-SEND issued on a drive).

'runJig' is milestone M3's group G3's; until it lands it is a stub.
-}
module Hsed.Jig.Run
  ( RunOptions (..)
  , runJig
  ) where

import           RIO

import           Hsed.Jig.Options (RunOptions (..))


-- | Run the jig.
runJig :: RunOptions -> IO ExitCode
runJig = error "G3: runJig"
