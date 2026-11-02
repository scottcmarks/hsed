{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Recover
Description : --recover: STACK_RESET on a drive's base ComID, and the checks that it took
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsed-jig --recover@ (design 9.3, 11): run alone on its target's one end
("Hsed.Jig.Run" runs it in place of the cases, under the Guard's
'Hsed.Jig.Guard.recoverMethods' on a drive), it clears what a run that was
stopped left on the drive's base ComID: Level 0 Discovery and the base
bound, STACK_RESET and its GET_COMID_RESPONSE (Core 3.3.4.7.5, PDF 65-66:
every session on the ComID aborted, its stack in its initial state), an
IF-RECV that finds the ComID idle (Core 3.3.10.5, PDF 84), and a
StartSession to the Admin SP as Anybody and its End of Session, which find
no session left (Core 5.1.5.3, PDF 138); its outcome in words.

At M4c's scaffold it is group G3's, and recovers nothing yet.
-}
module Hsed.Jig.Recover
  ( recoverScript
  ) where

import           RIO

import           Hsed.Jig.Script (Script)


-- | @--recover@ on an end: what it did, or why it did not recover. Group
--   G3's: refused until then.
recoverScript :: Script (Either Text Text)
recoverScript = pure (Left "G3: --recover, STACK_RESET on the base ComID (Core 3.3.4.7.5, PDF 65; design 9.3)")
