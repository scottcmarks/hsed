{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Law.Reset
Description : The resets' laws (Opal 2.30 3.2.3, 3.3.5.1 and 4.3.5; Core 5.2.2.4.4 and 5.3.4.1.1.2): what a reset leaves of the Locking SP's tables, the host's properties, Tries and the ComIDs
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The laws of the simulator's resets ("Hsed.Sim.Reset"), as "Hsed.Law.Sync"
has the synchronous protocol's: each names the paragraph it implements by
the first 12 hex digits of its IR hash and the Test Cases' cases that
observe it, and says what it holds as a QuickCheck property over the
simulator. A stub of M5a's scaffold, no law yet: its group G4 writes five,
LockOnReset (a Locking row's ReadLocked and WriteLocked set where the reset's
type is in its LockOnReset, Core 5.7.2.2.10, PDF 275), MBRDoneOnReset (MBRControl's
Done cleared likewise), the host's properties, Tries, and the ComIDs
Issued after a reset, and the law comments that bind them.
-}
module Hsed.Law.Reset
  ( laws
  ) where

import           Hsed.Law (Law)


-- | The resets' laws, those the comments bind: none until M5a's group G4.
--
-- >>> laws
-- []
laws :: [Law]
laws = []
