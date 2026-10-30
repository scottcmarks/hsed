{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Prereq
Description : The Test Cases' common prerequisites, and a case's waivers of them
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Each section of the Test Cases 1.02 lists prerequisites common to its cases
(5.2, PDF 29; 6.2, PDF 49; 7.2, PDF 76): the synchronous protocol idle on
every ComID, the Locking SP Manufactured, the credentials known, each
HostChallenge the current PIN, sessions read-write, no session open. A
case's own set is its section's list less what it waives ('Waiver'), each
waiver printed with its reason and page (design 9.1); the runner
establishes or checks the rest, in order (Test Cases 3.2.3, PDF 25).

'common' is milestone M3's group G3's; until it lands it is a stub.
-}
module Hsed.Jig.Prereq
  ( Prereq (..)
  , Section (..)
  , common
  , Waiver (..)
  ) where

import           RIO


-- | A common prerequisite.
data Prereq
  = SyncIdle                   -- ^ every ComID's synchronous protocol in Awaiting IF-SEND
  | LockingManufactured        -- ^ the Locking SP Manufactured
  | CredentialsKnown           -- ^ the values of the credentials used known
  | HostChallengeIsCurrentPin  -- ^ each StartSession's HostChallenge the authority's current PIN
  | ReadWriteSessions          -- ^ every session read-write
  | NoSessions                 -- ^ no session open at the start
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A section of the Test Cases, with its own list.
data Section
  = UseCases               -- ^ 5, its list 5.2 (PDF 29)
  | SpecificFunctionality  -- ^ 6, its list 6.2 (PDF 49)
  | ErrorCases             -- ^ 7, its list 7.2 (PDF 76)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A section's common prerequisites, each with its item number there.
common :: Section -> [(Prereq, Int)]
common = error "G3: common"

-- | A prerequisite a case does not need, and why.
data Waiver = Waiver
  { wPrereq :: !Prereq
  , wReason :: !Text
  }
  deriving (Eq, Show)
