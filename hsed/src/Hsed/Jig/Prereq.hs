{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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

A test's family names its section ('sectionOf': Use Case Tests are section
5, Specific Functionality 6, Error Cases 7), and a section's list its number
and page ('listAt'), which the reports print beside each waiver. Two waivers
every case of M3 makes ('lockingSPUntouched', 'noSessionOpened'): none of
them touches the Locking SP (design 9.1), and none opens a session, so the
items about sessions hold of it vacuously.

On a drive (M4c) the runner waives two prerequisites itself, each with its
reason here ('level0OutsideSync', 'sessionsUnobservable'): SyncIdle and
NoSessions of a case that needs Level 0 Discovery alone (UCT-01), which
neither concerns; and NoSessions where the drive's policy admits no
STACK_RESET, so that the run can neither see nor end a session an earlier
run left open. Where the policy admits STACK_RESET, one reset before the
case establishes both ("Hsed.Jig.Run").
-}
module Hsed.Jig.Prereq
  ( Prereq (..)
  , Section (..)
  , common
  , Waiver (..)
    -- * Sections and pages
  , sectionOf
  , listAt
    -- * Waivers the cases of M3 share
  , lockingSPUntouched
  , noSessionOpened
    -- * The runner's own waivers on a drive (M4c)
  , level0OutsideSync
  , sessionsUnobservable
  ) where

import qualified Data.Aeson         as A
import           RIO

import           Hsed.Jig.Catalogue (TestId (..))


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

-- | A section's common prerequisites, each with its item number there:
--   5.2 (PDF 29) and 6.2 (PDF 49) list the six in the same order; 7.2 (PDF
--   76) lists five, with no item about HostChallenge, so its read-write
--   sessions are item 4 and its open sessions item 5.
common :: Section -> [(Prereq, Int)]
common = \case
  UseCases              -> six
  SpecificFunctionality -> six
  ErrorCases            -> zip [SyncIdle, LockingManufactured, CredentialsKnown, ReadWriteSessions, NoSessions] [1 ..]
  where
    six = zip [SyncIdle, LockingManufactured, CredentialsKnown, HostChallengeIsCurrentPin, ReadWriteSessions, NoSessions] [1 ..]

-- | A prerequisite a case does not need, and why.
data Waiver = Waiver
  { wPrereq :: !Prereq
  , wReason :: !Text
  }
  deriving (Eq, Show)

instance A.ToJSON Waiver where
  toJSON w = A.object ["prereq" A..= tshow (wPrereq w), "reason" A..= wReason w]


-- * Sections and pages

-- | The section whose list applies to a test: its family's (Test Cases 5, 6
--   and 7: Use Case Tests, Specific Functionality, Error Test Cases).
sectionOf :: TestId -> Section
sectionOf = \case
  UCT _ -> UseCases
  SPF _ -> SpecificFunctionality
  ETC _ -> ErrorCases

-- | A section's list of common prerequisites: its number and its PDF page.
listAt :: Section -> (Text, Int)
listAt = \case
  UseCases              -> ("5.2", 29)
  SpecificFunctionality -> ("6.2", 49)
  ErrorCases            -> ("7.2", 76)


-- * Waivers the cases of M3 share

-- | The Locking SP need not be Manufactured for a case that never touches
--   it (design 9.1): UCT-01, SPF-02 case 1, ETC-02 and ETC-03 all waive it,
--   or every run would be skipped on the simulator before M5 and on disk6
--   (Manufactured-Inactive).
lockingSPUntouched :: Waiver
lockingSPUntouched = Waiver LockingManufactured "never touches the Locking SP (design 9.1)"

-- | What a case that opens no session needs of sessions, credentials and
--   challenges holds of it vacuously; no case of M3 opens one (sessions are
--   M4a's).
noSessionOpened :: [Waiver]
noSessionOpened =
  [ Waiver p "vacuous: the case opens no session" | p <- [CredentialsKnown, HostChallengeIsCurrentPin, ReadWriteSessions] ]


-- * The runner's own waivers on a drive (M4c)

-- | Why SyncIdle and NoSessions are waived on a drive for a case that needs
--   Level 0 Discovery alone (UCT-01): neither concerns it. The synchronous
--   exchange's rules apply to IF-SEND and IF-RECV on protocol 1 but not to
--   Level 0 Discovery (Core 3.3.10.3 b, PDF 84), and Level 0 Discovery's
--   IF-RECV is answered whatever the sessions (3.3.6.2, PDF 67).
level0OutsideSync :: Text
level0OutsideSync =
  "not needed: Level 0 Discovery is outside the synchronous protocol (Core 3.3.10.3 b, PDF 84) and answered whatever the "
  <> "sessions (3.3.6.2, PDF 67)"

-- | Why NoSessions is waived on a drive whose policy admits no STACK_RESET
--   (@roStackResetBeforeCases@ false, "Hsed.Jig.Options"): a session this
--   run left open fails the prerequisite instead, and one an earlier run
--   left open is unobservable until a StartSession to the Admin SP answers
--   SP_BUSY (Core 5.1.5.3, PDF 138), which @--recover@'s STACK_RESET clears
--   (3.3.4.7.5 item 1, PDF 65).
sessionsUnobservable :: Text
sessionsUnobservable =
  "unobservable on a drive whose policy admits no STACK_RESET: this run left no session open before this case, and a "
  <> "session an earlier run left open to the Admin SP answers StartSession SP_BUSY (Core 5.1.5.3, PDF 138), which "
  <> "hsed-jig --recover clears"
