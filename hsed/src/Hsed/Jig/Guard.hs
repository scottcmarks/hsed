{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Guard
Description : What may reach a drive: every command checked against a policy before its transport sees it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The run-time half of the jig's safety against the attached drive (design
9.3.2): before a command reaches a drive's transport, the Guard decides by
the run's policy, and a refused command is recorded ('Hsed.Jig.Transcript')
and never issued. At M3 a drive's policy is 'Level0Only': IF-RECV of Level
0 Discovery (protocol 1, ComID 1) and nothing else. 'AllowList' is M4c's,
written now for its test: an IF-SEND whose ComPacket parses to methods every
one of which is allowed (SMUID.Properties; SMUID.StartSession to the Admin
SP as Anybody or no one; a bare EndOfSession), the UIDs those of
"Hsed.Gen.Core.UIDs".

The Guard sends on a @'ReadOnly@ transport, which no other module can.

The bodies are milestone M3's group G3's; until it lands they are stubs.
-}
module Hsed.Jig.Guard
  ( Policy (..)
  , Allowed (..)
  , guardedSend
  , guardedRecv
  ) where

import           RIO

import           Hsed.Jig.Transcript (Transcript)
import           Hsed.Wire           (Cap (..), Request, Response, Transport)


-- | What a drive's commands may be.
data Policy
  = Level0Only           -- ^ IF-RECV of Level 0 Discovery alone (M3)
  | AllowList ![Allowed]  -- ^ IF-SENDs of these methods, and IF-RECV (M4c)
  deriving (Eq, Show)

-- | A method an 'AllowList' admits.
data Allowed
  = AllowProperties           -- ^ SMUID.Properties
  | AllowStartSessionAdminSP  -- ^ SMUID.StartSession to the Admin SP, as Anybody or no one
  | AllowEndOfSession         -- ^ a bare EndOfSession
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | An IF-SEND on a drive, if the policy allows it; otherwise the reason,
--   recorded, and the transport never called.
guardedSend :: Transcript -> Policy -> Transport 'ReadOnly -> Request -> IO (Either Text Response)
guardedSend = error "G3: guardedSend"

-- | An IF-RECV, if the policy allows it; otherwise the reason, recorded.
guardedRecv :: Transcript -> Policy -> Transport c -> Request -> IO (Either Text Response)
guardedRecv = error "G3: guardedRecv"
