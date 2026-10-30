{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Transcript
Description : Every command a run issued, or the Guard blocked, in order
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A run's record of the wire (design 9.2): each command issued with its
answer, each the Guard blocked with why, and notes, numbered in order. A
transport decorated by 'recording' adds every command it exchanges, so that
nothing reaches a drive unrecorded; the run's end checks the count of
IF-SENDs issued on a drive ('ifSendsIssued', none at M3), and the reports
print the entries, decoded.

The bodies are milestone M3's group G3's; until it lands they are stubs.
-}
module Hsed.Jig.Transcript
  ( TEntry (..)
  , Transcript
  , newTranscript
  , record
  , entries
  , recording
  , ifSendsIssued
  ) where

import           RIO

import           Hsed.Wire (Request, Response, Transport)


-- | One entry, numbered in the run's order: a command and its answer, a
--   command the Guard blocked and why, or a note.
data TEntry
  = Issued !Int !Request !Response
  | Blocked !Int !Request !Text
  | Note !Int !Text
  deriving (Eq, Show)

-- | A run's transcript, as it grows.
newtype Transcript = Transcript (IORef [TEntry])

-- | An empty transcript.
newTranscript :: IO Transcript
newTranscript = Transcript <$> newIORef []

-- | Add an entry.
record :: Transcript -> TEntry -> IO ()
record = error "G3: record"

-- | The entries, in order.
entries :: Transcript -> IO [TEntry]
entries = error "G3: entries"

-- | A transport that adds every command it exchanges to the transcript.
recording :: Transcript -> Transport c -> Transport c
recording = error "G3: recording"

-- | How many IF-SENDs the entries issued.
ifSendsIssued :: [TEntry] -> Int
ifSendsIssued = error "G3: ifSendsIssued"
