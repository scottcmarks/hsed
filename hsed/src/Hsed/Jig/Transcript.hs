{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Transcript
Description : Every command a run issued, or the Guard blocked, in order
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A run's record of the wire (design 9.2): each command issued with its
answer, each the Guard blocked with why, and notes, numbered in order. A
command is recorded with the end it went to, its transport's role
("Hsed.Wire".'Describe': @tper@ the simulator, @drive@, @drive-replay@), so
that a pair's transcript tells the simulator's commands from the drive's. A
transport decorated by 'recording' adds every command it exchanges, so that
nothing reaches a drive unrecorded (a command whose transport threw is
recorded too, as a transport failure, before the exception goes on); the
run's end checks the count of IF-SENDs issued on a drive ('ifSendsIssued',
none at M3), and the reports print the entries, decoded.

'record' numbers what it adds itself, the next number in the run's order:
the number an entry is given is replaced, so a caller writes @Note 0 "…"@.
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

import qualified Data.Aeson         as A
import           RIO

import           Hsed.Wire          (Describe (..), Dir (..), IfStatus (..), Request (..), Response (..), ifStatusName)
import           Hsed.Wire.Internal (Transport (..))


-- | One entry, numbered in the run's order: a command and its answer, a
--   command the Guard blocked and why, each with the end it went to (its
--   transport's role), or a note.
data TEntry
  = Issued !Int !Text !Request !Response
  | Blocked !Int !Text !Request !Text
  | Note !Int !Text
  deriving (Eq, Show)

-- | An entry's number.
numberOf :: TEntry -> Int
numberOf = \case
  Issued n _ _ _  -> n
  Blocked n _ _ _ -> n
  Note n _        -> n

-- | An entry numbered.
numbered :: Int -> TEntry -> TEntry
numbered n = \case
  Issued _ e q r  -> Issued n e q r
  Blocked _ e q w -> Blocked n e q w
  Note _ t        -> Note n t

-- | An entry as the reports print it: its number and kind, the end, the
--   command, and its answer, why it was blocked, or the note's text.
instance A.ToJSON TEntry where
  toJSON = \case
    Issued n e q r  -> A.object ["n" A..= n, "entry" A..= ("issued" :: Text), "end" A..= e, "request" A..= q, "response" A..= r]
    Blocked n e q w -> A.object ["n" A..= n, "entry" A..= ("blocked" :: Text), "end" A..= e, "request" A..= q, "reason" A..= w]
    Note n t        -> A.object ["n" A..= n, "entry" A..= ("note" :: Text), "text" A..= t]

-- | A run's transcript, as it grows (the newest entry first).
newtype Transcript = Transcript (IORef [TEntry])

-- | An empty transcript.
newTranscript :: IO Transcript
newTranscript = Transcript <$> newIORef []

-- | Add an entry, numbered the next in the run's order (from 1).
record :: Transcript -> TEntry -> IO ()
record (Transcript ref) e = atomicModifyIORef' ref (\es -> (numbered (maybe 1 ((+ 1) . numberOf) (listToMaybe es)) e : es, ()))

-- | The entries, in order.
entries :: Transcript -> IO [TEntry]
entries (Transcript ref) = reverse <$> readIORef ref

-- | A transport that adds every command it exchanges to the transcript, with
--   the transport's role, and a note of every event it causes. A command
--   whose transport throws is added as issued with a transport failure, then
--   the exception rethrown: it may have reached the drive.
recording :: Transcript -> Transport c -> Transport c
recording t tr = tr
  { tExchange = \q -> tryAny (tExchange tr q) >>= \case
      Right r -> r <$ record t (Issued 0 end q r)
      Left e  -> record t (Issued 0 end q (Response (TransportFailure ("threw: " <> tshow e)) mempty)) >> throwIO e
  , tInject = \ev -> do
      s <- tInject tr ev
      s <$ record t (Note 0 ("event " <> tshow ev <> ": " <> ifStatusName s))
  }
  where end = dRole (tDescribe tr)

-- | How many IF-SENDs the entries issued.
ifSendsIssued :: [TEntry] -> Int
ifSendsIssued es = length [ () | Issued _ _ q _ <- es, dir q == IfSend ]
