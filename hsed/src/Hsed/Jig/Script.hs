{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Jig.Script
Description : What a test case does, in the Test Cases' verbs, over the run's target
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The verbs a case is written in (design 9.1): Level 0 Discovery, IF-RECV on
a ComID, a raw IF-SEND (past the run's channel, which would hold a second
IF-SEND back: ETC-02's is the point), Properties as a whole exchange, and
expectations of a status or of a Table 45 answer. A script runs in the
run's environment ('RunEnv': its target, transcript, channel and options)
and keeps what later cases need ('RunCtx': the Level 0 response, the base
ComID UCT-01 records, the MaxComPacketSize Properties reported).

The verbs are milestone M3's group G3's; until it lands they are stubs.
-}
module Hsed.Jig.Script
  ( -- * The run's environment
    RunCtx (..)
  , RunEnv (..)
  , Script
    -- * Verdicts
  , Verdict (..)
    -- * Verbs
  , discover
  , ifRecvOn
  , rawIfSend
  , properties
  , expectStatus
  , expectT45
  , drain
  , simOnly
  ) where

import           RIO

import           Hsed.Jig.Options    (RunOptions)
import           Hsed.Jig.Target     (Target)
import           Hsed.Jig.Transcript (Transcript)
import           Hsed.Level0         (Level0)
import           Hsed.Sim.Sync       (T45Row)
import           Hsed.Wire           (Channel, ExchangeError, IfStatus, Response)


-- * The run's environment

-- | What the run has learned that later cases use.
data RunCtx = RunCtx
  { rcLevel0           :: !(Maybe Level0)  -- ^ the target's Level 0 response, once discovered
  , rcBaseComId        :: !(Maybe Word16)  -- ^ the base ComID UCT-01 records (Test Cases PDF 29)
  , rcMaxComPacketSize :: !(Maybe Word32)  -- ^ the TPer's MaxComPacketSize, once Properties reported it
  }
  deriving (Eq, Show)

-- | A script's environment: the run's target, transcript, channel, what it
--   has learned, its options, and where it logs.
data RunEnv = RunEnv
  { reTarget     :: !Target
  , reTranscript :: !Transcript
  , reChannel    :: !Channel
  , reCtx        :: !(IORef RunCtx)
  , reOpts       :: !RunOptions
  , reLogFunc    :: !LogFunc
  }

instance HasLogFunc RunEnv where
  logFuncL = lens reLogFunc (\e f -> e { reLogFunc = f })

-- | A case's steps.
type Script = RIO RunEnv


-- * Verdicts

-- | What a case, or a step of the jig's own, comes to.
data Verdict
  = Pass
  | Fail ![Text]         -- ^ each expectation that did not hold
  | Skipped !Text        -- ^ not run, and why (a prerequisite unmet, a capability the target lacks)
  | Unobservable !Text   -- ^ run, but what it checks cannot be seen on this target
  deriving (Eq, Show)


-- * Verbs

-- | Level 0 Discovery: IF-RECV of protocol 1, ComID 1, the response split
--   ("Hsed.Level0") and kept.
discover :: Script Level0
discover = error "G3: discover"

-- | IF-RECV of the run's transfer length on a ComID.
ifRecvOn :: Word16 -> Script Response
ifRecvOn = error "G3: ifRecvOn"

-- | IF-SEND of a payload on a ComID with a transfer length, past the run's
--   channel.
rawIfSend :: Word16 -> Word32 -> ByteString -> Script Response
rawIfSend = error "G3: rawIfSend"

-- | SMUID.Properties, a whole exchange on the base ComID: the TPer's
--   properties by name.
properties :: Script (Either ExchangeError [(Text, Integer)])
properties = error "G3: properties"

-- | Expect a response's status, the expectation named.
expectStatus :: Text -> IfStatus -> Response -> Script ()
expectStatus = error "G3: expectStatus"

-- | Expect an IF-RECV response's ComPacket header to be a Table 45 row's.
expectT45 :: Text -> T45Row -> Response -> Script ()
expectT45 = error "G3: expectT45"

-- | IF-RECV on a ComID until all is returned (Table 45's last row): the
--   synchronous protocol idle again.
drain :: Word16 -> Script ()
drain = error "G3: drain"

-- | Run a step on the simulator alone: 'Nothing' on a drive.
simOnly :: Script a -> Script (Maybe a)
simOnly = error "G3: simOnly"
