{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Types
Description : The simulated TPer's parameters and state, and what a step of it takes and gives
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The simulator of design docs/spec-as-source.md section 8, as milestone M4a
has it: a TPer with static ComIDs, each with its synchronous protocol state
(Core 3.3.10, PDF 82-87), the host's properties it keeps (5.2.2.4, PDF
144-145) and its ComID management buffer (3.3.4.7, PDF 62-66); its regular
sessions, each on a ComID (3.3.7.1, PDF 72-75); and its SPs, each with its
life cycle and its tables, the Admin SP's rows hand-written until M4b
("Hsed.Sim.AdminSP").

'SimParams' are what a run chooses ("Hsed.Sim.Params", re-exported here).
'TPerState' is everything a step changes, and everything has 'Eq', so that a
law can say a step leaves the state as it was. A step ("Hsed.Sim".@step@)
takes an 'Input' (an interface command, an event, or the end of the TPer's
processing of a ComID's command, 'Tick') and gives an 'Output'; an accepted
IF-SEND's payload is an 'Outcome'. The helpers over the state (sessions
opened and closed, rows and cells, a ComID's life derived from its sessions)
are "Hsed.Sim.State".
-}
module Hsed.Sim.Types
  ( -- * Parameters
    module Hsed.Sim.Params
    -- * State
  , TPerState (..)
  , ComIdState (..)
  , ComIdLife (..)
  , SyncState (..)
  , MgmtBuffer (..)
  , Session (..)
  , SPState (..)
  , initialState
  , firstTSN
    -- * A step
  , Input (..)
  , Output (..)
  , Outcome (..)
  ) where

import qualified RIO.Map            as Map
import           RIO

import           Hsed.Core.Table    (Row)
import           Hsed.Core.UID      (UID)
import           Hsed.Gen.Core.UIDs (uSP_Admin)
import           Hsed.OpalUIDs      (uSP_Locking)
import           Hsed.Sim.AdminSP   (adminSPTables)
import           Hsed.Sim.Params
import           Hsed.Wire          (Event, IfStatus, Request, Response)


-- * State

-- | A ComID's state (Core 3.3.3, PDF 56): a static ComID is Issued until a
--   session opens on it, Associated while one is open. A 'ComIdState' holds
--   Inactive or Issued; Associated is derived from the sessions
--   ("Hsed.Sim.State".@lifeOf@).
data ComIdLife
  = Inactive
  | Issued
  | Associated
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A ComID's synchronous protocol state (Core 3.3.10.4, PDF 84): awaiting
--   IF-SEND; processing, its response packets prepared but not yet
--   released ('Tick'); awaiting IF-RECV, the packets still to return.
data SyncState
  = S1
  | S2 ![ByteString]
  | S3 ![ByteString]
  deriving (Eq, Show)

-- | A ComID's ComID management buffer (Core 3.3.4.7, PDF 62-66; protocol
--   0x02): whether a HANDLE_COMID_REQUEST was ever made on it (before one,
--   GET_COMID_RESPONSE's No Response Available carries an Extended ComID of
--   zeros, 3.3.4.7.3, PDF 63), and the response staged for
--   GET_COMID_RESPONSE: the request code it answers (Table 28's, PDF 62) and
--   its bytes.
data MgmtBuffer = MgmtBuffer
  { mbRequested :: !Bool
  , mbResponse  :: !(Maybe (Word32, ByteString))
  }
  deriving (Eq, Show)

-- | A ComID's state: its extension, its life, its synchronous state, the
--   host's properties it keeps (Opal 2.30 Table 17's initial assumptions
--   until Properties sets them, Core 5.2.2.3-4, PDF 143-145) and its ComID
--   management buffer.
data ComIdState = ComIdState
  { cExt       :: !Word16
  , cLife      :: !ComIdLife
  , cSync      :: !SyncState
  , cHostProps :: !(Map Text Natural)
  , cMgmt      :: !MgmtBuffer
  }
  deriving (Eq, Show)

-- | A regular session (Core 3.3.7.1, PDF 72-75): the ComID it is on, its
--   TSN and HSN (the packet's Session, TSN then HSN, 3.3.7.1.2, PDF 73), the
--   SP it is open to, whether it is read-write, the authorities
--   authenticated at its startup and their classes (3.3.7.1.4, PDF 74-75;
--   3.4.2.2, PDF 89), and the host's properties as its ComID had them when
--   it started, which it keeps (5.2.2.4, PDF 144-145).
data Session = Session
  { sesComId       :: !Word16
  , sesTSN         :: !Word32
  , sesHSN         :: !Word32
  , sesSP          :: !UID
  , sesWrite       :: !Bool
  , sesAuthorities :: ![UID]
  , sesHostProps   :: !(Map Text Natural)
  }
  deriving (Eq, Show)

-- | An SP: its UID (its row of the Admin SP's SP table), its life cycle and
--   its tables, each the rows it holds by the table's UID (Core Table 240's
--   @UID of Table@, "Hsed.Gen.Core.TableUIDs").
data SPState = SPState
  { spsUID    :: !UID
  , spsLife   :: !LifeCycle
  , spsTables :: !(Map UID [Row])
  }
  deriving (Eq, Show)

-- | The TPer: its parameters, its ComIDs, its sessions by ComID and TSN,
--   the next TSN it assigns, its SPs by UID, and whether the initial
--   C_PIN_SID PIN was the MSID's: what Level 0's Initial C_PIN_SID PIN
--   Indicator reports (Opal 2.30 Table 6, PDF 23: "the initial C_PIN_SID
--   PIN value is equal to the C_PIN_MSID PIN value", a fact of manufacture
--   that a Set of the PIN does not change and only a Revert of the Admin SP
--   after SID authenticated sets to 0x00, Opal 5.1.2.2.1 item 2 a, PDF 93,
--   M5's), apart from the live C_PIN_SID row, which the Block SID
--   descriptor's SID Value State follows ("Hsed.Sim.Level0").
data TPerState = TPerState
  { tsParams              :: !SimParams
  , tsComIds              :: !(Map Word16 ComIdState)
  , tsSessions            :: !(Map (Word16, Word32) Session)
  , tsNextTSN             :: !Word32
  , tsSPs                 :: !(Map UID SPState)
  , tsInitialSidPinIsMsid :: !Bool
  }
  deriving (Eq, Show)

-- | A TPer as it starts: its static ComIDs, the base and the ones after it,
--   each Issued, with extension 0, awaiting IF-SEND, with the host's initial
--   properties and an empty management buffer; none past 0xFFFF or reserved
--   (parameters with such a ComID are refused, 'simParamsProblems'); no
--   session, the first TSN to assign 'firstTSN'; the Admin SP, Manufactured
--   (Opal 2.30 Table 29, PDF 50), with its hand-written rows
--   ("Hsed.Sim.AdminSP"), and the Locking SP in its Original Factory State
--   ('spLockingOFS'), its tables M5's; the initial C_PIN_SID PIN the MSID's
--   unless the parameters give the SID a PIN of its own ('spSidPin' other
--   than the MSID, Opal 2.30 4.2.1.8, PDF 49).
initialState :: SimParams -> TPerState
initialState p = TPerState
  { tsParams              = p
  , tsComIds              = Map.fromList [ (c, ComIdState 0 Issued S1 initialHostProperties (MgmtBuffer False Nothing))
                                         | c <- staticComIds p, not (reservedComId c) ]
  , tsSessions            = mempty
  , tsNextTSN             = firstTSN
  , tsSPs                 = Map.fromList
      [ (uSP_Admin, SPState uSP_Admin Manufactured (adminSPTables p))
      , (uSP_Locking, SPState uSP_Locking (spLockingOFS p) mempty)
      ]
  , tsInitialSidPinIsMsid = maybe True (== spMsid p) (spSidPin p)
  }

-- | The first TSN a TPer assigns a regular session: 4096, the TSNs 0 to 4095
--   being reserved for special sessions, the control session (0) among them
--   (Core 3.3.7.1.1, PDF 72).
firstTSN :: Word32
firstTSN = 4096


-- * A step

-- | What a step of the TPer takes: an interface command, an event, or the
--   end of its processing of a ComID's accepted IF-SEND (S2 to S3).
data Input
  = Cmd !Request
  | Ev !Event
  | Tick !Word16
  deriving (Eq, Show)

-- | What a step gives: a command's answer, an event's status, or nothing
--   but the state (a 'Tick').
data Output
  = Out !Response
  | Evented !IfStatus
  | Ticked
  deriving (Eq, Show)

-- | What an IF-SEND accepted in S1 on a static ComID comes to (design 8.2):
--   response packets prepared (to S2); a payload discarded, the ComID kept
--   in S1, and why (Core 3.3.10.6 S2:S1 c-d, PDF 85-86; 3.3.10.7 a, PDF 86);
--   or a session aborted, with the packets of the results completed before
--   the fault (3.2.2.4.2 item 1, PDF 39) and the TPer's CloseSession
--   prepared or not (3.3.10.7 b; 'spCloseSessionOnAbort').
data Outcome
  = Prepared ![ByteString]
  | Discarded !Text
  | SessionAborted ![ByteString] !(Maybe ByteString)
  deriving (Eq, Show)
