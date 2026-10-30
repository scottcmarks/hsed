{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Types
Description : The simulated TPer's parameters and state, and what a step of it takes and gives
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The simulator of design docs/spec-as-source.md section 8, as milestone M3
has it: a TPer with static ComIDs, each with its synchronous protocol state
(Core 3.3.10, PDF 82-87), answering Level 0 Discovery and, on the control
session, the Session Manager's Properties. Sessions, SPs and their tables
come at M4a.

'SimParams' are what a run chooses (the jig's simulator options): where the
spec leaves a value to the vendor or a range, the default is a value the
spec allows, each citing where (design 13.17: parameters, not comments, at
M3). 'TPerState' is everything a step changes, and everything has 'Eq', so
that a law can say a step leaves the state as it was. A step
("Hsed.Sim".@step@) takes an 'Input' (an interface command, an event, or the
end of the TPer's processing of a ComID's command, 'Tick') and gives an
'Output'; an accepted IF-SEND's payload is an 'Outcome'.

Parameters out of range are refused, not wrapped or dropped
('simParamsProblems': a ComID past 0xFFFF or in Core Table 26's reserved
range, a version past its 4-bit field, a limit of none), so that every
descriptor the simulator sends encodes and every static ComID is one a TPer
may have; the in-process transport is opened only with parameters that
pass. 'Limits' are what each end takes in a ComPacket (Core 5.2.2.4.1): the
TPer's from its properties, the host's Opal 2.30 Table 17's initial
assumptions, the host's properties not being kept before M4a.
-}
module Hsed.Sim.Types
  ( -- * Parameters
    SimParams (..)
  , defaultSimParams
  , LifeCycle (..)
  , Geometry (..)
  , Level0Profile (..)
  , simParamsProblems
    -- * Properties, and what each end takes
  , maxComPacketSizeName
  , maxResponseComPacketSizeName
  , maxPacketSizeName
  , maxPacketsName
  , maxSubpacketsName
  , maxMethodsName
  , hostRequirements
  , Limits (..)
  , tperLimits
  , hostLimits
    -- * State
  , TPerState (..)
  , ComIdState (..)
  , ComIdLife (..)
  , SyncState (..)
  , initialState
  , reservedComId
    -- * A step
  , Input (..)
  , Output (..)
  , Outcome (..)
  ) where

import qualified RIO.List            as L
import qualified RIO.Map             as Map
import           RIO
import qualified RIO.Text            as T

import           Hsed.Core.Code      (CodeRow (..), codeRow)
import           Hsed.Gen.Core.Codes (cComIDAssignments)
import           Hsed.Level0         (Descriptor)
import           Hsed.Wire           (Event, IfStatus, Request, Response, Unit (..), hex)


-- * Parameters

-- | The life cycle state of an SP created in manufacturing (Opal 2.30
--   5.2.2, PDF 96): the Locking SP's Original Factory State.
data LifeCycle
  = ManufacturedInactive
  | Manufactured
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What the Geometry Reporting descriptor reports (Opal 2.30 Table 5 and
--   3.1.1.4.2-5, PDF 21-22), which Opal takes from the LockingInfo table:
--   until that table is simulated (M5), parameters.
data Geometry = Geometry
  { gAlign            :: !Bool
  , gLogicalBlockSize :: !Word32
  , gGranularity      :: !Word64
  , gLowestAligned    :: !Word64
  }
  deriving (Eq, Show)

-- | A Level 0 response's descriptors to answer with beside the simulator's
--   own (@--profile@, design 9.2): read from a file, a drive's capture; the
--   codes the simulator lacks are added as they are, and none of its own is
--   dropped.
data Level0Profile = Level0Profile
  { lpSource      :: !FilePath
  , lpDescriptors :: ![Descriptor]
  }
  deriving (Eq, Show)

-- | What a simulator run chooses.
data SimParams = SimParams
  { spBaseComId            :: !Word16             -- ^ the first static ComID
  , spNComIds              :: !Word16             -- ^ how many static ComIDs
  , spUnits                :: !Unit               -- ^ what transfer lengths count
  , spLockingOFS           :: !LifeCycle          -- ^ the Locking SP's Original Factory State
  , spInactiveComIdDiscard :: !Bool               -- ^ discard a command to an inactive ComID rather than refuse it
  , spTPerProperties       :: ![(Text, Integer)]  -- ^ the TPer's properties, as Properties reports them
  , spSscMinor             :: !Word8              -- ^ the Opal SSC V2 descriptor's SSC Minor Version Number
  , spSiisRevision         :: !Word8              -- ^ the SIIS descriptor's SIIS Revision Number
  , spDataRemovalVersion   :: !Word8              -- ^ the Supported Data Removal Mechanism descriptor's Version
  , spGeometry             :: !Geometry
  , spDataStore            :: !(Word16, Word32, Word32)
    -- ^ the Additional DataStore Table descriptor's maximum number of tables, their maximum total size, and the
    --   alignment of a table's size
  , spProfile              :: !(Maybe Level0Profile)
  }
  deriving (Eq, Show)

-- | The defaults, each a value the spec allows:
--
-- * base ComID 0x1004: Base ComID is VU (Opal 2.30 Table 6's bullets, PDF
--   23); 0x1004 is what disk6 reports (spec/fixtures/disk6-query.txt);
-- * one ComID: Number of ComIDs is 0x0001 or larger (PDF 23);
-- * bytes: the in-process transport's unit (design 6.2; SIIS 1.20 4.3's
--   512-byte blocks with @--units blocks512@);
-- * the Locking SP Manufactured-Inactive, the Original Factory State an
--   Opal Locking SP created in manufacturing SHALL have (Opal 2.30 5.2.2,
--   PDF 96);
-- * a command to an inactive ComID refused with Other Invalid Command
--   Parameter, which Core allows beside discarding it (3.3.3.2-3, PDF 58-59);
-- * the TPer properties at Opal 2.30 Table 17's minima (PDF 33-34), the
--   numeric ones (DefSessionTimeout, mandatory with a VU value, is reported
--   from M4a, with sessions: design 8.5); their names are Core Table 167's
--   (Hsed.Gen.Core.Properties);
-- * SSC Minor Version Number 0x3, v2.30's (Opal 2.30 Table 7, PDF 24);
-- * SIIS Revision Number 0x20, v1.20's (SIIS 1.20 Table 3, PDF 22);
-- * the Supported Data Removal Mechanism descriptor's version 0x02 (Opal
--   2.30 Table 9, PDF 26);
-- * the geometry the LockingInfo table would give (Opal 2.30 3.1.1.4.2-5,
--   PDF 22): alignment required, 512-byte logical blocks, a granularity of
--   8 and the lowest aligned LBA 0, as disk6 reports them;
-- * the DataStore table descriptor: 1 table, 0xA00000 bytes in all, an
--   alignment of 1: the DataStore table an Opal 2.30 TPer SHALL have, at
--   least 10 MB (Opal 2.30 4.3.8.1, PDF 90: its Rows at least 0x00A00000),
--   above UCT-01 step 4b's least of 0xA0000 (Test Cases 1.02, PDF 30);
-- * no profile.
defaultSimParams :: SimParams
defaultSimParams = SimParams
  { spBaseComId            = 0x1004
  , spNComIds              = 1
  , spUnits                = UnitBytes
  , spLockingOFS           = ManufacturedInactive
  , spInactiveComIdDiscard = False
  , spTPerProperties       =
      [ (maxComPacketSizeName, 2048)
      , (maxResponseComPacketSizeName, 2048)
      , (maxPacketSizeName, 2028)
      , ("MaxIndTokenSize", 1992)
      , (maxPacketsName, 1)
      , (maxSubpacketsName, 1)
      , (maxMethodsName, 1)
      , ("MaxSessions", 1)
      , ("MaxAuthentications", 2)
      , ("MaxTransactionLimit", 1)
      ]
  , spSscMinor             = 0x3
  , spSiisRevision         = 0x20
  , spDataRemovalVersion   = 0x02
  , spGeometry             = Geometry
      { gAlign            = True
      , gLogicalBlockSize = 512
      , gGranularity      = 8
      , gLowestAligned    = 0
      }
  , spDataStore            = (1, 0xA00000, 1)
  , spProfile              = Nothing
  }

-- | What is out of range in a run's parameters; none for parameters the
--   simulator takes:
--
-- * no static ComID, or one past 0xFFFF, or one Core Table 26 reserves
--   (PDF 55: 0x0000-0x0FFF are Level 0's, TPER_RESET's, TCG's and vendors';
--   Opal 2.30 Table 14, PDF 30, the same), so that none wraps to 0x0000;
-- * an SSC minor version or a Data Removal version past its descriptor's
--   4-bit field (Opal 2.30 Tables 6 and 8, PDF 23-24);
-- * a limit the simulator reads of its properties ('tperLimits') of none.
simParamsProblems :: SimParams -> [Text]
simParamsProblems p = concat
  [ [ "no static ComID (Number of ComIDs is 0x0001 or larger, Opal 2.30 Table 6)" | spNComIds p == 0 ]
  , [ "static ComIDs from " <> hex (spBaseComId p) <> ", " <> tshow (spNComIds p) <> " of them, run past 0xFFFF" | lastComId > 0xFFFF ]
  , [ "static ComID " <> hex c <> " is reserved (Core Table 26)" | c <- staticComIds p, reservedComId c ]
  , [ "SSC Minor Version Number " <> tshow (spSscMinor p) <> " is past its 4 bits" | spSscMinor p > 0xF ]
  , [ "Data Removal version " <> tshow (spDataRemovalVersion p) <> " is past its 4 bits" | spDataRemovalVersion p > 0xF ]
  , [ name <> " " <> tshow v <> ": a limit of none" | (name, v) <- spTPerProperties p, name `elem` limitNames, v < 1 ]
  ]
  where
    lastComId  = toInteger (spBaseComId p) + toInteger (spNComIds p) - 1 :: Integer
    limitNames = [maxComPacketSizeName, maxResponseComPacketSizeName, maxPacketSizeName, maxPacketsName, maxSubpacketsName, maxMethodsName]


-- * Properties, and what each end takes

-- | The properties of Core Table 167 (PDF 141-142; "Hsed.Gen.Core.Properties")
--   the simulator and the jig read, by name: the largest ComPacket an end
--   takes, the largest ComPacket response, packet, packets per ComPacket,
--   subpackets per packet and method invocations or responses per
--   subpacket.
maxComPacketSizeName, maxResponseComPacketSizeName, maxPacketSizeName, maxPacketsName, maxSubpacketsName, maxMethodsName :: Text
maxComPacketSizeName         = "MaxComPacketSize"
maxResponseComPacketSizeName = "MaxResponseComPacketSize"
maxPacketSizeName            = "MaxPacketSize"
maxPacketsName               = "MaxPackets"
maxSubpacketsName            = "MaxSubpackets"
maxMethodsName               = "MaxMethods"

-- | Opal 2.30 Table 17 (PDF 33-34), Host Property Requirements: each host
--   property an Opal TPer accepts (M), with the least it allows, which is
--   also its Initial Assumption; MaxResponseComPacketSize (N), which a TPer
--   may ignore and leave out of the HostProperties result, is not among them.
hostRequirements :: [(Text, Natural)]
hostRequirements =
  [ (maxComPacketSizeName, 2048)
  , (maxPacketSizeName, 2028)
  , ("MaxIndTokenSize", 1992)
  , (maxPacketsName, 1)
  , (maxSubpacketsName, 1)
  , (maxMethodsName, 1)
  ]

-- | What one end takes in a ComPacket it is sent (Core 5.2.2.4.1, PDF
--   145-146): packets in a ComPacket, subpackets in a packet, method
--   invocations or responses in a subpacket, a packet's bytes and a
--   ComPacket's (each with its header).
data Limits = Limits
  { lPackets       :: !Int
  , lSubpackets    :: !Int
  , lMethods       :: !Int
  , lPacketSize    :: !Int
  , lComPacketSize :: !Int
  }
  deriving (Eq, Show)

-- | What the TPer takes, from its properties ('spTPerProperties'); a property
--   it does not report, Opal 2.30 Table 17's minimum (PDF 33).
tperLimits :: SimParams -> Limits
tperLimits p = Limits
  { lPackets       = prop maxPacketsName 1
  , lSubpackets    = prop maxSubpacketsName 1
  , lMethods       = prop maxMethodsName 1
  , lPacketSize    = prop maxPacketSizeName 2028
  , lComPacketSize = prop maxComPacketSizeName 2048
  }
  where prop name least = fromInteger (fromMaybe least (L.lookup name (spTPerProperties p)))

-- | What the host takes of the TPer's responses: Opal 2.30 Table 17's initial
--   assumptions ('hostRequirements'), the host's properties not being kept
--   before M4a (Core 5.2.2.4.1, PDF 145: the TPer SHALL NOT send more than the
--   host's), its ComPacket bounded by the TPer's MaxResponseComPacketSize as
--   well (Opal 2.30 3.3.1, PDF 29: a response SHALL fit the response buffer).
hostLimits :: SimParams -> Limits
hostLimits p = Limits
  { lPackets       = host maxPacketsName
  , lSubpackets    = host maxSubpacketsName
  , lMethods       = host maxMethodsName
  , lPacketSize    = host maxPacketSizeName
  , lComPacketSize = min (host maxComPacketSizeName) responseBuffer
  }
  where
    host name      = maybe 1 fromIntegral (L.lookup name hostRequirements)
    responseBuffer = maybe maxBound fromInteger (L.lookup maxResponseComPacketSizeName (spTPerProperties p))


-- * State

-- | A ComID's state (Core 3.3.3, PDF 56): a static ComID is Issued, with
--   no sessions until M4a.
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

-- | A ComID's state: its extension, its life and its synchronous state.
data ComIdState = ComIdState
  { cExt  :: !Word16
  , cLife :: !ComIdLife
  , cSync :: !SyncState
  }
  deriving (Eq, Show)

-- | The TPer: its parameters and its ComIDs.
data TPerState = TPerState
  { tsParams :: !SimParams
  , tsComIds :: !(Map Word16 ComIdState)
  }
  deriving (Eq, Show)

-- | A TPer as it starts: its static ComIDs, the base and the ones after it,
--   each Issued, with extension 0, awaiting IF-SEND; none past 0xFFFF or
--   reserved (parameters with such a ComID are refused, 'simParamsProblems').
initialState :: SimParams -> TPerState
initialState p = TPerState
  { tsParams = p
  , tsComIds = Map.fromList [ (c, ComIdState 0 Issued S1) | c <- staticComIds p, not (reservedComId c) ]
  }

-- | The static ComIDs parameters name: the base and the ones after it, as
--   far as 0xFFFF, counted in Integer so that none wraps.
staticComIds :: SimParams -> [Word16]
staticComIds p = [ fromInteger c | c <- [base .. base + toInteger (spNComIds p) - 1], c <= 0xFFFF ]
  where base = toInteger (spBaseComId p)

-- | Whether Core Table 26 reserves a ComID (PDF 55): any its rows do not
--   call non-reserved, for ComID management.
reservedComId :: Word16 -> Bool
reservedComId c = not (any ("non-reserved" `T.isInfixOf`) (maybe [] codeCells (codeRow cComIDAssignments (toInteger c))))


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
--   or a session aborted, with the TPer's CloseSession prepared or not
--   (3.3.10.7 b; from M4a).
data Outcome
  = Prepared ![ByteString]
  | Discarded !Text
  | SessionAborted !(Maybe ByteString)
  deriving (Eq, Show)
