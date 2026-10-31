{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Sim.Params
Description : What a simulator run chooses, what is out of range, and what each end takes in a ComPacket
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The parameters of the simulated TPer (design docs/spec-as-source.md section
8.1): where the spec leaves a value to the vendor or a range, the default is a
value the spec allows, each citing where (design 13.17: parameters, not
comments). They sit below "Hsed.Sim.Types", which re-exports them, so that
the hand-written Admin SP rows ("Hsed.Sim.AdminSP"), which the initial state
folds in and which read the MSID, the SID's PIN and its TryLimit from the
parameters, import no module that imports them.

Parameters out of range are refused, not wrapped or dropped
('simParamsProblems': a ComID past 0xFFFF or in Core Table 26's reserved
range, a version past its 4-bit field, a TPer property below the least Opal
2.30 Table 17 has a TPer report, a PIN longer than its column holds), so that
every descriptor the simulator sends encodes, every static ComID is one a
TPer may have, every limit is one an Opal TPer may report and every hand row
holds; the in-process transport is opened only with parameters that pass.
Opal 2.30 Table 17 is read from its generated form ("Hsed.Gen.Opal.Properties",
bound by @propsReq@, design 11): 'tperRequirements' is its TPer column, which
the default parameters report and a property not reported falls back to;
'hostRequirements' the host properties it has a TPer accept, each with the
least allowed; and 'initialHostProperties' the TPer's initial assumptions
about the host (Core Table 168 patched by Table 17's Initial Assumption cells,
the generated @initialAssumptions@), those of the host properties the TPer
supports: what a ComID's host properties start as (the host's default receive
length in "Hsed.Wire" cites the same MaxComPacketSize cell). 'Limits' are what
each end takes in a ComPacket (Core 5.2.2.4.1): the TPer's from its properties
('tperLimits'), the host's from the properties a ComID keeps of it
('comIdHostLimits'; 'hostLimits' those a ComID starts with).
-}
module Hsed.Sim.Params
  ( -- * Parameters
    SimParams (..)
  , defaultSimParams
  , LifeCycle (..)
  , Geometry (..)
  , Level0Profile (..)
  , simParamsProblems
  , staticComIds
  , reservedComId
    -- * Properties, and what each end takes
  , maxComPacketSizeName
  , maxResponseComPacketSizeName
  , maxPacketSizeName
  , maxIndTokenSizeName
  , maxPacketsName
  , maxSubpacketsName
  , maxMethodsName
  , maxSessionsName
  , defSessionTimeoutName
  , tperRequirements
  , hostRequirements
  , supportedHostProperties
  , initialHostProperties
  , Limits (..)
  , tperLimits
  , hostLimits
  , comIdHostLimits
  ) where

import qualified RIO.ByteString      as B
import qualified RIO.List            as L
import qualified RIO.Map             as Map
import           RIO
import qualified RIO.Text            as T

import           Hsed.Core.Code           (CodeRow (..), codeRow)
import           Hsed.Core.Properties     (Assumptions (..), Communicator (..), HostRequirement (..), PropertyRequirement (..),
                                           Requirement (..), TPerRequirement (..))
import           Hsed.Gen.Core.Codes      (cComIDAssignments)
import           Hsed.Gen.Opal.Properties (initialAssumptions, propertyRequirements)
import           Hsed.Level0              (Descriptor)
import           Hsed.Wire                (Unit (..), hex)


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
  , spMsid                 :: !ByteString         -- ^ C_PIN_MSID's PIN, the MSID (Opal 2.30 Table 25, PDF 48: VU)
  , spSidPin               :: !(Maybe ByteString) -- ^ C_PIN_SID's PIN; 'Nothing', the MSID's (Opal 2.30 4.2.1.8, PDF 49)
  , spSidTryLimit          :: !Natural            -- ^ C_PIN_SID's TryLimit (Table 25: VU); 0 no limit (Core 5.3.4.1.1.2, PDF 210)
  , spTPerResetEnabled     :: !Bool               -- ^ TPerInfo's ProgrammaticResetEnable: TPER_RESET enabled (Opal 2.30 Table 27, PDF 50)
  , spHostPropertyMax      :: ![(Text, Natural)]  -- ^ the most the TPer accepts of a host property, by name; none for the rest
                                                  --   (Opal 2.30 Table 17's maxima are VU, PDF 33-34)
  , spCloseSessionOnAbort  :: !Bool               -- ^ whether the TPer prepares CloseSession when it aborts a session, as it
                                                  --   MAY (Core 3.3.10.7 b, PDF 86; 3.3.7.1.5, PDF 75)
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
-- * the TPer properties at the least Opal 2.30 Table 17 has a TPer report
--   ('tperRequirements', PDF 33-34), DefSessionTimeout, which Table 17 leaves
--   VU, 0, no limit (Core Table 167, PDF 142);
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
-- * no profile;
-- * the MSID @HSED-SIM-MSID-0123456789ABCDEF01@, 32 bytes, as many as the
--   PIN column holds (Core Table 122, password, PDF 118), a value VU (Opal
--   2.30 Table 25, PDF 48) that no drive has, so that a report which leaks
--   it is seen for what it is;
-- * C_PIN_SID's PIN the MSID's: the take-ownership model UCT-03 step 1 reads
--   it by (Opal 2.30 4.2.1.8, PDF 49: SHALL, where an automated take
--   ownership process is required);
-- * C_PIN_SID's TryLimit 0, no limit (Opal 2.30 Table 25, PDF 48: VU; Core
--   5.3.4.1.1.2, PDF 210: Tries counts only while TryLimit is not 0);
-- * TPER_RESET disabled, TPerInfo's ProgrammaticResetEnable FALSE at the
--   factory (Opal 2.30 Table 27, PDF 50);
-- * no maximum on a host property (Opal 2.30 Table 17: Maximum allowed VU,
--   PDF 33-34);
-- * CloseSession prepared when a session is aborted, as the TPer MAY (Core
--   3.3.10.7 b, PDF 86; Opal 2.30 4.1.1.4 (O), PDF 35), so that the host
--   sees the abort (Test Cases 7.1, PDF 76, accepts either).
defaultSimParams :: SimParams
defaultSimParams = SimParams
  { spBaseComId            = 0x1004
  , spNComIds              = 1
  , spUnits                = UnitBytes
  , spLockingOFS           = ManufacturedInactive
  , spInactiveComIdDiscard = False
  , spTPerProperties       = [ (name, toInteger least) | (name, least) <- tperRequirements ]
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
  , spMsid                 = "HSED-SIM-MSID-0123456789ABCDEF01"
  , spSidPin               = Nothing
  , spSidTryLimit          = 0
  , spTPerResetEnabled     = False
  , spHostPropertyMax      = []
  , spCloseSessionOnAbort  = True
  }

-- | What is out of range in a run's parameters; none for parameters the
--   simulator takes:
--
-- * no static ComID, or one past 0xFFFF, or one Core Table 26 reserves
--   (PDF 55: 0x0000-0x0FFF are Level 0's, TPER_RESET's, TCG's and vendors';
--   Opal 2.30 Table 14, PDF 30, the same), so that none wraps to 0x0000;
-- * an SSC minor version or a Data Removal version past its descriptor's
--   4-bit field (Opal 2.30 Tables 6 and 8, PDF 23-24);
-- * a TPer property below the least Opal 2.30 Table 17 has a TPer report
--   ('tperRequirements', PDF 33-34), the limits the simulator reads of its
--   properties ('tperLimits') among them: so that no limit is one of none,
--   and every answer the simulator stages, RESPONSE_OVERFLOW's own included,
--   fits a response of the least MaxResponseComPacketSize;
-- * an MSID of no bytes (UCT-03 step 1b reads a PIN of some, Test Cases PDF
--   32), or an MSID or a SID PIN of more than the 32 the PIN column holds
--   (Core Table 122, password, PDF 118).
simParamsProblems :: SimParams -> [Text]
simParamsProblems p = concat
  [ [ "no static ComID (Number of ComIDs is 0x0001 or larger, Opal 2.30 Table 6)" | spNComIds p == 0 ]
  , [ "static ComIDs from " <> hex (spBaseComId p) <> ", " <> tshow (spNComIds p) <> " of them, run past 0xFFFF" | lastComId > 0xFFFF ]
  , [ "static ComID " <> hex c <> " is reserved (Core Table 26)" | c <- staticComIds p, reservedComId c ]
  , [ "SSC Minor Version Number " <> tshow (spSscMinor p) <> " is past its 4 bits" | spSscMinor p > 0xF ]
  , [ "Data Removal version " <> tshow (spDataRemovalVersion p) <> " is past its 4 bits" | spDataRemovalVersion p > 0xF ]
  , [ name <> " " <> tshow v <> ": below the " <> tshow least <> " Opal 2.30 Table 17 has a TPer report at least (PDF 33-34)"
    | (name, v) <- spTPerProperties p, Just least <- [L.lookup name tperRequirements], v < toInteger least ]
  , [ "an MSID of no bytes: UCT-03 step 1b reads a PIN of some (Test Cases PDF 32)" | B.null (spMsid p) ]
  , [ what <> " of " <> tshow (B.length pin) <> " bytes: the PIN column holds at most " <> tshow pinMost <> " (Core Table 122, password, PDF 118)"
    | (what, pin) <- [("the MSID", spMsid p)] <> [ ("the SID's PIN", sid) | Just sid <- [spSidPin p] ], B.length pin > pinMost ]
  ]
  where
    lastComId = toInteger (spBaseComId p) + toInteger (spNComIds p) - 1 :: Integer
    -- the size limitation of the password type (Core 5.1.3.63, Table 122, PDF 118: max_bytes, 32)
    pinMost = 32

-- | The static ComIDs parameters name: the base and the ones after it, as
--   far as 0xFFFF, counted in Integer so that none wraps.
staticComIds :: SimParams -> [Word16]
staticComIds p = [ fromInteger c | c <- [base .. base + toInteger (spNComIds p) - 1], c <= 0xFFFF ]
  where base = toInteger (spBaseComId p)

-- | Whether Core Table 26 reserves a ComID (PDF 55): any its rows do not
--   call non-reserved, for ComID management.
reservedComId :: Word16 -> Bool
reservedComId c = not (any ("non-reserved" `T.isInfixOf`) (maybe [] codeCells (codeRow cComIDAssignments (toInteger c))))


-- * Properties, and what each end takes

-- | The properties of Core Table 167 (PDF 141-142; "Hsed.Gen.Core.Properties")
--   the simulator and the jig read, by name: the largest ComPacket an end
--   takes, the largest ComPacket response, packet and token, packets per
--   ComPacket, subpackets per packet, method invocations or responses per
--   subpacket, sessions across the TPer's ComIDs, and a session's default
--   timeout.
maxComPacketSizeName, maxResponseComPacketSizeName, maxPacketSizeName, maxIndTokenSizeName, maxPacketsName, maxSubpacketsName,
  maxMethodsName, maxSessionsName, defSessionTimeoutName :: Text
maxComPacketSizeName         = "MaxComPacketSize"
maxResponseComPacketSizeName = "MaxResponseComPacketSize"
maxPacketSizeName            = "MaxPacketSize"
maxIndTokenSizeName          = "MaxIndTokenSize"
maxPacketsName               = "MaxPackets"
maxSubpacketsName            = "MaxSubpackets"
maxMethodsName               = "MaxMethods"
maxSessionsName              = "MaxSessions"
defSessionTimeoutName        = "DefSessionTimeout"

-- | Opal 2.30 Table 17 (PDF 33-34), Properties Requirements, the TPer's
--   column, as generated ("Hsed.Gen.Opal.Properties".@propertyRequirements@):
--   each TPer property an Opal TPer reports (M) with the least value it
--   reports; one whose least is VU (DefSessionTimeout) at 0, no limit (Core
--   Table 167, PDF 142: a session's default timeout, which the simulator, with
--   no clock, never applies; the lead's M4a decision, design 13.30); their
--   names are Core Table 167's ("Hsed.Gen.Core.Properties"). The default
--   parameters report exactly these, and a property a TPer does not report
--   falls back to its least ('tperLimits').
tperRequirements :: [(Text, Natural)]
tperRequirements =
  [ (reqName r, fromMaybe 0 (tprMinimum t)) | r <- propertyRequirements, Just t <- [reqTPer r], tprRequired t /= NotRequired ]

-- | Opal 2.30 Table 17 (PDF 33-34), Properties Requirements, the host's
--   column, as generated: each host property an Opal TPer accepts ((M), or
--   (O) were there one), with the least it allows; MaxResponseComPacketSize
--   (N), which a TPer may ignore and leave out of the HostProperties result,
--   is not among them. These are the host properties the simulator supports
--   (Core 5.2.2.3-4, PDF 143-144: the ones it keeps, raises to their least,
--   and returns; any other the host sends is ignored and not returned).
hostRequirements :: [(Text, Natural)]
hostRequirements = [ (reqName r, least) | (r, h) <- supportedHostProperties, Just least <- [hrMinimum h] ]

-- | The host properties the TPer supports, with what Table 17 requires of
--   each: the rows whose host requirement is (M) or (O), in the table's
--   order.
supportedHostProperties :: [(PropertyRequirement, HostRequirement)]
supportedHostProperties = [ (r, h) | r <- propertyRequirements, Just h <- [reqHost r], hrRequired h /= NotRequired ]

-- | The host's properties a ComID starts with, and returns to after a reset
--   (Core 5.2.2.4, PDF 144-145; 5.2.2.4.4-5, PDF 148): the TPer's initial
--   assumptions about the host, Core Table 168's patched by Opal 2.30 Table
--   17's Initial Assumption cells (PDF 33-34), as generated
--   ("Hsed.Gen.Opal.Properties".@initialAssumptions@, the TPer's about the
--   host), those of the host properties the TPer supports
--   ('supportedHostProperties': the others it never reads nor returns, 5.2.2.4).
initialHostProperties :: Map Text Natural
initialHostProperties = Map.fromList
  [ (name, v) | Assumptions TPer Host vs <- initialAssumptions, (name, v) <- vs, name `elem` map fst hostRequirements ]

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
--   it does not report, the least Opal 2.30 Table 17 has a TPer report
--   ('tperRequirements').
tperLimits :: SimParams -> Limits
tperLimits p = Limits
  { lPackets       = prop maxPacketsName
  , lSubpackets    = prop maxSubpacketsName
  , lMethods       = prop maxMethodsName
  , lPacketSize    = prop maxPacketSizeName
  , lComPacketSize = prop maxComPacketSizeName
  }
  where
    prop name  = fromInteger (fromMaybe (least name) (L.lookup name (spTPerProperties p)))
    least name = maybe 1 toInteger (L.lookup name tperRequirements)

-- | What the host takes of the TPer's responses, from the host's properties a
--   ComID keeps of it ("Hsed.Sim.Types".@cHostProps@: Opal 2.30 Table 17's
--   initial assumptions until the host sends its own, Core 5.2.2.4, PDF
--   144-145; the TPer SHALL NOT send more than the host's, 5.2.2.4.1, PDF
--   145): a property the host never sent is Table 17's initial assumption; one
--   it sent as 0 is no limit (Core Table 167, PDF 141-142); its ComPacket is
--   bounded by the TPer's MaxResponseComPacketSize as well (Opal 2.30 3.3.1,
--   PDF 29: a response SHALL fit the response buffer).
comIdHostLimits :: SimParams -> Map Text Natural -> Limits
comIdHostLimits p props = Limits
  { lPackets       = host maxPacketsName
  , lSubpackets    = host maxSubpacketsName
  , lMethods       = host maxMethodsName
  , lPacketSize    = host maxPacketSizeName
  , lComPacketSize = min (host maxComPacketSizeName) responseBuffer
  }
  where
    host name      = maybe (least name) bound (Map.lookup name props)
    bound 0        = maxBound
    bound n        = fromInteger (min (toInteger n) (toInteger (maxBound :: Int)))
    least name     = maybe 1 fromIntegral (Map.lookup name initialHostProperties)
    responseBuffer = maybe maxBound fromInteger (L.lookup maxResponseComPacketSizeName (spTPerProperties p))

-- | What the host takes before any Properties exchange: Opal 2.30 Table 17's
--   initial assumptions ('initialHostProperties'), what every ComID starts
--   with.
hostLimits :: SimParams -> Limits
hostLimits p = comIdHostLimits p initialHostProperties
