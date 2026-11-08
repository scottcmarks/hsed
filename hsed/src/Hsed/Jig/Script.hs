{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
expectations of a status or of a Table 45 answer; and, since M4a, the
session verbs: Properties with the host's own, StartSession read back as
its SyncSession, Get in a session, the host's End of Session, STACK_RESET,
and a session opened around a case's steps; and, since M5b, the Locking
SP's cases' verbs: a session as an authority with its PIN read from the
simulator's state ('asAuthority'), Set in a session, Start and End
Transaction, an IF-SEND in a session with its IF-RECVs apart, an answer
that is nothing or the TPer's CloseSession, a cell of the simulator's
state, TPER_RESET and a power cycle, the last two the simulator's alone
(design 9.3). A script runs in the run's
environment ('RunEnv': its target, transcript, channel and options) and
keeps what later cases need ('RunCtx': the Level 0 response, the base
ComID UCT-01 records, the properties Properties reported, the session a
case has open, the secrets it read, and what it observed for the report).

A script runs on one end of a run, the simulator or a drive (the runner
steps a pair's ends one after the other, "Hsed.Jig.Run"). On the simulator
the verbs use its transport as it is; on a drive every command goes through
the Guard ("Hsed.Jig.Guard".'GuardedDrive', which "Hsed.Jig.Target" puts the
drive behind when it opens it) under the drive's policy, Level0Only until
'baseComId' binds the drive's base ComID where the run admits more than
Level 0 Discovery on it, so that a refused command is recorded and answered
'Unsupported', never issued. On a drive an IF-RECV asks for 2048 bytes
whatever the TPer reports ('recvLength'): the host's MaxComPacketSize as
Opal 2.30 Table 17 has the TPer assume it (PDF 33), the value STACK_RESET
puts back (Core 3.3.4.7.5 item 7, PDF 65), and within what the kext's
command carries; a longer answer is asked for again at its MinTransfer
(Table 45, PDF 83). The host's exchange on a drive waits 15 s for each
command ('exchangeOn'), past the kext's own 10 s on its SCSI task, which
makes the host's giving up unlikely; the wait never cuts a command short
(every command of a drive is given under its recording's mask, so the
transport judges and keeps its answer whenever it comes); and where the
host gives up all the same, what the drive did with the command is no
longer known, and the drive is latched for the rest of the run
("Hsed.Jig.Guard".'Hsed.Jig.Guard.driveLatch'): nothing more reaches it,
End of Session and STACK_RESET included. 'admitsStackReset'
says whether the end takes STACK_RESET once its base ComID is bound: the
simulator always, a drive where the run admits it (@--recover@, or
STACK_RESET before each case) and has not latched it; and then
'closingStep' sends one after a step that failed, and 'startSession' one
after a StartSession whose SyncSession it could not read, so that no
session the TPer opened outlives its case unknown. Where the end takes
none, or the reset does not answer Success/Failure 0, such a session is
kept in the run's context (a session whose End of Session went unanswered
as 'rcSession', one whose SyncSession was not read as 'rcSessionUnread'),
for the next case's NoSessions and for the reports, which name it with
@--recover@'s hint.

A run is stopped between commands, never in one (M4c): its stop flag
('reStop') holds why (a signal's name; or @--interrupt-in-session@, which
'startSession' sets once it has opened a session on the drive's end, the
session left open on purpose), and 'stopIfAsked', which every verb that
issues a command checks first, throws 'Interrupted' with it: 'discover',
'ifRecvOn', 'rawIfSend', 'drain', 'stackReset' and the host's exchange
('exchangeOn', and with it Properties, StartSession, Get and End of
Session). What a verb has begun runs to its end (an exchange's IF-RECVs, a
drain's, STACK_RESET's polls check nothing), and 'attempt' makes no
verdict of 'Interrupted', which goes on to the runner. An answer a step
could not read, which may carry a secret where the step did not find it
(UCT-03's MSID read, a drive's SyncIdle probe), is masked in every report
as one ('transcriptMark', 'maskAnswersSince'; design 9.3 item 5); and on a
drive, SP_BUSY or NO_SESSIONS_AVAILABLE answered where a session was
expected to open says that a session an earlier run left open holds the
Admin SP, which @hsed-jig --recover@ clears ('expectSyncStatus').

An expectation that does not hold ends the script ('unmet'); 'attempt'
turns what a script comes to into its 'Verdict'; 'note' adds a line to the
transcript, for what a case does apart from its verdict; 'observe' adds a
name and a value to the case's observations, which the reports print beside
the verdict (UCT-02's two tables of properties). The host's side of Table
45 is read from the generated table
("Hsed.Gen.Core.StateTables".@table45@): an answer's Length,
OutstandingData and MinTransfer match a row when each equals what the row
prints and is not zero where the row names a quantity ('t45Of',
'expectT45'), an answer being a ComPacket of the ComID asked and a static
ComID's extension ('t45On'); 'drain' asks again as the host's loop does
("Hsed.Wire".@exchangeComPacket@), by MinTransfer, then by OutstandingData,
until everything is returned. The ComPackets the jig sends are the Test
Cases' (Table 6, PDF 27): one packet of the control session holding one
Data subpacket ('controlComPacket'), or one packet of an open session
('sessionComPacket'), or, for ETC-15 alone, one control packet of several
subpackets ('controlSubpackets').

Every whole exchange is the host's loop ('exchangeOn': IF-SEND through the
end's path, then IF-RECV until the TPer has returned everything), and every
answer is read as the host reads a TPer's stream ('returnedOf':
"Hsed.Core.Stream"'s atoms under the host's policy, then its
'Returned' items: a Session Manager response is an invocation of SMUID, Core
3.3.7.1.3, PDF 73; a method's response its results and status, 3.2.4.2, PDF
49; the TPer's End of Session, 3.2.2.3.3.3, PDF 37). StartSession is sent
with HSN 1 and Write True (Test Cases 4.1 e and h, PDF 26) and its
SyncSession read through the generated record ("Hsed.Core.Method".@fromCall@,
Core 5.2.3.2, PDF 150); the host's End of Session is CLOSE_SESSION (Test
Cases PDF 21), answered with the TPer's own. A status is named by Core Table
166's row ('statusName'), and where the Test Cases expect an error status a
session abort is accepted instead (7.1, PDF 76: 'expectSyncStatus').
-}
module Hsed.Jig.Script
  ( -- * The run's environment
    RunCtx (..)
  , emptyRunCtx
  , RunEnv (..)
  , Script
  , Interrupted (..)
    -- * Verdicts
  , Verdict (..)
  , attempt
  , unmet
  , note
  , observe
  , stopIfAsked
  , admitsStackReset
    -- * Verbs
  , discover
  , ifRecvOn
  , rawIfSend
  , properties
  , expectStatus
  , expectT45
  , drain
  , simOnly
    -- * Session verbs
  , PropertiesAnswer (..)
  , StartArgs (..)
  , SessionHandle (..)
  , SyncAnswer (..)
  , MethodOutcome (..)
  , propertiesWith
  , startSession
  , getCells
  , endSession
  , sessionComPacket
  , controlSubpackets
  , expectNoResponse
  , expectMethodStatus
  , expectSyncStatus
  , stackReset
  , opalV2Of
  , withSession
    -- * The verbs of M5b's cases
  , asAuthority
  , setCells
  , startTransaction
  , endTransaction
  , sendInSession
  , ifRecvWith
  , framesOf
  , expectNoResponseOrCloseSession
  , sessionGone
  , cleared
  , simCell
  , tperReset
  , powerCycle
  , heedNotices
    -- * What the verbs build on
  , baseComId
  , recvLength
  , driveRecvLength
  , driveTimeoutMicros
  , transcriptMark
  , maskAnswersSince
  , featureIn
  , transferFor
  , sendLength
  , controlComPacket
  , propertiesCall
  , maxComPacketSizeName
  , hostSessionNumber
  , exchangeOn
  , returnedOf
  , statusName
  , exchanged
  , opened
  , closingStep
  , adminSPAsAnybody
  , startSessionCall
  , sessionSubpackets
  , byteRows
  , dataStoreTable
  , tperInfoObject
  , tperResetBytes
  , spName
  , forgetSessionsOn
  , namedColumn
  , peekSim
  , Credential (..)
  , t45Of
  , t45On
  , t45RowOf
  , describeStatus
  , describeExchange
  , verdictText
  ) where

import qualified Data.Aeson             as A
import           Numeric                (showHex)
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO
import qualified RIO.Text               as T

import           Hsed.Core.Call         (MethodCall (..), MethodHeader (..), MethodResponse (..))
import           Hsed.Core.CellBlock    (CellBlock (..), RowAddress (..), cellBlockValue, noCells)
import           Hsed.Core.Code         (CodeRow (..), codeRow)
import           Hsed.Core.Feature      (Feature (..), decodeFeature)
import           Hsed.Core.Layout       (Layout (..), LayoutError)
import           Hsed.Core.Method       (MethodSignature (..), SigResponse (..), Signature (..), fromCall, invocation,
                                         paramErrorText, positional)
import           Hsed.Core.Preconfig    (PreCell (Given), PreconfRow (prCells), PreconfTable (ptRows))
import           Hsed.Core.Properties   (HostRequirement (..), PropertyRequirement (..), requirement)
import           Hsed.Core.Record       (DecodedRecord (..), LayoutRecord (..), decodeRecord, decodeRecordStrict, encodeRecord)
import           Hsed.Core.Stream       (Atom (..), Fault (..), Returned (..), atoms, hostPolicy, returnedItems)
import           Hsed.Core.StreamItem   (StreamItem (..))
import           Hsed.Core.Sync         (Setting (..), StateRow (..), StateTable (..), settingOf)
import           Hsed.Core.Table        (Cell (..), Col (..), Pre (Pre), TableDesc (..), columnNamed)
import           Hsed.Core.Token        (IsToken (..), Token (..))
import           Hsed.Core.UID          (UID, hNull, uidLower, (+:+))
import           Hsed.Core.Value        (Datum (..), IsValue (..), List (..), Name (..), NamedValue (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes    (StatusCode (..), cStatusCodes, pattern NO_SESSIONS_AVAILABLE, pattern SP_BUSY, pattern SUCCESS)
import           Hsed.Gen.Core.Layouts  (ComPacket (..), ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..),
                                         NoResponseAvailable (..), PacketHeader (..), STACK_RESETCommandRequest (..),
                                         STACK_RESETCommandResponse (..), STACK_RESETPending (..))
import qualified Hsed.Gen.Core.Methods  as Methods
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Gen.Core.UIDs     (uCloseSession, uProperties, uSMUID, uSP_Admin)
import           Hsed.Gen.Opal.Features (Level0DiscoveryOpalSSCV2Feature (..))
import           Hsed.Gen.Opal.Preconfig.AdminSP (adminSPTPerInfoTablePreconfiguration, uSP_Locking)
import qualified Hsed.Gen.Opal.Preconfig.LockingSP as LockingSP
import           Hsed.Gen.Opal.Properties (propertyRequirements)
import           Hsed.Jig.Guard         (Allowed (..), GuardedDrive, blockedResponse, driveAllowed, driveBind, driveInfo, driveLatch,
                                         driveLatched, driveRecv, driveSend, driveTransport, uintegerOf)
import           Hsed.Jig.Options       (RunOptions (..), TargetSpec (..), defaultRunOptions, renderWsUrl)
import           Hsed.Jig.Simulator     (Opened (..), Simulator (..))
import           Hsed.Jig.Target        (Target (..))
import           Hsed.Jig.Transcript    (TEntry (..), Transcript, entries, numberOf, record)
import           Hsed.Level0            (Descriptor (..), Level0 (..), Level0Problem (..), describeProblem, splitLevel0)
import           Hsed.Packet            (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, encodeComPacket)
import           Hsed.Sim.Sync          (T45Row (..), t45Rows)
import           Hsed.Sim.Types         (maxComPacketSizeName)
import           Hsed.Sim.View          (Credential (..), SimQuery (..), SimView (..))
import           Hsed.Transport.Cdb     (level0Cdb)
import           Hsed.Transport.Drive   (DriveInfo (..))
import           Hsed.Wire              (Cap (..), Channel, Describe (..), Dir (..), Event (..), ExchangeError (..), ExchangeOpts (..),
                                         IfStatus (..), Request (..), Response (..), Transport, Unit (..),
                                         comIdManagementProtocol, comPacketProtocol, dataKind, defaultExchangeOpts,
                                         describeTransport, exchangeComPacket, extendedComId, hex, ifRecv, ifSend, ifStatusDetail,
                                         ifStatusName, inject, level0ComId, mostCarried, roundUp, staticExtension, tperResetComId,
                                         transferLength, unitOf)


-- * The run's environment

-- | What the run has learned that later cases use, and what a case has
--   observed for its report.
data RunCtx = RunCtx
  { rcLevel0           :: !(Maybe Level0)           -- ^ the target's Level 0 response, once discovered
  , rcBaseComId        :: !(Maybe Word16)           -- ^ the base ComID UCT-01 records (Test Cases PDF 29)
  , rcMaxComPacketSize :: !(Maybe Word32)           -- ^ the TPer's MaxComPacketSize, once Properties reported it
  , rcSession          :: !(Maybe SessionHandle)    -- ^ the session open, if a case left one (M4a)
  , rcSecrets          :: ![ByteString]             -- ^ the secrets read (the MSID), masked in every report unless @--show-msid@
  , rcTPerProperties   :: !(Maybe [(Text, Integer)]) -- ^ the TPer's properties, once Properties reported them
  , rcHostProperties   :: !(Maybe [(Text, Integer)]) -- ^ the host's, as the TPer answered them
  , rcNegotiated       :: !(Maybe ([(Text, Natural)], PropertiesAnswer))
                                                    -- ^ the host properties a Properties sent (UCT-02's), and the TPer's
                                                    --   answer: the Pair's Properties comparison (design 13.32 (6))
  , rcObservations     :: ![(Text, Text)]           -- ^ what the case running has observed ('observe'), in order; the
                                                    --   runner takes them for the case's report and starts the next
                                                    --   case with none
  , rcSessionUnread    :: !(Maybe (Word16, Text))   -- ^ a session the TPer may have opened on a ComID that no STACK_RESET
                                                    --   has cleared since: a StartSession issued whose SyncSession the
                                                    --   host could not read, and why ('startSession')
  }
  deriving (Eq, Show)

-- | What a run starts knowing: nothing.
emptyRunCtx :: RunCtx
emptyRunCtx = RunCtx Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing [] Nothing

-- | A script's environment: the run's target, transcript, channel, what it
--   has learned, its options, its stop flag, and where it logs.
data RunEnv = RunEnv
  { reTarget     :: !Target
  , reTranscript :: !Transcript
  , reChannel    :: !Channel
  , reCtx        :: !(IORef RunCtx)
  , reOpts       :: !RunOptions
  , reStop       :: !(IORef (Maybe Text))  -- ^ why the run is to stop, once asked (a signal's name,
                                           --   @--interrupt-in-session@): nothing new is issued after it
  , reLogFunc    :: !LogFunc
  }

instance HasLogFunc RunEnv where
  logFuncL = lens reLogFunc (\e f -> e { reLogFunc = f })

-- | A case's steps.
type Script = RIO RunEnv

-- | A script stopped because the run was asked to stop, and why: thrown
--   between commands, never in the middle of an exchange begun (M4c).
newtype Interrupted = Interrupted Text
  deriving (Show)

instance Exception Interrupted


-- * Verdicts

-- | What a case, or a step of the jig's own, comes to.
data Verdict
  = Pass
  | Fail ![Text]         -- ^ each expectation that did not hold
  | Skipped !Text        -- ^ not run, and why (a prerequisite unmet, a capability the target lacks)
  | Unobservable !Text   -- ^ run, but what it checks cannot be seen on this target
  deriving (Eq, Show)

instance A.ToJSON Verdict where
  toJSON = \case
    Pass           -> A.object ["verdict" A..= ("pass" :: Text)]
    Fail rs        -> A.object ["verdict" A..= ("fail" :: Text), "reasons" A..= rs]
    Skipped r      -> A.object ["verdict" A..= ("skipped" :: Text), "reason" A..= r]
    Unobservable r -> A.object ["verdict" A..= ("unobservable" :: Text), "reason" A..= r]

-- | Why a script stopped short: an expectation that did not hold, or one
--   this target cannot observe.
data Stopped
  = Unmet !Text
  | Unseen !Text
  deriving (Show)

instance Exception Stopped

-- | Stop a script: an expectation that did not hold, said in words.
unmet :: Text -> Script a
unmet = throwIO . Unmet

-- | What a script comes to: its own verdict; 'Fail' where an expectation
--   did not hold ('unmet'); 'Unobservable' where this target cannot show
--   what it expects; and 'Fail' naming any other exception it threw, but
--   'Interrupted', which is no verdict of the script's: rethrown, for the
--   runner to take (M4c).
attempt :: Script Verdict -> Script Verdict
attempt s = tryAny s >>= \case
  Right v -> pure v
  Left e
    | Just stop@(Interrupted _) <- fromException e -> throwIO stop
    | otherwise -> pure $ case fromException e of
        Just (Unmet t)  -> Fail [t]
        Just (Unseen t) -> Unobservable t
        Nothing         -> Fail ["exception: " <> T.pack (displayException e)]


-- * The end a script talks to

-- | One end of a run: the simulator, or a drive behind the Guard.
data End
  = SimEnd !(Transport 'ReadWrite)
  | DriveEnd !GuardedDrive

theEnd :: Script End
theEnd = asks reTarget >>= \case
  SimTarget tr _ -> pure (SimEnd tr)
  DriveTarget g  -> pure (DriveEnd g)
  PairTarget _ _ -> unmet "a script runs on one end of a pair at a time (Hsed.Jig.Run)"

-- | The unit of the end's transfer lengths.
endUnit :: Script Unit
endUnit = theEnd <&> \case
  SimEnd tr  -> unitOf tr
  DriveEnd g -> unitOf (driveTransport g)

-- | IF-RECV on the end: the simulator's transport, or the drive's through
--   the Guard, under its policy.
recvOn :: Word8 -> Word16 -> Word32 -> Script Response
recvOn p c len = theEnd >>= \case
  SimEnd tr  -> liftIO (ifRecv tr p c len)
  DriveEnd g -> liftIO (either blockedResponse id <$> driveRecv g (Request IfRecv p c len mempty))

-- | IF-SEND on the end, the payload zero-padded to the transfer length: the
--   simulator's transport, or the drive's through the Guard, under its
--   policy.
sendOn :: Word8 -> Word16 -> Word32 -> ByteString -> Script Response
sendOn p c len bytes = theEnd >>= \case
  SimEnd tr  -> liftIO (ifSend tr p c len bytes)
  DriveEnd g -> do
    let padded = bytes <> B.replicate (fromIntegral len - B.length bytes) 0
    liftIO (either blockedResponse id <$> driveSend g (Request IfSend p c len padded))

-- | Add a note to the run's transcript.
note :: Text -> Script ()
note t = asks reTranscript >>= \tr -> liftIO (record tr (Note 0 t))

-- | Add an observation to the case running, a name and a value, for its
--   report (design 9.1: a table a case reads, reported apart from the
--   verdict).
observe :: Text -> Text -> Script ()
observe name value' = modifyCtx (\c -> c { rcObservations = rcObservations c <> [(name, value')] })

-- | Stop the script if the run was asked to stop ('reStop'), throwing
--   'Interrupted' with why: what each verb that issues a command does
--   first, so that nothing new is issued once a run is stopped, and a
--   command or an exchange begun completes (design 9.3, M4c).
stopIfAsked :: Script ()
stopIfAsked = asks reStop >>= readIORef >>= traverse_ (throwIO . Interrupted)

-- | Whether the end takes STACK_RESET: on the simulator, always; on a
--   drive, where what binding its base ComID admits holds 'AllowStackReset'
--   (@--recover@, or STACK_RESET before each case,
--   "Hsed.Jig.Options".@roStackResetBeforeCases@), whether or not the base
--   is bound yet, unless the run latched it ('driveLatch'). Its policy takes
--   STACK_RESET only once the base is bound, Level0Only before the bind
--   taking none; 'stackReset' binds nothing itself, so a reset on a drive
--   comes after 'baseComId', which binds.
admitsStackReset :: Script Bool
admitsStackReset = theEnd >>= \case
  SimEnd _   -> pure True
  DriveEnd g -> liftIO (driveLatched g) <&> \latched -> AllowStackReset `elem` driveAllowed g && isNothing latched

readCtx :: Script RunCtx
readCtx = asks reCtx >>= readIORef

modifyCtx :: (RunCtx -> RunCtx) -> Script ()
modifyCtx f = asks reCtx >>= (`modifyIORef'` f)


-- * Verbs

-- | Level 0 Discovery: IF-RECV of protocol 1, ComID 1, the response split
--   ("Hsed.Level0") and kept. The transfer length is the run's
--   (@--level0-len@, in the end's unit); when the response's Length of
--   Parameter Data asks for more, it is asked for once more with that
--   length, up to 255 blocks of 512 bytes (the count byte of the kext's CDB,
--   design 6.2), and only after a response that succeeded (a failed kext
--   command leaks a SCSI task in the kext, so none is asked again). On a
--   drive the kext's CDB is noted; the split's problems are noted too, one
--   Opal allows said so ("Hsed.Level0".'describeProblem').
discover :: Script Level0
discover = do
  stopIfAsked
  u <- endUnit
  len <- roundUp u . roLevel0Len <$> asks reOpts
  firstTry <- level0Of len
  l0 <- case [ asked | BeyondTransfer asked _ <- l0Problems firstTry ] of
    asked : _ | again > len -> level0Of again
      where again = min level0Most (roundUp u asked)
    _ -> pure firstTry
  unless (null (l0Problems l0)) (note ("Level 0 Discovery: " <> T.intercalate "; " (map describeProblem (l0Problems l0))))
  l0 <$ modifyCtx (\c -> c { rcLevel0 = Just l0 })

-- | One Level 0 IF-RECV, split; on a drive the CDB that carries it noted
--   first.
level0Of :: Word32 -> Script Level0
level0Of len = do
  theEnd >>= \case
    DriveEnd g | Just di <- driveInfo g -> note ("Level 0 Discovery, " <> tshow len <> " bytes: "
                                                <> either ("no CDB: " <>) (("the CDB, as the kext's shim builds it, " <>) . spacedHex)
                                                          (level0Cdb (diInterface di) (len `div` blockBytes)))
    _                                   -> pure ()
  r <- recvOn comPacketProtocol level0ComId len
  unless (status r == Good) (unmet ("Level 0 Discovery: the IF-RECV answered " <> describeStatus (status r)))
  either (unmet . ("Level 0 Discovery: the response does not split: " <>) . tshow) pure (splitLevel0 (rpayload r))

-- | The bytes of a 512-byte block, and the most a Level 0 IF-RECV asks for:
--   255 of them, the count byte of the kext's CDB, "Hsed.Wire".'mostCarried'
--   (design 6.2).
blockBytes, level0Most :: Word32
blockBytes = transferLength UnitBlocks512 1
level0Most = mostCarried UnitBlocks512

-- | IF-RECV of the run's transfer length on a ComID ('recvLength').
ifRecvOn :: Word16 -> Script Response
ifRecvOn c = stopIfAsked >> recvLength >>= recvOn comPacketProtocol c

-- | The length an IF-RECV of protocol 1 asks for, in the end's unit: on the
--   simulator the MaxComPacketSize Properties reported, or the least an
--   Opal TPer reports ('defaultExchangeOpts'); on a drive
--   'driveRecvLength', whatever the TPer reports.
recvLength :: Script Word32
recvLength = theEnd >>= \case
  DriveEnd g -> pure (roundUp (unitOf (driveTransport g)) driveRecvLength)
  SimEnd tr  -> roundUp (unitOf tr) . fromMaybe (xRecvLen defaultExchangeOpts) . rcMaxComPacketSize <$> readCtx

-- | A drive's IF-RECV length: the host's MaxComPacketSize as the TPer
--   assumes it until Properties has succeeded, Opal 2.30 Table 17's Initial
--   Assumption, 2048 (row 1, PDF 33; the generated 'propertyRequirements',
--   else 'defaultExchangeOpts''s 2048), the value STACK_RESET puts back
--   (Core 3.3.4.7.5 item 7, PDF 65), whatever the TPer reports: a TPer's
--   MaxComPacketSize may be more than the 255 blocks of 512 bytes the
--   kext's command carries (130,560), and a longer answer is asked for again
--   at its MinTransfer (Core Table 45 rows 2-3, PDF 83).
driveRecvLength :: Word32
driveRecvLength = maybe (xRecvLen defaultExchangeOpts) fromIntegral
                        (requirement maxComPacketSizeName propertyRequirements >>= reqHost >>= hrInitial)

-- | IF-SEND of a payload on a ComID with a transfer length, past the run's
--   channel.
rawIfSend :: Word16 -> Word32 -> ByteString -> Script Response
rawIfSend c len bytes = stopIfAsked >> sendOn comPacketProtocol c len bytes

-- | SMUID.Properties, a whole exchange on the base ComID: the TPer's
--   properties by name ('propertiesWith' no host properties). The host's
--   loop is "Hsed.Wire"'s, its IF-SEND the simulator's or the Guard's;
--   MaxComPacketSize, once reported, is kept.
properties :: Script (Either ExchangeError [(Text, Integer)])
properties = fmap paTPer <$> propertiesWith Nothing

-- | Expect a response's status, the expectation named. On a drive an error
--   the kext answers as a transport failure cannot show which (design 6.2):
--   expecting another status there is unobservable.
expectStatus :: Text -> IfStatus -> Response -> Script ()
expectStatus name expected r = unless (status r == expected) $ theEnd >>= \case
  DriveEnd _ | TransportFailure t <- status r, expected /= Good ->
    throwIO (Unseen (name <> ": " <> ifStatusName expected <> " expected; the transport answered a failure (" <> t
                     <> "), which carries no interface status (design 6.2)"))
  _ -> unmet (name <> ": " <> describeStatus (status r) <> ", not " <> describeStatus expected)

-- | Expect an IF-RECV response on a ComID to be a ComPacket of it whose
--   header is a Table 45 row's (Core 3.3.10.5, PDF 84-85: a response
--   ComPacket for the ExtendedComID specified).
expectT45 :: Text -> Word16 -> T45Row -> Response -> Script ()
expectT45 name c row r = do
  expectStatus name Good r
  answer <- either (unmet . ((name <> ": ") <>)) pure (t45On c r)
  case L.lookup row (zip t45Rows (states table45)) of
    Nothing -> unmet (name <> ": Table 45 has no row " <> tshow row)
    Just sr -> unless (matches sr answer) $
      unmet (name <> ": answered " <> showAnswer answer <> ", not Table 45's \"" <> state sr <> "\"")

-- | IF-RECV on a ComID until all is returned: the synchronous protocol
--   idle again. Each answer, a ComPacket of the ComID and a static ComID's
--   extension ('t45On'), is classified as the host's loop classifies it
--   ("Hsed.Wire".@exchangeComPacket@; Core Table 45, PDF 83): a MinTransfer
--   not zero (rows 2 and 3) asks again at once with at least that length,
--   and with no packets and no longer a length is no row; else
--   OutstandingData zero (rows 5 and 6) ends it; else the TPer has more to
--   come (rows 1 and 4), asked again after the host's poll interval. So an
--   answer Core 3.3.10.2.1 3.d.ii allows an SSC (OutstandingData the total,
--   or MinTransfer zero, PDF 83) drains as well. It gives up after as many
--   polls as the host's timeout holds (the run's @roExchange@,
--   'defaultExchangeOpts' unless a test shortens it).
drain :: Word16 -> Script ()
drain c = do
  stopIfAsked
  x <- asks (roExchange . reOpts)
  recvLength >>= go x 0
  where
    go x n len
      | n > pollsOf x = unmet ("drain " <> hex c <> ": still not idle after " <> tshow (pollsOf x) <> " IF-RECVs")
      | otherwise = do
          r <- recvOn comPacketProtocol c len
          unless (status r == Good) (unmet ("drain " <> hex c <> ": the IF-RECV answered " <> describeStatus (status r)))
          answer@(returned, outstanding, minTransfer) <- either (unmet . (("drain " <> hex c <> ": ") <>)) pure (t45On c r)
          u <- endUnit
          let longer = max len (roundUp u (fromInteger minTransfer))
          if | minTransfer /= 0, returned == 0, longer <= len ->
                 unmet ("drain " <> hex c <> ": answered " <> showAnswer answer <> ", a MinTransfer no larger than the request with "
                        <> "no packets: no row of Table 45")
             | minTransfer /= 0 -> go x (n + 1) longer
             | outstanding == 0 -> pure ()
             | otherwise        -> threadDelay (xPollMicros x) >> go x (n + 1) len

-- | How many times the host asks again while the TPer is busy: as many as
--   its timeout holds polls.
pollsOf :: ExchangeOpts -> Int
pollsOf x = xTimeoutMicros x `div` max 1 (xPollMicros x)

-- | Run a step on the simulator alone: 'Nothing' on a drive.
simOnly :: Script a -> Script (Maybe a)
simOnly s = asks reTarget >>= \case
  SimTarget _ _ -> Just <$> s
  _             -> pure Nothing


-- * Session verbs

-- | What Properties answered: the TPer's properties, and the host's where
--   the host sent its own (Core 5.2.2.1, PDF 140).
data PropertiesAnswer = PropertiesAnswer
  { paTPer :: ![(Text, Integer)]
  , paHost :: !(Maybe [(Text, Integer)])
  }
  deriving (Eq, Show)

-- | What StartSession is sent with (Core 5.2.3.1, PDF 149; Opal 2.30
--   4.1.1.2, PDF 34): the SP, the HostSigningAuthority if any, its
--   HostChallenge if any, Write.
data StartArgs = StartArgs
  { saSP        :: !UID
  , saAuthority :: !(Maybe UID)
  , saChallenge :: !(Maybe ByteString)
  , saWrite     :: !Bool
  }
  deriving (Eq, Show)

-- | A session the jig has open: its ComID, the TPer's TSN and the host's
--   HSN (Core 3.3.7.1.1, PDF 72).
data SessionHandle = SessionHandle
  { shComId :: !Word16
  , shTSN   :: !Word32
  , shHSN   :: !Word32
  }
  deriving (Eq, Show)

-- | What SyncSession answered: a session open, the startup failed with a
--   status (Core 5.3.4.1.5, PDF 215), or no SyncSession at all and why.
data SyncAnswer
  = Synced !SessionHandle
  | SyncFailed !StatusCode
  | NoSync !Text
  deriving (Eq, Show)

-- | A method's response in a session: its results and its status (Core
--   3.2.4.2, PDF 49).
data MethodOutcome = MethodOutcome
  { moResults :: ![Value]
  , moStatus  :: !StatusCode
  }
  deriving (Eq, Show)

-- | SMUID.Properties, a whole exchange on the base ComID, with the host's
--   properties where given (UCT-02, Test Cases PDF 32: each a name and a
--   uinteger, Core 5.2.2.1.1, PDF 140): the TPer's properties and the
--   host's answered, read against the generated signature's response
--   (@=> SMUID.Properties [ Properties : …, HostProperties = … ]@,
--   "Hsed.Core.Method".'positional'), both kept in the run's context, and
--   MaxComPacketSize for the exchanges after it; where the host's were
--   sent, they and the answer too, for the Pair's Properties comparison. A
--   property that is no name with a number is left out, as M3 read them.
propertiesWith :: Maybe [(Text, Natural)] -> Script (Either ExchangeError PropertiesAnswer)
propertiesWith host = do
  c <- baseComId
  bytes <- framed "the Properties ComPacket" (controlComPacket c (generate call))
  answer <- (>>= propertiesAnswerOf) <$> exchangeOn c bytes
  for_ answer $ \pa -> modifyCtx $ \x -> x
    { rcTPerProperties   = Just (paTPer pa)
    , rcHostProperties   = paHost pa
    , rcMaxComPacketSize = (fromInteger <$> L.lookup maxComPacketSizeName (paTPer pa)) <|> rcMaxComPacketSize x
    , rcNegotiated       = fmap (\sent -> (sent, pa)) host <|> rcNegotiated x
    }
  pure answer
  where
    call = invocation uSMUID (Methods.Properties (namedNumbersValue <$> host))

-- | The TPer's answer to Properties: an invocation of SMUID.Properties with
--   status SUCCESS (a Session Manager response is an invocation, Core
--   3.2.4.2; 5.2.2.1.2, PDF 140), its parameters the response's: the TPer's
--   properties, then HostProperties where the TPer answers the host's.
propertiesAnswerOf :: [PacketFrame] -> Either ExchangeError PropertiesAnswer
propertiesAnswerOf packets = do
  items <- first Malformed (returnedOf packets)
  mc <- case [ mc | ReturnedCall mc@(MethodCall (MethodHeader i m) _ _) <- items, i == uSMUID, m == uProperties ] of
    mc : _ -> Right mc
    []     -> Left (Malformed (if null packets then noResponse else "no SMUID.Properties in the answer"))
  unless (_statusCode mc == statusValue SUCCESS) $ Left (Malformed ("Properties answered " <> statusName (StatusCode (_statusCode mc))))
  (reqs, opts) <- case sigResponse (signature @Methods.Properties) of
    Invocation _ rs os -> Right (rs, os)
    _                  -> Left (Malformed "Properties' generated signature answers with no invocation")
  (required, optionals) <- first (Malformed . ("the answer's parameters are not Properties' response's: " <>) . paramErrorText)
                                 (positional reqs opts (_parameters mc))
  tper <- case required of
    [v] -> maybe (Left (Malformed "the answer's Properties is no list")) Right (namedNumbers v)
    _   -> Left (Malformed "the answer's Properties is not one parameter")
  hostAnswered <- case optionals of
    Just v : _ -> maybe (Left (Malformed "the answer's HostProperties is no list")) (Right . Just) (namedNumbers v)
    _          -> Right Nothing
  pure (PropertiesAnswer tper hostAnswered)

-- | SMUID.StartSession on the base ComID with HSN 1 (Test Cases 4.1 h, PDF
--   26) and Write as asked (True in every case, 4.1 e), its HostChallenge
--   and HostSigningAuthority where given (Core 5.2.3.1, PDF 149), its
--   SyncSession read back through the generated record
--   ("Hsed.Core.Method".@fromCall@, 5.2.3.2, PDF 150): a session open where
--   the status is SUCCESS, kept in the run's context; the status where it
--   is not (5.3.4.1.5, PDF 215: SyncSession with the identifying parameters
--   and the error); no SyncSession where the TPer answered CloseSession (an
--   abort, 3.3.7.1.5, PDF 75), nothing, or something else.
--   With @--interrupt-in-session@, a session opened on the drive's end (or
--   the run's one end) stops the run ('reStop', why said), the session left
--   open on purpose, for @--recover@ to clear (design 11, M4c).
--
--   A StartSession issued whose SyncSession the host could not read (an
--   exchange that failed past its IF-SEND, an answer whose MinTransfer the
--   transport cannot carry among them, or an answer with no SyncSession but
--   the TPer's own word that none opened: its CloseSession, Core 3.3.7.1.5,
--   PDF 75) may have left a session open, which the TPer holds once it
--   returns SyncSession (Core 3.3.4.5, PDF 62): on an end that takes
--   STACK_RESET ('admitsStackReset') one is sent on the ComID and noted,
--   which aborts it (3.3.4.7.5 item 1, PDF 65); where the end takes none,
--   or the reset does not answer Success, the run's context keeps it
--   ('rcSessionUnread'), and the next case's NoSessions and the reports
--   find it.
startSession :: StartArgs -> Script (Either ExchangeError SyncAnswer)
startSession args = do
  c <- baseComId
  bytes <- framed "the StartSession ComPacket" (controlComPacket c (generate (startSessionCall args)))
  exchange <- exchangeOn c bytes
  let answer = syncAnswerOf c <$> exchange
  case answer of
    Right (Synced h) -> do
      modifyCtx (\x -> x { rcSession = Just h })
      here <- interruptsHere
      when here $ asks reStop >>= \ref -> atomicModifyIORef' ref (\was -> (was <|> Just (interruptedIn h), ()))
    _ -> for_ (unreadSession exchange answer) (uncurry (sessionUnread c))
  pure answer

-- | SMUID.StartSession as 'startSession' sends it (Core 5.2.3.1, PDF 149):
--   HSN 1 (Test Cases 4.1 h, PDF 26), the SP, Write as asked (True in every
--   case, 4.1 e), the HostChallenge and HostSigningAuthority where given;
--   the call alone, for a case that sends it in a payload of its own
--   (since M5b: ETC-07's, an EndList before its Call, Test Cases PDF 80).
startSessionCall :: StartArgs -> MethodCall
startSessionCall (StartArgs sp authority challenge write) = invocation uSMUID Methods.StartSession
  { Methods.startSessionHostSessionID         = value (fromIntegral hostSessionNumber :: Natural)
  , Methods.startSessionSPID                  = value (token sp)
  , Methods.startSessionWrite                 = value (if write then 1 else 0 :: Natural)
  , Methods.startSessionHostChallenge         = value <$> challenge
  , Methods.startSessionHostExchangeAuthority = Nothing
  , Methods.startSessionHostExchangeCert      = Nothing
  , Methods.startSessionHostSigningAuthority  = value . token <$> authority
  , Methods.startSessionHostSigningCert       = Nothing
  , Methods.startSessionSessionTimeout        = Nothing
  , Methods.startSessionTransTimeout          = Nothing
  , Methods.startSessionInitialCredit         = Nothing
  , Methods.startSessionSignedHash            = Nothing
  }

-- | Why a StartSession's exchange may have left a session the host does not
--   know of, and whether a command of it failed in the transport or went
--   unanswered: its IF-SEND issued (any exchange error but 'Refused', which
--   comes before it: an answer the transport cannot carry the rest of,
--   'Uncollected', among them) and no SyncSession read back, the TPer not
--   having said that it opened none (a SyncSession of an error status, its
--   CloseSession); or none.
unreadSession :: Either ExchangeError [PacketFrame] -> Either ExchangeError SyncAnswer -> Maybe (Text, Bool)
unreadSession exchange answer = case (exchange, answer) of
  (_, Right (Synced _))        -> Nothing
  (_, Right (SyncFailed _))    -> Nothing
  (Right packets, Right (NoSync why))
    | aborted packets          -> Nothing
    | otherwise                -> Just (why, False)
  (Left (Refused _), _)        -> Nothing
  (Left e, _)                  -> Just (describeExchange e, gone e)
  _                            -> Nothing
  where
    aborted = either (const False) (any closeSessionReturned) . returnedOf
    gone = \case
      IfError (TransportFailure _) -> True
      TimedOut                     -> True
      _                            -> False

-- | A session StartSession may have opened on a ComID, unread, and whether
--   a command of its exchange failed in the transport or went unanswered:
--   kept in the run's context first; then, on an end that takes STACK_RESET,
--   where on a drive the transport neither failed nor went unanswered (the
--   kext transport latches on a failure, and the run on a host that gave
--   up: neither issues anything more), STACK_RESET on the ComID, noted,
--   which clears it where it answers Success/Failure 0.
sessionUnread :: Word16 -> Text -> Bool -> Script ()
sessionUnread c why gone = do
  modifyCtx (\x -> x { rcSessionUnread = Just (c, why) })
  admits <- admitsStackReset
  failed <- theEnd <&> \case
    DriveEnd _ -> gone
    SimEnd _   -> False
  when (admits && not failed) $ do
    outcome <- stackReset c
    when (outcome == Right 0) (modifyCtx (\x -> x { rcSession = Nothing, rcSessionUnread = Nothing }))
    note ("StartSession on " <> hex c <> ": no SyncSession read (" <> why <> "), so a session it may have opened is not known: "
          <> "STACK_RESET on " <> hex c <> ": " <> either id (\n -> "Success/Failure " <> tshow n) outcome)

-- | The HSN the jig opens every session with: 0x00000001 (Test Cases 4.1
--   h, PDF 26; SPF-07, the one test of other values, is not the jig's).
hostSessionNumber :: Word32
hostSessionNumber = 1

-- | Whether @--interrupt-in-session@ stops the run on this end: on a drive's
--   end, or on the run's one end (the simulator alone, for a run without a
--   drive); never on a pair's simulator.
interruptsHere :: Script Bool
interruptsHere = do
  opts <- asks reOpts
  onDrive <- theEnd <&> \case
    DriveEnd _ -> True
    SimEnd _   -> False
  pure (roInterruptInSession opts && (onDrive || not (paired (roTarget opts))))
  where
    paired = \case
      TPair _ _ -> True
      _         -> False

-- | Why @--interrupt-in-session@ stopped the run, the session it left open
--   named.
interruptedIn :: SessionHandle -> Text
interruptedIn h = "--interrupt-in-session: stopped after StartSession opened TSN " <> tshow (shTSN h) <> " on " <> hex (shComId h)
                  <> "; the session is left open on purpose"

-- | What a StartSession's answer on a ComID holds.
syncAnswerOf :: Word16 -> [PacketFrame] -> SyncAnswer
syncAnswerOf c packets = case returnedOf packets of
  Left why    -> NoSync why
  Right items -> case [ (mc, r) | ReturnedCall mc <- items, Just r <- [fromCall @Methods.SyncSession mc] ] of
    (mc, Right s) : _
      | _statusCode mc /= statusValue SUCCESS -> SyncFailed (StatusCode (_statusCode mc))
      | otherwise -> case (uintegerOf (Methods.syncSessionHostSessionID s), uintegerOf (Methods.syncSessionSPSessionID s)) of
          (Just hsn, Just tsn) -> Synced (SessionHandle c (fromIntegral tsn) (fromIntegral hsn))
          _                    -> NoSync "SyncSession's HostSessionID or SPSessionID is no uinteger (Core 5.2.3.2, PDF 150)"
    (_, Left e) : _ -> NoSync ("SyncSession's parameters do not read: " <> paramErrorText e)
    []
      | any closeSessionReturned items -> NoSync "the TPer answered CloseSession: the session startup was aborted (Core 3.3.7.1.5, PDF 75)"
      | null packets                   -> NoSync noResponse
      | otherwise                      -> NoSync "no SyncSession in the answer"

-- | Get on an object in a session, the cells a cell_block asks
--   ("Hsed.Core.CellBlock", Core 5.3.3.6, PDF 201), in one packet of the
--   session (Test Cases Table 6, PDF 27): its results and status (3.2.4.2,
--   PDF 49); a session the TPer aborted instead (its CloseSession answered,
--   or nothing) is no outcome.
getCells :: SessionHandle -> UID -> CellBlock -> Script (Either ExchangeError MethodOutcome)
getCells h object cb = do
  bytes <- framed "the Get ComPacket" (sessionComPacket h (generate (invocation object (Methods.Get (cellBlockValue cb)))))
  (>>= outcomeOf) <$> exchangeOn (shComId h) bytes

-- | A method's response among a session's answer.
outcomeOf :: [PacketFrame] -> Either ExchangeError MethodOutcome
outcomeOf packets = do
  items <- first Malformed (returnedOf packets)
  case [ r | ReturnedResults r <- items ] of
    MethodResponse rs st : _ -> Right (MethodOutcome rs (StatusCode st))
    []
      | any closeSessionReturned items -> Left (Malformed "the session was aborted: the TPer answered CloseSession (Core 3.3.7.1.5, PDF 75)")
      | null packets                   -> Left (Malformed noResponse)
      | otherwise                      -> Left (Malformed "no method response in the answer")

-- | The host's End of Session on a session (CLOSE_SESSION, Test Cases PDF
--   21), a bare End of Session token in one packet of the session: whether
--   the TPer answered with its own (Core 3.2.2.3.3.3, PDF 37; 3.3.7.1.5,
--   PDF 75), the session then gone from the run's context.
endSession :: SessionHandle -> Script (Either ExchangeError Bool)
endSession h = do
  bytes <- framed "the End of Session ComPacket" (sessionComPacket h (generate EndOfSession))
  closed <- fmap (either (const False) (elem ReturnedEndOfSession) . returnedOf) <$> exchangeOn (shComId h) bytes
  when (closed == Right True) (modifyCtx (\x -> x { rcSession = Nothing }))
  pure closed

-- | A ComPacket of one packet of a session, one data subpacket of these
--   tokens (Test Cases Table 6, PDF 27): the packet's Session the session's
--   TSN then HSN (Core 3.3.7.1.2, PDF 73), every other header field zero.
sessionComPacket :: SessionHandle -> ByteString -> Either LayoutError ByteString
sessionComPacket h tokens = encodeComPacket $
  ComPacketFrame (ComPacketHeader (shComId h) staticExtension 0 0)
                 [PacketFrame (PacketHeader (shTSN h) (shHSN h) 0 0 0) [DataSubPacket (DataSubPacketHeader dataKind) tokens]]

-- | A ComPacket of one control session packet of these data subpackets,
--   one for each payload (ETC-15's MaxSubpackets + 1, Test Cases PDF 88),
--   the headers otherwise as Table 6 has them.
controlSubpackets :: Word16 -> [ByteString] -> Either LayoutError ByteString
controlSubpackets c payloads = encodeComPacket $
  ComPacketFrame (ComPacketHeader c staticExtension 0 0)
                 [PacketFrame (PacketHeader 0 0 0 0 0) [ DataSubPacket (DataSubPacketHeader dataKind) p | p <- payloads ]]

-- | Expect an exchange to have returned no packets, the TPer's (0, 0, 0)
--   (ETC-04, ETC-08, ETC-15: Core Table 45's "All Response(s) returned - no
--   further data", PDF 83), the expectation named.
expectNoResponse :: Text -> Either ExchangeError [PacketFrame] -> Script ()
expectNoResponse name = \case
  Right []      -> pure ()
  Right packets -> unmet (name <> ": the TPer answered " <> tshow (length packets) <> " packet(s) where none was expected, "
                          <> "Table 45's (0, 0, 0)")
  Left e        -> unmet (name <> ": " <> describeExchange e)

-- | Expect a method's status, by Core Table 166's name ('statusName'), the
--   expectation named.
expectMethodStatus :: Text -> StatusCode -> MethodOutcome -> Script ()
expectMethodStatus name expected outcome = unless (moStatus outcome == expected) $
  unmet (name <> ": the method answered " <> statusName (moStatus outcome) <> ", not " <> statusName expected)

-- | Expect SyncSession's status, by name: SUCCESS a session open, another
--   status the startup failed with it; and where an error status is
--   expected, no SyncSession at all (the TPer aborted the startup, with or
--   without its CloseSession) is accepted and noted (Test Cases 7.1, PDF
--   76: session abort is an acceptable response to every error case).
--   On a drive, SP_BUSY or NO_SESSIONS_AVAILABLE where SUCCESS is expected
--   says why, and what clears it: a session an earlier run left open to the
--   Admin SP (Core 5.1.5.3, PDF 138), which @--recover@'s STACK_RESET
--   aborts (3.3.4.7.5 item 1, PDF 65).
expectSyncStatus :: Text -> StatusCode -> SyncAnswer -> Script ()
expectSyncStatus name expected = \case
  Synced _
    | expected == SUCCESS -> pure ()
    | otherwise           -> unmet (name <> ": SyncSession answered " <> statusName SUCCESS <> ", a session open, not " <> statusName expected)
  SyncFailed st
    | st == expected -> pure ()
    | otherwise      -> do
        onDrive <- asks reTarget <&> \case
          DriveTarget _ -> True
          _             -> False
        unmet (name <> ": SyncSession answered " <> statusName st <> ", not " <> statusName expected
               <> (if onDrive && expected == SUCCESS && st `elem` [SP_BUSY, NO_SESSIONS_AVAILABLE] then "; " <> sessionLeftOpen else ""))
  NoSync why
    | expected /= SUCCESS -> note (name <> ": no SyncSession, " <> why <> ": accepted as a session abort, which Test Cases 7.1 (PDF 76) "
                                   <> "allows where " <> statusName expected <> " is expected")
    | otherwise           -> unmet (name <> ": no SyncSession: " <> why)

-- | Why a drive answers a StartSession SP_BUSY or NO_SESSIONS_AVAILABLE
--   where no session of this run is open, and what clears it.
sessionLeftOpen :: Text
sessionLeftOpen = "a read-write session to the Admin SP was open before this one (Core 5.1.5.3, PDF 138): hsed-jig --recover clears it "
                  <> "(Core 3.3.4.7.5 item 1, PDF 65)"

-- | STACK_RESET on a ComID (Core 3.3.4.7.5, PDF 65-66): HANDLE_COMID_REQUEST,
--   an IF-SEND of protocol 2 carrying Table 35's request for the ComID's
--   Extended ComID (the ComID and a static ComID's extension), then
--   GET_COMID_RESPONSE, an IF-RECV of protocol 2 on the ComID, its answer
--   Table 36's response (its Success/Failure, 0 the stack reset), Table 37's
--   Pending (asked again, as many times as the host's timeout holds polls),
--   or Table 31's No Response Available, each told by its constants
--   ("Hsed.Core.Record".'decodeRecordStrict'); or why none came. On a drive
--   the Guard decides both commands (Level0Only refuses them; an allow list
--   takes them with 'AllowStackReset', on its base ComID alone), and the
--   base must be bound first ('baseComId'): this verb binds nothing. Once
--   its IF-SEND is out, its polls run to their end, a stop asked for or not
--   ('stopIfAsked'): as many as the host's timeout holds (the run's
--   @roExchange@), Pending each time. A reset that answers Success/Failure
--   0 forgets the session the run holds on the ComID, and a StartSession
--   unread there, since every session on it is aborted (3.3.4.7.5 item 1,
--   PDF 65; 'forgetSessionsOn'; since M5b), with no note of its own: its
--   callers note the reset as their step.
stackReset :: Word16 -> Script (Either Text Word32)
stackReset c = do
  stopIfAsked
  u <- endUnit
  x <- asks (roExchange . reOpts)
  case encodeRecord (STACK_RESETCommandRequest (extendedComId c)) of
    Left e        -> pure (Left ("the STACK_RESET request does not encode: " <> tshow e))
    Right request -> do
      sent <- sendOn comIdManagementProtocol c (roundUp u (fromIntegral (B.length request))) request
      if status sent /= Good
        then pure (Left ("HANDLE_COMID_REQUEST of STACK_RESET on " <> hex c <> " answered " <> describeStatus (status sent)))
        else retrieve x (0 :: Int) (roundUp u (fromIntegral (layoutSize (recordLayout (Proxy :: Proxy STACK_RESETCommandResponse)))))
  where
    retrieve x n len = recvOn comIdManagementProtocol c len >>= \r -> case stackResetAnswer c r of
      Right outcome -> Right outcome <$ when (outcome == 0) (modifyCtx (forgetSessionsOn c))
      Left (False, why) -> pure (Left why)
      Left (True, why)
        | n < pollsOf x -> threadDelay (xPollMicros x) >> retrieve x (n + 1) len
        | otherwise     -> pure (Left (why <> " after " <> tshow (pollsOf x) <> " polls"))

-- | A GET_COMID_RESPONSE answer after STACK_RESET on a ComID: Table 36's
--   Success/Failure; or why not, and whether to ask again (Pending).
stackResetAnswer :: Word16 -> Response -> Either (Bool, Text) Word32
stackResetAnswer c r
  | status r /= Good = Left (False, "GET_COMID_RESPONSE on " <> hex c <> " answered " <> describeStatus (status r))
  | Right (DecodedRecord resp _ _) <- decodeRecordStrict (rpayload r) :: Either LayoutError (DecodedRecord STACK_RESETCommandResponse) =
      if stack_RESETCommandResponseExtendedComID resp == extendedComId c
        then Right (stack_RESETCommandResponseSuccessFailure resp)
        else Left (False, "a STACK_RESET response for Extended ComID " <> hex (stack_RESETCommandResponseExtendedComID resp)
                          <> ", not " <> hex (extendedComId c) <> " (Core Table 36, PDF 66)")
  | Right (DecodedRecord (STACK_RESETPending _) _ _) <- decodeRecordStrict (rpayload r) :: Either LayoutError (DecodedRecord STACK_RESETPending) =
      Left (True, "STACK_RESET on " <> hex c <> " still pending (Core Table 37, PDF 66)")
  | Right (DecodedRecord (NoResponseAvailable _) _ _) <- decodeRecordStrict (rpayload r) :: Either LayoutError (DecodedRecord NoResponseAvailable) =
      Left (False, "No Response Available to GET_COMID_RESPONSE on " <> hex c <> " (Core Table 31, PDF 63): the TPer holds no STACK_RESET response")
  | otherwise = Left (False, "GET_COMID_RESPONSE on " <> hex c <> " answered neither a STACK_RESET response, nor Pending, nor No Response Available "
                             <> "(Core Tables 31, 36 and 37)")

-- | The Opal SSC V2 descriptor of a Level 0 response, decoded (Opal 2.30
--   Table 6, PDF 23), if it has one that decodes.
opalV2Of :: Level0 -> Maybe Level0DiscoveryOpalSSCV2Feature
opalV2Of l0 = case featureIn l0 of
  Just (Right d) -> Just (drRecord d)
  _              -> Nothing

-- | A session opened ('startSession', a startup that fails or goes
--   unanswered stopping the script), the steps run in it, the session ended
--   on the way out ('endSession'); a step that failed, or an End of Session
--   the TPer did not answer, is followed by STACK_RESET on the session's
--   ComID where the end takes it ('closingStep', noted), so that the next
--   case finds no session (Core 3.3.4.7.5, PDF 65: every session on the
--   ComID aborted).
withSession :: StartArgs -> (SessionHandle -> Script Verdict) -> Script Verdict
withSession args body = startSession args >>= exchanged "StartSession" >>= opened "StartSession" >>= \h ->
  attempt (body h) >>= closingStep "End of Session" h

-- | A case's End of Session after its steps in a session, the step named
--   (UCT-03's 1c, ETC-04's step 3): the steps' verdict, with a failure
--   added where the TPer did not answer the End of Session with its own
--   (Core 3.2.2.3.3.3, PDF 37) or the exchange gave nothing; after a step
--   that failed or an End of Session unanswered, on an end that takes
--   STACK_RESET ('admitsStackReset': the simulator, a drive whose run admits
--   it), STACK_RESET on the session's ComID (noted), so that the next case
--   finds no session (Core 3.3.4.7.5 item 1, PDF 65: every session on the
--   ComID aborted); on a drive whose run admits none, the session is left,
--   for the next case's NoSessions to find. A session whose End of Session
--   went unanswered stays in the run's context ('rcSession') until a
--   STACK_RESET answers Success/Failure 0: one that answers otherwise, or
--   none sent, leaves it there for the next case's reset or NoSessions and,
--   after the run's last case, for the reports, which name it with
--   @--recover@'s hint (the M4c review's verification).
closingStep :: Text -> SessionHandle -> Verdict -> Script Verdict
closingStep name h v = do
  closed <- endSession h
  let unanswered = case closed of
        Right True  -> []
        Right False -> [name <> ": the TPer did not answer the End of Session with its own (Core 3.2.2.3.3.3, PDF 37)"]
        Left e      -> [name <> ": " <> describeExchange e]
  unless (v == Pass && null unanswered) recover
  pure $ case v of
    Pass
      | null unanswered -> Pass
      | otherwise       -> Fail unanswered
    Fail rs -> Fail (rs <> unanswered)
    other   -> other
  where
    recover = admitsStackReset >>= \admits -> when admits $ do
      outcome <- stackReset (shComId h)
      when (outcome == Right 0) (modifyCtx (\x -> x { rcSession = Nothing, rcSessionUnread = Nothing }))
      note (name <> ": after a failure, STACK_RESET on " <> hex (shComId h) <> ": "
            <> either id (\n -> "Success/Failure " <> tshow n) outcome)


-- * The verbs of M5b's cases

-- The verbs the Locking SP's cases are written in (design 11 M5b, 9.1):
-- a session as an authority with its PIN, Set, the transaction tokens, an
-- IF-SEND in a session with its IF-RECVs apart (SPF-02 case 2), an answer
-- that is nothing or the TPer's CloseSession (ETC-09, ETC-14), a cell of
-- the simulator's state, TPER_RESET and a power cycle (SPF-12 case 1).

-- | StartSession's arguments for an authority of an SP, named, read-write,
--   its HostChallenge the authority's current PIN, read from the
--   simulator's state ('peekSim' of "Hsed.Sim.View"'s 'CredentialOf'; Test
--   Cases 5.2 item 4 and 6.2 item 4, PDF 29 and 49) and kept among the
--   run's secrets (design 9.3 item 5); the simulator's alone, a drive's PIN
--   being unknown (design 9.3): on a drive, and where the SP holds no
--   credential for the authority, the script stops.
asAuthority :: Text -> UID -> UID -> Script StartArgs
asAuthority name sp authority = peekSim (CredentialOf sp authority) >>= \case
  Nothing -> unmet ("StartSession as " <> name <> ": the simulator's alone, its PIN read from the simulator's state (design 9.3)")
  Just held -> case held of
    Nothing -> unmet ("StartSession as " <> name <> ": the simulator's " <> spName sp <> " holds no credential for " <> name)
    Just cr -> do
      modifyCtx (\x -> x { rcSecrets = L.nub (rcSecrets x <> [crPIN cr]) })
      pure (StartArgs sp (Just authority) (Just (crPIN cr)) True)

-- | An SP's name in the jig's texts: the Admin SP, the Locking SP (Opal
--   2.30 Table 20, PDF 45), another by its UID.
spName :: UID -> Text
spName sp
  | sp == uSP_Admin   = "Admin SP"
  | sp == uSP_Locking = "Locking SP"
  | otherwise         = "SP " <> tshow sp

-- | Set on an object in a session (Core 5.3.3.7, PDF 201-203): ObjectUID.Set
--   with no Where, its Values a RowValues list of the column numbers and
--   values given (5.3.3.7.2.2, PDF 202-203), in one packet of the session
--   (Test Cases Table 6, PDF 27): its results, an empty list, and its status
--   (5.3.3.7.3, PDF 203); a session the TPer aborted instead is no outcome.
setCells :: SessionHandle -> UID -> [(Natural, Value)] -> Script (Either ExchangeError MethodOutcome)
setCells h object cells = do
  bytes <- framed "the Set ComPacket" (sessionComPacket h (generate (invocation object (Methods.Set Nothing (Just values)))))
  (>>= outcomeOf) <$> exchangeOn (shComId h) bytes
  where values = L (List [ N (col .: v) | (col, v) <- cells ])

-- | Start Transaction alone in a packet of a session, its status 0x00 (Core
--   3.2.2.3.3.4, PDF 37), the whole exchange: the status of the TPer's
--   mirror, 0x00 where the level opened; a session the TPer aborted (its
--   CloseSession, or nothing) is no status.
startTransaction :: SessionHandle -> Script (Either ExchangeError Natural)
startTransaction h = transactionToken h StartTransaction 0

-- | End Transaction alone in a packet of a session with a status, 0x00
--   committing the innermost level, 0x01 aborting it (Core 3.2.2.3.3.5 and
--   Table 16, PDF 37-38), the whole exchange: the status of the TPer's
--   mirror.
endTransaction :: SessionHandle -> Natural -> Script (Either ExchangeError Natural)
endTransaction h = transactionToken h EndTransaction

-- | A transaction control token with its status, alone in one packet of a
--   session (Core 3.2.2.3.3.4-5, PDF 37: the token, then its uinteger
--   status), the whole exchange: the status of the TPer's mirror of that
--   token ('returnedOf'); the TPer's CloseSession instead, an abort, or
--   nothing, is no status.
transactionToken :: SessionHandle -> Token -> Natural -> Script (Either ExchangeError Natural)
transactionToken h t st = do
  bytes <- framed ("the " <> named <> " ComPacket") (sessionComPacket h (generate t <> generate (Unsigned st)))
  (>>= mirrored) <$> exchangeOn (shComId h) bytes
  where
    named = if t == StartTransaction then "Start Transaction" else "End Transaction"
    mirrored packets = do
      items <- first Malformed (returnedOf packets)
      case [ n | ReturnedTransaction t' n <- items, t' == t ] of
        n : _ -> Right n
        []
          | any closeSessionReturned items -> Left (Malformed "the session was aborted: the TPer answered CloseSession (Core 3.3.7.1.5, PDF 75)")
          | null packets                   -> Left (Malformed noResponse)
          | otherwise                      -> Left (Malformed ("no " <> named <> " mirror in the answer (Core 3.2.2.3.3.4-5, PDF 37)"))

-- | A method call in a packet of a session, the IF-SEND alone ('rawIfSend',
--   at the least transfer length that carries it), its answer left for
--   IF-RECVs of the case's own (SPF-02 case 2's, Test Cases PDF 53).
sendInSession :: SessionHandle -> MethodCall -> Script Response
sendInSession h call = do
  bytes <- framed "the session ComPacket" (sessionComPacket h (generate call))
  len <- sendLength bytes
  rawIfSend (shComId h) len bytes

-- | IF-RECV of a number of bytes on a ComID, rounded up to the end's unit
--   ('transferFor'): SPF-02 case 2's insufficient transfer length, then its
--   MinTransfer (Test Cases PDF 53; Core Table 45, PDF 83).
ifRecvWith :: Word16 -> Word32 -> Script Response
ifRecvWith c n = stopIfAsked >> transferFor n >>= recvOn comPacketProtocol c

-- | The packets of a ComPacket of a ComID that an IF-RECV delivered ('t45On':
--   a ComPacket of that ComID and a static ComID's extension, the IF-RECV
--   answered Good), for 'returnedOf' to read, none for Table 45's (0, 0,
--   0); or why not.
framesOf :: Word16 -> Response -> Either Text [PacketFrame]
framesOf c r = do
  unless (status r == Good) $ Left ("the IF-RECV answered " <> describeStatus (status r))
  _ <- t45On c r
  case decodeComPacket (rpayload r) of
    Left e                                             -> Left ("no ComPacket in the response: " <> tshow e)
    Right (DecodedFrame (ComPacketFrame _ packets) _ _) -> Right packets

-- | Expect an exchange to have returned no packets, the TPer's (0, 0, 0),
--   or the TPer's CloseSession alone, a session abort with it (Test Cases
--   7.1, PDF 76; ETC-09 and ETC-14, PDF 82 and 87), the expectation named,
--   which of the two noted; anything else, a response of the TPer's that
--   is neither, an answer that does not read or an exchange that failed,
--   stops the script.
expectNoResponseOrCloseSession :: Text -> Either ExchangeError [PacketFrame] -> Script ()
expectNoResponseOrCloseSession name = \case
  Right []      -> note (name <> ": the TPer answered (0, 0, 0), all responses returned, no further data (Core Table 45, PDF 83)")
  Right packets -> case returnedOf packets of
    Right [item] | closeSessionReturned item ->
      note (name <> ": the TPer answered its CloseSession alone: a session abort, which Test Cases 7.1 (PDF 76) accepts")
    Right items -> unmet (name <> ": the TPer answered " <> tshow (length items) <> " item(s) in " <> tshow (length packets)
                          <> " packet(s) where none, Table 45's (0, 0, 0), or its CloseSession alone was expected (Test Cases 7.1, PDF 76)")
    Left why    -> unmet (name <> ": an answer that does not read: " <> why)
  Left e        -> unmet (name <> ": " <> describeExchange e)

-- | A session the TPer aborted forgotten by the run's context
--   ('rcSession', where it holds that session), noted with why, so that the
--   next case's NoSessions finds none of this run's open (Core 3.3.7.1.5,
--   PDF 75).
sessionGone :: SessionHandle -> Text -> Script ()
sessionGone h why = do
  modifyCtx (\x -> x { rcSession = mfilter (/= h) (rcSession x) })
  note ("session TSN " <> tshow (shTSN h) <> " on " <> hex (shComId h) <> " aborted by the TPer, forgotten by the run: " <> why
        <> " (Core 3.3.7.1.5, PDF 75)")

-- | After a step that failed with its session perhaps still open: STACK_RESET
--   on the session's ComID, which aborts every session on it (Core 3.3.4.7.5
--   item 1, PDF 65), the session forgotten where the reset answered
--   Success/Failure 0 ('sessionGone'), so that the next case finds none of
--   this run's open; otherwise noted, for the next case's NoSessions. Shared
--   by ETC-09 and ETC-14 (design 13.34 (22)(d)).
cleared :: SessionHandle -> Script ()
cleared h = stackReset (shComId h) >>= \case
  Right 0 -> sessionGone h ("STACK_RESET on " <> hex (shComId h) <> " after a step failed: every session on the ComID aborted (Core 3.3.4.7.5 item 1, PDF 65)")
  outcome -> note ("after a step failed, STACK_RESET on " <> hex (shComId h) <> ": " <> either id (("Success/Failure " <>) . tshow) outcome)

-- | A cell of the simulator's state, by the SP's, the table's and the
--   row's UIDs and the column's name, found in the description the SP holds
--   ("Hsed.Sim.State".'Hsed.Sim.State.cellNamedIn'); 'Nothing' on a drive,
--   or where the state holds no such SP, row or cell: "Hsed.Sim.View"'s
--   'CellOf' query ('peekSim'), where M5c moved the reader.
simCell :: UID -> UID -> UID -> Text -> Script (Maybe Cell)
simCell sp table row name = join <$> peekSim (CellOf sp table row name)

-- | TPER_RESET (Opal 2.30 3.2.3 and Table 13, PDF 28-29): an IF-SEND of
--   protocol 0x02 to ComID 0x0004 of one unit of zeros ('tperResetBytes': a
--   non-zero length, its data ignored), no IF-RECV answering it; the
--   simulator's alone, nothing sent to a drive (design 9.3). Answered Good,
--   the TPer has aborted every session and put the host properties back to
--   their initial assumptions (3.2.3 b and g), so the run forgets the
--   session it holds, a StartSession unread and the host's properties,
--   noted; answered otherwise (TPER_RESET disabled, Table 26, PDF 49), the
--   status is the case's to judge.
tperReset :: Script (Either Text IfStatus)
tperReset = theEnd >>= \case
  DriveEnd _ -> pure (Left "TPER_RESET: the simulator's alone, nothing is sent to a drive (design 9.3)")
  SimEnd _   -> do
    stopIfAsked
    len <- transferFor tperResetBytes
    r <- sendOn comIdManagementProtocol tperResetComId len (B.replicate (fromIntegral tperResetBytes) 0)
    when (status r == Good) $ do
      modifyCtx (\x -> (forgetSessionsOn' x) { rcHostProperties = Nothing })
      note ("TPER_RESET on " <> hex tperResetComId <> ": " <> ifStatusName (status r) <> ": every session aborted and the host properties back "
            <> "to their initial assumptions (Opal 2.30 3.2.3 b and g, PDF 28-29); the run's session and host properties forgotten")
    pure (Right (status r))

-- | The bytes TPER_RESET's IF-SEND carries, zeros: 512, one 512-byte unit
--   and a whole number of bytes, a non-zero length whatever the end's unit
--   (Opal 2.30 Table 13, PDF 29: the data is ignored).
tperResetBytes :: Word32
tperResetBytes = transferLength UnitBlocks512 1

-- | A power cycle of the simulator through its transport's event
--   ("Hsed.Wire".'inject' of 'PowerCycle', which takes a @'ReadWrite@
--   transport alone, so that a drive can be given none): every session
--   aborted and the resets' rules applied (Core 3.3.7.1.5, PDF 76; 5.2.2.4.4,
--   PDF 148; Opal 2.30 3.3.5.1, PDF 31), the host properties back to their
--   initial assumptions with them, so that the run forgets the session it
--   holds, a StartSession unread and the host's properties, noted (the
--   recording transport notes the event itself, "Hsed.Jig.Transcript"); on
--   a drive, nothing, and why.
powerCycle :: Script (Either Text IfStatus)
powerCycle = theEnd >>= \case
  DriveEnd _ -> pure (Left "a power cycle: the simulator's alone, an event of its transport; a drive is given none (design 9.3)")
  SimEnd tr  -> do
    stopIfAsked
    s <- liftIO (inject tr PowerCycle)
    when (s == Good) $ do
      modifyCtx (\x -> (forgetSessionsOn' x) { rcHostProperties = Nothing })
      note ("power cycle: " <> ifStatusName s <> ": every session aborted and the host properties back to their initial assumptions "
            <> "(Core 3.3.7.1.5, PDF 76; 5.2.2.4.4, PDF 148); the run's session and host properties forgotten")
    pure (Right s)

-- | What the simulator says happened to it since it was last asked
--   ("Hsed.Jig.Simulator".'simNotices': over the LAN leg, a hub that
--   restarted, broadcasts missed, a reset by another client; in process,
--   nothing), each noted, and for each the run's session, a StartSession
--   unread and the host's properties forgotten as a power cycle forgets them
--   ('powerCycle'; Core 3.3.7.1.5, PDF 76): the TPer behind the notice may
--   be another, at its Original Factory State, or one reset (design 6.3,
--   11 M5c). Nothing is issued; on a drive, nothing. What the runner does
--   before each case and before the closing Level 0 ("Hsed.Jig.Run"), so
--   that the next case runs on what the TPer now holds.
heedNotices :: Script ()
heedNotices = asks reTarget >>= \case
  SimTarget _ sim -> liftIO (simNotices sim) >>= traverse_ (\notice -> do
    modifyCtx (\x -> (forgetSessionsOn' x) { rcHostProperties = Nothing })
    note ("the simulator " <> openedAs (simOpened sim) <> ": " <> notice <> ": the run's session, unread StartSession and host properties "
          <> "forgotten as a power cycle forgets them (Core 3.3.7.1.5, PDF 76)"))
  _               -> pure ()
  where
    openedAs = \case
      InProcess -> "in process"
      OverWs u  -> "over " <> renderWsUrl u

-- | The run's context without the session it holds on a ComID and the
--   StartSession unread there, once a STACK_RESET on that ComID answered
--   Success/Failure 0 (Core 3.3.4.7.5 item 1, PDF 65: every session on it
--   aborted); a session on another ComID is kept.
forgetSessionsOn :: Word16 -> RunCtx -> RunCtx
forgetSessionsOn c x = x { rcSession = mfilter ((/= c) . shComId) (rcSession x), rcSessionUnread = mfilter ((/= c) . fst) (rcSessionUnread x) }

-- | The run's context without any session it holds and any StartSession
--   unread: a reset of the whole TPer (TPER_RESET, a power cycle).
forgetSessionsOn' :: RunCtx -> RunCtx
forgetSessionsOn' x = x { rcSession = Nothing, rcSessionUnread = Nothing }


-- * What the verbs build on

-- | The base ComID: the one recorded (UCT-01's), else the Opal SSC V2
--   descriptor's of the Level 0 response, discovered if need be (Test Cases
--   4.1 g and Table 5, PDF 26: a static ComID the descriptor reports). On a
--   drive where the run admits more than Level 0 Discovery
--   ("Hsed.Jig.Guard".'Hsed.Jig.Guard.driveAllowed' not empty), the base is
--   bound ("Hsed.Jig.Guard".'Hsed.Jig.Guard.driveBind': once, from the
--   drive's own Level 0), so that the Guard admits the allowed list's
--   commands on it; a bind refused stops the script, saying why.
baseComId :: Script Word16
baseComId = do
  ctx <- readCtx
  c <- case rcBaseComId ctx of
    Just c  -> pure c
    Nothing -> do
      l0 <- maybe discover pure (rcLevel0 ctx)
      case featureIn l0 of
        Just (Right d) -> let c = level0DiscoveryOpalSSCV2FeatureBaseComID (drRecord d)
                          in c <$ modifyCtx (\x -> x { rcBaseComId = Just c })
        Just (Left e)  -> unmet ("the Opal SSC V2 Feature descriptor does not decode: " <> tshow e)
        Nothing        -> unmet "no Opal SSC V2 Feature descriptor in Level 0 Discovery: no base ComID to use"
  theEnd >>= \case
    DriveEnd g | not (null (driveAllowed g)) -> liftIO (driveBind g c) >>= either (unmet . ("the drive's " <>)) pure
    _                                        -> pure ()
  pure c

-- | The descriptor of a feature's code in a response, decoded leniently (a
--   pin read otherwise is a difference, not an error).
featureIn :: forall f. Feature f => Level0 -> Maybe (Either LayoutError (DecodedRecord f))
featureIn l0 = decodeFeature . dBytes <$> L.find ((== featureCode (Proxy :: Proxy f)) . toInteger . dCode) (l0Descriptors l0)

-- | The least transfer length in the end's unit that carries a number of
--   bytes.
transferFor :: Word32 -> Script Word32
transferFor n = endUnit <&> (`roundUp` n)

-- | The least transfer length that carries bytes, in the end's unit (Test
--   Cases Table 5, PDF 26).
sendLength :: ByteString -> Script Word32
sendLength = transferFor . fromIntegral . B.length

-- | A ComPacket on a ComID carrying tokens as the Test Cases have the host
--   send them (Table 6, PDF 27): one packet of the control session (its
--   Session all zeros), one Data subpacket, every other header field zero.
controlComPacket :: Word16 -> ByteString -> Either LayoutError ByteString
controlComPacket c tokens = encodeComPacket $
  ComPacketFrame (ComPacketHeader c staticExtension 0 0) [PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader dataKind) tokens]]

-- | SMUID.Properties with no HostProperties (Core 5.2.2.1), as tokens.
propertiesCall :: ByteString
propertiesCall = generate (invocation uSMUID (Methods.Properties Nothing))

-- | The host's whole exchange of a ComPacket on a ComID
--   ("Hsed.Wire".'exchangeComPacket'), unless the run was asked to stop
--   ('stopIfAsked'): the IF-SEND through the end's path (the simulator's
--   transport, or the drive's through the Guard, under its policy), then
--   IF-RECV at 'recvLength' until the TPer has returned everything; the
--   packets of the answer, none for Table 45's (0, 0, 0). The host polls and
--   waits as the run's @roExchange@ says, and on a drive gives each command
--   @roDriveTimeoutMicros@ ('driveTimeoutMicros' unless a test shortens it).
--
--   On a drive an exchange the host gave up on ('TimedOut') latches the
--   drive for the rest of the run ("Hsed.Jig.Guard".'driveLatch'): every
--   command of it was given under its recording's mask
--   ("Hsed.Jig.Transcript".'Hsed.Jig.Transcript.recording'), so the
--   transport judged and kept the answer it lost, and what the drive did
--   with the command, the exchange left where it was, is no longer known;
--   nothing more reaches the drive, End of Session and STACK_RESET
--   included.
exchangeOn :: Word16 -> ByteString -> Script (Either ExchangeError [PacketFrame])
exchangeOn c bytes = do
  stopIfAsked
  len <- recvLength
  env <- ask
  let opts = (roExchange (reOpts env)) { xRecvLen = len }
      waited = roDriveTimeoutMicros (reOpts env)
  theEnd >>= \case
    SimEnd tr  -> liftIO (exchangeComPacket opts (\q -> Right <$> ifSend tr (proto q) (comId q) (lenBytes q) (payload q))
                                            (reChannel env) tr c bytes)
    DriveEnd g -> liftIO $ do
      answer <- exchangeComPacket opts { xTimeoutMicros = waited } (driveSend g) (reChannel env) (driveTransport g) c bytes
      case answer of
        Left TimedOut -> driveLatch g ("the host gave up on an exchange on " <> hex c <> " (no answer within "
                                       <> tshow (waited `div` 1000) <> " ms a command, or the exchange's own bound): what the drive "
                                       <> "did with its last command is not known, so nothing more is issued on this drive in this run")
        _             -> pure ()
      pure answer

-- | How long the host's exchange waits for a drive's command, unless a test
--   shortens it ("Hsed.Jig.Options".@roDriveTimeoutMicros@): 15 s, past the
--   kext's own 10 s on its SCSI task (@SED_TIMEOUT@, macos-driver
--   UserKernelShared.h:71, which TPerDriver.cpp:318 gives each command), so
--   that the host's timeout is unlikely to end a wait. It never cuts a
--   command short: the run gives each command under its recording's mask,
--   and the transport judges and keeps the answer whenever it comes; a host
--   that gives up all the same latches the drive ('exchangeOn').
driveTimeoutMicros :: Int
driveTimeoutMicros = roDriveTimeoutMicros defaultRunOptions

-- | A mark in the run's transcript: the number of its newest entry, so that
--   'maskAnswersSince' takes what was issued after it.
transcriptMark :: Script Int
transcriptMark = asks reTranscript >>= liftIO . entries <&> maybe 0 numberOf . L.lastMaybe

-- | The answers this end's IF-RECVs brought since a mark ('transcriptMark')
--   made secrets of the run, why named: each answer's bytes up to its zero
--   padding, so that an answer a step could not read, which may carry a
--   secret where the step did not find it (UCT-03's MSID, Opal 2.30 Table
--   25, PDF 48), is printed masked in every report (design 9.3 item 5);
--   noted, with how many.
maskAnswersSince :: Int -> Text -> Script ()
maskAnswersSince mark why = do
  role <- theEnd <&> \case
    SimEnd tr  -> dRole (describeTransport tr)
    DriveEnd g -> dRole (describeTransport (driveTransport g))
  es <- asks reTranscript >>= liftIO . entries
  let answers = [ bytes | Issued n end q r <- es, n > mark, end == role, dir q == IfRecv
                        , let bytes = fst (B.spanEnd (== 0) (rpayload r)), not (B.null bytes) ]
  modifyCtx (\x -> x { rcSecrets = L.nub (rcSecrets x <> answers) })
  note (why <> ": " <> tshow (length answers) <> " IF-RECV answer(s) on the " <> role <> " after entry " <> tshow mark
        <> " masked in the reports as secrets (design 9.3 item 5)")

-- | What a TPer's answer holds, read as the host reads it: the data
--   subpackets' tokens, in order, under the host's atom policy
--   ("Hsed.Core.Stream".'hostPolicy'), then the items 'returnedItems' gives
--   them: Session Manager responses, method responses, the TPer's End of
--   Session; or why the answer does not read.
returnedOf :: [PacketFrame] -> Either Text [Returned]
returnedOf packets = do
  as <- first (\(off, why) -> "the answer's tokens do not read at byte " <> tshow off <> ": " <> why) (atoms hostPolicy tokens)
  case returnedItems (map atomToken as) of
    (items, Nothing) -> Right items
    (_, Just f)      -> Left ("the answer does not read as a TPer's responses, at token " <> tshow (faultAt f) <> ": " <> faultWhy f)
  where
    tokens = mconcat [ dataSubPacketPayload sp | p <- packets, sp <- pfSubPackets p, dataSubPacketKind (dataSubPacketHeader sp) == dataKind ]

-- | Whether an item is the TPer's CloseSession, which it transmits when it
--   aborts a session (Core 5.2.3.5, PDF 153; 3.3.7.1.5, PDF 75).
closeSessionReturned :: Returned -> Bool
closeSessionReturned = \case
  ReturnedCall (MethodCall (MethodHeader _ m) _ _) -> m == uCloseSession
  _                                                -> False

-- | Why an answer holds nothing: the TPer returned Table 45's (0, 0, 0).
noResponse :: Text
noResponse = "no response: the TPer returned (0, 0, 0), all responses returned, no further data (Core Table 45, PDF 83)"

-- | A status by its name in Core Table 166 ("Hsed.Gen.Core.Codes", PDF
--   137), with its value: @NOT_AUTHORIZED (1)@; a value the table has no
--   row for, by its value alone.
statusName :: StatusCode -> Text
statusName s = case codeRow cStatusCodes v of
  Just (CodeRow _ (n : _)) -> n <> " (" <> tshow v <> ")"
  _                        -> "status " <> tshow v
  where v = toInteger (statusValue s)

-- | What an exchange gave, the step named; a script stopped with why it
--   gave nothing ('describeExchange').
exchanged :: Text -> Either ExchangeError a -> Script a
exchanged name = either (unmet . ((name <> ": ") <>) . describeExchange) pure

-- | A session opened: SyncSession's status SUCCESS expected
--   ('expectSyncStatus'), the step named; the session's handle.
opened :: Text -> SyncAnswer -> Script SessionHandle
opened name answer = do
  expectSyncStatus name SUCCESS answer
  case answer of
    Synced h -> pure h
    _        -> unmet (name <> ": SyncSession opened no session")

-- | StartSession to the Admin SP with no authority, so as Anybody (Core
--   5.3.4.1.2.1, PDF 211-212), read-write: UCT-03 step 1a's and ETC-04 step
--   1's (Test Cases PDF 32 and 78).
adminSPAsAnybody :: StartArgs
adminSPAsAnybody = StartArgs uSP_Admin Nothing Nothing True

-- | A ComPacket of one packet of a session of these data subpackets, one
--   for each payload (ETC-14's MaxSubpackets + 1, Test Cases PDF 87), the
--   packet's Session the session's TSN then HSN (Core 3.3.7.1.2, PDF 73),
--   the headers otherwise as Table 6 has them (PDF 27): 'controlSubpackets'
--   in a session.
sessionSubpackets :: SessionHandle -> [ByteString] -> Either LayoutError ByteString
sessionSubpackets h payloads = encodeComPacket $
  ComPacketFrame (ComPacketHeader (shComId h) staticExtension 0 0)
                 [PacketFrame (PacketHeader (shTSN h) (shHSN h) 0 0 0) [ DataSubPacket (DataSubPacketHeader dataKind) p | p <- payloads ]]

-- | A cell_block of a byte table's rows from one to another, a startRow of
--   a row number and an endRow, no Table and no column component (Core
--   5.1.4.2.3, PDF 129-130): SPF-02 case 2's DataStore rows 0 to 1023 (Test
--   Cases PDF 53).
byteRows :: Natural -> Natural -> CellBlock
byteRows s e = noCells { cbStartRow = Just (RowNumber s), cbEndRow = Just e }

-- | The Locking SP's DataStore byte table, by the UID of the table it is:
--   its Table table row's UID's lower half followed by 00 00 00 00 (Core
--   3.2.5.3 b.i, PDF 51-52; Opal 2.30 Table 36, PDF 54), 00 00 10 01 00 00
--   00 00.
dataStoreTable :: UID
dataStoreTable = uidLower LockingSP.uTable_DataStore +:+ hNull

-- | The Admin SP's TPerInfo object, the one row of Opal 2.30 Table 27 (PDF
--   50), from the generated preconfiguration, 00 00 02 01 00 03 00 01: not
--   Core Table 244's (PDF 305), on which the Admin SP's AccessControl table
--   grants nothing.
tperInfoObject :: Maybe UID
tperInfoObject = listToMaybe [ u | Pre r _ <- ptRows adminSPTPerInfoTablePreconfiguration, Just (Given (UIDCell u)) <- [L.lookup 0 (prCells r)] ]

-- | The number of a column found by name in a generated table description
--   ("Hsed.Gen.Core.TableDescs"), the script stopping where the description
--   lacks it.
namedColumn :: TableDesc -> Text -> Script Natural
namedColumn desc name =
  maybe (unmet ("the generated " <> tdName desc <> " table description has no column " <> name)) (pure . fromIntegral . colNumber)
        (columnNamed name desc)

-- | A query of the simulator's state, answered through its view
--   ("Hsed.Jig.Simulator".'simView'; "Hsed.Sim.View"'s 'SimQuery', each
--   answered by the reader the jig had at M5b, an authority's credential
--   among them, 'CredentialOf', with 'Credential' re-exported here): on the
--   simulator its answer, in process or over the LAN leg alike (design 9.2,
--   11 M5c, Option A); 'Nothing' on a drive. A view that does not answer
--   (a hub that refused the query, its connection lost; never in process)
--   stops the script: "the simulator's state: " and why.
peekSim :: SimQuery a -> Script (Maybe a)
peekSim q = asks reTarget >>= \case
  SimTarget _ sim -> liftIO (peekView (simView sim) q) >>= either (unmet . ("the simulator's state: " <>)) (pure . Just)
  _               -> pure Nothing

-- | A list of names with numbers, as Properties carries the host's and the
--   TPer's properties (Core 5.2.2.1, PDF 140).
namedNumbersValue :: [(Text, Natural)] -> Value
namedNumbersValue props = L (List [ N (encodeUtf8 name .: n) | (name, n) <- props ])

-- | The names with numbers a value lists, each a Named byte string with a
--   uinteger or an integer; one that is not is left out, as M3 read them;
--   'Nothing' for a value that is no list.
namedNumbers :: Value -> Maybe [(Text, Integer)]
namedNumbers = \case
  L (List vs) -> Just (mapMaybe property vs)
  _           -> Nothing
  where
    property = \case
      N (NamedValue (Name (Datum (Bytes n))) (D (Datum v))) -> (,) (decodeUtf8Lenient n) <$> numeric v
      _                                                    -> Nothing
    numeric = \case
      Unsigned n -> Just (toInteger n)
      Signed i   -> Just i
      _          -> Nothing

-- | Bytes that must encode: a ComPacket the jig sends, else the script
--   stops naming what did not.
framed :: Text -> Either LayoutError ByteString -> Script ByteString
framed what = either (unmet . ((what <> " does not encode: ") <>) . tshow) pure

-- | An IF-RECV response's ComPacket header as Table 45 has it: its Length
--   (its payload's), OutstandingData and MinTransfer.
t45Of :: Response -> Either Text (Integer, Integer, Integer)
t45Of = fmap snd . comPacketHeaderOf

-- | 't45Of' an answer on a static ComID: a ComPacket of that ComID and a
--   static ComID's extension, 0x0000 (Core 3.3.3.1, PDF 57), or why not.
t45On :: Word16 -> Response -> Either Text (Integer, Integer, Integer)
t45On c r = do
  (h, answer) <- comPacketHeaderOf r
  unless (comPacketComID h == c) $ Left ("a ComPacket of ComID " <> hex (comPacketComID h) <> ", not " <> hex c)
  unless (comPacketComIDExtension h == staticExtension) $
    Left ("a ComPacket of extension " <> hex (comPacketComIDExtension h) <> ", not a static ComID's " <> hex staticExtension)
  pure answer

-- | A response's ComPacket header, and its Length, OutstandingData and
--   MinTransfer.
comPacketHeaderOf :: Response -> Either Text (ComPacketHeader, (Integer, Integer, Integer))
comPacketHeaderOf r = case decodeRecord (rpayload r) of
  Left e -> Left ("no ComPacket in the response: " <> tshow e)
  Right (DecodedRecord (ComPacket h body) _ _) ->
    Right (h, (toInteger (B.length body), toInteger (comPacketOutstandingData h), toInteger (comPacketMinTransfer h)))

-- | The row of Table 45 an answer is: its fields each what the row prints,
--   or not zero where the row names a quantity.
t45RowOf :: (Integer, Integer, Integer) -> Maybe T45Row
t45RowOf answer = listToMaybe [ row | (row, sr) <- zip t45Rows (states table45), matches sr answer ]

matches :: StateRow -> (Integer, Integer, Integer) -> Bool
matches sr (l, o, m) = and (zipWith fits ["Length", "OutstandingData", "MinTransfer"] [l, o, m])
  where
    fits field v = case settingOf table45 field sr of
      Just (Constant k) -> v == toInteger k
      Just (Quantity _) -> v /= 0
      Nothing           -> False

showAnswer :: (Integer, Integer, Integer) -> Text
showAnswer (l, o, m) = "(" <> tshow l <> ", " <> tshow o <> ", " <> tshow m <> ")"

-- | A status, with what it carries.
describeStatus :: IfStatus -> Text
describeStatus s = ifStatusName s <> maybe "" (\d -> " (" <> d <> ")") (ifStatusDetail s)

-- | Why an exchange returned no packets, in words.
describeExchange :: ExchangeError -> Text
describeExchange = \case
  IfError s     -> "a command answered " <> describeStatus s
  Malformed t   -> "an answer that does not read: " <> t
  TimedOut      -> "no answer within the host's timeout"
  Refused t     -> "refused: " <> t
  Uncollected t -> "an answer not collected: " <> t

-- | A verdict in words: what it is, and why.
verdictText :: Verdict -> Text
verdictText v = case v of
  Pass           -> "pass"
  Fail rs        -> "fail: " <> T.intercalate "; " rs
  Skipped r      -> "skipped: " <> r
  Unobservable r -> "unobservable: " <> r

-- | Bytes in hex, two digits each, spaced: @a1 08 0e@.
spacedHex :: ByteString -> Text
spacedHex = T.unwords . map (T.justifyRight 2 '0' . T.pack . (`showHex` "")) . B.unpack
