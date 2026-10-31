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
and a session opened around a case's steps. A script runs in the run's
environment ('RunEnv': its target, transcript, channel and options) and
keeps what later cases need ('RunCtx': the Level 0 response, the base
ComID UCT-01 records, the properties Properties reported, the session a
case has open, the secrets it read, and what it observed for the report).

A script runs on one end of a run, the simulator or a drive (the runner
steps a pair's ends one after the other, "Hsed.Jig.Run"). On the simulator
the verbs use its transport as it is; on a drive every command goes through
the Guard under M3's policy ('drivePolicy', Level0Only), so that a refused
one is recorded and answered 'Unsupported', never issued.

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
    -- * Verdicts
  , Verdict (..)
  , attempt
  , unmet
  , note
  , observe
    -- * Verbs
  , discover
  , ifRecvOn
  , rawIfSend
  , properties
  , expectStatus
  , expectT45
  , drain
  , simOnly
    -- * Session verbs (group G3's)
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
    -- * What the verbs build on
  , drivePolicy
  , baseComId
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
  , namedColumn
  , peekSim
  , Credential (..)
  , simCredential
  , t45Of
  , t45On
  , t45RowOf
  , describeStatus
  , describeExchange
  , verdictText
  ) where

import qualified Data.Aeson             as A
import           Data.Bits              (shiftL, (.|.))
import           Numeric                (showHex)
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import           RIO
import qualified RIO.Text               as T

import           Hsed.Core.Call         (MethodCall (..), MethodHeader (..), MethodResponse (..))
import           Hsed.Core.CellBlock    (CellBlock, cellBlockValue)
import           Hsed.Core.Code         (CodeRow (..), codeRow)
import           Hsed.Core.Feature      (Feature (..), decodeFeature)
import           Hsed.Core.Layout       (Layout (..), LayoutError)
import           Hsed.Core.Method       (MethodSignature (..), SigResponse (..), Signature (..), fromCall, invocation,
                                         paramErrorText, positional)
import           Hsed.Core.Record       (DecodedRecord (..), LayoutRecord (..), decodeRecord, decodeRecordStrict, encodeRecord)
import           Hsed.Core.Stream       (Atom (..), Fault (..), Returned (..), atoms, hostPolicy, returnedItems)
import           Hsed.Core.StreamItem   (StreamItem (..))
import           Hsed.Core.Sync         (Setting (..), StateRow (..), StateTable (..), settingOf)
import           Hsed.Core.Table        (Cell (..), Col (..), TableDesc (..), columnNamed)
import           Hsed.Core.Token        (IsToken (..), Token (..))
import           Hsed.Core.UID          (UID)
import           Hsed.Core.Value        (Datum (..), IsValue (..), List (..), Name (..), NamedValue (..), Value (..), (.:))
import           Hsed.Gen.Core.Codes    (StatusCode (..), cStatusCodes, pattern SUCCESS)
import           Hsed.Gen.Core.Layouts  (ComPacket (..), ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..),
                                         NoResponseAvailable (..), PacketHeader (..), STACK_RESETCommandRequest (..),
                                         STACK_RESETCommandResponse (..), STACK_RESETPending (..))
import qualified Hsed.Gen.Core.Methods  as Methods
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Gen.Core.TableDescs (authorityTableDesc, c_PINTableDesc)
import           Hsed.Gen.Core.UIDs     (uCloseSession, uProperties, uSMUID, uSP_Admin)
import           Hsed.Gen.Opal.Features (Level0DiscoveryOpalSSCV2Feature (..))
import           Hsed.Jig.Guard         (Policy (..), guardedRecv, guardedSend)
import           Hsed.Jig.Options       (RunOptions (..))
import           Hsed.Jig.Target        (Target (..))
import           Hsed.Jig.Transcript    (TEntry (..), Transcript, record)
import           Hsed.Level0            (Descriptor (..), Level0 (..), Level0Problem (..), describeProblem, splitLevel0)
import           Hsed.Packet            (ComPacketFrame (..), PacketFrame (..), encodeComPacket)
import           Hsed.Sim.State         (cellOf, columnOf, rowWithUID)
import           Hsed.Sim.Sync          (T45Row (..), t45Rows)
import           Hsed.Sim.Types         (TPerState, maxComPacketSizeName)
import           Hsed.Transport.Cdb     (level0Cdb)
import           Hsed.Transport.Drive   (DriveInfo (..))
import           Hsed.Transport.InProc  (SimControl (..))
import           Hsed.Wire              (Cap (..), Channel, Dir (..), ExchangeError (..), ExchangeOpts (..), IfStatus (..),
                                         Request (..), Response (..), Transport, Unit (..), comIdManagementProtocol,
                                         comPacketProtocol, dataKind, defaultExchangeOpts, exchangeComPacket, hex, ifRecv,
                                         ifSend, ifStatusDetail, ifStatusName, level0ComId, mostCarried, roundUp,
                                         staticExtension, transferLength, unitOf)


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
  , rcObservations     :: ![(Text, Text)]           -- ^ what the case running has observed ('observe'), in order; the
                                                    --   runner takes them for the case's report and starts the next
                                                    --   case with none
  }
  deriving (Eq, Show)

-- | What a run starts knowing: nothing.
emptyRunCtx :: RunCtx
emptyRunCtx = RunCtx Nothing Nothing Nothing Nothing [] Nothing Nothing []

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
--   what it expects; and 'Fail' naming any other exception it threw.
attempt :: Script Verdict -> Script Verdict
attempt s = tryAny s <&> \case
  Right v -> v
  Left e  -> case fromException e of
    Just (Unmet t)  -> Fail [t]
    Just (Unseen t) -> Unobservable t
    Nothing         -> Fail ["exception: " <> T.pack (displayException e)]


-- * The end a script talks to

-- | One end of a run: the simulator, or a drive and what the IORegistry
--   says of it.
data End
  = SimEnd !(Transport 'ReadWrite)
  | DriveEnd !(Transport 'ReadOnly) !(Maybe DriveInfo)

theEnd :: Script End
theEnd = asks reTarget >>= \case
  SimTarget tr _    -> pure (SimEnd tr)
  DriveTarget tr di -> pure (DriveEnd tr di)
  PairTarget _ _    -> unmet "a script runs on one end of a pair at a time (Hsed.Jig.Run)"

-- | The unit of the end's transfer lengths.
endUnit :: Script Unit
endUnit = theEnd <&> \case
  SimEnd tr     -> unitOf tr
  DriveEnd tr _ -> unitOf tr

-- | The policy a drive's commands pass at M3: Level 0 Discovery alone
--   (design 9.3.2).
drivePolicy :: Policy
drivePolicy = Level0Only

-- | IF-RECV on the end: the simulator's transport, or the Guard's.
recvOn :: Word8 -> Word16 -> Word32 -> Script Response
recvOn p c len = theEnd >>= \case
  SimEnd tr     -> liftIO (ifRecv tr p c len)
  DriveEnd tr _ -> do
    t <- asks reTranscript
    liftIO (either blocked id <$> guardedRecv t drivePolicy tr (Request IfRecv p c len mempty))

-- | IF-SEND on the end, the payload zero-padded to the transfer length: the
--   simulator's transport, or the Guard's.
sendOn :: Word8 -> Word16 -> Word32 -> ByteString -> Script Response
sendOn p c len bytes = theEnd >>= \case
  SimEnd tr     -> liftIO (ifSend tr p c len bytes)
  DriveEnd tr _ -> do
    t <- asks reTranscript
    let padded = bytes <> B.replicate (fromIntegral len - B.length bytes) 0
    liftIO (either blocked id <$> guardedSend t drivePolicy tr (Request IfSend p c len padded))

-- | A command the Guard refused, as the transport's answer.
blocked :: Text -> Response
blocked why = Response (Unsupported ("blocked by the Guard: " <> why)) mempty

-- | Add a note to the run's transcript.
note :: Text -> Script ()
note t = asks reTranscript >>= \tr -> liftIO (record tr (Note 0 t))

-- | Add an observation to the case running, a name and a value, for its
--   report (design 9.1: a table a case reads, reported apart from the
--   verdict).
observe :: Text -> Text -> Script ()
observe name value' = modifyCtx (\c -> c { rcObservations = rcObservations c <> [(name, value')] })

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
    DriveEnd _ (Just di) -> note ("Level 0 Discovery, " <> tshow len <> " bytes: "
                                  <> either ("no CDB: " <>) (("the CDB, as the kext's shim builds it, " <>) . spacedHex)
                                            (level0Cdb (diInterface di) (len `div` blockBytes)))
    _                    -> pure ()
  r <- recvOn comPacketProtocol level0ComId len
  unless (status r == Good) (unmet ("Level 0 Discovery: the IF-RECV answered " <> describeStatus (status r)))
  either (unmet . ("Level 0 Discovery: the response does not split: " <>) . tshow) pure (splitLevel0 (rpayload r))

-- | The bytes of a 512-byte block, and the most a Level 0 IF-RECV asks for:
--   255 of them, the count byte of the kext's CDB, "Hsed.Wire".'mostCarried'
--   (design 6.2).
blockBytes, level0Most :: Word32
blockBytes = transferLength UnitBlocks512 1
level0Most = mostCarried UnitBlocks512

-- | IF-RECV of the run's transfer length on a ComID: the MaxComPacketSize
--   Properties reported, or the least an Opal TPer reports
--   ('defaultExchangeOpts'), in the end's unit.
ifRecvOn :: Word16 -> Script Response
ifRecvOn c = recvLength >>= recvOn comPacketProtocol c

recvLength :: Script Word32
recvLength = do
  u <- endUnit
  roundUp u . fromMaybe (xRecvLen defaultExchangeOpts) . rcMaxComPacketSize <$> readCtx

-- | IF-SEND of a payload on a ComID with a transfer length, past the run's
--   channel.
rawIfSend :: Word16 -> Word32 -> ByteString -> Script Response
rawIfSend = sendOn comPacketProtocol

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
  DriveEnd _ _ | TransportFailure t <- status r, expected /= Good ->
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
--   polls as the host's timeout holds ('defaultExchangeOpts').
drain :: Word16 -> Script ()
drain c = recvLength >>= go 0
  where
    go n len
      | n > polls = unmet ("drain " <> hex c <> ": still not idle after " <> tshow polls <> " IF-RECVs")
      | otherwise = do
          r <- recvOn comPacketProtocol c len
          unless (status r == Good) (unmet ("drain " <> hex c <> ": the IF-RECV answered " <> describeStatus (status r)))
          answer@(returned, outstanding, minTransfer) <- either (unmet . (("drain " <> hex c <> ": ") <>)) pure (t45On c r)
          u <- endUnit
          let longer = max len (roundUp u (fromInteger minTransfer))
          if | minTransfer /= 0, returned == 0, longer <= len ->
                 unmet ("drain " <> hex c <> ": answered " <> showAnswer answer <> ", a MinTransfer no larger than the request with "
                        <> "no packets: no row of Table 45")
             | minTransfer /= 0 -> go (n + 1) longer
             | outstanding == 0 -> pure ()
             | otherwise        -> threadDelay (xPollMicros defaultExchangeOpts) >> go (n + 1) len

-- | How many times the host asks again while the TPer is busy: as many as
--   its timeout holds polls ('defaultExchangeOpts').
polls :: Int
polls = xTimeoutMicros defaultExchangeOpts `div` max 1 (xPollMicros defaultExchangeOpts)

-- | Run a step on the simulator alone: 'Nothing' on a drive.
simOnly :: Script a -> Script (Maybe a)
simOnly s = asks reTarget >>= \case
  SimTarget _ _ -> Just <$> s
  _             -> pure Nothing


-- * Session verbs (group G3's)

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
--   MaxComPacketSize for the exchanges after it. A property that is no name
--   with a number is left out, as M3 read them.
propertiesWith :: Maybe [(Text, Natural)] -> Script (Either ExchangeError PropertiesAnswer)
propertiesWith host = do
  c <- baseComId
  bytes <- framed "the Properties ComPacket" (controlComPacket c (generate call))
  answer <- (>>= propertiesAnswerOf) <$> exchangeOn c bytes
  for_ answer $ \pa -> modifyCtx $ \x -> x
    { rcTPerProperties   = Just (paTPer pa)
    , rcHostProperties   = paHost pa
    , rcMaxComPacketSize = (fromInteger <$> L.lookup maxComPacketSizeName (paTPer pa)) <|> rcMaxComPacketSize x
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
startSession :: StartArgs -> Script (Either ExchangeError SyncAnswer)
startSession (StartArgs sp authority challenge write) = do
  c <- baseComId
  bytes <- framed "the StartSession ComPacket" (controlComPacket c (generate call))
  answer <- fmap (syncAnswerOf c) <$> exchangeOn c bytes
  case answer of
    Right (Synced h) -> modifyCtx (\x -> x { rcSession = Just h })
    _                -> pure ()
  pure answer
  where
    call = invocation uSMUID Methods.StartSession
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

-- | The HSN the jig opens every session with: 0x00000001 (Test Cases 4.1
--   h, PDF 26; SPF-07, the one test of other values, is not the jig's).
hostSessionNumber :: Word32
hostSessionNumber = 1

-- | What a StartSession's answer on a ComID holds.
syncAnswerOf :: Word16 -> [PacketFrame] -> SyncAnswer
syncAnswerOf c packets = case returnedOf packets of
  Left why    -> NoSync why
  Right items -> case [ (mc, r) | ReturnedCall mc <- items, Just r <- [fromCall @Methods.SyncSession mc] ] of
    (mc, Right s) : _
      | _statusCode mc /= statusValue SUCCESS -> SyncFailed (StatusCode (_statusCode mc))
      | otherwise -> case (uinteger (Methods.syncSessionHostSessionID s), uinteger (Methods.syncSessionSPSessionID s)) of
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
expectSyncStatus :: Text -> StatusCode -> SyncAnswer -> Script ()
expectSyncStatus name expected = \case
  Synced _
    | expected == SUCCESS -> pure ()
    | otherwise           -> unmet (name <> ": SyncSession answered " <> statusName SUCCESS <> ", a session open, not " <> statusName expected)
  SyncFailed st
    | st == expected -> pure ()
    | otherwise      -> unmet (name <> ": SyncSession answered " <> statusName st <> ", not " <> statusName expected)
  NoSync why
    | expected /= SUCCESS -> note (name <> ": no SyncSession, " <> why <> ": accepted as a session abort, which Test Cases 7.1 (PDF 76) "
                                   <> "allows where " <> statusName expected <> " is expected")
    | otherwise           -> unmet (name <> ": no SyncSession: " <> why)

-- | STACK_RESET on a ComID (Core 3.3.4.7.5, PDF 65-66): HANDLE_COMID_REQUEST,
--   an IF-SEND of protocol 2 carrying Table 35's request for the ComID's
--   Extended ComID (the ComID and a static ComID's extension), then
--   GET_COMID_RESPONSE, an IF-RECV of protocol 2 on the ComID, its answer
--   Table 36's response (its Success/Failure, 0 the stack reset), Table 37's
--   Pending (asked again, as many times as the host's timeout holds polls),
--   or Table 31's No Response Available, each told by its constants
--   ("Hsed.Core.Record".'decodeRecordStrict'); or why none came. On a drive
--   the Guard decides both commands (Level0Only refuses them until M4c's
--   @--recover@).
stackReset :: Word16 -> Script (Either Text Word32)
stackReset c = do
  u <- endUnit
  case encodeRecord (STACK_RESETCommandRequest (extendedComId c)) of
    Left e        -> pure (Left ("the STACK_RESET request does not encode: " <> tshow e))
    Right request -> do
      sent <- sendOn comIdManagementProtocol c (roundUp u (fromIntegral (B.length request))) request
      if status sent /= Good
        then pure (Left ("HANDLE_COMID_REQUEST of STACK_RESET on " <> hex c <> " answered " <> describeStatus (status sent)))
        else retrieve (0 :: Int) (roundUp u (fromIntegral (layoutSize (recordLayout (Proxy :: Proxy STACK_RESETCommandResponse)))))
  where
    retrieve n len = recvOn comIdManagementProtocol c len >>= \r -> case stackResetAnswer c r of
      Right outcome -> pure (Right outcome)
      Left (False, why) -> pure (Left why)
      Left (True, why)
        | n < polls -> threadDelay (xPollMicros defaultExchangeOpts) >> retrieve (n + 1) len
        | otherwise -> pure (Left (why <> " after " <> tshow polls <> " polls"))

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

-- | A static ComID's Extended ComID as the ComID management payloads carry
--   it (Core 3.3.4.7, PDF 62): the ComID in the upper two bytes, the
--   extension (a static ComID's 0x0000, 3.3.3.1, PDF 57) in the lower.
extendedComId :: Word16 -> Word32
extendedComId c = (fromIntegral c `shiftL` 16) .|. fromIntegral staticExtension

-- | The Opal SSC V2 descriptor of a Level 0 response, decoded (Opal 2.30
--   Table 6, PDF 23), if it has one that decodes.
opalV2Of :: Level0 -> Maybe Level0DiscoveryOpalSSCV2Feature
opalV2Of l0 = case featureIn l0 of
  Just (Right d) -> Just (drRecord d)
  _              -> Nothing

-- | A session opened ('startSession', a startup that fails or goes
--   unanswered stopping the script), the steps run in it, the session ended
--   on the way out ('endSession'); a step that failed, or an End of Session
--   the TPer did not answer, is followed on the simulator by STACK_RESET on
--   the session's ComID (noted), so that the next case finds no session
--   (Core 3.3.4.7.5, PDF 65: every session on the ComID aborted).
withSession :: StartArgs -> (SessionHandle -> Script Verdict) -> Script Verdict
withSession args body = startSession args >>= exchanged "StartSession" >>= opened "StartSession" >>= \h ->
  attempt (body h) >>= closingStep "End of Session" h

-- | A case's End of Session after its steps in a session, the step named
--   (UCT-03's 1c, ETC-04's step 3): the steps' verdict, with a failure
--   added where the TPer did not answer the End of Session with its own
--   (Core 3.2.2.3.3.3, PDF 37) or the exchange gave nothing; after a step
--   that failed or an End of Session unanswered, on the simulator,
--   STACK_RESET on the session's ComID (noted), so that the next case finds
--   no session (Core 3.3.4.7.5 item 1, PDF 65: every session on the ComID
--   aborted).
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
    recover = simOnly (stackReset (shComId h)) >>= \case
      Nothing      -> pure ()
      Just outcome -> do
        modifyCtx (\x -> x { rcSession = Nothing })
        note (name <> ": after a failure, STACK_RESET on " <> hex (shComId h) <> ": "
              <> either id (\n -> "Success/Failure " <> tshow n) outcome)


-- * What the verbs build on

-- | The base ComID: the one recorded (UCT-01's), else the Opal SSC V2
--   descriptor's of the Level 0 response, discovered if need be (Test Cases
--   4.1 g and Table 5, PDF 26: a static ComID the descriptor reports).
baseComId :: Script Word16
baseComId = do
  ctx <- readCtx
  case rcBaseComId ctx of
    Just c  -> pure c
    Nothing -> do
      l0 <- maybe discover pure (rcLevel0 ctx)
      case featureIn l0 of
        Just (Right d) -> let c = level0DiscoveryOpalSSCV2FeatureBaseComID (drRecord d)
                          in c <$ modifyCtx (\x -> x { rcBaseComId = Just c })
        Just (Left e)  -> unmet ("the Opal SSC V2 Feature descriptor does not decode: " <> tshow e)
        Nothing        -> unmet "no Opal SSC V2 Feature descriptor in Level 0 Discovery: no base ComID to use"

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
--   ("Hsed.Wire".'exchangeComPacket'): the IF-SEND through the end's path
--   (the simulator's transport, or the Guard's under 'drivePolicy'), then
--   IF-RECV, with the length Properties reported or the default, until the
--   TPer has returned everything; the packets of the answer, none for Table
--   45's (0, 0, 0).
exchangeOn :: Word16 -> ByteString -> Script (Either ExchangeError [PacketFrame])
exchangeOn c bytes = do
  len <- recvLength
  env <- ask
  let opts = defaultExchangeOpts { xRecvLen = len }
  theEnd >>= \case
    SimEnd tr     -> liftIO (exchangeComPacket opts (\q -> Right <$> ifSend tr (proto q) (comId q) (lenBytes q) (payload q))
                                               (reChannel env) tr c bytes)
    DriveEnd tr _ -> liftIO (exchangeComPacket opts (guardedSend (reTranscript env) drivePolicy tr) (reChannel env) tr c bytes)

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

-- | The number of a column found by name in a generated table description
--   ("Hsed.Gen.Core.TableDescs"), the script stopping where the description
--   lacks it.
namedColumn :: TableDesc -> Text -> Script Natural
namedColumn desc name =
  maybe (unmet ("the generated " <> tdName desc <> " table description has no column " <> name)) (pure . fromIntegral . colNumber)
        (columnNamed name desc)

-- | The simulator's state, on the simulator ("Hsed.Transport.InProc"'s
--   control); 'Nothing' on a drive.
peekSim :: Script (Maybe TPerState)
peekSim = asks reTarget >>= \case
  SimTarget _ control -> Just <$> liftIO (scPeek control)
  _                   -> pure Nothing

-- | An authority's credential as the simulator's Admin SP holds it: its
--   C_PIN object's PIN, TryLimit and Tries (Core 5.3.4.1.1.2, PDF 210).
data Credential = Credential
  { crPIN      :: !ByteString
  , crTryLimit :: !Natural
  , crTries    :: !Natural
  }
  deriving (Eq, Show)

-- | An authority's credential read from the simulator's state: the Admin
--   SP's Authority row of the UID, its Credential a reference to a C_PIN
--   object (Opal 2.30 Table 24, PDF 48), that object's PIN, TryLimit and
--   Tries (Table 25, PDF 48-49; a limit the row does not give is 0), the
--   columns found by name in the generated descriptions; 'Nothing' for an
--   authority the Admin SP lacks or one with no credential.
simCredential :: UID -> TPerState -> Maybe Credential
simCredential authority st = do
  authorityRow <- rowWithUID uSP_Admin (tdTableUID authorityTableDesc) authority st
  RefCell _ credential <- cellNamed authorityTableDesc "Credential" authorityRow
  pinRow <- rowWithUID uSP_Admin (tdTableUID c_PINTableDesc) credential st
  BytesCell pin <- cellNamed c_PINTableDesc "PIN" pinRow
  pure (Credential pin (count "TryLimit" pinRow) (count "Tries" pinRow))
  where
    cellNamed desc name row = columnOf desc name >>= (`cellOf` row)
    count name row = case cellNamed c_PINTableDesc name row of
      Just (UIntCell n) | n >= 0 -> fromInteger n
      _                          -> 0

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

-- | A value that is a uinteger.
uinteger :: Value -> Maybe Natural
uinteger = \case
  D (Datum (Unsigned n)) -> Just n
  _                      -> Nothing

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
  IfError s   -> "a command answered " <> describeStatus s
  Malformed t -> "an answer that does not read: " <> t
  TimedOut    -> "no answer within the host's timeout"
  Refused t   -> "refused: " <> t

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
