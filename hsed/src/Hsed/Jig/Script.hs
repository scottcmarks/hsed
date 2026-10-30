{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

A script runs on one end of a run, the simulator or a drive (the runner
steps a pair's ends one after the other, "Hsed.Jig.Run"). On the simulator
the verbs use its transport as it is; on a drive every command goes through
the Guard under M3's policy ('drivePolicy', Level0Only), so that a refused
one is recorded and answered 'Unsupported', never issued.

An expectation that does not hold ends the script ('unmet'); 'attempt'
turns what a script comes to into its 'Verdict'; 'note' adds a line to the
transcript, for what a case does apart from its verdict. The host's side of
Table 45 is read from the generated table
("Hsed.Gen.Core.StateTables".@table45@): an answer's Length,
OutstandingData and MinTransfer match a row when each equals what the row
prints and is not zero where the row names a quantity ('t45Of',
'expectT45'), an answer being a ComPacket of the ComID asked and a static
ComID's extension ('t45On'); 'drain' asks again as the host's loop does
("Hsed.Wire".@exchangeComPacket@), by MinTransfer, then by OutstandingData,
until everything is returned. The ComPackets the jig sends are the Test
Cases' (Table 6, PDF 27): one packet of the control session holding one
Data subpacket ('controlComPacket').
-}
module Hsed.Jig.Script
  ( -- * The run's environment
    RunCtx (..)
  , RunEnv (..)
  , Script
    -- * Verdicts
  , Verdict (..)
  , attempt
  , unmet
  , note
    -- * Verbs
  , discover
  , ifRecvOn
  , rawIfSend
  , properties
  , expectStatus
  , expectT45
  , drain
  , simOnly
    -- * What the verbs build on
  , drivePolicy
  , baseComId
  , featureIn
  , transferFor
  , sendLength
  , controlComPacket
  , propertiesCall
  , maxComPacketSizeName
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

import           Hsed.Core.Call         (MethodCall (..), MethodHeader (..), Parameters (..))
import           Hsed.Core.Feature      (Feature (..), decodeFeature)
import           Hsed.Core.Layout       (LayoutError)
import           Hsed.Core.Method       (invocation)
import           Hsed.Core.Record       (DecodedRecord (..), decodeRecord)
import           Hsed.Core.StreamItem   (StreamItem (..))
import           Hsed.Core.Sync         (Setting (..), StateRow (..), StateTable (..), settingOf)
import           Hsed.Core.Token        (Token (..))
import           Hsed.Core.Value        (Datum (..), List (..), Name (..), NamedValue (..), Value (..))
import           Hsed.Gen.Core.Layouts  (ComPacket (..), ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..),
                                         PacketHeader (..))
import qualified Hsed.Gen.Core.Methods  as Methods
import           Hsed.Gen.Core.StateTables (table45)
import           Hsed.Gen.Core.UIDs     (uProperties, uSMUID)
import           Hsed.Gen.Opal.Features (Level0DiscoveryOpalSSCV2Feature (..))
import           Hsed.Jig.Guard         (Policy (..), guardedRecv, guardedSend)
import           Hsed.Jig.Options       (RunOptions (..))
import           Hsed.Jig.Target        (Target (..))
import           Hsed.Jig.Transcript    (TEntry (..), Transcript, record)
import           Hsed.Level0            (Descriptor (..), Level0 (..), Level0Problem (..), describeProblem, splitLevel0)
import           Hsed.Packet            (ComPacketFrame (..), PacketFrame (..), encodeComPacket)
import           Hsed.Sim.Sync          (T45Row (..), t45Rows)
import           Hsed.Sim.Types         (maxComPacketSizeName)
import           Hsed.Transport.Cdb     (level0Cdb)
import           Hsed.Transport.Drive   (DriveInfo (..))
import           Hsed.Wire              (Cap (..), Channel, Dir (..), ExchangeError (..), ExchangeOpts (..), IfStatus (..),
                                         Request (..), Response (..), Transport, Unit (..), comPacketProtocol, dataKind,
                                         defaultExchangeOpts, exchangeComPacket, hex, ifRecv, ifSend, ifStatusDetail,
                                         ifStatusName, level0ComId, mostCarried, roundUp, staticExtension, transferLength,
                                         unitOf)


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
--   properties by name. The host's loop is "Hsed.Wire"'s, its IF-SEND the
--   simulator's or the Guard's; MaxComPacketSize, once reported, is kept.
properties :: Script (Either ExchangeError [(Text, Integer)])
properties = do
  c <- baseComId
  len <- recvLength
  bytes <- either (unmet . ("the Properties ComPacket does not encode: " <>) . tshow) pure (controlComPacket c propertiesCall)
  env <- ask
  let opts = defaultExchangeOpts { xRecvLen = len }
  answered <- theEnd >>= \case
    SimEnd tr     -> liftIO (exchangeComPacket opts (\q -> Right <$> ifSend tr (proto q) (comId q) (lenBytes q) (payload q))
                                               (reChannel env) tr c bytes)
    DriveEnd tr _ -> liftIO (exchangeComPacket opts (guardedSend (reTranscript env) drivePolicy tr) (reChannel env) tr c bytes)
  let props = answered >>= propertiesOf
  for_ (either (const Nothing) (L.lookup maxComPacketSizeName) props) $ \m ->
    modifyCtx (\x -> x { rcMaxComPacketSize = Just (fromInteger m) })
  pure props

-- | The TPer's properties from the packets of its answer: the subpackets'
--   tokens parse to SMUID.Properties (a Session Manager response is a method
--   invocation, Core 3.2.4.2) with status 0, whose first parameter is the
--   list of the TPer's properties, each a name and a number (Core 5.2.2.1,
--   PDF 140).
propertiesOf :: [PacketFrame] -> Either ExchangeError [(Text, Integer)]
propertiesOf packets = do
  let tokens = mconcat [ dataSubPacketPayload sp | p <- packets, sp <- pfSubPackets p, dataSubPacketKind (dataSubPacketHeader sp) == dataKind ]
  MethodCall (MethodHeader invoker method) (Parameters required _) st <-
    first (Malformed . ("the answer does not parse as a method invocation: " <>) . T.pack) (parseByteString tokens)
  unless (invoker == uSMUID && method == uProperties) (Left (Malformed "the answer is no SMUID.Properties"))
  unless (st == 0) (Left (Malformed ("Properties answered status " <> tshow st)))
  case required of
    L (List vs) : _ -> Right (mapMaybe property vs)
    _               -> Left (Malformed "the answer's first parameter is no list of properties")
  where
    property = \case
      N (NamedValue (Name (Datum (Bytes n))) (D (Datum v))) -> (,) (decodeUtf8Lenient n) <$> numeric v
      _                                                    -> Nothing
    numeric = \case
      Unsigned n -> Just (toInteger n)
      Signed i   -> Just i
      _          -> Nothing

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
    polls = xTimeoutMicros defaultExchangeOpts `div` max 1 (xPollMicros defaultExchangeOpts)
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

-- | Run a step on the simulator alone: 'Nothing' on a drive.
simOnly :: Script a -> Script (Maybe a)
simOnly s = asks reTarget >>= \case
  SimTarget _ _ -> Just <$> s
  _             -> pure Nothing


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
  ComPacketFrame (ComPacketHeader c 0 0 0) [PacketFrame (PacketHeader 0 0 0 0 0) [DataSubPacket (DataSubPacketHeader dataKind) tokens]]

-- | SMUID.Properties with no HostProperties (Core 5.2.2.1), as tokens.
propertiesCall :: ByteString
propertiesCall = generate (invocation uSMUID (Methods.Properties Nothing))

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
  Malformed t -> "an answer that is no ComPacket of the ComID: " <> t
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
