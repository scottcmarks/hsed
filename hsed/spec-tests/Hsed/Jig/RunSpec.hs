{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.RunSpec
Description : Test specifications for Hsed.Jig.Run: the jig run against the simulator, a replay and both
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

M3's acceptance on the simulator (design 11; the plan's integration step 3)
as @hsed-jig run@ makes it: @--tests UCT-01,ETC-02,ETC-03,SPF-02:1@ passes
in bytes and in 512-byte blocks, each with and without a response held for
an IF-RECV; ETC-02's second IF-SEND is answered Synchronous Protocol
Violation; SPF-02:1's IF-RECV in S1 (0, 0, 0); a held run shows an IF-RECV
answered (0, 1, 0); the jig's extensions pass; the Markdown report prints
the LockingManufactured waiver with PDF 29. M4a's acceptance (design 11):
UCT-02, ETC-08 and ETC-15 pass on the scaffold's Session Manager now,
UCT-02 observing both tables (DefSessionTimeout 0 met as no limit, the
lead's decision) and its echo extension passing, ETC-08 and ETC-15
observing (0, 0, 0); and, with the simulator's sessions (groups G1 and G2,
merged at the integration, which retired the waits), the ten cases pass
in the four configurations, UCT-03 with its partial-verdict text and its
extensions (NOT_AUTHORIZED, SP_BUSY, the MSID the simulator's), ETC-04
observing (0, 0, 0), ETC-21/adminsp seeing NOT_AUTHORIZED (and, at TryLimit
3, counting Tries; at its limit, skipped, its extension with it), the MSID
masked in every report unless @--show-msid@, UCT-03 skipped with
@--sim-sid-pin@, its extensions not run, and UCT-01 failing 4c.ii there,
and a session left open before a case established closed by STACK_RESET,
noted. Then a drive-like response
synthesized from disk6's sedutil query, replayed as a drive: UCT-01 passes,
the cases that need more than Level 0 are skipped with their reasons (a
replay answers Level 0 Discovery alone, whatever the run admits), no
IF-SEND is issued, and the waivers of SyncIdle, NoSessions (not needed, Level
0 Discovery outside the synchronous protocol) and LockingManufactured are
printed; paired with the simulator, 0x0403 is
missing without a profile and the comparison passes with it; standing in
for the kext's drive, the replay's transcript notes the hardware run's CDB
and the capture is written; a drive whose Level 0 Discovery fails is asked
once and never again; a header revision Opal allows is noted as allowed.
The simulator as the drive (Support's fakeDrive, M4c), behind the Guard as
a run opens a drive: UCT-01 passes, the drive's policy Level0Only throughout
and its base ComID never bound, Level 0 Discovery the one command it is
issued, its SyncIdle and NoSessions waived as not needed. M4c's acceptance
on it, @--tests UCT-02,UCT-03:1a-1c,SPF-02:1@, in both ways a drive's
SyncIdle and NoSessions are had: with STACK_RESET before each case (the
default, Scott's decision of 2026-11-02), 16 commands, 7 IF-SENDs, three
STACK_RESETs noted, no NoSessions waived; without it, 13 commands, 4
IF-SENDs, none of protocol 2, SyncIdle probed and NoSessions waived as
unobservable; either way exit 0, the policy AllowList 0x1004 of what the
run admits, nothing outside it and nothing refused, the MSID one secret
masked in every report; and again on a drive whose TPer reports a
MaxComPacketSize more than the kext carries, every IF-RECV asking for 2048.
@--interrupt-in-session@ stops a run after StartSession, exit 130, the
session left open on purpose and the case skipped as interrupted; the run
after it fails 1a with SP_BUSY and @--recover@'s hint without STACK_RESET,
and passes with it. A case off design 9.3's drive list is skipped on a
drive; the Guard's refusal is a problem of the run (exit 1); a command
outside the policy is a safety violation (exit 3, 'runExit'). UCT-03's
MSID read answered with what does not read as the MSID is masked in every
report.
On a pair each end's verdicts and Level 0 are named by its transport's
role, as the transcript's entries are. And what the run refuses before it
opens anything: a key the catalogue lacks (exit 2), @--recover@ with
@--tests@, a pair or @--interrupt-in-session@ (exit 2), a drive without the
serial it must have (exit 2), a Level 0 length a drive or a replay cannot
carry (exit 2), a drive without root (exit 3).

Since the M4c review: the stop signals' handlers put back as they were
found, base's own SIGINT handler among them; a StartSession whose
SyncSession does not read cleared by STACK_RESET where the run admits it
and otherwise kept for the next case's NoSessions and the reports, in both
modes; a STACK_RESET before a case that answers Success/Failure 1, or
Pending for ever (the host's polls shortened, @roExchange@), an unmet
prerequisite; @--interrupt-in-session@ with SPF-02:1 after UCT-03 and
@--capture-level0@: SPF-02:1 not run, no capture, no Level 0 after the
cases; on the simulator a stop during a case's extension keeps the case's
TCG verdict, the extension skipped as interrupted. Since its verification:
a StartSession whose answer asks a MinTransfer the drive's transport cannot
carry, the IF-SEND issued, treated as one whose SyncSession does not read,
in both modes; a session whose End of Session goes unanswered and whose
closing STACK_RESET answers Success/Failure 1, in the run's last case, kept
for the reports; a stop in the first of UCT-03's three extensions, the two
after it not run; a SIGHUP set up as base sets up SIGINT, its action reset
once delivered, put back so after a run, which one SIGHUP delivered then
shows by the default action it leaves (sigaction read through
spec-tests/cbits/hsed_test_signals.c).
-}
module Hsed.Jig.RunSpec (spec) where

import qualified Data.Aeson             as A
import qualified Data.Aeson.KeyMap      as KM
import           Numeric                (showHex)
import qualified RIO.ByteString         as B
import qualified RIO.List               as L
import qualified RIO.Map                as Map
import           RIO
import           RIO.Directory          (doesFileExist, listDirectory)
import           RIO.FilePath           (takeExtension, (</>))
import qualified RIO.Text               as T
import           Data.Dynamic           (dynTypeRep, toDyn)
import           Foreign.C.Types        (CInt (..))
import qualified GHC.Conc.Signal        as Conc
import           System.Posix.Signals   (Handler (..), installHandler, raiseSignal, sigHUP, sigINT)
import           Test.Hspec

import           Hsed.Core.Call         (MethodCall (..), MethodHeader (..), MethodResponse (..))
import           Hsed.Core.Feature      (Allowance (..), Feature (..))
import           Hsed.Core.Method       (invocation)
import           Hsed.Core.StreamItem   (StreamItem (..))
import           Hsed.Core.Token        (IsToken (..), Token (EndOfSession))
import           Hsed.Core.UID          (UID)
import           Hsed.Core.Layout       (LayoutError)
import           Hsed.Core.Record       (DecodedRecord (..), decodeRecordStrict, encodeRecord)
import           Hsed.Core.Value        (IsValue (..), List (..), Value (..), (.:))
import           Hsed.Gen.Core.Layouts  (ComPacketHeader (..), DataSubPacket (..), DataSubPacketHeader (..), PacketHeader (..),
                                         STACK_RESETCommandResponse (..), STACK_RESETPending (..))
import qualified Hsed.Gen.Core.Methods  as Methods
import           Hsed.Gen.Core.UIDs     (methodIDUIDs, uAuthority_SID, uC_PIN_SID, uMethodID_Get, uSMUID, uSP_Admin)
import           Hsed.Gen.Opal.Features (Level0DiscoveryOpalSSCV2Feature)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uC_PIN_MSID)
import           Hsed.Jig.Catalogue     (CaseKey, parseCaseKeys)
import           Hsed.Jig.Files         (runningAsRoot)
import           Hsed.Jig.Guard         (Allowed (..), Policy (..), driveClose, guardDrive, readOnlyMethods)
import           Hsed.Jig.Options       (TargetSpec (..), defaultRunOptions)
import           Hsed.Jig.Prereq        (level0OutsideSync, sessionsUnobservable)
import           Hsed.Jig.Report        (verifyTranscript)
import           Hsed.Jig.Run
import           Hsed.Jig.Script        (controlComPacket)
import           Hsed.Jig.Support       (at, atPath, disk6Like, fakeDrive, onFakeDrive, silently)
import           Hsed.Jig.Target        (Target (..))
import           Hsed.Jig.Transcript    (TEntry (..), newTranscript)
import           Hsed.Level0.Sedutil    (SedutilFeature (..), SedutilQuery (..), parseQuery, synthesize)
import           Hsed.Packet            (ComPacketFrame (..), DecodedFrame (..), PacketFrame (..), decodeComPacket, encodeComPacket)
import           Hsed.Sim.FeatureSets   (DataStoreFeature)
import           Hsed.Sim.State         (lifeOf)
import           Hsed.Sim.Types         (ComIdLife (Associated), SimParams (..), TPerState (..), defaultSimParams, maxComPacketSizeName)
import           Hsed.Transport.Drive   (DriveInfo (..), InterfaceType (..), RawDrive (..))
import           Hsed.Transport.InProc  (InProcOpts (..), SimControl (..))
import           Hsed.Transport.Opened  (openedInProc)
import           Hsed.Transport.Replay  (openReplay)
import           Hsed.Wire              (ExchangeOpts (..), defaultExchangeOpts, exactly, exchangeComPacket, extendedComId, ifSend,
                                         newChannel)
import           Hsed.Wire.Internal     (Describe (..), Dir (..), IfStatus (..), Request (Request, comId, lenBytes, payload, proto),
                                         Response (..), Transport (..), Unit (..))
import qualified Hsed.Wire.Internal     as Wire


keys :: Text -> [CaseKey]
keys = either (error . T.unpack) id . parseCaseKeys

-- | Whether a signal's handler is reset to the default once delivered (1), stays (0), or there is none, the
--   default action or ignored (-1): what System.Posix.Signals does not give back (spec-tests/cbits/hsed_test_signals.c).
foreign import ccall unsafe "hsed_test_signal_resets"
  signalResets :: CInt -> IO CInt

-- | The ten cases of M4a, in the catalogue's order.
tenCases :: [CaseKey]
tenCases = keys "UCT-01,UCT-02,UCT-03:1a-1c,SPF-02:1,ETC-02,ETC-03,ETC-04,ETC-08,ETC-15,ETC-21/adminsp"

-- | The four cases of M3 on the simulator, in a unit, holding a response
--   for so many IF-RECVs.
onSim :: Unit -> Int -> RunOptions
onSim u hold = defaultRunOptions
  { roTarget = TSim
  , roTests  = keys "UCT-01,ETC-02,ETC-03,SPF-02:1"
  , roSim    = defaultSimParams { spUnits = u }
  , roHold   = hold
  }

-- | The four configurations the acceptance runs in: bytes and blocks, a
--   response released at once or held for an IF-RECV.
configurations :: [(Unit, Int)]
configurations = [(UnitBytes, 0), (UnitBlocks512, 0), (UnitBytes, 1), (UnitBlocks512, 1)]

configured :: (Unit, Int) -> String
configured (u, hold) = show u <> (if hold > 0 then ", a response held for an IF-RECV" else "")

-- | A run into a temporary directory, quietly: its exit code, its JSON
--   report and its Markdown, and what else the test does in the directory.
jigIn :: FilePath -> RunOptions -> IO (ExitCode, A.Value, Text)
jigIn dir opts = do
  let reports = dir </> "report"
  code <- silently (runJig opts { roReportDir = reports })
  files <- listDirectory reports
  case ([ f | f <- files, takeExtension f == ".json" ], [ f | f <- files, takeExtension f == ".md" ]) of
    ([json], [md]) -> do
      decoded <- readFileBinary (reports </> json) >>= either throwString pure . A.eitherDecodeStrict'
      text <- readFileUtf8 (reports </> md)
      pure (code, decoded, text)
    _ -> throwString ("not one report in " <> reports <> ": " <> show files)

jig :: RunOptions -> IO (ExitCode, A.Value, Text)
jig opts = withSystemTempDirectory "hsed-jig" (`jigIn` opts)

-- | A run into a temporary directory: its exit code and its three reports
--   as text.
jigTexts :: RunOptions -> IO (ExitCode, Text, Text, Text)
jigTexts opts = withSystemTempDirectory "hsed-jig" $ \dir -> do
  let reports = dir </> "report"
  code <- silently (runJig opts { roReportDir = reports })
  files <- listDirectory reports
  let one ext = case [ f | f <- files, takeExtension f == ext ] of
        [f] -> readFileUtf8 (reports </> f)
        _   -> throwString ("not one " <> ext <> " report in " <> reports)
  (,,,) code <$> one ".json" <*> one ".md" <*> one ".xml"

-- | A JSON array's elements.
elements :: Maybe A.Value -> [A.Value]
elements = \v -> case v of
  Just (A.Array xs) -> toList xs
  _                 -> []

-- | Each case's key and verdict.
verdicts :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
verdicts json = [ (at "key" c, at "verdict" c) | c <- elements (at "cases" json) ]

-- | Each extension's verdict.
extensionVerdicts :: A.Value -> [Maybe A.Value]
extensionVerdicts json = [ at "verdict" x | c <- elements (at "cases" json), x <- elements (at "extensions" c) ]

-- | A case's observations, by its key.
observations :: A.Value -> Text -> [(Text, Text)]
observations json key =
  [ (n, v) | c <- elements (at "cases" json), at "key" c == str key, o <- elements (at "observations" c)
           , Just (A.String n) <- [at "name" o], Just (A.String v) <- [at "value" o] ]

-- | The transcript's notes.
notes :: A.Value -> [Text]
notes json = [ t | e <- elements (at "transcript" json), Just (A.String t) <- [at "text" e] ]

-- | The rows of Table 45 the IF-RECVs of the transcript were answered with.
t45s :: A.Value -> [Text]
t45s json = [ row | e <- elements (at "transcript" json), Just (A.String row) <- [atPath ["decode", "response", "t45"] e] ]

-- | The statuses the transcript's IF-SENDs were answered with.
sendStatuses :: A.Value -> [Text]
sendStatuses json =
  [ s | e <- elements (at "transcript" json), atPath ["request", "op"] e == Just "if_send"
      , Just (A.String s) <- [atPath ["response", "status"] e] ]

-- | The method calls decoded in the transcript's IF-RECV answers: each
--   method's name and status.
answeredCalls :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
answeredCalls json =
  [ (at "method" mc, at "status" mc)
  | e <- elements (at "transcript" json), p <- elements (atPath ["decode", "response", "packets"] e)
  , sp <- elements (at "subpackets" p), Just mc <- [at "method_call" sp] ]

str :: Text -> Maybe A.Value
str = Just . A.String

-- | Bytes as hex, as a transcript prints them unmasked.
hexOf :: ByteString -> Text
hexOf = T.pack . concatMap (\w -> (if w < 16 then "0" else "") <> showHex w "") . B.unpack

-- | The message of design 9.3 item 6's error paths, which no drive runs.
errorPath :: Text
errorPath = "needs an IF-SEND off the allow list, an error path: hardware error paths stay off a drive (design 9.3 item 6 and 13.8, Scott's)"

-- | An IF-SEND of protocol 1 on 0x1004 of tokens in a ComPacket of the
--   control session, padded to a 512-byte block.
blockOf :: ByteString -> Request
blockOf tokens = Request IfSend 1 0x1004 512 (bytes <> B.replicate (512 - B.length bytes) 0)
  where bytes = either (error . show) id (controlComPacket 0x1004 tokens)

-- | Whether an IF-SEND's ComPacket carries Get on C_PIN_MSID.
getsMsid :: Request -> Bool
getsMsid q = case decodeComPacket (payload q) of
  Right (DecodedFrame (ComPacketFrame _ ps) _ _) ->
    or [ i == uC_PIN_MSID && m == uMethodID_Get
       | p <- ps, sp <- pfSubPackets p, Right (MethodCall (MethodHeader i m) _ _) <- [parseByteString (dataSubPacketPayload sp)] ]
  Left _ -> False

-- | A TPer's answer to a Get, its packet's Session kept, holding a result
--   that is no @[[3 = bytes]]@ but carries bytes in column 4: a result
--   UCT-03's 1b cannot read as the MSID; padded to the length asked.
answerHolding :: ByteString -> Response -> Int -> ByteString
answerHolding bytes r len = framedAnswer <> B.replicate (len - B.length framedAnswer) 0
  where
    result = generate (MethodResponse [L (List [N ((4 :: Natural) .: bytes)])] 0)
    packetHeader = case decodeComPacket (rpayload r) of
      Right (DecodedFrame (ComPacketFrame _ (p : _)) _ _) -> pfHeader p
      _                                                    -> error "the simulator's answer holds no packet"
    framedAnswer = either (error . show) id $ encodeComPacket $
      ComPacketFrame (ComPacketHeader 0x1004 0 0 0) [PacketFrame packetHeader [DataSubPacket (DataSubPacketHeader 0) result]]

-- | StartSession to the Admin SP as Anybody, HSN 1, read-write (what the
--   jig's UCT-03 1a sends), as tokens.
startSessionCall :: ByteString
startSessionCall = startAs Nothing Nothing

-- | StartSession to the Admin SP as SID with a wrong HostChallenge (what
--   ETC-21/adminsp sends), as tokens.
wrongSidCall :: ByteString
wrongSidCall = startAs (Just uAuthority_SID) (Just "wrong")

startAs :: Maybe UID -> Maybe ByteString -> ByteString
startAs authority challenge = generate $ invocation uSMUID Methods.StartSession
  { Methods.startSessionHostSessionID         = value (1 :: Natural)
  , Methods.startSessionSPID                  = value (token uSP_Admin)
  , Methods.startSessionWrite                 = value (1 :: Natural)
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

-- | A run over a drive behind the Guard as a run opens one (as
--   "Hsed.Jig.Support".'onFakeDrive' does: a new transcript, the run's target
--   named as the drive's device), the drive admitting on its bind what the
--   selected cases admit ('driveAllowedFor'), or what is given: its exit
--   code, its JSON report, and its Markdown and JUnit reports as text.
onDriveReports :: RunOptions -> Maybe [Allowed] -> (RawDrive, DriveInfo, SimControl) -> IO (ExitCode, A.Value, Text, Text)
onDriveReports opts admits (raw, di, _) = withSystemTempDirectory "hsed-jig" $ \dir -> do
  t <- newTranscript
  g <- guardDrive t (fromMaybe (either (const []) (driveAllowedFor opts) (selectedCases (roTests opts))) admits) (Just di) raw
  let reports = dir </> "report"
  code <- silently (runOpened opts { roTarget = TDevice ("/dev/" <> diBsd di), roReportDir = reports } t (DriveTarget g))
            `finally` driveClose g
  files <- listDirectory reports
  let one ext = case [ f | f <- files, takeExtension f == ext ] of
        [f] -> readFileUtf8 (reports </> f)
        _   -> throwString ("not one " <> ext <> " report in " <> reports)
  json <- one ".json" >>= either throwString pure . A.eitherDecodeStrict' . encodeUtf8
  (,,,) code json <$> one ".md" <*> one ".xml"

-- | A block's bytes: what is given, zero-padded to 512.
padded :: ByteString -> ByteString
padded b = b <> B.replicate (512 - B.length b) 0

-- | A STACK_RESET response made to answer Success/Failure 1, the stack not
--   reset (Core Table 36, PDF 66); any other answer as it was.
resetFailing :: ByteString -> ByteString
resetFailing bytes = case decodeRecordStrict bytes :: Either LayoutError (DecodedRecord STACK_RESETCommandResponse) of
  Right (DecodedRecord response _ _) -> either (const bytes) padded (encodeRecord response { stack_RESETCommandResponseSuccessFailure = 1 })
  Left _                             -> bytes

-- | StartSession's method UID, 00 00 00 00 00 00 FF 02 (Core Table 241,
--   PDF 303), as it stands in a ComPacket's tokens, a short atom of eight
--   bytes (A8h): an IF-SEND carrying it is StartSession's.
startSessionUid :: ByteString
startSessionUid = B.pack [0xA8, 0, 0, 0, 0, 0, 0, 0xFF, 0x02]

-- | Whether an IF-SEND's payload is a ComPacket holding the host's End of
--   Session: a packet of a session (TSN not 0) whose one data subpacket
--   holds the End of Session token alone (Core 3.2.2.3.3.3, PDF 37).
endsSession :: ByteString -> Bool
endsSession bytes = case decodeComPacket bytes of
  Right (DecodedFrame (ComPacketFrame _ ps) _ _) ->
    any (\p -> packetTSN (pfHeader p) /= 0 && map dataSubPacketPayload (pfSubPackets p) == [generate EndOfSession]) ps
  _ -> False

-- | A ComPacket on 0x1004 of no packets, with OutstandingData and
--   MinTransfer as given, padded to a length (Core Table 17, PDF 40;
--   Table 45, PDF 83).
emptyComPacket :: Word32 -> Word32 -> Word32 -> ByteString
emptyComPacket outstanding minTransfer len =
  exactly len (either (const mempty) id (encodeComPacket (ComPacketFrame (ComPacketHeader 0x1004 0 outstanding minTransfer) [])))

-- | The commands a report's transcript issued, in order: each one's op,
--   protocol and ComID.
issuedOps :: A.Value -> [(Maybe A.Value, Maybe A.Value, Maybe A.Value)]
issuedOps json = [ (atPath ["request", "op"] e, atPath ["request", "proto"] e, atPath ["request", "comid"] e)
                 | e <- elements (at "transcript" json), at "entry" e == str "issued" ]

-- | Level 0 Discovery's IF-RECV; an IF-RECV and an IF-SEND of protocol 1 on
--   0x1004; STACK_RESET's IF-SEND and GET_COMID_RESPONSE's IF-RECV.
level0Op, recvOp, sendOp, resetSendOp, resetRecvOp :: (Maybe A.Value, Maybe A.Value, Maybe A.Value)
level0Op    = (str "if_recv", num 1, num 1)
recvOp      = (str "if_recv", num 1, num 4100)
sendOp      = (str "if_send", num 1, num 4100)
resetSendOp = (str "if_send", num 2, num 4100)
resetRecvOp = (str "if_recv", num 2, num 4100)

num :: Integer -> Maybe A.Value
num = Just . A.Number . fromInteger

-- | The methods the IF-SENDs of protocol 1 a report's transcript issued
--   carry, in order, each by its name, a bare End of Session as
--   @EndOfSession@.
sentMethods :: A.Value -> [Text]
sentMethods json =
  [ m | e <- elements (at "transcript" json), at "entry" e == str "issued", atPath ["request", "op"] e == str "if_send"
      , p <- elements (atPath ["decode", "request", "packets"] e), sp <- elements (at "subpackets" p)
      , m <- [ n | Just (A.String n) <- [atPath ["method_call", "method"] sp] ]
             <> [ "EndOfSession" | i <- elements (at "items" sp), at "end_of_session" i == Just (A.Bool True) ] ]

-- | Get's name as a report prints a method, the generated MethodID table's
--   (Core Table 242).
getName :: Text
getName = maybe "Get" T.pack (L.lookup uMethodID_Get methodIDUIDs)

-- | Each case's waivers: its key, and each waiver's prerequisite and reason.
waiversOf :: A.Value -> [(Text, Text, Text)]
waiversOf json = [ (k, w, r) | c <- elements (at "cases" json), Just (A.String k) <- [at "key" c], x <- elements (at "waivers" c)
                             , Just (A.String w) <- [at "prereq" x], Just (A.String r) <- [at "reason" x] ]

-- | Each case's reasons: a failure's, or a skip's.
reasonsOf :: A.Value -> [(Text, [Text])]
reasonsOf json = [ (k, [ r | A.String r <- elements (at "reasons" c) ] <> [ r | Just (A.String r) <- [at "reason" c] ])
                 | c <- elements (at "cases" json), Just (A.String k) <- [at "key" c] ]

-- | A policy as a report's "policies" prints the drive's.
drivePolicyJSON :: Policy -> A.Value
drivePolicyJSON p = case A.toJSON p of
  A.Object o -> A.Object (o <> KM.fromList ["end" A..= ("drive" :: Text)])
  v          -> v

-- | The one JSON report a run wrote under a directory's @report@.
reportIn :: FilePath -> IO A.Value
reportIn dir = listDirectory (dir </> "report") >>= \fs -> case [ f | f <- fs, takeExtension f == ".json" ] of
  [f] -> readFileBinary (dir </> "report" </> f) >>= either throwString pure . A.eitherDecodeStrict'
  _   -> throwString "not one report"

spec :: Spec
spec = do
  describe "what a run refuses before it opens anything" $ do
    it "a key the catalogue lacks: exit 2" $
      silently (runJig defaultRunOptions { roTests = keys "SPF-13" }) `shouldReturn` ExitFailure 2
    it "a drive without --expect-serial, alone or paired, root or not: exit 2" $ do
      silently (runJig defaultRunOptions { roTarget = TDevice "/dev/disk99" }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roTarget = TPair TSim (TDevice "/dev/disk99") }) `shouldReturn` ExitFailure 2
    it "a Level 0 length a drive or a replay cannot carry (255 blocks of 512), alone or paired: exit 2; one it can, opened" $ do
      silently (runJig defaultRunOptions { roTarget = TReplay "nowhere.bin", roLevel0Len = 1048576 }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roTarget = TReplay "nowhere.bin", roLevel0Len = 130561 }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roTarget = TPair TSim (TReplay "nowhere.bin"), roLevel0Len = 1048576 }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roTarget = TReplay "nowhere.bin", roLevel0Len = 130560 }) `shouldReturn` ExitFailure 1
    it "simulator parameters out of range, the Locking SP's bounds among them (M5a; Opal 2.30 Table 40 note 2, PDF 82), alone or paired: exit 2; a replay alone reads none" $ do
      let outOfBounds = defaultSimParams { spLockingUsers = 7 }
      silently (runJig defaultRunOptions { roSim = outOfBounds }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roSim = outOfBounds, roTarget = TPair TSim (TReplay "nowhere.bin") }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roSim = outOfBounds, roTarget = TReplay "nowhere.bin" }) `shouldReturn` ExitFailure 1
    it "a drive without root, alone or paired: exit 3" $ do
      root <- runningAsRoot
      let named = defaultRunOptions { roExpectSerial = Just "S0000000000000" }
      if root
        then pendingWith "running as root: the gate lets a drive through"
        else do
          silently (runJig named { roTarget = TDevice "/dev/disk99" }) `shouldReturn` ExitFailure 3
          silently (runJig named { roTarget = TPair TSim (TDevice "/dev/disk99") }) `shouldReturn` ExitFailure 3
          silently (runJig named { roTarget = TDevice "/dev/disk99", roRecover = True }) `shouldReturn` ExitFailure 3
    it "--recover with --tests, or with a pair: exit 2 (it runs alone, on one end)" $ do
      silently (runJig defaultRunOptions { roRecover = True, roTests = keys "UCT-02" }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roRecover = True, roTarget = TPair TSim (TReplay "nowhere.bin") }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roRecover = True, roTests = keys "UCT-02", roTarget = TDevice "/dev/disk99" }) `shouldReturn` ExitFailure 2
    it "--recover with --interrupt-in-session: exit 2 (it runs to its end; its own probe's session would be left open)" $ do
      silently (runJig defaultRunOptions { roRecover = True, roInterruptInSession = True }) `shouldReturn` ExitFailure 2
      silently (runJig defaultRunOptions { roRecover = True }) `shouldReturn` ExitSuccess

  describe "UCT-01's minima" $
    it "are the ones Opal and the DataStore feature set allow, the Test Cases' (PDF 30): 1, 4, 8; 1, 0xA0000, 1" $ do
      [ L.lookup f (featureAllowances (Proxy :: Proxy Level0DiscoveryOpalSSCV2Feature))
        | f <- ["Number of ComIDs", "Number of Locking SP Admin Authorities Supported", "Number of Locking SP User Authorities Supported"] ]
        `shouldBe` map (Just . Minimum) [1, 4, 8]
      [ L.lookup f (featureAllowances (Proxy :: Proxy DataStoreFeature))
        | f <- ["Maximum number of DataStore Tables", "Maximum total size of DataStore Tables", "DataStore Table size alignment"] ]
        `shouldBe` map (Just . Minimum) [1, 0xA0000, 1]

  describe "--target sim --tests UCT-01,ETC-02,ETC-03,SPF-02:1" $ do
    for_ configurations $ \c@(u, hold) ->
      it ("passes in " <> configured c) $ do
        (code, json, md) <- jig (onSim u hold)
        verdicts json `shouldBe` [ (str k, str "pass") | k <- ["UCT-01", "SPF-02:1", "ETC-02", "ETC-03"] ]
        code `shouldBe` ExitSuccess
        sendStatuses json `shouldSatisfy` elem "synchronous_protocol_violation"
        t45s json `shouldSatisfy` elem "all_returned_no_data"
        md `shouldSatisfy` T.isInfixOf "LockingManufactured (Test Cases 5.2 item 2, PDF 29) waived"
    it "held, answers an IF-RECV (0, 1, 0), and passes both extensions" $ do
      (_, json, _) <- jig (onSim UnitBytes 1)
      t45s json `shouldSatisfy` elem "responses_to_come"
      extensionVerdicts json `shouldBe` [str "pass", str "pass"]
    it "asks for Level 0 once more, at its whole length, when the length asked for cut it short" $ do
      (code, json, _) <- jig (onSim UnitBytes 0) { roTests = keys "UCT-01", roLevel0Len = 100 }
      code `shouldBe` ExitSuccess
      [ n | e <- elements (at "transcript" json), atPath ["request", "comid"] e == Just (A.Number 1)
          , Just (A.Number n) <- [atPath ["request", "len"] e] ]
        `shouldBe` [100, 216]
    it "not held, passes the Empty-atom extension and skips the one that needs a response held" $ do
      (_, json, _) <- jig (onSim UnitBytes 0)
      extensionVerdicts json `shouldBe` [str "pass", str "skipped"]
      t45s json `shouldSatisfy` notElem "responses_to_come"

  describe "--target sim --tests UCT-02,ETC-08,ETC-15 (M4a's cases on the control session)" $ do
    for_ configurations $ \c@(u, hold) ->
      it ("passes in " <> configured c <> ": UCT-02 with both tables observed and its echo extension, ETC-08 and ETC-15 observing (0, 0, 0)") $ do
        (code, json, md) <- jig (onSim u hold) { roTests = keys "UCT-02,ETC-08,ETC-15" }
        verdicts json `shouldBe` [ (str k, str "pass") | k <- ["UCT-02", "ETC-08", "ETC-15"] ]
        code `shouldBe` ExitSuccess
        extensionVerdicts json `shouldBe` [str "pass"]
        observations json "UCT-02" `shouldSatisfy` elem ("TPer MaxComPacketSize", "2048")
        observations json "UCT-02" `shouldSatisfy` elem ("TPer DefSessionTimeout", "0")
        observations json "UCT-02" `shouldSatisfy` elem ("2k", "DefSessionTimeout = 0 (no limit, Core Table 167); the Test Cases' minimum is met")
        observations json "UCT-02" `shouldSatisfy` elem ("host MaxComPacketSize", "4096")
        length (observations json "UCT-02") `shouldSatisfy` (>= 11 + 3)
        for_ ["ETC-08", "ETC-15"] $ \k -> observations json k `shouldSatisfy` any (("(0, 0, 0)" `T.isInfixOf`) . snd)
        observations json "ETC-15" `shouldSatisfy` elem ("step 1", "MaxSubpackets = 1")
        md `shouldSatisfy` T.isInfixOf "- 2k: DefSessionTimeout = 0 (no limit, Core Table 167); the Test Cases' minimum is met"

  describe "--target sim --tests <the ten cases of M4a>" $
    for_ configurations $ \c@(u, hold) ->
      it ("passes in " <> configured c) $ do
        (code, json, _) <- jig (onSim u hold) { roTests = tenCases }
        verdicts json `shouldBe` [ (str k, str "pass") | k <- ["UCT-01", "UCT-02", "UCT-03:1a-1c", "SPF-02:1", "ETC-02", "ETC-03", "ETC-04", "ETC-08", "ETC-15", "ETC-21/adminsp"] ]
        code `shouldBe` ExitSuccess
        extensionVerdicts json `shouldSatisfy` notElem (str "fail")

  describe "UCT-03:1a-1c, ETC-04 and ETC-21/adminsp on the simulator" $ do
    it "UCT-03 passes, its partial-verdict text observed, its extensions passing: NOT_AUTHORIZED, SP_BUSY, the MSID the simulator's" $ do
        (code, json, md) <- jig (onSim UnitBytes 0) { roTests = keys "UCT-03:1a-1c" }
        verdicts json `shouldBe` [(str "UCT-03:1a-1c", str "pass")]
        code `shouldBe` ExitSuccess
        observations json "UCT-03:1a-1c" `shouldSatisfy` elem ("step 1", "1a-1c of step 1; 1d-1f set the SID PIN and are not run")
        extensionVerdicts json `shouldBe` [str "pass", str "pass", str "pass"]
        md `shouldSatisfy` T.isInfixOf "1a-1c of step 1; 1d-1f set the SID PIN and are not run"
        answeredCalls json `shouldSatisfy` elem (str "SyncSession", Just (A.Number 3))
    it "ETC-04 observes (0, 0, 0) after its packet of another Session, and ETC-21/adminsp sees NOT_AUTHORIZED, its Tries extension passing" $ do
        (code, json, _) <- jig (onSim UnitBytes 0) { roTests = keys "ETC-04,ETC-21/adminsp" }
        verdicts json `shouldBe` [(str "ETC-04", str "pass"), (str "ETC-21/adminsp", str "pass")]
        code `shouldBe` ExitSuccess
        observations json "ETC-04" `shouldSatisfy` any (("(0, 0, 0)" `T.isInfixOf`) . snd)
        answeredCalls json `shouldSatisfy` elem (str "SyncSession", Just (A.Number 1))
        extensionVerdicts json `shouldBe` [str "pass"]

  describe "the secrets a run reads (design 9.3 item 5)" $ do
    it "masks the MSID UCT-03 reads in every report, each giving its length and digest; --show-msid shows it" $ do
        let msid = spMsid defaultSimParams
        (code, json, md, xml) <- jigTexts (onSim UnitBytes 0) { roTests = keys "UCT-03:1a-1c" }
        code `shouldBe` ExitSuccess
        for_ [json, md, xml] $ \report -> do
          report `shouldNotSatisfy` T.isInfixOf (hexOf msid)
          report `shouldNotSatisfy` T.isInfixOf (decodeUtf8Lenient msid)
        for_ [md, xml] $ \report -> report `shouldSatisfy` T.isInfixOf "32 bytes, sha256 a6c4ca3911e4"
        json `shouldSatisfy` T.isInfixOf "\"secrets\":[{\"len\":32,\"sha256\":\"a6c4ca3911e4\",\"shown\":false}]"
        (_, shownJson, _, _) <- jigTexts (onSim UnitBytes 0) { roTests = keys "UCT-03:1a-1c", roShowMsid = True }
        shownJson `shouldSatisfy` T.isInfixOf (hexOf msid)
        shownJson `shouldSatisfy` T.isInfixOf "\"shown\":true"
    it "with --sim-sid-pin, UCT-03:1a-1c is skipped (step 2 needs the vendor's SID PIN) and its extensions are not run, each skipped with its reason, no session opened; UCT-01 fails 4c.ii, a set SID PIN leaving the Test Cases' factory-state baseline (TC 4.2, PDF 28); ETC-21/adminsp's SID PIN is the one secret read" $ do
        (code, json, md, _) <- jigTexts (onSim UnitBytes 0) { roTests = keys "UCT-01,UCT-03:1a-1c,ETC-21/adminsp", roSim = defaultSimParams { spSidPin = Just "SID!" } }
        let decoded = either error id (A.eitherDecodeStrict' (encodeUtf8 json))
        verdicts decoded `shouldBe` [(str "UCT-01", str "fail"), (str "UCT-03:1a-1c", str "skipped"), (str "ETC-21/adminsp", str "pass")]
        [ r | c <- elements (at "cases" decoded), at "key" c == str "UCT-01", A.String r <- elements (at "reasons" c) ] `shouldBe` ["4c.ii: SID Value State = 1, not 0"]
        [ r | c <- elements (at "cases" decoded), Just (A.String r) <- [at "reason" c] ] `shouldSatisfy` all ("step 2 needs the vendor's SID PIN" `T.isInfixOf`)
        let uct03Extensions = [ (at "verdict" x, at "reason" x) | c <- elements (at "cases" decoded), at "key" c == str "UCT-03:1a-1c", x <- elements (at "extensions" c) ]
        length uct03Extensions `shouldBe` 3
        uct03Extensions `shouldSatisfy` all (\(v, r) -> v == str "skipped" && case r of
                                                                           Just (A.String t) -> "not run, the case skipped: " `T.isPrefixOf` t && "step 2 needs the vendor's SID PIN" `T.isInfixOf` t
                                                                           _                 -> False)
        answeredCalls decoded `shouldSatisfy` all (\(m, s) -> m /= str "SyncSession" || s /= Just (A.Number 0))
        md `shouldSatisfy` T.isInfixOf "- secrets read: 1,"
        md `shouldSatisfy` T.isInfixOf "Initial C_PIN_SID PIN Indicator 255"
        md `shouldSatisfy` T.isInfixOf "SID Value State 1"
        code `shouldBe` ExitFailure 1
    it "ETC-21/adminsp at TryLimit 3: the case passes, Tries 1 after it, and its Tries extension sees 1 before and 2 after, observed under the extension's name" $ do
        (code, json, _) <- jig (onSim UnitBytes 0) { roTests = keys "ETC-21/adminsp", roSim = defaultSimParams { spSidTryLimit = 3 } }
        verdicts json `shouldBe` [(str "ETC-21/adminsp", str "pass")]
        code `shouldBe` ExitSuccess
        extensionVerdicts json `shouldBe` [str "pass"]
        observations json "ETC-21/adminsp" `shouldSatisfy` elem ("prerequisite", "C_PIN_SID TryLimit 3, Tries 0 (Test Cases ETC-21 prerequisite 1, PDF 92)")
        observations json "ETC-21/adminsp" `shouldSatisfy` any (\(n, v) -> "the Tries rule" `T.isPrefixOf` n && ": Tries" `T.isSuffixOf` n && v == "1 before, 2 after, TryLimit 3")
    it "ETC-21/adminsp with Tries at its TryLimit is skipped, its own prerequisite unmet, the common ones established, its extension skipped with the case's reason" $
        withSystemTempDirectory "hsed-jig" $ \dir -> do
          (sim, control) <- openedInProc defaultSimParams { spSidTryLimit = 1 } (InProcOpts UnitBytes 0)
          channel <- newChannel
          let base = spBaseComId defaultSimParams
              bytes = either (error . show) id (controlComPacket base wrongSidCall)
          failed <- exchangeComPacket defaultExchangeOpts (\q -> Right <$> ifSend sim (proto q) (comId q) (lenBytes q) (payload q)) channel sim base bytes
          failed `shouldSatisfy` isRight
          t <- newTranscript
          code <- silently (runOpened defaultRunOptions { roTests = keys "ETC-21/adminsp", roReportDir = dir </> "report" } t (SimTarget sim control))
          code `shouldBe` ExitFailure 1
          json <- reportIn dir
          verdicts json `shouldBe` [(str "ETC-21/adminsp", str "skipped")]
          [ r | c <- elements (at "cases" json), Just (A.String r) <- [at "reason" c] ] `shouldBe` ["prerequisite unmet: C_PIN_SID's Tries 1 is not below its TryLimit 1"]
          [ (at "verdict" x, at "reason" x) | c <- elements (at "cases" json), x <- elements (at "extensions" c) ]
            `shouldBe` [(str "skipped", str "not run, the case skipped: prerequisite unmet: C_PIN_SID's Tries 1 is not below its TryLimit 1")]
          map (at "prerequisites") (elements (at "cases" json)) `shouldBe` [str "established"]

  describe "a case that stops with a session open" $
    it "leaves the next case runnable: its NoSessions is established by STACK_RESET on the ComID, noted in the transcript" $
        withSystemTempDirectory "hsed-jig" $ \dir -> do
          (sim, control) <- openedInProc defaultSimParams (InProcOpts UnitBytes 0)
          channel <- newChannel
          let base = spBaseComId defaultSimParams
              bytes = either (error . show) id (controlComPacket base startSessionCall)
          opened <- exchangeComPacket defaultExchangeOpts (\q -> Right <$> ifSend sim (proto q) (comId q) (lenBytes q) (payload q)) channel sim base bytes
          opened `shouldSatisfy` isRight
          held <- scPeek control
          Map.size (tsSessions held) `shouldBe` 1
          t <- newTranscript
          code <- silently (runOpened defaultRunOptions { roTests = keys "UCT-02", roReportDir = dir </> "report" } t (SimTarget sim control))
          code `shouldBe` ExitSuccess
          released <- scPeek control
          Map.null (tsSessions released) `shouldBe` True
          json <- reportIn dir
          notes json `shouldSatisfy` any (T.isPrefixOf "NoSessions: STACK_RESET on 0x1004")
          verdicts json `shouldBe` [(str "UCT-02", str "pass")]

  describe "a drive-like response replayed as the drive" $ do
    it "passes UCT-01, skips the others with their reasons, issues no IF-SEND, prints the waivers" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, json, md) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01,ETC-02,ETC-03,SPF-02:1" }
        verdicts json `shouldBe` [ (str "UCT-01", str "pass"), (str "SPF-02:1", str "skipped"), (str "ETC-02", str "skipped"), (str "ETC-03", str "skipped") ]
        [ r | c <- elements (at "cases" json), Just (A.String r) <- [at "reason" c] ] `shouldBe`
          [ "needs IF-RECV beyond Level 0 Discovery: a replay answers Level 0 Discovery alone (Hsed.Transport.Replay)", errorPath, errorPath ]
        code `shouldBe` ExitFailure 1
        at "if_send_issued" json `shouldBe` Just (A.Number 0)
        sendStatuses json `shouldBe` []
        for_ ["SyncIdle (Test Cases 5.2 item 1, PDF 29) waived: " <> level0OutsideSync
             , "NoSessions (Test Cases 5.2 item 6, PDF 29) waived: " <> level0OutsideSync
             , "LockingManufactured (Test Cases 5.2 item 2, PDF 29) waived"] $ \w ->
          md `shouldSatisfy` T.isInfixOf w
        reports <- listDirectory (dir </> "report")
        for_ [ f | f <- reports, takeExtension f == ".json" ] $ \f ->
          silently (verifyTranscript (dir </> "report" </> f) True True) `shouldReturn` ExitSuccess
    it "skips the six cases of M4a too, each for what it needs: UCT-02, UCT-03:1a-1c and ETC-04 as a replay answers Level 0 Discovery alone, whatever the run admits" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
            replayed = "needs IF-SEND: a replay answers Level 0 Discovery alone (Hsed.Transport.Replay)"
        disk6Like >>= writeFileBinary file
        (code, json, _) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-02,UCT-03:1a-1c,ETC-04,ETC-08,ETC-15,ETC-21/adminsp" }
        map snd (verdicts json) `shouldBe` replicate 6 (str "skipped")
        reasonsOf json `shouldBe`
          [ ("UCT-02", [replayed]), ("UCT-03:1a-1c", [replayed]), ("ETC-04", [replayed]), ("ETC-08", [errorPath]), ("ETC-15", [errorPath])
          , ("ETC-21/adminsp", ["the simulator's alone: no drive policy admits it (design 9.3)"]) ]
        code `shouldBe` ExitFailure 1
        at "if_send_issued" json `shouldBe` Just (A.Number 0)
        (at "policies" json, at "blocked" json) `shouldBe` (Just (A.toJSON [A.object ["end" A..= ("drive-replay" :: Text), "policy" A..= ("level0_only" :: Text)]]), num 0)
    it "passes UCT-01 alone: exit 0" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, _, _) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01" }
        code `shouldBe` ExitSuccess
    it "notes a header revision Opal 2.30 Table 2 allows as allowed, in the transcript and the Markdown, and passes UCT-01" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "minor-2.bin"
        bytes <- disk6Like
        writeFileBinary file (B.take 6 bytes <> B.pack [0x00, 0x02] <> B.drop 8 bytes)
        (code, json, md) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01" }
        code `shouldBe` ExitSuccess
        notes json `shouldSatisfy` any (T.isPrefixOf "Level 0 Discovery: allowed (Opal 2.30 Table 2, PDF 19): HeaderDiffers")
        md `shouldSatisfy` T.isInfixOf "- allowed (Opal 2.30 Table 2, PDF 19): HeaderDiffers"
    it "fails UCT-01's step 1c on a response whose Length of Parameter Data asks for more than 255 blocks" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "huge.bin"
        bytes <- disk6Like
        writeFileBinary file (B.pack [0x00, 0x03, 0x0D, 0x40] <> B.drop 4 bytes)
        (code, json, _) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01" }
        code `shouldBe` ExitFailure 1
        [ r | c <- elements (at "cases" json), A.String r <- elements (at "reasons" c) ] `shouldSatisfy` any ("1c:" `T.isPrefixOf`)
    it "says UCT-01's step 4c is not evaluated on a response without the Block SID descriptor, with Table 7's version" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "no-block-sid.bin"
        query <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
        q <- either (throwString . T.unpack) pure (parseQuery query)
        writeFileBinary file (synthesize q { sqFeatures = filter ((/= 0x0402) . sfCode) (sqFeatures q) })
        (code, json, _) <- jigIn dir defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01" }
        code `shouldBe` ExitSuccess
        [ t | t <- notes json, "UCT-01 4c not evaluated" `T.isPrefixOf` t ]
          `shouldSatisfy` any ("TCG Opal SSC Specification v2.00 (Opal 2.30 Table 7, PDF 24)" `T.isInfixOf`)

  describe "a replay standing in for the kext's drive, with what the IORegistry would say of disk6" $
    it "notes the CDB the hardware run expects, issues no IF-SEND, and captures the response with its sidecar" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        bytes <- disk6Like
        let file    = dir </> "disk6-like-level0.bin"
            capture = dir </> "capture" </> "disk6-level0.bin"
            reports = dir </> "report"
        writeFileBinary file bytes
        tr <- openReplay file >>= either (throwString . T.unpack) pure
        t <- newTranscript
        g <- guardDrive t [] (Just (DriveInfo "disk6" "TESTSERIAL0001" "625" IfSAT 0)) (RawDrive tr (\_ -> pure (Left "a replay binds nothing")))
        code <- silently $ runOpened
          defaultRunOptions { roTarget = TReplay file, roTests = keys "UCT-01", roCapture = Just capture, roReportDir = reports }
          t (DriveTarget g)
        code `shouldBe` ExitSuccess
        readFileBinary capture `shouldReturn` bytes
        doesFileExist (dir </> "capture" </> "disk6-level0.json") `shouldReturn` True
        json <- listDirectory reports >>= \fs -> case [ f | f <- fs, takeExtension f == ".json" ] of
          [f] -> readFileBinary (reports </> f) >>= either throwString pure . A.eitherDecodeStrict'
          _   -> throwString "not one report"
        notes json `shouldSatisfy` any ("a1 08 0e 01 04 00 01 00 00 5c 00 00" `T.isInfixOf`)
        at "if_send_issued" json `shouldBe` Just (A.Number 0)

  describe "a drive whose Level 0 Discovery fails" $
    it "is asked once: UCT-01 fails, and neither the pair nor the capture asks it again" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        asked <- newIORef (0 :: Int)
        let failing = MkTransport
              { tExchange = \_ -> Response (TransportFailure "0xe00002bc") mempty <$ modifyIORef' asked (+ 1)
              , tInject   = \_ -> pure (Unsupported "a drive")
              , tUnit     = UnitBlocks512
              , tDescribe = Describe "drive" "a failing test drive" UnitBlocks512 Nothing Nothing Nothing Nothing
              , tClose    = pure ()
              }
            capture = dir </> "capture" </> "disk6-level0.bin"
        (sim, control) <- openedInProc defaultSimParams (InProcOpts UnitBytes 0)
        t <- newTranscript
        g <- guardDrive t [] (Just (DriveInfo "disk6" "TESTSERIAL0001" "625" IfSAT 0)) (RawDrive failing (\_ -> pure (Left "a test drive binds nothing")))
        code <- silently $ runOpened
          defaultRunOptions { roTarget = TPair TSim (TReplay "a failing drive"), roTests = keys "UCT-01"
                            , roCapture = Just capture, roReportDir = dir </> "report" }
          t (PairTarget (SimTarget sim control) (DriveTarget g))
        code `shouldBe` ExitFailure 1
        readIORef asked `shouldReturn` 1
        doesFileExist capture `shouldReturn` False

  describe "the simulator as the drive, behind the Guard as a run opens a drive (Support.fakeDrive, M4c)" $
    it "passes UCT-01, the drive's policy Level0Only throughout and its base ComID never bound, Level 0 Discovery the one command issued" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      binds <- newIORef []
      let counted = raw { rdBind = \b -> modifyIORef' binds (<> [b]) >> rdBind raw b }
      for_ [False, True] $ \reset -> do
        (code, json) <- onFakeDrive defaultRunOptions { roTests = keys "UCT-01", roStackResetBeforeCases = reset } (counted, di, control)
        code `shouldBe` ExitSuccess
        verdicts json `shouldBe` [(str "UCT-01", str "pass")]
        at "policies" json `shouldBe` Just (A.toJSON [A.object ["end" A..= ("drive" :: Text), "policy" A..= ("level0_only" :: Text)]])
        readIORef binds `shouldReturn` ([] :: [Word16])
        issuedOps json `shouldBe` [level0Op]
        (at "blocked" json, at "if_send_issued" json, at "outside_policy" json) `shouldBe` (num 0, num 0, Just (A.toJSON ([] :: [A.Value])))
        [ (w, r) | (_, w, r) <- waiversOf json, w `elem` ["SyncIdle", "NoSessions"] ]
          `shouldBe` [("SyncIdle", level0OutsideSync), ("NoSessions", level0OutsideSync)]

  describe "M4c's acceptance on the simulator as the drive (Support.fakeDrive): --tests UCT-02,UCT-03:1a-1c,SPF-02:1" $ do
    let acceptance reset = defaultRunOptions { roTests = keys "UCT-02,UCT-03:1a-1c,SPF-02:1", roStackResetBeforeCases = reset }
        msid = spMsid defaultSimParams
        noSecretIn report = do
          report `shouldNotSatisfy` T.isInfixOf (hexOf msid)
          report `shouldNotSatisfy` T.isInfixOf (decodeUtf8Lenient msid)
        allPass json = verdicts json `shouldBe` [ (str k, str "pass") | k <- ["UCT-02", "UCT-03:1a-1c", "SPF-02:1"] ]
    it "without STACK_RESET before the cases: exit 0; AllowList 0x1004 of the read-only methods; 13 commands, each IF-SEND followed by its IF-RECV, 4 IF-SENDs, none of protocol 2; SyncIdle probed, NoSessions waived as unobservable" $ do
      drive <- fakeDrive defaultSimParams
      (code, json, md, xml) <- onDriveReports (acceptance False) Nothing drive
      code `shouldBe` ExitSuccess
      allPass json
      at "policies" json `shouldBe` Just (A.toJSON [drivePolicyJSON (AllowList 0x1004 readOnlyMethods)])
      issuedOps json `shouldBe` [ level0Op, recvOp, sendOp, recvOp, recvOp, sendOp, recvOp, sendOp, recvOp, sendOp, recvOp, recvOp, recvOp ]
      sentMethods json `shouldBe` ["Properties", "StartSession", getName, "EndOfSession"]
      (at "if_send_issued" json, at "blocked" json, at "outside_policy" json) `shouldBe` (num 4, num 0, Just (A.toJSON ([] :: [A.Value])))
      waiversOf json `shouldBe`
        [ ("UCT-02", "LockingManufactured", "never touches the Locking SP (design 9.1)")
        , ("UCT-02", "CredentialsKnown", "vacuous: the case opens no session")
        , ("UCT-02", "HostChallengeIsCurrentPin", "vacuous: the case opens no session")
        , ("UCT-02", "ReadWriteSessions", "vacuous: the case opens no session")
        , ("UCT-02", "NoSessions", sessionsUnobservable)
        , ("UCT-03:1a-1c", "LockingManufactured", "never touches the Locking SP (design 9.1)")
        , ("UCT-03:1a-1c", "NoSessions", sessionsUnobservable)
        , ("SPF-02:1", "LockingManufactured", "never touches the Locking SP (design 9.1)")
        , ("SPF-02:1", "CredentialsKnown", "vacuous: the case opens no session")
        , ("SPF-02:1", "HostChallengeIsCurrentPin", "vacuous: the case opens no session")
        , ("SPF-02:1", "ReadWriteSessions", "vacuous: the case opens no session")
        , ("SPF-02:1", "NoSessions", sessionsUnobservable) ]
      for_ [ "CredentialsKnown: vacuous, the case authenticates no authority", "HostChallengeIsCurrentPin: vacuous, the case sends no HostChallenge"
           , "ReadWriteSessions: every session the jig opens is read-write" ] $ \n ->
        notes json `shouldSatisfy` any (n `T.isPrefixOf`)
      notes json `shouldSatisfy` not . any ("STACK_RESET" `T.isInfixOf`)
      json `shouldSatisfy` (== Just (A.toJSON [A.object ["len" A..= (32 :: Int), "sha256" A..= ("a6c4ca3911e4" :: Text), "shown" A..= False]])) . at "secrets"
      for_ [md, xml] $ \report -> report `shouldSatisfy` T.isInfixOf "32 bytes, sha256 a6c4ca3911e4"
      for_ [decodeUtf8Lenient (toStrictBytes (A.encode json)), md, xml] noSecretIn
    it "with STACK_RESET before each case, the default: exit 0; AllowList 0x1004 of the read-only methods and STACK_RESET; 16 commands, 7 IF-SENDs, a STACK_RESET pair before each case, noted; no NoSessions waived" $ do
      drive <- fakeDrive defaultSimParams
      (code, json, md, xml) <- onDriveReports defaultRunOptions { roTests = keys "UCT-02,UCT-03:1a-1c,SPF-02:1" } Nothing drive
      code `shouldBe` ExitSuccess
      allPass json
      at "policies" json `shouldBe` Just (A.toJSON [drivePolicyJSON (AllowList 0x1004 (readOnlyMethods <> [AllowStackReset]))])
      issuedOps json `shouldBe`
        [ level0Op, resetSendOp, resetRecvOp, sendOp, recvOp
        , resetSendOp, resetRecvOp, sendOp, recvOp, sendOp, recvOp, sendOp, recvOp
        , resetSendOp, resetRecvOp, recvOp ]
      sentMethods json `shouldBe` ["Properties", "StartSession", getName, "EndOfSession"]
      (at "if_send_issued" json, at "blocked" json, at "outside_policy" json) `shouldBe` (num 7, num 0, Just (A.toJSON ([] :: [A.Value])))
      [ t | t <- notes json, "SyncIdle and NoSessions: STACK_RESET on 0x1004: " `T.isPrefixOf` t ] `shouldBe` replicate 3
        ("SyncIdle and NoSessions: STACK_RESET on 0x1004: Success/Failure 0: every session on it aborted, its stack in its initial state, its host "
         <> "properties back to their defaults (Core 3.3.4.7.5 items 1, 4, 6 and 7, PDF 65)")
      length [ () | t <- notes json, "NoSessions: met by the same STACK_RESET on 0x1004" `T.isPrefixOf` t ] `shouldBe` 3
      [ k | (k, "NoSessions", _) <- waiversOf json ] `shouldBe` []
      json `shouldSatisfy` (== Just (A.toJSON [A.object ["len" A..= (32 :: Int), "sha256" A..= ("a6c4ca3911e4" :: Text), "shown" A..= False]])) . at "secrets"
      for_ [decodeUtf8Lenient (toStrictBytes (A.encode json)), md, xml] noSecretIn
    it "passes on a drive whose TPer reports MaxComPacketSize 131072, more than the 130,560 bytes a kext command carries: every IF-RECV of protocol 1 on 0x1004 asks for 2048" $
      for_ [False, True] $ \reset -> do
        let large = defaultSimParams { spTPerProperties = [ (n, if n == maxComPacketSizeName then 131072 else v) | (n, v) <- spTPerProperties defaultSimParams ] }
        (code, json) <- fakeDrive large >>= onFakeDrive (acceptance reset)
        code `shouldBe` ExitSuccess
        allPass json
        observations json "UCT-02" `shouldSatisfy` elem ("TPer MaxComPacketSize", "131072")
        let lengths = [ n | e <- elements (at "transcript" json), at "entry" e == str "issued", atPath ["request", "op"] e == str "if_recv"
                          , atPath ["request", "proto"] e == num 1, atPath ["request", "comid"] e == num 4100
                          , Just (A.Number n) <- [atPath ["request", "len"] e] ]
        (length lengths, all (== 2048) lengths) `shouldBe` (if reset then 5 else 8, True)
    it "without STACK_RESET, finds 0x1004 not idle where an exchange was left on it before the run: the probe's answer masked as a secret, the case skipped with --recover's hint; with it, the reset clears the exchange" $ do
      for_ [False, True] $ \reset -> do
        drive@(raw, _, _) <- fakeDrive defaultSimParams
        _ <- tExchange (rdTransport raw) (blockOf (generate (invocation uSMUID (Methods.Properties Nothing))))
        (code, json) <- onFakeDrive (acceptance reset) { roTests = keys "UCT-02" } drive
        if reset
          then (code, verdicts json) `shouldBe` (ExitSuccess, [(str "UCT-02", str "pass")])
          else do
            code `shouldBe` ExitFailure 1
            reasonsOf json `shouldSatisfy` \case
              [("UCT-02", [r])] -> "prerequisite unmet: SyncIdle: ComID 0x1004 answered (" `T.isPrefixOf` r
                                   && "), not idle: an exchange left from before this run; hsed-jig --recover resets it (STACK_RESET, Core 3.3.4.7.5, PDF 65)" `T.isSuffixOf` r
              _                 -> False
            length (elements (at "secrets" json)) `shouldBe` 1
            notes json `shouldSatisfy` any ("SyncIdle: 1 IF-RECV answer(s) on the drive after entry " `T.isPrefixOf`)
    it "without STACK_RESET, takes a probe's (0, 0, 0) in a ComPacket naming another ComID as idle, noted, SPF-02:1 judging the ComID" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      renamed <- newIORef False
      let inner = rdTransport raw
          idle bytes = B.take 4 bytes <> B.pack [0, 0] <> B.drop 6 bytes
          zeroComId = raw { rdTransport = inner { tExchange = \q -> do
            r <- tExchange inner q
            first' <- if Wire.dir q == IfRecv && proto q == 1 && comId q == 0x1004 then atomicModifyIORef' renamed (\was -> (True, not was)) else pure False
            pure (if first' then r { rpayload = idle (rpayload r) } else r) } }
      (code, json) <- onFakeDrive (acceptance False) { roTests = keys "UCT-02" } (zeroComId, di, control)
      (code, verdicts json) `shouldBe` (ExitSuccess, [(str "UCT-02", str "pass")])
      notes json `shouldSatisfy` elem "SyncIdle: ComID 0x1004 answered (0, 0, 0), nothing to return, in a ComPacket of ComID 0x0000, not 0x1004: idle; SPF-02:1 judges the ComID (Core 3.3.10.5 S1 b, PDF 84)"
    it "skips ETC-04 selected with UCT-02, a case off design 9.3's drive list, its needs met: exit 1" $ do
      (code, json) <- fakeDrive defaultSimParams >>= onFakeDrive defaultRunOptions { roTests = keys "UCT-02,ETC-04" }
      verdicts json `shouldBe` [(str "UCT-02", str "pass"), (str "ETC-04", str "skipped")]
      L.lookup "ETC-04" (reasonsOf json) `shouldBe` Just ["not among the cases a drive runs at M4c (design 9.3: UCT-01, UCT-02, UCT-03:1a-1c, SPF-02:1)"]
      code `shouldBe` ExitFailure 1
    it "skips ETC-04 selected alone, the drive admitting Level 0 Discovery alone (no case selected from the list needs more), never bound: exit 1" $ do
      (code, json) <- fakeDrive defaultSimParams >>= onFakeDrive defaultRunOptions { roTests = keys "ETC-04" }
      reasonsOf json `shouldBe` [("ETC-04", ["needs IF-SEND: this run admits Level 0 Discovery alone on the drive: no case it selected from design 9.3's drive list needs more"])]
      at "policies" json `shouldBe` Just (A.toJSON [A.object ["end" A..= ("drive" :: Text), "policy" A..= ("level0_only" :: Text)]])
      issuedOps json `shouldBe` []
      code `shouldBe` ExitFailure 1
    it "fails a run whose drive the Guard refused a command: UCT-02's Properties off its allow list, Blocked, a problem of the run, exit 1" $ do
      drive <- fakeDrive defaultSimParams
      (code, json, _, _) <- onDriveReports defaultRunOptions { roTests = keys "UCT-02" } (Just [AllowEndOfSession]) drive
      code `shouldBe` ExitFailure 1
      verdicts json `shouldBe` [(str "UCT-02", str "fail")]
      (at "blocked" json, at "outside_policy" json) `shouldBe` (num 1, Just (A.toJSON ([] :: [A.Value])))
      at "problems" json `shouldBe` Just (A.toJSON ["the Guard refused 1 command(s) (each Blocked in the transcript)" :: Text])

  describe "--interrupt-in-session on the simulator as the drive (M4c)" $ do
    let uct03 reset = defaultRunOptions { roTests = keys "UCT-03:1a-1c", roStackResetBeforeCases = reset }
        interrupted json = [ t | Just (A.String t) <- [at "interrupted" json] ]
    for_ [False, True] $ \reset ->
      it ("stops the run once StartSession has opened a session" <> (if reset then ", with STACK_RESET before each case" else "")
          <> ": exit 130, the session left open on 0x1004, no End of Session, the case skipped as interrupted, the reports written") $ do
        drive@(_, _, control) <- fakeDrive defaultSimParams
        (code, json) <- onFakeDrive (uct03 reset) { roInterruptInSession = True } drive
        code `shouldBe` ExitFailure 130
        interrupted json `shouldSatisfy` \case
          [t] -> all (`T.isInfixOf` t) [ "--interrupt-in-session: stopped after StartSession opened TSN ", " on 0x1004; the session is left open on purpose"
                                        , "; session TSN ", " on ComID 0x1004 left open (End of Session not sent): hsed-jig --recover" ]
          _   -> False
        (lifeOf 0x1004 <$> scPeek control) `shouldReturn` Just Associated
        sentMethods json `shouldBe` ["StartSession"]
        reasonsOf json `shouldSatisfy` \case
          [("UCT-03:1a-1c", [r])] -> "interrupted: --interrupt-in-session: stopped after StartSession opened TSN " `T.isPrefixOf` r
          _                       -> False
        at "exit" json `shouldBe` num 130
    it "leaves the next run without STACK_RESET to fail 1a with SP_BUSY and --recover's hint: exit 1" $ do
      drive@(_, _, control) <- fakeDrive defaultSimParams
      (stopped, _) <- onFakeDrive (uct03 False) { roInterruptInSession = True } drive
      stopped `shouldBe` ExitFailure 130
      (code, json) <- onFakeDrive (uct03 False) drive
      code `shouldBe` ExitFailure 1
      reasonsOf json `shouldBe`
        [("UCT-03:1a-1c", ["1a: StartSession to the Admin SP: SyncSession answered SP_BUSY (3), not SUCCESS (0); a read-write session to the Admin SP "
                           <> "was open before this one (Core 5.1.5.3, PDF 138): hsed-jig --recover clears it (Core 3.3.4.7.5 item 1, PDF 65)"])]
      (lifeOf 0x1004 <$> scPeek control) `shouldReturn` Just Associated
    it "leaves the next run with STACK_RESET before each case to clear the session itself: exit 0" $ do
      drive@(_, _, control) <- fakeDrive defaultSimParams
      (stopped, _) <- onFakeDrive (uct03 True) { roInterruptInSession = True } drive
      stopped `shouldBe` ExitFailure 130
      (code, json) <- onFakeDrive (uct03 True) drive
      code `shouldBe` ExitSuccess
      verdicts json `shouldBe` [(str "UCT-03:1a-1c", str "pass")]
      (lifeOf 0x1004 <$> scPeek control) `shouldNotReturn` Just Associated

  describe "the stop signals (M4c, design 9.3)" $ do
    it "caught for the run's length, SIGINT, SIGTERM or SIGHUP writes its name into the run's stop flag, and says so, the command in flight left to complete" $ do
      stop <- newIORef Nothing
      let settled n = readIORef stop >>= \was -> if isJust was || n <= (0 :: Int) then pure was else threadDelay 10000 >> settled (n - 1)
      silently (withStopSignals stop (raiseSignal sigHUP >> settled 200)) `shouldReturn` Just "SIGHUP"
    it "puts back the handlers it found after the run: a Haskell handler of SIGHUP runs again, and base's own SIGINT handler is in base's table again (the lead, the M4c review)" $ do
      stop <- newIORef Nothing
      seen <- newIORef False
      -- the entry base's signal table holds for a signal, read by swapping it out and back
      let entryType sig = do
            entry <- Conc.setHandler sig Nothing
            _ <- Conc.setHandler sig entry
            pure (dynTypeRep . snd <$> entry)
          settled n = readIORef seen >>= \was -> if was || n <= (0 :: Int) then pure was else threadDelay 10000 >> settled (n - 1)
      bracket (installHandler sigHUP (Catch (writeIORef seen True)) Nothing) (\old -> installHandler sigHUP old Nothing) $ \_ -> do
        intBefore <- entryType sigINT
        withStopSignals stop (pure ())
        intAfter <- entryType sigINT
        (intAfter, isJust intBefore) `shouldBe` (intBefore, True)
        raiseSignal sigHUP
        settled 200 `shouldReturn` True
        readIORef stop `shouldReturn` Nothing
    it "puts back a signal's action reset once delivered, as base's own SIGINT handler's is, so that a second signal after the run kills: a SIGHUP set up so is reset once after the run, as sigaction reports it (Linux) or as one SIGHUP delivered after the run shows by the default action it leaves (macOS) (the lead's (5); the M4c review's verification)" $ do
      stop <- newIORef Nothing
      let once = pure () :: IO ()
          -- SIGHUP's action as sigaction reads it, once it is the one wanted or after 2 s
          settledTo want n = signalResets sigHUP >>= \r -> if r == want || n <= (0 :: Int) then pure r else threadDelay 10000 >> settledTo want (n - 1)
      intBefore <- signalResets sigINT
      bracket (installHandler sigHUP (CatchOnce once) Nothing) (\old -> installHandler sigHUP old Nothing) $ \_ -> do
        -- the action reset once delivered (STG_SIG_RST), the table's entry an IO (): as GHC.TopHandler sets up SIGINT
        _ <- Conc.setHandler sigHUP (Just (const once, toDyn once))
        withStopSignals stop (pure ())
        -- reset once, as the platform shows it: Linux's sigaction reports SA_RESETHAND (1); macOS's does not (0, as for a
        -- repeating handler), so there one SIGHUP is delivered, which the handler put back takes, and with it its action:
        -- the default left behind, where a handler put back as a repeating one stays. (On CI's Linux runner the action
        -- read 1 and stayed so for 2 s after a SIGHUP was raised: the delivery is macOS's check alone.)
        signalResets sigHUP >>= \case
          1 -> pure ()
          r -> do
            r `shouldBe` 0
            raiseSignal sigHUP
            settledTo (-1) 200 `shouldReturn` (-1)
        signalResets sigINT `shouldReturn` intBefore
      readIORef stop `shouldReturn` Nothing

  describe "the exit code, and what a drive's own transcript holds outside its policy (M4c)" $ do
    it "outsidePolicy decides each command again: a forged Set on 0x1004 and an IF-RECV on 0x1005 are outside AllowList 0x1004 of the read-only methods, a run's own commands are not; the exit from them is 3" $ do
      let answer = Response Good mempty
          setPin = blockOf (generate (invocation uC_PIN_SID (Methods.Set Nothing (Just (L (List [N ((3 :: Natural) .: ("forged" :: ByteString))]))))))
          recvElsewhere = Request IfRecv 1 0x1005 2048 mempty
          own = [ Request IfRecv 1 1 2048 mempty, blockOf (generate (invocation uSMUID (Methods.Properties Nothing))), Request IfRecv 1 0x1004 2048 mempty ]
          forged = [ Issued n "drive" q answer | (n, q) <- zip [1 ..] (own <> [setPin, recvElsewhere]) ]
                   <> [Blocked 6 "drive" setPin "refused", Note 7 "a note"]
          policy = AllowList 0x1004 readOnlyMethods
      outsidePolicy policy forged `shouldBe` [setPin, recvElsewhere]
      outsidePolicy policy (take 3 forged) `shouldBe` []
      runExit [ ("drive", q) | q <- outsidePolicy policy forged ] Nothing False `shouldBe` ExitFailure 3
      runExit [ ("drive", q) | q <- outsidePolicy policy forged ] (Just "SIGINT") True `shouldBe` ExitFailure 3
    it "is 130 for a run stopped, outranking a failure; 1 for a failure; else 0" $ do
      runExit [] (Just "SIGINT") True `shouldBe` ExitFailure 130
      runExit [] Nothing True `shouldBe` ExitFailure 1
      runExit [] Nothing False `shouldBe` ExitSuccess

  describe "UCT-03's MSID read answered with what does not read as the MSID (M4c, design 9.3 item 5)" $
    it "masks that answer in every report, a known byte string in it, and the step's reason says so" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      forging <- newIORef False
      let known = "A-KNOWN-BYTE-STRING-NOT-THE-PIN" :: ByteString
          inner = rdTransport raw
          forged = raw { rdTransport = inner { tExchange = \q -> do
            r <- tExchange inner q
            when (Wire.dir q == IfSend) (writeIORef forging (getsMsid q))
            readIORef forging >>= \case
              True | Wire.dir q == IfRecv, proto q == 1 -> writeIORef forging False $> r { rpayload = answerHolding known r (fromIntegral (lenBytes q)) }
              _                                    -> pure r } }
      (code, json, md, xml) <- onDriveReports defaultRunOptions { roTests = keys "UCT-03:1a-1c" } Nothing (forged, di, control)
      code `shouldBe` ExitFailure 1
      reasonsOf json `shouldBe`
        [("UCT-03:1a-1c", ["1b: Get C_PIN_MSID's PIN: no PIN (column 3, a byte string) among the result's 1 cells; any answer to it masked in the "
                           <> "reports as a secret (design 9.3 item 5)"])]
      notes json `shouldSatisfy` any ("1b: Get C_PIN_MSID's PIN: 1 IF-RECV answer(s) on the drive after entry " `T.isPrefixOf`)
      length (elements (at "secrets" json)) `shouldBe` 1
      for_ [decodeUtf8Lenient (toStrictBytes (A.encode json)), md, xml] $ \report -> do
        report `shouldNotSatisfy` T.isInfixOf (hexOf known)
        report `shouldNotSatisfy` T.isInfixOf (decodeUtf8Lenient known)

  describe "a StartSession whose SyncSession the host could not read (the M4c review; Core 3.3.4.5, PDF 62)" $ do
    -- the drive's answer to StartSession with SyncSession's MethodID (..FF03) made another (..FF7F)
    let garbling raw = do
          answering <- newIORef False
          let inner = rdTransport raw
              syncSession = B.pack [0xA8, 0, 0, 0, 0, 0, 0, 0xFF, 0x03]
          pure raw { rdTransport = inner { tExchange = \q -> do
            r <- tExchange inner q
            when (Wire.dir q == IfSend) (writeIORef answering (B.isInfixOf startSessionUid (payload q)))
            readIORef answering >>= \case
              True | Wire.dir q == IfRecv, proto q == 1 ->
                let (ahead, rest) = B.breakSubstring syncSession (rpayload r)
                in writeIORef answering False $> if B.null rest then r
                                                  else r { rpayload = ahead <> B.pack [0xA8, 0, 0, 0, 0, 0, 0, 0xFF, 0x7F] <> B.drop 9 rest }
              _ -> pure r } }
    it "with STACK_RESET before each case: the case fails at 1a, STACK_RESET clears the session the TPer opened, noted: exit 1, nothing open" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      garbled <- garbling raw
      (code, json, _, _) <- onDriveReports defaultRunOptions { roTests = keys "UCT-03:1a-1c" } Nothing (garbled, di, control)
      code `shouldBe` ExitFailure 1
      reasonsOf json `shouldSatisfy` \case
        [("UCT-03:1a-1c", [r])] -> "1a: StartSession to the Admin SP: no SyncSession" `T.isPrefixOf` r
        _                       -> False
      notes json `shouldSatisfy` any (\t -> "StartSession on 0x1004: no SyncSession read (" `T.isPrefixOf` t && "STACK_RESET on 0x1004: Success/Failure 0" `T.isSuffixOf` t)
      issuedOps json `shouldBe` [level0Op, resetSendOp, resetRecvOp, sendOp, recvOp, resetSendOp, resetRecvOp]
      (lifeOf 0x1004 <$> scPeek control) `shouldNotReturn` Just Associated
      [ p | A.String p <- elements (at "problems" json) ] `shouldBe` []
    it "without STACK_RESET: the session kept, the next case's NoSessions unmet, the reports naming it for --recover: exit 1" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      garbled <- garbling raw
      (code, json, _, _) <- onDriveReports defaultRunOptions { roTests = keys "UCT-03:1a-1c,SPF-02:1", roStackResetBeforeCases = False }
                                           Nothing (garbled, di, control)
      code `shouldBe` ExitFailure 1
      reasonsOf json `shouldSatisfy` \case
        [("UCT-03:1a-1c", [_]), ("SPF-02:1", [r])] ->
          "prerequisite unmet: NoSessions: this run may have left a session open on 0x1004, a StartSession's answer unread (" `T.isPrefixOf` r
        _ -> False
      [ p | A.String p <- elements (at "problems" json) ]
        `shouldSatisfy` any (\p -> "the drive may hold a session on ComID 0x1004 that a StartSession opened" `T.isPrefixOf` p
                                   && "hsed-jig --recover" `T.isSuffixOf` p)
      (lifeOf 0x1004 <$> scPeek control) `shouldReturn` Just Associated
    -- the drive's first answer to StartSession made a ComPacket of no packets asking a MinTransfer of 200,000 bytes, more
    -- than a drive's transport carries (255 blocks, 130,560 bytes): the IF-SEND taken, the rest of the answer not collected
    -- (Core Table 45 row 2, PDF 83; the M4c review's verification)
    let asking raw = do
          answering <- newIORef False
          let inner = rdTransport raw
          pure raw { rdTransport = inner { tExchange = \q -> do
            r <- tExchange inner q
            when (Wire.dir q == IfSend) (writeIORef answering (B.isInfixOf startSessionUid (payload q)))
            readIORef answering >>= \case
              True | Wire.dir q == IfRecv, proto q == 1 -> writeIORef answering False $> r { rpayload = emptyComPacket 200000 200000 (lenBytes q) }
              _                                         -> pure r } }
        uncollected = "an answer not collected: MinTransfer 200000 asks a transfer of 200192 bytes, which the transport cannot carry"
    it "with STACK_RESET before each case, an answer asking a MinTransfer the transport cannot carry: the case fails at 1a, STACK_RESET clears the session, noted: exit 1, nothing open" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      asked <- asking raw
      (code, json, _, _) <- onDriveReports defaultRunOptions { roTests = keys "UCT-03:1a-1c" } Nothing (asked, di, control)
      code `shouldBe` ExitFailure 1
      reasonsOf json `shouldBe` [("UCT-03:1a-1c", ["1a: StartSession to the Admin SP: " <> uncollected])]
      notes json `shouldSatisfy` elem ("StartSession on 0x1004: no SyncSession read (" <> uncollected <> "), so a session it may have opened "
                                       <> "is not known: STACK_RESET on 0x1004: Success/Failure 0")
      issuedOps json `shouldBe` [level0Op, resetSendOp, resetRecvOp, sendOp, recvOp, resetSendOp, resetRecvOp]
      (lifeOf 0x1004 <$> scPeek control) `shouldNotReturn` Just Associated
      [ p | A.String p <- elements (at "problems" json) ] `shouldBe` []
    it "without STACK_RESET, the same answer: the session kept, the next case's NoSessions unmet, the reports naming it for --recover: exit 1" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      asked <- asking raw
      (code, json, _, _) <- onDriveReports defaultRunOptions { roTests = keys "UCT-03:1a-1c,SPF-02:1", roStackResetBeforeCases = False }
                                           Nothing (asked, di, control)
      code `shouldBe` ExitFailure 1
      reasonsOf json `shouldBe`
        [ ("UCT-03:1a-1c", ["1a: StartSession to the Admin SP: " <> uncollected])
        , ("SPF-02:1", ["prerequisite unmet: NoSessions: this run may have left a session open on 0x1004, a StartSession's answer unread ("
                        <> uncollected <> "): hsed-jig --recover"]) ]
      [ p | A.String p <- elements (at "problems" json) ]
        `shouldBe` ["the drive may hold a session on ComID 0x1004 that a StartSession opened and the host could not read (" <> uncollected
                    <> "), which no STACK_RESET has cleared: hsed-jig --recover"]
      (lifeOf 0x1004 <$> scPeek control) `shouldReturn` Just Associated

  describe "a session whose End of Session goes unanswered, and whose closing STACK_RESET answers Success/Failure 1, in the run's last case (the M4c review's verification)" $
    it "keeps the session for the reports, which name it with --recover's hint, the failed reset noted: exit 1" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      stage <- newIORef (0 :: Int)
      let inner = rdTransport raw
          -- after the host's End of Session, its IF-RECV answered with nothing (Table 45's (0, 0, 0), PDF 83), then the
          -- closing STACK_RESET's GET_COMID_RESPONSE answered Success/Failure 1 (Core Table 36, PDF 66)
          closing = raw { rdTransport = inner { tExchange = \q -> do
            r <- tExchange inner q
            at' <- readIORef stage
            let edited
                  | Wire.dir q == IfSend, proto q == 1, endsSession (payload q) = writeIORef stage 1 $> r
                  | at' == 1, Wire.dir q == IfRecv, proto q == 1 = writeIORef stage 2 $> r { rpayload = emptyComPacket 0 0 (lenBytes q) }
                  | at' == 2, Wire.dir q == IfRecv, proto q == 2 = writeIORef stage 3 $> r { rpayload = resetFailing (rpayload r) }
                  | otherwise = pure r
            edited } }
      (code, json, _, _) <- onDriveReports defaultRunOptions { roTests = keys "UCT-03:1a-1c" } Nothing (closing, di, control)
      code `shouldBe` ExitFailure 1
      readIORef stage `shouldReturn` 3
      reasonsOf json `shouldBe` [("UCT-03:1a-1c", ["1c: End of Session: the TPer did not answer the End of Session with its own (Core 3.2.2.3.3.3, PDF 37)"])]
      notes json `shouldSatisfy` elem "1c: End of Session: after a failure, STACK_RESET on 0x1004: Success/Failure 1"
      [ p | A.String p <- elements (at "problems" json) ] `shouldSatisfy` \case
        [p] -> "the drive may hold session TSN " `T.isPrefixOf` p
               && " on ComID 0x1004, its End of Session unanswered or not sent, and no STACK_RESET answering Success since: hsed-jig --recover" `T.isSuffixOf` p
        _   -> False

  describe "STACK_RESET before a case that does not reset (the M4c review; Core Tables 36-37, PDF 66)" $ do
    -- the drive's GET_COMID_RESPONSE answers edited
    let answering edit raw = raw { rdTransport = (rdTransport raw) { tExchange = \q -> do
          r <- tExchange (rdTransport raw) q
          pure (if Wire.dir q == IfRecv && proto q == 2 then r { rpayload = edit (rpayload r) } else r) } }
        failing = resetFailing
        stillPending = const (either (const "") padded (encodeRecord (STACK_RESETPending (extendedComId 0x1004))))
    it "answered Success/Failure 1: the case's SyncIdle and NoSessions unmet, the case skipped, exit 1" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      (code, json, _, _) <- onDriveReports defaultRunOptions { roTests = keys "UCT-02" } Nothing (answering failing raw, di, control)
      code `shouldBe` ExitFailure 1
      reasonsOf json `shouldBe`
        [("UCT-02", ["prerequisite unmet: SyncIdle and NoSessions: STACK_RESET on 0x1004: Success/Failure 1: the stack not reset (Core Table 36, PDF 66)"])]
    it "answered Pending for ever: as many polls as the host's timeout holds, then unmet, the case skipped, exit 1" $ do
      (raw, di, control) <- fakeDrive defaultSimParams
      let quick = defaultExchangeOpts { xPollMicros = 1000, xTimeoutMicros = 20000 }
      (code, json, _, _) <- onDriveReports defaultRunOptions { roTests = keys "UCT-02", roExchange = quick } Nothing (answering stillPending raw, di, control)
      code `shouldBe` ExitFailure 1
      reasonsOf json `shouldSatisfy` \case
        [("UCT-02", [r])] -> "prerequisite unmet: SyncIdle and NoSessions: STACK_RESET on 0x1004: STACK_RESET on 0x1004 still pending" `T.isPrefixOf` r
                             && "after 20 polls" `T.isSuffixOf` r
        _                 -> False
      length (filter (== resetRecvOp) (issuedOps json)) `shouldBe` 21

  describe "--interrupt-in-session before a later case and a capture (the M4c review)" $
    it "stops the run in UCT-03: SPF-02:1 not run, no capture written, no Level 0 after the cases: exit 130" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        drive <- fakeDrive defaultSimParams
        let capture = dir </> "capture.bin"
        (code, json) <- onFakeDrive defaultRunOptions { roTests = keys "UCT-03:1a-1c,SPF-02:1", roInterruptInSession = True, roCapture = Just capture } drive
        code `shouldBe` ExitFailure 130
        reasonsOf json `shouldSatisfy` \case
          [("UCT-03:1a-1c", [r]), ("SPF-02:1", [r'])] -> "interrupted: --interrupt-in-session" `T.isPrefixOf` r
                                                         && "not run: the run was interrupted (--interrupt-in-session" `T.isPrefixOf` r'
          _ -> False
        doesFileExist capture `shouldReturn` False
        length (filter (== level0Op) (issuedOps json)) `shouldBe` 1

  describe "a stop during a case's extension, on the simulator (the M4c review)" $ do
    it "keeps the case's TCG verdict, skips that extension as interrupted and the cases after it as not run: exit 130" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        (sim, control) <- openedInProc defaultSimParams (InProcOpts UnitBytes 0)
        stop <- newIORef Nothing
        -- the run stopped as a signal would, while SPF-02:1's extension sends its IF-SEND (its case sends none)
        let stopping = sim { tExchange = \q -> when (Wire.dir q == IfSend) (writeIORef stop (Just "SIGINT")) >> tExchange sim q }
        t <- newTranscript
        code <- silently (runOpenedStopping defaultRunOptions { roTests = keys "SPF-02:1,ETC-04", roReportDir = dir </> "report" } t stop
                                            (SimTarget stopping control))
        json <- reportIn dir
        code `shouldBe` ExitFailure 130
        verdicts json `shouldBe` [(str "SPF-02:1", str "pass"), (str "ETC-04", str "skipped")]
        [ (at "verdict" x, at "reason" x) | c <- take 1 (elements (at "cases" json)), x <- elements (at "extensions" c) ]
          `shouldBe` [(str "skipped", str "interrupted: SIGINT")]
        reasonsOf json `shouldSatisfy` elem ("ETC-04", ["not run: the run was interrupted (SIGINT)"])
    it "stopped in the first of UCT-03's three extensions: that one interrupted, the two after it not run, the run having been interrupted; the case's verdict kept: exit 130 (the M4c review's verification)" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        (sim, control) <- openedInProc defaultSimParams (InProcOpts UnitBytes 0)
        stop <- newIORef Nothing
        starts <- newIORef (0 :: Int)
        -- the run stopped as a signal would, while the first extension's StartSession is sent (the case's own is the first)
        let stopping = sim { tExchange = \q -> do
              when (Wire.dir q == IfSend && B.isInfixOf startSessionUid (payload q)) $ do
                n <- atomicModifyIORef' starts (\k -> (k + 1, k + 1))
                when (n == 2) (writeIORef stop (Just "SIGINT"))
              tExchange sim q }
        t <- newTranscript
        code <- silently (runOpenedStopping defaultRunOptions { roTests = keys "UCT-03:1a-1c", roReportDir = dir </> "report" } t stop
                                            (SimTarget stopping control))
        json <- reportIn dir
        code `shouldBe` ExitFailure 130
        verdicts json `shouldBe` [(str "UCT-03:1a-1c", str "pass")]
        [ (at "verdict" x, at "reason" x) | c <- elements (at "cases" json), x <- elements (at "extensions" c) ]
          `shouldBe` [ (str "skipped", str "interrupted: SIGINT")
                     , (str "skipped", str "not run: the run was interrupted (SIGINT)")
                     , (str "skipped", str "not run: the run was interrupted (SIGINT)") ]
        readIORef starts `shouldReturn` 2

  describe "the simulator paired with the replay (the Pair runner)" $ do
    it "finds 0x0403 missing without a profile: exit 1" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, json, _) <- jigIn dir defaultRunOptions { roTarget = TPair TSim (TReplay file), roTests = keys "UCT-01" }
        atPath ["pair", "missing"] json `shouldBe` Just (A.toJSON [0x0403 :: Int])
        code `shouldBe` ExitFailure 1
        [ at "end" e | c <- elements (at "cases" json), e <- elements (at "ends" c) ] `shouldBe` [str "tper", str "drive-replay"]
        [ at "end" l | l <- elements (at "level0" json) ] `shouldBe` [str "tper", str "drive-replay"]
        L.nub [ at "end" e | e <- elements (at "transcript" json), at "entry" e == str "issued" ] `shouldBe` [str "tper", str "drive-replay"]
    it "with it as the profile, finds extra 0x0005 and 0x0404, only differences allowed: exit 0" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let file = dir </> "disk6-like-level0.bin"
        disk6Like >>= writeFileBinary file
        (code, json, _) <- jigIn dir defaultRunOptions
          { roTarget = TPair TSim (TReplay file), roTests = keys "UCT-01", roProfile = Just file }
        (atPath ["pair", "extra"] json, atPath ["pair", "missing"] json, atPath ["pair", "differences"] json)
          `shouldBe` (Just (A.toJSON [0x0005, 0x0404 :: Int]), Just (A.toJSON ([] :: [Int])), Just (A.toJSON ([] :: [Int])))
        code `shouldBe` ExitSuccess
        L.length (elements (atPath ["pair", "allowed"] json)) `shouldSatisfy` (> 0)
