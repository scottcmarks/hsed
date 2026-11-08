{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.WsTargetSpec
Description : A run over ws://127.0.0.1:<port>/ws against the same run in process: whole transcripts equal
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The jig's ws:// target (design 11 M5c, group G3's acceptance). A hub in the suite's own
threads ("Hsed.Lan.Support".'withHub': 127.0.0.1 and an ephemeral port, control and reset
allowed, its backend from the run's parameters and hold) serves a run over
@ws:\/\/127.0.0.1:PORT\/ws@ beside the same run in process, both with the host's polls
shortened to 1 ms: at configurations 1, 2 (@--hold-processing@), 1m and 2m (@--locking-ofs
manufactured@), M4a's ten cases with ETC-20 and with M5b's seven, the whole transcripts are
equal entry for entry, notes and numbers included, and the JSON reports equal but for the
start, the target, the report directory and what the first target says was opened. The reset
clause: at manufactured SPF-12:1/no-media's power cycle goes over the hub's reset op and
leaves ReadLocked and WriteLocked TRUE, a probe on the hub receiving one @sed.evt@,
@power_cycle@, its @seq@ the probe's hello's plus 1. The open's refusals, each exiting 1
naming its reason: a hub without @--allow-remote-control@, a hub whose units, hold, Original
Factory State, inactive ComIDs' discard, SID PIN or profile are not the run's; and a hub
without @--allow-remote-reset@ answers the power cycle unsupported, which fails SPF-12's
extension. What a hub's @app.sed@ is held to ("Hsed.Jig.Target".'hubRefusals') is checked
here first, with no hub.

A hub restarted under a run (hub B started on hub A's port, as
"Hsed.Transport.Mhtx.RestartSpec" restarts one under the client alone): the case in flight
(ETC-04, its call in the session it opened held by hub A's backend) fails with a transport
failure when A stops, the request never sent again, B's backend never seeing it; the request
issued while no hub listens is refused, naming the restart; the restart is noted before the
next case, the run's session forgotten as a power cycle forgets it ("Hsed.Jig.Script".
'Hsed.Jig.Script.heedNotices'); and the later cases pass on B's TPer.
-}
module Hsed.Jig.WsTargetSpec (spec) where

import qualified Data.Aeson                as A
import qualified Data.Aeson.Key            as K
import qualified Data.Aeson.KeyMap         as KM
import qualified RIO.ByteString            as B
import           GHC.IO.Handle             (hDuplicate, hDuplicateTo)
import           RIO
import           RIO.Directory             (listDirectory)
import           RIO.FilePath              (takeExtension, (</>))
import qualified RIO.Text                  as T

import           Test.Hspec

import           Hsed.CaseKey              (CaseKey, parseCaseKeys)
import           Hsed.Jig.Options          (RunOptions (..), TargetSpec (..), WsUrl (..), defaultRunOptions)
import           Hsed.Jig.Run              (runJig)
import           Hsed.Jig.Target           (hubRefusals)
import           Hsed.Lan.Support          (probe, withHub, withHubOn)
import           Hsed.Sha256               (sha256Hex)
import           Hsed.Sim.Profile          (loadProfile)
import           Hsed.Sim.Types            (LifeCycle (..), SimParams (..), defaultSimParams)
import           Hsed.Sim.View             (setupOf)
import           Hsed.Transport.InProc     (InProcOpts (..))
import           Hsed.Transport.Mhtx.Conn  (Conn (..), Incoming (..))
import           Hsed.Transport.Mhtx.Frame (Allow (..), FromServer (..), Hello (..), SedApp (..), SedEvt (..), fromServer)
import           Hsed.Transport.Mhtx.Hub   (Backend (..), HubOpts (..), backendOf, defaultHubOpts)
import           Hsed.Wire                 (Event (..), ExchangeOpts (..), Unit (..), defaultExchangeOpts)
import           Hsed.Wire.Internal        (Dir (..), Request (payload), Transport (..))
import qualified Hsed.Wire.Internal        as W


spec :: Spec
spec = describe "Hsed.Jig.Target, a ws:// target" $ do
  describe "a hub's app.sed held to the run (hubRefusals)" $ do
    it "serves a run whose simulator flags and hold are the hub's setup, a hub that allows no reset among them" $ do
      hubRefusals defaultRunOptions served `shouldBe` []
      hubRefusals defaultRunOptions served { saAllows = [AllowPeek, AllowRelease] } `shouldBe` []
    it "refuses a hub that allows no peek, and one that publishes no setup, each saying why" $ do
      hubRefusals defaultRunOptions served { saAllows = [AllowReset] }
        `shouldBe` [ "the hub allows no peek: hsed-sim --allow-remote-control, which the run's reads of the simulator take (design 11 M5c, "
                     <> "Option A)" ]
      hubRefusals defaultRunOptions served { saSetup = Nothing }
        `shouldBe` ["the hub publishes no setup (app.sed.setup): the run's simulator flags cannot be held to it"]
    it "refuses a hub whose units, hold, Locking SP OFS, inactive ComIDs' discard or SID PIN is not the run's, each named by its flag" $
      [ hubRefusals defaultRunOptions { roSim = p, roHold = h } served | (p, h) <- otherwise' ] `shouldBe`
        [ ["the hub's setup differs: --units: blocks512 asked, bytes on the hub"]
        , ["the hub's setup differs: --hold-ifrecvs: 1 asked, 0 on the hub"]
        , ["the hub's setup differs: --locking-ofs: manufactured asked, manufactured-inactive on the hub"]
        , ["the hub's setup differs: --inactive-comid-discard: on asked, off on the hub"]
        , ["the hub's setup differs: --sim-sid-pin: " <> sidPinNamed <> " asked, none (the MSID's) on the hub"]
        ]

  describe "a run over ws://127.0.0.1:<port>/ws against the same run in process (roExchange's 1 ms polls)" $ do
    it "at configuration 1 (M4a's ten and ETC-20): whole transcripts equal, the reports equal but for the start, the target, the report directory and opened" $
      sameOverWs defaultSimParams 0 (oldAnd "ETC-20")
    it "at configuration 2 (--hold-processing): the same" $
      sameOverWs defaultSimParams 1 (oldAnd "ETC-20")
    it "at configuration 1m (--locking-ofs manufactured, M4a's ten and M5b's seven): the same" $
      sameOverWs manufactured 0 (oldAnd seven)
    it "at configuration 2m (--locking-ofs manufactured, --hold-processing): the same" $
      sameOverWs manufactured 1 (oldAnd seven)

  describe "the reset clause over the hub's reset op" $
    it "at manufactured SPF-12:1/no-media passes, its power cycle over the reset op leaving ReadLocked and WriteLocked TRUE; a probe receives one sed.evt, power_cycle, its seq the probe's hello's plus 1" $
      withHubOf allowing manufactured 0 (\port -> do
        events <- newTVarIO []
        helloSeq' <- newEmptyTMVarIO
        withAsync (probe port (collect helloSeq' events)) $ \_ -> do
          s0 <- within "the probe's hello" (atomically (readTMVar helloSeq'))
          (code, json, _) <- jig (onWs port manufactured 0 (keys "SPF-12:1/no-media"))
          verdicts json `shouldBe` [(str "SPF-12:1/no-media", str "pass")]
          extensionVerdicts json `shouldBe` [str "pass"]
          notes json `shouldSatisfy` elem "event PowerCycle: good"
          code `shouldBe` ExitSuccess
          evts <- within "the probe's sed.evt" (atomically (readTVar events >>= \es -> if null es then retrySTM else pure es))
          [ (evtSeq e, evtEvent e) | e <- evts ] `shouldBe` [(s0 + 1, PowerCycle)])

  describe "a hub restarted under the run (hub B on hub A's port)" $
    it "fails the case in flight with a transport failure, never sending it again; notes the restart and the session it forgets; passes the later cases on hub B's TPer" $ do
      arrived <- newEmptyMVar
      gate <- newEmptyMVar
      seenByB <- newTVarIO []
      backendA <- holdingInSession arrived gate <$> backendFor defaultSimParams 0
      backendB <- logging seenByB <$> backendFor defaultSimParams 0
      portOfA <- newEmptyMVar
      (held, (code, json, _)) <- withAsync (readMVar portOfA >>= \port -> jig (onWs port defaultSimParams 0 restartCases)) $ \run -> do
        (port, held) <- withHub allowing backendA $ \port _ ->
          putMVar portOfA port >> (,) port <$> within "hub A's IF-SEND in ETC-04's session" (takeMVar arrived)
        -- hub A stopped (its peers closed with 1001, its socket closed); hub B on its port, a new server_id
        ran <- withHubOn port allowing backendB (\_ _ -> withinMicros 20000000 "the run over hub B" (wait run)) `finally` putMVar gate ()
        pure (held, ran)
      code `shouldBe` ExitFailure 1
      [ (k, v) | (Just (A.String k), Just (A.String v)) <- verdicts json ] `shouldBe` restartVerdicts
      transcriptText json `shouldSatisfy` T.isInfixOf "the request was sent, and is never sent again"
      notes json `shouldSatisfy` any (\n -> all (`T.isInfixOf` n)
        [ "the simulator over ws://127.0.0.1:", "/ws: the hub restarted, server_id ", ": a new TPer at its OFS"
        , "the run's session, unread StartSession and host properties forgotten as a power cycle forgets them" ])
      readTVarIO seenByB >>= (`shouldSatisfy` notElem held)

  describe "the open's refusals" $ do
    it "refuses a hub without --allow-remote-control: exit 1, naming why, no report" $
      withHubOf defaultHubOpts { hoAllowReset = True } defaultSimParams 0 (\port -> do
        (code, said) <- refused (onWs port defaultSimParams 0 (keys "UCT-01"))
        code `shouldBe` ExitFailure 1
        said `shouldSatisfy` T.isInfixOf "the hub allows no peek: hsed-sim --allow-remote-control")
    it "refuses a hub whose units, hold, OFS, discard, SID PIN or profile are not the run's: exit 1, each named by its flag" $
      do
        profile <- loadProfile ("../spec/fixtures" </> "disk6-level0.bin") >>= either (throwString . T.unpack) pure
        for_ [ (defaultSimParams { spUnits = UnitBlocks512 }, 0, "--units: bytes asked, blocks512 on the hub")
             , (defaultSimParams, 1, "--hold-ifrecvs: 0 asked, 1 on the hub")
             , (manufactured, 0, "--locking-ofs: manufactured-inactive asked, manufactured on the hub")
             , (defaultSimParams { spInactiveComIdDiscard = True }, 0, "--inactive-comid-discard: off asked, on on the hub")
             , (defaultSimParams { spSidPin = Just sidPin }, 0, "--sim-sid-pin: none (the MSID's) asked, 4 bytes")
             , (defaultSimParams { spProfile = Just profile }, 0, "--profile: none asked")
             ] $ \(hubParams, hubHold, reason) ->
          withHubOf allowing hubParams hubHold $ \port -> do
            (code, said) <- refused (onWs port defaultSimParams 0 (keys "UCT-01"))
            code `shouldBe` ExitFailure 1
            said `shouldSatisfy` T.isInfixOf ("the hub's setup differs: " <> reason)
    it "with a hub without --allow-remote-reset, fails SPF-12's extension: event PowerCycle unsupported" $
      withHubOf defaultHubOpts { hoAllowControl = True } manufactured 0 (\port -> do
        (code, json, _) <- jig (onWs port manufactured 0 (keys "SPF-12:1/no-media"))
        code `shouldBe` ExitFailure 1
        extensionVerdicts json `shouldSatisfy` notElem (str "pass")
        notes json `shouldSatisfy` any ("event PowerCycle: unsupported" `T.isPrefixOf`))

keys :: Text -> [CaseKey]
keys = either (error . T.unpack) id . parseCaseKeys

-- | M4a's ten cases and others.
oldAnd :: Text -> [CaseKey]
oldAnd more = keys ("UCT-01,UCT-02,UCT-03:1a-1c,SPF-02:1,ETC-02,ETC-03,ETC-04,ETC-08,ETC-15,ETC-21/adminsp," <> more)

-- | M5b's seven, which a manufactured run selects (Scott's answer Q1 of M5b).
seven :: Text
seven = "SPF-02:2,SPF-11,SPF-12:1/no-media,ETC-07,ETC-09,ETC-14,ETC-21"

manufactured :: SimParams
manufactured = defaultSimParams { spLockingOFS = Manufactured }

-- | C_PIN_SID's PIN as run 6 gives it (--sim-sid-pin 53494421), and how a setup names it: its length and the first
--   12 hex digits of its SHA-256 alone ("Hsed.Sim.View".'setupOf').
sidPin :: ByteString
sidPin = "SID!"

sidPinNamed :: Text
sidPinNamed = "4 bytes, sha256 " <> T.take 12 (sha256Hex sidPin)

-- | What a hub publishes that serves the default run: a TPer in bytes, everything allowed, the default setup.
served :: SedApp
served = SedApp
  { saRole      = "tper"
  , saImpl      = "hsed-sim test"
  , saUnits     = UnitBytes
  , saSsc       = Just "opal-2.30"
  , saErrata    = Just "scott"
  , saBaseComId = Just 0x1004
  , saNComIds   = Just 1
  , saAllows    = [minBound .. maxBound]
  , saSetup     = Just (setupOf defaultSimParams 0)
  }

-- | The run's simulator flags and hold set otherwise than the hub's, one at a time.
otherwise' :: [(SimParams, Int)]
otherwise' =
  [ (defaultSimParams { spUnits = UnitBlocks512 }, 0)
  , (defaultSimParams, 1)
  , (manufactured, 0)
  , (defaultSimParams { spInactiveComIdDiscard = True }, 0)
  , (defaultSimParams { spSidPin = Just sidPin }, 0)
  ]

-- | A hub's options that allow the control ops and the reset op.
allowing :: HubOpts
allowing = defaultHubOpts { hoAllowControl = True, hoAllowReset = True }

-- | An action given the port of a hub in the suite's threads, its backend the simulator of the parameters and hold.
withHubOf :: HubOpts -> SimParams -> Int -> (Word16 -> IO a) -> IO a
withHubOf opts params hold act = do
  backend <- backendOf opts params (InProcOpts (spUnits params) hold) >>= either (throwString . T.unpack) pure
  withHub opts backend (\port _ -> act port)

-- | The default simulator's backend for a hub that allows everything.
backendFor :: SimParams -> Int -> IO Backend
backendFor params hold = backendOf allowing params (InProcOpts (spUnits params) hold) >>= either (throwString . T.unpack) pure

-- | The cases of the restart: M4a's first six and ETC-08, ETC-15 and ETC-21/adminsp, ETC-04 the one that opens a
--   session (as Anybody, on the Admin SP) and calls in it.
restartCases :: [CaseKey]
restartCases = keys "UCT-01,UCT-02,ETC-02,ETC-03,ETC-04,ETC-08,ETC-15,ETC-21/adminsp"

-- | Their verdicts when hub A stops while ETC-04's call in its session is held.
restartVerdicts :: [(Text, Text)]
restartVerdicts =
  [ ("UCT-01", "pass"), ("UCT-02", "pass"), ("ETC-02", "pass"), ("ETC-03", "pass"), ("ETC-04", "fail"), ("ETC-08", "pass")
  , ("ETC-15", "pass"), ("ETC-21/adminsp", "pass") ]

-- | A backend whose first IF-SEND in a session (its first packet's HSN not 0: Core Table 18, PDF 41, the Session
--   field TSN then HSN, bytes 24-27 of the ComPacket after Table 17's 20, PDF 40) is held: its request given, its answer only once the gate opens.
holdingInSession :: MVar Request -> MVar () -> Backend -> Backend
holdingInSession arrived gate b = b { beTransport = (beTransport b) { tExchange = \r -> do
  when (W.dir r == IfSend && B.any (/= 0) (B.take 4 (B.drop 24 (payload r)))) $
    tryPutMVar arrived r >>= \isFirst -> when isFirst (readMVar gate)
  tExchange (beTransport b) r } }

-- | A backend that keeps every request it is given.
logging :: TVar [Request] -> Backend -> Backend
logging seen b = b { beTransport = (beTransport b) { tExchange = \r -> do
  atomically (modifyTVar' seen (<> [r]))
  tExchange (beTransport b) r } }

-- | A run's options over ws://127.0.0.1:<port>/ws, the host's polls shortened as the in-process tests shorten them.
onWs :: Word16 -> SimParams -> Int -> [CaseKey] -> RunOptions
onWs port params hold ks = (inProcess params hold ks) { roTarget = TWs (WsUrl port "/ws") }

-- | The same run in process.
inProcess :: SimParams -> Int -> [CaseKey] -> RunOptions
inProcess params hold ks = defaultRunOptions
  { roTests    = ks
  , roSim      = params
  , roHold     = hold
  , roExchange = defaultExchangeOpts { xPollMicros = 1000 }
  }

-- | A run in process and the same over a hub of its parameters and hold: the same exit, whole transcripts equal,
--   the reports equal but for what differs by design.
sameOverWs :: SimParams -> Int -> [CaseKey] -> Expectation
sameOverWs params hold ks = do
  (codeIn, inProc, _) <- jig (inProcess params hold ks)
  withHubOf allowing params hold $ \port -> do
    (codeWs, overWs, _) <- jig (onWs port params hold ks)
    codeWs `shouldBe` codeIn
    at "transcript" overWs `shouldBe` at "transcript" inProc
    invariant overWs `shouldBe` invariant inProc

-- | A report less what differs between a ws:// run and an in-process one by design: its start, its target, its
--   report directory, and what its first target says was opened.
invariant :: A.Value -> A.Value
invariant = \case
  A.Object o -> A.Object (KM.delete "started" (adjusted "options" options (adjusted "targets" targets o)))
  other      -> other
  where
    adjusted k f o = maybe o (\v -> KM.insert k (f v) o) (KM.lookup k o)
    options = \case
      A.Object o -> A.Object (KM.delete "target" (KM.delete "report_dir" o))
      other      -> other
    targets = \case
      A.Array ts -> A.toJSON (zipWith opened [0 :: Int ..] (toList ts))
      other      -> other
    opened i = \case
      A.Object t | i == 0 -> A.Object (KM.delete "opened" t)
      other               -> other

-- | A run into a temporary directory, its printing kept in a file there: its exit code, its JSON report and what it
--   printed.
jig :: RunOptions -> IO (ExitCode, A.Value, Text)
jig opts = withSystemTempDirectory "hsed-jig-ws" $ \dir -> do
  let reports = dir </> "report"
  code <- printingTo (dir </> "printed.txt") (runJig opts { roReportDir = reports })
  printed <- readFileUtf8 (dir </> "printed.txt")
  files <- listDirectory reports
  case [ f | f <- files, takeExtension f == ".json" ] of
    [f] -> readFileBinary (reports </> f) >>= either throwString (\json -> pure (code, json, printed)) . A.eitherDecodeStrict'
    fs  -> throwString ("not one JSON report in " <> reports <> ": " <> show fs <> "; the run printed " <> T.unpack printed)

-- | A run refused before it writes a report: its exit code and what it printed.
refused :: RunOptions -> IO (ExitCode, Text)
refused opts = withSystemTempDirectory "hsed-jig-ws" $ \dir -> do
  code <- printingTo (dir </> "printed.txt") (runJig opts { roReportDir = dir </> "report" })
  (,) code <$> readFileUtf8 (dir </> "printed.txt")

-- | An action with what it prints to stdout and stderr written to a file (no device opened).
printingTo :: FilePath -> IO a -> IO a
printingTo file act = do
  hFlush stdout
  hFlush stderr
  withFile file WriteMode $ \h ->
    bracket ((,) <$> hDuplicate stdout <*> hDuplicate stderr)
            (\(out, err) -> do
               hFlush stdout >> hFlush stderr
               hDuplicateTo out stdout >> hDuplicateTo err stderr
               hClose out >> hClose err)
            (\_ -> hDuplicateTo h stdout >> hDuplicateTo h stderr >> act)

-- | A probe's connection: its hello's seq given, then every reset broadcast it receives kept.
collect :: TMVar Integer -> TVar [SedEvt] -> Conn -> IO ()
collect helloSeq' events conn = connReceive conn >>= \case
  InClosed _ -> pure ()
  incoming   -> do
    case fromServer incoming of
      ServerHello h -> atomically (void (tryPutTMVar helloSeq' (helloSeq h)))
      ServerEvt e   -> atomically (modifyTVar' events (<> [e]))
      _             -> pure ()
    collect helloSeq' events conn

-- | What a probe waits for, within 5 s: a hub that never sends it fails the example, not the suite.
within :: String -> IO a -> IO a
within = withinMicros 5000000

-- | 'within' the microseconds given.
withinMicros :: Int -> String -> IO a -> IO a
withinMicros micros what act = timeout micros act >>= maybe (throwString (what <> ": not in time")) pure

-- | A field of a JSON object.
at :: Text -> A.Value -> Maybe A.Value
at k = \case
  A.Object o -> KM.lookup (K.fromText k) o
  _          -> Nothing

elements :: Maybe A.Value -> [A.Value]
elements = \case
  Just (A.Array xs) -> toList xs
  _                 -> []

str :: Text -> Maybe A.Value
str = Just . A.String

-- | Each case's key and verdict.
verdicts :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
verdicts json = [ (at "key" c, at "verdict" c) | c <- elements (at "cases" json) ]

-- | Each extension's verdict.
extensionVerdicts :: A.Value -> [Maybe A.Value]
extensionVerdicts json = [ at "verdict" x | c <- elements (at "cases" json), x <- elements (at "extensions" c) ]

-- | The transcript as JSON's text.
transcriptText :: A.Value -> Text
transcriptText = maybe "" (decodeUtf8Lenient . toStrictBytes . A.encode) . at "transcript"

-- | The transcript's notes.
notes :: A.Value -> [Text]
notes json = [ t | e <- elements (at "transcript" json), Just (A.String t) <- [at "text" e] ]
