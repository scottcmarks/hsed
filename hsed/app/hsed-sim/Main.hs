{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : hsed-sim, the simulator served over the LAN leg (M5c)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The command line of the simulator's mh_transport hub (design 7 item 5, 11
M5c): the in-process simulator behind "Hsed.Transport.Mhtx.Hub", listening
on 127.0.0.1 alone (Scott's answer Q1; @--listen@ defaults to an ephemeral
port), its heartbeat, the simulator's flags as hsed-jig reads them
("Hsed.Sim.Flags"), what it allows besides commands (@--allow-remote-control@,
@--allow-remote-reset@, @--conformance-hooks@), and a token file.

It writes one line to stdout, @PORT=<n>@, once its socket listens, and
logs to stderr (never the token). It exits 2 for a usage error: a flag it
cannot read (@--listen@ another host among them), parameters out of range
("Hsed.Sim.Types".'Hsed.Sim.Types.simParamsProblems'), a heartbeat it
cannot run ('Hsed.Transport.Mhtx.Hub.heartbeatProblems'), running as root
(euid 0; the three 'Hsed.Transport.Mhtx.Hub.usageProblems'), a @--profile@ or
@--token-file@ that is not a regular file outside @\/dev@
('plainFileProblem', checked before either is read). It exits 1 when what
it was given will not serve: a profile or token it cannot read, parameters
its SPs do not fold with, a port it cannot bind. SIGINT and SIGTERM close
every peer with 1001 ('hubClose') and exit 0.
-}
module Main (main) where

import qualified Network.Socket           as N
import           Options.Applicative
import           RIO
import qualified RIO.Text                 as T
#if !defined(mingw32_HOST_OS)
import           System.Posix.Signals     (Handler (Catch), installHandler, sigINT, sigTERM)
import           System.Posix.User        (getEffectiveUserID)
#endif

import           Hsed.Sim.Flags           (lockingOFSName, parseLockingOFS, parseSidPin, parseUnits, unitName)
import           Hsed.Sim.Profile         (loadProfile)
import           Hsed.Sim.Types           (SimParams (..), defaultSimParams)
import           Hsed.Transport.InProc    (InProcOpts (..))
import           Hsed.Transport.Mhtx.Conn (bindLoopback)
import           Hsed.Transport.Mhtx.Hub  (HubOpts (..), backendOf, defaultHubOpts, hubClose, newHub, parseListen,
                                           plainFileProblem, readTokenFile, runHub, usageProblems)
import           Hsed.Version             (hsedVersion)


-- | What the command line asks for: the port on 127.0.0.1 (0, an ephemeral
--   one), the hub's options, the simulator's parameters and its transport's,
--   the profile and the token file to read.
data SimOptions = SimOptions !Word16 !HubOpts !SimParams !InProcOpts !(Maybe FilePath) !(Maybe FilePath)

main :: IO ()
main = execParser hsedSim >>= serve >>= exitWith

-- | Serve the simulator as the options say: the usage errors (exit 2), then
--   the profile, the token and the simulator read, the socket bound (exit 1
--   where one fails), then the hub served until a signal (exit 0).
serve :: SimOptions -> IO ExitCode
serve (SimOptions port opts params inProc profile tokenFile) = do
  root <- runningAsRoot
  files <- catMaybes <$> traverse (uncurry plainFileProblem) (given "--profile" profile <> given "--token-file" tokenFile)
  case usageProblems root params opts <> files of
    problems@(_ : _) -> complain problems $> ExitFailure 2
    []               -> readFiles >>= \case
      Left why                -> complain [why] $> ExitFailure 1
      Right (profiled, token) -> backendOf opts { hoToken = token } profiled inProc >>= \case
        Left why     -> complain ["the simulator: " <> why] $> ExitFailure 1
        Right backend -> do
          stop <- newEmptyMVar
          installStops stop
          tryAny (bindLoopback port) >>= \case
            Left e              -> complain ["--listen 127.0.0.1:" <> tshow port <> ": " <> tshow e] $> ExitFailure 1
            Right (sock, bound) -> flip finally (N.close sock) $ do
              hPutBuilder stdout (getUtf8Builder ("PORT=" <> displayShow bound <> "\n"))
              hFlush stdout
              hub <- newHub opts { hoToken = token } backend
              say ("hsed-sim " <> hsedVersion <> ": serving 127.0.0.1:" <> tshow bound <> " (" <> settings token <> ")")
              -- the peers closed while the hub still serves, so that warp's shutdown cuts none of them first
              withAsync (runHub hub sock) $ \serving -> tryAny (race (readMVar stop) (wait serving)) >>= \case
                Left e           -> complain ["the hub: " <> tshow e] $> ExitFailure 1
                Right (Right ()) -> complain ["the hub stopped serving"] $> ExitFailure 1
                Right (Left sig) -> do
                  say ("hsed-sim: " <> sig <> ": closing every peer with 1001 (going away)")
                  hubClose hub $> ExitSuccess
  where
    given name = maybe [] (\file -> [(name, file)])
    readFiles = do
      loaded <- traverse loadProfile profile
      token  <- traverse readTokenFile tokenFile
      pure $ do
        p <- maybe (Right Nothing) (bimap ("--profile " <>) Just) loaded
        t <- sequenceA token
        pure (params { spProfile = p }, t)
    settings token = T.intercalate ", " $
      [ "heartbeat " <> tshow (hoIntervalMs opts) <> " ms, timeout " <> tshow (hoTimeoutMs opts) <> " ms"
      , "allows " <> T.intercalate ", " (["commands", "describe"] <> [ op | (op, allowed) <- ops, allowed ]) ]
      <> [ "the conformance hooks /emit and /clients" | hoHooks opts ]
      <> [ "a token asked of every client" | isJust token ]
    ops = [("peek", hoAllowControl opts), ("release", hoAllowControl opts), ("reset", hoAllowReset opts)]

-- | Lines to stderr, each named hsed-sim's.
complain :: [Text] -> IO ()
complain = traverse_ (\l -> say ("hsed-sim: " <> l))

-- | A line to stderr.
say :: Text -> IO ()
say t = hPutBuilder stderr (getUtf8Builder (display t <> "\n"))

-- | Whether hsed-sim runs as root (euid 0); never, on Windows.
runningAsRoot :: IO Bool
#if defined(mingw32_HOST_OS)
runningAsRoot = pure False
#else
runningAsRoot = (== 0) <$> getEffectiveUserID
#endif

-- | SIGINT and SIGTERM caught: the signal's name put in the variable, which
--   the server waits on (a second signal finds it full). On Windows, none.
installStops :: MVar Text -> IO ()
#if defined(mingw32_HOST_OS)
installStops _ = pure ()
#else
installStops stop = for_ [(sigINT, "SIGINT"), (sigTERM, "SIGTERM")] $ \(sig, name) ->
  void (installHandler sig (Catch (void (tryPutMVar stop name))) Nothing)
#endif

hsedSim :: ParserInfo SimOptions
hsedSim = info (simOptions <**> helper <**> versionOption)
  ( fullDesc
 <> header "hsed-sim - the hsed simulator served over mh_transport frames"
 <> progDesc ("Serves the in-process simulator to mh_transport clients (hsed-jig --target ws://127.0.0.1:PORT/ws) on "
              <> "127.0.0.1 alone, writing PORT=<n> to stdout once it listens (docs/spec-as-source.md 7, 11 M5c; docs/wire.md).")
 <> failureCode 2 )
  where
    versionOption = infoOption (T.unpack hsedVersion) (long "version" <> help "Show the version")

simOptions :: Parser SimOptions
simOptions = mk
  <$> option (eitherReader parseListen)
        (long "listen" <> metavar "127.0.0.1:PORT" <> value 0 <> showDefaultWith (\p -> "127.0.0.1:" <> show p)
         <> help "where to listen: 127.0.0.1 alone, PORT 0 an ephemeral port (printed as PORT=<n> once listening)")
  <*> option auto (long "heartbeat-interval-ms" <> metavar "MS" <> value (hoIntervalMs defaultHubOpts) <> showDefault
        <> help "hello's heartbeat interval: a client pings this often")
  <*> option auto (long "dead-timeout-ms" <> metavar "MS" <> value (hoTimeoutMs defaultHubOpts) <> showDefault
        <> help "a client silent this long is closed with 4000 (heartbeat timeout); above the interval")
  <*> option (eitherReader parseUnits) (long "units" <> metavar "UNITS" <> value (spUnits sim0) <> showDefaultWith (T.unpack . unitName)
        <> help "what transfer lengths count, bytes or blocks512")
  <*> option (eitherReader errata) (long "errata" <> metavar "ERRATA" <> value () <> showDefaultWith (const "scott")
        <> help "the errata the simulator was compiled under: scott alone (hsedc compiles them in)")
  <*> option (eitherReader parseLockingOFS) (long "locking-ofs" <> metavar "OFS" <> value (spLockingOFS sim0)
        <> showDefaultWith (T.unpack . lockingOFSName)
        <> help "the Locking SP's Original Factory State, manufactured-inactive or manufactured")
  <*> optional (option (eitherReader parseSidPin) (long "sim-sid-pin" <> metavar "HEX"
        <> help "C_PIN_SID's PIN as hex digits, two a byte (an optional 0x before them), 4 to 32 bytes"))
  <*> option (eitherReader count) (long "hold-ifrecvs" <> metavar "N" <> value 0 <> showDefault
        <> help "hold a prepared response for N IF-RECVs (0, released at once)")
  <*> switch (long "inactive-comid-discard" <> help "discard a command to an inactive ComID rather than refuse it")
  <*> optional (strOption (long "profile" <> metavar "FILE.bin"
        <> help "add a Level 0 response's descriptors the simulator lacks (a regular file outside /dev)"))
  <*> switch (long "allow-remote-control"
        <> help "answer the control ops, peek (the simulator's state, its PINs among it) and release")
  <*> switch (long "allow-remote-reset" <> help "answer the reset op: a power cycle, hardware reset or hot plug, broadcast")
  <*> switch (long "conformance-hooks" <> help "serve POST /emit and GET /clients, for mh_transport's conformance suite")
  <*> optional (strOption (long "token-file" <> metavar "FILE"
        <> help ("close a client whose token query parameter is not this file's bytes, a line ending at their end left "
                 <> "out (4001, before hello), and refuse the hooks such a request (401); a regular file outside /dev")))
  where
    sim0 = defaultSimParams
    mk port intervalMs deadMs u () ofs sidPin hold discard profile control reset hooks token = SimOptions port
      defaultHubOpts { hoIntervalMs = intervalMs, hoTimeoutMs = deadMs, hoAllowControl = control, hoAllowReset = reset
                     , hoHooks = hooks }
      sim0 { spUnits = u, spLockingOFS = ofs, spSidPin = sidPin, spInactiveComIdDiscard = discard }
      (InProcOpts u hold)
      profile
      token
    errata e
      | e == "scott" = Right ()
      | otherwise    = Left ("--errata " <> e <> ": scott alone, the errata hsedc compiled the simulator under")
    count s = case readMaybe s of
      Just n | n >= 0 -> Right n
      _               -> Left ("--hold-ifrecvs " <> s <> ": a count of IF-RECVs, 0 or more")
