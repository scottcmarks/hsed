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
@--allow-remote-reset@, @--conformance-hooks@), and a token file. A usage
error exits 2. Its flags are read; serving them is not yet written (group
G2): it says so and exits 1.
-}
module Main (main) where

import           Options.Applicative
import           RIO
import qualified RIO.Text                as T

import           Hsed.Sim.Flags          (lockingOFSName, parseLockingOFS, parseSidPin, parseUnits, unitName)
import           Hsed.Sim.Types          (SimParams (..), defaultSimParams)
import           Hsed.Transport.InProc   (InProcOpts (..))
import           Hsed.Transport.Mhtx.Hub (HubOpts (..), defaultHubOpts, parseListen)
import           Hsed.Version            (hsedVersion)


-- | What the command line asks for: the port on 127.0.0.1 (0, an ephemeral
--   one), the hub's options, the simulator's parameters and its transport's,
--   the profile and the token file to read.
data SimOptions = SimOptions !Word16 !HubOpts !SimParams !InProcOpts !(Maybe FilePath) !(Maybe FilePath)

main :: IO ()
main = execParser hsedSim >>= serve

-- | Serve the simulator as the options say.
serve :: SimOptions -> IO ()
serve _options = do
  hPutBuilder stderr "hsed-sim: not yet written: group G2 (app/hsed-sim/Main.hs: the hub served on 127.0.0.1)\n"
  exitWith (ExitFailure 1)

hsedSim :: ParserInfo SimOptions
hsedSim = info (simOptions <**> helper <**> versionOption)
  ( fullDesc
 <> header "hsed-sim - the hsed simulator served over mh_transport frames"
 <> progDesc ("Serves the in-process simulator to mh_transport clients (hsed-jig --target ws://127.0.0.1:PORT/PATH) on "
              <> "127.0.0.1 alone (docs/spec-as-source.md 7, 11 M5c; docs/wire.md).")
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
        <> help "a client silent this long is closed with 4000 (heartbeat timeout)")
  <*> option (eitherReader parseUnits) (long "units" <> metavar "UNITS" <> value (spUnits sim0) <> showDefaultWith (T.unpack . unitName)
        <> help "what transfer lengths count, bytes or blocks512")
  <*> option (eitherReader errata) (long "errata" <> metavar "ERRATA" <> value () <> showDefaultWith (const "scott")
        <> help "the errata the simulator was compiled under: scott alone (hsedc compiles them in)")
  <*> option (eitherReader parseLockingOFS) (long "locking-ofs" <> metavar "OFS" <> value (spLockingOFS sim0)
        <> showDefaultWith (T.unpack . lockingOFSName)
        <> help "the Locking SP's Original Factory State, manufactured-inactive or manufactured")
  <*> optional (option (eitherReader parseSidPin) (long "sim-sid-pin" <> metavar "HEX"
        <> help "C_PIN_SID's PIN as hex digits, two a byte (an optional 0x before them), 4 to 32 bytes"))
  <*> option auto (long "hold-ifrecvs" <> metavar "N" <> value 0 <> showDefault
        <> help "hold a prepared response for N IF-RECVs")
  <*> switch (long "inactive-comid-discard" <> help "discard a command to an inactive ComID rather than refuse it")
  <*> optional (strOption (long "profile" <> metavar "FILE.bin"
        <> help "add a Level 0 response's descriptors the simulator lacks"))
  <*> switch (long "allow-remote-control"
        <> help "answer the control ops, peek (the simulator's state, its PINs among it) and release")
  <*> switch (long "allow-remote-reset" <> help "answer the reset op: a power cycle, hardware reset or hot plug, broadcast")
  <*> switch (long "conformance-hooks" <> help "serve POST /emit and GET /clients, for mh_transport's conformance suite")
  <*> optional (strOption (long "token-file" <> metavar "FILE"
        <> help "close a client whose token query parameter is not this file's (4001, before hello)"))
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
