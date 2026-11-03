{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Support
Description : What hsed's tests share: a run silenced, a report's JSON read, a drive-like response, the simulator as a drive, an allowance as printed
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

'silently' keeps a run's printing out of the test's output; 'at' and
'atPath' read into a report's JSON; 'disk6Like' is a drive-like Level 0
response synthesized from what sedutil printed of disk6; 'fakeDrive' is
the simulator as an attached drive, a @'ReadOnly@ transport in 512-byte
blocks with what the IORegistry would say of disk6 and a bind of its base
ComID, and 'onFakeDrive' a run over it as a run opens a drive, behind the
Guard (M4c); 'isAsPrinted' tells an allowance that is a condition as
printed.
-}
module Hsed.Jig.Support
  ( -- * Running quietly
    silently
    -- * Reports
  , at
  , atPath
  , disk6Like
    -- * A drive without the drive
  , fakeDrive
  , onFakeDrive
    -- * Allowances
  , isAsPrinted
  ) where

import qualified Data.Aeson               as A
import qualified Data.Aeson.Key           as K
import qualified Data.Aeson.KeyMap        as KM
import           GHC.IO.Handle            (hDuplicate, hDuplicateTo)
import           RIO
import           RIO.Directory            (listDirectory)
import           RIO.FilePath             (takeExtension, (</>))
import qualified RIO.Text                 as T

import           Hsed.Core.Feature        (Allowance (..))
import           Hsed.Jig.Guard           (driveClose, guardDrive)
import           Hsed.Jig.Options         (RunOptions (..), TargetSpec (..))
import           Hsed.Jig.Run             (driveAllowedFor, runOpened, selectedCases)
import           Hsed.Jig.Target          (Target (..))
import           Hsed.Jig.Transcript      (newTranscript)
import           Hsed.Level0.Sedutil      (parseQuery, synthesize)
import           Hsed.Sim.Types           (SimParams (..))
import           Hsed.Transport.Drive     (DriveInfo (..), InterfaceType (..), RawDrive (..))
import           Hsed.Transport.InProc    (InProcOpts (..), SimControl)
import           Hsed.Transport.Opened    (openedInProc)
import           Hsed.Wire                (hex)
import           Hsed.Wire.Internal       (Describe (..), IfStatus (..), Transport (..), Unit (..))


-- * Running quietly

-- | An action with what it prints to stdout and stderr thrown away.
silently :: IO a -> IO a
silently act = do
  hFlush stdout
  hFlush stderr
  withFile "/dev/null" WriteMode $ \devNull ->
    bracket ((,) <$> hDuplicate stdout <*> hDuplicate stderr)
            (\(out, err) -> do
               hFlush stdout >> hFlush stderr
               hDuplicateTo out stdout >> hDuplicateTo err stderr
               hClose out >> hClose err)
            (\_ -> hDuplicateTo devNull stdout >> hDuplicateTo devNull stderr >> act)


-- * Reports

-- | A field of a JSON object.
at :: Text -> A.Value -> Maybe A.Value
at k = \v -> case v of
  A.Object o -> KM.lookup (K.fromText k) o
  _          -> Nothing

-- | Fields one inside the other.
atPath :: [Text] -> A.Value -> Maybe A.Value
atPath ks v = foldM (flip at) v ks

-- | A drive-like Level 0 response, synthesized from what sedutil printed of
--   disk6 (spec/fixtures/disk6-query.txt; tests run in hsed/).
disk6Like :: IO ByteString
disk6Like = do
  query <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
  either (throwString . T.unpack) (pure . synthesize) (parseQuery query)


-- * A drive without the drive

-- | The simulator as an attached drive, for a drive's run without the
--   drive: the simulator of the parameters, in 512-byte blocks as the
--   kext's transport counts (its @spUnits@ and the transport's,
--   "Hsed.Transport.InProc"), behind a @'ReadOnly@ transport that says it
--   is a drive (role @drive@, impl @fake drive@, units blocks512, device
--   @/dev/disk6@), causes no event and closes nothing; what the IORegistry
--   would say of disk6 (serial TESTSERIAL0001, kext build 625, SAT,
--   registry id 0); a bind that records the base ComID once, takes the same
--   base again, and refuses another; and the simulator's control, to look
--   at its state.
fakeDrive :: SimParams -> IO (RawDrive, DriveInfo, SimControl)
fakeDrive params = do
  (sim, control) <- openedInProc params { spUnits = UnitBlocks512 } (InProcOpts UnitBlocks512 0)
  bound <- newIORef Nothing
  let drive = MkTransport
        { tExchange = tExchange sim
        , tInject   = \_ -> pure (Unsupported "a fake drive causes no events")
        , tUnit     = UnitBlocks512
        , tDescribe = Describe "drive" "fake drive" UnitBlocks512 Nothing Nothing Nothing (Just "/dev/disk6")
        , tClose    = pure ()
        }
      bind base = atomicModifyIORef' bound $ \was -> case was of
        Just b | b /= base -> (was, Left ("the fake drive's base ComID is " <> hex b <> " already, not " <> hex base))
        _                  -> (Just base, Right ())
  pure (RawDrive drive bind, DriveInfo "disk6" "TESTSERIAL0001" "625" IfSAT 0, control)

-- | A run over a fake drive ('fakeDrive') as a run opens one: a new
--   transcript, the drive behind the Guard with what the selected cases
--   admit ("Hsed.Jig.Run".'driveAllowedFor'; none for keys the catalogue
--   lacks), the run ('runOpened') into a temporary report directory,
--   quietly, its target named as the drive's device; its exit code and its
--   JSON report (null where it wrote none, a usage error's).
onFakeDrive :: RunOptions -> (RawDrive, DriveInfo, SimControl) -> IO (ExitCode, A.Value)
onFakeDrive opts (raw, di, _) = withSystemTempDirectory "hsed-jig" $ \dir -> do
  t <- newTranscript
  g <- guardDrive t (either (const []) (driveAllowedFor opts) (selectedCases (roTests opts))) (Just di) raw
  let reports = dir </> "report"
  code <- silently (runOpened opts { roTarget = TDevice ("/dev/" <> diBsd di), roReportDir = reports } t (DriveTarget g))
            `finally` driveClose g
  files <- either (const []) id <$> tryIO (listDirectory reports)
  (,) code <$> case [ f | f <- files, takeExtension f == ".json" ] of
    []  -> pure A.Null
    [f] -> readFileBinary (reports </> f) >>= either throwString pure . A.eitherDecodeStrict'
    fs  -> throwString ("more than one JSON report in " <> reports <> ": " <> show fs)


-- * Allowances

-- | Whether an allowance is a condition as printed ('AsPrinted'), which the
--   Pair compares unchecked unless it evaluates it.
isAsPrinted :: Allowance -> Bool
isAsPrinted = \a -> case a of
  AsPrinted _ -> True
  _           -> False
