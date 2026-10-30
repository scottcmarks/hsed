{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Cli
Description : What each hsed-jig command does, apart from reading its command line
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The commands of the host jig (design 9.2), here rather than in
@app/hsed-jig/Main.hs@ (which reads the command line, as hsedc's does) so
that the tests can run them:

* @run@ (the default): run cases against a target ("Hsed.Jig.Run");
* @level0 decode FILE [--against QUERY.txt]@: split and decode a Level 0
  response, a capture or a synthesized one; with @--against@, compare it
  with every value sedutil printed of the drive ("Hsed.Level0.Sedutil") and
  ask it to hold the Configurable Namespace Locking descriptor, which
  sedutil prints no line for though the kext reports it on disk6 (design
  11, M3);
* @level0 synthesize QUERY.txt OUT.bin@: a drive-like response from
  sedutil's values, a test aid until the drive's own is captured;
* @verify-transcript REPORT.json [--no-if-send]@ and @diff A.json B.json@:
  a run's report read back ("Hsed.Jig.Report").

Each gives the process's exit code: 0 success, 1 a failure, 2 a usage
error, and for @run@ 3 a safety violation. The first of @run@'s safety
gates are here, before anything else runs: a drive target (@/dev/diskN@,
alone or paired) needs @--expect-serial@ (a usage error without it), and
root, which hsed itself requires, the loaded kext (build 625) checking no
privilege (design 9.3); the run itself checks that no IF-SEND was issued on
a drive.
-}
module Hsed.Jig.Cli
  ( Command (..)
  , runCommand
  ) where

import           Numeric              (showHex)
import qualified RIO.ByteString       as B
import           RIO
import qualified RIO.Text             as T

import           Hsed.Core.Feature    (Allowance (..), Feature (featureCode))
import           Hsed.Core.Layout     (Content (..))
import           Hsed.Jig.Files       (runningAsRoot, writeOwned)
import           Hsed.Jig.Options     (TargetSpec (..))
import           Hsed.Jig.Report      (diffReports, verifyTranscript)
import           Hsed.Jig.Run         (RunOptions (..), runJig)
import           Hsed.Level0          (DecodedDescriptor (..), Descriptor (..), Level0 (..), decodeDescriptor,
                                       splitLevel0)
import           Hsed.Level0.Sedutil  (compareQuery, parseQuery, synthesize)
import           Hsed.Sim.FeatureSets (NamespaceLockingFeature)


-- | An hsed-jig command.
data Command
  = Run RunOptions
  | Level0Decode FilePath (Maybe FilePath)    -- ^ the response, sedutil's printed query to compare it with
  | Level0Synthesize FilePath FilePath        -- ^ sedutil's printed query, the response to write
  | VerifyTranscript FilePath Bool            -- ^ the report, whether an IF-SEND fails it
  | Diff FilePath FilePath
  deriving (Eq, Show)

-- | Run a command: the exit code.
runCommand :: Command -> IO ExitCode
runCommand = \case
  Run opts                      -> run opts
  Level0Decode file against     -> level0Decode file against
  Level0Synthesize query out    -> level0Synthesize query out
  VerifyTranscript report noIfs -> verifyTranscript report noIfs
  Diff a b                      -> diffReports a b


-- * run

-- | Run the jig, a drive target refused before anything is opened: without
--   @--expect-serial@ (exit 2, a usage error), or without root (exit 3).
run :: RunOptions -> IO ExitCode
run opts = case devices (roTarget opts) of
  []      -> runJig opts
  devs
    | isNothing (roExpectSerial opts) -> do
        traverse_ (\d -> complain ("hsed-jig: --target " <> d <> " needs --expect-serial SERIAL: a drive is opened only as "
                                    <> "the one named")) devs
        pure (ExitFailure 2)
    | otherwise -> do
        root <- runningAsRoot
        if root
          then runJig opts
          else do
            traverse_ (\d -> complain ("hsed-jig: --target " <> d <> " needs root (sudo), hsed's own gate: the kext checks no "
                                        <> "privilege (design 9.3); build as yourself, never with sudo stack")) devs
            pure (ExitFailure 3)
  where
    devices = \case
      TDevice d -> [d]
      TPair a b -> devices a <> devices b
      _         -> []


-- * level0

-- | Split a response, print it and its descriptors decoded, and, against
--   sedutil's printed query, every value that disagrees: a failure if the
--   split found problems or the comparison anything.
level0Decode :: FilePath -> Maybe FilePath -> IO ExitCode
level0Decode file against = do
  bytes <- readFileBinary file
  case splitLevel0 bytes of
    Left e   -> failWith ("level0 decode: " <> T.pack file <> ": " <> tshow e)
    Right l0 -> do
      traverse_ say (describeLevel0 l0)
      disagreements <- maybe (pure []) (againstQuery l0) against
      traverse_ (say . ("disagrees: " <>)) disagreements
      pure (if null (l0Problems l0) && null disagreements then ExitSuccess else ExitFailure 1)

-- | What a response holds that sedutil's printed query says otherwise, and
--   the Configurable Namespace Locking descriptor if it lacks one.
againstQuery :: Level0 -> FilePath -> IO [Text]
againstQuery l0 queryFile = do
  query <- readFileUtf8 queryFile
  pure $ case parseQuery query of
    Left e  -> [T.pack queryFile <> ": " <> e]
    Right q -> compareQuery q l0 <> [ noNamespaceLocking | cnl `notElem` map dCode (l0Descriptors l0) ]
  where
    cnl = fromInteger (featureCode (Proxy :: Proxy NamespaceLockingFeature))
    noNamespaceLocking = "no Configurable Namespace Locking descriptor (" <> hex cnl <> ")"

-- | Write a drive-like response from sedutil's printed query.
level0Synthesize :: FilePath -> FilePath -> IO ExitCode
level0Synthesize queryFile out = do
  query <- readFileUtf8 queryFile
  case parseQuery query of
    Left e  -> failWith ("level0 synthesize: " <> T.pack queryFile <> ": " <> e)
    Right q -> do
      let bytes = synthesize q
      writeOwned out bytes
      say ("wrote " <> T.pack out <> ": " <> tshow (B.length bytes) <> " bytes")
      pure ExitSuccess

-- | A response as lines: its Length of Parameter Data, its problems, each
--   descriptor and what decoding it read.
describeLevel0 :: Level0 -> [Text]
describeLevel0 l0 =
  ("Length of Parameter Data " <> tshow (l0LengthOfParameterData l0) <> ", " <> tshow (length (l0Descriptors l0)) <> " descriptors")
  : map (("problem: " <>) . tshow) (l0Problems l0)
  <> concatMap descriptor (l0Descriptors l0)
  where
    descriptor d =
      (hex (dCode d) <> " version " <> tshow (dVersion d) <> ", " <> tshow (B.length (dBytes d)) <> " bytes")
      : maybe ["  not decoded"] decoded (decodeDescriptor d)
    decoded dd =
      ("  " <> ddSource dd)
      : map (\(name, c) -> "  " <> name <> " = " <> content c) (ddFields dd)
      <> map (("  differs: " <>) . tshow) (ddDifferences dd)
      <> map (\(name, a) -> "  allows: " <> name <> " " <> allowance a) (ddAllowances dd)
    content = \case
      Number n -> tshow n <> " (" <> hex n <> ")"
      Octets b -> T.pack (concatMap byteHex (B.unpack b))
    byteHex w = (if w < 16 then ('0' :) else id) (showHex w "")
    allowance = \case
      VendorUnique -> "any value (VU)"
      AnyVersion v -> hex v <> " or any version"
      Minimum v    -> tshow v <> " or larger"
      AsPrinted t  -> "as printed: " <> t

hex :: Integral a => a -> Text
hex n = "0x" <> T.pack (showHex n "")

say :: Text -> IO ()
say t = hPutBuilder stdout (getUtf8Builder (display t <> "\n"))

complain :: Text -> IO ()
complain t = hPutBuilder stderr (getUtf8Builder (display t <> "\n"))

failWith :: Text -> IO ExitCode
failWith t = complain t >> pure (ExitFailure 1)
