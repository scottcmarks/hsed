{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.Capture
Description : A drive's Level 0 response written as a fixture, with its sidecar
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@--capture-level0 FILE@ (design 10.2, M3's deliverable
@spec/fixtures/disk6-level0.bin@): the IF-RECV payload exactly as the kext
returned it, and beside it @FILE.json@ with what produced it: the date (UTC),
the kext's build, the serial masked, the CDB, the transfer length asked and
the bytes transferred, the payload's sha256, and the jig's version
("Hsed.Version"). Both through "Hsed.Jig.Files", so that a capture under
sudo stays the user's.

The sidecar is @FILE@ with its extension replaced (@disk6-level0.bin@,
@disk6-level0.json@), its keys one to a line in a fixed order, so that a
committed capture diffs plainly; beside the fields above it carries the
drive's other registry facts (BSD name, interface, registry entry id) and
the response's Length of Parameter Data, read with Core Table 39's
generated layout. A capture is of a good response only: any other status
writes nothing. A line on stderr names both files, the sha256 and the
Length of Parameter Data.
-}
module Hsed.Jig.Capture
  ( captureLevel0
  , sidecarOf
  ) where

import qualified Data.Aeson            as A
import qualified RIO.ByteString        as B
import qualified RIO.ByteString.Lazy   as BL
import           RIO
import           RIO.FilePath          (replaceExtension, takeFileName)
import qualified RIO.Text              as T
import           RIO.Time              (defaultTimeLocale, formatTime, getCurrentTime)

import           Hsed.Core.Layout      (LayoutError)
import           Hsed.Core.Record      (DecodedRecord (..), decodeRecord)
import           Hsed.Gen.Core.Layouts (Level0DiscoveryHeader (..))
import           Hsed.Jig.Files        (writeOwned)
import           Hsed.Jig.Sha256       (sha256Hex)
import           Hsed.Transport.Cdb    (cdbHex)
import           Hsed.Transport.Drive  (DriveInfo (..), maskSerial)
import           Hsed.Version          (hsedVersion)
import           Hsed.Wire             (IfStatus (..), Response (..), ifStatusName)


-- | Write a capture: the file, the drive, the CDB that asked, the response.
--   Throws, writing nothing, if the response is not good or the sidecar
--   would be the capture itself (a @FILE@ named @.json@).
--
--   The bytes transferred are the payload's: the kext's transport answers
--   good only when the kext moved exactly the length asked
--   ("Hsed.Transport.MacKext"), and fails the command otherwise.
captureLevel0 :: FilePath -> DriveInfo -> ByteString -> Response -> IO ()
captureLevel0 file di cdb r = do
  unless (status r == Good) $
    throwString ("captureLevel0 " <> file <> ": the response is " <> T.unpack (ifStatusName (status r))
                 <> ", not good: nothing written")
  when (sidecar == file) $
    throwString ("captureLevel0 " <> file <> ": its sidecar would be itself; name the capture .bin")
  now <- getCurrentTime
  let fields =
        [ ("capture",                  A.toJSON (takeFileName file))
        , ("captured",                 A.toJSON (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now))
        , ("hsed_jig",                 A.toJSON hsedVersion)
        , ("bsd",                      A.toJSON (diBsd di))
        , ("serial",                   A.toJSON (maskSerial (diSerial di)))
        , ("kext_build",               A.toJSON (diKextBuild di))
        , ("interface",                A.toJSON (diInterface di))
        , ("registry_id",              A.toJSON (diRegistryId di))
        , ("cdb",                      A.toJSON (cdbHex cdb))
        , ("len_bytes",                A.toJSON (B.length bytes))
        , ("transferred",              A.toJSON (B.length bytes))
        , ("sha256",                   A.toJSON digest)
        , ("length_of_parameter_data", maybe A.Null A.toJSON lopd)
        ]
  writeOwned file bytes
  writeOwned sidecar (encodeUtf8 (renderObject fields))
  hPutBuilder stderr $ getUtf8Builder $
    "captured " <> fromString file <> ": " <> display (B.length bytes) <> " bytes, sha256 " <> display digest
    <> ", Length of Parameter Data " <> maybe "unread" display lopd <> "; its sidecar " <> fromString sidecar <> "\n"
  where
    sidecar = sidecarOf file
    bytes   = rpayload r
    digest  = sha256Hex bytes
    lopd    = either (const Nothing) (Just . level0DiscoveryHeaderLengthOfParameterData . drRecord)
                     (decodeRecord bytes :: Either LayoutError (DecodedRecord Level0DiscoveryHeader))

-- | Where a capture's sidecar goes: the capture with its extension replaced
--   by @.json@.
--
-- >>> sidecarOf "spec/fixtures/disk6-level0.bin"
-- "spec/fixtures/disk6-level0.json"
sidecarOf :: FilePath -> FilePath
sidecarOf file = replaceExtension file "json"

-- | A JSON object, a key and its value to a line, in the order given.
renderObject :: [(Text, A.Value)] -> Text
renderObject fields = "{\n" <> T.intercalate ",\n" (map line fields) <> "\n}\n"
  where
    line (k, v) = "  " <> json (A.toJSON k) <> ": " <> json v
    json = decodeUtf8Lenient . BL.toStrict . A.encode
