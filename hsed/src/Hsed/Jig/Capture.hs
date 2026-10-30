{-# LANGUAGE NoImplicitPrelude #-}

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

'captureLevel0' is milestone M3's group G4's; until it lands it is a stub.
-}
module Hsed.Jig.Capture
  ( captureLevel0
  ) where

import           RIO

import           Hsed.Transport.Drive (DriveInfo)
import           Hsed.Wire            (Response)


-- | Write a capture: the file, the drive, the CDB that asked, the response.
captureLevel0 :: FilePath -> DriveInfo -> ByteString -> Response -> IO ()
captureLevel0 = error "G4: captureLevel0"
