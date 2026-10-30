{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Drive
Description : What the jig knows of an attached drive: its interface and the identity it is held to
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The portable half of the kext transport ("Hsed.Transport.MacKext"): what the
IORegistry says of the TPer a BSD device sits on, read before anything is
opened (design 7.2). The interface type decides the CDB that carries an
interface command ("Hsed.Transport.Cdb": SAT's ATA PASS-THROUGH A1h, or
SCSI's SECURITY PROTOCOL IN A2h); the serial is the one the jig holds the
drive to (@--expect-serial@), the IDENTIFY serial with its padding trimmed;
the kext's build is recorded beside every capture (the safety posture of
design 13 depends on it). Reports print the serial masked ('maskSerial').
-}
module Hsed.Transport.Drive
  ( InterfaceType (..)
  , DriveInfo (..)
  , maskSerial
  ) where

import qualified Data.Aeson as A
import           RIO
import qualified RIO.Text   as T


-- $setup
-- >>> :set -XOverloadedStrings

-- | How the kext reaches the drive's TPer: its IORegistry @TPer Interface Type@.
data InterfaceType
  = IfSAT   -- ^ ATA commands through SCSI-ATA Translation (the RTL9210 bridge of disk6)
  | IfSCSI  -- ^ SCSI commands
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A drive's TPer as the IORegistry describes it.
data DriveInfo = DriveInfo
  { diBsd        :: !Text           -- ^ the BSD name, @disk6@
  , diSerial     :: !Text           -- ^ the IDENTIFY serial, trimmed
  , diKextBuild  :: !Text           -- ^ the loaded kext's build (its CFBundleVersion)
  , diInterface  :: !InterfaceType
  , diRegistryId :: !Word64         -- ^ the TPer's IORegistry entry id
  }
  deriving (Eq, Show)

-- | A serial with all but its last four characters starred out, as reports
--   and the capture's sidecar print it.
--
-- >>> maskSerial "S59BNM0RA15802W"
-- "***********802W"
maskSerial :: Text -> Text
maskSerial s = T.replicate (T.length s - shown) "*" <> T.takeEnd shown s
  where shown = min 4 (T.length s)

instance A.ToJSON InterfaceType where
  toJSON = A.String . \case
    IfSAT  -> "sat"
    IfSCSI -> "scsi"

-- | The serial masked.
instance A.ToJSON DriveInfo where
  toJSON d = A.object
    [ "bsd"         A..= diBsd d
    , "serial"      A..= maskSerial (diSerial d)
    , "kext_build"  A..= diKextBuild d
    , "interface"   A..= diInterface d
    , "registry_id" A..= diRegistryId d
    ]
