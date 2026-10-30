{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.MacKext
Description : The attached drive through the BrightPlaza TPer kext: a 'ReadOnly transport, Level 0 IF-RECV only
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The drive's transport on macOS (design 7.2), through a C shim over IOKit
(@cbits/hsed_kext.c@) compiled only under the package's manual flag @kext@:
it locates the TPer under a BSD device in the IORegistry, opens the kext's
user client, issues the one command M3 needs, IF-RECV of Level 0 Discovery,
and closes it. A @'ReadOnly@ transport: no IF-SEND exists on it, in the
types or in the shim.

Built without the flag (the default, and CI's), 'openKext' refuses.
-}
module Hsed.Transport.MacKext
  ( KextOpts (..)
  , openKext
  ) where

import           RIO

import           Hsed.Transport.Drive (DriveInfo)
import           Hsed.Wire            (Cap (..), Transport)


-- | The drive to open: its BSD name, and the serial it must have.
data KextOpts = KextOpts
  { koBsd          :: !Text
  , koExpectSerial :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | The drive's transport and what the IORegistry says of it, or why not.
openKext :: KextOpts -> IO (Either Text (Transport 'ReadOnly, DriveInfo))
openKext _ = pure (Left "built without -fkext")
