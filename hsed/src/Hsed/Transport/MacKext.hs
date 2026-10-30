{-# LANGUAGE CPP               #-}
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
(@cbits/hsed_kext.{h,c}@) compiled only under the package's manual flag
@kext@ (@stack build hsed --flag hsed:kext@, which defines @HSED_KEXT@): it
locates the TPer under a BSD device in the IORegistry, opens the kext's
user client, issues the one command M3 needs, IF-RECV of Level 0 Discovery,
and closes it. A @'ReadOnly@ transport: no IF-SEND exists on it, in the
types or in the shim; and the process must run as root, hsed's own gate
(the loaded kext, build 625, checks no privilege).

Built without the flag (the default, and CI's), 'openKext' refuses. With
it, the shim's four functions are imported here; 'openKext' over them is
milestone M3's group G4's, a stub until it lands.
-}
module Hsed.Transport.MacKext
  ( KextOpts (..)
  , openKext
#ifdef HSED_KEXT
    -- * The shim (cbits/hsed_kext.h)
  , hsedKextLocate
  , hsedKextOpen
  , hsedKextLevel0
  , hsedKextClose
#endif
  ) where

import           RIO
#ifdef HSED_KEXT
import           Foreign.C.String     (CString)
import           Foreign.C.Types      (CInt (..))
import           Foreign.Ptr          (Ptr)
#endif

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
#ifdef HSED_KEXT
openKext = error "G4: openKext"

-- | @hsed_kext_locate@: the TPer under a BSD name, its registry entry read
--   into an @hsed_kext_info@; opens nothing.
foreign import ccall safe "hsed_kext_locate"
  hsedKextLocate :: CString -> Ptr () -> IO CInt

-- | @hsed_kext_open@: the user client opened, exclusively.
foreign import ccall safe "hsed_kext_open"
  hsedKextOpen :: Ptr () -> IO CInt

-- | @hsed_kext_level0@: IF-RECV of Level 0 Discovery, a number of 512-byte
--   blocks into a page-aligned buffer of a size; the CDB used and the bytes
--   transferred written back.
foreign import ccall safe "hsed_kext_level0"
  hsedKextLevel0 :: Ptr () -> Word8 -> Ptr Word8 -> Word64 -> Ptr Word8 -> Ptr Word64 -> IO CInt

-- | @hsed_kext_close@: the user client closed.
foreign import ccall safe "hsed_kext_close"
  hsedKextClose :: Ptr () -> IO CInt
#else
openKext _ = pure (Left "built without -fkext")
#endif
