{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-|
Module      : Hsed.Core.Util
Description : Utilities
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Utilities

-}

{-# LANGUAGE NoImplicitPrelude #-}


module Hsed.Core.Util where

import           Data.Attoparsec.ByteString.Char8 (isSpace_w8)
import           Data.ByteString                  (ByteString, filter, null,
                                                   spanEnd)
import           Data.Either                      (either)
import           Data.Tuple                       (fst)
import           GHC.Base                         (id, not, ($), (.))
import           GHC.Err                          (error)
import           GHC.Show                         (showString, shows)

import           Data.ByteString.Base16           (decode)
#if MIN_VERSION_base16_bytestring(1,0,0)
import           Data.ByteString                  (empty)
import           GHC.Base                         (const)
#endif

import           Hsed.Core.Base_Type              (Core_bytes (..),
                                                   Implementation_bytes (..),
                                                   safeCreate)
import           Hsed.Core.UID

-- | Convert to a UID from a string of eight hex digit pairs divided by a single space
--
--   E.g. "01 02 03 04 05 06 07 08"
--
--   Throws an error on any other input.
--   This means that if used in a quasiquoter, the malformed string will not compile.
--
hexUID :: ByteString -> UID
hexUID = UID . cf
    where
      cf :: ByteString -> Core_bytes 8
      cf = Core_bytes . either error id . safeCreate . Implementation_bytes . bytesfn
      bytesfn :: ByteString -> ByteString
      bytesfn hexBytes =
          if null remainder
            then bytes
            else error $ showString "Invalid UID bytes: " . shows hexBytes $ ""
          where
            (bytes,remainder) = decode16 $ filter (not . isSpace_w8) hexBytes

-- | 'Data.ByteString.Base16.decode' with its pre-1.0 @(decoded, remainder)@ interface:
--   base16-bytestring >= 1.0 returns @Either String ByteString@ instead.
decode16 :: ByteString -> (ByteString, ByteString)
#if MIN_VERSION_base16_bytestring(1,0,0)
decode16 s = either (const (empty, s)) (\b -> (b, empty)) (decode s)
#else
decode16 = decode
#endif


-- | Trim trailing whitespace from e.g. a field in a quasiquoted table
trimTrailingWhitespace :: ByteString -> ByteString
trimTrailingWhitespace s = fst $ spanEnd isSpace_w8 s
