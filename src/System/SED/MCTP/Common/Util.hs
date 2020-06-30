{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-|
Module      : System.SED.MCTP.Common.Util
Description : Utilities
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Utilities

-}

{-# LANGUAGE NoImplicitPrelude #-}


module System.SED.MCTP.Common.Util where

import           Data.Attoparsec.ByteString.Char8 (isSpace_w8)
import           Data.ByteString                  (ByteString, filter, null,
                                                   spanEnd)
import           Data.Either                      (either)
import           Data.Tuple                       (fst)
import           GHC.Base                         (id, not, ($), (.))
import           GHC.Err                          (error)
import           GHC.Show                         (showString, shows)

import           Data.ByteString.Base16           (decode)

import           System.SED.MCTP.Common.Base_Type (Core_bytes (..),
                                                   Core_some_bytes (..),
                                                   safeCreate)
import           System.SED.MCTP.Common.UID

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
      cf = Core_bytes . either error id . safeCreate . Core_some_bytes . bytesfn
      bytesfn :: ByteString -> ByteString
      bytesfn hexBytes =
          if null remainder
            then bytes
            else error $ showString "Invalid UID bytes: " . shows hexBytes $ ""
          where
            (bytes,remainder) = decode $ filter (not . isSpace_w8) hexBytes



-- | Trim trailing whitespace from e.g. a field in a quasiquoted table
trimTrailingWhitespace :: ByteString -> ByteString
trimTrailingWhitespace s = fst $ spanEnd isSpace_w8 s
