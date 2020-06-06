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

import           Data.Attoparsec.ByteString.Char8 (hexadecimal, isSpace_w8,
                                                   parseOnly)
import           Data.ByteString                  (ByteString, pack, spanEnd)
import           Data.ByteString.Char8            (split)
import           Data.Either                      (either)
import           Data.Tuple                       (fst)
import           GHC.Base                         (id, map, ($), (.))
import           GHC.Err                          (error)

import           Data.Smart                       (unsafeCreate)
import           System.SED.MCTP.Common.Base_Type (Core_bytes (..))
import           System.SED.MCTP.Common.UID

-- | Convert to a UID from a string of eight hex digit pairs divided by a single space
--
--   E.g. "01 02 03 04 05 06 07 08"
--
--   Throws an error on any other input.
--   This means that if used in a quasiquoter, the malformed string will not compile.
--
hexUID :: ByteString -> UID
hexUID = UID . Core_bytes . unsafeCreate . pack . map (either error id . parseOnly hexadecimal) . split ' '

-- | Trim trailing whitespace from e.g. a field in a quasiquoted table
trimTrailingWhitespace :: ByteString -> ByteString
trimTrailingWhitespace s = fst $ spanEnd isSpace_w8 s
