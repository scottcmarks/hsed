{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}


{-|
Module      : System.SED.MCTP.Common.Base_Type
Description : Core data type formats
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Formats.

-}

module System.SED.MCTP.Common.Base_Type
  ( Core_some_integer       (..)
  , Core_some_uinteger      (..)
  , Core_some_bytes         (..)
  , Core_integer       (..)
  , Core_uinteger      (..)
  , Core_bytes         (..)
  , Core_max_bytes     (..)
  , safeCreate
  , core_bytes_4
  , core_bytes_8
  , toList
  , append
  , replicate
  , map
  , take
  , drop
  , bounds
  , length
  , padLeft
  , padRight

  , ci
  , cu
  , cb
  , cm
  )
where

import           Data.ByteString                        (pack)
import           GHC.Base                               (($))
import           GHC.Word                               (Word8 (..))
import           System.SED.MCTP.Common.Base_Type.Class (Core_bytes (..),
                                                         Core_integer (..),
                                                         Core_max_bytes (..),
                                                         Core_some_bytes (..),
                                                         Core_some_integer (..),
                                                         Core_some_uinteger (..),
                                                         Core_uinteger (..),
                                                         append, bounds, drop,
                                                         length, map, padLeft,
                                                         padRight, replicate,
                                                         safeCreate, take,
                                                         toList, unsafeCreate)
import           System.SED.MCTP.Common.Base_Type.TH    (cb, ci, cm, cu)



core_bytes_4 ::
    Word8 -> Word8 -> Word8 -> Word8
 -> Core_bytes 4
core_bytes_4 b3 b2 b1 b0 =
    Core_bytes $ unsafeCreate $ Core_some_bytes $ pack [b3, b2, b1, b0]


core_bytes_8 ::
    Word8 -> Word8 -> Word8 -> Word8
 -> Word8 -> Word8 -> Word8 -> Word8
 -> Core_bytes 8
core_bytes_8 b7 b6 b5 b4 b3 b2 b1 b0 =
    Core_bytes $ unsafeCreate $ Core_some_bytes $ pack [b7, b6, b5, b4, b3, b2, b1, b0]
