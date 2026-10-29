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
  ( Implementation_integer       (..)
  , Implementation_uinteger      (..)
  , Implementation_bytes         (..)
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
  , ci'
  , cu
  , cu'
  , cb'
  , cm
  , cm'
  )
where

import           GHC.Base                               (($))
import           GHC.Exts                               (IsList (..))
import           GHC.Word                               (Word8 (..))
import           System.SED.MCTP.Common.Base_Type.TH    (cb', ci, ci', cm, cm',
                                                         cu, cu')
import           System.SED.MCTP.Common.Base_Type.Types (Core_bytes (..),
                                                         Core_integer (..),
                                                         Core_max_bytes (..),
                                                         Core_uinteger (..),
                                                         Implementation_bytes (..),
                                                         Implementation_integer (..),
                                                         Implementation_uinteger (..),
                                                         append, bounds, drop,
                                                         length, map, padLeft,
                                                         padRight, replicate,
                                                         safeCreate, take,
                                                         unsafeCreate)



core_bytes_4 ::
    Word8 -> Word8 -> Word8 -> Word8
 -> Core_bytes 4
core_bytes_4 b3 b2 b1 b0 =
    Core_bytes $ unsafeCreate $ fromList [b3, b2, b1, b0]


core_bytes_8 ::
    Word8 -> Word8 -> Word8 -> Word8
 -> Word8 -> Word8 -> Word8 -> Word8
 -> Core_bytes 8
core_bytes_8 b7 b6 b5 b4 b3 b2 b1 b0 =
    Core_bytes $ unsafeCreate $ fromList [b7, b6, b5, b4, b3, b2, b1, b0]
