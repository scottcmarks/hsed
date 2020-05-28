{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NoImplicitPrelude    #-}
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
  ( Core_some_integer  (..)
  , Core_some_uinteger (..)
  , Core_some_bytes    (..)
  , Core_some_maxbytes (..)
  , Core_integer       (..)
  , Core_uinteger      (..)
  , Core_bytes         (..)
  , Core_max_bytes     (..)
  )
where

import           Data.ByteString  (ByteString)
import           Data.String      (IsString (..))
import           GHC.Num          (Integer)
import           GHC.Show         (Show (..))
import           Numeric.Natural  (Natural)

import           Data.BoundedSize (FixedSize, HasSize (..), IsBytes (..),
                                   MaxSize)


newtype Core_some_integer  = Core_some_integer  Integer
newtype Core_some_uinteger = Core_some_uinteger Natural
newtype Core_some_bytes    = Core_some_bytes    ByteString
        deriving (HasSize, IsBytes, IsString, Show) via ByteString
newtype Core_some_maxbytes = Core_some_maxbytes ByteString
        deriving (HasSize, IsBytes, IsString, Show) via ByteString


newtype Core_integer   n = Core_integer   (FixedSize n Core_some_integer )
newtype Core_uinteger  n = Core_uinteger  (FixedSize n Core_some_uinteger)

newtype Core_bytes     n = Core_bytes     (FixedSize n Core_some_bytes   )
        deriving (HasSize, IsString, Show) via (FixedSize n Core_some_bytes)
newtype Core_max_bytes n = Core_max_bytes (MaxSize   n Core_some_maxbytes)
        deriving (HasSize, IsString, Show) via (MaxSize   n Core_some_maxbytes)
