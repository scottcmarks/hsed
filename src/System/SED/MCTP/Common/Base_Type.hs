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
  ( Core_some_integer  (..)
  , Core_some_uinteger (..)
  , Core_some_bytes    (..)
  , Core_some_max_bytes (..)
  , Core_integer       (..)
  , Core_uinteger      (..)
  , Core_bytes         (..)
  , Core_max_bytes     (..)
  )
where

import           Data.BoundedSize                  (FixedSize, HasSize (..),
                                                    IsBytes (..), MaxSize)
import           Data.ByteString                   (ByteString)
import           Data.String                       (IsString (..))
import           GHC.Base                          (undefined)
import           GHC.Classes                       (Eq (..), Ord (..))
import           GHC.Num                           (Integer, Num)
import           GHC.Read                          (Read (..))
import           GHC.Show                          (Show (..))
import           Numeric.Natural                   (Natural)
import           System.SED.MCTP.Common.StreamItem


newtype Core_some_integer  = Core_some_integer  Integer
        deriving (Eq, Ord, HasSize, Num, Read, Show) via (Integer)
newtype Core_some_uinteger = Core_some_uinteger Natural
        deriving (Eq, Ord, HasSize, Num, Read, Show) via (Natural)
newtype Core_some_bytes    = Core_some_bytes    ByteString
        deriving (Eq, Ord, HasSize, IsBytes, IsString, Read, Show) via ByteString
newtype Core_some_max_bytes = Core_some_max_bytes ByteString
        deriving (Eq, Ord, HasSize, IsBytes, IsString, Read, Show) via ByteString


newtype Core_integer   n = Core_integer   (FixedSize n Core_some_integer )
        deriving (Eq, Ord, HasSize, Num, Read, Show) via (FixedSize n Core_some_integer )
newtype Core_uinteger  n = Core_uinteger  (FixedSize n Core_some_uinteger)
        deriving (Eq, Ord, HasSize, Num, Read, Show) via (FixedSize n Core_some_uinteger)

newtype Core_bytes     n = Core_bytes     (FixedSize n Core_some_bytes   )
        deriving (Eq, Ord, HasSize, IsString, Show) via (FixedSize n Core_some_bytes)
newtype Core_max_bytes n = Core_max_bytes (MaxSize   n Core_some_max_bytes)
        deriving (Eq, Ord, HasSize, IsString, Show) via (MaxSize   n Core_some_max_bytes)




instance StreamItem (Core_uinteger n ) where
    parser = undefined
    generate _ = "<uinteger_n>"
instance StreamItem (Core_integer n  ) where
    parser = undefined
    generate _ = "<integer_n>"
instance StreamItem (Core_max_bytes n) where
    parser = undefined
    generate _ = "<max_bytes_n>"
