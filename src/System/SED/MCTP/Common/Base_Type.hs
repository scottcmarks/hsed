{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

import           Data.BoundedSize (BoundedSize)
import           Data.ByteString  (ByteString)
import           GHC.Num          (Integer)
import           Numeric.Natural  (Natural)


newtype Core_some_integer  = Core_some_integer  Integer
newtype Core_some_uinteger = Core_some_uinteger Natural
newtype Core_some_bytes    = Core_some_bytes    ByteString
newtype Core_some_maxbytes = Core_some_maxbytes ByteString

newtype Core_integer   n = Core_integer   (BoundedSize Core_some_integer  n n)
newtype Core_uinteger  n = Core_uinteger  (BoundedSize Core_some_uinteger n n)
newtype Core_bytes     n = Core_bytes     (BoundedSize Core_some_bytes    n n)
newtype Core_max_bytes n = Core_max_bytes (BoundedSize Core_some_maxbytes 0 n)
