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
  , ci
  , cu
  , cb
  , cm
  )
where

import           System.SED.MCTP.Common.Base_Type.Class (Core_bytes (..),
                                                         Core_integer (..),
                                                         Core_max_bytes (..),
                                                         Core_some_bytes (..),
                                                         Core_some_integer (..),
                                                         Core_some_max_bytes (..),
                                                         Core_some_uinteger (..),
                                                         Core_uinteger (..))
import           System.SED.MCTP.Common.Base_Type.TH    (cb, ci, cm, cu)
