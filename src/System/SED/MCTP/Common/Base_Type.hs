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
  ( Core_integer       (..)
  , Core_uinteger      (..)
  , Core_bytes         (..)
  , Core_max_bytes     (..)
  , safeCreate
  , toList
  , ci
  , cu
  , cb
  , cm
  )
where

import           System.SED.MCTP.Common.Base_Type.Class (Core_bytes (..),
                                                         Core_integer (..),
                                                         Core_max_bytes (..),
                                                         Core_uinteger (..),
                                                         safeCreate, toList)
import           System.SED.MCTP.Common.Base_Type.TH    (cb, ci, cm, cu)
