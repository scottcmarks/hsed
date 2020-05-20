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
  (
    Core_integer
  , Core_uinteger
  , Core_bytes
  , Core_max_bytes

  )
where

import           Data.ByteString (ByteString)
import           Data.SizedText  (Sized)
import           GHC.Num         (Integer)
import           Numeric.Natural (Natural)


type Core_integer   n = Sized Integer    n n
type Core_uinteger  n = Sized Natural    n n
type Core_bytes     n = Sized ByteString n n
type Core_max_bytes n = Sized ByteString 0 n
