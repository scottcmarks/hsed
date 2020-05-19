{-# LANGUAGE DataKinds #-}
{-|
Module      : System.SED.MCTP.Common.BaseTypes
Description : Core data type formats
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Formats.

-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}


module System.SED.MCTP.Common.BaseTypes
  (
    Core_integer
  , Core_uinteger
  , Core_bytes
  , Core_maxbytes

  )
    where

import           Data.ByteString (ByteString)
import           Data.SizedText  (Sized)
import           GHC.Num         (Integer)
import           Numeric.Natural (Natural)


type Core_integer  n = Sized Integer    n n
type Core_uinteger n = Sized Natural    n n
type Core_bytes    n = Sized ByteString n n
type Core_maxbytes n = Sized ByteString 0 n
