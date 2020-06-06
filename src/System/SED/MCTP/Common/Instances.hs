{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : System.SED.MCTP.Common.Instances
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Orphan instances.

-}




module System.SED.MCTP.Common.Instances where

import           Data.BoundedSize (BoundedSize (..))
import qualified Data.ByteString  as B (ByteString, unpack)
import           Data.IsBytes     (IsBytes (..))
import           Data.Smart       (Smart (..))
import           GHC.Base         (undefined, (.))
import           GHC.TypeLits     (KnownNat)

instance (KnownNat l, KnownNat u) => IsBytes (BoundedSize l u B.ByteString)
  where
      type Elem (BoundedSize l u B.ByteString) = Elem B.ByteString
      append = undefined -- B.append
      replicate = undefined -- B.replicate
      map = undefined -- B.map
      take = undefined -- B.take
      drop = undefined -- B.drop
      toList = B.unpack . unwrap -- undefined -- toList -- TODO
