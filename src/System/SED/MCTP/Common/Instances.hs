{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
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

import           Data.BoundedSize (type (?), BoundedSize (..), Predicate (..))
import qualified Data.ByteString  as B (ByteString, unpack)
import           Data.IsBytes     (IsBytes (..))
import           GHC.Base         (undefined, (.))
import           GHC.TypeLits     (KnownNat)

instance (KnownNat l, KnownNat u) => IsBytes ( B.ByteString ? (BoundedSize l u))
  where
      type Elem (B.ByteString ? (BoundedSize l u)) = Elem B.ByteString
      append = undefined -- B.append
      replicate = undefined -- B.replicate
      map = undefined -- B.map
      take = undefined -- B.take
      drop = undefined -- B.drop
      toList = B.unpack . plain -- undefined -- toList -- TODO
