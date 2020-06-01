{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

{-|

sized-text combinators are defined for members of 'IsBoundedSize'
class. The package includes 'IsBoundedSize' instances for several
common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.BoundedSize as S

-}
module Data.BoundedSize
       ( HasSize(..)
       , IsBytes(..)
       , IsBoundedSize(..)
       , IsFixedSize
       , FixedSize
       , IsMaxSize
       , MaxSize
       , IsBoundedSizeBytes
       , IsFixedSizeBytes
       , IsMaxSizeBytes
       , AtLeast
       , fx
       , mx
       , unsafeCreateExp
       , typeFromInt
       )
where

import           Data.BoundedSize.Class (AtLeast, FixedSize, HasSize (..),
                                         IsBoundedSize (..), IsFixedSize,
                                         IsMaxSize, MaxSize)
import           Data.BoundedSize.TH    (fx, mx, typeFromInt, unsafeCreateExp)
import           Data.IsBytes           (IsBytes (..))
import           GHC.TypeLits           (KnownNat)

-- | Class of types which can be assigned a type-level minimum and maximum length.
class (IsBoundedSize l u a, IsBytes a) => IsBoundedSizeBytes l u a
instance (KnownNat l, KnownNat u, IsBytes a) => IsBoundedSizeBytes l u a

class (IsFixedSize n a, IsBytes a) => IsFixedSizeBytes n a
instance (KnownNat n, IsBytes a) => IsFixedSizeBytes n a

class (IsMaxSize n a, IsBytes a) => IsMaxSizeBytes n a
instance (KnownNat n, IsBytes a) => IsMaxSizeBytes n a
