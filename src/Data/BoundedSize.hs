{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
       , fx
       , mx
       , unsafeCreateExp
       , typeFromInt
       , AtLeast
       )
where

import           Data.BoundedSize.Class (FixedSize, HasSize (..),
                                         IsBoundedSize (..), IsFixedSize,
                                         IsMaxSize, MaxSize)
import           Data.BoundedSize.TH    (fx, mx, typeFromInt, unsafeCreateExp)
import           Data.IsBytes           (IsBytes (..))
import           GHC.TypeLits           (type (<=), KnownNat)

-- | Class of types which can be assigned a type-level minimum and maximum length.
class (IsBoundedSize l u a, IsBytes a) => IsBoundedSizeBytes l u a
instance (KnownNat l, KnownNat u, IsBytes a) => IsBoundedSizeBytes l u a

class (IsFixedSize n a, IsBytes a) => IsFixedSizeBytes n a
instance (KnownNat n, IsBytes a) => IsFixedSizeBytes n a

class (IsMaxSize n a, IsBytes a) => IsMaxSizeBytes n a
instance (KnownNat n, IsBytes a) => IsMaxSizeBytes n a

-- | Sometimes a MaxSize value is given, but the only thing known about
--   the "max" is that the value is legal, e.g. "foo" could be a MaxSize 3 String,
--   or a MaxSize 4 String, so it is AtLeast MaxSize 3
--   Then it can be later cast to a specific MaxSize (of at least 3).
type AtLeast c m =  forall l. (KnownNat l, m <= l) => c l
