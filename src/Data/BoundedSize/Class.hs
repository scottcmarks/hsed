{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}



{-|

Use this module when you need to add an 'IsBoundedSize' instance to a
type.

-}

module Data.BoundedSize.Class
       ( HasSize(..)
       , BoundedSize(..)
       , IsBoundedSize
       , IsFixedSize
       , FixedSize
       , IsMaxSize
       , MaxSize
       , AtLeast
       , Refined
       , unsafeCreate
       , plain
       , type (?)
       , Predicate(..)
       , fromNat
       )

where

import           Data.HasSize    (HasSize (..))
import           Data.Proxy      (Proxy (..))
import           Data.Refined    (type (?), Predicate (..), Refined, plain,
                                  unsafeCreate)
import           GHC.Base        (Int, Monoid (..), Semigroup (..), ($), (.))
import           GHC.Classes     (Eq (..), Ord (..), (&&))
import           GHC.Num         (Num)
import           GHC.Real        (fromIntegral)
import           GHC.Show        (Show (..), showString, shows)
import           GHC.TypeLits    (type (<=), KnownNat, Nat, natVal)



import           Test.QuickCheck (Arbitrary (..), shrink, suchThatMap)


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> import           Data.Proxy(Proxy(..))
-- >>> import           GHC.Types    (Int)



-- | Class of types which can be assigned a type-level minimum and maximum size.
--
type role BoundedSize phantom phantom nominal
newtype BoundedSize (l::Nat) (u::Nat) a = BoundedSize a
    deriving (Eq, Ord, Show, Semigroup, Monoid) via a

class (KnownNat l, KnownNat u, HasSize a) => IsBoundedSize l u a where
instance (KnownNat l, KnownNat u, HasSize a) => IsBoundedSize l u a where

-- | Instance of predicate for values with type-level minimum and maximum size.
instance (KnownNat l, KnownNat u, HasSize a) => Predicate (BoundedSize l u) a where
    predicate (BoundedSize x) = lower <= n && n <= upper
       where
         lower = fromNat (Proxy @l)
         upper = fromNat (Proxy @u)
         n = size x
    failMsg (BoundedSize x) = showString "size is " . shows n . showString " but should be " $
                  if lower == upper
                    then show lower
                    else showString "between " . shows lower .
                         showString " and " $ show upper
       where
         lower = fromNat (Proxy @l) :: Int
         upper = fromNat (Proxy @u) :: Int
         n = size x



-- | Class of types which can be assigned a fixed type-level size.
class (IsBoundedSize l l a) => IsFixedSize l a
instance (IsBoundedSize l l a) => IsFixedSize l a
type FixedSize n = BoundedSize n n

-- | Class of types which can be assigned a maximum type-level size.
class (IsBoundedSize 0 l a) => IsMaxSize l a
instance (IsBoundedSize 0 l a) => IsMaxSize l a
type MaxSize n = BoundedSize 0 n


-- | Sometimes a 'MaxSize' value is given, but the only thing known about
--   the "max" is that the value is legal, e.g. "foo" could be a 'MaxSize' @3@ 'String',
--   or a 'MaxSize' @4@ 'String', so it is 'AtLeast' 'MaxSize' @3@
--   Then it can be later cast to a specific 'MaxSize' (of at least @3@).
type AtLeast c m =  forall l . (KnownNat l, m <= l) => c l


-- | 'Arbitrary' values for testing
--
instance {-# OVERLAPPING #-} (KnownNat l, HasSize a, Arbitrary a, Predicate (BoundedSize l u) a) => Arbitrary (a ? (BoundedSize l u)) where
    arbitrary = arbitrary `suchThatMap` create
    shrink t = [ unsafeCreate s | s <- shrink $ plain t, l <= size s]
       where l = fromNat (Proxy @l)



-- | Type-level 'KnownNat' to value-level 'Num'
--
-- >>> fromNat (Proxy @5) :: Int
-- 5
fromNat :: (Num b, KnownNat n) => proxy n -> b
fromNat = fromIntegral . natVal
