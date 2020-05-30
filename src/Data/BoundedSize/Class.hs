{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}



{-|

Use this module when you need to add an 'IsBoundedSize' instance to a
type.

-}

module Data.BoundedSize.Class
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
       )

where

import           Data.Either         (Either (..), either)
import           Data.Functor        ((<$>))
import           Data.HasSize        (HasSize (..))
import           Data.IsBytes        (IsBytes (..))
import           Data.Proxy          (Proxy (..))
import           Data.Smart          (ErrorMessage (..), Smart (..))
import           Data.String         (IsString (..), String)
import           GHC.Base            (id, ($), (++), (.))
import           GHC.Classes         (Eq (..), Ord (..), (&&))
import           GHC.Err             (error)
import           GHC.Num             (Num (..))
import           GHC.Read            (Read (..))
import           GHC.Show            (Show (..))
import           GHC.TypeLits        (KnownNat, Nat)
import           GHC.TypeLits.Extras (fromNat)


-- -- $setup
-- -- >>> :set -XDataKinds
-- -- >>> :set -XTemplateHaskell
-- -- >>> :set -XOverloadedStrings
-- -- >>> :set -XTypeApplications
-- -- >>> import           Data.Proxy



-- | Class of types which can be assigned a type-level minimum and maximum length.
class HasSize a => IsBoundedSize (l::Nat) (u::Nat) a where
    data BoundedSize l u a

-- | Class of types which can be assigned a fixed type-level size.
class (IsBoundedSize l l a) => IsFixedSize l a where {}
type FixedSize n a = BoundedSize n n a


-- | Class of types which can be assigned a maximum type-level size.
class (IsBoundedSize 0 l a) => IsMaxSize l a where {}
type MaxSize n a = BoundedSize 0 n a



-- | Class of types which can be assigned a type-level minimum and maximum length.
class (IsBytes a, IsBoundedSize l u a) => IsBoundedSizeBytes l u a where { }

class (IsBoundedSizeBytes l l a) => IsFixedSizeBytes l a where { }

class (IsBoundedSizeBytes 0 l a) => IsMaxSizeBytes l a where { }



instance (HasSize a) => IsBoundedSize l u a where
    data BoundedSize l u a = BdSzWrapper a
        deriving (Eq, Ord)

instance (KnownNat l, KnownNat u, HasSize a) => Smart (BoundedSize l u) a where
    type ErrorMessage (BoundedSize l u) a = String
    unsafeCreate = BdSzWrapper
    unwrap (BdSzWrapper t) = t
    predicate b =
      let lt = size (unwrap b)
          vl = fromNat (Proxy @l)
          vu = fromNat (Proxy @u)
      in if vl <= lt && lt <= vu
           then Right b
           else Left $ "length is " ++ show lt ++ " but should be " ++
                       if vl == vu
                       then show vl
                       else "between " ++ show vl ++ " and " ++ show vu

trans :: (Smart c a, ErrorMessage c a ~ String) => (a -> a) -> c a -> c a
trans f = either error id . safeCreate . f . unwrap

bitrans
  :: (Smart c a, ErrorMessage c a ~ String) =>
     (a -> a -> a) -> c a -> c a -> c a
bitrans op x y = either error id $ safeCreate $ (unwrap x) `op` (unwrap y)

instance (IsString a, Smart (BoundedSize l u) a) => IsString ((BoundedSize l u) a) where
    fromString = either error id . safeCreate . fromString

instance (HasSize a, Smart (BoundedSize l u) a) => HasSize (BoundedSize l u a)  where
    size = size . unwrap

instance (Num a, Smart (BoundedSize l u) a) => Num ((BoundedSize l u) a) where
    (+) = bitrans (+)
    (-) = bitrans (-)
    (*) = bitrans (-)
    abs = trans abs
    signum = trans signum
    fromInteger = either error id . safeCreate . fromInteger

instance (Read a, Smart (BoundedSize l u) a) => Read ((BoundedSize l u) a) where
    readPrec = either error id . safeCreate <$> readPrec

instance (Show a, Smart (BoundedSize l u) a) => Show ((BoundedSize l u) a) where
    show = show . unwrap
