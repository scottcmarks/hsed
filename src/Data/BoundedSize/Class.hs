
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
       , IsBoundedSize(..)
       , IsFixedSize
       , FixedSize
       , IsMaxSize
       , MaxSize
       , AtLeast
       )

where

import           Data.Either         (Either (..), either)
import           Data.Functor        ((<$>))
import           Data.HasSize        (HasSize (..))
import           Data.Proxy          (Proxy (..))
import           Data.Smart          (ErrorMessage (..), Smart (..))
import           Data.String         (IsString (..), String)
import           GHC.Base            (id, ($), (++), (.))
import           GHC.Classes         (Eq (..), Ord (..), (&&))
import           GHC.Err             (error)
import           GHC.Num             (Num (..))
import           GHC.Read            (Read (..))
import           GHC.Show            (Show (..))
import           GHC.TypeLits        (type (<=), KnownNat)
import           GHC.TypeLits.Extras (fromNat)


-- -- $setup
-- -- >>> :set -XDataKinds
-- -- >>> :set -XTemplateHaskell
-- -- >>> :set -XOverloadedStrings
-- -- >>> :set -XTypeApplications
-- -- >>> import           Data.Proxy



-- | Class of types which can be assigned a type-level minimum and maximum size.
--
class (KnownNat l, KnownNat u, HasSize a) => IsBoundedSize l u a where
    data BoundedSize l u a
instance (KnownNat l, KnownNat u, HasSize a) => IsBoundedSize l u a where
    data BoundedSize l u a = BdSzWrapper a
        deriving (Eq, Ord)

-- | Instance of Smart constructor for values with type-level minimum and maximum
--   size.
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

-- | Class of types which can be assigned a fixed type-level size.
class (IsBoundedSize l l a) => IsFixedSize l a
instance (IsBoundedSize l l a) => IsFixedSize l a
type FixedSize n a = BoundedSize n n a

-- | Class of types which can be assigned a maximum type-level size.
class (IsBoundedSize 0 l a) => IsMaxSize l a
instance (IsBoundedSize 0 l a) => IsMaxSize l a
type MaxSize n a = BoundedSize 0 n a




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
    (*) = bitrans (*)
    abs = trans abs
    signum = trans signum
    fromInteger = either error id . safeCreate . fromInteger

instance (Read a, Smart (BoundedSize l u) a) => Read ((BoundedSize l u) a) where
    readPrec = either error id . safeCreate <$> readPrec

instance (Show a, Smart (BoundedSize l u) a) => Show ((BoundedSize l u) a) where
    show = show . unwrap

-- | Sometimes a MaxSize value is given, but the only thing known about
--   the "max" is that the value is legal, e.g. "foo" could be a MaxSize 3 String,
--   or a MaxSize 4 String, so it is AtLeast MaxSize 3
--   Then it can be later cast to a specific MaxSize (of at least 3).
type AtLeast c m =  forall l. (KnownNat l, m <= l) => c l
