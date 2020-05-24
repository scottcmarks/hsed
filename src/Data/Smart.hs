{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Data.Smart
Description : Smart constructors
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Smart constructors refined with predicates.

-}

module Data.Smart
  (
    Smart (..)
  , IsBoundedSize(..)
  , IsFixedSize
  , FixedSize
  , IsBoundedSizeText(..)
  , IsFixedSizeText
  , FixedSizeText
  )
where

import           Data.ByteString     as B
import           Data.Either
import           Data.Maybe
import           Data.Proxy          (Proxy (..))
import           Data.String
import           GHC.Base            (Eq (..), Int, Nat, Ord (..), const, error,
                                      id, ($), (&&), (++), (.))
import           GHC.Show            (Show (..))
import           GHC.TypeLits        (KnownNat)
import           GHC.TypeLits.Extras (fromNat)
import           GHC.Word            (Word8 (..))


class Smart c a where
    type ErrorMessage c a

    -- | Forget type-level minimum and maximum size, obtaining the underlying value.
    unwrap :: c a -> a


    -- | Simply wrap a value in a BoundedSize as is, assuming any length.
    --
    -- __WARNING__ Use it only when you know what you're doing.
    --
    -- For example, an expression like
    --
    -- > unsafeCreate "somestring" :: BoundedSize 50 100 String
    --
    -- will typecheck, although the stored size information will not
    -- match actual value size. This may result in wrong behaviour of
    -- all functions defined for "IsBoundedSize".
    --
    -- When writing new "IsBoundedSize" instances, make this simply apply
    -- the constructor of "BoundedSize".
    unsafeCreate :: a -> c a

    -- | Refinement predicate.  Either pass a wrapped value as Right or
    --   give an error message as Left
    predicate :: c a -> Either (ErrorMessage c a) (c a)

    -- | Primary smart constructor into Either.
    --
    safeCreate :: a -> Either (ErrorMessage c a) (c a)
    safeCreate = predicate . unsafeCreate

    -- | Simplified smart constructor into Maybe.
    create :: a -> Maybe (c a)
    create = either (const Nothing) Just . safeCreate


-- | Class of types which can be assigned a type-level minimum and maximum length.
class IsBoundedSize (l::Nat) (u::Nat) a where
    -- | Data family which wraps values of the underlying type giving
    -- them a type-level size. @BoundedSize 6 10 t@ means a value of type @t@ of
    -- size between 6 and 10.
    data BoundedSize l u a
    size :: a -> Int

-- | Class of types which can be assigned a fixed type-level size.
class (IsBoundedSize l l a) => IsFixedSize l a where

type FixedSize l a = (IsBoundedSize l l a) => BoundedSize l l a



-- | Class of types which can be assigned a type-level minimum and maximum length.
class (IsBoundedSize l u a) => IsBoundedSizeText l u a where
  -- | Basic element type. For @IsBoundedSizeTextText [a]@, this is @a@.
  type Elem a

  length :: a -> Int

  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a

class (IsBoundedSizeText l l a) => IsFixedSizeText l a where

type FixedSizeText l a =(IsFixedSizeText l a) => FixedSize l a

instance forall l u. IsBoundedSize l u B.ByteString where
    data BoundedSize l u B.ByteString = ByteString B.ByteString
        deriving (Eq, Ord)
    size = B.length

instance forall l u. (KnownNat l, KnownNat u) => Smart (BoundedSize l u) B.ByteString where
    type ErrorMessage (BoundedSize l u) B.ByteString = String
    unsafeCreate = ByteString
    unwrap (ByteString t) = t
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

instance forall l u a. (KnownNat l, KnownNat u, Show a, Smart (BoundedSize l u) a) => Show (BoundedSize l u a) where
    show = show . unwrap

instance forall l u.Smart (BoundedSize l u) B.ByteString => IsString (BoundedSize l u B.ByteString) where
    fromString = either error id . safeCreate . fromString

instance forall l u. IsBoundedSizeText l u B.ByteString where
  type Elem B.ByteString = Word8
  length = B.length
  append = B.append
  replicate = B.replicate
  map = B.map
  take = B.take
  drop = B.drop
