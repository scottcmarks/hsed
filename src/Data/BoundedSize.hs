{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
{-|

sized-text combinators are defined for members of 'IsBoundedSize'
class. The package includes 'IsBoundedSize' instances for several
common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.BoundedSize as S

-}
module Data.BoundedSize
  (
    -- * Constructing bounded-size values
    --
    -- | See also 'unsafeCreate'
    create

    -- * Working with bounded-size values
  , size

    -- * IsBoundedSize class
  , BoundedSize
  , IsBoundedSize(unwrap)

    -- * FixedSize class @FixedSize l a == BoundedSize l l a
  , FixedSize
       , IsBoundedSizeText(..)
   , FixedSizeText
)
where

import           Data.ByteString     ()
import qualified Data.ByteString     as B
import           Data.Either         (Either (..), either)
import           Data.Maybe          (Maybe (..), maybe)
import           Data.Proxy          (Proxy (..))
import           Data.String         (IsString (..), String)
import           GHC.Base            (const, id, ($), (++), (.))
import           GHC.Classes         (Eq (..), Ord (..), (&&))
import           GHC.Err             (error)
import           GHC.Show            (Show (..))
import           GHC.TypeLits        (KnownNat, Nat)
import           GHC.TypeLits.Extras (fromNat)
import           GHC.Types           (Int)
import           GHC.Word            (Word8)


-- -- $setup
-- -- >>> :set -XDataKinds
-- -- >>> :set -XTemplateHaskell
-- -- >>> :set -XOverloadedStrings
-- -- >>> :set -XTypeApplications
-- -- >>> :set -Wno-type-defaults
-- -- >>> import           Data.Proxy
-- -- >>> import           Data.ByteString      (ByteString)
-- -- >>> import           Data.Char(toUpper)
-- -- >>> import           Data.String          (IsString (..))


-- -- $setup
-- -- >>> :set -XDataKinds
-- -- >>> :set -XTemplateHaskell
-- -- >>> :set -XOverloadedStrings
-- -- >>> :set -XTypeApplications
-- -- >>> import           Data.Proxy


-- | Class of types which can be assigned a type-level minimum and maximum length.
class IsBoundedSize (l::Nat) (u::Nat) a where
  -- | Data family which wraps values of the underlying type giving
  -- them a type-level size. @BoundedSize 6 10 t@ means a value of type @t@ of
  -- size between 6 and 10.
  data BoundedSize l u a

  -- | Simply wrap a value in a BoundedSize as is, assuming any length.
  --
  -- __WARNING__ Use it only when you know what you're doing.
  --
  -- For example, an expression like
  --
  -- > unsafeCreate "somestring" :: BoundedSize String 50 100
  --
  -- will typecheck, although the stored size information will not
  -- match actual value size. This may result in wrong behaviour of
  -- all functions defined for "IsBoundedSize".
  --
  -- When writing new "IsBoundedSize" instances, make this simply apply
  -- the constructor of "BoundedSize".
  unsafeCreate :: a -> BoundedSize l u a

  -- | Forget type-level minimum and maximum size, obtaining the underlying value.
  unwrap :: BoundedSize l u a -> a


  size :: a -> Int

  predicate :: (KnownNat l, KnownNat u) => BoundedSize l u a -> Either String (BoundedSize l u a)

  safeCreate :: (KnownNat l, KnownNat u) => a -> Either String (BoundedSize l u a)
  safeCreate = predicate . unsafeCreate

  create :: (KnownNat l, KnownNat u) => a -> Maybe (BoundedSize l u a)
  create = either (const Nothing) Just . safeCreate



-- | Class of types which can be assigned a fixed type-level size.
type FixedSize (l :: Nat) a = BoundedSize l l a



-- | Class of types which can be assigned a type-level minimum and maximum length.
class (IsBoundedSize l u a) => IsBoundedSizeText l u a where
  -- | Data family which wraps values of the underlying Text-like type giving
  -- them a type-level length. @BoundedSizeText 6 10 t@ means a value of type @t@ of
  -- length between 6 and 10.

  -- | Basic element type. For @IsBoundedSizeTextText [a]@, this is @a@.
  type Elem a

  length :: a -> Int

  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a



instance forall l u a. (IsString a, KnownNat l, KnownNat u, IsBoundedSizeText l u a) => IsString(BoundedSize l u a)
  where fromString s =
            maybe (error "prohibits coercion to BoundedSizeText") id
                  (create (fromString s))


type FixedSizeText (l :: Nat) a = (IsBoundedSizeText l l a) => FixedSize l a

instance forall l u. (KnownNat l, KnownNat u) => IsBoundedSize l u B.ByteString where
  data BoundedSize l u B.ByteString = ByteString B.ByteString
    deriving (Eq, Ord)
  unsafeCreate = ByteString
  unwrap (ByteString t) = t
  size = B.length
  predicate b@(ByteString t) =
      let lt = B.length t
          vl = fromNat (Proxy @l)
          vu = fromNat (Proxy @u)
      in if vl <= lt && lt <= vu
           then Right b
           else Left $ "length " ++ show lt ++ " should be " ++
                if vl == vu
                then show vl
                else "between " ++ show vl ++ " and " ++ show vu

instance forall l u. (KnownNat l, KnownNat u) => IsBoundedSizeText l u B.ByteString where
  type Elem B.ByteString = Word8
  length = B.length
  append = B.append
  replicate = B.replicate
  map = B.map
  take = B.take
  drop = B.drop

instance forall l u a. (Show a, KnownNat l, KnownNat u, IsBoundedSize l u a) => Show (BoundedSize l u a) where
    show = show . unwrap
