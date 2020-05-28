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



{-|

Use this module when you need to add an 'IsBoundedSize' instance to a
type.

-}

module Data.BoundedSize.Class
       ( HasSize(..)
       , IsBytes(..)
       , IsBoundedSize(..)
       , FixedSize
       , MaxSize
       , IsBoundedSizeBytes
       , BoundedSizeBytes
       , FixedSizeBytes
       , MaxSizeBytes
       , BoundedSizeByteString
       , FixedSizeByteString
       , MaxSizeByteString
       )

where

import qualified Data.ByteString     as B
import           Data.Either         (Either (..), either)
import           Data.Maybe          (Maybe (..))
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
-- import           Prelude             hiding (drop, length, replicate, take)

-- -- $setup
-- -- >>> :set -XDataKinds
-- -- >>> :set -XTemplateHaskell
-- -- >>> :set -XOverloadedStrings
-- -- >>> :set -XTypeApplications
-- -- >>> import           Data.Proxy


class HasSize a where
    size :: a -> Int

-- | Class of types which can be assigned a type-level minimum and maximum length.
class (HasSize a) => IsBoundedSize (l::Nat) (u::Nat) a where
  -- | Data family which wraps values of the underlying type giving
  -- them a type-level size. @BoundedSize t 6 10@ means a value of type @t@ of
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

  predicate :: (KnownNat l, KnownNat u) => BoundedSize l u a -> Either String (BoundedSize l u a)

  safeCreate :: (KnownNat l, KnownNat u) => a -> Either String (BoundedSize l u a)
  safeCreate = predicate . unsafeCreate

  create :: (KnownNat l, KnownNat u) => a -> Maybe (BoundedSize l u a)
  create = either (const Nothing) Just . safeCreate


instance (Show a, KnownNat l, KnownNat u, IsBoundedSize l u a) =>
         Show (BoundedSize l u a)
  where
    show = show . unwrap

instance (IsString a, KnownNat l, KnownNat u, IsBoundedSize l u a) =>
         IsString(BoundedSize l u a)
  where
    fromString = either error id . safeCreate . fromString


-- | Class of types which can be assigned a fixed type-level size.
type FixedSize n a = BoundedSize n n a

-- | Class of types which can be assigned a maximum type-level size.
type MaxSize n a = BoundedSize 0 n a


instance (KnownNat l, KnownNat u, HasSize a) => IsBoundedSize l u a where
  data BoundedSize l u a = BdSz a
    deriving (Eq, Ord)
  unsafeCreate = BdSz
  unwrap (BdSz t) = t
  predicate b@(BdSz t) =
      let lt = size t
          vl = fromNat (Proxy @l)
          vu = fromNat (Proxy @u)
      in if vl <= lt && lt <= vu
           then Right b
           else Left $ "length " ++ show lt ++ " should be " ++
                       if vl == vu
                       then show vl
                       else "between " ++ show vl ++ " and " ++ show vu








-- | Class of types with ByteString-like operations
class (HasSize a) => IsBytes a where
  type Elem a
  length :: a -> Int
  length = size
  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a

-- | Class of ByteString-like types which can be assigned
--   a type-level minimum and maximum length.
class (IsBoundedSize l u a, IsBytes a) => IsBoundedSizeBytes l u a where { }




type BoundedSizeBytes l u a = IsBoundedSizeBytes l u a => BoundedSize l u a

type FixedSizeBytes n a = IsBoundedSizeBytes n n a => FixedSize n a

type MaxSizeBytes n a = IsBoundedSizeBytes n n a => MaxSize n a





instance HasSize B.ByteString
  where
      size = B.length
instance IsBytes B.ByteString
  where
    type Elem B.ByteString = Word8
    append = B.append
    replicate = B.replicate
    map = B.map
    take = B.take
    drop = B.drop


type BoundedSizeByteString l u = BoundedSizeBytes l u B.ByteString

type FixedSizeByteString n = FixedSizeBytes n B.ByteString

type MaxSizeByteString n = MaxSizeBytes n B.ByteString
