{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
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
       ( IsBytes(..)
       , IsBoundedSize(..)
       , FixedSize
       , IsBoundedSizeBytes
       , FixedSizeBytes
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
-- import           Prelude             hiding (drop, length, replicate, take)

-- -- $setup
-- -- >>> :set -XDataKinds
-- -- >>> :set -XTemplateHaskell
-- -- >>> :set -XOverloadedStrings
-- -- >>> :set -XTypeApplications
-- -- >>> import           Data.Proxy


-- | Class of types which can be assigned a type-level minimum and maximum length.
class IsBoundedSize (l::Nat) (u::Nat) a where
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


  size :: a -> Int

  predicate :: (KnownNat l, KnownNat u) => BoundedSize l u a -> Either String (BoundedSize l u a)

  safeCreate :: (KnownNat l, KnownNat u) => a -> Either String (BoundedSize l u a)
  safeCreate = predicate . unsafeCreate

  create :: (KnownNat l, KnownNat u) => a -> Maybe (BoundedSize l u a)
  create = either (const Nothing) Just . safeCreate




-- | Class of types with ByteString-like operations
class IsBytes a where
  type Elem a
  length :: a -> Int
  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a

instance IsBytes B.ByteString where
  type Elem B.ByteString = Word8
  length = B.length
  append = B.append
  replicate = B.replicate
  map = B.map
  take = B.take
  drop = B.drop


-- | Class of ByteString-like types which can be assigned a type-level minimum and maximum length.
class (IsBoundedSize l u a, IsBytes a) => IsBoundedSizeBytes l u a where { }


instance forall l u a. (IsString a, KnownNat l, KnownNat u, IsBoundedSizeBytes l u a) => IsString(BoundedSize l u a)
  where fromString s =
            maybe (error "prohibits coercion to BoundedSizeBytes") id
                  (create (fromString s))

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


instance forall l u a. (Show a, KnownNat l, KnownNat u, IsBoundedSize l u a) => Show (BoundedSize l u a) where
    show = show . unwrap






-- | Class of types which can be assigned a fixed type-level size.
type FixedSize n a = BoundedSize n n a


type FixedSizeBytes n a = IsBoundedSizeBytes n n a => FixedSize n a
