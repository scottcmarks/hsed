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

import qualified Data.ByteString     as B
import           Data.Either         (Either (..), either)
import           Data.HasSize        (HasSize (..))
import           Data.IsBytes        (IsBytes (..))
import           Data.Proxy          (Proxy (..))
import           Data.Smart          (Smart (..))
import           Data.String         (IsString (..), String)
import           GHC.Base            (id, ($), (++), (.))
import           GHC.Classes         (Eq (..), Ord (..), (&&))
import           GHC.Err             (error)
import           GHC.Show            (Show (..))
import           GHC.TypeLits        (KnownNat, Nat)
import           GHC.TypeLits.Extras (fromNat)
-- import           Prelude             hiding (drop, length, replicate, take)

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


instance (IsString a, Smart (BoundedSize l u) a) => IsString ((BoundedSize l u) a) where
    fromString = either error id . safeCreate . fromString

instance IsBoundedSizeBytes l u B.ByteString where {}




instance (Show a, Smart (BoundedSize l u) a) => Show ((BoundedSize l u) a) where
    show = show . unwrap
