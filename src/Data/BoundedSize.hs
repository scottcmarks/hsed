{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|

The 'BoundedSize' @l@ @u@ instance of 'Predicate' and various specializations;
sized-text combinators are defined for members of 'IsBoundedSize'
class. The package includes 'IsBoundedSize' instances for several
common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.BoundedSize as S

-}
module Data.BoundedSize
       ( HasSize(..)
       , BoundedSize(..)
       , IsBoundedSize
       , IsFixedSize
       , FixedSize
       , IsMaxSize
       , MaxSize
       , AtLeast
       , fx
       , mx
       , unsafeCreateExp
       , typeFromInt

       , IsBoundedSizeListOps
       , IsFixedSizeListOps
       , IsMaxSizeListOps
       , append
       , replicate
       , map
       , take
       , drop
       , bounds
       , length
       , padLeft
       , padRight

       , Refined
       , plain
       , unsafeCreate
       , type (?)
       , Predicate(..)
       , fromNat
       )
where

import           Data.BoundedSize.Class (type (?), AtLeast, BoundedSize (..),
                                         FixedSize, HasSize (..), IsBoundedSize,
                                         IsFixedSize, IsMaxSize, MaxSize,
                                         Predicate (..), Refined, fromNat,
                                         plain, unsafeCreate)
import           Data.BoundedSize.TH    (fx, mx, typeFromInt, unsafeCreateExp)
import           Data.ListLike          (ListOps)
import qualified Data.ListLike          as B (append, drop, length, map,
                                              replicate, take)
import           Data.Proxy             (Proxy (..))
import           GHC.Base               (Int, ($), (.))
import           GHC.Exts               (IsList (..))
import           GHC.Num                ((-))
import           GHC.TypeLits           (type (+), KnownNat)


-- | Class of types which can be assigned a type-level minimum and maximum length.
class (IsBoundedSize l u a, ListOps a) => IsBoundedSizeListOps l u a
instance (KnownNat l, KnownNat u, ListOps a) => IsBoundedSizeListOps l u a

class (IsFixedSize n a, ListOps a) => IsFixedSizeListOps n a
instance (KnownNat n, ListOps a) => IsFixedSizeListOps n a

class (IsMaxSize n a, ListOps a) => IsMaxSizeListOps n a
instance (KnownNat n, ListOps a) => IsMaxSizeListOps n a

-- $setup
-- >>> :set -Wno-type-defaults
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XTemplateHaskell
-- >>> :set -XFlexibleContexts
-- >>> import Data.BoundedSize.TH (fx,mx)
-- >>> import Data.ByteString(ByteString)
-- >>> import Data.Char (toUpper)
-- >>> import Data.String (String)



-- | Append two 'BoundedSize' values together.
--
-- >>> append $(mx "foo") $(fx "bear")
-- "foobear"
--
-- >>> :type append $(mx "foo") $(fx "bear")
-- append $(mx "foo") $(fx "bear")
--   :: (Data.ListLike.Base.ListLike a (Item a), HasSize a,
--       Data.String.IsString a) =>
--      a ? BoundedSize 4 7
--
-- >>> append $(fx "Hello, ") $(mx "world!")
-- "Hello, world!"
append ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeListOps l1 u1 a
     , IsBoundedSizeListOps l2 u2 a
     , IsBoundedSizeListOps (l1 + l2) (u1 + u2) a
     )
  =>  a ? BoundedSize l1 u1
  ->  a ? BoundedSize l2 u2
  ->  a ? BoundedSize (l1 + l2) (u1 + u2)
append a b = unsafeCreate $ B.append (plain a) (plain b)

-- | Construct a new 'BoundedSize' of maximum length from a basic element.
--
-- >>> replicate '=' :: String ? BoundedSize 5 10
-- "=========="
replicate ::
     forall l u a.
     (IsBoundedSizeListOps l u a)
  => Item a
  -> a ? BoundedSize l u
replicate e = unsafeCreate $ B.replicate t e
  where
    t = fromNat (Proxy @u)

-- | Map a BoundedSize to a BoundedSize of the same length.
--
-- >>> map toUpper $(fx "Hello") :: String ? BoundedSize 5 5
-- "HELLO"
map ::
     forall l u a.
     (IsBoundedSizeListOps l u a)
  => (Item a -> Item a)
  -> a ? BoundedSize l u
  -> a ? BoundedSize l u
map f s = unsafeCreate $ B.map f $ plain s

-- | Reduce 'BoundedSize' length, preferring elements on the left.
--
-- >>> take $(mx "Foobar") :: String ? BoundedSize 0 3
-- "Foo"
-- >>> :t take $(fx "Hello")
-- take $(fx "Hello")
--   :: (Data.ListLike.Base.ListLike a (Item a), KnownNat l2,
--       KnownNat u2, HasSize a, Data.String.IsString a) =>
--      a ? BoundedSize l2 u2
--
-- >>> :t take $(fx "Hello") :: ByteString ? BoundedSize 4 32
-- take $(fx "Hello") :: ByteString ? BoundedSize 4 32
--   :: ByteString ? BoundedSize 4 32
-- >>> take $(fx "Hello") :: ByteString ? BoundedSize 4 32
-- "Hello"
-- >>> length (take $(fx "Hello") :: ByteString ? BoundedSize 4 32)
-- 5
-- >>> take $(fx "Hello") :: ByteString ? BoundedSize 4 32
-- "Hello"
-- >>> take $(fx "Hello") :: ByteString ? BoundedSize 4 4
-- "Hell"
-- >>> take $(fx "HelloHelloHelloHelloHelloHelloHello") :: ByteString ? BoundedSize 4 32
-- "HelloHelloHelloHelloHelloHelloHe"
take ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeListOps l1 u1 a
     , IsBoundedSizeListOps l2 u2 a
     )
  => a ? BoundedSize l1 u1
  -> a ? BoundedSize l2 u2
take s = unsafeCreate $ B.take t $ plain s
  where
    t = fromNat (Proxy @u2)

-- | Reduce 'BoundedSize' length, preferring elements on the right.
--
-- >>> drop $(fx "Foobar") :: String ? BoundedSize 2 2
-- "ar"
-- >>> drop $(mx "Foobar") :: String ? BoundedSize 0 2
-- "ar"
drop ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeListOps l1 u1 a
     , IsBoundedSizeListOps l2 u2 a
     )
  => a ? BoundedSize l1 u1
  -> a ? BoundedSize l2 u2
drop s = unsafeCreate $ B.drop (B.length s' - t) s'
  where
    s' = plain s
    t = fromNat (Proxy @u2)

-- | Obtain length bounds from the type.
bounds ::
     forall l u a. (IsBoundedSize l u a)
  => a ? BoundedSize l u
  -> (Int, Int)
bounds _ = (lower, upper)
  where
    lower = fromNat (Proxy @l)
    upper = fromNat (Proxy @u)

-- | Obtain value-level length.  Consult the @plain@ data value.
length ::
     forall l u a. (IsBoundedSizeListOps l u a)
  => a ? BoundedSize l u
  -> Int
length = B.length . plain

-- | Fill a 'BoundedSize' with extra elements up to target length, padding
-- original elements to the left.
padLeft ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeListOps l1 u1 a
     , IsBoundedSizeListOps l2 u2 a
     , IsBoundedSizeListOps 0 (u2 + u1) a
     )
  => Item a
  -> a ? BoundedSize l1 u1
  -> a ? BoundedSize l2 u2
padLeft pad = drop . ((replicate pad :: a ? BoundedSize 0 u2) `append`)

-- | Like 'padLeft', but original elements are padded to the right.
padRight ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeListOps l1 u1 a
     , IsBoundedSizeListOps l2 u2 a
     , IsBoundedSizeListOps 0 (u1 + u2) a
     )
  => Item a
  -> a ? BoundedSize l1 u1
  -> a ? BoundedSize l2 u2
padRight pad = take . (`append` (replicate pad :: a ? BoundedSize 0 u2))
