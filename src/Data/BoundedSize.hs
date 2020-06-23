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

{-|

sized-text combinators are defined for members of 'IsBoundedSize'
class. The package includes 'IsBoundedSize' instances for several
common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.BoundedSize as S

-}
module Data.BoundedSize
       ( C.HasSize(..)
       , C.BoundedSize(..)
       , C.IsBoundedSize
       , C.IsFixedSize
       , C.FixedSize
       , C.IsMaxSize
       , C.MaxSize
       , C.AtLeast
       , C.fx
       , C.mx
       , C.unsafeCreateExp
       , C.typeFromInt

       , IsBoundedSizeBytes
       , IsFixedSizeBytes
       , IsMaxSizeBytes
       , append
       , replicate
       , map
       , take
       , drop
       , bounds
       , length
       , padLeft
       , padRight

       , type (?)
       , Predicate(..)
       , fromNat
       )
where

import           Data.BoundedSize.Class (type (?), Predicate (..), fromNat)
import qualified Data.BoundedSize.Class as C (AtLeast, BoundedSize (..),
                                              FixedSize, HasSize (..),
                                              IsBoundedSize, IsFixedSize,
                                              IsMaxSize, MaxSize)
import qualified Data.BoundedSize.TH    as C (fx, mx, typeFromInt,
                                              unsafeCreateExp)
import qualified Data.IsBytes           as B (IsBytes (..))
import           Data.Proxy             (Proxy (..))
import           GHC.Base               (Int, ($), (.))
import           GHC.Num                ((-))
import           GHC.TypeLits           (type (+), KnownNat)

-- | Class of types which can be assigned a type-level minimum and maximum length.
class (C.IsBoundedSize l u a, B.IsBytes a) => IsBoundedSizeBytes l u a
instance (KnownNat l, KnownNat u, B.IsBytes a) => IsBoundedSizeBytes l u a

class (C.IsFixedSize n a, B.IsBytes a) => IsFixedSizeBytes n a
instance (KnownNat n, B.IsBytes a) => IsFixedSizeBytes n a

class (C.IsMaxSize n a, B.IsBytes a) => IsMaxSizeBytes n a
instance (KnownNat n, B.IsBytes a) => IsMaxSizeBytes n a

-- $setup
-- >>> :set -Wno-type-defaults
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XTemplateHaskell
-- >>> import Data.BoundedSize.TH (fx,mx)
-- >>> import Data.ByteString(ByteString)
-- >>> import Data.Char (toUpper)
-- >>> import Data.String (String)




-- | Append two BoundedSizes together.
-- "foobear"
--
-- >>> append $(mx "foo") $(fx "bear")
-- "foobear"
--
-- >>> :type append $(mx "foo") $(fx "bear")
-- append $(mx "foo") $(fx "bear")
--   :: (B.IsBytes a, Data.String.IsString a) => a ? C.BoundedSize 4 7
--
-- >>> append $(fx "Hello, ") $(mx "world!")
-- "Hello, world!"
append ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeBytes l1 u1 a
     , IsBoundedSizeBytes l2 u2 a
     , IsBoundedSizeBytes (l1 + l2) (u1 + u2) a
     )
  =>  a ? C.BoundedSize l1 u1
  ->  a ? C.BoundedSize l2 u2
  ->  a ? C.BoundedSize (l1 + l2) (u1 + u2)
append a b = unsafeCreate $ B.append (plain a) (plain b)

-- | Construct a new BoundedSize of maximum length from a basic element.
--
-- >>> replicate '=' :: String ? C.BoundedSize 5 10
-- "=========="
replicate ::
     forall l u a.
     (IsBoundedSizeBytes l u a)
  => B.Elem a
  -> a ? C.BoundedSize l u
replicate e = unsafeCreate $ B.replicate t e
  where
    t = fromNat (Proxy @u)

-- | Map a BoundedSize to a BoundedSize of the same length.
--
-- >>> map toUpper $(fx "Hello") :: String ? C.BoundedSize 5 5
-- "HELLO"
map ::
     forall l u a.
     (IsBoundedSizeBytes l u a)
  => (B.Elem a -> B.Elem a)
  -> a ? C.BoundedSize l u
  -> a ? C.BoundedSize l u
map f s = unsafeCreate $ B.map f $ plain s

-- | Reduce BoundedSize length, preferring elements on the left.
--
-- >>> take $(mx "Foobar") :: String ? C.BoundedSize 0 3
-- "Foo"
-- >>> :t take $(fx "Hello")
-- take $(fx "Hello")
--   :: (B.IsBytes a, KnownNat l2, KnownNat u2,
--       Data.String.IsString a) =>
--      a ? C.BoundedSize l2 u2
--
-- >>> :t take $(fx "Hello") :: ByteString ? C.BoundedSize 4 32
-- take $(fx "Hello") :: ByteString ? C.BoundedSize 4 32
--   :: ByteString ? C.BoundedSize 4 32
-- >>> take $(fx "Hello") :: ByteString ? C.BoundedSize 4 32
-- "Hello"
-- >>> length (take $(fx "Hello") :: ByteString ? C.BoundedSize 4 32)
-- 5
-- >>> take $(fx "Hello") :: ByteString ? C.BoundedSize 4 32
-- "Hello"
-- >>> take $(fx "Hello") :: ByteString ? C.BoundedSize 4 4
-- "Hell"
-- >>> take $(fx "HelloHelloHelloHelloHelloHelloHello") :: ByteString ? C.BoundedSize 4 32
-- "HelloHelloHelloHelloHelloHelloHe"
take ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeBytes l1 u1 a
     , IsBoundedSizeBytes l2 u2 a
     )
  => a ? C.BoundedSize l1 u1
  -> a ? C.BoundedSize l2 u2
take s = unsafeCreate $ B.take t $ plain s
  where
    t = fromNat (Proxy @u2)

-- | Reduce BoundedSize length, preferring elements on the right.
--
-- >>> drop $(fx "Foobar") :: String ? C.BoundedSize 2 2
-- "ar"
-- >>> drop $(mx "Foobar") :: String ? C.BoundedSize 0 2
-- "ar"
drop ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeBytes l1 u1 a
     , IsBoundedSizeBytes l2 u2 a
     )
  => a ? C.BoundedSize l1 u1
  -> a ? C.BoundedSize l2 u2
drop s = unsafeCreate $ B.drop (B.length s' - t) s'
  where
    s' = plain s
    t = fromNat (Proxy @u2)

-- | Obtain length bounds from the type.
bounds ::
     forall l u a. (C.IsBoundedSize l u a)
  => a ? C.BoundedSize l u
  -> (Int, Int)
bounds _ = (lower, upper)
  where
    lower = fromNat (Proxy @l)
    upper = fromNat (Proxy @u)

-- | Obtain value-level length.  Consult the actual data value.
length ::
     forall l u a. (IsBoundedSizeBytes l u a)
  => a ? C.BoundedSize l u
  -> Int
length = B.length . plain

-- | Fill a BoundedSize with extra elements up to target length, padding
-- original elements to the left.
padLeft ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeBytes l1 u1 a
     , IsBoundedSizeBytes l2 u2 a
     , IsBoundedSizeBytes 0 (u2 + u1) a
     )
  => B.Elem a
  -> a ? C.BoundedSize l1 u1
  -> a ? C.BoundedSize l2 u2
padLeft pad = drop . ((replicate pad :: a ? C.BoundedSize 0 u2) `append`)

-- | Like 'padLeft', but original elements are padded to the right.
padRight ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeBytes l1 u1 a
     , IsBoundedSizeBytes l2 u2 a
     , IsBoundedSizeBytes 0 (u1 + u2) a
     )
  => B.Elem a
  -> a ? C.BoundedSize l1 u1
  -> a ? C.BoundedSize l2 u2
padRight pad = take . (`append` (replicate pad :: a ? C.BoundedSize 0 u2))
