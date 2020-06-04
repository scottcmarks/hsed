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
       , C.IsBoundedSize(..)
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
       )
where

import qualified Data.BoundedSize.Class as C (AtLeast, FixedSize, HasSize (..),
                                              IsBoundedSize (..), IsFixedSize,
                                              IsMaxSize, MaxSize)
import qualified Data.BoundedSize.TH    as C (fx, mx, typeFromInt,
                                              unsafeCreateExp)
import qualified Data.IsBytes           as B (IsBytes (..))
import           Data.Proxy             (Proxy (..))
import           Data.Smart
import           GHC.Base               (Int, ($), (.))
import           GHC.Num                (fromInteger, (-))
import           GHC.TypeLits           (type (+), KnownNat)
import           GHC.TypeLits.Extras

-- | Class of types which can be assigned a type-level minimum and maximum length.
class (C.IsBoundedSize l u a, B.IsBytes a) => IsBoundedSizeBytes l u a
instance (KnownNat l, KnownNat u, B.IsBytes a) => IsBoundedSizeBytes l u a

class (C.IsFixedSize n a, B.IsBytes a) => IsFixedSizeBytes n a
instance (KnownNat n, B.IsBytes a) => IsFixedSizeBytes n a

class (C.IsMaxSize n a, B.IsBytes a) => IsMaxSizeBytes n a
instance (KnownNat n, B.IsBytes a) => IsMaxSizeBytes n a



-- | Append two Sizeds together.
-- append $(sz "foo") $(st "bear")
--   :: (C.IsSizedText a, IsString a) => Sized a 4 7
--
-- >>> :type append $(sz "foo") $(st "bear")
-- append $(sz "foo") $(st "bear")
--   :: (C.IsSizedText a, IsString a) => Sized a 4 7
-- >>> append $(sz "foo") $(st "bear")
-- "foobear"
-- >>> :type append $(st "Hello, ") $(sz "world!")
-- append $(st "Hello, ") $(sz "world!")
--   :: (C.IsSizedText a, IsString a) => Sized a 7 13
-- >>> append $(st "Hello, ") $(sz "world!")
-- "Hello, world!"
append ::
     forall l1 u1 l2 u2 a. (KnownNat l1, KnownNat u1, KnownNat l2, KnownNat u2, KnownNat (l1 + l2), KnownNat (u1 + u2), B.IsBytes a)
  => C.BoundedSize l1 u1 a
  -> C.BoundedSize l2 u2 a
  -> C.BoundedSize (l1 + l2) (u1 + u2) a
append a b = unsafeCreate $ B.append (unwrap a) (unwrap b)

-- | Construct a new Sized of maximum length from a basic element.
--
-- >>> replicate '=' :: C.BoundedSize 5 10 String
-- "=========="
replicate ::
     forall l u a. (IsBoundedSizeBytes l u a)
  => B.Elem a
  -> C.BoundedSize l u a
replicate e = unsafeCreate $ B.replicate t e
  where
    t = fromNat (Proxy @u)

-- | Map a Sized to a Sized of the same length.
--
-- >>> map toUpper $(st "Hello") :: Sized String 5 5
-- "HELLO"
map ::
     IsBoundedSizeBytes l u a => (B.Elem a -> B.Elem a) -> C.BoundedSize l u a -> C.BoundedSize l u a
map f s = unsafeCreate $ B.map f $ unwrap s

-- | Reduce Sized length, preferring elements on the left.
--
-- >>> take $(sz "Foobar") :: Sized String 0 3
-- "Foo"
-- >>> :t take $(st "Hello")
-- take $(st "Hello")
--   :: (C.IsSizedText a, KnownNat l2, KnownNat u2, IsString a,
--       (l2 <=? 5) ~ 'True) =>
--      Sized a l2 u2
-- >>> :t take $(st "Hello") :: Sized ByteString 4 32
-- take $(st "Hello") :: Sized ByteString 4 32
--   :: Sized ByteString 4 32
-- >>> take $(st "Hello") :: Sized ByteString 4 32
-- "Hello"
-- >>> length (take $(st "Hello") :: Sized ByteString 4 32)
-- 5
-- >>> take $(st "Hello") :: Sized ByteString 4 32
-- "Hello"
-- >>> take $(st "Hello") :: Sized ByteString 4 4
-- "Hell"
-- >>> take $(st "HelloHelloHelloHelloHelloHelloHello") :: Sized ByteString 4 32
-- "HelloHelloHelloHelloHelloHelloHe"
take ::
     forall l1 u1 l2 u2 a.
     ( KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , B.IsBytes a
     )
  => C.BoundedSize l1 u1 a
  -> C.BoundedSize l2 u2 a
take s = unsafeCreate $ B.take t $ unwrap s
  where
    t = fromNat (Proxy @u2)

-- | Reduce Sized length, preferring elements on the right.
--
-- >>> drop $(st "Foobar") :: Sized String 2 2
-- "ar"
-- >>> drop $(sz "Foobar") :: Sized String 0 2
-- "ar"
drop ::
     forall a l1 u1 l2 u2.
     ( KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , B.IsBytes a
     )
  => C.BoundedSize l1 u1 a
  -> C.BoundedSize l2 u2 a
drop s = unsafeCreate $ B.drop (B.length s' - t) s'
  where
    s' = unwrap s
    t = fromInteger $ fromNat (Proxy @u2)

-- | Obtain length bounds from the type.
bounds ::
     forall l u a. (C.IsBoundedSize l u a)
  => C.BoundedSize l u a
  -> (Int, Int)
bounds _ = (lower, upper)
  where
    lower = fromNat (Proxy @l)
    upper = fromNat (Proxy @u)

-- | Obtain value-level length.  Consult the actual data value.
length ::
     forall l u a. (IsBoundedSizeBytes l u a)
  => C.BoundedSize l u a
  -> Int
length = B.length . unwrap

-- | Fill a Sized with extra elements up to target length, padding
-- original elements to the left.
padLeft ::
     forall a l1 u1 l2 u2.
     ( IsBoundedSizeBytes l1 u1 a
     , KnownNat l2
     , KnownNat u2
     , KnownNat (u2 + u1)
     )
  => B.Elem a
  -> C.BoundedSize l1 u1 a
  -> C.BoundedSize l2 u2 a
padLeft pad = drop . ((replicate pad :: C.BoundedSize 0 u2 a) `append`)

-- | Like 'padLeft', but original elements are padded to the right.
padRight ::
     forall l1 u1 l2 u2 a.
     ( IsBoundedSizeBytes l1 u1 a
     , KnownNat l2
     , KnownNat u2
     , KnownNat (u1 + u2)
     )
  => B.Elem a
  -> C.BoundedSize l1 u1 a
  -> C.BoundedSize l2 u2 a
padRight pad = take . (`append` (replicate pad :: C.BoundedSize 0 u2 a))
