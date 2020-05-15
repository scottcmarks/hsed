{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
{-|

sized-text combinators are defined for members of 'C.IsSizedText'
class. The package includes 'C.IsSizedText' instances for several
common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.SizedText as S

-}
module Data.SizedText
  (
    -- * Constructing sized texts
    --
    -- | See also 'C.unsafeCreate'
    createLeft
  , createRight
  , create
  , replicate
  , st
  , sz

    -- * Working with sized texts
  , append
  , take
  , drop
  , map
  , padLeft
  , padRight
  , bounds
  , length

    -- * C.IsSizedText class
  , C.Sized
  , C.IsSizedText(Elem, unsafeCreate, unwrap)
  ) where

import           GHC.TypeLits
import           Prelude              hiding (drop, length, map, replicate,
                                       take)

import           Data.Proxy
import           Data.SizedText.Class (Sized, create, createLeft, createRight,
                                       fromNat)
import qualified Data.SizedText.Class as C (Elem, IsSizedText (..))
import           Data.SizedText.TH


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -Wno-type-defaults
-- >>> import           Data.Proxy
-- >>> import           Data.ByteString      (ByteString)
-- >>> import           Data.Char(toUpper)
-- >>> import           Data.String          (IsString (..))


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
     forall a l1 u1 l2 u2. (C.IsSizedText a, KnownNat l1, KnownNat u1, KnownNat l2, KnownNat u2)
  => Sized a l1 u1
  -> Sized a l2 u2
  -> Sized a (l1 + l2) (u1 + u2)
append a b = C.unsafeCreate $ C.append (C.unwrap a) (C.unwrap b)

-- | Construct a new Sized of maximum length from a basic element.
--
-- >>> replicate '=' :: Sized String 5 10
-- "=========="
replicate ::
     forall a l u. (C.IsSizedText a, KnownNat l, KnownNat u)
  => C.Elem a
  -> Sized a l u
replicate e = C.unsafeCreate $ C.replicate t e
  where
    t = fromNat (Proxy @u)

-- | Map a Sized to a Sized of the same length.
--
-- >>> map toUpper $(st "Hello") :: Sized String 5 5
-- "HELLO"
map ::
     C.IsSizedText a => (C.Elem a -> C.Elem a) -> Sized a l u -> Sized a l u
map f s = C.unsafeCreate $ C.map f $ C.unwrap s

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
     forall a l1 u1 l2 u2.
     ( C.IsSizedText a
     , KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , l2 <= l1
     )
  => Sized a l1 u1
  -> Sized a l2 u2
take s = C.unsafeCreate $ C.take t $ C.unwrap s
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
     ( C.IsSizedText a
     , KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , l2 <= l1
     )
  => Sized a l1 u1
  -> Sized a l2 u2
drop s = C.unsafeCreate $ C.drop (C.length s' - t) s'
  where
    s' = C.unwrap s
    t = fromNat (Proxy @u2)

-- | Obtain length bounds from the type.
bounds ::
     forall a l u. (C.IsSizedText a, KnownNat l, KnownNat u)
  => Sized a l u
  -> (Int, Int)
bounds _ = (lower, upper) -- FIXME: Bounds type from Ix?
  where
    lower = fromNat (Proxy @l)
    upper = fromNat (Proxy @u)

-- | Obtain value-level length.  Consult the actual data value.
length ::
     forall a l u. (C.IsSizedText a, KnownNat l, KnownNat u)
  => Sized a l u
  -> Int
length = C.length . C.unwrap

-- | Fill a Sized with extra elements up to target length, padding
-- original elements to the left.
padLeft ::
     forall a l1 u1 l2 u2.
     ( C.IsSizedText a
     , KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , KnownNat (u2 + u1)
     , l2 <= l1
     , u1 <= u2
     )
  => C.Elem a
  -> Sized a l1 u1
  -> Sized a l2 u2
padLeft pad = drop . ((replicate pad :: Sized a 0 u2) `append`)

-- | Like 'padLeft', but original elements are padded to the right.
padRight ::
     forall a l1 u1 l2 u2.
     ( C.IsSizedText a
     , KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , KnownNat (u1 + u2)
     , l2 <= l1
     , u1 <= u2
     )
  => C.Elem a
  -> Sized a l1 u1
  -> Sized a l2 u2
padRight pad = take . (`append` (replicate pad :: Sized a 0 u2))
