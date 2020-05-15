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
import           Data.SizedText.Class (create, createLeft, createRight, fromNat)
import qualified Data.SizedText.Class as C (Elem, IsSizedText (..), Sized)
import           Data.SizedText.TH


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -Wno-type-defaults
-- >>> import           Data.Proxy
-- >>> import           Data.Char(toUpper)
-- >>> import           Data.String          (IsString (..))


-- | Append two C.Sizeds together.
--
-- >>> :type append $(sz "foo") $(st "bear")
-- append $(sz "foo") $(st "bear")
--   :: (C.IsSizedText a, IsString a) => C.Sized a 4 7
-- >>> append $(sz "foo") $(st "bear")
-- "foobear"
-- >>> :type append $(st "Hello, ") $(sz "world!")
-- append $(st "Hello, ") $(sz "world!")
--   :: (C.IsSizedText a, IsString a) => C.Sized a 7 13
-- >>> append $(st "Hello, ") $(sz "world!")
-- "Hello, world!"
append ::
     forall a l1 u1 l2 u2. (C.IsSizedText a, KnownNat l1, KnownNat u1, KnownNat l2, KnownNat u2)
  => C.Sized a l1 u1
  -> C.Sized a l2 u2
  -> C.Sized a (l1 + l2) (u1 + u2)
append a b = C.unsafeCreate $ C.append (C.unwrap a) (C.unwrap b)

-- | Construct a new C.Sized of maximum length from a basic element.
--
-- >>> replicate '=' :: C.Sized String 5 10
-- "=========="
replicate ::
     forall a l u. (C.IsSizedText a, KnownNat l, KnownNat u)
  => C.Elem a
  -> C.Sized a l u
replicate e = C.unsafeCreate $ C.replicate t e
  where
    t = fromNat (Proxy @u)

-- | Map a C.Sized to a C.Sized of the same length.
--
-- >>> map toUpper $(st "Hello") :: C.Sized String 5 5
-- "HELLO"
map ::
     C.IsSizedText a => (C.Elem a -> C.Elem a) -> C.Sized a l u -> C.Sized a l u
map f s = C.unsafeCreate $ C.map f $ C.unwrap s

-- | Reduce C.Sized length, preferring elements on the left.
--
-- >>> take $(sz "Foobar") :: C.Sized String 0 3
-- "Foo"
take ::
     forall a l1 u1 l2 u2.
     ( C.IsSizedText a
     , KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , l2 <= l1
     )
  => C.Sized a l1 u1
  -> C.Sized a l2 u2
take s = C.unsafeCreate $ C.take t $ C.unwrap s
  where
    t = fromNat (Proxy @u2)

-- | Reduce C.Sized length, preferring elements on the right.
--
-- >>> drop $(st "Foobar") :: C.Sized String 2 2
-- "ar"
-- >>> drop $(sz "Foobar") :: C.Sized String 0 2
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
  => C.Sized a l1 u1
  -> C.Sized a l2 u2
drop s = C.unsafeCreate $ C.drop (C.length s' - t) s'
  where
    s' = C.unwrap s
    t = fromNat (Proxy @u2)

-- | Obtain length bounds from the type.
bounds ::
     forall a l u. (C.IsSizedText a, KnownNat l, KnownNat u)
  => C.Sized a l u
  -> (Int, Int)
bounds _ = (lower, upper) -- FIXME: Bounds type from Ix?
  where
    lower = fromNat (Proxy @l)
    upper = fromNat (Proxy @u)

-- | Obtain value-level length.  Consult the actual data value.
length ::
     forall a l u. (C.IsSizedText a, KnownNat l, KnownNat u)
  => C.Sized a l u
  -> Int
length = C.length . C.unwrap

-- | Fill a C.Sized with extra elements up to target length, padding
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
  -> C.Sized a l1 u1
  -> C.Sized a l2 u2
padLeft pad = drop . ((replicate pad :: C.Sized a 0 u2) `append`)

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
  -> C.Sized a l1 u1
  -> C.Sized a l2 u2
padRight pad = take . (`append` (replicate pad :: C.Sized a 0 u2))
