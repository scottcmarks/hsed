{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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
       -- , st
       , create
       , replicate

         -- * Working with sized texts
       , append
       , take
       , drop
       , map
       , padLeft
       , padRight

       , bounds
       , length

       , fromStatic
       , toStatic

         -- * C.IsSizedText class
       , C.Sized
       , C.IsSizedText(Elem, unsafeCreate, unwrap)
       )

where

import           GHC.TypeLits
import           Prelude              hiding (drop, length, map, replicate,
                                       take)

import           Data.Proxy
import qualified Data.SizedText.Class as C (Elem, IsSizedText (..), Sized)
import qualified Data.StaticText      as S


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char (toUpper)

-- | Safely create a C.Sized, possibly altering the source to match
-- target length. If target maximum length is less than the source length,
-- the source gets truncated. If target minimum length is less than
-- the source length, the source is padded using the provided basic element.
-- Elements on the left are preferred.
--
-- >>> createLeft ' ' "foobarbaz" :: C.Sized String 0 6
-- "foobar"
-- >>> createLeft '#' "foobarbaz" :: C.Sized String 12 20
-- "foobarbaz###"

createLeft :: forall a l u.
              (C.IsSizedText a, KnownNat l, KnownNat u) =>
              C.Elem a -> a -> C.Sized a l u
createLeft e s =
  C.unsafeCreate $
  C.take ut $
  C.append s $
  C.replicate (lt - C.length s) e
  where
    lt = fromIntegral $ natVal (Proxy :: Proxy l)
    ut = fromIntegral $ natVal (Proxy :: Proxy u)



-- | Just like 'createLeft', except that elements on the right are preferred.
--
-- >>> createRight '@' "foobarbaz" :: Sized String 0 6
-- "barbaz"
-- >>> createRight '!' "foobarbaz" :: C.Sized String 12 20
-- "!!!foobarbaz"
createRight :: forall a l u.
              (C.IsSizedText a, KnownNat l, KnownNat u) =>
              C.Elem a -> a -> C.Sized a l u
createRight e s =
  C.unsafeCreate $
  C.drop (len - ut) $
  C.append (C.replicate (lt - len) e) s
  where
    len = C.length s
    lt = fromIntegral $ natVal (Proxy :: Proxy l)
    ut = fromIntegral $ natVal (Proxy :: Proxy u)


-- | Attempt to safely create a C.Sized if it matches target length.
--
-- >>> create "foobar" :: Maybe (C.Sized String 6 10)
-- Just "foobar"
-- >>> create "barbaz" :: Maybe (C.Sized String 0 4)
-- Nothing
--
-- This is safer than 'C.unsafeCreate' and unlike with 'createLeft' /
-- 'createRight' the source value is left unchanged. However, this
-- implies a further run-time check for Nothing values.
create :: forall a (l :: Nat) (u :: Nat).
              (C.IsSizedText a, KnownNat l, KnownNat u) =>
          a -> Maybe (C.Sized a l u)
create s =
  if lt <= len && len <= ut
  then Just $ C.unsafeCreate s
  else Nothing
  where
    len = C.length s
    lt  = fromIntegral $ natVal (Proxy :: Proxy l)
    ut  = fromIntegral $ natVal (Proxy :: Proxy u)



-- | Append two C.Sizeds together.
--
-- >>> append $(st "foo") $(st "bar") :: C.Sized String 6 6
-- "foobar"
append :: forall a l1 u1 l2 u2.
          (C.IsSizedText a) => C.Sized a l1 u1 -> C.Sized a l2 u2 -> C.Sized a (l1 + l2) (u1 + u2)
append a b = C.unsafeCreate $ C.append (C.unwrap a) (C.unwrap b)


-- | Construct a new C.Sized of maximum length from a basic element.
--
-- >>> replicate '=' :: C.Sized String 5 10
-- "=========="
replicate :: forall a l u.
             (C.IsSizedText a, KnownNat l, KnownNat u) => C.Elem a -> C.Sized a l u
replicate e =
  C.unsafeCreate $ C.replicate t e
  where
    t = fromIntegral $ natVal (Proxy :: Proxy u)


-- | Map a C.Sized to a C.Sized of the same length.
--
-- >>> map toUpper $(st "Hello") :: C.Sized String 5 5
-- "HELLO"
map :: C.IsSizedText a =>
       (C.Elem a -> C.Elem a) -> C.Sized a l u -> C.Sized a l u
map f s =
  C.unsafeCreate $ C.map f $ C.unwrap s


-- | Reduce C.Sized length, preferring elements on the left.
--
-- >>> take $(st "Foobar") :: C.Sized String 3
-- "Foo"
take :: forall a l1 u1 l2 u2.
        (C.IsSizedText a, KnownNat l1, KnownNat u1, KnownNat l2, KnownNat u2, l2 <= l1) =>
        C.Sized a l1 u1 -> C.Sized a l2 u2
take s =
  C.unsafeCreate $ C.take t $ C.unwrap s
  where
    t = fromIntegral $ natVal (Proxy :: Proxy u2)


-- | Reduce C.Sized length, preferring elements on the right.
--
-- >>> drop $(st "Foobar") :: C.Sized String 2
-- "ar"
drop :: forall a l1 u1 l2 u2.
        (C.IsSizedText a, KnownNat l1, KnownNat u1, KnownNat l2, KnownNat u2, l2 <= l1) =>
        C.Sized a l1 u1 -> C.Sized a l2 u2
drop s =
  C.unsafeCreate $ C.drop (C.length s' - t) s'
  where
    s' = C.unwrap s
    t = fromIntegral $ natVal (Proxy :: Proxy u2)


-- | Obtain length bounds from the type.
bounds :: forall a l u.
        (C.IsSizedText a, KnownNat l, KnownNat u) =>
        C.Sized a l u -> (Int, Int)
bounds _ = (fromIntegral lower, fromIntegral upper)  -- FIXME: Bounds type from Ix?
  where
    lower = natVal (Proxy :: Proxy l)
    upper = natVal (Proxy :: Proxy u)



-- | Obtain value-level length.  Consult the actual data value.
length :: forall a l u.
          (C.IsSizedText a, KnownNat l, KnownNat u) => C.Sized a l u -> Int
length = C.length . C.unwrap


-- | Fill a C.Sized with extra elements up to target length, padding
-- original elements to the left.
padLeft :: forall a l1 u1 l2 u2.
           (C.IsSizedText a, KnownNat l1, KnownNat u1, KnownNat l2, KnownNat u2, KnownNat (u2 + u1), l2 <= l1, u1 <= u2) =>
           C.Elem a -> C.Sized a l1 u1 -> C.Sized a l2 u2
padLeft pad = drop . ((replicate pad :: C.Sized a 0 u2) `append`)


-- | Like 'padLeft', but original elements are padded to the right.
padRight :: forall a l1 u1 l2 u2.
           (C.IsSizedText a, KnownNat l1, KnownNat u1, KnownNat l2, KnownNat u2, KnownNat (u1 + u2), l2 <= l1, u1 <= u2) =>
           C.Elem a -> C.Sized a l1 u1 -> C.Sized a l2 u2
padRight pad = take . (`append` (replicate pad :: C.Sized a 0 u2))


fromStatic :: (S.IsStaticText a, C.IsSizedText a) => S.Static a i -> C.Sized a i i
fromStatic = C.unsafeCreate . S.unwrap

toStatic :: (C.IsSizedText a, S.IsStaticText a) => C.Sized a i i -> S.Static a i
toStatic = S.unsafeCreate . C.unwrap
