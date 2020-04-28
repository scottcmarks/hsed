
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

sized-text combinators are defined for members of 'C.IsSizedStaticText'
class. The package includes 'C.IsSizedStaticText' instances for several
common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.SizedStaticText as S

-}
module Data.SizedStaticText
  (
    -- * Constructing sized texts
    --
    -- | See also 'C.unsafeCreate'
    createLeft
  , createRight
  , sz
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

    -- * StaticText conversions
  , fromStatic
  , toStatic

    -- * IsSizedStaticText class
  , SizedStatic
  , IsSizedStaticText(Elem, unsafeCreate, unwrap)
  ) where

import           GHC.TypeLits
import           Prelude                    hiding (drop, length, map,
                                             replicate, take)

import           Data.Proxy
import           Data.SizedStaticText.Class (Elem, IsSizedStaticText,
                                             SizedStatic)
import qualified Data.SizedStaticText.Class as C (IsSizedStaticText (..))
import           Data.SizedStaticText.TH
import           Data.StaticText            (IsStaticText, Static)
import qualified Data.StaticText            as S

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char (toUpper)

-- | Elements on the left are preferred.
-- >>> createLeft ' ' "foobarbaz" :: SizedStatic String 0 6
-- "foobar"
-- >>> createLeft '@' "foobarbaz" :: SizedStatic String 12 20
-- "foobarbaz@@@"
createLeft ::
     forall a l u. (IsSizedStaticText a, KnownNat l, KnownNat u, l <= u)
  => Elem a
  -> a
  -> SizedStatic a l u
createLeft e s =
  C.unsafeCreate $ C.take ut $ C.append s $ C.replicate (lt - C.length s) e
  where
    lt = fromIntegral $ natVal (Proxy :: Proxy l)
    ut = fromIntegral $ natVal (Proxy :: Proxy u)

-- | Just like 'createLeft', except that elements on the right are preferred.
-- >>> createRight '@' "foobarbaz" :: SizedStatic String 0 6
-- "barbaz"
-- >>> createRight '#' "foobarbaz" :: SizedStatic String 12 20
-- "###foobarbaz"
createRight ::
     forall a l u. (IsSizedStaticText a, KnownNat l, KnownNat u, l <= u)
  => Elem a
  -> a
  -> SizedStatic a l u
createRight e s =
  C.unsafeCreate $ C.drop (len - ut) $ C.append (C.replicate (lt - len) e) s
  where
    len = C.length s
    lt = fromIntegral $ natVal (Proxy :: Proxy l)
    ut = fromIntegral $ natVal (Proxy :: Proxy u)

-- | Attempt to safely create a SizedStatic if it matches target length.
--
-- >>> create "foobar" :: Maybe (SizedStatic String 6 10)
-- Just "foobar"
-- >>> create "barbaz" :: Maybe (SizedStatic String 0 4)
-- Nothing
--
-- This is safer than 'C.unsafeCreate' and unlike with 'createLeft'
-- 'createRight' the source value is left unchanged. However, this
-- implies a further run-time check for Nothing values.
create ::
     forall a (l :: Nat) (u :: Nat). (IsSizedStaticText a, KnownNat l, KnownNat u, l <= u)
  => a
  -> Maybe (SizedStatic a l u)
create s =
  if lt <= len && len <= ut
    then Just $ C.unsafeCreate s
    else Nothing
  where
    len = C.length s
    lt = fromIntegral $ natVal (Proxy :: Proxy l)
    ut = fromIntegral $ natVal (Proxy :: Proxy u)

-- | Append two SizedStatics together.
--
-- >>> append $(sz "foo") $(sz "bar") :: SizedStatic String 6 6
-- "foobar"
-- >>> :set -fno-warn-type-defaults
-- >>> append $(sz "Hello, ") $(sz "world!")
-- "Hello, world!"
-- >>> :type it
-- it
--   :: (IsSizedStaticText a, Data.String.IsString a) =>
--      SizedStatic a 13 13
append ::
     forall a l1 u1 l2 u2. (IsSizedStaticText a, KnownNat l1, KnownNat u1, KnownNat l2, KnownNat u2,
                            KnownNat (l1 + l2), KnownNat (u1 + u2))
  => SizedStatic a l1 u1
  -> SizedStatic a l2 u2
  -> SizedStatic a (l1 + l2) (u1 + u2)
append a b = C.unsafeCreate $ C.append (C.unwrap a) (C.unwrap b)

-- | Construct a new SizedStatic of maximum length from a basic element.
--
-- >>> replicate '=' :: SizedStatic String 5 10
-- "=========="
replicate ::
     forall a l u. (IsSizedStaticText a, KnownNat l, KnownNat u, l <= u)
  => Elem a
  -> SizedStatic a l u
replicate e = C.unsafeCreate $ C.replicate t e
  where
    t = fromIntegral $ natVal (Proxy :: Proxy u)

-- | Map a SizedStatic to a SizedStatic of the same length.
--
-- >>> map toUpper $(sz "Hello") :: SizedStatic String 5 5
-- "HELLO"
map ::
     IsSizedStaticText a => (Elem a -> Elem a) -> SizedStatic a l u -> SizedStatic a l u
map f s = C.unsafeCreate $ C.map f $ C.unwrap s

-- | Reduce SizedStatic length, preferring elements on the left.
--
-- >>> take $(sz "Foobar") :: SizedStatic String 1 3
-- "Foo"
take ::
     forall a l1 u1 l2 u2.
     ( IsSizedStaticText a
     , KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , l2 <= l1
     )
  => SizedStatic a l1 u1
  -> SizedStatic a l2 u2
take s = C.unsafeCreate $ C.take t $ C.unwrap s
  where
    t = fromIntegral $ natVal (Proxy :: Proxy u2)

-- | Reduce SizedStatic length, preferring elements on the right.
--
-- >>> drop $(sz "Foobar") :: SizedStatic String 0 2
-- "ar"
drop ::
     forall a l1 u1 l2 u2.
     ( IsSizedStaticText a
     , KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , l2 <= l1
     )
  => SizedStatic a l1 u1
  -> SizedStatic a l2 u2
drop s = C.unsafeCreate $ C.drop (C.length s' - t) s'
  where
    s' = C.unwrap s
    t = fromIntegral $ natVal (Proxy :: Proxy u2)

-- | Obtain length bounds from the type.
bounds ::
     forall a l u. (IsSizedStaticText a, KnownNat l, KnownNat u)
  => SizedStatic a l u
  -> (Int, Int)
bounds _ = (fromIntegral lower, fromIntegral upper)
  where
    lower = natVal (Proxy :: Proxy l)
    upper = natVal (Proxy :: Proxy u)

-- | Obtain value-level length.  Consult the actual data value.
--
-- >>> length $ (take $(sz "password") :: SizedStatic String 4 32)
-- 8
length ::
     forall a l u. (IsSizedStaticText a, KnownNat l, KnownNat u)
  => SizedStatic a l u
  -> Int
length = C.length . C.unwrap

-- | Fill a SizedStatic with extra elements up to target length, padding
-- original elements to the left.
padLeft ::
     forall a l1 u1 l2 u2.
     ( IsSizedStaticText a
     , KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , KnownNat (u2 + u1)
     , l2 <= l1
     , u1 <= u2
     )
  => Elem a
  -> SizedStatic a l1 u1
  -> SizedStatic a l2 u2
padLeft pad = drop . ((replicate pad :: SizedStatic a 0 u2) `append`)

-- | Like 'padLeft', but original elements are padded to the right.
padRight ::
     forall a l1 u1 l2 u2.
     ( IsSizedStaticText a
     , KnownNat l1
     , KnownNat u1
     , KnownNat l2
     , KnownNat u2
     , KnownNat (u1 + u2)
     , l2 <= l1
     , u1 <= u2
     )
  => Elem a
  -> SizedStatic a l1 u1
  -> SizedStatic a l2 u2
padRight pad = take . (`append` (replicate pad :: SizedStatic a 0 u2))

fromStatic :: (IsStaticText a, IsSizedStaticText a) => Static a i -> SizedStatic a i i
fromStatic = C.unsafeCreate . S.unwrap

toStatic :: (IsSizedStaticText a, IsStaticText a) => SizedStatic a i i -> Static a i
toStatic = S.unsafeCreate . C.unwrap
