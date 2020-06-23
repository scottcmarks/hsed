{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Data.IsBytes
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Class of things with ByteString-like operations

-}

module Data.IsBytes
  (
   IsBytes (..)
  )
where

import           Data.ByteString       as B
import           Data.ByteString.Short as S
import           Data.HasSize          (HasSize (..))
import           Data.Refined          (type (?), Predicate (..))
import           Data.Vector           as V
import           GHC.Base              (Int, id, ($), (.))
import qualified GHC.List              as L
import           GHC.Word              (Word8 (..))






class HasSize a => IsBytes a where
  -- | Basic element type. For @IsBoundedSizeBytesText [a]@, this is @a@.
  type Elem a

  length :: a -> Int
  length = size

  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a
  toList :: a -> [Elem a]

instance IsBytes B.ByteString
  where
    type Elem B.ByteString = Word8
    append = B.append
    replicate = B.replicate
    map = B.map
    take = B.take
    drop = B.drop
    toList = B.unpack

instance IsBytes S.ShortByteString
  where
    type Elem S.ShortByteString = Word8
    append a b = S.toShort $ B.append (S.fromShort a) (S.fromShort b)
    replicate n = S.toShort . B.replicate n
    map f = S.toShort . B.map f . S.fromShort
    take n = S.toShort . B.take n . S.fromShort
    drop n = S.toShort . B.drop n . S.fromShort
    toList = B.unpack . S.fromShort

instance IsBytes [a]
  where
    type Elem [a] = a
    append = (L.++)
    replicate = L.replicate
    map = L.map
    take = L.take
    drop = L.drop
    toList = id

instance IsBytes (V.Vector a)
  where
    type Elem (V.Vector a) = a
    append = (V.++)
    replicate = V.replicate
    map = V.map
    take = V.take
    drop = V.drop
    toList = V.toList


instance (IsBytes(a), Predicate p a) => IsBytes(a ? p)
  where
      type Elem (a ? p) = Elem a
      append a b = refine $ Data.IsBytes.append (plain a) (plain b)
      replicate n = refine . Data.IsBytes.replicate n
      map f = refine . Data.IsBytes.map f . plain
      take n = refine . Data.IsBytes.take n . plain
      drop n = refine . Data.IsBytes.drop n . plain
      toList = Data.IsBytes.toList . plain
