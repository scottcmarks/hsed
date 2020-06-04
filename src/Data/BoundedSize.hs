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

-- $setup
-- >>> :set -Wno-type-defaults
-- >>> :set -XDataKinds
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
--   :: (B.IsBytes a, Data.String.IsString a) => C.BoundedSize 4 7 a
--
-- >>> append $(fx "Hello, ") $(mx "world!")
-- "Hello, world!"
append ::
     forall l1 u1 l2 u2 a. (KnownNat l1, KnownNat u1, KnownNat l2, KnownNat u2, KnownNat (l1 + l2), KnownNat (u1 + u2), B.IsBytes a)
  => C.BoundedSize l1 u1 a
  -> C.BoundedSize l2 u2 a
  -> C.BoundedSize (l1 + l2) (u1 + u2) a
append a b = unsafeCreate $ B.append (unwrap a) (unwrap b)

-- | Construct a new BoundedSize of maximum length from a basic element.
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

-- | Map a BoundedSize to a BoundedSize of the same length.
--
-- >>> map toUpper $(fx "Hello") :: C.BoundedSize 5 5 String
-- "HELLO"
map ::
     IsBoundedSizeBytes l u a => (B.Elem a -> B.Elem a) -> C.BoundedSize l u a -> C.BoundedSize l u a
map f s = unsafeCreate $ B.map f $ unwrap s

-- | Reduce BoundedSize length, preferring elements on the left.
--
-- >>> take $(mx "Foobar") :: C.BoundedSize 0 3 String
-- "Foo"
-- >>> :t take $(fx "Hello")
-- take $(fx "Hello")
--   :: (KnownNat l2, KnownNat u2, B.IsBytes a,
--       Data.String.IsString a) =>
--      C.BoundedSize l2 u2 a
--
-- >>> :t take $(fx "Hello") :: C.BoundedSize 4 32 ByteString
-- take $(fx "Hello") :: C.BoundedSize 4 32 ByteString
--   :: C.BoundedSize 4 32 ByteString
-- >>> take $(fx "Hello") :: C.BoundedSize 4 32 ByteString
-- "Hello"
-- >>> length (take $(fx "Hello") :: C.BoundedSize 4 32 ByteString)
-- 5
-- >>> take $(fx "Hello") :: C.BoundedSize 4 32 ByteString
-- "Hello"
-- >>> take $(fx "Hello") :: C.BoundedSize 4 4 ByteString
-- "Hell"
-- >>> take $(fx "HelloHelloHelloHelloHelloHelloHello") :: C.BoundedSize 4 32 ByteString
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

-- | Reduce BoundedSize length, preferring elements on the right.
--
-- >>> drop $(fx "Foobar") :: C.BoundedSize 2 2 String
-- "ar"
-- >>> drop $(mx "Foobar") :: C.BoundedSize 0 2 String
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

-- | Fill a BoundedSize with extra elements up to target length, padding
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
