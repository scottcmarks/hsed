{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

{-|

Use this module when you need to add an 'IsSizedText' instance to a
type.

-}

module Data.SizedText.Class
       ( IsSizedText(..)
       , Static
       , fromNat
       )

where

import           Prelude
import qualified Prelude               as P

#define WITH_BS
#ifdef WITH_BS
import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as BS
import           GHC.Word
#endif

#define WITH_TEXT
#ifdef WITH_TEXT
import qualified Data.Text             as T
#endif

#define WITH_VECTOR
#ifdef WITH_VECTOR
import qualified Data.Vector           as V
#endif

#if MIN_VERSION_base(4,9,0)
import           GHC.TypeLits          hiding (Text)
#else
import           GHC.TypeLits
#endif

-- $setup
-- >>> -- :set -XDataKinds
-- >>> -- :set -XTemplateHaskell
-- >>> -- :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Data.Char (toUpper)
-- >>> import           Data.Proxy


-- | Extract type-level Nat as a value-level Int
-- >>> fromNat (Proxy @5)
-- 5
fromNat :: (KnownNat n) => proxy n -> Int
fromNat = fromIntegral . natVal

-- | Class of types which can be assigned a type-level minimum and maximum length.
class IsSizedText a where
  -- | Data family which wraps values of the underlying type giving
  -- them a type-level length. @Sized t 6 10@ means a value of type @t@ of
  -- length between 6 and 10.
  data Sized a (l :: Nat) (u::Nat)

  -- | Basic element type. For @IsSizedText [a]@, this is @a@.
  type Elem a

  -- | Simply wrap a value in a Sized as is, assuming any length.
  --
  -- __WARNING__ Use it only when you know what you're doing.
  --
  -- For example, an expression like
  --
  -- > unsafeCreate "somestring" :: Sized String 50 100
  --
  -- will typecheck, although the stored length information will not
  -- match actual string size. This may result in wrong behaviour of
  -- all functions defined for "IsSizedText".
  --
  -- When writing new "IsSizedText" instances, make this simply apply
  -- the constructor of "Sized".
  unsafeCreate :: a -> Sized a l u

  -- | Forget type-level minimum and maximum length, obtaining the underlying value.
  unwrap :: Sized a l u -> a

  length :: a -> Int
  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a


instance (Show a, IsSizedText a) => Show (Sized a l u) where
  show = show . unwrap
  showsPrec p = showsPrec p . unwrap

instance IsSizedText [a] where
  type Elem [a] = a

  data Sized [a] l u = List [a]
    deriving (Eq, Ord)

  unsafeCreate = List
  unwrap (List l) = l

  length = P.length
  append = (P.++)
  replicate = P.replicate
  map = P.map
  take = P.take
  drop = P.drop


#ifdef WITH_TEXT
instance IsSizedText T.Text where
  type Elem T.Text = Char

  data Sized T.Text l u = Text T.Text
    deriving (Eq, Ord)

  unsafeCreate = Text
  unwrap (Text t) = t

  length = T.length
  append = T.append
  replicate n c = T.replicate n (T.singleton c)
  map = T.map
  take = T.take
  drop = T.drop
#endif


#ifdef WITH_BS
instance IsSizedText B.ByteString where
  type Elem B.ByteString = Word8

  data Sized B.ByteString l u = ByteString B.ByteString
    deriving (Eq, Ord)

  unsafeCreate = ByteString
  unwrap (ByteString t) = t

  length = B.length
  append = B.append
  replicate = B.replicate
  map = B.map
  take = B.take
  drop = B.drop

-- | IsSizedText instance for 'BS.ShortByteString' uses intermediate
-- 'B.ByteString's (pinned) for all modification operations.
instance IsSizedText BS.ShortByteString where
  type Elem BS.ShortByteString = Word8

  data Sized BS.ShortByteString l u = ShortByteString BS.ShortByteString
    deriving (Eq, Ord)

  unsafeCreate = ShortByteString
  unwrap (ShortByteString t) = t

  length = BS.length
  append a b = BS.toShort $ B.append (BS.fromShort a) (BS.fromShort b)
  replicate n = BS.toShort . B.replicate n
  map f = BS.toShort . B.map f . BS.fromShort
  take n = BS.toShort . B.take n . BS.fromShort
  drop n = BS.toShort . B.drop n . BS.fromShort
#endif


#ifdef WITH_VECTOR
instance IsSizedText (V.Vector a) where
  type Elem (V.Vector a) = a

  data Sized (V.Vector a) l u = Vector (V.Vector a)
    deriving (Eq, Ord)

  unsafeCreate = Vector
  unwrap (Vector t) = t

  length = V.length
  append = (V.++)
  replicate = V.replicate
  map = V.map
  take = V.take
  drop = V.drop
#endif


-- | Class of types which can be assigned a type-level fixed length.
type Static a (l :: Nat) = Sized a l l
