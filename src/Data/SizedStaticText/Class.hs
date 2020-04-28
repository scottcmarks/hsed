{-# LANGUAGE CPP           #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

{-|

Use this module when you need to add an 'IsSizedStaticText' instance to a
type.

-}

module Data.SizedStaticText.Class
       ( IsSizedStaticText(..)
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


-- | Class of types which can be assigned a type-level minimum and maximum length.
class IsSizedStaticText a where
  -- | Data family which wraps values of the underlying type giving
  -- them type-level length bounds. @SizedStatic t 6 10@ means a value of type @t@ of
  -- length between 6 and 10.
  data SizedStatic a (l :: Nat) (u :: Nat)

  -- | Basic element type. For @IsSizedStaticText [a]@, this is @a@.
  type Elem a

  -- | Simply wrap a value in a SizedStatic as is, assuming any length.
  --
  -- __WARNING__ Use it only when you know what you're doing.
  --
  -- For example, an expression like
  --
  -- > unsafeCreate "somestring" :: SizedStatic String 50 100
  --
  -- will typecheck, although the stored length information will not
  -- match actual string size. This may result in wrong behaviour of
  -- all functions defined for "IsSizedStaticText".
  --
  -- When writing new "IsSizedStaticText" instances, make this simply apply
  -- the constructor of "SizedStatic".
  unsafeCreate :: a -> SizedStatic a l u

  -- | Obtain the underlying value.
  unwrap :: SizedStatic a l u -> a

  length :: a -> Int
  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a


instance (Show a, IsSizedStaticText a) => Show (SizedStatic a l u) where
  show = show . unwrap
  showsPrec p = showsPrec p . unwrap

instance IsSizedStaticText [a] where
  type Elem [a] = a

  data SizedStatic [a] l u = List [a]
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
instance IsSizedStaticText T.Text where
  type Elem T.Text = Char

  data SizedStatic T.Text l u = Text T.Text
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
instance IsSizedStaticText B.ByteString where
  type Elem B.ByteString = Word8

  data SizedStatic B.ByteString l u = ByteString B.ByteString
    deriving (Eq, Ord)

  unsafeCreate = ByteString
  unwrap (ByteString t) = t

  length = B.length
  append = B.append
  replicate = B.replicate
  map = B.map
  take = B.take
  drop = B.drop

-- | IsSizedStaticText instance for 'BS.ShortByteString' uses intermediate
-- 'B.ByteString's (pinned) for all modification operations.
instance IsSizedStaticText BS.ShortByteString where
  type Elem BS.ShortByteString = Word8

  data SizedStatic BS.ShortByteString l u = ByteStringS BS.ShortByteString
    deriving (Eq, Ord)

  unsafeCreate = ByteStringS
  unwrap (ByteStringS t) = t

  length = BS.length
  append a b = BS.toShort $ B.append (BS.fromShort a) (BS.fromShort b)
  replicate n = BS.toShort . B.replicate n
  map f = BS.toShort . B.map f . BS.fromShort
  take n = BS.toShort . B.take n . BS.fromShort
  drop n = BS.toShort . B.drop n . BS.fromShort
#endif


#ifdef WITH_VECTOR
instance IsSizedStaticText (V.Vector a) where
  type Elem (V.Vector a) = a

  data SizedStatic (V.Vector a) l u = Vector (V.Vector a)
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
