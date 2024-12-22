{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module      : TypeTests
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental
-}

module TypeTests where

import           Data.Coerce (coerce)
import           GHC.Base    (($))
import           GHC.Base    ((.))
import           GHC.Classes (Eq (..), Ord (..))
import           GHC.Enum    (Enum (..))
import           GHC.Read    (Read (..))
import           GHC.Show    (Show (..), showString)
import           GHC.Show    (shows)
import           GHC.Types   (Type)
import           GHC.Word    (Word8)


newtype (a::Type) ? (p::k) = Refined a
    deriving (Eq,Ord,Read)
deriving instance forall (a::Type) k (p::k) . (Show a) => Show (a ? p)
infix 1 ?


data Table_Kind = Null | Object_Table | Byte_Table
    deriving (Eq,Enum,Show,Read)

data Bytes4 = Bytes4 Word8 Word8 Word8 Word8
    deriving (Eq,Ord,Show,Read)
b4Null :: Bytes4
b4Null = Bytes4 0 0 0 0

b4SP :: Bytes4
b4SP = Bytes4 0x00 0x02 0x00 0x05

newtype HalfUID = HalfUID Bytes4
    deriving (Eq,Ord,Show,Read)

hNull :: HalfUID
hNull = HalfUID b4Null

data Bytes8 = Bytes8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8
    deriving (Eq,Ord,Show,Read)
b8Null :: Bytes8
b8Null = Bytes8 0 0 0 0 0 0 0 0

newtype UID = UID Bytes8
    deriving (Eq,Ord,Show,Read)

uNull :: UID
uNull = UID b8Null

newtype For_Table_Kind (k::Table_Kind) (a::Type) = For_Table_Kind (a ? k)
    deriving (Eq,Ord,Read)
instance forall (a :: Type) (k :: Table_Kind)  .  (Show a) => Show(For_Table_Kind k a) where
    show (For_Table_Kind (Refined a)) = showString "For_Table_Kind (" . shows a $ ")"



type HalfUID_For_Table_Kind (k::Table_Kind) = For_Table_Kind k HalfUID

type For_Object_Table (a::Type) = For_Table_Kind 'Object_Table a


type HalfUID_For_Object_Table = For_Table_Kind 'Object_Table HalfUID
type HalfUID_For_Object_Table' = HalfUID_For_Table_Kind 'Object_Table
type HalfUID_For_Object_Table'' = For_Object_Table HalfUID

nthNull :: HalfUID ? For_Table_Kind 'Null
nthNull = coerce b4Null

othSP :: HalfUID ? For_Table_Kind 'Object_Table
othSP = coerce b4SP

class Is_HalfUID_For_Table_Kind (k::Table_Kind) (a::Type) where

instance Is_HalfUID_For_Table_Kind 'Object_Table HalfUID_For_Object_Table
