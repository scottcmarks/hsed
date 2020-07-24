{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : System.SED.MCTP.Common.Reference_Types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Reference_Types per Core Spec

-}




module System.SED.MCTP.Common.Reference_Types

where


import           Data.Refined                      (type (?), plain,
                                                    unsafeCreate)
import           Data.Word8                        (Word8)
import           GHC.Base                          (Eq (..), Ord (..),
                                                    undefined, ($), (.))
import           GHC.Enum                          (Enum (..))
import           GHC.Exts                          (IsList (..))
import           GHC.Show                          (Show (..))
import           Test.QuickCheck                   (Arbitrary (..))

import           System.SED.MCTP.Common.StreamItem (StreamItem (..))
import           System.SED.MCTP.Common.Token      (IsToken (..))
import           System.SED.MCTP.Common.UID        (HalfUID (..), UID (..),
                                                    hNull, halfUID,
                                                    showCore_bytes, uid,
                                                    uidUpper, (+:+))

data TableKinds =
    Null_Table
  | Object_Table
  | Byte_Table
    deriving (Enum, Eq, Show)


type role TableKind phantom _
newtype TableKind (k::TableKinds) a = TableKind a
   deriving (Eq,Ord,Show,StreamItem,IsList,Arbitrary,IsToken) via a



instance StreamItem a => StreamItem (a ? TableKind k)
  where generate = generate . plain
        parser = undefined -- this works --> unsafeCreate <$> parser  -- TODO Hmm.  I suppose the parser is a de facto predicate

type Table_HalfUID k      = HalfUID ? (TableKind k)

type Table_UID k          = UID ? (TableKind k)


class IsTable_HalfUID a where
    fromTable_HalfUID :: Table_HalfUID k -> a
    toTable_HalfUID   :: a -> Table_HalfUID k

instance IsTable_HalfUID (Table_HalfUID k) where
    fromTable_HalfUID = unsafeCreate . plain
    toTable_HalfUID   = unsafeCreate . plain

instance IsTable_HalfUID (Table_UID k) where
    fromTable_HalfUID = unsafeCreate . (+:+ hNull) . plain
    toTable_HalfUID   = unsafeCreate . uidUpper    . plain

instance IsTable_HalfUID (HalfUID) where
    fromTable_HalfUID = plain
    toTable_HalfUID = unsafeCreate



newtype Null_Table_HalfUID   = Null_Table_HalfUID (Table_HalfUID 'Null_Table)
    deriving(Eq, Ord, StreamItem,IsTable_HalfUID) via (Table_HalfUID 'Null_Table)
instance Show Null_Table_HalfUID where
     show (Null_Table_HalfUID t) = showCore_bytes "ntHalfUID" fb
         where (HalfUID fb) = plain t

newtype Object_Table_HalfUID   = Object_Table_HalfUID (Table_HalfUID 'Object_Table)
    deriving(Eq, Ord, StreamItem,IsTable_HalfUID) via (Table_HalfUID 'Object_Table)
instance Show Object_Table_HalfUID where
     show (Object_Table_HalfUID t) = showCore_bytes "otHalfUID" fb
         where (HalfUID fb) = plain t

newtype Byte_Table_HalfUID   = Byte_Table_HalfUID (Table_HalfUID 'Byte_Table)
    deriving(Eq, Ord, StreamItem,IsTable_HalfUID) via (Table_HalfUID 'Byte_Table)
instance Show Byte_Table_HalfUID where
     show (Byte_Table_HalfUID t) = showCore_bytes "btHalfUID" fb
         where (HalfUID fb) = plain t

newtype Null_Table_UID   = Null_Table_UID (Table_UID 'Null_Table)
    deriving(Eq, Ord, StreamItem,IsTable_HalfUID) via (Table_UID 'Null_Table)
instance Show Null_Table_UID where
     show (Null_Table_UID t) = showCore_bytes "ntUID" fb
         where (UID fb) = plain t

newtype Object_Table_UID   = Object_Table_UID (Table_UID 'Object_Table)
    deriving(Eq, Ord, StreamItem,IsTable_HalfUID) via (Table_UID 'Object_Table)
instance Show Object_Table_UID where
     show (Object_Table_UID t) = showCore_bytes "otUID" fb
         where (UID fb) = plain t

newtype Byte_Table_UID   = Byte_Table_UID (Table_UID 'Byte_Table)
    deriving(Eq, Ord, StreamItem,IsTable_HalfUID) via (Table_UID 'Byte_Table)
instance Show Byte_Table_UID where
     show (Byte_Table_UID t) = showCore_bytes "btUID" fb
         where (UID fb) = plain t


nthNull ::  Null_Table_HalfUID
nthNull = fromTable_HalfUID $ toTable_HalfUID $ hNull

ntuNull :: Null_Table_UID
ntuNull = fromTable_HalfUID $ toTable_HalfUID $ hNull


ntHalfUID :: Word8 -> Word8 -> Word8 -> Word8 -> Null_Table_HalfUID
ntHalfUID b3 b2 b1 b0 = Null_Table_HalfUID   $ unsafeCreate $ halfUID b3 b2 b1 b0

btHalfUID :: Word8 -> Word8 -> Word8 -> Word8 -> Byte_Table_HalfUID
btHalfUID b3 b2 b1 b0 = Byte_Table_HalfUID   $ unsafeCreate $ halfUID b3 b2 b1 b0

otHalfUID :: Word8 -> Word8 -> Word8 -> Word8 -> Object_Table_HalfUID
otHalfUID b3 b2 b1 b0 = Object_Table_HalfUID $ unsafeCreate $ halfUID b3 b2 b1 b0


ntUID :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Null_Table_UID
ntUID u3 u2 u1 u0 l3 l2 l1 l0 = Null_Table_UID   $ unsafeCreate $ uid u3 u2 u1 u0 l3 l2 l1 l0

btUID :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Byte_Table_UID
btUID u3 u2 u1 u0 l3 l2 l1 l0 = Byte_Table_UID   $ unsafeCreate $ uid u3 u2 u1 u0 l3 l2 l1 l0

otUID :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Object_Table_UID
otUID u3 u2 u1 u0 l3 l2 l1 l0 = Object_Table_UID $ unsafeCreate $ uid u3 u2 u1 u0 l3 l2 l1 l0
