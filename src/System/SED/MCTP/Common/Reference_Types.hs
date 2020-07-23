{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import           GHC.Show                          (Show (..))
import           System.SED.MCTP.Common.StreamItem (StreamItem (..))
import           System.SED.MCTP.Common.UID        (HalfUID (..), UID (..),
                                                    hNull, halfUID, uidUpper,
                                                    (+:+))

data TableKinds =
    Null_Table
  | Object_Table
  | Byte_Table
    deriving (Enum, Eq, Show)


type role TableKind phantom _
newtype TableKind (k::TableKinds) a = TableKind a
   deriving (Eq,Ord,Show,StreamItem) via a

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


type Null_Table_HalfUID   = Table_HalfUID 'Null_Table
type Object_Table_HalfUID = Table_HalfUID 'Object_Table
type Byte_Table_HalfUID   = Table_HalfUID 'Byte_Table

type Null_Table_UID       = Table_UID 'Null_Table
type Object_Table_UID     = Table_UID 'Object_Table
type Byte_Table_UID       = Table_UID 'Byte_Table


nthNull :: Null_Table_HalfUID
nthNull = unsafeCreate $ hNull

ntuNull :: Null_Table_UID
ntuNull = fromTable_HalfUID nthNull


tHalfUID :: Word8 -> Word8 -> Word8 -> Word8 -> Table_HalfUID k
tHalfUID b3 b2 b1 b0 = unsafeCreate $ halfUID b3 b2 b1 b0


btHalfUID :: Word8 -> Word8 -> Word8 -> Word8 -> Byte_Table_HalfUID
btHalfUID = tHalfUID

otHalfUID :: Word8 -> Word8 -> Word8 -> Word8 -> Object_Table_HalfUID
otHalfUID = tHalfUID
