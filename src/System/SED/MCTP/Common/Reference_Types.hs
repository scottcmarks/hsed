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

-- import           Data.Functor                      ((<$>))
import           Data.Refined                      (type (?), plain,
                                                    unsafeCreate)
import           GHC.Base                          (Eq (..), Ord (..),
                                                    undefined, ($), (.))
import           GHC.Enum                          (Enum (..))
import           GHC.Show                          (Show (..))
import           System.SED.MCTP.Common.StreamItem (StreamItem (..))
import           System.SED.MCTP.Common.TableUIDs  ()
import           System.SED.MCTP.Common.UID        (HalfUID (..), UID (..),
                                                    hNull, (+:+))

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

instance IsTable_HalfUID (Table_HalfUID k) where
    fromTable_HalfUID th = unsafeCreate $ plain th

instance IsTable_HalfUID (Table_UID k) where
    fromTable_HalfUID th = unsafeCreate $ plain th +:+ hNull


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
