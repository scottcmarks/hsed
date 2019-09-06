{-# OPTIONS_GHC -fno-warn-type-defaults #-}
\documentstyle{article}
\begin{document}
\chapter{Formats}

Define the descriptions of types by the Format values in the Types table.


\begin{code}
{-|
Module      : System.SED.Common.Format
Description : Core data type formats
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Formats.

-}

{-# LANGUAGE NoImplicitPrelude
           , DataKinds
           , DeriveAnyClass
           , KindSignatures
           , ScopedTypeVariables
           , GADTs
           , PolyKinds
           , FlexibleInstances
           , StandaloneDeriving
           , OverloadedStrings
#-}

{-# LANGUAGE FlexibleContexts
#-}

module System.SED.Common.Format
    where

import            Data.Attoparsec.ByteString (many1, anyWord8)
import Data.ByteString(ByteString, singleton)
import Data.Functor((<$>))
import GHC.Enum(Enum(..))
import GHC.Base((<*>), pure, error, (<>), (.), mempty, undefined, Eq(..), Int)
-- import GHC.Natural
import GHC.Real(fromIntegral)
import GHC.Show(Show)
import GHC.TypeNats(natVal, KnownNat)
import GHC.Types(Nat)

import System.SED.Common.UID(UID(..))
import System.SED.Common.StreamItem

data Max_bytes :: Nat -> *
deriving instance Show(Max_bytes n)
deriving instance Eq(Max_bytes n)

instance StreamItem Core_table_kind
    where generate = singleton . fromIntegral . succ . fromEnum
          parser = toEnum <$> pred <$> fromIntegral <$> anyWord8


data Core_table_kind = Object_table | Byte_table
    deriving (Enum, Eq, Show)

data Core_uinteger_2   = Core_uinteger_2 {fromCore_uinteger_2::Int}
    deriving (Eq,Show) -- , StreamItem) -- FIXME
data Core_integer_2    = Core_integer_2 {fromCore_integer_2::Int}
    deriving (Eq,Show)
data Core_uidref       = Core_uidref {fromCore_uidref::UID}
    deriving (Eq,Show)
data Core_max_bytes_32 = Core_max_bytes_32 {fromCore_max_bytes_32::Max_bytes 32} -- FIXME:  Need non-bogus max_bytes
    deriving (Eq,Show)

newtype Core_uidref_Base_Type     = Core_uidref_Base_Type     Core_uidref
    deriving (Eq,Show)
newtype Core_uidref_non_Base_Type = Core_uidref_non_Base_Type Core_uidref
    deriving (Eq,Show)
newtype Core_uidref_Byte_table    = Core_uidref_Byte_table    Core_uidref
    deriving (Eq,Show)
newtype Core_uidref_Object_table  = Core_uidref_Object_table  Core_uidref
    deriving (Eq,Show)

data Known_Core_Type :: Nat -> *
    where
        Base_Type                    ::                                                     Known_Core_Type  0
        Simple_Type                  :: Core_uidref_Base_Type -> Core_uinteger_2         -> Known_Core_Type  1
        Enumeration_Type             :: [(Core_uinteger_2, Core_uinteger_2)]             -> Known_Core_Type  2 -- 1 <= length
        Alternative_Type             ::                      [Core_uidref_non_Base_Type] -> Known_Core_Type  3 -- 2 <= length
        List_Type                    :: Core_uinteger_2   ->  Core_uidref_non_Base_Type  -> Known_Core_Type  4
        Restricted_Reference_Type'5  ::                      [Core_uidref_Byte_table]    -> Known_Core_Type  5
        Restricted_Reference_Type'6  ::                      [Core_uidref_Object_table]  -> Known_Core_Type  6
        General_Reference_Type'7     ::                                                     Known_Core_Type  7
        General_Reference_Type'8     ::                                                     Known_Core_Type  8
        General_Reference_Type'9     ::                                                     Known_Core_Type  9
        General_Reference_Table_Type :: Core_table_kind                                  -> Known_Core_Type 10
        Named_Value_Name_Type        :: Core_max_bytes_32 ->  Core_uidref_non_Base_Type  -> Known_Core_Type 11
        Named_Value_Integer_Type     :: Core_integer_2    ->  Core_uidref_non_Base_Type  -> Known_Core_Type 12
        Named_Value_Uinteger_Type    :: Core_uinteger_2   ->  Core_uidref_non_Base_Type  -> Known_Core_Type 13
        Struct_Type                  ::                      [Core_uidref_non_Base_Type] -> Known_Core_Type 14 -- 1 <= length
        Set_Type                     :: [(Core_uinteger_2, Core_uinteger_2)]             -> Known_Core_Type 15 -- 1 <= length

deriving instance Show (Known_Core_Type n)
deriving instance Eq (Known_Core_Type n)

data Some_Core_Type = forall (n :: Nat). KnownNat n => Some_Core_Type (Known_Core_Type n)

deriving instance Show (Some_Core_Type)
-- deriving instance Eq (Some_Core_Type)

instance StreamItem(Some_Core_Type) where
    generate (Some_Core_Type ct) = singleton (fromIntegral (natVal ct)) <> genFields ct
      where genFields :: Known_Core_Type n -> ByteString
            genFields Base_Type = mempty
            genFields (Simple_Type base_uidref size) = generate base_uidref <> generate size
            genFields (Enumeration_Type ranges) = generate ranges
            genFields (Alternative_Type alternatives) = generate alternatives
            genFields (List_Type maxSize elementType) = generate maxSize <> generate elementType
            genFields (Restricted_Reference_Type'5 uidrefs) = generate uidrefs
            genFields (Restricted_Reference_Type'6 uidrefs) = generate uidrefs
            genFields General_Reference_Type'7 = mempty
            genFields General_Reference_Type'8 = mempty
            genFields General_Reference_Type'9 = mempty
            genFields (General_Reference_Table_Type k) = generate k
            genFields (Named_Value_Name_Type     name uidref) = generate name <> generate uidref
            genFields (Named_Value_Integer_Type  int  uidref) = generate int  <> generate uidref
            genFields (Named_Value_Uinteger_Type uint uidref) = generate uint <> generate uidref
            genFields (Struct_Type flds ) = generate flds
            genFields (Set_Type ranges) = generate ranges
    parser = do tag <- anyWord8
                case tag of
                   0 -> Some_Core_Type <$>
                          pure Base_Type
                   1 -> Some_Core_Type <$>
                          (Simple_Type <$> parser <*> parser)
                   2 -> Some_Core_Type <$>
                          (Enumeration_Type <$> many1 ( (,) <$> parser <*> parser))
                   3 -> Some_Core_Type <$>
                          (Alternative_Type <$> ( (:) <$> parser <*> many1 parser))
                   4 -> Some_Core_Type <$>
                          (List_Type <$> parser <*> parser)
                   5 -> Some_Core_Type <$>
                          (Restricted_Reference_Type'5 <$> many1 parser)
                   6 -> Some_Core_Type <$>
                          (Restricted_Reference_Type'6 <$> many1 parser)
                   7 -> Some_Core_Type <$>
                          pure General_Reference_Type'7
                   8 -> Some_Core_Type <$>
                          pure General_Reference_Type'8
                   9 -> Some_Core_Type <$>
                          pure General_Reference_Type'9
                   10 -> Some_Core_Type <$>
                          (General_Reference_Table_Type <$> parser)
                   11 -> Some_Core_Type <$>
                          (Named_Value_Name_Type <$> parser <*> parser)
                   12 -> Some_Core_Type <$>
                          (Named_Value_Integer_Type <$> parser <*> parser)
                   13 -> Some_Core_Type <$>
                          (Named_Value_Uinteger_Type <$> parser <*> parser)
                   14 -> Some_Core_Type <$>
                          (Struct_Type <$> many1 parser)
                   15 -> Some_Core_Type <$>
                          (Set_Type <$> many1 ( (,) <$> parser <*> parser))
                   _ -> error "Unknown tag"




instance StreamItem(Core_uinteger_2  ) where
    parser = undefined
    generate _ = "<uinteger_2>"
instance StreamItem(Core_integer_2   ) where
    parser = undefined
    generate _ = "<integer_2>"
instance StreamItem(Core_uidref      ) where
    parser = undefined
    generate _ = "<uidref>"
instance StreamItem(Core_max_bytes_32) where
    parser = undefined
    generate _ = "<max_bytes_32>"
instance StreamItem(Core_uidref_Base_Type) where
    parser = undefined
    generate (Core_uidref_Base_Type _base_uidref) = "<uidref_Base_Type>"
instance StreamItem(Core_uidref_non_Base_Type) where
    parser = undefined
    generate (Core_uidref_non_Base_Type _base_uidref) = "<uidref_non_Base_Type>"
instance StreamItem(Core_uidref_Byte_table) where
    parser = undefined
    generate (Core_uidref_Byte_table _base_uidref) = "<uidref_Byte_table>"
instance StreamItem(Core_uidref_Object_table) where
    parser = undefined
    generate (Core_uidref_Object_table _base_uidref) = "<uidref_Object_table>"

-- newtype Non_Base_Type_uidref = Non_Base_Type_uidref Core_uidref
--     deriving (Eq,Ord,Show)
-- isNon_Base_Type_uidref :: Core_uidref -> Bool
-- isNon_Base_Type_uidref u = u >> True -- FIXME
-- mkNon_Base_Type_uidref :: Core_uidref -> Non_Base_Type_uidref
-- mkNon_Base_Type_uidref u = assert (isNon_Base_Type_uidref u) (Non_Base_Type_uidref u)
-- unNon_Base_Type_uidref :: Non_Base_Type_uidref -> Core_uidref
-- unNon_Base_Type_uidref (Non_Base_Type_uidref u) = u

{-
table_kind = 1|2
Base_Type = 0
Simple_Type = 1 bytes_8 uinteger_2
Enumeration_Type = 2 1*(uinteger_2 uinteger_2)
Alternative_Type = 3 2*bytes_8
List_Type = 4 uinteger_2 bytes_8
Restricted_Reference_Type = 5|6 1*bytes_8
General_Reference_Type = 7|8|9
General_Reference_Table_Type = 10 table_kind
Named_Value_Name_Type = 11 1*32bytes bytes_8
Name_Value_Integer_Type = 12 integer_2 bytes_8
Name_Value_Uinteger_Type = 13 uinteger_2 bytes_8
Struct_Type = 14 1*bytes_8
Set_Type = 15 1*(uinteger_2 uinteger_2)
-}
\end{code}
\end{document}
