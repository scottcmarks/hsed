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

import            Data.Attoparsec.ByteString (anyWord8)
import Data.ByteString(ByteString, singleton)
import Data.Functor((<$>))
import GHC.Enum(Enum(..))
import GHC.Base(error, (<>), (.), mempty, undefined, Eq(..), Int)
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
    deriving (Eq,Show)
data Core_integer_2    = Core_integer_2 {fromCore_integer_2::Int}
    deriving (Eq,Show)
data Core_uidref       = Core_uidref {fromCore_uidref::UID}
    deriving (Eq,Show)
data Core_max_bytes_32 = Core_max_bytes_32 {fromCore_max_bytes_32::Max_bytes 32} -- FIXME:  Need non-bogus max_bytes
    deriving (Eq,Show)

newtype Core_uidref_Base_Type_only     = Core_uidref_Base_Type_only     Core_uidref
    deriving (Eq,Show)
newtype Core_uidref_non_Base_Type_only = Core_uidref_non_Base_Type_only Core_uidref
    deriving (Eq,Show)
newtype Core_uidref_Byte_table_only    = Core_uidref_Byte_table_only    Core_uidref
    deriving (Eq,Show)
newtype Core_uidref_Object_table_only  = Core_uidref_Object_table_only  Core_uidref
    deriving (Eq,Show)

data Core_Type :: Nat -> *
    where
        Base_Type                    ::                                                          Core_Type  0
        Simple_Type                  :: Core_uidref_Base_Type_only -> Core_uinteger_2         -> Core_Type  1
        Enumeration_Type             :: [(Core_uinteger_2, Core_uinteger_2)]                  -> Core_Type  2 -- 1 <= length
        Alternative_Type             ::                      [Core_uidref_non_Base_Type_only] -> Core_Type  3 -- 2 <= length
        List_Type                    :: Core_uinteger_2   ->  Core_uidref_non_Base_Type_only  -> Core_Type  4
        Restricted_Reference_Type'5  ::                      [Core_uidref_Byte_table_only]    -> Core_Type  5
        Restricted_Reference_Type'6  ::                      [Core_uidref_Object_table_only]  -> Core_Type  6
        General_Reference_Type'7     ::                                                          Core_Type  7
        General_Reference_Type'8     ::                                                          Core_Type  8
        General_Reference_Type'9     ::                                                          Core_Type  9
        General_Reference_Table_Type :: Core_table_kind                                       -> Core_Type 10
        Named_Value_Name_Type        :: Core_max_bytes_32 ->  Core_uidref_non_Base_Type_only  -> Core_Type 11
        Named_Value_Integer_Type     :: Core_integer_2    ->  Core_uidref_non_Base_Type_only  -> Core_Type 12
        Named_Value_Uinteger_Type    :: Core_uinteger_2   ->  Core_uidref_non_Base_Type_only  -> Core_Type 13
        Struct_Type                  ::                      [Core_uidref_non_Base_Type_only] -> Core_Type 14 -- 1 <= length
        Set_Type                     :: [(Core_uinteger_2, Core_uinteger_2)]                  -> Core_Type 15 -- 1 <= length

deriving instance Show (Core_Type n)
deriving instance Eq (Core_Type n)

data Some_Core_Type = forall (n :: Nat). KnownNat n => Some_Core_Type (Core_Type n)

deriving instance Show (Some_Core_Type)
-- deriving instance Eq (Some_Core_Type)

instance StreamItem(Some_Core_Type) where
    generate = undefined
    parser = undefined

instance (KnownNat n) => StreamItem(Core_Type n) where
    generate ct = singleton (fromIntegral (natVal ct)) <> generateData ct
      where generateData :: Core_Type n -> ByteString
            generateData Base_Type = mempty
            generateData (Simple_Type base_uidref size) = generate base_uidref <> generate size
            generateData (Enumeration_Type ranges) = generate ranges
            generateData (Alternative_Type alternatives) = generate alternatives
            generateData (List_Type maxSize elementType) = generate maxSize <> generate elementType
            generateData (Restricted_Reference_Type'5 uidrefs) = generate uidrefs
            generateData (Restricted_Reference_Type'6 uidrefs) = generate uidrefs
            generateData General_Reference_Type'7 = mempty
            generateData General_Reference_Type'8 = mempty
            generateData General_Reference_Type'9 = mempty
            generateData (General_Reference_Table_Type k) = generate k
            generateData (Named_Value_Name_Type     name uidref) = generate name <> generate uidref
            generateData (Named_Value_Integer_Type  int  uidref) = generate int  <> generate uidref
            generateData (Named_Value_Uinteger_Type uint uidref) = generate uint <> generate uidref
            generateData (Struct_Type flds ) = generate flds
            generateData (Set_Type ranges) = generate ranges
    parser = do tag <- anyWord8
                case tag of
                  _ -> error "Unknown tag"


    -- maybeParse bs' =
    --     case bs' of
    --       "" ->  error "Can't maybeParse empty bytestring"
    --       bs -> maybeParser (head bs) bs
    --   where
    --     maybeParser :: Word8 -> ByteString -> Maybe (Core_Type n)
    --     maybeParser hd = undefined hd
          -- ( case hd of
          --   0 -> Just <$> const Base_Type
          --   1 -> Just <$> Simple_Type                    <$> maybeParse <*> maybeParse
          --   2 -> Just <$> Enumeration_Type               <$> maybeParse
          --   3 -> Just <$> Alternative_Type               <$> maybeParse
          --   4 -> Just <$> List_Type                      <$> maybeParse <*> maybeParse
          --   5 -> Just <$> Restricted_Reference_Type'5    <$> maybeParse
          --   6 -> Just <$> Restricted_Reference_Type'6    <$> maybeParse
          --   7 -> Just <$> const General_Reference_Type'7
          --   8 -> Just <$> const General_Reference_Type'8
          --   9 -> Just <$> const General_Reference_Type'9
          --   10 -> Just <$> General_Reference_Table_Type   <$> maybeParse
          --   11 -> Just <$> Named_Value_Name_Type          <$> maybeParse <*> maybeParse
          --   12 -> Just <$> Named_Value_Integer_Type       <$> maybeParse <*> maybeParse
          --   13 -> Just <$> Named_Value_Uinteger_Type      <$> maybeParse <*> maybeParse
          --   14 -> Just <$> Struct_Type                    <$> maybeParse
          --   15 -> Just <$> Set_Type                       <$> maybeParse
          --   _ -> error "Illegal Core_Type tag"
          -- )
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
instance StreamItem(Core_uidref_Base_Type_only) where
    parser = undefined
    generate (Core_uidref_Base_Type_only _base_uidref) = "<uidref_Base_Type_only>"
instance StreamItem(Core_uidref_non_Base_Type_only) where
    parser = undefined
    generate (Core_uidref_non_Base_Type_only _base_uidref) = "<uidref_non_Base_Type_only>"
instance StreamItem(Core_uidref_Byte_table_only) where
    parser = undefined
    generate (Core_uidref_Byte_table_only _base_uidref) = "<uidref_Byte_table_only>"
instance StreamItem(Core_uidref_Object_table_only) where
    parser = undefined
    generate (Core_uidref_Object_table_only _base_uidref) = "<uidref_Object_table_only>"

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
