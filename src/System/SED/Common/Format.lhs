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

import Data.ByteString(ByteString,head,singleton)
import Data.Functor((<$>))
import GHC.Enum(Enum(..))
import GHC.Maybe(Maybe(..))
import GHC.Base((<*>), const, error, (<>), fmap, (.), mconcat, mempty, undefined, Eq(..), Int)
-- import GHC.Natural
import GHC.Real(fromIntegral)
import GHC.Show(Show)
import GHC.TypeNats(natVal, KnownNat)
import GHC.Types(Nat)

import System.SED.Common.UID(UID(..))

data Max_bytes :: Nat -> *
deriving instance Show(Max_bytes n)
deriving instance Eq(Max_bytes n)

class IsFormatItem a where
    encode :: a -> ByteString
    encode _ct = undefined
    decode :: ByteString -> Maybe a
    decode _bs = undefined


data Core_table_kind = Object_table | Byte_table
    deriving (Enum, Eq, Show)

instance IsFormatItem Core_table_kind
    where encode = singleton . fromIntegral . succ . fromEnum
          decode = Just . toEnum . pred . fromIntegral . head

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

instance (KnownNat n) => IsFormatItem(Core_Type n) where
    encode ct = singleton (fromIntegral (natVal ct)) <> encodeData ct
      where encodeData :: Core_Type n -> ByteString
            encodeData Base_Type = mempty
            encodeData (Simple_Type base_uidref size) = encode base_uidref <> encode size
            encodeData (Enumeration_Type ranges) = encode ranges
            encodeData (Alternative_Type alternatives) = encode alternatives
            encodeData (List_Type maxSize elementType) = encode maxSize <> encode elementType
            encodeData (Restricted_Reference_Type'5 uidrefs) = encode uidrefs
            encodeData (Restricted_Reference_Type'6 uidrefs) = encode uidrefs
            encodeData General_Reference_Type'7 = mempty
            encodeData General_Reference_Type'8 = mempty
            encodeData General_Reference_Type'9 = mempty
            encodeData (General_Reference_Table_Type k) = encode k
            encodeData (Named_Value_Name_Type     name uidref) = encode name <> encode uidref
            encodeData (Named_Value_Integer_Type  int  uidref) = encode int  <> encode uidref
            encodeData (Named_Value_Uinteger_Type uint uidref) = encode uint <> encode uidref
            encodeData (Struct_Type flds ) = encode flds
            encodeData (Set_Type ranges) = encode ranges
    decode bs' =
        case bs' of
          "" ->  error "Can't decode empty bytestring"
          bs -> decoder
      where
        decoder :: ByteString -> Maybe (Core_Type n)
        decoder =
          ( case (head bs) of
            0 -> Just <$> const Base_Type
            1 -> Just <$> Simple_Type                    <$> decode <*> decode
            2 -> Just <$> Enumeration_Type               <$> decode
            3 -> Just <$> Alternative_Type               <$> decode
            4 -> Just <$> List_Type                      <$> decode <*> decode
            5 -> Just <$> Restricted_Reference_Type'5    <$> decode
            6 -> Just <$> Restricted_Reference_Type'6    <$> decode
            7 -> Just <$> const General_Reference_Type'7
            8 -> Just <$> const General_Reference_Type'8
            9 -> Just <$> const General_Reference_Type'9
            10 -> Just <$> General_Reference_Table_Type   <$> decode
            11 -> Just <$> Named_Value_Name_Type          <$> decode <*> decode
            12 -> Just <$> Named_Value_Integer_Type       <$> decode <*> decode
            13 -> Just <$> Named_Value_Uinteger_Type      <$> decode <*> decode
            14 -> Just <$> Struct_Type                    <$> decode
            15 -> Just <$> Set_Type                       <$> decode
            _ -> error "Illegal Core_Type tag"
          )
instance (IsFormatItem a) => IsFormatItem([a]) where
    encode = mconcat . fmap encode
instance (IsFormatItem a) => IsFormatItem(a,a) where
    encode (start, stop) = encode start <> encode stop
instance IsFormatItem(Core_uinteger_2  ) where
    encode _ = "<uinteger_2>"
instance IsFormatItem(Core_integer_2   ) where
    encode _ = "<integer_2>"
instance IsFormatItem(Core_uidref      ) where
    encode _ = "<uidref>"
instance IsFormatItem(Core_max_bytes_32) where
    encode _ = "<max_bytes_32>"
instance IsFormatItem(Core_uidref_Base_Type_only) where
    encode (Core_uidref_Base_Type_only _base_uidref) = "<uidref_Base_Type_only>"
instance IsFormatItem(Core_uidref_non_Base_Type_only) where
    encode (Core_uidref_non_Base_Type_only _base_uidref) = "<uidref_non_Base_Type_only>"
instance IsFormatItem(Core_uidref_Byte_table_only) where
    encode (Core_uidref_Byte_table_only _base_uidref) = "<uidref_Byte_table_only>"
instance IsFormatItem(Core_uidref_Object_table_only) where
    encode (Core_uidref_Object_table_only _base_uidref) = "<uidref_Object_table_only>"

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
