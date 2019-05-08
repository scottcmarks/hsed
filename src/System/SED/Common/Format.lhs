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
           -- , ScopedTypeVariables
           , GADTs
           -- , PolyKinds
           , FlexibleInstances
           , StandaloneDeriving
           , OverloadedStrings
#-}



module System.SED.Common.Format
    where

import Data.ByteString
import Data.Maybe
import GHC.Base
-- import GHC.Natural
import GHC.Real
import GHC.Show
import GHC.TypeNats


data Core_table_kind :: Nat -> *
  where
    Object_table :: Core_table_kind 1
    Byte_table   :: Core_table_kind 2
deriving instance Show (Core_table_kind n)
deriving instance Eq (Core_table_kind n)


data Core_uinteger_2   = Core_uinteger_2
data Core_integer_2    = Core_integer_2
data Core_uidref       = Core_uidref
data Core_max_bytes_32 = Core_max_bytes_32

newtype Core_uidref_Base_Type_only     = Core_uidref_Base_Type_only     Core_uidref
newtype Core_uidref_non_Base_Type_only = Core_uidref_non_Base_Type_only Core_uidref
newtype Core_uidref_Byte_table_only    = Core_uidref_Byte_table_only    Core_uidref
newtype Core_uidref_Object_table_only  = Core_uidref_Object_table_only  Core_uidref

data Core_Type :: Nat -> *
    where
        Base_Type                    ::                                                          Core_Type  0
        Simple_Type                  :: Core_uidref_Base_Type_only -> Core_uinteger_2         -> Core_Type  1
        Enumeration_Type             :: [(Core_uinteger_2, Core_uinteger_2)]                  -> Core_Type  2 -- 1 <= length
        Alternative_Type             ::                      [Core_uidref_non_Base_Type_only] -> Core_Type  3 -- 2 <= length
        List_Type                    :: Core_uinteger_2   -> [Core_uidref_non_Base_Type_only] -> Core_Type  4
        Restricted_Reference_Type'5  ::                      [Core_uidref_Byte_table_only]    -> Core_Type  5
        Restricted_Reference_Type'6  ::                      [Core_uidref_Object_table_only]  -> Core_Type  6
        General_Reference_Type'7     ::                                                          Core_Type  7
        General_Reference_Type'8     ::                                                          Core_Type  8
        General_Reference_Type'9     ::                                                          Core_Type  9
        General_Reference_Table_Type :: Core_table_kind n                                     -> Core_Type 10
        Named_Value_Name_Type        :: Core_max_bytes_32 ->  Core_uidref_non_Base_Type_only  -> Core_Type 11
        Named_Value_Integer_Type     :: Core_integer_2    ->  Core_uidref_non_Base_Type_only  -> Core_Type 12
        Named_Value_Uinteger_Type    :: Core_uinteger_2   ->  Core_uidref_non_Base_Type_only  -> Core_Type 13
        Struct_Type                  ::                      [Core_uidref_non_Base_Type_only] -> Core_Type 14 -- 1 <= length
        Set_Type                     :: [(Core_uinteger_2, Core_uinteger_2)]                  -> Core_Type 15 -- 1 <= length

instance (KnownNat n) => Show (Core_Type n)
    where show ct = "{- Core_Type " <> show (natVal ct) <> " -}"

class IsFormatItem a where
    encode :: a -> ByteString
    encode _ct = undefined
    decode :: ByteString -> Maybe a
    decode _bs = undefined

instance (KnownNat n) => IsFormatItem(Core_Type n) where
    encode ct = singleton (fromIntegral (natVal ct)) <> encodeData ct
      where encodeData _ct = mempty

instance (IsFormatItem a) => IsFormatItem([a]) where
    encode = mconcat . fmap encode
instance IsFormatItem(Core_uinteger_2  ) where
    encode _ = "<uinteger_2>"
instance IsFormatItem(Core_integer_2   ) where
    encode _ = "<integer_2>"
instance IsFormatItem(Core_uidref      ) where
    encode _ = "<uidref>"
instance IsFormatItem(Core_max_bytes_32) where
    encode _ = "<max_bytes_32>"


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
