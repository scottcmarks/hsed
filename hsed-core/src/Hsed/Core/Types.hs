{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Core.Types
Description : The runtime form of a Type object's Format (Core 5.1.1)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A 'TypeDef' is what a Type table row's Format column says, one constructor per
Format code of Core 5.1.1 (@Type = Base_Type \/ Simple_Type \/ ... \/ Set_Type@),
in the form the spec compiler produces from the printed text and the simulator
reads at run time. The spec writes Type objects by name in place of their UIDs
(Core 5.1.3), so a reference is a 'TypeRef' carrying the name until it is
resolved, and an asterisk is 'Star', an SSC-specific or implementation-specific
value. The binary type_def encoding is not here (that is
"System.SED.MCTP.Common.Format").

'Enumerated' ties a generated enumeration type back to its Type object
(design docs/spec-as-source.md section 4.1).
-}
module Hsed.Core.Types
  ( -- * Type definitions
    TypeDef (..)
  , Operand (..)
  , TypeRef (..)
  , TableRef (..)
  , RestrictedKind (..)
  , GeneralKind (..)
  , TableKind (..)
  , formatCode
  , formatName
  , references
    -- * Enumerations
  , Enumerated (..)
  , enumValue
  , enumFromValue
  ) where

import           Prelude                    (toEnum)
import           RIO

import           System.SED.MCTP.Common.UID (UID)


-- | A @uinteger_2@ operand of a Format: a literal, or the spec's @*@, which marks
--   a value an SSC or an implementation fixes (Core 5.1.3: "An asterisk (*) in any
--   of the descriptive tables indicates SSC-specific or implementation-specific
--   values").
data Operand = Lit !Int | Star
  deriving (Eq, Ord, Show)

-- | A Type object named in a Format cell, not yet resolved to its UID
--   (Core 5.1.3: "the names of Type objects are used in place of their UID").
data TypeRef = Ref !Text
  deriving (Eq, Ord, Show)

-- | A table named by a @uidref{...}@ or @ref{...}@ of a Restricted_Reference_Type:
--   a T240 table name (@C_PIN@ for @uidref{C_PINTableUID}@), a glob over table
--   names (@C_AES_*@ in T77 cred_object_uidref), or a bare @*@ (T105 log_row_ref).
data TableRef = Table !Text | TableGlob !Text | TableStar
  deriving (Eq, Ord, Show)

-- | The two Restricted_Reference_Type codes.
data RestrictedKind
  = ByteTableRow    -- ^ code 5: the row number of a row in one of the named byte tables
  | ObjectTableUID  -- ^ code 6: the UID of an object in one of the named object tables
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The three General_Reference_Type codes.
data GeneralKind
  = AnyByteTableRow  -- ^ code 7: the row number of a row of some byte table
  | AnyObjectUID     -- ^ code 8: the UID of some object
  | AnyTableUID      -- ^ code 9: the UID of some table
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | @table_kind@ of a General_Reference_Table_Type (1 and 2 in the Format column).
data TableKind = ObjectTable | ByteTable
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One Format, one constructor per Format code.
data TypeDef
  = BaseType                                     -- ^ 0: integer, uinteger, bytes, max_bytes
  | SimpleType !TypeRef !Operand                 -- ^ 1: a Base_Type and the size of this instance
  | EnumType ![(Operand, Operand)]               -- ^ 2: one or more inclusive ranges
  | AltType ![TypeRef]                           -- ^ 3: @typeOr@ of two or more types
  | ListType !Operand !TypeRef                   -- ^ 4: the maximum length and the element type
  | RestrictedRef !RestrictedKind ![TableRef]    -- ^ 5, 6: a row or an object in one of the tables
  | GeneralRef !GeneralKind                      -- ^ 7, 8, 9
  | GeneralRefTable !TableKind                   -- ^ 10
  | NamedValueName !Text !TypeRef                -- ^ 11: a @max_bytes_32@ name and the value's type
  | NamedValueInteger !Int !TypeRef              -- ^ 12 (@Name_Value_Integer_Type@ in the ABNF)
  | NamedValueUinteger !Int !TypeRef             -- ^ 13 (@Name_Value_Uinteger_Type@ in the ABNF)
  | StructType ![TypeRef]                        -- ^ 14: one or more component types
  | SetType ![(Operand, Operand)]                -- ^ 15: one or more inclusive ranges
  deriving (Eq, Show)

-- | The Format code the ABNF of Core 5.1.1 assigns.
formatCode :: TypeDef -> Int
formatCode = \case
  BaseType                     -> 0
  SimpleType {}                -> 1
  EnumType {}                  -> 2
  AltType {}                   -> 3
  ListType {}                  -> 4
  RestrictedRef ByteTableRow _ -> 5
  RestrictedRef ObjectTableUID _ -> 6
  GeneralRef AnyByteTableRow   -> 7
  GeneralRef AnyObjectUID      -> 8
  GeneralRef AnyTableUID       -> 9
  GeneralRefTable {}           -> 10
  NamedValueName {}            -> 11
  NamedValueInteger {}         -> 12
  NamedValueUinteger {}        -> 13
  StructType {}                -> 14
  SetType {}                   -> 15

-- | The Format code's name as the Type tables spell it, suffix included:
--   @Restricted_Reference_Type{6}@, @General_Reference_Type{8}@.
formatName :: TypeDef -> Text
formatName t = case t of
  BaseType             -> "Base_Type"
  SimpleType {}        -> "Simple_Type"
  EnumType {}          -> "Enumeration_Type"
  AltType {}           -> "Alternative_Type"
  ListType {}          -> "List_Type"
  RestrictedRef {}     -> "Restricted_Reference_Type" <> braced
  GeneralRef {}        -> "General_Reference_Type" <> braced
  GeneralRefTable {}   -> "General_Reference_Table_Type"
  NamedValueName {}    -> "Named_Value_Name_Type"
  NamedValueInteger {} -> "Name_Value_Integer_Type"
  NamedValueUinteger {} -> "Name_Value_Uinteger_Type"
  StructType {}        -> "Struct_Type"
  SetType {}           -> "Set_Type"
  where braced = "{" <> tshow (formatCode t) <> "}"

-- | The Type objects a definition refers to, in Format order.
references :: TypeDef -> [TypeRef]
references = \case
  SimpleType r _         -> [r]
  AltType rs             -> rs
  ListType _ r           -> [r]
  NamedValueName _ r     -> [r]
  NamedValueInteger _ r  -> [r]
  NamedValueUinteger _ r -> [r]
  StructType rs          -> rs
  _                      -> []


-- | A generated enumeration type: the Type object it came from and the inclusive
--   value range of that object's Format. Neither method mentions @e@, so both
--   are used with a type application: @typeUID \@Core_adv_key_mode@.
--
--   The generator gives every value of the range a constructor, filling the
--   gaps with @_NA@ constructors, so that 'fromEnum' plus the lower bound is
--   the value the spec assigns ('enumValue').
class Enumerated e where
  typeUID :: UID
  range   :: (Int, Int)

-- | The value the spec assigns to a constructor.
enumValue :: forall e. (Enumerated e, Enum e) => e -> Int
enumValue = (+ fst (range @e)) . fromEnum

-- | The constructor for a value, if it lies in the range.
enumFromValue :: forall e. (Enumerated e, Enum e, Bounded e) => Int -> Maybe e
enumFromValue v
  | lo <= v && v <= hi && i <= fromEnum (maxBound @e) = Just (toEnum i)
  | otherwise                                        = Nothing
  where
    (lo, hi) = range @e
    i        = v - lo
