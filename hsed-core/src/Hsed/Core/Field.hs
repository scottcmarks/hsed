{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Core.Field
Description : The runtime form of a record table: each field's constant, or each field's type
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Two kinds of table describe a record field by field (design
docs/spec-as-source.md section 3.4):

* a @FIELD | VALUE@ table (the @fieldValue@ rule; Core 2.01 Tables 27, 29
  and 30, the command blocks of GET_COMID, HANDLE_COMID_REQUEST and
  GET_COMID_RESPONSE) gives each field a constant: bytes printed in hex, or a
  name for a value the text defines ('FieldConst');
* a @Field | Type@ or @Value | Type | Range@ table (the @fieldType@ rule;
  Tables 22, 23 and 34) gives each field a Type object of the spec
  ("Hsed.Core.TypeName": its name and its UID), and may narrow its values
  ('FieldDef').

The generated modules (@Hsed.Gen.\<Spec\>.Fields@) hold each table as a list of
its rows.
-}
module Hsed.Core.Field
  ( -- * Constants
    FieldValue (..)
  , FieldConst (..)
    -- * Types
  , FieldDef (..)
    -- * Lookups
  , constNamed
  , defNamed
  ) where

import           RIO
import qualified RIO.List                   as L

import           Hsed.Core.Code             (Span)
import           Hsed.Core.TypeName         (TypeName)


-- $setup
-- >>> :set -XOverloadedStrings

-- | What a row of a @FIELD | VALUE@ table gives its field.
data FieldValue
  = Constant ![Word8]  -- ^ bytes printed in hex: @02@, @00 01@, @0x0004@
  | Symbolic !Text     -- ^ a name for a value the text defines: @IF-RECV@, @Allocated ComID@
  deriving (Eq, Show)

-- | One row of a @FIELD | VALUE@ table: a field and its constant.
data FieldConst = FieldConst
  { fcName  :: !Text
  , fcValue :: !FieldValue
  }
  deriving (Eq, Show)

-- | One row of a @Field | Type@ or @Value | Type | Range@ table: a field, its
--   Type object by name and UID, and the values its Range cell allows.
data FieldDef = FieldDef
  { fdName  :: !Text
  , fdType  :: !TypeName
  , fdRange :: !(Maybe Span)
  }
  deriving (Eq, Show)

-- | A field's constant, by the field's name.
--
-- >>> constNamed [FieldConst "Command" (Symbolic "IF-RECV"), FieldConst "Protocol ID" (Constant [0x02])] "Protocol ID"
-- Just (Constant [2])
constNamed :: [FieldConst] -> Text -> Maybe FieldValue
constNamed rows name = fcValue <$> L.find ((== name) . fcName) rows

-- | A field's definition, by the field's name.
defNamed :: [FieldDef] -> Text -> Maybe FieldDef
defNamed rows name = L.find ((== name) . fdName) rows
