{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Core.Table
Description : The runtime form of a table description and of the rows the spec gives a table
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What the spec compiler generates from Core's table descriptions (Core 5.3-5.8,
@\<T\> Table Description@) and from the tables of values that fill those
tables' rows (a credential's default values, the default authorities of a
template), in the form the simulator reads at run time (design
docs/spec-as-source.md section 4.1).

A 'TableDesc' is one table: its name and its two UIDs from Table 240 and its
columns. A 'Col' is one row of the description: column number, name,
whether it takes part in the table's unique value (Core 3.2.5.4) and its
type, the Type object the description names, with that object's UID when a
Type table row defines it (Core 2.01 names four column types no Type row
defines; the spec compiler reports them).

A 'Row' is the values a table of values gives some columns of one row, by
column number, each read as its column's type says ('Cell'); a column it does
not give keeps the column's default. A preconfigured row is a 'Pre': the row
and, where the SSC's legend says so, the access each of its cells has
('Access'); Core gives no access.
-}
module Hsed.Core.Table
  ( -- * Table descriptions
    ColNum
  , TableDesc (..)
  , Col (..)
  , columnNumbered
  , columnNamed
    -- * Rows
  , Cell (..)
  , Row
  , Pre (..)
  , Access (..)
  ) where

import           RIO
import qualified RIO.List                   as L

import           Hsed.Core.UID              (UID)


-- * Table descriptions

-- | A column number, @0x00@ first.
type ColNum = Int

-- | One table as its description gives it.
data TableDesc = TableDesc
  { tdName      :: !Text    -- ^ the table's name in Table 240: @Authority@
  , tdTableUID  :: !UID     -- ^ the UID of the table itself (Table 240's @UID of Table@)
  , tdObjectUID :: !UID     -- ^ the UID of its descriptor object, its row in the Table table
  , tdColumns   :: ![Col]   -- ^ in column-number order, @0x00@ first
  }
  deriving (Eq, Show)

-- | One column of a table description.
data Col = Col
  { colNumber  :: !ColNum
  , colName    :: !Text
  , colUnique  :: !Bool          -- ^ @IsUnique@: part of the table's unique value
  , colType    :: !Text          -- ^ the Column Type as the description prints it: a Type object's name
  , colTypeUID :: !(Maybe UID)   -- ^ that Type object's UID; Nothing when no Type table row defines it
  }
  deriving (Eq, Show)

-- | The column of a description with a number.
columnNumbered :: ColNum -> TableDesc -> Maybe Col
columnNumbered n = L.find ((== n) . colNumber) . tdColumns

-- | The column of a description with a name.
columnNamed :: Text -> TableDesc -> Maybe Col
columnNamed n = L.find ((== n) . colName) . tdColumns


-- * Rows

-- | One cell's value, read as its column's type says.
data Cell
  = UIDCell !UID          -- ^ a @uid@
  | TextCell !Text        -- ^ a @max_bytes@ type, such as @name@, printed as text
  | EnumCell !Text !Int   -- ^ an enumeration's label and the value the spec assigns it
  | RefCell !Text !UID    -- ^ a reference to an object, printed as the object's name: the name and the object's UID
  | UIntCell !Integer     -- ^ a @uinteger@ type
  | IntCell !Integer      -- ^ an @integer@ type
  deriving (Eq, Show)

-- | The values a table of values gives some columns of one row, by column number, in order.
type Row = [(ColNum, Cell)]

-- | A preconfigured row and the access its cells have, by column number.
data Pre r = Pre
  { row    :: !r
  , access :: ![(ColNum, Access)]
  }
  deriving (Eq, Show)

-- | The access a preconfigured cell has, in an SSC's legend (design example E11).
data Access
  = ReadOnlySpecified     -- ^ read-only, its value specified
  | ReadOnlyVU            -- ^ read-only, its value vendor unique
  | WritePersonalizable   -- ^ writable, personalizable
  | WriteFixed            -- ^ writable, its value fixed at manufacture
  | NotDefined            -- ^ the column is not defined for this SSC
  | Unspecified           -- ^ the legend says nothing
  deriving (Eq, Ord, Show, Enum, Bounded)
