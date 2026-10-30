{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Core.TypeName
Description : A type as the spec names it: its text, and the Type object's UID when a type row defines it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The spec names a type wherever a column, a record field, a property or a
method parameter has one, by the name of its Type object (Core 5.1.3: "the
names of Type objects are used in place of their UID") or, in a method
signature, by an abstract type or a grouping (@cell_block@, @list [ uidref
... ]@, Core 5.1.4). One record carries such a reference for all four
('Hsed.Core.Table.Col', 'Hsed.Core.Field.FieldDef',
'Hsed.Core.Properties.Property', 'Hsed.Core.Method.Param'; the M2b review):
the text as printed, spacing collapsed, and the UID of the Type object when
the whole text is the Name of a type row the spec compiler read
("Hsed.Gen.Core.Types"); Nothing where it is not (a column type no Type row
defines, Core 2.01's four; an abstract type or a grouping in a signature).
-}
module Hsed.Core.TypeName
  ( TypeName (..)
  ) where

import           RIO

import           Hsed.Core.UID (UID)


-- | A type as the spec names it.
data TypeName = TypeName
  { typeNameText :: !Text         -- ^ as printed, spacing collapsed: @uinteger@, @cell_block@, @list [ uidref ... ]@
  , typeNameUID  :: !(Maybe UID)  -- ^ the Type object's UID when the text is a type row's Name
  }
  deriving (Eq, Show)
