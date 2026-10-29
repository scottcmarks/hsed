{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Core.Sync
Description : The runtime form of a state table: the fields a TPer sets in each state (Core 3.3.10)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A 'StateTable' is what the spec compiler generates from a table a
@stateTable "M.name"@ comment binds (Core Table 45, the IF-RECV ComPacket field
values of the synchronous protocol, Core 3.3.10.2): for each state the table
names, the setting of each field, either a constant the table prints or a
quantity it names in words ("Data Length"). The qualified name the comment
gives is the hand-written code the table is bound to (the synchronous
protocol's law and host loop, design docs/spec-as-source.md section 6.1 and
example E10), which turns each quantity into a computation.
-}
module Hsed.Core.Sync
  ( StateTable (..)
  , StateRow (..)
  , Setting (..)
  , settingOf
  ) where

import           RIO
import qualified RIO.List as L


-- | One state table, as its comment binds it.
data StateTable = StateTable
  { boundTo :: !Text        -- ^ the qualified name the comment binds the table to, @Sync.table45@
  , fields  :: ![Text]      -- ^ the fields the table sets, in its column order
  , states  :: ![StateRow]  -- ^ its rows, in the order printed
  }
  deriving (Eq, Show)

-- | One row: a state, and the setting of each field, in the order of 'fields'.
data StateRow = StateRow
  { state    :: !Text
  , settings :: ![Setting]
  }
  deriving (Eq, Show)

-- | What a state table gives a field.
data Setting
  = Constant !Natural  -- ^ a value the table prints (@0x01@)
  | Quantity !Text     -- ^ a quantity it names in words (@Data Length@)
  deriving (Eq, Show)

-- | The setting a row gives a field, by the field's name.
settingOf :: StateTable -> Text -> StateRow -> Maybe Setting
settingOf t field row = L.elemIndex field (fields t) >>= \i -> listToMaybe (drop i (settings row))
