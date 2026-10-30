{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Types
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Datatypes for @hsed@: hsed-core's codec and the Core tables' generated Type
objects, enumerations and UIDs, under one import, with "Hsed.Core.Types", the
runtime of the generated Type objects and enumerations (since the M2d
review): an enumeration's value on the wire is 'Hsed.Core.Types.enumValue',
never 'fromEnum', which counts from 0 and is one less for the enumerations
whose range starts at 1 (Core T126 reencrypt_state, T138 table_kind). It
re-exports neither
"Hsed.Gen.Core.Methods", whose method records share their names with RIO's
(@Set@), nor "Hsed.Core.Layout", whose field kinds share theirs with
"Hsed.Core.Token"'s: import those qualified (design docs/spec-as-source.md
sections 13.25 (k) and 13.26 (l)).

-}


module Hsed.Types
  ( module Hsed.Core.Base_Type
  , module Hsed.Core.Call
  , module Hsed.TypeUIDs
  , module Hsed.Core.Simple_Type
  , module Hsed.SpecialPurposeUIDs
  , module Hsed.Core.Reference_Types
  , module Hsed.Core.StreamItem
  , module Hsed.TableUIDs
  , module Hsed.Core.Token
  , module Hsed.Core.Types
  , module Hsed.Core.UID
  , module Hsed.Core.Value
  )
where

import           Hsed.Core.Base_Type
import           Hsed.Core.Call
import           Hsed.Core.Reference_Types
import           Hsed.Core.Simple_Type
import           Hsed.Core.StreamItem
import           Hsed.Core.Token
import           Hsed.Core.Types
import           Hsed.Core.UID
import           Hsed.Core.Value
import           Hsed.SpecialPurposeUIDs
import           Hsed.TableUIDs
import           Hsed.TypeUIDs
