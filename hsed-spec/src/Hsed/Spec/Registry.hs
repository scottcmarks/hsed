{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Spec.Registry
Description : The rules hsedc implements, one line each
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Every rule is one module under @Hsed/Spec/Rules/@ exporting one
'Hsed.Spec.RuleDef.RuleDef'; this list is the only place that names them all
(docs/adding-a-rule.md). Keep the imports and the list in alphabetical order,
one line per rule: two rules added in parallel each add an import and a list
line, which git may report as a conflict where both insert at one place; keep
both lines, in order. The generic modules (classification, checks,
compilation, printing, the command line) iterate this list and know no rule by
name; @RegistrySpec@ checks that every module under @Hsed/Spec/Rules/@ is
listed here and that no two rules share a name or a shape.
-}
module Hsed.Spec.Registry
  ( registry
  ) where

import           Hsed.Spec.RuleDef          (RuleDef)
import           Hsed.Spec.Rules.EnumValues (enumValuesRule)
import           Hsed.Spec.Rules.TableUIDs  (tableUIDsRule)
import           Hsed.Spec.Rules.TypeRow    (typeRowRule)


-- | The rules, in alphabetical order.
registry :: [RuleDef]
registry =
  [ enumValuesRule
  , tableUIDsRule
  , typeRowRule
  ]
