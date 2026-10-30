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

import           Hsed.Spec.RuleDef             (RuleDef)
import           Hsed.Spec.Rules.ByteFieldValue (byteFieldValueRule)
import           Hsed.Spec.Rules.Codes         (codesRule)
import           Hsed.Spec.Rules.ConstValues   (constValuesRule)
import           Hsed.Spec.Rules.EnumValues    (enumValuesRule)
import           Hsed.Spec.Rules.FieldType     (fieldTypeRule)
import           Hsed.Spec.Rules.FieldValue    (fieldValueRule)
import           Hsed.Spec.Rules.Preconfig     (preconfigRule)
import           Hsed.Spec.Rules.Props         (propsRule)
import           Hsed.Spec.Rules.Ranges        (rangesRule)
import           Hsed.Spec.Rules.Sig           (sigRule)
import           Hsed.Spec.Rules.StateTable    (stateTableRule)
import           Hsed.Spec.Rules.StatusCodes   (statusCodesRule)
import           Hsed.Spec.Rules.TableDesc     (tableDescRule)
import           Hsed.Spec.Rules.TableUIDs     (tableUIDsRule)
import           Hsed.Spec.Rules.Terminology   (terminologyRule)
import           Hsed.Spec.Rules.TokenEncoding (tokenEncodingRule)
import           Hsed.Spec.Rules.TypeRow       (typeRowRule)
import           Hsed.Spec.Rules.UidTable      (uidTableRule)


-- | The rules, in alphabetical order.
registry :: [RuleDef]
registry =
  [ byteFieldValueRule
  , codesRule
  , constValuesRule
  , enumValuesRule
  , fieldTypeRule
  , fieldValueRule
  , preconfigRule
  , propsRule
  , rangesRule
  , sigRule
  , stateTableRule
  , statusCodesRule
  , tableDescRule
  , tableUIDsRule
  , terminologyRule
  , tokenEncodingRule
  , typeRowRule
  , uidTableRule
  ]
