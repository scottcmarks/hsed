{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Spec.Readers.AccessControl
Description : The AccessControl and ACE preconfiguration reader
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The reader of an SP's AccessControl and ACE preconfiguration tables (Opal
T22 and T23, PDF 39-47; at M5 the Locking SP's T38 and T39), which the
preconfig rule reads them through: each AccessControl row's MethodID, ACL
and GetACLACL resolved against the SP's MethodID and ACE rows, each ACE's
BooleanExpr read from its infix print into the postfix list the runtime
evaluates (Core 5.3.4.3.2 and Table 210, PDF 226-227), each ACE's Columns
read in the table its citing AccessControl rows invoke on (Core 5.3.2.9.5,
PDF 165). A skeleton at M4b's scaffold: the reader and its codes,
@C-ACL-REFS@ and @C-ACL-EXPR@, are the preconfiguration group's (G2).
-}
module Hsed.Spec.Readers.AccessControl
  ( accessControlCodes
  ) where

import           Hsed.Spec.Common (CodeDef)


-- | The codes the AccessControl reader claims, for the catalogue
--   ("Hsed.Spec.Readers".'Hsed.Spec.Readers.readerCodes'): none yet (G2's
--   C-ACL-REFS and C-ACL-EXPR).
accessControlCodes :: [CodeDef]
accessControlCodes = []
