{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Readers.AccessControl
Description : The AccessControl and ACE preconfiguration reader
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The reader of an SP's AccessControl and ACE preconfiguration tables (Opal
T22 and T23, PDF 39-47; at M5 the Locking SP's T38 and T39), which the
preconfig rule ("Hsed.Spec.Rules.Preconfig") reads them through. A table is
an access control table when its description has a column holding an
invoking ID (@table_or_object_ref@, Core Table 139, PDF 122:
'Hsed.Spec.Readers.Cell.KUIDRef'; Core's AccessControl table, Table 175, PDF
162), and an ACE table when it has a BooleanExpr or a Columns column
('Hsed.Spec.Readers.Cell.KExpr', 'Hsed.Spec.Readers.Cell.KColumns'; Core's
ACE table, Table 177, PDF 164-165): by the kinds, not by the SP or the
table's name.

* An access control row's InvokingID is a table's UID, an object's, or a
  table's half-UID and @TT TT TT TT@, the shorthand for the lower bytes of
  every object of that table (Opal *AC1-*AC7, PDF 38-39: 'everyObject'); its
  MethodID names a row of the SP's MethodID table, and its ACL columns (the
  ACL and the four meta-ACLs, type @ACL@, Core Table 50, PDF 101) name rows
  of its ACE table ('aclColumn').
* An ACE's BooleanExpr is read from its infix print into the postfix list
  the runtime evaluates (Core 5.3.4.3.2 and Table 210, PDF 226-227; the
  reading is 'Hsed.Spec.Readers.Cell.readCellWith''s, each operand an
  authority of the SP).
* An ACE's Columns names columns of the table a method needing the ACE is
  invoked on (Core 5.3.2.9.5, PDF 165), so of each table an access control
  row whose ACLs name the ACE invokes on, by the half-UID of its InvokingID
  ('aceHalves'; a wildcard or a template row counts by its half), a name
  being one column in all of them; @All@ is the empty set, every column, and
  needs no table. Every ACE of Opal Table 23 that lists columns applies to
  one table ('aceHalf'), the one its informative group row names; at M5 the
  Locking SP's Table 39 names some in the ACLs of two (CommonName on the
  Authority and Locking objects, PDF 78).

Codes, claimed here ('accessControlCodes') and given by the preconfig rule
('aclCode'):

* @C-ACL-REFS@ (an Error): an access control row's reference (its MethodID,
  an ACL's ACE) names no row of the SP; a BooleanExpr's operand names no
  authority of the SP; a Columns name is no column of a table the ACE
  applies to, or not one column of them all, or the ACE lists columns and
  no row names it;
* @C-ACL-EXPR@ (an Error): a BooleanExpr that does not read as an infix
  expression of authorities and boolean_ACE's operators (Core Table 61, PDF
  104), Not before And before Or, parentheses grouping.
-}
module Hsed.Spec.Readers.AccessControl
  ( -- * The tables
    isAccessControl
  , isACE
  , aclColumn
    -- * The table an ACE applies to
  , Invocation (..)
  , everyObject
  , invokedHalf
  , aceHalves
  , aceHalf
    -- * Codes
  , aclCode
  , accessControlCodes
  ) where

import qualified Data.Text              as DT
import qualified RIO.Char               as C
import qualified RIO.List               as L
import qualified RIO.Text               as T
import           RIO

import           Hsed.Spec.Common       (CodeDef (..), Reports (..), Severity (..))
import           Hsed.Spec.Readers.Cell (ColKind (..), Problem (..))
import           Hsed.Spec.Readers.Text (nameKey, readHexByte)
import           Hsed.Spec.RuleDef      (parseUIDCell)


-- * The tables

-- | Whether a table's columns are an access control table's: one holds an invoking ID.
isAccessControl :: [ColKind] -> Bool
isAccessControl = elem KUIDRef

-- | Whether a table's columns are an ACE table's: one holds an expression or column names.
isACE :: [ColKind] -> Bool
isACE = any (\case
  KExpr {}  -> True
  KColumns  -> True
  _         -> False)

-- | Whether a column of an access control table names ACEs: a list of references.
aclColumn :: ColKind -> Bool
aclColumn = \case
  KList (KRef _) -> True
  _              -> False


-- * The table an ACE applies to

-- | One access control row, as the ACE reader reads it: the half-UID of the
--   table its InvokingID invokes on, when it reads, and the names its ACL
--   columns give.
data Invocation = Invocation
  { ivHalf :: !(Maybe [Word8])
  , ivACEs :: ![Text]
  }
  deriving (Eq, Show)

-- | The half-UID of a table whose every object an InvokingID stands for:
--   @00 00 00 01 TT TT TT TT@, whitespace aside (Opal *AC1-*AC7, PDF 38-39).
everyObject :: Text -> Maybe [Word8]
everyObject t = case T.splitAt 8 (T.filter (not . C.isSpace) t) of
  (half, wild) | T.length half == 8, T.toUpper wild == "TTTTTTTT" -> traverse readHexByte (DT.chunksOf 2 half)
  _                                                               -> Nothing

-- | The half-UID of the table an InvokingID invokes on: an object's or a
--   table's UID's upper half, or the table every object of which it stands for.
invokedHalf :: Text -> Maybe [Word8]
invokedHalf t = everyObject t <|> (take 4 <$> parseUIDCell t)

-- | The half-UIDs of the tables the access control rows whose ACLs name an
--   ACE (its name compared as 'nameKey' compares) invoke on, in the rows' order.
aceHalves :: [Invocation] -> Text -> [[Word8]]
aceHalves invocations ace = L.nub [ h | Invocation (Just h) names <- invocations, nameKey ace `elem` map nameKey names ]

-- | The half-UID of the one table the access control rows naming an ACE
--   invoke on; or the tables' half-UIDs, none or several, when they are not one.
aceHalf :: [Invocation] -> Text -> Either [[Word8]] [Word8]
aceHalf invocations ace = case aceHalves invocations ace of
  [h] -> Right h
  hs  -> Left hs


-- * Codes

-- | The code of a problem reading a cell in a table of the SP's access
--   control (True: an access control table's): an expression's C-ACL-EXPR; a
--   name naming nothing in an access control table, an expression or the
--   column names of an ACE, C-ACL-REFS; Nothing for any other, whose code is
--   the reading rule's.
aclCode :: Bool -> ColKind -> Problem -> Maybe Text
aclCode accessControl kind = \case
  BadExpr _ -> Just "C-ACL-EXPR"
  Unresolved _
    | accessControl || isACE [kind] -> Just "C-ACL-REFS"
  _ -> Nothing

-- | The codes the AccessControl reader claims, for the catalogue
--   ("Hsed.Spec.Readers".'Hsed.Spec.Readers.readerCodes').
accessControlCodes :: [CodeDef]
accessControlCodes =
  [ CodeDef "C-ACL-REFS" [Error] (Defect "an AccessControl or ACE row names a method, an ACE, an authority or a column its SP does not have, or an ACE lists columns of no one table")
  , CodeDef "C-ACL-EXPR" [Error] (Defect "an ACE's BooleanExpr does not read as an expression of authorities and boolean_ACE's operators") ]
