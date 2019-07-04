\documentstyle{article}
\begin{document}
\chapter{ColumnTypes Template Haskell}

Table column types.


\begin{code}
{-|
Module      : System.SED.Common.ColumnTypes.TH
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Template Haskell for parsing Table column types in Section 5.1.3.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
             PolyKinds, KindSignatures #-}
{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples,
             MultiParamTypeClasses, RoleAnnotations, CPP, TypeOperators,
             PolyKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-unused-imports #-} -- FIXME

module System.SED.Common.ColumnTypes.TH where

-- import           Data.Map
-- import           GHC
-- import           GHC.Base
-- import           GHC.Tuple
-- import           Language.Haskell.TH
-- import           Language.Haskell.TH.Syntax
-- import           RIO

-- import           Extras.Bytes
-- import           Extras.GitVersion                (gitVersion)
-- import           Extras.Hex
-- import           Extras.Integral
-- import qualified Paths_hsed


import           Data.Attoparsec.ByteString       hiding (takeWhile)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  hiding (foldr, putStrLn, tail,
                                                          take, takeWhile, zip)
import           Data.ByteString.Char8       as C (unpack)
import           Data.Either                      (either, Either, fromRight)
import           Data.Foldable                    (foldr)
import           Data.Functor                     ((<$>))
import qualified Data.List.NonEmpty               as NE (fromList)
import           Data.Map.Internal                (Map (..), fromList)
import           Data.String                      (IsString (..))
import           Data.Version
import           GHC                              ()
import           GHC.Base                         (Monoid(..), Eq, (.), id, mconcat, Int, Semigroup (..), String,
                                                   error, liftA2, many, mapM,
                                                   pure, undefined, ($), (*>),
                                                   (++), (<*), (<*>), (==))
import           GHC.List                         (tail, zip, (!!))
import           GHC.Show                         (Show (..))
import           GHC.Tuple                        ()
import           Language.Haskell.TH
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.PprLib       hiding (char, (<>), empty)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.PrettyPrint                 hiding (char, (<>), empty)

import           Extras.Bytes                     hiding (take)
-- import           Extras.GitVersion                (gitVersion)
import           Extras.Hex
import           Extras.Integral                  hiding (char)

import           System.IO                        hiding (char8)
import           System.SED.Common.THUtil
import           System.SED.Common.UID
import           System.SED.Common.Util           (hexUID,
                                                   trimTrailingWhitespace)


import           System.SED.Common.UID


\end{code}

5.1.3 Column Types

This section describes each of the column types in the Template Reference sections of the Core
Specification. The UID, Name, and Format columns identify the column values of the Type table. These
values SHALL comprise the Type table for every SP, prior to any personalization. These types SHALL
NOT be able to be changed or deleted by the host.

Included in this section are descriptions of the column types for each column of each table defined in
this specification, as well as descriptions of each of the component types of the column types.
Component types are types that have entries in the Type table, but are not referenced directly as
column types. They are used to make up other types that do represent column types.

The UID column in the description table in each section SHALL be the UID for that type.

The Name column specifies the name for that type.

The Format column identifies the structure of the associated type. The first value in the Format column
is the name of that type's Format code. Additional values listed in the column are determined by the
type's format code. For readability, the names of Type objects are used in place of their UID, and
commas are used to separate values.

An asterisk (*) in any of the descriptive tables indicates SSC-specific or implementation-specific values.

+-------------------------------------------------+
|               Table 46 AC_element               |
+-----------------------+----------+--------------+
|UID                    |Name      |Format        |
+-----------------------+----------+--------------+
|00 00 00 05 00 00 08 01|AC_element|List_Type,    |
|                       |          |*,            |
|                       |          |ACE_expression|
+-----------------------+----------+--------------+



\begin{code}

-- | QuasiQuoter for Type Table row tables.
--
--   Each row of the Type Table is in its own table in the Core Spec.
ttype :: QuasiQuoter
ttype = QuasiQuoter
    { quoteExp = undefined
    , quotePat = undefined
    , quoteDec = returnQ <$> ttypeDecs
    , quoteType = undefined
    }

ttypeDecs :: String -> [Dec]
ttypeDecs s = ds
  where
    TypeTableRowDecs ds =
        dTypeTableRow $ parseTable typeTableParser s


typeTableParser :: Parser TypeTableRow
typeTableParser = do
    pieceLengths <- skipSpace *> title *> rowSep
    rows         <- header pieceLengths *> rowSep *> many1 (typeTableRow pieceLengths) -- <-- the data
    ()           <- rowSep *> blankLines *> endOfInput
    pure $ foldr (<>) (TypeTableRow empty empty []) rows

typeTableRow :: [Int] -> Parser TypeTableRow
typeTableRow lengths =
    do
        [uidField, typeName, format] <- tableRowFields lengths
        pure $ TypeTableRow uidField typeName [trimComma format]
  where trimComma bs = if last bs == ordw ',' then init bs else bs

dTypeTableRow :: TypeTableRow -> TypeTableRowDecs
dTypeTableRow (TypeTableRow u n _fs) =
    TypeTableRowDecs [dSig typeName ''UID, dVal typeName $ eUID typeUID]
  where typeName = mkName $ mconcat ["u", C.unpack n, "Type"]
        typeUID = hexUID u


title :: Parser ByteString
title = string "Table 50 ACL" <* endOfLine
  <?> "Table title"

spaces :: Parser ByteString
spaces = takeWhile (== ' ')

rowSepFieldLengths :: Parser [Int]
rowSepFieldLengths =
      char8 '+' *> many (length <$> takeWhile (== '-') <*  char8 '+' )
  <?> "row separator fields"

rowSep :: Parser [Int]
rowSep = (:) <$> (length <$> spaces) <*> rowSepFieldLengths <* endOfLine
  <?> "row separator"

blankLines :: Parser ()
blankLines = many (spaces *> endOfLine) *> pure ()
  <?> "blank lines"

tableRowFields :: [Int] -> Parser [ByteString]
tableRowFields lengths = tail <$> parseLine
    where
      parseLine = (mapM takeField lengths <* endOfLine) <?> "Type Table row fields"
      takeField len = trimTrailingWhitespace <$> take len <* char '|'

data TypeTableRow = TypeTableRow TypeUIDField TypeName [FormatString]
    deriving (Show)

instance Semigroup TypeTableRow where
    (TypeTableRow u1 n1 f1) <> (TypeTableRow _u2 _n2 f2) =
        (TypeTableRow u1 n1 (f1 <> f2))

type TypeUIDField = ByteString

type TypeName = ByteString

type FormatString = ByteString


header :: [Int] -> Parser ()
header lengths = tableRowFields lengths *> pure ()
  <?> ("header " ++ show lengths)


formatString :: TypeTableRow -> ByteString
formatString (TypeTableRow "List_Type" _maxLength _elementTYpe) = mconcat [ "L" -- FIXME
                                                                          ]
formatString t = error $ mconcat [ "No case for ", show t, "?" ]


data TypeTableRowDecs =  TypeTableRowDecs [Dec]
  deriving(Eq, Show)


instance Semigroup TypeTableRowDecs
  where (TypeTableRowDecs d1) <> (TypeTableRowDecs d2) =
            TypeTableRowDecs (d1<>d2)

instance Monoid TypeTableRowDecs
  where mempty = TypeTableRowDecs []


\end{code}
\end{document}
