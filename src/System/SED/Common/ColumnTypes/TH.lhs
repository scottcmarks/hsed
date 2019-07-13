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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DerivingVia       #-}

module System.SED.Common.ColumnTypes.TH where


import           Data.Attoparsec.ByteString       (Parser, endOfInput, inClass,
                                                   many1, parseOnly, skipWhile,
                                                   string, take, takeTill, takeWhile,
                                                   (<?>))
import           Data.Attoparsec.ByteString.Char8 (decimal, char8, endOfLine,
                                                   isDigit_w8, isEndOfLine,
                                                   isHorizontalSpace,
                                                   skipSpace)
import           Data.Attoparsec.Combinator       (option)
import           Data.ByteString                  (ByteString, append, empty, init,
                                                   last, length)
import           Data.ByteString.Char8            (unpack)
import           Data.Either                      (either)
import           Data.Foldable                    (foldr)
import           Data.Functor                     ((<$>))
import           Data.String                      (String)

import           GHC.Base                         (Eq, Int, Monoid(..), Semigroup(..),
                                                   error, id, pure, mapM, many,
                                                   undefined,
                                                   ($), (*>), (<*), (<*>), (==))
import           GHC.Enum                         (enumFromTo)
import           GHC.List                         (tail, (++))
import           GHC.Show                         (show, Show)

import           Language.Haskell.TH.Quote        (QuasiQuoter(..), quoteType,
                                                   quoteDec, quotePat, quoteExp)
import           Language.Haskell.TH.Syntax       (Dec, mkName, returnQ)


import           Extras.Integral                  (ordw)

import           System.SED.Common.THUtil         (eUID, dVal, dSig, parseTable)
import           System.SED.Common.UID            (UID)
import           System.SED.Common.Util           (trimTrailingWhitespace, hexUID)


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
    pieceLengths <- skipSpace *> typeTableTitle *> rowSep
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
  where typeName = mkName $ mconcat ["u", unpack n, "Type"]
        typeUID = hexUID u


typeTableTitle :: Parser ByteString
typeTableTitle = (append <$> string "Table " <*> takeTill isEndOfLine) <* endOfLine
  <?> "Table typeTableTitle"

spaces :: Parser ByteString
spaces = takeWhile isHorizontalSpace

rowSepFieldLengths :: Parser [Int]
rowSepFieldLengths =
      (char8 '+' <?> "initial")
   *> many (length <$> takeWhile (== ordw '-')
                   <* (char8 '+' <?> "trailing"))
  <?> "row separator fields"

rowSep :: Parser [Int]
rowSep = ( (:) <$> (length <$> spaces) <*> rowSepFieldLengths ) <* endOfLine
  <?> "row separator"

blankLines :: Parser ()
blankLines = many (spaces *> endOfLine) *> pure ()
  <?> "blank lines"

tableRowFields :: [Int] -> Parser [ByteString]
tableRowFields lengths = tail <$> parseLine
    where
      parseLine = (mapM takeField lengths <* endOfLine) <?> "Table row fields"
      takeField len = trimTrailingWhitespace <$> take len <* char8 '|'

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
formatString (TypeTableRow "List_Type" _maxLength _elementTYpe) =
    mconcat [ "L" -- FIXME
            ]
formatString t = error $ mconcat [ "No case for ", show t, "?" ]


data TypeTableRowDecs =  TypeTableRowDecs [Dec]
  deriving(Eq, Show)

instance Semigroup TypeTableRowDecs
  where (TypeTableRowDecs d1) <> (TypeTableRowDecs d2) =
            TypeTableRowDecs (d1<>d2)

instance Monoid TypeTableRowDecs
  where mempty = TypeTableRowDecs []






-- | QuasiQuoter for Type Table enum tables.
--
--   Some enumerations are explicated by tables following the Type Tables that refer to them.
tenum :: QuasiQuoter
tenum = QuasiQuoter
    { quoteExp = undefined
    , quotePat = undefined
    , quoteDec = returnQ <$> tenumDecs
    , quoteType = undefined
    }


tenumDecs :: String -> [Dec]
tenumDecs s = ds
  where
    EnumDecs ds =
        dEnum $ parseTable enumTableParser s

dEnum :: (ByteString, [EnumRow]) -> EnumDecs
dEnum = undefined
-- dEnum (n, enumValueLabelPairs) =
--     EnumDecs [dSig enumName ''UID, dVal enumName $ eUID typeUID]
--   where enumName = mkName $ mconcat ["u", unpack n, "Type"]
--         typeUID = hexUID u


enumTableParser :: Parser (ByteString, [EnumRow])
enumTableParser = do
    enumType <- skipSpace *> enumTableTitle
    pieceLengths <- rowSep
    _            <- header pieceLengths *> rowSep
    rows         <- many1 (enumTableRow pieceLengths <* rowSep) -- <-- the data
    ()           <- blankLines *> endOfInput
    pure (enumType, rows)


data EnumRow = EnumRow ByteString [Int]
    deriving (Show)

enumTableTitle :: Parser ByteString
enumTableTitle = "Table " *> skipWhile isDigit_w8 *> " "
                          *> takeWhile idChar <*
                             takeTill isEndOfLine <* endOfLine
             <?> "Enum Table Title"
  where idChar = inClass "a-zA-Z0-9_"

enumTableRow :: [Int] -> Parser EnumRow
enumTableRow lengths = do
    [sv,n] <- tableRowFields lengths
    let ev = either error id  $ parseOnly pRange sv
    pure $ EnumRow n ev
  where pRange = do
            v1 <- dInt
            option [v1] (enumFromTo v1 <$> ("-" *> dInt))
        dInt = decimal :: Parser Int

newtype EnumDecs = EnumDecs [Dec]
  deriving(Eq, Show, Semigroup, Monoid) via [Dec]

\end{code}
\end{document}
