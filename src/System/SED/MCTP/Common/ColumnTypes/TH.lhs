\documentstyle{article}
\begin{document}
\chapter{ColumnTypes Template Haskell}

Table column types.


\begin{code}
{-|
Module      : System.SED.MCTP.Common.ColumnTypes.TH
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

module System.SED.MCTP.Common.ColumnTypes.TH where


import           Data.Attoparsec.ByteString       (Parser, endOfInput, inClass,
                                                   many1, parseOnly, skipWhile,
                                                   string, take, takeTill, takeWhile,
                                                   (<?>))
import           Data.Attoparsec.ByteString.Char8 (decimal, char8, endOfLine,
                                                   isDigit_w8, isEndOfLine,
                                                   isHorizontalSpace,
                                                   skipSpace)
import           Data.Attoparsec.Combinator       (option)
import           Data.ByteString                  (ByteString, append, empty, filter,
                                                   init, intercalate, last, length,
                                                   splitWith)
import           Data.ByteString.Char8            (unpack)
import           Data.Either                      (either)
import           Data.Foldable                    (concatMap, foldr, maximum)
import           Data.Functor                     ((<$>))
import           Data.List                        (sortOn, (\\), transpose)
import           Data.Map                         (fromListWith, toList)
import           Data.String                      (String)
import           Data.Tuple                       (snd, fst)

import           GHC.Arr(range)
import           GHC.Base                         (flip, Monoid(..), Semigroup(..),
                                                   error, id, pure, map, mapM, many,
                                                   undefined,
                                                   (.), ($), (*>), (<*), (<*>), (||))
import           GHC.Classes                      (Eq(..), Ord(..))
import           GHC.Enum                         (Bounded(..), Enum(..), enumFromTo)
import           GHC.List                         (tail, (++))
import           GHC.Show                         (show, Show(..))
import           GHC.Types                        (Int)

import           Language.Haskell.TH.Quote        (QuasiQuoter(..), quoteType,
                                                   quoteDec, quotePat, quoteExp)
import           Language.Haskell.TH.Syntax       (Dec, mkName, returnQ)


import           Extras.Integral                  (ordw)

import           System.SED.MCTP.Common.THUtil         (dData, dSig, dVal, eUID, parseTable)
import           System.SED.MCTP.Common.UID            (UID)
import           System.SED.MCTP.Common.Util           (trimTrailingWhitespace, hexUID)


\end{code}

5.1.3 Column Types

This section describes each of the column types in the Template Reference sections of the Core
Specification. The UID, Name, and Format columns identify the column values of the Type table. These
values SHALL comprise the Type table for every SP, prior to any personalization. These types SHALL
NOT be able to be changed or deleted by the host.

Included in this section are descriptions of the column types for each column of each table defined in
Component types are types that have entries in the Type table, but are not referenced directly as
column types. They are used to make up other types that do represent column types.

The UID column in the description table in each section SHALL be the UID for that type.

The Name column specifies the name for that type.

The Format column identifies the structure of the associated type. The first value in the Format column
is the name of that type's Format code. Additional values listed in the column are determined by the
type's format code. For readability, the names of Type objects are used in place of their UID, and
commas are used to separate values.

An asterisk (*) in any of the descriptive tables indicates SSC-specific or implementation-specific values.


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
      parseLine = map mconcat . transpose
                    <$> many1 (mapM takeField lengths <* endOfLine)
                          <?> "Table row fields"
      takeField len = trimTrailingWhitespace <$> take len <* char8 '|'

data TypeTableRow = TypeTableRow TypeUIDField TypeName [FormatString]
    deriving (Show)

instance Semigroup TypeTableRow where
    (TypeTableRow u1 n1 f1) <> (TypeTableRow _u2 _n2 f2) =
        TypeTableRow u1 n1 (f1 <> f2)

type TypeUIDField = ByteString

type TypeName = ByteString

type FormatString = ByteString


header :: [Int] -> Parser ()
header lengths = many1 (tableRowFields lengths) *> pure ()
  <?> ("header " ++ show lengths)







formatString :: TypeTableRow -> ByteString
formatString (TypeTableRow "List_Type" _maxLength _elementTYpe) =
    mconcat [ "L" -- FIXME
            ]
formatString t = error $ mconcat [ "No case for ", show t, "?" ]


newtype TypeTableRowDecs =  TypeTableRowDecs [Dec]
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
tenumDecs = dEnum <$> parseTable enumTableParser

dEnum :: (String, [EnumRow]) -> [Dec]
dEnum (enumName, enumRows) = [dData name constructors derivations]
  where name = mkName coreName
        constructors = map (mkName . fst) enumRowsValueLabelPairs
        derivations = [ ''Bounded, ''Enum, ''Eq, ''Ord, ''Show ]
        coreName = "Core_" <> enumName
        enumRowsValueLabelPairs = concatMap genEnum consolidatedPairs
        genEnum :: (String, [Int]) -> [(String, Int)]
        genEnum (n, (v:vs)) = (n,v) : map (\v'->(mconcat[n, "_", show v'],v')) vs
        genEnum (_, []) = error "No values"
        consolidatedPairs = sortOn snd $ toList $ fromListWith (flip(++)) fullPairs
        fullPairs =
            if missingValues /= []
            then (mconcat[coreName, "_NA"]::String, missingValues) : givenPairs
            else givenPairs
        missingValues = fullRange \\ givenValues
        givenPairs = map enumRowValueLabelPairs enumRows
        enumRowValueLabelPairs :: EnumRow -> (String, [Int])
        enumRowValueLabelPairs (EnumRow n vs) = (mconcat[coreName, "_", n], vs)
        givenValues = concatMap (\(EnumRow _ vs) -> vs) enumRows
        maxv = maximum givenValues
        fullRange = range(0,maxv)


enumTableParser :: Parser (String, [EnumRow])
enumTableParser = do
    enumType <- skipSpace *> enumTableTitle
    pieceLengths <- rowSep
    _            <- header pieceLengths *> rowSep
    enumRows     <- many1 (enumTableRow pieceLengths <* rowSep) -- <-- the data
    ()           <- blankLines *> endOfInput
    pure (enumType, enumRows)

data EnumRow = EnumRow String [Int]
    deriving (Show)

enumTableTitle :: Parser String
enumTableTitle = unpack <$> ("Table " *> skipWhile isDigit_w8 *> " "
                                      *> takeWhile idChar <*
                                         takeTill isEndOfLine <* endOfLine)
             <?> "Enum Table Title"
  where idChar = inClass "a-zA-Z0-9_"


enumTableRow :: [Int] -> Parser EnumRow
enumTableRow lengths = do
    [sv,n] <- tableRowFields lengths
    let ev = either error id  $ parseOnly pRange sv
    pure $ EnumRow (scrub n) ev
  where
      pRange = do
          v1 <- dInt
          option [v1] (enumFromTo v1 <$> ("-" *> dInt))
      dInt = decimal :: Parser Int
      scrub n = unpack
          $ intercalate "_"
          $ splitWith scrubbed
          $ filter notComma n
      scrubbed c = c == 32 || c == 45
      -- ^ space or dash
      notComma c = c /= 44
      -- ^ comma

\end{code}
\end{document}
