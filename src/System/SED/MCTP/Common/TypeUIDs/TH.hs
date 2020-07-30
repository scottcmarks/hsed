{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : System.SED.MCTP.Common.TypeUIDs.TH
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Template Haskell for parsing Table column types in Section 5.1.3.

-}


module System.SED.MCTP.Common.TypeUIDs.TH where


import           Data.Attoparsec.ByteString             (Parser, endOfInput,
                                                         inClass, many1,
                                                         parseOnly, skipWhile,
                                                         string, take, takeTill,
                                                         takeWhile, (<?>))
import           Data.Attoparsec.ByteString.Char8       (char8, decimal,
                                                         endOfLine, isDigit_w8,
                                                         isEndOfLine,
                                                         isHorizontalSpace,
                                                         skipSpace)
import           Data.Attoparsec.Combinator             (option)
import           Data.ByteString                        (ByteString, append,
                                                         empty, filter, init,
                                                         intercalate, last,
                                                         length, splitWith)
import           Data.ByteString.Char8                  (unpack)
import           Data.Either                            (either)
import           Data.Foldable                          (concatMap, foldr,
                                                         maximum)
import           Data.Functor                           ((<$>))
import           Data.List                              (sortOn, transpose,
                                                         (\\))
import           Data.Map                               (fromListWith, toList)
import           Data.Proxy                             (Proxy (..))
import           Data.Set                               (Set)
import           Data.String                            (String)
import           Data.Tuple                             (fst, snd)

import           GHC.Arr                                (range)
import           GHC.Base                               (Monoid (..), Type,
                                                         Semigroup (..), error,
                                                         flip, id, many, map,
                                                         mapM, pure, undefined,
                                                         ($), (*>), (.), (<*),
                                                         (<*>), (||))
import           GHC.Classes                            (Eq (..), Ord (..))
import           GHC.Enum                               (Bounded (..),
                                                         Enum (..), enumFromTo)
import           GHC.List                               (tail, (++))
import           GHC.Show                               (Show (..), show)
import           GHC.TypeLits                           (KnownNat)
import           GHC.Types                              (Int, Nat)
import           GHC.Word                               (Word8)
import           Language.Haskell.TH.Quote              (QuasiQuoter (..),
                                                         quoteDec, quoteExp,
                                                         quotePat, quoteType)
import           Language.Haskell.TH.Syntax             (Dec, mkName, returnQ)


import           Data.BoundedSize                       (type (?),
                                                         BoundedSize (..))
import           System.SED.MCTP.Common.Reference_Types (Byte_Table_UID,
                                                         Object_Table_UID,
                                                         Table_Kind)

import           System.SED.MCTP.Common.Simple_Type     (Core_integer_2,
                                                         Core_max_bytes_32,
                                                         Core_uinteger_2)
import           System.SED.MCTP.Common.TableUIDs       ()

import           Data.BoundedSize                       (fromNat)
import           System.SED.MCTP.Common.THUtil          (dData, dSig, dVal,
                                                         eLitS, eUID,
                                                         parseTable)
import           System.SED.MCTP.Common.Token           (ordw)
import           System.SED.MCTP.Common.UID             (UID)
import           System.SED.MCTP.Common.Util            (hexUID,
                                                         trimTrailingWhitespace)

{-

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


-}

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
ttypeDecs s = dTypeTableRow $ parseTable typeTableParser s


typeTableParser :: Parser TypeTableRow
typeTableParser = do
    pieceLengths <- skipSpace *> typeTableTitle *> rowSep
    rows         <- header pieceLengths *> rowSep *> many1 (typeTableRow pieceLengths) -- <-- the data
    ()           <- rowSep *> blankLines *> endOfInput
    pure $ foldr (<>) (TypeTableRow empty empty "") rows

typeTableRow :: [Int] -> Parser TypeTableRow
typeTableRow lengths =
    do
        [uidField, typeName, format] <- tableRowFields lengths
        pure $ TypeTableRow uidField typeName (trimComma format)
  where trimComma bs = if last bs == ordw ',' then init bs else bs

dTypeTableRow :: TypeTableRow -> [Dec]
dTypeTableRow (TypeTableRow u n fs) = [ dSig uidName ''UID
                                      , dVal uidName $ eUID typeUID
                                      , dSig formatName ''String
                                      , dVal formatName $ eLitS (unpack fs)
                                      ]
  where typeTag = unpack n <> "Type"
        uidName = mkName $ "u" <> typeTag
        typeUID = hexUID u
        formatName = mkName $ "f" <> typeTag


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

data TypeTableRow = TypeTableRow TypeUIDField TypeName FormatString
    deriving (Show)

instance Semigroup TypeTableRow where
    (TypeTableRow u1 n1 f1) <> (TypeTableRow "" "" f2) =
        TypeTableRow u1 n1 (f1 <> f2)
    _ <> _ =
        undefined

type TypeUIDField = ByteString

type TypeName = ByteString

type FormatString = ByteString


header :: [Int] -> Parser ()
header lengths = many1 (tableRowFields lengths) *> pure ()
  <?> ("header " ++ show lengths)


-- TODO Properties
type Type_UID = UID
type Base_Type_UID = Type_UID
type Non_Base_Type_UID = Type_UID
type Named_Value_Type_UID = Type_UID


type Core_type_def_max_enum_ranges          = 16
type Core_type_def_max_alternative_uidrefs  = 16
type Core_type_def_max_byte_table_uidrefs   = 16
type Core_type_def_max_object_table_uidrefs = 16
type Core_type_def_max_struct_uidrefs       = 16
type Core_type_def_max_set_ranges           = 16

data Core_Format (n::Nat) :: Type
    where
        Base_Type_Format                    ::                                                                                                                                           Core_Format  0
        Simple_Type_Format                  ::  Base_Type_UID -> Core_uinteger_2                                                                                                      -> Core_Format  1
        Enumeration_Type_Format             ::                                        ([(Core_uinteger_2, Core_uinteger_2)] ? (BoundedSize 1 Core_type_def_max_enum_ranges))          -> Core_Format  2
        Alternative_Type_Format             ::                                        ((Set Non_Base_Type_UID)              ? (BoundedSize 2 Core_type_def_max_alternative_uidrefs))  -> Core_Format  3
        List_Type_Format                    :: Core_uinteger_2   -> Non_Base_Type_UID                                                                                                 -> Core_Format  4
        Restricted_Reference_Type_5_Format  ::                                        ((Set Byte_Table_UID)                 ? (BoundedSize 1 Core_type_def_max_byte_table_uidrefs))   -> Core_Format  5
        Restricted_Reference_Type_6_Format  ::                                        ((Set Object_Table_UID)               ? (BoundedSize 1 Core_type_def_max_object_table_uidrefs)) -> Core_Format  6
        General_Reference_Type_7_Format     ::                                                                                                                                           Core_Format  7
        General_Reference_Type_8_Format     ::                                                                                                                                           Core_Format  8
        General_Reference_Type_9_Format     ::                                                                                                                                           Core_Format  9
        General_Reference_Table_Type_Format :: Table_Kind                                                                                                                             -> Core_Format 10
        Named_Value_Name_Type_Format        :: Core_max_bytes_32 -> Non_Base_Type_UID                                                                                                 -> Core_Format 11
        Named_Value_Integer_Type_Format     :: Core_integer_2    -> Non_Base_Type_UID                                                                                                 -> Core_Format 12
        Named_Value_Uinteger_Type_Format    :: Core_uinteger_2   -> Non_Base_Type_UID                                                                                                 -> Core_Format 13
        Struct_Type_Format                  ::                                        ([Named_Value_Type_UID]               ? (BoundedSize 1 Core_type_def_max_struct_uidrefs))       -> Core_Format 14
        Set_Type_Format                     ::                                        ([(Core_uinteger_2, Core_uinteger_2)] ? (BoundedSize 1 Core_type_def_max_set_ranges))           -> Core_Format 15


data Some_Core_Format =  forall n. KnownNat n => Some_Core_Format(Core_Format n)

class Is_Core_Format (a)
  where
    formatCode :: a -> Word8
instance (KnownNat n) => Is_Core_Format (Core_Format n) where
    formatCode _ =  fromNat(Proxy @n)
instance Is_Core_Format (Some_Core_Format) where
    formatCode (Some_Core_Format x) = formatCode x

-- | Parse the string in the Format colum
formatParser :: Parser (Proxy Some_Core_Format)
formatParser = undefined






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
        enumRowsValueLabelPairs = sortOn snd $ concatMap genEnum consolidatedPairs
        genEnum :: (String, [Int]) -> [(String, Int)]
        genEnum (n, (v:vs)) = (n,v) : map (\v'->(mconcat[n, "_", show v'],v')) vs
        genEnum (_, []) = error "No values"
        consolidatedPairs = toList $ fromListWith (flip(++)) fullPairs -- TODO -- complain on duplication instead of just papering over
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
