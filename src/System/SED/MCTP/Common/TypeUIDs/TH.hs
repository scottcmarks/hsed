{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies       #-}
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


import           Data.Attoparsec.ByteString       (takeTill)
import           Data.Attoparsec.ByteString.Char8       (isEndOfLine,
                                                         Parser, endOfInput,
                                                         inClass, many1,
                                                         parseOnly, skipWhile,
                                                         takeWhile, string, choice,
                                                         take, (<?>), char8, decimal,
                                                         endOfLine, isDigit,
                                                         isHorizontalSpace,
                                                         skipSpace)
import           Data.Attoparsec.Combinator             (option)
import           Data.ByteString                        (ByteString, append,
                                                         empty, filter, init,
                                                         intercalate,
                                                         length, splitWith)
import           Data.ByteString.Char8                  (unpack, last)
import           Data.Either                            (either)
import           Data.Foldable                          (concatMap, foldr,
                                                         maximum)
import           Data.Functor                           ((<$>))
import           Data.List                              (sortOn, transpose,
                                                         (\\))
import           Data.Map                               (fromListWith)
import           Data.Proxy                             (Proxy (..))
import           Data.Set                               (Set
--                                                        , member
                                                        )
import           Data.String                            (String, IsString(..))
import           Data.Tuple                             (fst, snd)

import           GHC.Arr                                (range)
import           GHC.Base                               (Monoid (..), Type,
                                                         Semigroup (..), error,
                                                         flip, id, many, map,
                                                         mapM, pure, undefined,
                                                         ($), (*>), (.), (<*),
                                                         (<*>), (||), (<|>))
import           GHC.Classes                            (Eq (..), Ord (..))
import           GHC.Enum                               (Bounded (..),
                                                         Enum (..), enumFromTo)
import GHC.Exts(IsList(..))
import           GHC.List                               (tail, (++))
import           GHC.Show                               (Show (..), showString)
import           GHC.TypeLits                           (KnownNat, Symbol)
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

import           Data.BoundedSize                       (Predicate(..), fromNat)
import           System.SED.MCTP.Common.THUtil          (dData, dSig, dVal,
                                                         eLitS, eUID,
                                                         parseTable)
import           System.SED.MCTP.Common.Token           (ordw)
import           System.SED.MCTP.Common.UID             (UID)
import           System.SED.MCTP.Common.Util            (hexUID,
                                                         trimTrailingWhitespace)
import           System.SED.MCTP.Common.Base_Type       (Core_bytes(..), Core_max_bytes(..),
                                                         Core_integer(..), Core_uinteger(..),
                                                         Implementation_uinteger)

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
  where trimComma bs = if last bs == ',' then init bs else bs

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
typeTableTitle = (append <$> "Table " <*> takeTill isEndOfLine) <* endOfLine
  <?> "Table typeTableTitle"

spaces :: Parser ByteString
spaces = takeWhile (isHorizontalSpace . ordw)

rowSepFieldLengths :: Parser [Int]
rowSepFieldLengths =
      (char8 '+' <?> "initial")
   *> many (length <$> takeWhile (== '-')
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

data Some_Core_Format =  forall n. (Show (Core_Format n),KnownNat n) => Some_Core_Format(Core_Format n)

class Is_Core_Format (a)
  where
    formatCode :: a -> Word8
instance (KnownNat n) => Is_Core_Format (Core_Format n) where
    formatCode _ =  fromNat(Proxy @n)
instance Is_Core_Format (Some_Core_Format) where
    formatCode (Some_Core_Format x) = formatCode x


instance (KnownNat n) => Show (Core_Format n) where
    show = showString "Core_Format " . show . formatCode

instance Show Some_Core_Format where
    showsPrec p (Some_Core_Format x) =
          showString "Some_Core_Format ("
        . showsPrec p x
        . showString ")"




type Type_Name = String
type Base_Type_Name = Type_Name
type Non_Base_Type_Name = Type_Name
type Named_Value_Type_Name = Type_Name
type Table_Name = String
type Byte_Table_Name = Table_Name
type Object_Table_Name = Table_Name

type role One_Of phantom _
newtype One_Of s a = One_Of a

class IsOne_Of s a where
    refiningSet :: s a -> Set a
    refiningSet = undefined

instance IsOne_Of (One_Of s) a

instance Predicate (One_Of (Set a)) a where
    predicate (One_Of _x) = undefined -- (x `member` s)
    failMsg (One_Of _x) = undefined

core_Base_Type_Names :: Set String
core_Base_Type_Names = fromList ["bytes","integer","max_bytes","uinteger"]
type Core_Base_Type_Names = Type

core_Table_Kind_Names :: Set String
core_Table_Kind_Names = fromList ["Byte", "Object"]
type Core_Table_Kind_Names = Type


data Core_Format_Spec =
    Base_Type_Spec                      ( String ? One_Of Core_Base_Type_Names)
  | Simple_Type_Spec                    ( Base_Type_Name, Core_uinteger_2 )
  | Enumeration_Type_Spec               ( [(Core_uinteger_2, Core_uinteger_2)]  ? (BoundedSize 1 Core_type_def_max_enum_ranges) )
  | Alternative_Type_Spec               ( (Set Non_Base_Type_Name)              ? (BoundedSize 2 Core_type_def_max_alternative_uidrefs))
  | List_Type_Spec                      ( Core_uinteger_2, Non_Base_Type_Name)
  | Restricted_Reference_Type_5_Spec    ( (Set Byte_Table_Name)                 ? (BoundedSize 1 Core_type_def_max_byte_table_uidrefs))
  | Restricted_Reference_Type_6_Spec    ( (Set Object_Table_Name)               ? (BoundedSize 1 Core_type_def_max_object_table_uidrefs))
  | General_Reference_Type_7_Spec
  | General_Reference_Type_8_Spec
  | General_Reference_Type_9_Spec
  | General_Reference_Table_Type_Spec   ( String ? One_Of Core_Table_Kind_Names)
  | Named_Value_Name_Type_Spec          ( Core_max_bytes_32, Non_Base_Type_Name)
  | Named_Value_Integer_Type_Spec       ( Core_integer_2,  Non_Base_Type_Name)
  | Named_Value_Uinteger_Type_Spec      ( Core_uinteger_2, Non_Base_Type_Name)
  | Struct_Type_Spec                    ( [Named_Value_Type_Name]               ? (BoundedSize 1 Core_type_def_max_struct_uidrefs))
  | Set_Type_Spec                       ( [(Core_uinteger_2, Core_uinteger_2)]  ? (BoundedSize 1 Core_type_def_max_set_ranges))




stringSetParser :: (IsList l, Item l ~ String) => l -> Parser ByteString
stringSetParser = choice . map (string . fromString) . toList


-- | Parse the string in the Format colum
formatSpecParser :: Parser Core_Format_Spec
formatSpecParser = baseTypeFormatSpecParser
               <|> simpleTypeFormatSpecParser
               <?> "invalid Format specification"

baseTypeFormatSpecParser :: Parser Core_Format_Spec
baseTypeFormatSpecParser =  undefined -- TODO -- "Base_Type" *> stringSetParser core_Base_Type_Names

simpleTypeFormatSpecParser :: Parser Core_Format_Spec
simpleTypeFormatSpecParser = undefined -- TODO -- string "Simple_Type" *> "," *> pure Simple_Type_Spec <$> undefined

{-
"Alternative_Type"
"Base_Type"
"Enumeration_Type"
"General_Reference_Table_Type"
"General_Reference_Type{7}"
"General_Reference_Type{8}"
"General_Reference_Type{9}"
"List_Type"
"Name_Value_Uinteger_Type"
"Restricted_Reference_Type{6}"
"Set_Type"
"Simple_Type"
"Struct_Type"
-}




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
enumTableTitle = unpack <$> ("Table " *> skipWhile isDigit *> " "
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


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type U1 = "abc"
type U2 = "u2"

data Core_type (uid::Symbol) a where
    Core_type_uinteger :: Core_type "00000005" (Core_uinteger_2 -> Implementation_uinteger)

type family Core_Base_type (tag::Symbol) :: Nat->Type
type instance Core_Base_type "00000002"  = Core_Base_type "bytes"
type instance Core_Base_type "bytes"     = Core_bytes
type instance Core_Base_type "00000003"  = Core_Base_type "max_bytes"
type instance Core_Base_type "max_bytes" = Core_max_bytes
type instance Core_Base_type "00000004"  = Core_Base_type "integer"
type instance Core_Base_type "integer"   = Core_integer
type instance Core_Base_type "00000005"  = Core_Base_type "uinteger"
type instance Core_Base_type "uinteger"  = Core_uinteger

type Core_Simple_Type (t::Nat->Type) (n::Nat) = t n

type family Core_Data_type (tag::Symbol) :: Type
type instance Core_Data_type "00000238"  = Core_Data_type "bytes_4"
type instance Core_Data_type "bytes_4"   = Core_Simple_Type Core_bytes 4
type Core_bytes_4 = Core_bytes 4
