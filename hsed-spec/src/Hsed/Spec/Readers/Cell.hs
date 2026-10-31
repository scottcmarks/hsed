{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Readers.Cell
Description : What a described column holds, a cell read as it, and the runtime cell it prints as
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The column reader the rules whose tables a table description types share
(@constValues@'s default values, @preconfig@'s rows; docs/adding-a-rule.md: a
helper several rules share lives under @Readers/@), moved here from
"Hsed.Spec.Rules.TableDesc" at M4b's scaffold, unchanged but for the types it
reads by: a 'TypeEnv', the Type objects and the enumeration labels of a spec
and, for a spec read over another (Opal over Core), of its base
("Hsed.Spec.Rules.TableDesc".'Hsed.Spec.Rules.TableDesc.typeEnv' builds it).

What a column holds ('ColKind') comes from its Type object's Format and, for
an enumeration, its enumeration table's labels; a column of the @uid@ type,
Core Table 145, PDF 123, is 'KUID', a uid cell, where any other @bytes@ type
is 'KBytes'. A cell is read as that kind ('readCell') and printed as the
runtime 'Hsed.Core.Table.Cell' it is ('renderCell': a 'UIDCell', a
'BytesCell' for bytes of any other type, since M4a).
-}
module Hsed.Spec.Readers.Cell
  ( -- * The types a column is read by
    TypeEnv (..)
    -- * What a column holds
  , ColKind (..)
  , colKind
  , CellValue (..)
  , readCell
  , renderCell
  , zeroValue
    -- * Codes
  , cellCodes
  ) where

import qualified Data.Char         as DC
import qualified Data.Text         as DT
import qualified Data.Text.Read    as TR
import qualified RIO.ByteString    as B
import qualified RIO.Char          as C
import qualified RIO.Text          as T
import           Numeric           (showHex)
import           RIO

import           Hsed.Core.Types   (Operand (..), RestrictedKind (..), TableRef (..), TypeDef (..), TypeRef (..),
                                    formatName)
import           Hsed.Spec.Common  (CodeDef)
import qualified Hsed.Spec.Gen     as Gen


-- * The types a column is read by

-- | The Type objects and enumeration labels a column's type is read by.
data TypeEnv = TypeEnv
  { teType   :: Text -> Maybe (Either Text TypeDef)
    -- ^ the Type object a name names: its Format's TypeDef, or why its Format
    --   did not parse; Nothing when no Type object has the name
  , teLabels :: Text -> [(Text, Int)]
    -- ^ each label of the type's enumeration table with the first value it is given
  }


-- * What a column holds

-- | What a column of a described table holds, as its Type object's Format says.
data ColKind
  = KUID                           -- ^ the @uid@ type (Core Table 145): a uid of eight bytes
  | KBytes !Int                    -- ^ a Simple_Type of @bytes@ other than @uid@: that many bytes (@bytes_32@)
  | KText !Int                     -- ^ a Simple_Type of @max_bytes@: text of at most that many bytes (@name@)
  | KUInt !Int                     -- ^ a Simple_Type of @uinteger@: an unsigned integer of that many bytes
  | KInt !Int                      -- ^ a Simple_Type of @integer@
  | KEnum !Text ![(Int, Int)] ![(Text, Int)]
    -- ^ an Enumeration_Type: its name, its ranges and, from its enumeration table, each label's first value
  | KRef ![TableRef]               -- ^ a Restricted_Reference_Type{6}: an object of one of the tables
  deriving (Eq, Show)

-- | What a column of a type holds, or why it is not read from a cell. A
--   Simple_Type's base is one of the four base types of Core 5.1.1 (@bytes@,
--   @max_bytes@, @uinteger@, @integer@), a Type object whose Format is
--   @Base_Type@.
colKind :: TypeEnv -> Text -> Either Text ColKind
colKind env name = do
  td <- typeDefOf name
  case td of
    SimpleType (Ref base) (Lit n) -> do
      btd <- maybe (Left (name <> "'s base " <> base <> " names no Type object")) Right (teType env base)
      case btd of
        Right BaseType -> case base of
          "bytes"
            | name == "uid" -> Right KUID
            | otherwise     -> Right (KBytes n)
          "max_bytes" -> Right (KText n)
          "uinteger"  -> Right (KUInt n)
          "integer"   -> Right (KInt n)
          _           -> Left (name <> "'s base " <> base <> " is not a base type of Core 5.1.1")
        _ -> Left (name <> "'s base " <> base <> " is not a Base_Type")
    EnumType ranges -> do
      rs <- maybe (Left (name <> "'s range is not literal")) Right (traverse lits ranges)
      Right (KEnum name rs (teLabels env name))
    RestrictedRef ObjectTableUID tables -> Right (KRef tables)
    other -> Left ("a column of type " <> name <> " (" <> formatName other <> ") is not read from a table cell")
  where
    typeDefOf n = case teType env n of
      Nothing        -> Left (n <> " names no Type object")
      Just (Left _)  -> Left (n <> "'s Format did not parse")
      Just (Right t) -> Right t
    lits (Lit a, Lit b) = Just (a, b)
    lits _              = Nothing

-- | A cell read as what its column holds.
data CellValue
  = CvUID ![Word8]                 -- ^ a @uid@'s eight bytes
  | CvBytes ![Word8]
  | CvText !Text
  | CvUInt !Integer !(Maybe Int)   -- ^ the value, and the hex digits it is printed with (Nothing: in decimal)
  | CvInt !Integer
  | CvEnum !Text !Int
  | CvRef !Text ![Word8]           -- ^ an object named, and its UID
  deriving (Eq, Show)

-- | Read a non-blank cell as a column of a kind holds it; an object reference
--   is resolved by the caller's function from the object's name and the tables
--   the type allows to the object's UID.
readCell :: (Text -> [TableRef] -> Either Text [Word8]) -> ColKind -> Text -> Either Text CellValue
readCell resolve kind raw = case kind of
  KUID -> case bytesOf cell of
    Just bs | length bs == 8 -> Right (CvUID bs)
    _ -> Left (tshow cell <> " is not a uid of eight bytes in hex")
  KBytes n -> case bytesOf cell of
    Just bs | length bs == n -> Right (CvBytes bs)
    _ -> Left (tshow cell <> " is not " <> tshow n <> " bytes in hex")
  KText n
    | T.null cell                    -> Left "the text is empty"
    | B.length (encodeUtf8 cell) > n -> Left (tshow cell <> " is longer than " <> tshow n <> " bytes")
    | otherwise                      -> Right (CvText cell)
  KUInt n -> do
    v <- maybe (Left (tshow cell <> " is not an unsigned integer")) Right (number cell)
    if v < 256 ^ n then Right (CvUInt v (hexDigits cell)) else Left (tshow cell <> " does not fit in " <> tshow n <> " bytes")
  KInt n -> do
    v <- maybe (Left (tshow cell <> " is not an integer")) Right (signed cell)
    let bound = 2 ^ (8 * n - 1)
    if negate bound <= v && v < bound then Right (CvInt v) else Left (tshow cell <> " does not fit in " <> tshow n <> " bytes")
  KEnum name ranges labelled -> case (lookup cell labelled, number cell) of
    (Just v, _) -> Right (CvEnum cell v)
    (Nothing, Just v)
      | any (\(lo, hi) -> toInteger lo <= v && v <= toInteger hi) ranges -> Right (CvEnum (tshow v) (fromIntegral v))
    _ -> Left (tshow cell <> " is no label of " <> name <> "'s enumeration ("
               <> T.intercalate ", " (map fst labelled) <> ")")
  KRef tables -> CvRef cell <$> resolve cell tables
  where
    cell = T.strip raw
    hexDigits t = T.length <$> T.stripPrefix "0x" (T.toLower t)
    number t = case T.stripPrefix "0x" (T.toLower t) of
      Just h  -> either (const Nothing) (\(v, rest) -> if T.null rest then Just v else Nothing) (TR.hexadecimal h)
      Nothing -> either (const Nothing) (\(v, rest) -> if T.null rest then Just v else Nothing) (TR.decimal t)
    signed t = case T.stripPrefix "-" t of
      Just rest -> negate <$> number rest
      Nothing   -> number t
    bytesOf t = let hex = T.filter (not . C.isSpace) t
                in if even (T.length hex) && T.all C.isHexDigit hex
                     then Just (map byte (DT.chunksOf 2 hex)) else Nothing
    byte = T.foldl' (\acc c -> acc * 16 + fromIntegral (DC.digitToInt c)) 0

-- | The runtime 'Hsed.Core.Table.Cell' a value prints as: bytes of a type
--   other than @uid@ a @BytesCell@, its byte string a literal (the module's
--   OverloadedStrings), however many bytes.
renderCell :: CellValue -> Text
renderCell = \case
  CvUID bs        -> "UIDCell (" <> Gen.uidExpr bs <> ")"
  CvBytes bs      -> "BytesCell " <> tshow (B.pack bs)
  CvText t        -> "TextCell " <> tshow t
  CvUInt v digits -> "UIntCell " <> maybe (tshow v) (hexLiteral v) digits
  CvInt v         -> "IntCell " <> if v < 0 then "(" <> tshow v <> ")" else tshow v
  CvEnum l v      -> "EnumCell " <> tshow l <> " " <> tshow v
  CvRef n bs      -> "RefCell " <> tshow n <> " (" <> Gen.uidExpr bs <> ")"
  where hexLiteral v w = "0x" <> T.justifyRight w '0' (T.toUpper (T.pack (showHex v "")))

-- | The value a column of a kind takes where a row gives none: its type's
--   default (design 13.31 (a); Core's stated defaults, PDF 163-171, 210, 233).
--   [S G2]
zeroValue :: ColKind -> Maybe CellValue
zeroValue _ = error "G2: zeroValue"


-- * Codes

-- | The codes the column reader claims, for the catalogue
--   ("Hsed.Spec.Readers".'Hsed.Spec.Readers.readerCodes'): none yet (G2's
--   preconfiguration reader adds the cell conventions' codes).
cellCodes :: [CodeDef]
cellCodes = []
