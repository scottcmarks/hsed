{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Format
Description : The Format column of a Type table row, parsed to a TypeDef
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Format cell of a Core Type table (Core 5.1.3: "The first value in the Format
column is the name of that type's Format code. Additional values listed in the
column are determined by the type's format code. For readability, the names of
Type objects are used in place of their UID, and commas are used to separate
values.") parsed against the ABNF of Core 5.1.1 into a 'TypeDef'.

The tokenizer accepts the spacing the printed tables use (@uidref {X}@ and
@uidref{X}@, @General_Reference_Type {7}@, @Simple_Type,uinteger, 2@), the suffix
forms @{5}@..@{9}@ after a Format code's name, decimal literals and the @*@
placeholder; 'formatNormalisations' lists the spacing and spelling it reads past,
which hsedc reports (@N-FORMAT-NORMALISED@, design example E3: "normalised and
reported"). Names are resolved against an 'Env' of the Type objects and the
T240 tables; an unknown name is @E-FMT-RESOLVE@ with the nearest known name, so
that under the published errata set Table 75's @Fractoin@ reads
@unresolved type name Fractoin (nearest: Fraction, T84)@ (design example E3).
-}
module Hsed.Spec.Format
  ( -- * Environment
    Env (..)
  , mkEnv
    -- * Parsing
  , parseFormat
  , formatNormalisations
  , locate
    -- * Pieces, exported for the tests
  , Token (..)
  , tokenize
  , tableName
  , nearest
  , globMatch
  ) where

import           Data.Char      (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Data.Text      as T
import qualified Data.Text.Read as TR
import           RIO
import           RIO.List       (lastMaybe, minimumByMaybe, scanl, zip3)
import qualified RIO.Map        as Map
import qualified RIO.Set        as Set

import           Hsed.Core.Types
import           Hsed.Spec.Common


-- | What a Format cell may name: the Type objects (with their table number when
--   known, for the @nearest@ hint) and the tables of T240 that a @uidref{...}@
--   may point at.
data Env = Env
  { envTypes  :: !(Map Text (Maybe Int))
  , envTables :: !(Set Text)
  }
  deriving (Eq, Show)

mkEnv :: [(Text, Maybe Int)] -> [Text] -> Env
mkEnv types tables = Env (Map.fromList types) (Set.fromList tables)


-- * Tokens

-- | The lexemes of a Format cell.
data Token
  = TComma
  | TStar
  | TNum !Int
  | TName !Text    -- ^ @[A-Za-z_][A-Za-z0-9_]*@
  | TBraced !Text  -- ^ the trimmed text between @{@ and @}@
  deriving (Eq, Show)

-- | Lex a cell; whitespace separates and is otherwise ignored.
tokenize :: Text -> Either Finding [Token]
tokenize = go
  where
    go t = case T.uncons t of
      Nothing -> Right []
      Just (c, rest)
        | isSpace c -> go rest
        | c == ','  -> (TComma :) <$> go rest
        | c == '*'  -> (TStar :) <$> go rest
        | c == '{'  -> braced rest
        | isDigit c -> number t
        | c == '-', Just (d, _) <- T.uncons rest, isDigit d -> negated rest
        | isNameStart c -> let (name, rest') = T.span isNameChar t in (TName name :) <$> go rest'
        | otherwise -> Left (syntax ("unexpected character " <> tshow c))
    braced t = case T.breakOn "}" t of
      (_, "")         -> Left (syntax "missing }")
      (inside, rest)  -> (TBraced (T.strip inside) :) <$> go (T.drop 1 rest)
    number t = let (digits, rest) = T.span isDigit t
               in either (Left . syntax . T.pack) (\(n, _) -> (TNum n :) <$> go rest) (decimal digits)
    negated t = let (digits, rest) = T.span isDigit t
                in either (Left . syntax . T.pack) (\(n, _) -> (TNum (negate n) :) <$> go rest) (decimal digits)
    decimal = TR.decimal :: Text -> Either String (Int, Text)
    isNameStart c = isAlpha c || c == '_'
    isNameChar c  = isAlphaNum c || c == '_'


-- * Arguments

-- | A comma-separated value after the Format code.
data Arg
  = AStar
  | ANum !Int
  | AName !Text
  | ARef !Text !Text   -- ^ keyword (@uidref@ or @ref@) and the braced target
  deriving (Eq, Show)

-- | Split the tokens at the commas: the code (name and optional @{n}@) and the arguments.
arguments :: [Token] -> Either Finding ((Text, Maybe Int), [Arg])
arguments tokens = case splitOn tokens of
  (fc : args) -> (,) <$> formatCodeOf fc <*> traverse argument args
  []            -> Left (syntax "empty Format")
  where
    splitOn = foldr cut [[]]
    cut TComma acc = [] : acc
    cut tok (grp : rest) = (tok : grp) : rest
    cut tok [] = [[tok]]
    formatCodeOf = \case
      [TName n]            -> Right (n, Nothing)
      [TName n, TBraced s] -> either (const (Left (syntax ("bad suffix {" <> s <> "} after " <> n))))
                                     (\(k, _) -> Right (n, Just k))
                                     (TR.decimal s :: Either String (Int, Text))
      other                -> Left (syntax ("the Format code is not a name: " <> tshow other))
    argument = \case
      [TStar]              -> Right AStar
      [TNum n]             -> Right (ANum n)
      [TName n]            -> Right (AName n)
      [TName kw, TBraced t]
        | kw `elem` ["uidref", "ref"] -> Right (ARef kw t)
      []                   -> Left (syntax "empty value between commas")
      other                -> Left (syntax ("unexpected value " <> tshow other))


-- * Parsing

-- | Parse one Format cell. The 'Finding' carries no location; 'locate' adds one.
parseFormat :: Env -> Text -> Either Finding TypeDef
parseFormat env text = tokenize text >>= arguments >>= uncurry (build env)

build :: Env -> (Text, Maybe Int) -> [Arg] -> Either Finding TypeDef
build env fc@(name, suffix) args = case (name, suffix, args) of
  ("Base_Type", Nothing, [])                   -> Right BaseType
  ("Simple_Type", Nothing, [AName b, size])     -> SimpleType <$> typeRef env b <*> operand size
  ("Enumeration_Type", Nothing, _)              -> EnumType <$> ranges args
  ("Alternative_Type", Nothing, _ : _ : _)      -> AltType <$> traverse (typeArg env) args
  ("List_Type", Nothing, [size, AName e])       -> ListType <$> operand size <*> typeRef env e
  ("Restricted_Reference_Type", Just 5, _ : _)  -> RestrictedRef ByteTableRow <$> traverse (tableArg env) args
  ("Restricted_Reference_Type", Just 6, _ : _)  -> RestrictedRef ObjectTableUID <$> traverse (tableArg env) args
  ("General_Reference_Type", Just 7, [])        -> Right (GeneralRef AnyByteTableRow)
  ("General_Reference_Type", Just 8, [])        -> Right (GeneralRef AnyObjectUID)
  ("General_Reference_Type", Just 9, [])        -> Right (GeneralRef AnyTableUID)
  ("General_Reference_Table_Type", Nothing, [ANum 1]) -> Right (GeneralRefTable ObjectTable)
  ("General_Reference_Table_Type", Nothing, [ANum 2]) -> Right (GeneralRefTable ByteTable)
  (_, Nothing, [AName n, AName t])
    | name `elem` ["Named_Value_Name_Type", "Name_Value_Name_Type"] -> NamedValueName n <$> typeRef env t
  (_, Nothing, [ANum i, AName t])
    | name `elem` ["Named_Value_Integer_Type", "Name_Value_Integer_Type"] -> NamedValueInteger i <$> typeRef env t
    | name `elem` ["Named_Value_Uinteger_Type", "Name_Value_Uinteger_Type"], i >= 0 -> NamedValueUinteger i <$> typeRef env t
  ("Struct_Type", Nothing, _ : _)               -> StructType <$> traverse (typeArg env) args
  ("Set_Type", Nothing, _)                      -> SetType <$> ranges args
  _ | spelled fc `Set.member` formatCodes       -> Left (arity fc args)
    | otherwise                                 -> Left (unknownCode fc)
  where
    ranges as = case traverse operand as of
      Left e -> Left e
      Right ops
        | null ops || odd (length ops) -> Left (arity fc args)
        | otherwise                    -> Right (pairs ops)
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _              = []

-- | A @uinteger_2@ argument.
operand :: Arg -> Either Finding Operand
operand = \case
  AStar          -> Right Star
  ANum n | n >= 0 -> Right (Lit n)
  other          -> Left (syntax ("expected a count or *, got " <> describe other))

-- | A Type object argument.
typeArg :: Env -> Arg -> Either Finding TypeRef
typeArg env = \case
  AName n -> typeRef env n
  other   -> Left (syntax ("expected a type name, got " <> describe other))

-- | A table argument of a Restricted_Reference_Type.
tableArg :: Env -> Arg -> Either Finding TableRef
tableArg env = \case
  AStar     -> Right TableStar
  ARef _ t  -> tableRef env (tableName t)
  other     -> Left (syntax ("expected uidref{...}, ref{...} or *, got " <> describe other))

describe :: Arg -> Text
describe = \case
  AStar     -> "*"
  ANum n    -> tshow n
  AName n   -> n
  ARef kw t -> kw <> "{" <> t <> "}"

-- | Resolve a Type object's name.
typeRef :: Env -> Text -> Either Finding TypeRef
typeRef env n
  | n `Map.member` envTypes env = Right (Ref n)
  | otherwise = Left (unresolved "type" n (hint <$> nearest (Map.keys (envTypes env)) n))
  where
    hint near = near <> maybe "" ((", T" <>) . tshow) (join (Map.lookup near (envTypes env)))

-- | Resolve a table's name, which may be a glob (@C_AES_*@).
tableRef :: Env -> Text -> Either Finding TableRef
tableRef env n
  | n `Set.member` tables = Right (Table n)
  | "*" `T.isInfixOf` n = if any (globMatch n) (Set.toList tables)
                            then Right (TableGlob n)
                            else Left (unresolved "table" n (nearest (Set.toList tables) (T.filter (/= '*') n)))
  | otherwise = Left (unresolved "table" n (nearest (Set.toList tables) n))
  where tables = envTables env

-- | What 'parseFormat' reads differently from the printed text, each as printed
--   and as read: a name spaced from its braces (@uidref {AuthorityTableUID}@,
--   @General_Reference_Type {7}@) and a table named with @Table_UID@ (T76's
--   @ColumnTable_UID@) rather than @TableUID@.
formatNormalisations :: Text -> [(Text, Text)]
formatNormalisations cell = spaced <> suffixed
  where
    spaced =
      [ (name <> gap <> braced, name <> braced)
      | (before, after) <- T.breakOnAll "{" cell
      , let gap = T.takeWhileEnd isSpace before
            name = T.takeWhileEnd isNameChar (T.dropWhileEnd isSpace before)
            braced = T.takeWhile (/= '}') after <> "}"
      , not (T.null gap), not (T.null name) ]
    suffixed =
      [ (inner, T.dropEnd (T.length "Table_UID") inner <> "TableUID")
      | (_, after) <- T.breakOnAll "{" cell
      , let inner = T.strip (T.takeWhile (/= '}') (T.drop 1 after))
      , "Table_UID" `T.isSuffixOf` inner ]
    isNameChar c = isAlphaNum c || c == '_'

-- | The T240 table name inside a @uidref{...}@: @C_PINTableUID@ and
--   @ColumnTable_UID@ (T76's spelling) both name their table.
tableName :: Text -> Text
tableName t = fromMaybe t (asum (map (`T.stripSuffix` t) ["TableUID", "Table_UID", "Table"]))

-- | @*@ matches any run of characters.
globMatch :: Text -> Text -> Bool
globMatch pat t = case T.breakOn "*" pat of
  (lit, "")   -> lit == t
  (lit, rest) -> case T.stripPrefix lit t of
    Nothing -> False
    Just t' -> any (globMatch (T.drop 1 rest)) (T.tails t')

-- | The known name closest to an unknown one (fewest edits, then alphabetical).
nearest :: [Text] -> Text -> Maybe Text
nearest known n = snd <$> minimumByMaybe compare [ (levenshtein n k, k) | k <- known ]

levenshtein :: Text -> Text -> Int
levenshtein a b = fromMaybe 0 (lastMaybe (foldl' row [0 .. T.length a] (T.unpack b)))
  where
    row prev@(p : ps) c = scanl step (p + 1) (zip3 (T.unpack a) prev ps)
      where step left (ac, diag, up) = min (left + 1) (min (up + 1) (diag + if ac == c then 0 else 1))
    row [] _ = []

-- | Place a finding at a table of a spec.
locate :: SpecId -> Text -> Maybe Int -> Finding -> Finding
locate s a p f = f { spec = Just s, anchor = Just a, page = p }


-- * Findings

syntax :: Text -> Finding
syntax = finding Error "E-FMT-SYNTAX"

unresolved :: Text -> Text -> Maybe Text -> Finding
unresolved what n near =
  finding Error "E-FMT-RESOLVE" ("unresolved " <> what <> " name " <> n <> maybe "" (\h -> " (nearest: " <> h <> ")") near)

unknownCode :: (Text, Maybe Int) -> Finding
unknownCode fc = finding Error "E-FMT-CODE"
  ("unknown Format code " <> spelled fc <> maybe "" (\h -> " (nearest: " <> h <> ")") (nearest (Set.toList formatCodes) (spelled fc)))

arity :: (Text, Maybe Int) -> [Arg] -> Finding
arity fc args = finding Error "E-FMT-ARITY"
  (spelled fc <> " does not take " <> T.intercalate ", " (map describe args))

spelled :: (Text, Maybe Int) -> Text
spelled (n, s) = n <> maybe "" (\k -> "{" <> tshow k <> "}") s

-- | The sixteen Format codes as the Type tables spell them (dev/Core_Types.txt),
--   plus the ABNF's @Name_Value_@ spellings of codes 12 and 13.
formatCodes :: Set Text
formatCodes = Set.fromList
  [ "Base_Type", "Simple_Type", "Enumeration_Type", "Alternative_Type", "List_Type"
  , "Restricted_Reference_Type{5}", "Restricted_Reference_Type{6}"
  , "General_Reference_Type{7}", "General_Reference_Type{8}", "General_Reference_Type{9}"
  , "General_Reference_Table_Type"
  , "Named_Value_Name_Type", "Named_Value_Integer_Type", "Named_Value_Uinteger_Type"
  , "Name_Value_Name_Type", "Name_Value_Integer_Type", "Name_Value_Uinteger_Type"
  , "Struct_Type", "Set_Type" ]
