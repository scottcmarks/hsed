{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

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
"Hsed.Spec.Rules.TableDesc" at M4b's scaffold and extended at M4b for an
SSC's preconfiguration tables (Opal 2.30 sections 4.2-4.3, PDF 35-89). It
reads by a 'TypeEnv', the Type objects and the enumeration labels of a spec
and, for a spec read over another (Opal over Core), of its base
("Hsed.Spec.Rules.TableDesc".'Hsed.Spec.Rules.TableDesc.typeEnv' builds it).

What a column holds ('ColKind') comes from its Type object's Format and, for
an enumeration, its enumeration table's labels; a column of the @uid@ type,
Core Table 145, PDF 123, is 'KUID', a uid cell, where any other @bytes@ type
is 'KBytes'. Since M4b also: @password@ (Core Table 122, PDF 118) a
credential's bytes ('KPassword'); @table_or_object_ref@ (Table 139, PDF
122), an alternative of general references to an object or a table, a UID
('KUIDRef'); a List_Type a list of its element's kind ('KList': @ACL@, Table
50, PDF 101; @SSC@, Table 132, PDF 120), but @AC_element@ (Table 46, PDF 100),
a list of @ACE_expression@ (Table 48, PDF 101), an alternative of an
authority's reference and @boolean_ACE@, which is the infix expression the
ACE tables print and the runtime evaluates in postfix ('KExpr'); @ACE_columns@
(Table 47, PDF 100-101) column names ('KColumns'); @byte_table_ref@ (Table 63,
PDF 104) 'KByteTableRef'; a Struct_Type (@date@, Table 78, PDF 107) blank only
('KStruct'); and a type that names no Type object but is @\<t\>_def_\<v\>@
(Core Table 215's @boolean_def_false@, PDF 232-233) is read as @t@ defaulting
to @v@ ('KDefault': the lead's answer 4, read here and nowhere else, so that
Core's C-FMT-RESOLVE Warnings stay).

A cell is read as that kind ('readCell', and 'readCellWith' with the
resolution of names and columns and what reading found) and printed as the
runtime 'Hsed.Core.Table.Cell' it is ('renderCell': a 'UIDCell', a
'BytesCell' for bytes of any other type and for a password, a 'ListCell' for
a list, a set or an empty struct). A label reads as the enumeration prints
it, else as a number its ranges hold, else as the label whose
'Hsed.Spec.Readers.Text.nameKey' it is (case and spaces aside: Opal Table
29's @Manufactured-Inactive@ for Table 49's @manufactured-inactive@, PDF 50,
98), else as the one label it begins (@T@ and @F@ for True and False, Opal
Tables 18-29, PDF 35-50), the last two remarked ('LabelRead',
@N-PRECONF-LABEL@); the value keeps the label as printed. A text longer than
its type allows is kept as printed and remarked ('OverLength',
@W-PRECONF-NAME-LENGTH@: Opal Table 23's 40- and 55-byte ACE names against
name's 32, PDF 47; the lead's answer 5). @Null@ in a reference column is the
NULL UID, all zeroes (Core 3.2.5.3, PDF 52; 5.1.2, PDF 96).

An SSC's preconfiguration cell prints more than its value ('printedCell'):
a footnote marker (@*AC1@, @*MT1@, @*ST1@, @*TP1@, @*SP1@, @*ACE1@: an
asterisk, capitals and digits) and a digit printed after a placeholder
(Table 24's @(+XX)1@, PDF 48) are its row's notes; @(O)@ marks its row
optional (PDF 38, 46-47); @(+XX)@ marks its row a template and is its
placeholder (Table 24, PDF 48; the Locking SP's @(+XX XX)@, @(+MMMM)@ and
@(+NNNN)@ at M5); straight and curly quotes are removed, an unbalanced one
too (Table 20 row 12's @DataRemovalMecha nism”@, PDF 37), an empty pair
being an empty value (Table 25's @“”@, PDF 49); and @VU@ and @MSID@ are
values the TPer chooses ('vendorMarker'; Opal Table 1, PDF 12; Table 25, PDF
48). The extractor joins a cell's lines with a space; the reader keeps the
line breaks ('printedText', from the IR's @lines@ while they still give its
text) and joins them as the kind reads them: with nothing for a kind that
holds identifiers ('identifierKind': names, references, lists of them,
column names), since every break inside a word in these tables is a line
break (Table 20's @Obje|ct@, Table 24's @C_PIN_Admin|1@, Table 22's
@ACE_C_PIN_SID_Set_PI|N@, PDF 36-46); with a space for any other kind, a
label read as printed either way; an expression joins two words a line break
parts when neither is an operator.

'zeroValue' is a kind's default, the value a column takes where a
preconfiguration row gives none (design 13.31 (a)): an unsigned or signed
integer 0, an enumeration its lowest value with its label, a reference the
NULL UID, a list, a set or a struct empty, a text or a password empty, bytes
zeroes; @\<t\>_def_\<v\>@ its @v@. These agree with Core's stated defaults:
TryLimit 0 (PDF 171), Tries 0 (PDF 210), CharSet the NULL UID (PDF 171,
210), Frozen False (PDF 233), ClockStart and ClockEnd an empty struct
without the Clock template (PDF 168-169), the Log columns 0 and LogTo the
NULL UID without the Log template (PDF 163-164).

Codes ('cellCodes', claimed here, given by the rule that reads the cell from
its remarks): @N-PRECONF-LABEL@ (a Note) and @W-PRECONF-NAME-LENGTH@ (a
Warning).
-}
module Hsed.Spec.Readers.Cell
  ( -- * The types a column is read by
    TypeEnv (..)
    -- * What a column holds
  , ColKind (..)
  , colKind
  , identifierKind
  , CellValue (..)
  , readCell
  , renderCell
  , zeroValue
    -- * Reading a cell with its names, its columns and its remarks
  , Resolver (..)
  , Problem (..)
  , problemText
  , Remark (..)
  , readCellWith
  , listItems
  , matchLabel
    -- * A cell as an SSC's preconfiguration table prints it
  , PrintedCell (..)
  , printedCell
  , hasValue
  , printedText
  , joinedText
  , vendorMarker
    -- * Codes
  , cellCodes
  ) where

import qualified Data.Char              as DC
import qualified Data.Text              as DT
import qualified Data.Text.Read         as TR
import qualified RIO.ByteString         as B
import qualified RIO.Char               as C
import qualified RIO.List               as L
import qualified RIO.Text               as T
import           Numeric                (showHex)
import           RIO

import           Hsed.Core.Types        (GeneralKind (..), Operand (..), RestrictedKind (..), TableKind (..), TableRef (..),
                                         TypeDef (..), TypeRef (..), formatName)
import           Hsed.Spec.Common       (CodeDef (..), Reports (..), Severity (..))
import qualified Hsed.Spec.Gen          as Gen
import           Hsed.Spec.IR           (CellIR (..))
import           Hsed.Spec.Readers.Text (nameKey, readNumber)


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
  | KPassword !Int                 -- ^ the @password@ type (Core Table 122): a credential's bytes, at most that many
  | KUIDRef                        -- ^ an alternative of general references to an object or a table
                                   --   (@table_or_object_ref@, Core Table 139): a table's or an object's UID
  | KList !ColKind                 -- ^ a List_Type: items of the element's kind (@ACL@, Core Table 50; @SSC@, Table 132)
  | KExpr !Text ![(Text, Int)] ![TableRef]
    -- ^ a List_Type of an alternative of a reference and an enumeration (@AC_element@ over
    --   @ACE_expression@, Core Tables 46 and 48): the operators' type, their labels, and the
    --   tables an operand names (Authority)
  | KColumns                       -- ^ the @ACE_columns@ type (Core Table 47): column names, read in the table an ACE applies to
  | KByteTableRef                  -- ^ a General_Reference_Table_Type of a byte table (@byte_table_ref@, Core Table 63)
  | KStruct !Text                  -- ^ a Struct_Type (@date@, Core Table 78): blank only
  | KDefault !Text !ColKind        -- ^ @\<t\>_def_\<v\>@, which names no Type object: @t@'s kind, defaulting to @v@
  deriving (Eq, Show)

-- | What a column of a type holds, or why it is not read from a cell. A
--   Simple_Type's base is one of the four base types of Core 5.1.1 (@bytes@,
--   @max_bytes@, @uinteger@, @integer@), a Type object whose Format is
--   @Base_Type@; @uid@, @password@ and @ACE_columns@ are read by their names,
--   the other kinds by their Formats.
colKind :: TypeEnv -> Text -> Either Text ColKind
colKind env name = case teType env name of
  Nothing -> case defaulted name of
    Just (t, v) | isJust (teType env t) -> KDefault v <$> colKind env t
    _                                   -> Left (name <> " names no Type object")
  Just (Left _)   -> Left (name <> "'s Format did not parse")
  Just (Right td) -> kindOf td
  where
    kindOf = \case
      SimpleType (Ref base) (Lit n) -> do
        btd <- maybe (Left (name <> "'s base " <> base <> " names no Type object")) Right (teType env base)
        case btd of
          Right BaseType -> case base of
            "bytes"
              | name == "uid"      -> Right KUID
              | otherwise          -> Right (KBytes n)
            "max_bytes"
              | name == "password" -> Right (KPassword n)
              | otherwise          -> Right (KText n)
            "uinteger"  -> Right (KUInt n)
            "integer"   -> Right (KInt n)
            _           -> Left (name <> "'s base " <> base <> " is not a base type of Core 5.1.1")
          _ -> Left (name <> "'s base " <> base <> " is not a Base_Type")
      EnumType ranges -> do
        rs <- maybe (Left (name <> "'s range is not literal")) Right (traverse lits ranges)
        Right (KEnum name rs (teLabels env name))
      RestrictedRef ObjectTableUID tables -> Right (KRef tables)
      AltType alts | not (null alts), all generalUID alts -> Right KUIDRef
      ListType _ (Ref e) -> case typeOf e of
        Just (AltType alts) | Just k <- expression alts -> Right k
        _                                               -> KList <$> colKind env e
      SetType _ | name == "ACE_columns" -> Right KColumns
      GeneralRefTable ByteTable -> Right KByteTableRef
      StructType _ -> Right (KStruct name)
      other -> Left ("a column of type " <> name <> " (" <> formatName other <> ") is not read from a table cell")
    typeOf n = teType env n >>= either (const Nothing) Just
    generalUID (Ref r) = typeOf r `elem` [Just (GeneralRef AnyObjectUID), Just (GeneralRef AnyTableUID)]
    -- an alternative of an object reference and an enumeration: the operands and the operators of an expression
    expression alts = case [ (r, typeOf r) | Ref r <- alts ] of
      [a, b] | Just k <- pair a b <|> pair b a -> Just k
      _                                        -> Nothing
    pair (_, Just (RestrictedRef ObjectTableUID tables)) (op, Just (EnumType _)) = Just (KExpr op (teLabels env op) tables)
    pair _ _                                                                     = Nothing
    lits (Lit a, Lit b) = Just (a, b)
    lits _              = Nothing

-- | @boolean_def_false@: the type @boolean@ and the default @false@.
defaulted :: Text -> Maybe (Text, Text)
defaulted n = case DT.breakOnEnd "_def_" n of
  (before, v) | Just t <- T.stripSuffix "_def_" before, not (T.null t), not (T.null v) -> Just (t, v)
  _ -> Nothing

-- | Whether a kind holds identifiers, whose printed lines join with nothing
--   between them: names, references, lists of them, column names.
identifierKind :: ColKind -> Bool
identifierKind = \case
  KText _       -> True
  KPassword _   -> True
  KRef _        -> True
  KByteTableRef -> True
  KColumns      -> True
  KList k       -> identifierKind k
  KDefault _ k  -> identifierKind k
  _             -> False

-- | A cell read as what its column holds.
data CellValue
  = CvUID ![Word8]                 -- ^ a @uid@'s eight bytes
  | CvBytes ![Word8]
  | CvText !Text
  | CvUInt !Integer !(Maybe Int)   -- ^ the value, and the hex digits it is printed with (Nothing: in decimal)
  | CvInt !Integer
  | CvEnum !Text !Int
  | CvRef !Text ![Word8]           -- ^ an object named, and its UID
  | CvList ![CellValue]            -- ^ a list, a set (an ACE's column numbers, an expression in postfix) or an empty struct
  deriving (Eq, Show)

-- | Read a non-blank cell as a column of a kind holds it; an object reference
--   is resolved by the caller's function from the object's name and the tables
--   the type allows to the object's UID. What reading finds besides the value
--   ('Remark') is not kept; 'readCellWith' keeps it.
readCell :: (Text -> [TableRef] -> Either Text [Word8]) -> ColKind -> Text -> Either Text CellValue
readCell resolve kind raw = bimap problemText fst (readCellWith rv kind raw)
  where
    rv = Resolver
      { rvRef    = \n tables -> (n,) <$> resolve n tables
      , rvColumn = \n -> Left (tshow n <> " names a column, and no table is given to read it in") }


-- * Reading a cell with its names, its columns and its remarks

-- | What reading a cell needs from outside it: the object a name or a UID
--   names among the tables its type allows (the name it is kept under, and
--   its UID), and the number of a column a name names, in the table an ACE
--   applies to.
data Resolver = Resolver
  { rvRef    :: Text -> [TableRef] -> Either Text (Text, [Word8])
  , rvColumn :: Text -> Either Text Int
  }

-- | Why a cell does not read.
data Problem
  = Unread !Text       -- ^ the text is no value of the kind
  | Unresolved !Text   -- ^ a name names no row, no column or no one table
  | BadExpr !Text      -- ^ an expression that does not parse
  deriving (Eq, Show)

-- | What a problem says.
problemText :: Problem -> Text
problemText = \case
  Unread t     -> t
  Unresolved t -> t
  BadExpr t    -> t

-- | What reading a cell found that is no reason to refuse it.
data Remark
  = LabelRead !Text !Text !Text !Bool
    -- ^ an enumeration's label printed otherwise: the type, the label as printed, the
    --   enumeration's label, and whether it was read by its first letters (else case and spaces aside)
  | OverLength !Text !Int !Int
    -- ^ a text longer than its type allows, kept as printed: the text, its bytes, the type's
  deriving (Eq, Show)

-- | Read a non-blank cell, its line breaks as @\\n@, as a column of a kind
--   holds it: the value and what reading it found, or why it does not read.
readCellWith :: Resolver -> ColKind -> Text -> Either Problem (CellValue, [Remark])
readCellWith rv kind raw = case kind of
  KUID -> unread $ case bytesOf flat of
    Just bs | length bs == 8 -> Right (CvUID bs)
    _ -> Left (tshow flat <> " is not a uid of eight bytes in hex")
  KBytes n -> unread $ case bytesOf flat of
    Just bs | length bs == n -> Right (CvBytes bs)
    _ -> Left (tshow flat <> " is not " <> tshow n <> " bytes in hex")
  KText n -> Right (CvText tight, [ OverLength tight len n | let len = B.length (encodeUtf8 tight), len > n ])
  KUInt n -> unread $ do
    v <- maybe (Left (tshow flat <> " is not an unsigned integer")) Right (number flat)
    if v < 256 ^ n then Right (CvUInt v (hexDigits flat)) else Left (tshow flat <> " does not fit in " <> tshow n <> " bytes")
  KInt n -> unread $ do
    v <- maybe (Left (tshow flat <> " is not an integer")) Right (signed flat)
    let bound = 2 ^ (8 * n - 1)
    if negate bound <= v && v < bound then Right (CvInt v) else Left (tshow flat <> " does not fit in " <> tshow n <> " bytes")
  KEnum name ranges labelled -> enum name ranges labelled
  KRef tables
    | isNull tight -> Right (nullRef tight, [])
    | otherwise    -> bimap Unresolved (\(n, u) -> (CvRef n u, [])) (rvRef rv tight tables)
  KPassword n
    | B.length (encodeUtf8 tight) > n -> Left (Unread (tshow tight <> " is longer than " <> tshow n <> " bytes"))
    | otherwise                       -> Right (CvBytes (B.unpack (encodeUtf8 tight)), [])
  KUIDRef
    | isNull flat -> Right (nullRef flat, [])
    | otherwise   -> unread $ case bytesOf flat of
        Just bs | length bs == 8 -> Right (CvUID bs)
        _ -> Left (tshow flat <> " is not the UID of a table or an object, eight bytes in hex")
  KList k -> do
    items <- traverse (readCellWith rv k) (listItems k raw)
    Right (CvList (map fst items), concatMap snd items)
  KExpr opType ops tables -> expression opType ops tables
  KColumns
    | nameKey tight == "all" -> Right (CvList [], [])
    | otherwise -> do
        ns <- traverse column (commaItems tight)
        Right (CvList [ CvUInt (toInteger n) Nothing | n <- ns ], [])
  KByteTableRef
    | isNull tight -> Right (nullRef tight, [])
    | otherwise    -> Left (Unread (tshow tight <> " is no byte table's reference: a preconfiguration prints Null, the NULL UID"))
  KStruct n -> Left (Unread (tshow flat <> " is not read: a column of type " <> n <> " (Struct_Type) is read blank, its default an empty struct"))
  KDefault _ k -> readCellWith rv k raw
  where
    flat  = T.strip (T.unwords (map T.strip (T.lines raw)))
    tight = T.concat (map T.strip (T.lines raw))
    unread = bimap Unread (, [])
    hexDigits t = T.length <$> T.stripPrefix "0x" (T.toLower t)
    number t = case T.stripPrefix "0x" (T.toLower t) of
      Just h  -> either (const Nothing) (\(v, rest) -> if T.null rest then Just v else Nothing) (TR.hexadecimal h)
      Nothing -> either (const Nothing) (\(v, rest) -> if T.null rest then Just v else Nothing) (TR.decimal t)
    signed t = case T.stripPrefix "-" t of
      Just rest -> negate <$> number rest
      Nothing   -> number t
    -- a label as printed, its lines joined with or without a space; a number; else by its key
    enum name ranges labelled = case (lookup flat labelled, lookup tight labelled, number flat) of
      (Just v, _, _) -> Right (CvEnum flat v, [])
      (_, Just v, _) -> Right (CvEnum tight v, [])
      (_, _, Just v)
        | any (\(lo, hi) -> toInteger lo <= v && v <= toInteger hi) ranges -> Right (CvEnum (tshow v) (fromIntegral v), [])
      _ -> case matchLabel labelled tight of
        Just (label, v, byPrefix) -> Right (CvEnum tight v, [LabelRead name tight label byPrefix])
        Nothing -> Left (Unread (tshow flat <> " is no label of " <> name <> "'s enumeration ("
                                 <> T.intercalate ", " (map fst labelled) <> ")"))
    column item = case readNumber item of
      Just n  -> Right (fromInteger n)
      Nothing -> first Unresolved (rvColumn rv item)
    -- infix to postfix (Core 5.3.4.3.2 and Table 210, PDF 226-227), each operand an object's reference
    expression opType ops tables = do
      items <- first BadExpr (postfix ops (exprWords ops raw))
      read' <- traverse (exprItem opType tables) items
      Right (CvList (map fst read'), concatMap snd read')
    exprItem opType tables = \case
      Operand n
        | isNull n  -> Right (nullRef n, [])
        | otherwise -> bimap Unresolved (\(n', u) -> (CvRef n' u, [])) (rvRef rv n tables)
      Operator printed label v -> Right (CvEnum printed v, [ LabelRead opType printed label False | printed /= label ])

-- | A NULL UID reference, under the name printed.
nullRef :: Text -> CellValue
nullRef t = CvRef t (replicate 8 0)

-- | Is a reference printed @Null@, the NULL UID (Core 3.2.5.3, PDF 52)?
isNull :: Text -> Bool
isNull t = nameKey t == "null"

-- | Hex bytes, whitespace aside.
bytesOf :: Text -> Maybe [Word8]
bytesOf t = if even (T.length hex) && T.all C.isHexDigit hex then Just (map byte (DT.chunksOf 2 hex)) else Nothing
  where
    hex = T.filter (not . C.isSpace) t
    byte = T.foldl' (\acc c -> acc * 16 + fromIntegral (DC.digitToInt c)) 0

-- | The items of a list cell, its line breaks as @\\n@ joined as the element
--   kind reads them: the text between brackets when it is bracketed (Opal
--   Table 27's @[“Opal”]@, PDF 50), split at its commas, blank items dropped.
listItems :: ColKind -> Text -> [Text]
listItems k raw = commaItems (unbracket joined)
  where
    joined = if identifierKind k then T.concat (map T.strip (T.lines raw)) else T.strip (T.unwords (map T.strip (T.lines raw)))
    unbracket t = fromMaybe t (T.stripPrefix "[" t >>= T.stripSuffix "]")

commaItems :: Text -> [Text]
commaItems = filter (not . T.null) . map T.strip . DT.splitOn ","

-- | The label an enumeration gives a text printed otherwise: the label whose
--   'nameKey' the text's is (case and spaces aside), else the one label whose
--   key begins with it; the label, its value, and whether it was the second.
matchLabel :: [(Text, Int)] -> Text -> Maybe (Text, Int, Bool)
matchLabel labelled printed
  | T.null key = Nothing
  | (l, v) : _ <- [ lv | lv@(l, _) <- labelled, nameKey l == key ] = Just (l, v, False)
  | [(l, v)] <- L.nubBy (\a b -> fst a == fst b) [ lv | lv@(l, _) <- labelled, key `T.isPrefixOf` nameKey l ] = Just (l, v, True)
  | otherwise = Nothing
  where key = nameKey printed


-- * Expressions

-- | One item of an expression in postfix: an operand's name, or an operator
--   as printed with its label and value.
data ExprItem = Operand !Text | Operator !Text !Text !Int

-- | The words of an expression, line by line: parentheses apart, and two
--   words a line break parts joined where neither is an operator (an
--   authority's name broken over two lines).
exprWords :: [(Text, Int)] -> Text -> [Text]
exprWords ops raw = foldl' add [] (filter (not . null) (map lineWords (T.lines raw)))
  where
    -- a word's parentheses each a word of its own: (A is ( and A
    lineWords = concatMap (DT.groupBy (\a b -> not (paren a) && not (paren b))) . T.words
    paren = (`elem` ("()" :: String))
    add acc line = case (reverse acc, line) of
      (end : front, w : ws) | operand end, operand w -> reverse front <> [end <> w] <> ws
      _                                              -> acc <> line
    operand w = w `notElem` ["(", ")"] && isNothing (operatorOf ops w)

-- | The label and value an operator's word names, its case aside (@OR@ is
--   boolean_ACE's @Or@, Core Table 61, PDF 104).
operatorOf :: [(Text, Int)] -> Text -> Maybe (Text, Int)
operatorOf ops w = listToMaybe [ lv | lv@(l, _) <- ops, nameKey l == nameKey w ]

-- | An operator's binding and whether it takes one operand, by its label:
--   Not binds tightest and takes one, then And, then Or (boolean_ACE, Core
--   Table 61, PDF 104; Core prints no rule of precedence, 5.3.4.3.2 using
--   parentheses, PDF 226, so this is the usual one, the M4b plan's).
binding :: Text -> (Int, Bool)
binding label = case nameKey label of
  "not" -> (3, True)
  "and" -> (2, False)
  _     -> (1, False)

-- | An infix expression's words in postfix (the shunting yard): operands in
--   order, each operator after its operands, binary ones left-associative,
--   parentheses grouping; or why the words are no expression.
postfix :: [(Text, Int)] -> [Text] -> Either Text [ExprItem]
postfix ops = go True [] []
  where
    go :: Bool -> [Maybe (Text, Text, Int)] -> [ExprItem] -> [Text] -> Either Text [ExprItem]
    go expecting stack out = \case
      []
        | null out && null stack -> Right []
        | expecting -> Left "the expression ends where an authority is expected"
        | otherwise -> finish stack out
      w : ws -> case (w, operatorOf ops w) of
        ("(", _)
          | expecting -> go True (Nothing : stack) out ws
          | otherwise -> Left ("a parenthesis follows an authority where an operator (" <> operators <> ") is expected")
        (")", _)
          | expecting -> Left "a closing parenthesis where an authority is expected"
          | otherwise -> case break isNothing stack of
              (popped, _ : rest) -> go False rest (out <> map operator popped) ws
              (_, [])            -> Left "a closing parenthesis closes no group"
        (_, Just (label, v))
          | snd (binding label) -> if expecting then go True (Just (w, label, v) : stack) out ws
                                   else Left (tshow w <> ", which takes one operand, follows an authority")
          | expecting -> Left (tshow w <> " has no authority before it")
          | otherwise ->
              let (popped, rest) = span (maybe False (\(_, l, _) -> fst (binding l) >= fst (binding label))) stack
              in go True (Just (w, label, v) : rest) (out <> map operator popped) ws
        _
          | expecting -> go False stack (out <> [Operand w]) ws
          | otherwise -> Left (tshow w <> " follows an authority where an operator (" <> operators <> ") is expected")
    operators = T.intercalate ", " (map fst ops)
    finish stack out
      | any isNothing stack = Left "a parenthesis opens a group no parenthesis closes"
      | otherwise           = Right (out <> map operator stack)
    operator = \case
      Just (w, label, v) -> Operator w label v
      Nothing            -> Operator "(" "(" 0


-- | The runtime 'Hsed.Core.Table.Cell' a value prints as: bytes of a type
--   other than @uid@ a @BytesCell@, its byte string a literal (the module's
--   OverloadedStrings), however many bytes; a list a @ListCell@.
renderCell :: CellValue -> Text
renderCell = \case
  CvUID bs        -> "UIDCell (" <> Gen.uidExpr bs <> ")"
  CvBytes bs      -> "BytesCell " <> tshow (B.pack bs)
  CvText t        -> "TextCell " <> tshow t
  CvUInt v digits -> "UIntCell " <> maybe (tshow v) (hexLiteral v) digits
  CvInt v         -> "IntCell " <> if v < 0 then "(" <> tshow v <> ")" else tshow v
  CvEnum l v      -> "EnumCell " <> tshow l <> " " <> tshow v
  CvRef n bs      -> "RefCell " <> tshow n <> " (" <> Gen.uidExpr bs <> ")"
  CvList vs       -> "ListCell [" <> T.intercalate ", " (map renderCell vs) <> "]"
  where hexLiteral v w = "0x" <> T.justifyRight w '0' (T.toUpper (T.pack (showHex v "")))

-- | The value a column of a kind takes where a row gives none: its type's
--   default (design 13.31 (a); Core's stated defaults, PDF 163-171, 210, 233).
--   Nothing for an enumeration with no literal range, or a default
--   @\<t\>_def_\<v\>@'s @v@ that does not read as @t@.
zeroValue :: ColKind -> Maybe CellValue
zeroValue = \case
  KUID           -> Just (CvUID (replicate 8 0))
  KBytes n       -> Just (CvBytes (replicate n 0))
  KText _        -> Just (CvText "")
  KUInt _        -> Just (CvUInt 0 Nothing)
  KInt _         -> Just (CvInt 0)
  KEnum _ rs ls  -> (\lo -> CvEnum (fromMaybe (tshow lo) (listToMaybe [ l | (l, v) <- ls, v == lo ])) lo) <$> lowest rs
  KRef _         -> Just (nullRef "Null")
  KPassword _    -> Just (CvBytes [])
  KUIDRef        -> Just (CvUID (replicate 8 0))
  KList _        -> Just (CvList [])
  KExpr {}       -> Just (CvList [])
  KColumns       -> Just (CvList [])
  KByteTableRef  -> Just (nullRef "Null")
  KStruct _      -> Just (CvList [])
  KDefault v k   -> case k of
    KEnum _ _ ls -> (\(l, n, _) -> CvEnum l n) <$> matchLabel ls v
    _            -> either (const Nothing) (Just . fst) (readCellWith none k v)
  where
    lowest rs = listToMaybe (L.sort (map fst rs))
    none = Resolver (\n _ -> Left (tshow n <> " is no default")) (\n -> Left (tshow n <> " is no default"))


-- * A cell as an SSC's preconfiguration table prints it

-- | A cell of an SSC's preconfiguration table with its conventions read.
data PrintedCell = PrintedCell
  { pcLines       :: ![Text]         -- ^ its value's lines, each stripped and none blank: its notes, @(O)@,
                                     --   its placeholder and its quotes removed
  , pcQuoted      :: !Bool           -- ^ it printed a quote (an empty pair is an empty value)
  , pcNotes       :: ![Text]         -- ^ its footnote markers as printed (@*AC1@), and a digit after its placeholder
  , pcOptional    :: !Bool           -- ^ it printed @(O)@
  , pcPlaceholder :: !(Maybe Text)   -- ^ what its @(+…)@ holds, its words single-spaced: @XX@, @XX XX@, @MMMM@
  }
  deriving (Eq, Show)

-- | A cell's conventions read: its text by its IR lines while they give it
--   (an edited or corrected text by itself), its placeholder (one per cell)
--   and the digits after it, its markers and @(O)@ word by word, its quotes
--   character by character.
printedCell :: CellIR -> PrintedCell
printedCell c = PrintedCell
  { pcLines       = filter (not . T.null) (map (T.strip . T.filter (not . isQuote)) kept)
  , pcQuoted      = any (T.any isQuote) kept
  , pcNotes       = superscript <> filter isMarker (concatMap T.words lns)
  , pcOptional    = "(O)" `elem` concatMap T.words lns
  , pcPlaceholder = placeholder
  }
  where
    source = if not (null c.lines) && T.intercalate " " c.lines == c.text then c.lines else [c.text]
    (placeholder, superscript, rest) = takePlaceholder (T.intercalate "\n" source)
    lns  = T.lines rest
    kept = [ T.unwords [ w | w <- T.words l, not (isMarker w), w /= "(O)" ] | l <- lns ]

-- | The first @(+…)@ in a text: what it holds, the digits printed right
--   after it, and the text without both.
takePlaceholder :: Text -> (Maybe Text, [Text], Text)
takePlaceholder t = case DT.breakOn "(+" t of
  (before, rest)
    | not (T.null rest), (inside, close) <- DT.breakOn ")" (T.drop 2 rest), not (T.null close) ->
        let (digits, after) = T.span C.isDigit (T.drop 1 close)
        in (Just (T.unwords (T.words inside)), [ digits | not (T.null digits) ], before <> after)
  _ -> (Nothing, [], t)

-- | A footnote marker: an asterisk, capitals, digits (@*AC1@, @*ACE1@).
isMarker :: Text -> Bool
isMarker w = case T.uncons w of
  Just ('*', rest) -> let (letters, digits) = T.span C.isAsciiUpper rest
                      in not (T.null letters) && not (T.null digits) && T.all C.isDigit digits
  _ -> False

-- | A straight or curly quote.
isQuote :: Char -> Bool
isQuote = (`elem` ("\"\x201C\x201D\x2018\x2019" :: String))

-- | Does a printed cell give a value: a line, or a quoted empty one?
hasValue :: PrintedCell -> Bool
hasValue pc = not (null (pcLines pc)) || pcQuoted pc

-- | A printed cell's value, its line breaks as @\\n@, for 'readCellWith'.
printedText :: PrintedCell -> Text
printedText = T.intercalate "\n" . pcLines

-- | A printed cell's value as a kind joins its lines, a kind's not known
--   with a space: what a message or a comment shows.
joinedText :: Maybe ColKind -> PrintedCell -> Text
joinedText kind pc
  | maybe False identifierKind kind = T.concat (pcLines pc)
  | otherwise                       = T.unwords (pcLines pc)

-- | A value the TPer chooses: @VU@ (vendor unique, Opal Table 1, PDF 12) or
--   @MSID@ (C_PIN_MSID's PIN, Table 25, PDF 48), printed alone and unquoted.
vendorMarker :: PrintedCell -> Maybe Text
vendorMarker pc = case pcLines pc of
  [l] | l `elem` ["VU", "MSID"], not (pcQuoted pc) -> Just l
  _ -> Nothing


-- * Codes

-- | The codes the column reader claims, for the catalogue
--   ("Hsed.Spec.Readers".'Hsed.Spec.Readers.readerCodes'): its remarks', which
--   the rule reading a cell gives.
cellCodes :: [CodeDef]
cellCodes =
  [ CodeDef "N-PRECONF-LABEL"       [Note]    (AsPrinted "an enumeration's label printed in another case or spacing, or by its first letters (T for True), read as the label")
  , CodeDef "W-PRECONF-NAME-LENGTH" [Warning] (Defect "a text longer than its type allows (a name of more than 32 bytes), kept as printed") ]
