{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Spec.Grammar
Description : The payload grammar of an annotation: parser, elaborator and printer
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.3:

@
payload  ::= binding | modifier+              -- a bare modifier chain applies to the table's rule default
binding  ::= name arg* | modifier binding | binding '&' binding
modifier ::= name arg*
arg      ::= name | int | 0xHEX | "text" | (ref) | [arg, ...] | name ~> name | \@'Shape
ref      ::= core int | opal int | § "dotted"
@

'parsePayload' reads the surface syntax into an 'Expr' with positions, then
elaborates it with the smart constructors of "Hsed.Spec.DSL" so that every
name, arity and modifier shape is checked, and 'printPayload' prints the typed
result back into the surface syntax: @parsePayload . printPayload == Right@.
The printed form is canonical: a modifier is written before what it modifies
(@rangeWords ["to"] (cols 0 1 enumValues)@; the design's @enumValues (cols 0 1)@
also parses), @§ "5.1.3"@ prints as @section "5.1.3"@ because @§@ is no Haskell
name, and the same text is what the generated bindings module contains.

Two modifiers are checked beyond their arity: @erratum@ takes only Scott's set
(@erratum published [...]@ is @E-ERRATUM-PUBLISHED@: the published set is the
text as printed and takes no corrections), and @waive@ names one of the checks
"Hsed.Spec.DSL".'waivableChecks' lists.
-}
module Hsed.Spec.Grammar
  ( -- * Payloads
    Payload (..)
  , parsePayload
  , printPayload
    -- * Printing the typed language
  , printSomeBinding
  , printBinding
  , printModifier
  , printModifiers
  , printRef
  , haskellString
    -- * The vocabulary
  , vocabulary
    -- * The surface syntax
  , Expr (..)
  , SrcPos
  , parseExpr
  , elaborate
  ) where

import qualified Data.List                  as List
import qualified Data.List.NonEmpty         as NE
import qualified RIO.Char                   as C
import qualified RIO.Text                   as T
import           RIO                        hiding (many, try, (&))
import           Text.Megaparsec            hiding (Pos)
import           Text.Megaparsec.Char       (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

import           Hsed.Spec.Common           (Finding, Severity (..), errataSetName,
                                             finding, parseErrataSet)
import           Hsed.Spec.DSL
import           Hsed.Spec.Shape


-- * Payloads

-- | What an annotation says: a binding, or modifiers of the rule default the
--   anchor would get anyway. The modifiers are listed innermost first, so that
--   applying them in order to the default gives the binding.
data Payload
  = Full SomeBinding
  | Modifiers [Modifier]
  deriving stock (Eq, Show)

parsePayload :: Text -> Either Finding Payload
parsePayload t = parseExpr t >>= elaborate

printPayload :: Payload -> Text
printPayload (Full sb)       = printSomeBinding sb
printPayload (Modifiers ms)  = printModifiers ms


-- * The surface syntax

-- | Line and column, 1-based.
type SrcPos = (Int, Int)

-- | The surface syntax, before names are resolved.
data Expr
  = EApp SrcPos Text [Expr]   -- ^ a name applied to arguments; a bare name has none
  | EAnd [Expr]            -- ^ @a & b & c@, two or more
  | EInt SrcPos Integer
  | EStr SrcPos Text
  | EList SrcPos [Expr]
  | EPair Expr Expr        -- ^ @(0, "KeyChangeZone")@
  | EArrow Expr Expr       -- ^ @"Fractoin" ~> "Fraction"@
  | EShape SrcPos Text        -- ^ @\@'Codes@
  | ESect SrcPos Text      -- ^ @§ "5.1.3"@
  deriving stock (Eq, Show)

type Parser = Parsec Void Text

parseExpr :: Text -> Either Finding Expr
parseExpr = first syntaxFinding . parse (sc *> pChain <* eof) ""

syntaxFinding :: ParseErrorBundle Text Void -> Finding
syntaxFinding bundle = finding Error "E-SYNTAX" (showPos (pstateSourcePos pst) <> ": " <> message)
  where
    e        = NE.head (bundleErrors bundle)
    (_, pst) = reachOffset (errorOffset e) (bundlePosState bundle)
    message  = T.strip (T.pack (parseErrorTextPretty e))

showPos :: SourcePos -> Text
showPos p = tshow (unPos (sourceLine p)) <> ":" <> tshow (unPos (sourceColumn p))

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pos :: Parser SrcPos
pos = (\p -> (unPos (sourceLine p), unPos (sourceColumn p))) <$> getSourcePos

pName :: Parser Text
pName = lexeme (T.pack <$> ((:) <$> satisfy C.isLower <*> many (satisfy identChar))) <?> "name"
  where identChar c = C.isAlphaNum c || c == '_' || c == '\''

pShapeName :: Parser Text
pShapeName = lexeme (T.pack <$> ((:) <$> satisfy C.isUpper <*> many (satisfy C.isAlphaNum))) <?> "shape"

pInt :: Parser Integer
pInt = lexeme (try (char '0' *> (char 'x' <|> char 'X') *> L.hexadecimal) <|> L.decimal) <?> "integer"

pStr :: Parser Text
pStr = lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))) <?> "string"

-- | @a & b & c@ at the top level or inside parentheses.
pChain :: Parser Expr
pChain = toChain <$> pExpr `sepBy1` symbol "&"
  where
    toChain [e] = e
    toChain es  = EAnd es

pExpr :: Parser Expr
pExpr = do
  a <- pApp
  option a (EArrow a <$> (symbol "~>" *> pApp))

pApp :: Parser Expr
pApp = do
  p <- pos
  (EApp p <$> pName <*> many pAtom) <|> pAtom

pAtom :: Parser Expr
pAtom = choice
  [ EInt <$> pos <*> pInt
  , EStr <$> pos <*> pStr
  , EList <$> pos <*> between (symbol "[") (symbol "]") (pChain `sepBy` symbol ",")
  , pParen
  , EShape <$> (pos <* symbol "@'") <*> pShapeName
  , ESect <$> (pos <* symbol "§") <*> pStr
  , (\p n -> EApp p n []) <$> pos <*> pName
  ]

pParen :: Parser Expr
pParen = do
  _ <- symbol "("
  e <- pChain
  (EPair e <$> (symbol "," *> pChain) <* symbol ")") <|> (e <$ symbol ")")

posOf :: Expr -> Maybe SrcPos
posOf = \case
  EApp p _ _   -> Just p
  EAnd (e : _) -> posOf e
  EAnd []      -> Nothing
  EInt p _     -> Just p
  EStr p _     -> Just p
  EList p _    -> Just p
  EPair e _    -> posOf e
  EArrow e _   -> posOf e
  EShape p _   -> Just p
  ESect p _    -> Just p


-- * Elaboration

-- | A chain so far: bare modifiers, or a binding they apply to.
data Elab
  = EMods [Modifier]
  | EBind SomeBinding

-- | Resolve the names of a parsed payload against the vocabulary of "Hsed.Spec.DSL".
elaborate :: Expr -> Either Finding Payload
elaborate e = chain e <&> \case
  EMods ms -> Modifiers ms
  EBind sb -> Full sb

grammarError :: Maybe SrcPos -> Text -> Finding
grammarError p msg = finding Error "E-GRAMMAR" (maybe "" (\(l, c) -> tshow l <> ":" <> tshow c <> ": ") p <> msg)

chain :: Expr -> Either Finding Elab
chain e = case e of
  EAnd es -> EBind <$> joinRules es
  EApp p n args
    | Just arity <- modifierArity n -> do
        let (own, rest) = List.splitAt arity args
        m <- modifierOf p n own
        operand rest >>= maybe (Right (EMods [m])) (chain >=> apply m)
    | n == "doc" || n == "rule" -> EBind <$> joinRules [e]
    | otherwise -> binding p n args
  _ -> Left (grammarError (posOf e) "expected a binding or a modifier")
  where
    apply m (EMods ms) = Right (EMods (ms <> [m]))
    apply m (EBind sb) = EBind <$> applyModifierSome m sb
    -- what follows a modifier's own arguments: nothing, a name with the rest
    -- of the line as its arguments, or one parenthesised expression
    operand :: [Expr] -> Either Finding (Maybe Expr)
    operand []                        = Right Nothing
    operand (EApp p n [] : more)      = Right (Just (EApp p n more))
    operand [e']                      = Right (Just e')
    operand (e' : _)                  = Left (grammarError (posOf e') "expected the binding the modifier applies to")

-- | A binding name with its own arguments and, after them, any parenthesised
--   modifier chains written after it (@enumValues (cols 0 1)@).
binding :: SrcPos -> Text -> [Expr] -> Either Finding Elab
binding p n args = case bindingArity n of
  Nothing    -> Left (grammarError (Just p) ("unknown name " <> n))
  Just arity -> do
    let (own, post) = List.splitAt arity args
    sb <- construct p n own
    EBind <$> foldM applyPost sb post
  where
    applyPost sb e = chain e >>= \case
      EMods ms -> foldM (flip applyModifierSome) sb ms
      EBind _  -> Left (grammarError (posOf e) ("a parenthesised argument of " <> n <> " must be a modifier chain"))

-- | @doc@ and @rule@ tokens joined by @&@: on the document when a @doc@ token is
--   among them, else on a heading.
joinRules :: [Expr] -> Either Finding SomeBinding
joinRules es
  | any hasDoc es = SomeBinding <$> (traverse (ruleTerm RDoc) es >>= joined)
  | otherwise     = SomeBinding <$> (traverse (ruleTerm RHeading) es >>= joined)
  where
    hasDoc (EApp _ "doc" _) = True
    hasDoc (EAnd es')       = any hasDoc es'
    hasDoc _                = False

-- | Left-associated, as @&@ is.
joined :: RuleBearing t => [Binding t] -> Either Finding (Binding t)
joined (b : bs) = Right (foldl' Also b bs)
joined []       = Left (grammarError Nothing "nothing to join")

-- | The two shapes that carry rule tokens.
data RuleShape (t :: Shape) where
  RDoc     :: RuleShape 'Doc
  RHeading :: RuleShape 'Heading

ruleTerm :: RuleShape t -> Expr -> Either Finding (Binding t)
ruleTerm rs e = case e of
  EAnd es -> case rs of
    RDoc     -> traverse (ruleTerm rs) es >>= joined
    RHeading -> traverse (ruleTerm rs) es >>= joined
  EApp p "doc" [EApp _ n []] -> case rs of
    RDoc     -> maybe (Left (grammarError (Just p) ("unknown specification " <> n))) (Right . DocB) (specByName n)
    RHeading -> Left (grammarError (Just p) "doc belongs on the document anchor")
  EApp p "rule" [EApp _ n []]
    | RuleName n `elem` knownRules -> case rs of
        RDoc     -> Right (RuleB (RuleName n))
        RHeading -> Right (RuleB (RuleName n))
    | otherwise -> Left (grammarError (Just p) ("unknown rule " <> n))
  EApp p "doc" _  -> Left (grammarError (Just p) "doc takes one name: doc core201")
  EApp p "rule" _ -> Left (grammarError (Just p) "rule takes one name: rule typeRow")
  _               -> Left (grammarError (posOf e) "only doc and rule tokens join with &")

modifierArity :: Text -> Maybe Int
modifierArity n = lookup n modifierArities

modifierArities :: [(Text, Int)]
modifierArities =
  [ ("erratum", 2), ("cols", 2), ("rangeWords", 1), ("enumInDescription", 0), ("expand", 0), ("as", 1)
  , ("waive", 3) ]

-- | Every name a payload may start with: @doc@, @rule@, the bindings and the
--   modifiers. @tools/extract/hsed_annot.py@ keeps the anchors each may sit on,
--   for annotate.py to refuse a mismatch before hsedc sees it; a test keeps the
--   two lists equal.
vocabulary :: [Text]
vocabulary = ["doc", "rule"] <> map fst bindingArities <> map fst modifierArities

modifierOf :: SrcPos -> Text -> [Expr] -> Either Finding Modifier
modifierOf p n own = case (n, own) of
  ("erratum", [EApp sp setName [], EList _ ps]) -> case parseErrataSet setName of
    Just Scott     -> Erratum Scott <$> traverse arrow ps
    Just Published -> Left (finding Error "E-ERRATUM-PUBLISHED" (tshow (fst sp) <> ":" <> tshow (snd sp)
                        <> ": erratum published: the published set is the text as printed and takes no corrections; write erratum scott"))
    Nothing        -> Left (grammarError (Just sp) ("unknown errata set " <> setName))
  ("waive", [EStr sp c, EStr _ subject, EStr _ reason])
    | c `elem` waivableChecks             -> Right (Waive c subject reason)
    | otherwise                           -> Left (grammarError (Just sp) ("waive: hsedc waives only " <> T.intercalate ", " waivableChecks <> ", not " <> c))
  ("cols", [EInt _ a, EInt _ b])          -> Right (Cols (fromInteger a) (fromInteger b))
  ("rangeWords", [EList _ ws])            -> RangeWords <$> traverse str ws
  ("enumInDescription", [])               -> Right EnumInDescription
  ("expand", [])                          -> Right Expand
  ("as", [EShape sp s])                   -> maybe (Left (grammarError (Just sp) ("unknown shape " <> s))) (Right . As) (readShape s)
  _                                       -> Left (grammarError (Just p) (n <> ": expected " <> usage))
  where
    usage = fromMaybe "" (lookup n
      [ ("erratum", "erratum scott [\"old\" ~> \"new\", ...]"), ("cols", "cols A B")
      , ("rangeWords", "rangeWords [\"to\", ...]"), ("enumInDescription", "no arguments")
      , ("expand", "no arguments"), ("as", "as @'Shape")
      , ("waive", "waive \"C-UID-UNIQUE\" \"subject\" \"reason\"") ])

-- | The binding names and how many arguments each takes.
bindingArity :: Text -> Maybe Int
bindingArity n = lookup n bindingArities

bindingArities :: [(Text, Int)]
bindingArities =
  [ ("typeRow", 0), ("enumValues", 0), ("tableDesc", 0), ("columnsAddition", 0), ("tableUIDs", 0)
  , ("uidTable", 0), ("statusCodes", 0), ("codes", 0), ("fieldValue", 0), ("fieldType", 0)
  , ("byteFieldValue", 0), ("byteGrid", 0), ("layout", 1), ("feature", 0), ("featureNamed", 1)
  , ("bits", 2), ("constValues", 0), ("ranges", 0), ("preconfig", 0), ("tokens", 0)
  , ("stateTable", 1), ("propsNames", 0), ("propsInitial", 0), ("propsReq", 0), ("legend", 0)
  , ("decision", 0), ("sig", 0), ("prose", 0), ("behaviour", 1), ("fails", 1), ("law", 1)
  , ("refines", 1), ("narrows", 1), ("replaces", 1), ("extends", 1) ]

construct :: SrcPos -> Text -> [Expr] -> Either Finding SomeBinding
construct p n own = case (n, own) of
  ("typeRow", [])              -> fixed TypeRowB
  ("enumValues", [])           -> fixed EnumValuesB
  ("tableDesc", [])            -> fixed TableDescB
  ("columnsAddition", [])      -> fixed ColumnsAdditionB
  ("tableUIDs", [])            -> fixed TableUIDsB
  ("uidTable", [])             -> fixed UidTableB
  ("statusCodes", [])          -> fixed StatusCodesB
  ("codes", [])                -> fixed CodesB
  ("fieldValue", [])           -> fixed FieldValueB
  ("fieldType", [])            -> fixed FieldTypeB
  ("byteFieldValue", [])       -> fixed ByteFieldValueB
  ("byteGrid", [])             -> fixed (LayoutB Nothing)
  ("layout", [EStr _ s])       -> fixed (LayoutB (Just s))
  ("feature", [])              -> fixed (FeatureB Nothing)
  ("featureNamed", [EStr _ s]) -> fixed (FeatureB (Just s))
  ("bits", [EInt _ k, EList _ ps]) -> fixed . Bits (fromInteger k) =<< traverse bit ps
  ("constValues", [])          -> fixed ConstValuesB
  ("ranges", [])               -> fixed RangesB
  ("preconfig", [])            -> fixed PreconfigB
  ("tokens", [])               -> fixed TokensB
  ("stateTable", [EStr _ q])   -> fixed (StateTableB q)
  ("propsNames", [])           -> fixed PropsNames
  ("propsInitial", [])         -> fixed PropsInitial
  ("propsReq", [])             -> fixed PropsReqB
  ("legend", [])               -> fixed LegendB
  ("decision", [])             -> fixed DecisionB
  ("sig", [])                  -> fixed SigB
  ("prose", [])                -> fixed ProseB
  ("behaviour", [EStr _ q])    -> fixed (Behaviour q)
  ("fails", [EStr _ q])        -> fixed (Fails q)
  ("law", [EStr _ q])          -> fixed (Law q)
  ("refines", [r])             -> (\r' -> PolyBinding (Refines r')) <$> ref r
  ("narrows", [r])             -> (\r' -> PolyBinding (Narrows r')) <$> ref r
  ("replaces", [EList _ rs])   -> (\rs' -> PolyBinding (Replaces rs')) <$> traverse ref rs
  ("extends", [r])             -> fixed . Extends =<< ref r
  _                            -> Left (grammarError (Just p) (n <> ": expected " <> usage))
  where
    fixed :: Binding s -> Either Finding SomeBinding
    fixed = Right . SomeBinding
    usage = fromMaybe "no arguments" (lookup n
      [ ("layout", "layout \"TypeName\""), ("featureNamed", "featureNamed \"TypeName\"")
      , ("bits", "bits BYTE [(BIT, \"Field\"), ...]"), ("stateTable", "stateTable \"Module.name\"")
      , ("behaviour", "behaviour \"Module.name\""), ("fails", "fails \"Module.name\"")
      , ("law", "law \"Module.name\""), ("refines", "refines (core N)")
      , ("narrows", "narrows (core N)"), ("replaces", "replaces [core N, ...]")
      , ("extends", "extends (core N)") ])
    bit (EPair (EInt _ i) (EStr _ s)) = Right (fromInteger i, s)
    bit e                         = Left (grammarError (posOf e) "expected (BIT, \"Field\")")

ref :: Expr -> Either Finding Ref
ref = \case
  EApp _ "core" [EInt _ n]    -> Right (Core (fromInteger n))
  EApp _ "opal" [EInt _ n]    -> Right (Opal (fromInteger n))
  EApp _ "section" [EStr _ s] -> Right (Section s)
  ESect _ s                   -> Right (Section s)
  e                           -> Left (grammarError (posOf e) "expected a reference: core N, opal N or section \"x.y\"")

arrow :: Expr -> Either Finding (Text, Text)
arrow (EArrow a b) = (,) <$> word a <*> word b
arrow e            = Left (grammarError (posOf e) "expected \"old\" ~> \"new\"")

-- | A string literal, or a bare name standing for one.
word :: Expr -> Either Finding Text
word (EStr _ s)    = Right s
word (EApp _ n []) = Right n
word e             = Left (grammarError (posOf e) "expected a string")

str :: Expr -> Either Finding Text
str (EStr _ s) = Right s
str e        = Left (grammarError (posOf e) "expected a string")


-- * Printing

-- | A string literal as Haskell and the grammar both read it.
haskellString :: Text -> Text
haskellString = T.pack . show . T.unpack

list :: [Text] -> Text
list xs = "[" <> T.intercalate ", " xs <> "]"

printRef :: Ref -> Text
printRef (Core n)    = "core " <> tshow n
printRef (Opal n)    = "opal " <> tshow n
printRef (Section s) = "section " <> haskellString s

printModifier :: Modifier -> Text
printModifier = \case
  Erratum e ps      -> "erratum " <> errataSetName e <> " " <> list [ haskellString a <> " ~> " <> haskellString b | (a, b) <- ps ]
  Cols a b          -> "cols " <> tshow a <> " " <> tshow b
  RangeWords ws     -> "rangeWords " <> list (map haskellString ws)
  EnumInDescription -> "enumInDescription"
  Expand            -> "expand"
  As sh             -> "as @'" <> shapeName sh
  Waive c sj r      -> "waive " <> T.unwords (map haskellString [c, sj, r])

-- | A bare modifier chain, innermost first: @[Cols 0 1, RangeWords ["to"]]@ is @rangeWords ["to"] (cols 0 1)@.
printModifiers :: [Modifier] -> Text
printModifiers = \case
  []     -> ""
  m : ms -> foldl' (\inner m' -> printModifier m' <> " " <> wrap inner) (printModifier m) ms
  where wrap t = if T.any C.isSpace t then "(" <> t <> ")" else t

printSomeBinding :: SomeBinding -> Text
printSomeBinding (SomeBinding b) = printBinding b
printSomeBinding (PolyBinding b) = printBinding (b :: Binding 'Prose)

-- | The comment text of a binding; also valid Haskell over "Hsed.Spec.DSL".
printBinding :: Binding s -> Text
printBinding = \case
  DocB sid                -> "doc " <> specName sid
  RuleB r                 -> "rule " <> ruleNameText r
  TypeRowB                -> "typeRow"
  EnumValuesB             -> "enumValues"
  TableDescB              -> "tableDesc"
  ColumnsAdditionB        -> "columnsAddition"
  TableUIDsB              -> "tableUIDs"
  UidTableB               -> "uidTable"
  StatusCodesB            -> "statusCodes"
  CodesB                  -> "codes"
  FieldValueB             -> "fieldValue"
  FieldTypeB              -> "fieldType"
  ByteFieldValueB         -> "byteFieldValue"
  LayoutB Nothing         -> "byteGrid"
  LayoutB (Just s)        -> "layout " <> haskellString s
  FeatureB Nothing        -> "feature"
  FeatureB (Just s)       -> "featureNamed " <> haskellString s
  Bits k fs               -> "bits " <> tshow k <> " " <> list [ "(" <> tshow i <> ", " <> haskellString s <> ")" | (i, s) <- fs ]
  ConstValuesB            -> "constValues"
  RangesB                 -> "ranges"
  PreconfigB              -> "preconfig"
  TokensB                 -> "tokens"
  StateTableB q           -> "stateTable " <> haskellString q
  PropsNames              -> "propsNames"
  PropsInitial            -> "propsInitial"
  PropsReqB               -> "propsReq"
  LegendB                 -> "legend"
  DecisionB               -> "decision"
  SigB                    -> "sig"
  ProseB                  -> "prose"
  Behaviour q             -> "behaviour " <> haskellString q
  Fails q                 -> "fails " <> haskellString q
  Law q                   -> "law " <> haskellString q
  Refines r               -> "refines (" <> printRef r <> ")"
  Narrows r               -> "narrows (" <> printRef r <> ")"
  Replaces rs             -> "replaces " <> list (map printRef rs)
  Extends r               -> "extends (" <> printRef r <> ")"
  Also a b                -> printBinding a <> " & " <> rightOperand b
  WithErratum e ps b      -> printModifier (Erratum e ps) <> " " <> operand b
  WithWaiver c sj r b     -> printModifier (Waive c sj r) <> " " <> operand b
  WithCols a c b          -> printModifier (Cols a c) <> " " <> operand b
  WithRangeWords ws b     -> printModifier (RangeWords ws) <> " " <> operand b
  WithEnumInDescription b -> "enumInDescription " <> operand b
  WithExpand b            -> "expand " <> operand b
  WithAs b                -> printModifier (As (bindingShape b)) <> " " <> operand b
  where
    operand b
      | isAtomicBinding b = printBinding b
      | otherwise         = "(" <> printBinding b <> ")"
    -- & is left-associative; a right operand that is itself a join keeps its parentheses
    rightOperand b@(Also _ _) = "(" <> printBinding b <> ")"
    rightOperand b            = printBinding b
