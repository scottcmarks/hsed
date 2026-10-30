{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.Sig
Description : The sig rule: every method signature block, parsed and joined to its method UID
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E9. The method signature blocks of the IR
(@sigs@, docs/ir-v1.md) are signature units: @rule sig@ on the @doc@ anchor
binds all of them, and the rule reads them as 'BoundSig's (it never fires on
a table). A block is an invocation, @Invoker.Method [ parameters ]@, one head
per invoking ID the method takes (@TableUID.Get [@ then @ObjectUID.Get [@),
then, unless nothing answers it, @=>@ and the response: a result list @[ …
]@, or, for a Session Manager method, an invocation the TPer answers with
(@SMUID.SyncSession [ … ]@), whose list may be only a reference to another
block (@[ see SyncSession definition in 5.2.3.2 ]@). A parameter is @Name :
type@ (required, positional) or @Name = type@ (optional, Named by its 0-based
index among the optional ones); the required ones come first (Core 3.2.4.1).
The method a block defines is the one its section's heading names: the
invocation's, or the response's when the block defines the answer (5.2.3.2
SyncSession answers StartSession).

Checks, each with the code its findings carry:

* @C-SIG-PARSE@: the block parses (an Error otherwise); a parameter list
  not closed before @=>@ is read as closed there, a Warning (5.3.3.17
  GetPackage); a comma before the closing bracket is the Note
  @N-SIG-NORMALISED@ (5.3.3.18 SetPackage);
* @C-SIG-METHOD@: the method is joined by name to exactly one row of the UID
  tables that list methods (their name column @Method Name@: the MethodID UIDs
  and the Session Manager Method UIDs, "Hsed.Spec.Rules.UidTable"); a block
  no row names is a Warning at the block (the PDF names the method one way in
  its signature and another, or not at all, in the tables), two rows an
  Error; a method row no block defines is a Note at its row.

Compiled into 'methodsModule' ("Hsed.Core.Method" is the runtime): per block
a record named after the method, one field per parameter (@getCellblock ::
Value@; an optional one @Maybe Value@), an @instance MethodSignature@ with the
signature as printed, and, when joined, an @instance Method@ whose
@methodUID@ is the joined row's generated UID; a block whose heads name
another method than the one it defines (SyncSession's, headed
@SMUID.StartSession@) says which in its provenance line. An invoking ID the
heads name that a row of the UID tables without names describes (@SMUID@,
@ThisSP@ in the Special Purpose UIDs' Purpose column, word for word) is given
as a constant, @uSMUID :: UID@, with that row's provenance, in the uidTable
rule's module beside the other UIDs ('uidsModule').
-}
module Hsed.Spec.Rules.Sig
  ( sigRule
  , methodsModule
    -- * Reading the bound blocks
  , SigParam (..)
  , ParamList (..)
  , ResponseRead (..)
  , SigRead (..)
  , sigReads
  , methodRows
  , invokerRows
    -- * Parsing a block
  , Part (..)
  , parseBlock
  ) where

import qualified Data.Text                as DT
import qualified RIO.Char                 as C
import qualified RIO.List                 as L
import qualified RIO.Text                 as T
import           RIO

import           Hsed.Spec.Anchor         (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common         (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.DSL            (SomeBinding (..), sig)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (HeadingIR (..), SigIR (..), SpecIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Readers.Text   (lowerIdent)
import           Hsed.Spec.Rules.UidTable (UidRow (..), UidValue (..), rowPage, rowRef, rowText, uidHeaders, uidRows, uidsModule)


-- | The rule.
sigRule :: RuleDef
sigRule = RuleDef
  { ruleName    = RuleName "sig"
  , bindsTo     = SomeBinding sig
  , exemplar    = (SpecId "core-2.01", SigId "5.3.3.6")
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ _ -> False
  , ruleIdent   = const Nothing
  , ruleModule  = methodsModule
  , ruleCodes   =
      [ CodeDef "C-SIG-PARSE"      [Error, Warning]       (Defect "a method signature does not parse as printed")
      , CodeDef "C-SIG-METHOD"     [Error, Warning, Note] (Defect "a method signature and the UID tables disagree: a signature no UID row names, or a method UID with no signature")
      , CodeDef "N-SIG-NORMALISED" [Note]                 (AsPrinted "a method signature read with its punctuation normalised") ]
  , ruleChecks  = \ctx _ -> checks ctx
  , ruleCompile = \ctx _ -> compile ctx
  }

-- | The runtime module of the method records: the @Method@ and @MethodSignature@ classes they
--   instantiate, @Signature@, @Param@, @SigResponse@, @Value@.
methodModule :: Text
methodModule = "Hsed.Core.Method"

-- | @Hsed.Gen.\<Spec\>.Methods@.
methodsModule :: Gen.GenModule
methodsModule = Gen.GenModule
  { Gen.leaf        = "Methods"
  , Gen.description = "Methods: a record per method signature, its signature as printed and its UID"
  , Gen.notes       = [ "The parameters are untyped (Value) and their types text: the Type objects they name are"
                      , "TypeDef values, not yet Haskell types. The records are named after their methods (Get,"
                      , "Set, Next), names other modules export too: import this module qualified." ] }


-- * Parsing a block

-- | One parameter as printed: @Cellblock : cell_block@.
data SigParam = SigParam
  { spName :: !Text
  , spType :: !Text     -- ^ spacing collapsed
  }
  deriving (Eq, Show)

-- | A bracketed list: parameters (required, then optional), or a reference to
--   the block that gives them (@see SyncSession definition in 5.2.3.2@: the
--   method and the section).
data ParamList
  = Params ![SigParam] ![SigParam]
  | SeeRef !Text !(Maybe Text)
  deriving (Eq, Show)

-- | An invocation, or a response: its heads (invoking ID and method; a result
--   list has none) and its list.
data Part = Part
  { pHeads :: ![(Text, Text)]
  , pList  :: !ParamList
  }
  deriving (Eq, Show)

-- | A block's invocation and its response, if any, with what reading them
--   found (each a severity, a code and a message).
parseBlock :: Text -> Either Text ((Part, Maybe Part), [(Severity, Text, Text)])
parseBlock text = do
  let (callText, rest) = DT.breakOn "=>" text
      responseText = T.drop 2 rest
  (call, callNotes) <- part "the invocation" callText
  response <- if T.null rest then pure Nothing else Just <$> part "the response" responseText
  pure ((call, fst <$> response), callNotes <> maybe [] snd response)
  where
    part what t = do
      let (heads, afterHeads) = takeHeads [] (T.stripStart t)
      body <- maybe (Left (what <> " has no [")) Right (T.stripPrefix "[" afterHeads)
      let (content, closed, after) = bracketed body
      unless (T.null (T.strip after)) (Left (what <> " has text after its ]: " <> tshow (T.strip after)))
      (list, notes) <- paramList what content
      let unclosed = [ (Warning, "C-SIG-PARSE", what <> "'s parameter list is not closed before =>: read as closed there") | not closed ]
      pure (Part heads list, unclosed <> notes)

    -- the heads: Invoker.Method, each but the last maybe with its own [
    takeHeads acc t = case dotted t of
      Just (h, t') ->
        let t'' = T.stripStart t'
        in case T.stripPrefix "[" t'' of
             Just t3 | isJust (dotted (T.stripStart t3)) -> takeHeads (h : acc) (T.stripStart t3)
             _                                          -> takeHeads (h : acc) t''
      Nothing -> (reverse acc, t)
    dotted t = do
      let (a, t1) = T.span identChar t
      t2 <- T.stripPrefix "." t1
      let (b, t3) = T.span identChar t2
      guard (not (T.null a) && not (T.null b))
      pure ((a, b), t3)

-- | The text up to the bracket that closes an opened one, whether it was found,
--   and the text after it.
bracketed :: Text -> (Text, Bool, Text)
bracketed t = go (0 :: Int) 0 (T.unpack t)
  where
    go depth n = \case
      [] -> (t, False, "")
      c : cs
        | c `elem` ("[{" :: String) -> go (depth + 1) (n + 1) cs
        | c `elem` ("]}" :: String), depth == 0 -> (T.take n t, True, T.drop (n + 1) t)
        | c `elem` ("]}" :: String) -> go (depth - 1) (n + 1) cs
        | otherwise -> go depth (n + 1) cs

-- | A list's content: empty, a reference, or parameters.
paramList :: Text -> Text -> Either Text (ParamList, [(Severity, Text, Text)])
paramList what content
  | T.null stripped = Right (Params [] [], [])
  | Just ref <- seeRef = Right (ref, [])
  | otherwise = do
      let pieces = map T.strip (splitTop stripped)
          (trailing, given) = case reverse pieces of
            "" : more -> (True, reverse more)
            _         -> (False, pieces)
      params <- traverse param given
      let (req, rest) = L.span fst params
      case filter fst rest of
        (_, p) : _ -> Left (what <> ": the required parameter " <> spName p <> " follows an optional one (3.2.4.1)")
        [] -> pure ()
      let names = map (spName . snd) params
      when (L.nub names /= names) (Left (what <> " names a parameter twice"))
      pure ( Params (map snd req) (map snd rest)
           , [ (Note, "N-SIG-NORMALISED", what <> "'s parameter list ends with a comma before ]: read without it") | trailing ] )
  where
    stripped = T.strip (collapse content)
    seeRef = case T.words stripped of
      w : m : more | T.toLower w == "see" ->
        Just (SeeRef m (L.lastMaybe [ s | s <- map (T.dropWhileEnd (not . C.isDigit)) more, isSection s ]))
      _ -> Nothing
    isSection s = not (T.null s) && T.all (\c -> C.isDigit c || c == '.') s && T.any C.isDigit s
    param p = case T.span identChar p of
      (n, after) | not (T.null n), Just (sep, ty) <- T.uncons (T.stripStart after), sep `elem` (":=" :: String)
                 , not (T.null (T.strip ty)) -> Right (sep == ':', SigParam n (T.strip ty))
      _ -> Left (what <> ": " <> tshow p <> " is not Name : type or Name = type")

-- | Split at the commas outside brackets and braces.
splitTop :: Text -> [Text]
splitTop t = go (0 :: Int) [] [] (T.unpack t)
  where
    go _ cur acc [] = reverse (T.pack (reverse cur) : acc)
    go d cur acc (c : cs)
      | c == ',' && d == 0          = go d [] (T.pack (reverse cur) : acc) cs
      | c `elem` ("[{" :: String)   = go (d + 1) (c : cur) acc cs
      | c `elem` ("]}" :: String)   = go (max 0 (d - 1)) (c : cur) acc cs
      | otherwise                   = go d (c : cur) acc cs

identChar :: Char -> Bool
identChar c = C.isAlphaNum c || c == '_'

collapse :: Text -> Text
collapse = T.unwords . T.words


-- * Reading the bound blocks

-- | What answers a method, as the rule reads it (the runtime's is
--   "Hsed.Core.Method".@SigResponse@).
data ResponseRead
  = SResults ![SigParam] ![SigParam]
  | SInvocation !Text ![SigParam] ![SigParam]
  | SInvocationOf !Text
  | SNoResponse
  deriving (Eq, Show)

-- | One block read: the method it defines and its signature.
data SigRead = SigRead
  { srBound    :: !BoundSig
  , srMethod   :: !Text
  , srInvokers :: ![Text]
  , srRequired :: ![SigParam]
  , srOptional :: ![SigParam]
  , srResponse :: !ResponseRead
  , srAnswers  :: !(Maybe Text)
  , srFindings :: ![Finding]       -- ^ what reading it found besides an Error (located)
  }

-- | Every bound block read, or the C-SIG-PARSE Error that it does not read.
sigReads :: Context -> [Either Finding SigRead]
sigReads ctx = map (readSig ctx) (boundSigs (ruleShape sigRule) ctx)

readSig :: Context -> BoundSig -> Either Finding SigRead
readSig ctx bs = first (atSig ctx bs . finding Error "C-SIG-PARSE") $ do
  ((call, response), notes) <- parseBlock s.text
  let callMethods = L.nub (map snd (pHeads call))
      respMethod = response >>= \r -> snd <$> listToMaybe (pHeads r)
      named m = m `elem` headingWords
  unless (map (\(i, m) -> i <> "." <> m) (pHeads call) == s.heads)
    (Left ("the heads read are " <> tshow (pHeads call) <> ", the IR's " <> tshow s.heads))
  callMethod <- case callMethods of
    [m] -> Right m
    ms  -> Left ("the heads name " <> tshow (length ms) <> " methods: " <> T.intercalate ", " ms)
  let located = [ atSig ctx bs (finding sev code msg) | (sev, code, msg) <- notes ]
      invokers p = L.nub (map fst (pHeads p))
  if named callMethod
    then do
      (req, opt) <- params "the invocation" (pList call)
      resp <- case response of
        Nothing -> Right SNoResponse
        Just r -> case (pHeads r, pList r) of
          ([], Params a b)        -> Right (SResults a b)
          ([], SeeRef m _)        -> Left ("the result list is only a reference to " <> m)
          ((_, m) : _, Params a b) -> Right (SInvocation m a b)
          ((_, m) : _, SeeRef _ _) -> Right (SInvocationOf m)
      Right (SigRead bs callMethod (invokers call) req opt resp Nothing located)
    else case (response, respMethod) of
      (Just r, Just m) | named m -> do
        (req, opt) <- params "the response" (pList r)
        Right (SigRead bs m (invokers r) req opt SNoResponse (Just callMethod) located)
      _ -> Left ("the heading " <> tshow heading <> " names neither " <> callMethod <> maybe "" (" nor " <>) respMethod)
  where
    s = bsSig bs
    heading = fromMaybe "" (listToMaybe [ h.title | h <- (cxIR ctx).headings, h.section == Just s.section ])
    headingWords = T.words (T.map (\c -> if identChar c then c else ' ') heading)
    params what = \case
      Params a b -> Right (a, b)
      SeeRef m _ -> Left (what <> "'s parameters are only a reference to " <> m)

-- | The rows of the UID tables that list methods (their name column is
--   @Method Name@), each with its method name.
methodRows :: Context -> [(Text, UidRow)]
methodRows ctx = [ (n, r) | r <- uidRows ctx, Just n <- [urName r], isMethodTable r ]
  where isMethodTable r = "methodname" `elem` snd (uidHeaders (urBound r))

-- | The rows of the UID tables with no names (a Purpose describes them) that
--   an invoking ID names word for word: @ThisSP@ is row 2's @"This SP"@.
invokerRows :: Context -> Text -> [UidRow]
invokerRows ctx invoker =
  [ r | r <- uidRows ctx, isNothing (urIdent r), isNothing (urName r), Exact _ <- [urValue r], invoker `spelledIn` rowText r ]
  where
    spelledIn i t = let ws = T.words (T.map (\c -> if C.isAlphaNum c then c else ' ') t)
                    in or [ T.toLower (T.concat (take k (drop j ws))) == T.toLower i | j <- [0 .. length ws - 1], k <- [1 .. 3] ]


-- * Checks

-- | A block's join: the method rows that name its method.
joins :: Context -> SigRead -> [UidRow]
joins ctx sr = [ r | (n, r) <- methodRows ctx, n == srMethod sr, Exact _ <- [urValue r] ]

checks :: Context -> [Finding]
checks ctx = lefts reads <> concatMap srFindings rows <> concatMap joinCheck rows <> unsigned
  where
    reads = sigReads ctx
    rows  = rights reads
    tables = L.nub [ renderAnchorId (btAnchor (urBound r)) | (_, r) <- methodRows ctx ]
    joinCheck sr = case joins ctx sr of
      [_] -> []
      []  -> [ atSig ctx (srBound sr) (finding Warning "C-SIG-METHOD"
                 (srMethod sr <> " has no UID: no row of " <> T.intercalate " or " tables <> " names it")) ]
      rs  -> [ atSig ctx (srBound sr) (finding Error "C-SIG-METHOD"
                 (srMethod sr <> " is named by " <> T.intercalate " and " [ rowRef r | r <- rs ])) ]
    defined = map srMethod rows
    unsigned =
      [ locatedAt (cxSpec ctx) (renderAnchorId (btAnchor (urBound r))) (rowPage r)
          (finding Note "C-SIG-METHOD" (rowRef r <> ", " <> n <> ", has no signature block"))
      | not (null reads), (n, r) <- methodRows ctx, isJust (urIdent r), n `notElem` defined ]


-- * Declarations

compile :: Context -> [Emitted]
compile ctx = map (EmitIn uidsModule . invokerDecl ctx) invokers <> concat [ map Emit (methodDecls ctx sr (joins ctx sr)) | Right sr <- sigReads ctx ]
  where
    invokers = [ (i, r) | i <- L.nub (concat [ srInvokers sr | Right sr <- sigReads ctx ]), [r] <- [invokerRows ctx i] ]

-- | @uSMUID :: UID@, from the row that describes it.
invokerDecl :: Context -> (Text, UidRow) -> Gen.Decl
invokerDecl ctx (i, r) = (Gen.decl p n body)
  { Gen.declRank    = urRow r
  , Gen.declImports = [Gen.ImportSome Gen.uidModule ["UID", "uid"]] }
  where
    bt = urBound r
    p  = atRow ctx (urCells r) (boundProvenance ctx bt)
    n  = "u" <> i
    bs = case urValue r of
      Exact b -> b
      _       -> []
    body = [ Gen.provenanceComment p <> ", row " <> tshow (urRow r + 1) <> ": the invoking ID " <> i <> " of the signature heads"
           , n <> " :: UID", n <> " = " <> Gen.uidExpr bs ]

-- | A block's record and instances.
methodDecls :: Context -> SigRead -> [UidRow] -> [Gen.Decl]
methodDecls ctx sr joined = [ (Gen.decl p m body) { Gen.declRank = p.ordinal, Gen.declImports = imports } ]
  where
    bs   = srBound sr
    m    = srMethod sr
    -- a block headed by another method than the one it defines says which it defines
    p    = let p0 = sigProvenanceOf ctx bs
           in if m `elem` map (T.takeWhileEnd (/= '.')) (bsSig bs).heads then p0
              else p0 { Gen.caption = p0.caption <> " => " <> m }
    s    = bsSig bs
    doc  = Gen.provenanceComment p
    field prm = lowerIdent m <> capitalise (spName prm)
    capitalise t = maybe t (\(c, cs) -> T.cons (C.toUpper c) cs) (T.uncons t)
    fields = [ field prm <> " :: Value" | prm <- srRequired sr ] <> [ field prm <> " :: Maybe Value" | prm <- srOptional sr ]
    record = case fields of
      [] -> [ "data " <> m <> " = " <> m, "  deriving (Eq, Show)" ]
      f : fs -> [ "data " <> m <> " = " <> m, "  { " <> f ] <> map ("  , " <>) fs <> [ "  }", "  deriving (Eq, Show)" ]
    paramExpr prm = "Param " <> tshow (spName prm) <> " " <> tshow (spType prm)
    listExpr indent = \case
      []     -> ["[]"]
      [x]    -> ["[" <> x <> "]"]
      x : xs -> ("" : [ indent <> "[ " <> x ]) <> map ((indent <> ", ") <>) xs <> [indent <> "]"]
    fieldLines name items = case listExpr "        " items of
      [one]       -> [ "    , " <> name <> " = " <> one ]
      _ : several -> ("    , " <> name <> " =") : several
      []          -> []
    responseLines = case srResponse sr of
      SNoResponse          -> [ "    , sigResponse = NoResponse" ]
      SInvocationOf n      -> [ "    , sigResponse = InvocationOf " <> tshow n ]
      SResults a b         -> resp ("Results") a b
      SInvocation n a b    -> resp ("Invocation " <> tshow n) a b
    resp con a b
      | T.length oneLine <= 100 = [oneLine]
      | otherwise = [ "    , sigResponse = " <> con, "        " <> inline a, "        " <> inline b ]
      where oneLine = "    , sigResponse = " <> con <> " " <> inline a <> " " <> inline b
    inline xs = "[" <> T.intercalate ", " (map paramExpr xs) <> "]"
    sigLines =
      [ "  signature = Signature"
      , "    { sigMethod   = " <> tshow m
      , "    , sigInvokers = [" <> T.intercalate ", " (map tshow (srInvokers sr)) <> "]" ]
      <> fieldLines "sigRequired" (map paramExpr (srRequired sr))
      <> fieldLines "sigOptional" (map paramExpr (srOptional sr))
      <> responseLines
      <> [ "    , sigAnswers  = " <> maybe "Nothing" (\a -> "Just " <> tshow a) (srAnswers sr)
         , "    , sigSpecRef  = " <> tshow (Cm.specIdText (cxSpec ctx) <> ":" <> s.section)
         , "    }" ]
    accessors name xs = case [ field prm <> " m" | prm <- xs ] of
      [] -> [ "  " <> name <> " _ = []" ]
      a : more
        | T.length oneLine <= 100 -> [oneLine]
        | otherwise -> ("  " <> name <> " m =") : ("    [ " <> a) : map ("    , " <>) more <> ["    ]"]
        where oneLine = "  " <> name <> " m = [" <> T.intercalate ", " (a : more) <> "]"
    signatureInstance =
      [ "", "-- the signature of " <> m <> ", " <> renderAnchorId (bsAnchor bs) <> " (PDF " <> tshow (s.page + 1) <> ")"
      , "instance MethodSignature " <> m <> " where" ]
      <> sigLines
      <> accessors "requiredValues" (srRequired sr) <> accessors "optionalValues" (srOptional sr)
    methodInstance = case joined of
      [r] | Just i <- urIdent r ->
        [ "", "-- the UID of " <> m <> ", " <> rowRef r <> " (PDF " <> tshow (rowPage r) <> ")"
        , "instance Method " <> m <> " where", "  methodUID = " <> i ]
      _ -> [ "", "-- no instance Method " <> m <> ": no one row of the UID tables names it (C-SIG-METHOD)" ]
    body = [doc] <> record <> signatureInstance <> methodInstance
    usesValue = not (null fields)
    imports =
      [ Gen.ImportSome methodModule (["MethodSignature (..)", "SigResponse (..)", "Signature (..)"]
                                       <> [ "Param (..)" | usesParam ] <> [ "Value" | usesValue ]
                                       <> [ "Method (..)" | hasMethod ])
      , Gen.ImportSome "Prelude" ["Eq", "Maybe (..)", "Show"] ]
      <> [ Gen.ImportSome (Gen.moduleName (cxSpec ctx) uidsModule) [i] | [r] <- [joined], Just i <- [urIdent r] ]
    hasMethod = case joined of
      [r] -> isJust (urIdent r)
      _   -> False
    usesParam = not (null (srRequired sr <> srOptional sr)) || case srResponse sr of
      SResults a b      -> not (null (a <> b))
      SInvocation _ a b -> not (null (a <> b))
      _                 -> False
