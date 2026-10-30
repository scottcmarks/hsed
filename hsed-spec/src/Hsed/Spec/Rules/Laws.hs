{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.Laws
Description : The law reader: the sentences and state tables a comment binds to hand-written laws
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design example E10, section 4.3 and milestone M3. A @law "Module.name"@
comment on a list item or paragraph (Core 3.3.10.2.1 item 3a, PDF 82: @law
"Sync.ifSendWhileBusy"@) binds the sentence, a paragraph unit of the
'Hsed.Spec.Shape.Sentence' shape, to the hand-written law
@Hsed.Law.\<Module\>.\<name\>@: a @Hsed.Law.Law@ of the hsed package, which
names the sentence by its hash, says it as a QuickCheck property over the
simulator, and lists the Test Cases' cases that observe it. A
@stateTable "Module.name"@ comment binds a table to a law the same way (Core
Table 45, @stateTable "Sync.table45"@), read by "Hsed.Spec.Rules.StateTable".
The reader is reached only by the comments: its name, @laws@, is no rule token
(and not @law@, which the generic modules print as the binding's keyword), and
it never fires on a table. It reads the paragraph units ('boundParas', in the
anchors' natural order) and the state tables the stateTable reader generates
(those C-STATE-TABLE passes, in anchor order), sentences first: 'lawReads'.

Checks, each an Error at the unit (the paragraph, or the table):

* @C-LAW-BOUND@: a law comment's name is a qualified name, @Module.name@ (two
  or more parts, each an ASCII identifier, the modules capitalised and the
  last a variable that is no keyword), and no two units of the spec are bound
  to one name (each after the first is refused, citing the first). The other
  half runs when hsed builds: the generated module refers to each law by its
  qualified name, so that a name the code lacks fails to compile.
* @C-LAW-HASH@: a sentence has its hash in the IR (the sha256 of its words, 64
  hex digits), of which the hand-written law states the first 12. A changed
  sentence is re-anchoring's to catch (M6); hsed's @LawsSpec@ holds the two
  hashes equal.

Compiled into 'lawsModule' (@Hsed.Gen.\<Spec\>.Laws@; nothing where the spec
binds no law): @laws :: [LawRef]@, one entry per unit the checks pass, each
with the unit's anchor (@core-2.01:3.3.10.2.1#3a@), its PDF page (1-based), the
first 12 hex digits of its hash in the IR (a paragraph's, or a table's cells'
as its provenance line gives it), the name its comment gives and the law that
name refers to (@Hsed.Law.Sync.ifSendWhileBusy@, its module imported
qualified). A unit a check refuses is left out. The module is generated code
importing hand-written code, so it declares nothing the oracle compares (no
UID, no name map, no enumeration) and is compiled by hsed alone.
-}
module Hsed.Spec.Rules.Laws
  ( lawsRule
  , lawsModule
    -- * Reading the bound units
  , LawRead (..)
  , lawReads
  , qualifiedLaw
  , lawModuleName
  , lawReference
  ) where

import qualified Data.Text                 as DT
import qualified RIO.Char                  as C
import qualified RIO.List                  as L
import qualified RIO.Text                  as T
import           RIO

import           Hsed.Spec.Anchor          (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common          (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.DSL             (Binding (..), SomeBinding (..), law)
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.StateTable (StateTableRead (..), stateTableReads, stateTableRule)


-- | The reader: reached only by a @law@ comment (or, for a table, a @stateTable@ one).
lawsRule :: RuleDef
lawsRule = RuleDef
  { ruleName    = RuleName "laws"
  , bindsTo     = SomeBinding (law "")
  , exemplar    = (SpecId "core-2.01", ItemId "3.3.10.2.1" "3a")
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ _ -> False
  , ruleIdent   = const Nothing
  , ruleModule  = lawsModule
  , ruleCodes   =
      [ CodeDef "C-LAW-BOUND" [Error] Bookkeeping
      , CodeDef "C-LAW-HASH"  [Error] Bookkeeping ]
  , ruleChecks  = \ctx _ -> let reads = lawReads ctx in concatMap (\r -> map (locate r) (problems reads r)) reads
  , ruleCompile = \ctx _ -> compile ctx
  }

-- | The runtime module of a law and of what the generated module lists: @Law@, @LawRef@.
lawModule :: Text
lawModule = "Hsed.Law"

-- | @Hsed.Gen.\<Spec\>.Laws@.
lawsModule :: Gen.GenModule
lawsModule = Gen.GenModule
  { Gen.leaf        = "Laws"
  , Gen.description = "The normative sentences and state tables bound to hand-written laws"
  , Gen.notes       = [ "Each entry is a unit a comment binds to a law (a sentence by law, a state table by"
                      , "stateTable): its anchor, its PDF page, the first 12 hex digits of its hash in the IR,"
                      , "the name its comment gives, and the law of Hsed.Law.<Module> that name refers to, so"
                      , "that a law the comments name and the code lacks fails to compile. hsed's LawsSpec holds"
                      , "the list to the hand-written laws by name and hash, and runs each law's property." ] }


-- * Reading the bound units

-- | One unit bound to a hand-written law.
data LawRead = LawRead
  { lwAnchor :: !AnchorId        -- ^ @3.3.10.2.1#3a@, @T45@
  , lwQName  :: !Text            -- ^ the name its comment gives, @Sync.ifSendWhileBusy@
  , lwTable  :: !Bool            -- ^ a state table (@stateTable@), not a sentence (@law@)
  , lwProv   :: !Gen.Provenance  -- ^ where it is: its pages, and its hash in the IR (@cellsHash@)
  }
  deriving (Eq, Show)

-- | Every unit bound to a law: the sentences the law comments bind, in the
--   anchors' natural order, then the state tables the stateTable reader
--   generates (those C-STATE-TABLE passes), in anchor order.
lawReads :: Context -> [LawRead]
lawReads ctx =
  [ LawRead (bpAnchor bp) (fromMaybe "" (qnameOf (bpBinding bp))) False (paraProvenanceOf ctx bp)
  | bp <- boundParas (ruleShape lawsRule) ctx ]
  <> [ LawRead (btAnchor bt) (stQName r) True (boundProvenance ctx bt)
     | r <- stateTableReads ctx, let bt = stBound r, btAnchor bt `notElem` leftOut ]
  where
    leftOut = [ aid | Excluded aid _ <- ruleCompile stateTableRule ctx (boundTables (ruleShape stateTableRule) ctx) ]

-- | The name of a law binding, under its modifiers.
qnameOf :: SomeBinding -> Maybe Text
qnameOf = \case
  SomeBinding b -> go b
  PolyBinding _ -> Nothing
  where
    go :: Binding s -> Maybe Text
    go = \case
      Law q             -> Just q
      WithErratum _ _ b -> go b
      WithAs b          -> go b
      _                 -> Nothing

-- | @Module.Sub.name@: two or more parts, each an ASCII identifier, the modules
--   capitalised and the last a variable that is no keyword.
--
-- >>> map qualifiedLaw ["Sync.ifSendWhileBusy", "Sync.Sub.x'", "sync.x", "Sync", "Sync.X", "Sync.type", "Sync.if-send"]
-- [True,True,False,False,False,False,False]
qualifiedLaw :: Text -> Bool
qualifiedLaw q = case reverse (DT.splitOn "." q) of
  name : modules@(_ : _) -> all ident (name : modules) && all (startsWith C.isUpper) modules
                            && startsWith (\c -> C.isLower c || c == '_') name && name `notElem` keywords
  _                      -> False
  where
    ident p = not (T.null p) && T.all (\c -> C.isAscii c && (C.isAlphaNum c || c == '_' || c == '\'')) p
    startsWith f p = maybe False (f . fst) (T.uncons p)
    keywords = [ "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix"
               , "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_" ]

-- | The hand-written module a law's name refers to: @Hsed.Law.Sync@ for @Sync.ifSendWhileBusy@.
lawModuleName :: Text -> Text
lawModuleName q = T.intercalate "." (lawModule : reverse (drop 1 (reverse (DT.splitOn "." q))))

-- | The law a name refers to, qualified: @Hsed.Law.Sync.ifSendWhileBusy@.
lawReference :: Text -> Text
lawReference q = lawModule <> "." <> q


-- * The checks

-- | What C-LAW-BOUND and C-LAW-HASH find wrong with one unit, given every unit of the spec.
problems :: [LawRead] -> LawRead -> [Finding]
problems everyUnit r =
  [ finding Error "C-LAW-BOUND" ("the comment's name " <> tshow (lwQName r) <> " is not a qualified name, Module.name"
                                 <> " (its modules capitalised, its name a variable)")
  | not (qualifiedLaw (lwQName r)) ]
  <> [ finding Error "C-LAW-BOUND" ("the name " <> lwQName r <> " is bound already, by the comment at " <> renderAnchorId (lwAnchor o))
     | o <- take 1 earlier ]
  <> [ finding Error "C-LAW-HASH" ("the paragraph has no sha256 in the IR (" <> tshow sha <> ")")
     | not (lwTable r), T.length sha /= 64 || not (T.all C.isHexDigit sha) ]
  where
    sha     = (lwProv r).cellsHash
    earlier = filter ((== lwQName r) . lwQName) (takeWhile (/= r) everyUnit)

-- | Locate a finding at its unit: the paragraph's page and item anchor, or the table's.
locate :: LawRead -> Finding -> Finding
locate r = locatedAt (lwProv r).spec (renderAnchorId (lwAnchor r)) (lwProv r).pdfPage1


-- * Declarations

compile :: Context -> [Emitted]
compile ctx =
  [ Excluded (lwAnchor r) (T.intercalate " and " (L.nub (map Cm.code fs)) <> " failed") | (r, fs@(_ : _)) <- checked ]
  <> case [ r | (r, []) <- checked ] of
       []            -> []
       good@(r0 : _) -> [Emit (lawsDecl ctx r0 good)]
  where
    reads   = lawReads ctx
    checked = [ (r, problems reads r) | r <- reads ]

-- | @laws :: [LawRef]@, an entry per unit, each under its provenance line;
--   the declaration's own provenance is its first unit's.
lawsDecl :: Context -> LawRead -> [LawRead] -> Gen.Decl
lawsDecl ctx r0 rs = (Gen.decl (lwProv r0) "laws" body)
  { Gen.declImports = Gen.ImportSome lawModule ["LawRef (..)"] : map Gen.ImportQualified (L.nub (map (lawModuleName . lwQName) rs)) }
  where
    sid  = specIdText (cxSpec ctx)
    body =
      map ("-- " <>) (prefixFirst (Gen.wrapText 86 (T.unwords
        [ "What the comments of", sid, "bind to hand-written laws, sentences (law) first, then state tables"
        , "(stateTable), each in the anchors' order: the unit's anchor, its PDF page, the first 12 hex"
        , "digits of its hash in the IR, the name its comment gives, and the law that name refers to." ])))
      <> [ "laws :: [LawRef]", "laws =" ]
      <> Gen.listBlock "  " (map entry rs)
    prefixFirst = zipWith (\i l -> (if i == (0 :: Int) then "| " else "  ") <> l) [0 ..]
    entry r =
      [ "-- " <> fromMaybe c (T.stripPrefix "-- | " c)
      , "LawRef"
      , "  { lrAnchor = " <> tshow (sid <> ":" <> renderAnchorId (lwAnchor r))
      , "  , lrPage   = " <> tshow (lwProv r).pdfPage1
      , "  , lrHash   = " <> tshow (T.take 12 (lwProv r).cellsHash)
      , "  , lrName   = " <> tshow (lwQName r)
      , "  , lrLaw    = " <> lawReference (lwQName r)
      , "  }" ]
      where c = Gen.provenanceComment (lwProv r)
