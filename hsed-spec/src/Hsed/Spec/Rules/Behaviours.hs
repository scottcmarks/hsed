{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.Behaviours
Description : The behaviours reader: the method headings a comment binds to hand-written behaviours, and their Fails lists
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 13.16 and milestone M4a (the plan's answer 1). Where the spec
says what a method does in words under its heading, the code that does it is
hand-written (@Hsed.Sim.\<Module\>@) and the heading is a behaviour: a
@behaviour "Module.name"@ comment on a method's heading (Core 5.3.3.6, PDF
201: @behaviour "Base.get"@; 5.2.2.1, PDF 140: @behaviour
"SessionManager.properties"@) binds the heading, a heading unit of the
'Hsed.Spec.Shape.Method' shape, to the hand-written @Hsed.Sim.Behaviour@ of
the method's generated record (@Hsed.Gen.\<Spec\>.Methods@), which names what
it implements by the hash of the heading's signature block, says which of
the Test Cases observe it, and runs the method over the simulated TPer. A
@fails "Module.name"@ comment on the method's Fails heading (5.3.3.6.3, PDF
201: @fails "Base.getFails"@) binds that list, one labelled item per way the
method fails, to a hand-written @Fails@ the same way. The reader is reached
only by the comments: its name, @behaviours@, is no rule token (and not
@behaviour@, which the generic modules print as the binding's keyword), and
it never fires on a table. It reads the heading units ('boundHeadings', in
the anchors' natural order), the binding's constructor telling a behaviour
from a fails: 'behaviourReads'.

Checks, each an Error at the heading:

* @C-BEHAVIOUR-BOUND@: a comment's name is a qualified name, @Module.name@
  ("Hsed.Spec.Rules.Laws".'qualifiedLaw': two or more parts, each an ASCII
  identifier, the modules capitalised and the last a variable that is no
  keyword), and no two units of the spec, behaviours and Fails lists
  together, are bound to one name (each after the first is refused, citing
  the first). The other half runs when hsed builds: the generated module
  refers to each behaviour by its qualified name, so that a name the code
  lacks fails to compile.
* @C-BEHAVIOUR-METHOD@: a behaviour's heading is a method's: its section has
  a signature block ("Hsed.Spec.Rules.Sig".'sigReads', the signature unit
  @\<section\>/sig@), bound and read, whose method the sig rule joins to a
  UID (@C-SIG-METHOD@: the record has an @instance Method@, which a
  behaviour needs to run it; 5.6.4.2 Stir has none). The other half runs when
  hsed builds: the generated module names each behaviour's record with a
  type application, so that a behaviour of another method's record fails to
  compile.
* @C-FAILS-LIST@: a fails comment sits on a heading titled @Fails@ whose
  parent section is bound by a behaviour (the Fails list of a method the
  simulator runs, bound once), and that section has at least one labelled
  list item in the IR (@a@, @b@, ...: docs/ir-v1.md), each with its sha256
  there, of which the hand-written item states the first 12 hex digits.

Compiled into 'behavioursModule' (@Hsed.Gen.\<Spec\>.Behaviours@; nothing
where the spec binds no behaviour): @behaviours :: [BehaviourRef]@, one
entry per behaviour the checks pass, in anchor order, each with the
heading's anchor (@core-2.01:5.3.3.6@), its PDF page (1-based), the first 12
hex digits of its signature block's hash in the IR (the one the behaviour
states), the name its comment gives, the behaviour that name refers to at
the method's record (@forMethod \@Hsed.Gen.Core.Methods.Get
Hsed.Sim.Base.get@, both modules imported qualified) and its Fails list where
a fails comment binds one (the Fails heading's anchor and page, the name,
each item's label with the first 12 hex digits of its hash, and the @Fails@
that name refers to, @forFails \@Hsed.Gen.Core.Methods.Get
Hsed.Sim.Base.getFails@). A unit a check refuses is left out, and so is a
Fails list whose method's heading is. The module is generated code importing
hand-written code (@Hsed.Sim.Behaviour@ and @Hsed.Sim.\<Module\>@): it
declares nothing the oracle compares (no UID, no name map, no enumeration,
nothing under a table's provenance) and is compiled by hsed alone, which no
@Hsed.Sim@ module may import (a cycle); hsed's @BehavioursSpec@ holds it to
the hand-written dispatch lists by name and hash, and runs each Fails item's
property.
-}
module Hsed.Spec.Rules.Behaviours
  ( behavioursRule
  , behavioursModule
    -- * Reading the bound units
  , BehaviourRead (..)
  , behaviourReads
  , failsItemsOf
  , parentSection
  , behaviourModuleName
  , behaviourReference
  ) where

import qualified Data.Text            as DT
import qualified RIO.Char             as C
import qualified RIO.List             as L
import qualified RIO.Map              as Map
import qualified RIO.Text             as T
import           RIO

import           Hsed.Spec.Anchor     (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common     (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common     as Cm
import           Hsed.Spec.DSL        (Binding (..), SomeBinding (..), behaviour)
import qualified Hsed.Spec.Gen        as Gen
import           Hsed.Spec.IR         (HeadingIR (..), ParaIR (..), ParaKind (..), SpecIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.Laws (qualifiedLaw)
import           Hsed.Spec.Rules.Sig  (SigRead (..), joinedUID, methodsModule, sigReads)


-- | The reader: reached only by a @behaviour@ comment, or a @fails@ one.
behavioursRule :: RuleDef
behavioursRule = RuleDef
  { ruleName    = RuleName "behaviours"
  , bindsTo     = SomeBinding (behaviour "")
  , exemplar    = (SpecId "core-2.01", SectionId "5.3.3.6")
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ _ -> False
  , ruleIdent   = const Nothing
  , ruleModule  = behavioursModule
  , ruleCodes   =
      [ CodeDef "C-BEHAVIOUR-BOUND"  [Error] Bookkeeping
      , CodeDef "C-BEHAVIOUR-METHOD" [Error] Bookkeeping
      , CodeDef "C-FAILS-LIST"       [Error] Bookkeeping ]
  , ruleChecks  = \ctx _ -> let reads = behaviourReads ctx
                            in concatMap (\r -> map (atHeading ctx (bvBound r)) (problems ctx reads r)) reads
  , ruleCompile = \ctx _ -> compile ctx
  }

-- | The hand-written modules the behaviours live in: @Hsed.Sim.\<Module\>@.
simModule :: Text
simModule = "Hsed.Sim"

-- | The runtime module of a behaviour and of what the generated module lists:
--   @BehaviourRef@, @FailsRef@, @forMethod@, @forFails@.
behaviourModule :: Text
behaviourModule = "Hsed.Sim.Behaviour"

-- | @Hsed.Gen.\<Spec\>.Behaviours@.
behavioursModule :: Gen.GenModule
behavioursModule = Gen.GenModule
  { Gen.leaf        = "Behaviours"
  , Gen.description = "The method headings bound to hand-written behaviours, and their Fails lists"
  , Gen.notes       = [ "Each entry is a method's heading a behaviour comment binds to the hand-written code that runs"
                      , "the method (Hsed.Sim.<Module>): its anchor, its PDF page, the first 12 hex digits of its"
                      , "signature block's hash in the IR, the name its comment gives, and the behaviour of that name at"
                      , "the method's generated record (Hsed.Gen.<Spec>.Methods), so that a behaviour the comments name"
                      , "and the code lacks, or one of another method's record, fails to compile; where a fails comment"
                      , "binds the method's Fails list, its heading, name and items beside it. hsed's BehavioursSpec"
                      , "holds the list to the hand-written dispatch lists by name and hash, and runs each Fails item's"
                      , "property." ] }


-- * Reading the bound units

-- | One heading bound to a hand-written behaviour, or to its Fails list.
data BehaviourRead = BehaviourRead
  { bvAnchor  :: !AnchorId         -- ^ @5.3.3.6@; a Fails list's, @5.3.3.6.3@
  , bvSection :: !Text             -- ^ the section number the anchor names
  , bvQName   :: !Text             -- ^ the name its comment gives, @Base.get@
  , bvFails   :: !Bool             -- ^ a Fails list (@fails@), not a method's heading (@behaviour@)
  , bvBound   :: !BoundHeading     -- ^ the unit: its heading (title, page) and its binding
  , bvProv    :: !Gen.Provenance   -- ^ where it is: its page, and its title's hash ('headingHash')
  }

-- | Every heading unit a behaviour or a fails comment binds, in the anchors'
--   natural order (@5.2.3.5@ before @5.3.3.6@, @5.3.3.6@ before @5.3.3.6.3@).
behaviourReads :: Context -> [BehaviourRead]
behaviourReads ctx =
  [ BehaviourRead (bhAnchor bh) (renderAnchorId (bhAnchor bh)) q isFails bh (headingProvenanceOf ctx bh)
  | bh <- boundHeadings (ruleShape behavioursRule) ctx, Just (q, isFails) <- [nameOf (bhBinding bh)] ]

-- | The name of a behaviour or fails binding, and which of the two it is, under its modifiers.
nameOf :: SomeBinding -> Maybe (Text, Bool)
nameOf = \case
  SomeBinding b -> go b
  PolyBinding _ -> Nothing
  where
    go :: Binding s -> Maybe (Text, Bool)
    go = \case
      Behaviour q       -> Just (q, False)
      Fails q           -> Just (q, True)
      WithErratum _ _ b -> go b
      WithAs b          -> go b
      _                 -> Nothing

-- | The labelled list items of a section, in document order, each with its
--   sha256 in the IR: a Fails list's items @a@, @b@, ... (Core 5.3.3.6.3, PDF 201).
failsItemsOf :: Context -> Text -> [(Text, Text)]
failsItemsOf ctx s = [ (label, p.hash) | p <- (cxIR ctx).paras, p.section == Just s, p.kind == LI, Just label <- [p.item] ]

-- | The section a section lies in: @5.3.3.6@ for @5.3.3.6.3@; Nothing for a top-level one.
--
-- >>> map parentSection ["5.3.3.6.3", "5", ""]
-- [Just "5.3.3.6",Nothing,Nothing]
parentSection :: Text -> Maybe Text
parentSection s = case reverse (DT.splitOn "." s) of
  _ : rest@(_ : _) -> Just (T.intercalate "." (reverse rest))
  _                -> Nothing

-- | The hand-written module a behaviour's name refers to: @Hsed.Sim.Base@ for @Base.get@.
behaviourModuleName :: Text -> Text
behaviourModuleName q = T.intercalate "." (simModule : reverse (drop 1 (reverse (DT.splitOn "." q))))

-- | The behaviour a name refers to, qualified: @Hsed.Sim.Base.get@.
behaviourReference :: Text -> Text
behaviourReference q = simModule <> "." <> q


-- * The checks

-- | The method a behaviour's heading defines: its section's signature block,
--   read by the sig rule, and the generated UID its method joins to; or why
--   the heading is no method's that a behaviour can run.
methodOf :: Context -> BehaviourRead -> Either Text (SigRead, Text)
methodOf ctx r = do
  sr <- case (Map.member aid (cxSigs ctx), Map.lookup aid (cxSigClasses ctx) >>= boundBinding, reads) of
    (False, _, _)          -> Left ("the section " <> s <> " has no signature block: a behaviour binds a method's heading,"
                                    <> " whose section gives the method's signature")
    (True, Nothing, _)     -> Left ("the signature block " <> s <> "/sig is bound by no rule, so no method record is generated for it")
    (True, Just _, [])     -> Left ("the signature block " <> s <> "/sig does not read (C-SIG-PARSE), so no method record is generated for it")
    (True, Just _, sr : _) -> Right sr
  case joinedUID ctx sr of
    Just i  -> Right (sr, i)
    Nothing -> Left ("the method " <> srMethod sr <> " of " <> s <> "/sig has no UID (C-SIG-METHOD), so no instance Method runs it")
  where
    s     = bvSection r
    aid   = SigId s
    reads = [ sr | Right sr <- sigReads ctx, bsAnchor (srBound sr) == aid ]

-- | What the checks find wrong with one unit, given every unit of the spec.
problems :: Context -> [BehaviourRead] -> BehaviourRead -> [Finding]
problems ctx everyUnit r =
  [ finding Error "C-BEHAVIOUR-BOUND" ("the comment's name " <> tshow (bvQName r) <> " is not a qualified name, Module.name"
                                       <> " (its modules capitalised, its name a variable)")
  | not (qualifiedLaw (bvQName r)) ]
  <> [ finding Error "C-BEHAVIOUR-BOUND" ("the name " <> bvQName r <> " is bound already, by the comment at " <> renderAnchorId (bvAnchor o))
     | o <- take 1 (filter ((== bvQName r) . bvQName) earlier) ]
  <> (if bvFails r
        then map (finding Error "C-FAILS-LIST") (failsProblems ctx everyUnit earlier r)
        else [ finding Error "C-BEHAVIOUR-METHOD" why | Left why <- [methodOf ctx r] ])
  where
    earlier = takeWhile ((/= bvAnchor r) . bvAnchor) everyUnit

-- | What C-FAILS-LIST finds wrong with a Fails list, given every unit of the
--   spec and the units before it.
failsProblems :: Context -> [BehaviourRead] -> [BehaviourRead] -> BehaviourRead -> [Text]
failsProblems ctx everyUnit earlier r =
  [ "the heading " <> s <> " is titled " <> tshow (titleOf r) <> ", not Fails: a fails comment binds a method's Fails list"
  | not (titledFails r) ]
  <> case parentSection s of
       Nothing -> [ "the heading " <> s <> " is no section's subsection: a Fails list lies under its method's heading" ]
       Just p  ->
         [ "its method's heading " <> p <> " is bound by no behaviour comment: a Fails list is bound beside the method the simulator runs"
         | p `notElem` [ bvSection b | b <- everyUnit, not (bvFails b) ] ]
         <> [ "the method " <> p <> " has its Fails list bound already, by the comment at " <> renderAnchorId (bvAnchor o)
            | o <- take 1 [ b | b <- earlier, bvFails b, titledFails b, parentSection (bvSection b) == Just p ] ]
  <> [ "the section has no labelled list item in the IR: a Fails list's items are a, b, ... (docs/ir-v1.md)" | null items ]
  <> [ "the list item " <> label <> " has no sha256 in the IR (" <> tshow h <> ")" | (label, h) <- items, not (isSha256 h) ]
  where
    s             = bvSection r
    titleOf b     = (bhHeading (bvBound b)).title
    titledFails b = T.toLower (T.strip (titleOf b)) == "fails"
    items         = failsItemsOf ctx s
    isSha256 h    = T.length h == 64 && T.all C.isHexDigit h


-- * Declarations

-- | A behaviour the checks pass: its method's record, its signature block's
--   provenance (whose hash the behaviour states) and its Fails list, where
--   one is bound and passes, with the list's items.
data Entry = Entry
  { eRead   :: !BehaviourRead
  , eMethod :: !Text
  , eSig    :: !Gen.Provenance
  , eFails  :: !(Maybe (BehaviourRead, [(Text, Text)]))
  }

compile :: Context -> [Emitted]
compile ctx =
  [ Excluded (bvAnchor r) (T.intercalate " and " (L.nub (map Cm.code fs)) <> " failed") | (r, fs@(_ : _)) <- checked ]
  <> [ Excluded (bvAnchor f) ("its method's heading " <> p <> " is left out")
     | f <- goodFails, Just p <- [parentSection (bvSection f)], p `notElem` map (bvSection . eRead) entries ]
  <> case entries of
       []    -> []
       e : _ -> [Emit (behavioursDecl ctx e entries)]
  where
    reads     = behaviourReads ctx
    checked   = [ (r, problems ctx reads r) | r <- reads ]
    good      = [ r | (r, []) <- checked ]
    goodFails = filter bvFails good
    entries   = [ Entry r (srMethod sr) (sigProvenanceOf ctx (srBound sr)) (failsOf r)
                | r <- good, not (bvFails r), Right (sr, _) <- [methodOf ctx r] ]
    failsOf r = listToMaybe [ (f, failsItemsOf ctx (bvSection f)) | f <- goodFails, parentSection (bvSection f) == Just (bvSection r) ]

-- | @behaviours :: [BehaviourRef]@, an entry per behaviour, each under its
--   heading's provenance line, its Fails list under the Fails heading's; the
--   declaration's own provenance is its first behaviour's heading's.
behavioursDecl :: Context -> Entry -> [Entry] -> Gen.Decl
behavioursDecl ctx e0 es = (Gen.decl (bvProv (eRead e0)) "behaviours" body)
  { Gen.declImports    = [ Gen.ImportSome behaviourModule (["BehaviourRef (..)", "forMethod"] <> [ x | anyFails, x <- ["FailsRef (..)", "forFails"] ])
                         , Gen.ImportSome "Prelude" ["Maybe (..)"]
                         , Gen.ImportQualified methodsName ]
                         <> map Gen.ImportQualified (L.nub (concatMap modulesOf es))
  , Gen.declExtensions = ["TypeApplications"] }
  where
    sid         = specIdText (cxSpec ctx)
    methodsName = Gen.moduleName (cxSpec ctx) methodsModule
    anyFails    = any (isJust . eFails) es
    modulesOf e = behaviourModuleName (bvQName (eRead e)) : [ behaviourModuleName (bvQName f) | Just (f, _) <- [eFails e] ]
    body =
      map ("-- " <>) (prefixFirst (Gen.wrapText 86 (T.unwords
        [ "What the comments of", sid, "bind to hand-written behaviours, in the anchors' order: each method"
        , "heading's anchor, its PDF page, the first 12 hex digits of its signature block's hash in the IR,"
        , "the name its comment gives, the behaviour that name refers to at the method's generated record,"
        , "and its Fails list where a fails comment binds one: the Fails heading's anchor and page, the"
        , "name, each item's label with the first 12 hex digits of its hash in the IR, and the Fails that"
        , "name refers to." ])))
      <> [ "behaviours :: [BehaviourRef]", "behaviours =" ]
      <> Gen.listBlock "  " (map entry es)
    prefixFirst = zipWith (\i l -> (if i == (0 :: Int) then "| " else "  ") <> l) [0 ..]
    record e    = methodsName <> "." <> eMethod e
    entry e =
      [ provLine (bvProv (eRead e))
      , "BehaviourRef"
      , "  { brAnchor    = " <> tshow (sid <> ":" <> bvSection (eRead e))
      , "  , brPage      = " <> tshow (bvProv (eRead e)).pdfPage1
      , "  , brHash      = " <> tshow (T.take 12 (eSig e).cellsHash)
      , "  , brName      = " <> tshow (bvQName (eRead e))
      , "  , brBehaviour = forMethod @" <> record e <> " " <> behaviourReference (bvQName (eRead e)) ]
      <> failsLines e
      <> [ "  }" ]
    failsLines e = case eFails e of
      Nothing -> [ "  , brFails     = Nothing" ]
      Just (f, items) ->
        [ "    " <> provLine (bvProv f)
        , "  , brFails     = Just FailsRef"
        , "      { frAnchor = " <> tshow (sid <> ":" <> bvSection f)
        , "      , frPage   = " <> tshow (bvProv f).pdfPage1
        , "      , frName   = " <> tshow (bvQName f)
        , "      , frItems  =" ]
        <> Gen.listLines "          " [ "(" <> tshow label <> ", " <> tshow (T.take 12 h) <> ")" | (label, h) <- items ]
        <> [ "      , frFails  = forFails @" <> record e <> " " <> behaviourReference (bvQName f)
           , "      }" ]
    provLine p = let c = Gen.provenanceComment p in "-- " <> fromMaybe c (T.stripPrefix "-- | " c)
