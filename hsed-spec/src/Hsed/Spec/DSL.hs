{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Hsed.Spec.DSL
Description : The typed binding language: one constructor per shape, modifiers, and the comment vocabulary
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A @'Binding' s@ is what a comment asserts about an anchor of shape @s@
(design section 4.2). The lowercase smart constructors are the comment
vocabulary of section 3.3: @hsedc bind@ prints each parsed payload with them
into @Hsed.Bindings.\<Spec\>@, so GHC's type check of that module is the check of
every comment. A modifier of the wrong shape (@cols 0 1 typeRow@) is a type
error there; the same check at run time, for a payload that was just parsed,
is 'applyModifier'.

A rule token sits on a heading or on the document (@rule typeRow & rule
enumValues@ at Core section 5.1.3, @doc core201 & rule codes@ on page 1): the
'RuleBearing' shapes. @&@ joins only those. 'Refines', 'Narrows' and
'Replaces' take their shape from the target they name, so they are
shape-polymorphic here and 'PolyBinding' carries them until the rules resolve
the target.

'bindingKind' says which anchors a binding belongs on (design section 3.1):
@doc@ and its rule tokens on the document, rule tokens or a @behaviour@ on a
heading, a table binding (with its modifiers) on a table, @law@, @fails@ or
@behaviour@ on a list item or paragraph. "Hsed.Spec.Rules" refuses a binding
on an anchor of another kind.
-}
module Hsed.Spec.DSL
  ( -- * Bindings
    Binding (..)
  , QName
  , Ref (..)
  , RuleBearing
  , LayoutShape
  , GridShape
  , RuleToken (..)
  , shapeOfBinding
  , bindingShape
  , isAtomicBinding
  , BindingKind (..)
  , bindingKind
  , someBindingKind
    -- * Modifiers
  , Modifier (..)
  , applyModifier
  , applyModifierSome
    -- * Packed bindings and bound anchors
  , SomeBinding (..)
  , someBindingShape
  , atShape
  , Bound (..)
  , at
    -- * Checking a binding against a form
  , bindingAccepts
    -- * The comment vocabulary
    -- ** Document and rule anchors
  , doc, rule
    -- ** Table bindings
  , typeRow, enumValues, tableDesc, columnsAddition, tableUIDs, uidTable, statusCodes, codes
  , fieldValue, fieldType, byteFieldValue, byteGrid, feature
  , constValues, ranges, preconfig, tokens, stateTable, propsNames, propsInitial, propsReq
  , legend, decision, sig, prose
    -- ** Prose bindings
  , behaviour, fails, law
    -- ** Cross-spec bindings
  , refines, narrows, replaces, extends
  , core, opal, section
    -- ** Joining rule tokens
  , (&)
    -- ** Modifiers
  , erratum, cols, rangeWords, enumInDescription, expand, as, waive, layout, bits
  , (~>), scott, waivableChecks
    -- ** Names
  , core201, opal230, knownSpecs, specName, specByName
  , tokenEncoding, terminology, knownRules
    -- * Re-exported for generated modules
  , Anchor (..)
  , anchor
  , Shape (..)
  , SpecId (..)
  , RuleName (..)
  , ErrataSet (..)
  ) where

import           Data.Type.Equality (TestEquality (..), (:~:) (..))
import qualified RIO.Char           as C
import qualified RIO.Text           as T
import           RIO                hiding ((&))
import           Text.Show          (showChar, showParen, showString, showsPrec)

import           Hsed.Spec.Anchor   (Anchor (..), anchor)
import           Hsed.Spec.Common   (ErrataSet (..), Finding (Finding), RuleName (..), Severity (..),
                                     SpecId (..), finding)
import           Hsed.Spec.Shape


-- * Bindings

-- | A qualified Haskell name in the runtime, @Sync.ifSendWhileBusy@.
type QName = Text

-- | A reference to a table or a section of a specification, @core 171@, @opal 26@, @section "5.1.3"@.
data Ref
  = Core Int
  | Opal Int
  | Section Text
  deriving stock (Eq, Ord, Show)

-- | The shapes that can carry @rule@ tokens: a heading or the document.
class KnownShape s => RuleBearing (s :: Shape)
instance RuleBearing 'Doc
instance RuleBearing 'Heading

-- | The shapes a byte layout is generated from, which @layout "Name"@ names:
--   a byte grid, a feature descriptor, a @Byte(s) | Field | Value(s)@ table.
class KnownShape s => LayoutShape (s :: Shape)
instance LayoutShape 'ByteGrid
instance LayoutShape 'Feature
instance LayoutShape 'ByteFieldValue

-- | The shapes read as a grid of bytes and bits, whose bit map @bits k [...]@
--   declares where the extent rule cannot read one (design example E8).
class LayoutShape s => GridShape (s :: Shape)
instance GridShape 'ByteGrid
instance GridShape 'Feature

-- | What a comment asserts about an anchor of shape @s@.
data Binding (s :: Shape) where
  -- document and rule anchors
  DocB                  :: SpecId -> Binding 'Doc
  RuleB                 :: RuleBearing s => RuleName -> Binding s
  -- table bindings, one per table shape
  TypeRowB              :: Binding 'TypeRow
  EnumValuesB           :: Binding 'EnumValues
  TableDescB            :: Binding 'TableDesc
  ColumnsAdditionB      :: Binding 'ColumnsAddition
  TableUIDsB            :: Binding 'TableUIDs
  UidTableB             :: Binding 'UidTable
  StatusCodesB          :: Binding 'StatusCodes
  CodesB                :: Binding 'Codes
  FieldValueB           :: Binding 'FieldValue
  FieldTypeB            :: Binding 'FieldType
  ByteFieldValueB       :: Binding 'ByteFieldValue
  ByteGridB             :: Binding 'ByteGrid
  FeatureB              :: Binding 'Feature
  ConstValuesB          :: Binding 'ConstValues
  RangesB               :: Binding 'Ranges
  PreconfigB            :: Binding 'Preconfig
  TokensB               :: Binding 'Tokens
  StateTableB           :: QName -> Binding 'StateTable
  PropsNames            :: Binding 'Props
  PropsInitial          :: Binding 'Props
  PropsReqB             :: Binding 'PropsReq
  LegendB               :: Binding 'Legend
  DecisionB             :: Binding 'Decision
  SigB                  :: Binding 'Sig
  ProseB                :: Binding 'Prose
  -- prose bindings
  Behaviour             :: QName -> Binding 'Heading
  Fails                 :: QName -> Binding 'Prose
  Law                   :: QName -> Binding 'Sentence   -- ^ a paragraph unit: the sentence or list item it sits on
  -- cross-spec bindings, shaped by their target
  Refines               :: KnownShape s => Ref -> Binding s
  Narrows               :: KnownShape s => Ref -> Binding s
  Replaces              :: KnownShape s => [Ref] -> Binding s
  Extends               :: Ref -> Binding 'ColumnsAddition
  -- joining rule tokens
  Also                  :: RuleBearing s => Binding s -> Binding s -> Binding s
  -- modifiers
  WithErratum           :: ErrataSet -> [(Text, Text)] -> Binding s -> Binding s
  WithWaiver            :: Text -> Text -> Text -> Binding 'TypeRow -> Binding 'TypeRow   -- ^ check code, subject, reason
  WithCols              :: Int -> Int -> Binding 'EnumValues -> Binding 'EnumValues
  WithRangeWords        :: [Text] -> Binding 'EnumValues -> Binding 'EnumValues
  WithEnumInDescription :: Binding 'EnumValues -> Binding 'EnumValues
  WithExpand            :: Binding 'Preconfig -> Binding 'Preconfig
  WithAs                :: Binding s -> Binding s
  WithLayout            :: LayoutShape s => Text -> Binding s -> Binding s        -- ^ the layout's name when the caption's will not do
  WithBits              :: GridShape s => Int -> [(Int, Text)] -> Binding s -> Binding s  -- ^ a byte's declared bit map

deriving stock instance Show (Binding s)
deriving stock instance Eq (Binding s)

-- | The shape of a binding, recovered from its constructors.
shapeOfBinding :: Binding s -> SShape s
shapeOfBinding = \case
  DocB _                  -> SDoc
  RuleB _                 -> shapeSing
  TypeRowB                -> STypeRow
  EnumValuesB             -> SEnumValues
  TableDescB              -> STableDesc
  ColumnsAdditionB        -> SColumnsAddition
  TableUIDsB              -> STableUIDs
  UidTableB               -> SUidTable
  StatusCodesB            -> SStatusCodes
  CodesB                  -> SCodes
  FieldValueB             -> SFieldValue
  FieldTypeB              -> SFieldType
  ByteFieldValueB         -> SByteFieldValue
  ByteGridB               -> SByteGrid
  FeatureB                -> SFeature
  ConstValuesB            -> SConstValues
  RangesB                 -> SRanges
  PreconfigB              -> SPreconfig
  TokensB                 -> STokens
  StateTableB _           -> SStateTable
  PropsNames              -> SProps
  PropsInitial            -> SProps
  PropsReqB               -> SPropsReq
  LegendB                 -> SLegend
  DecisionB               -> SDecision
  SigB                    -> SSig
  ProseB                  -> SProse
  Behaviour _             -> SHeading
  Fails _                 -> SProse
  Law _                   -> SSentence
  Refines _               -> shapeSing
  Narrows _               -> shapeSing
  Replaces _              -> shapeSing
  Extends _               -> SColumnsAddition
  Also a _                -> shapeOfBinding a
  WithErratum _ _ b       -> shapeOfBinding b
  WithWaiver _ _ _ _      -> STypeRow
  WithCols _ _ _          -> SEnumValues
  WithRangeWords _ _      -> SEnumValues
  WithEnumInDescription _ -> SEnumValues
  WithExpand _            -> SPreconfig
  WithAs b                -> shapeOfBinding b
  WithLayout _ b          -> shapeOfBinding b
  WithBits _ _ b          -> shapeOfBinding b

bindingShape :: Binding s -> Shape
bindingShape = sShape . shapeOfBinding

-- | A binding that prints as one word, so it needs no parentheses as an operand.
isAtomicBinding :: Binding s -> Bool
isAtomicBinding = \case
  TypeRowB         -> True
  EnumValuesB      -> True
  TableDescB       -> True
  ColumnsAdditionB -> True
  TableUIDsB       -> True
  UidTableB        -> True
  StatusCodesB     -> True
  CodesB           -> True
  FieldValueB      -> True
  FieldTypeB       -> True
  ByteFieldValueB  -> True
  ByteGridB        -> True
  FeatureB         -> True
  ConstValuesB     -> True
  RangesB          -> True
  PreconfigB       -> True
  TokensB          -> True
  PropsNames       -> True
  PropsInitial     -> True
  PropsReqB        -> True
  LegendB          -> True
  DecisionB        -> True
  SigB             -> True
  ProseB           -> True
  _                -> False

-- | What a binding is about, which fixes the anchors it may sit on.
data BindingKind
  = DocKind [SpecId] Bool  -- ^ @doc@ tokens (with any rule tokens), and whether a @doc@ token comes first
  | RuleKind               -- ^ rule tokens only
  | TableKind              -- ^ a table binding, with or without modifiers
  | BehaviourKind          -- ^ @behaviour "M.f"@
  | LawKind                -- ^ @law "M.f"@ or @fails "M.f"@
  | SigKind                -- ^ @sig@: signature blocks are rule-bound and carry no annotation
  | ModifiedKind BindingKind  -- ^ modifiers on a binding that is not a table's
  deriving stock (Eq, Show)

-- | The kind of a binding, from its constructors.
bindingKind :: Binding s -> BindingKind
bindingKind b = case b of
  DocB _                  -> rules
  RuleB _                 -> rules
  Also _ _                -> rules
  Behaviour _             -> BehaviourKind
  Law _                   -> LawKind
  Fails _                 -> LawKind
  SigB                    -> SigKind
  WithErratum _ _ inner   -> modified inner
  WithWaiver _ _ _ inner  -> modified inner
  WithAs inner            -> modified inner
  _                       -> TableKind
  where
    rules = case docTokens b of
      []   -> RuleKind
      docs -> DocKind docs (leftmostIsDoc b)
    modified :: Binding t -> BindingKind
    modified inner = case bindingKind inner of
      TableKind      -> TableKind
      ModifiedKind k -> ModifiedKind k
      k              -> ModifiedKind k
    docTokens :: Binding t -> [SpecId]
    docTokens = \case
      DocB sid -> [sid]
      Also l r -> docTokens l <> docTokens r
      _        -> []
    leftmostIsDoc :: Binding t -> Bool
    leftmostIsDoc = \case
      DocB _   -> True
      Also l _ -> leftmostIsDoc l
      _        -> False

-- | 'bindingKind' of a packed binding; one shaped by its target is a table binding.
someBindingKind :: SomeBinding -> BindingKind
someBindingKind (SomeBinding b) = bindingKind b
someBindingKind (PolyBinding b) = bindingKind (b :: Binding 'Prose)


-- * Modifiers

-- | A modifier as the grammar sees it, before its shape is checked.
data Modifier
  = Erratum ErrataSet [(Text, Text)]   -- ^ @erratum scott ["Fractoin" ~> "Fraction"]@
  | Cols Int Int                       -- ^ @cols 0 1@: the two columns of a wider enumeration table
  | RangeWords [Text]                  -- ^ @rangeWords ["to"]@
  | EnumInDescription                  -- ^ the enumeration is read from the Description column
  | Expand                             -- ^ expand the @(+XX)@ template rows of a preconfiguration
  | As Shape                           -- ^ @as \@'Codes@: the form is odd; E-SHAPE is a Warning
  | Waive Text Text Text               -- ^ @waive "C-UID-UNIQUE" "T94 00 00 00 05 00 00 02 15" "reason"@
  | LayoutName Text                    -- ^ @layout "ComPacketHeader"@: the layout's name, the caption's overridden
  | Bits Int [(Int, Text)]             -- ^ @bits 5 [(0, "KeyChangeZone"), (1, "IdScope")]@: a byte's bit map
  deriving stock (Eq, Show)

-- | Apply a modifier to a binding, checking at run time what the smart
--   constructors check at compile time, and that a binding takes one
--   @layout@ and one @bits@ for each byte (a second would be read by no one).
applyModifier :: forall s. Modifier -> Binding s -> Either Finding (Binding s)
applyModifier m b = case m of
  LayoutName n
    | l : _ <- layoutsOf b -> Left (finding Error "E-MODIFIER" ("a second layout name, " <> tshow n <> " beside " <> tshow l <> ": a binding takes one"))
  Bits k _
    | k `elem` bitsBytesOf b -> Left (finding Error "E-MODIFIER" ("a second bits modifier for byte " <> tshow k <> ": a binding takes one for each byte"))
  _ -> applyModifier' m b

-- | The layout names and the bytes of the bits modifiers a binding already carries.
layoutsOf :: Binding s -> [Text]
layoutsOf = \case
  WithLayout n b          -> n : layoutsOf b
  WithBits _ _ b          -> layoutsOf b
  WithErratum _ _ b       -> layoutsOf b
  WithAs b                -> layoutsOf b
  _                       -> []

bitsBytesOf :: Binding s -> [Int]
bitsBytesOf = \case
  WithBits k _ b          -> k : bitsBytesOf b
  WithLayout _ b          -> bitsBytesOf b
  WithErratum _ _ b       -> bitsBytesOf b
  WithAs b                -> bitsBytesOf b
  _                       -> []

applyModifier' :: forall s. Modifier -> Binding s -> Either Finding (Binding s)
applyModifier' m b = case m of
  Erratum e ps      -> Right (WithErratum e ps b)
  Waive c s r       -> onShape STypeRow (WithWaiver c s r)
  Cols a c          -> onShape SEnumValues (WithCols a c)
  RangeWords ws     -> onShape SEnumValues (WithRangeWords ws)
  EnumInDescription -> onShape SEnumValues WithEnumInDescription
  Expand            -> onShape SPreconfig WithExpand
  As sh
    | sh == have    -> Right (WithAs b)
    | otherwise     -> Left (mismatch sh)
  LayoutName n      -> case shapeOfBinding b of
    SByteGrid       -> Right (WithLayout n b)
    SFeature        -> Right (WithLayout n b)
    SByteFieldValue -> Right (WithLayout n b)
    _               -> Left (mismatchAny [ByteGrid, Feature, ByteFieldValue])
  Bits k ps         -> case shapeOfBinding b of
    SByteGrid       -> Right (WithBits k ps b)
    SFeature        -> Right (WithBits k ps b)
    _               -> Left (mismatchAny [ByteGrid, Feature])
  where
    have = bindingShape b
    onShape :: SShape t -> (Binding t -> Binding t) -> Either Finding (Binding s)
    onShape want f = case testEquality (shapeOfBinding b) want of
      Just Refl -> Right (f b)
      Nothing   -> Left (mismatch (sShape want))
    mismatch want = finding Error "E-MODIFIER"
      (modifierWord m <> " applies to a " <> shapeName want <> " binding, not to a " <> shapeName have <> " one")
    mismatchAny wants = finding Error "E-MODIFIER"
      (modifierWord m <> " applies to a " <> T.intercalate ", " (map shapeName wants) <> " binding, not to a " <> shapeName have <> " one")

modifierWord :: Modifier -> Text
modifierWord = \case
  Erratum _ _       -> "erratum"
  Waive _ _ _       -> "waive"
  Cols a c          -> "cols " <> tshow a <> " " <> tshow c
  RangeWords _      -> "rangeWords"
  EnumInDescription -> "enumInDescription"
  Expand            -> "expand"
  As sh             -> "as @'" <> shapeName sh
  LayoutName _      -> "layout"
  Bits k _          -> "bits " <> tshow k


-- * Packed bindings and bound anchors

-- | A binding whose shape is known only at run time, or one that takes its
--   shape from a cross-spec target the rules have yet to resolve.
data SomeBinding
  = forall s. SomeBinding (Binding s)
  | PolyBinding (forall s. KnownShape s => Binding s)

instance Show SomeBinding where
  showsPrec d (SomeBinding b) = showParen (d > 10) (showString "SomeBinding " . showsPrec 11 b)
  showsPrec d (PolyBinding b) = showParen (d > 10) (showString "PolyBinding " . showsPrec 11 (b :: Binding 'Prose))

instance Eq SomeBinding where
  SomeBinding a == SomeBinding b = case testEquality (shapeOfBinding a) (shapeOfBinding b) of
    Just Refl -> a == b
    Nothing   -> False
  PolyBinding a == PolyBinding b = (a :: Binding 'Prose) == b
  _ == _ = False

-- | The shape, when the binding fixes one.
someBindingShape :: SomeBinding -> Maybe Shape
someBindingShape (SomeBinding b) = Just (bindingShape b)
someBindingShape (PolyBinding _) = Nothing

-- | The binding at a shape: a polymorphic one instantiated there, a fixed one only if it matches.
atShape :: forall t. KnownShape t => SomeBinding -> Maybe (Binding t)
atShape (PolyBinding b) = Just b
atShape (SomeBinding b) = case testEquality (shapeOfBinding b) (shapeSing @t) of
  Just Refl -> Just b
  Nothing   -> Nothing

-- | 'applyModifier' on a packed binding; a shaped modifier fixes the shape of a polymorphic one.
applyModifierSome :: Modifier -> SomeBinding -> Either Finding SomeBinding
applyModifierSome m (SomeBinding b) = SomeBinding <$> applyModifier m b
applyModifierSome m (PolyBinding b) = case m of
  Erratum e ps      -> Right (PolyBinding (WithErratum e ps b))
  Waive c s r       -> Right (SomeBinding (WithWaiver c s r b))
  Cols a c          -> Right (SomeBinding (WithCols a c b))
  RangeWords ws     -> Right (SomeBinding (WithRangeWords ws b))
  EnumInDescription -> Right (SomeBinding (WithEnumInDescription b))
  Expand            -> Right (SomeBinding (WithExpand b))
  As sh             -> case toSomeShape sh of
    SomeShape (_ :: SShape t) -> Right (SomeBinding (WithAs (b :: Binding t)))
  LayoutName _      -> Left (unshaped "layout")
  Bits k _          -> Left (unshaped ("bits " <> tshow k))
  where
    -- layout and bits apply to several shapes, so they cannot fix the shape of a
    -- binding that takes its shape from its target
    unshaped w = finding Error "E-MODIFIER"
      (w <> " applies to a binding whose shape is fixed, not to one that takes its shape from its target (refines, narrows, replaces)")

-- | An anchor with its binding, the two agreeing on the shape.
data Bound = forall s. Bound (Anchor s) (Binding s)

instance Show Bound where
  showsPrec d (Bound a b) = showParen (d > 10)
    (showString "Bound " . showsPrec 11 a . showChar ' ' . showsPrec 11 b)

at :: Anchor s -> Binding s -> Bound
at = Bound


-- * Checking a binding against a form

-- | The E-SHAPE check of a binding with its modifiers in effect: @cols a b@
--   projects the form onto the two columns, @as@ demotes the finding to a
--   Warning, and @enumInDescription@ asks only for a Description column.
bindingAccepts :: Binding s -> Form -> Maybe Finding
bindingAccepts = go Error
  where
    go :: Severity -> Binding t -> Form -> Maybe Finding
    go sev b form = case b of
      WithCols a c inner          -> either (Just . atForm sev form) (go sev inner) (projectColumns a c form)
      WithAs inner                -> go Warning inner form
      WithErratum _ _ inner       -> go sev inner form
      WithWaiver _ _ _ inner      -> go sev inner form
      WithRangeWords _ inner      -> go sev inner form
      WithExpand inner            -> go sev inner form
      WithLayout _ inner          -> go sev inner form
      WithBits _ _ inner          -> go sev inner form
      WithEnumInDescription inner -> description sev form <|> go sev inner form { fCols = 2, fHeaderNorm = ["enumerationvalue", "description"] }
      _                           -> acceptsWith sev (bindingShape b) form
    description sev form
      | "description" `elem` fHeaderNorm form = Nothing
      | otherwise = Just (atForm sev form "enumInDescription expects a Description column")
    atForm sev form = Finding sev "E-SHAPE" (Just (fSpec form)) (fId form) (fPage form)


-- * The comment vocabulary

-- | @doc core201@: the document anchor, naming its specification.
doc :: SpecId -> Binding 'Doc
doc = DocB

-- | The names @rule@ accepts: a binding whose shape names a rule (@rule
--   typeRow@) or a 'RuleName' value for the rules without one of their own
--   (@rule terminology@). A name that is neither is a type error in the
--   generated module, which is the point.
class RuleToken r where
  ruleNameOf :: r -> RuleName

instance RuleToken RuleName                   where ruleNameOf = id
instance RuleToken (Binding 'TypeRow)         where ruleNameOf _ = RuleName "typeRow"
instance RuleToken (Binding 'EnumValues)      where ruleNameOf _ = RuleName "enumValues"
instance RuleToken (Binding 'TableDesc)       where ruleNameOf _ = RuleName "tableDesc"
instance RuleToken (Binding 'ColumnsAddition) where ruleNameOf _ = RuleName "columnsAddition"
instance RuleToken (Binding 'TableUIDs)       where ruleNameOf _ = RuleName "tableUIDs"
instance RuleToken (Binding 'UidTable)        where ruleNameOf _ = RuleName "uidTable"
instance RuleToken (Binding 'StatusCodes)     where ruleNameOf _ = RuleName "statusCodes"
instance RuleToken (Binding 'Codes)           where ruleNameOf _ = RuleName "codes"
instance RuleToken (Binding 'FieldValue)      where ruleNameOf _ = RuleName "fieldValue"
instance RuleToken (Binding 'FieldType)       where ruleNameOf _ = RuleName "fieldType"
instance RuleToken (Binding 'ByteFieldValue)  where ruleNameOf _ = RuleName "byteFieldValue"
instance RuleToken (Binding 'ByteGrid)        where ruleNameOf _ = RuleName "byteGrid"
instance RuleToken (Binding 'Feature)         where ruleNameOf _ = RuleName "feature"
instance RuleToken (Binding 'ConstValues)     where ruleNameOf _ = RuleName "constValues"
instance RuleToken (Binding 'Ranges)          where ruleNameOf _ = RuleName "ranges"
instance RuleToken (Binding 'Preconfig)       where ruleNameOf _ = RuleName "preconfig"
instance RuleToken (Binding 'Sig)             where ruleNameOf _ = RuleName "sig"

-- | @rule typeRow@ on a heading or the document: the rule governs the subtree.
rule :: (RuleToken r, RuleBearing s) => r -> Binding s
rule = RuleB . ruleNameOf

-- | The rules of design section 3.4 that no binding's shape names.
tokenEncoding, terminology :: RuleName
tokenEncoding = RuleName "tokenEncoding"
terminology   = RuleName "terminology"

-- | Every rule name of design section 3.4, as @rule@ accepts them.
knownRules :: [RuleName]
knownRules = map RuleName
  [ "typeRow", "enumValues", "tableDesc", "columnsAddition", "tableUIDs", "uidTable"
  , "statusCodes", "codes", "fieldValue", "fieldType", "byteFieldValue", "byteGrid"
  , "feature", "constValues", "ranges", "preconfig", "tokenEncoding", "terminology", "sig" ]

typeRow :: Binding 'TypeRow
typeRow = TypeRowB

enumValues :: Binding 'EnumValues
enumValues = EnumValuesB

tableDesc :: Binding 'TableDesc
tableDesc = TableDescB

columnsAddition :: Binding 'ColumnsAddition
columnsAddition = ColumnsAdditionB

tableUIDs :: Binding 'TableUIDs
tableUIDs = TableUIDsB

uidTable :: Binding 'UidTable
uidTable = UidTableB

statusCodes :: Binding 'StatusCodes
statusCodes = StatusCodesB

codes :: Binding 'Codes
codes = CodesB

fieldValue :: Binding 'FieldValue
fieldValue = FieldValueB

fieldType :: Binding 'FieldType
fieldType = FieldTypeB

byteFieldValue :: Binding 'ByteFieldValue
byteFieldValue = ByteFieldValueB

-- | A byte layout named by its caption.
byteGrid :: Binding 'ByteGrid
byteGrid = ByteGridB

-- | A feature descriptor: a byte grid captioned @… Feature Descriptor@.
feature :: Binding 'Feature
feature = FeatureB

constValues :: Binding 'ConstValues
constValues = ConstValuesB

ranges :: Binding 'Ranges
ranges = RangesB

preconfig :: Binding 'Preconfig
preconfig = PreconfigB

tokens :: Binding 'Tokens
tokens = TokensB

-- | @stateTable "Sync.table45"@
stateTable :: QName -> Binding 'StateTable
stateTable = StateTableB

propsNames :: Binding 'Props
propsNames = PropsNames

propsInitial :: Binding 'Props
propsInitial = PropsInitial

propsReq :: Binding 'PropsReq
propsReq = PropsReqB

legend :: Binding 'Legend
legend = LegendB

decision :: Binding 'Decision
decision = DecisionB

sig :: Binding 'Sig
sig = SigB

prose :: Binding 'Prose
prose = ProseB

-- | @behaviour "Base.get"@ on a method heading.
behaviour :: QName -> Binding 'Heading
behaviour = Behaviour

-- | @fails "Base.getFails"@ on a Fails list.
fails :: QName -> Binding 'Prose
fails = Fails

-- | @law "Sync.ifSendWhileBusy"@ on a normative sentence or list item: the
--   paragraph, a unit of its own shape ('Sentence'), is bound to the
--   hand-written law the name gives.
law :: QName -> Binding 'Sentence
law = Law

-- | @refines (core 42)@: the same shape as the target, with this table's differences.
refines :: KnownShape s => Ref -> Binding s
refines = Refines

-- | @narrows (core 26)@: the target's rows restricted to this table's.
narrows :: KnownShape s => Ref -> Binding s
narrows = Narrows

-- | @replaces [core 127, core 128]@: this table shadows the targets.
replaces :: KnownShape s => [Ref] -> Binding s
replaces = Replaces

-- | @extends (core 171)@: columns added to the target's table description.
extends :: Ref -> Binding 'ColumnsAddition
extends = Extends

core :: Int -> Ref
core = Core

opal :: Int -> Ref
opal = Opal

-- | @section "5.1.3"@, the grammar's @§ "5.1.3"@.
section :: Text -> Ref
section = Section

infixl 1 &
-- | @rule a & rule b@: independent rule tokens on one anchor.
(&) :: RuleBearing s => Binding s -> Binding s -> Binding s
(&) = Also

-- | @erratum scott ["Fractoin" ~> "Fraction"] typeRow@: Scott's correction of
--   the text as printed (design decision 13.5). There is no published
--   counterpart: the published set is the text as printed.
erratum :: ErrataSet -> [(Text, Text)] -> Binding s -> Binding s
erratum = WithErratum

-- | @waive "C-UID-UNIQUE" "T94 00 00 00 05 00 00 02 15" "reason" typeRow@: the
--   named check's Error on this table about exactly this subject is a Warning
--   carrying the reason. For C-UID-UNIQUE the subject is the other table of
--   the clash and the UID both print: the Error is waived only when exactly
--   those two type rows share exactly that UID. A waiver that waives nothing is
--   @W-WAIVER-UNUSED@, and the Error stands. C-UID-UNIQUE is about type rows,
--   so a waiver sits on a type row alone: on any other binding it is a type
--   error here and @E-MODIFIER@ in 'applyModifier'.
waive :: Text -> Text -> Text -> Binding 'TypeRow -> Binding 'TypeRow
waive = WithWaiver

-- | The checks a @waive@ may name.
waivableChecks :: [Text]
waivableChecks = ["C-UID-UNIQUE"]

-- | @cols 0 1 enumValues@: the two columns of a wider enumeration table.
cols :: Int -> Int -> Binding 'EnumValues -> Binding 'EnumValues
cols = WithCols

-- | @rangeWords ["to"] enumValues@: the words that spell a range besides @-@.
rangeWords :: [Text] -> Binding 'EnumValues -> Binding 'EnumValues
rangeWords = WithRangeWords

-- | @enumInDescription (replaces [core 102, core 103])@
enumInDescription :: Binding 'EnumValues -> Binding 'EnumValues
enumInDescription = WithEnumInDescription

-- | @expand preconfig@: expand the @(+XX)@ template rows.
expand :: Binding 'Preconfig -> Binding 'Preconfig
expand = WithExpand

-- | @layout "ComPacketHeader" byteGrid@: the layout's name, where the
--   caption's will not do (design section 3.4: "name from the caption;
--   @layout "Name"@ overrides"); on a byte grid, a feature descriptor or a
--   @Byte(s) | Field | Value(s)@ table.
layout :: LayoutShape s => Text -> Binding s -> Binding s
layout = WithLayout

-- | @bits 5 [(0, "KeyChangeZone"), (1, "IdScope")] feature@: the bit map of
--   byte 5, each listed bit a one-bit field of that name, the rest reserved:
--   the extent rule's declared fallback where a byte's cells are neither eight,
--   one, nor drawn over the header's bit columns (design example E8); on a byte
--   grid or a feature descriptor. Several bytes take several.
bits :: GridShape s => Int -> [(Int, Text)] -> Binding s -> Binding s
bits = WithBits

-- | @as \@'Codes codes@: the form is odd and known to be; E-SHAPE is a Warning.
as :: forall s. Binding s -> Binding s
as = WithAs

infix 4 ~>
-- | An erratum pair, @"Fractoin" ~> "Fraction"@.
(~>) :: Text -> Text -> (Text, Text)
(~>) = (,)

scott :: ErrataSet
scott = Scott

core201, opal230 :: SpecId
core201 = SpecId "core-2.01"
opal230 = SpecId "opal-2.30"

-- | The specifications a @doc@ token may name, by their grammar name.
knownSpecs :: [(Text, SpecId)]
knownSpecs = [ (specName s, s) | s <- [core201, opal230] ]

-- | The grammar's name for a specification: @core-2.01@ is @core201@.
specName :: SpecId -> Text
specName = T.filter C.isAlphaNum . specIdText

specByName :: Text -> Maybe SpecId
specByName n = lookup n knownSpecs
