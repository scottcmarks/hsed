{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.Feature
Description : The feature rule: a feature descriptor's record, its code, version, length, layout and allowances
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E8. @feature@ fires on a byte grid captioned
@… Feature Descriptor@ (the @Feature@ shape; Core Tables 42 TPer Feature
Descriptor and 43 Locking Feature Descriptor, Table 41's @Feature Descriptor
Template Format@ staying @byteGrid@'s; Opal Tables 3-6 and 8) and refines
@byteGrid@. The caption picks out those tables across a spec, so the rule
rides on the @doc@ anchor (design 13.24). A descriptor is read with the
shared readers:

* its grid ("Hsed.Spec.Readers.Grid": the extent rule, a @bits k [...]@
  modifier as its fallback) and its layout and record
  ("Hsed.Spec.Readers.Record".'Structure', as @byteGrid@'s), the layout named
  by the caption's title, the record by the title less its last word
  (@TPer Feature Descriptor@ gives @TPerFeature@, Opal's @Level 0 Discovery -
  TPer Feature Descriptor@ @Level0DiscoveryTPerFeature@) or by @layout
  \"Name\"@ as written;
* its pins ("Hsed.Spec.Readers.Pins", @The \<Field\> field SHALL be set to
  \<V\>.@ in its section and their subsections, or Opal's bullets @\<Field\> =
  \<V\>@), matched to its fields by name, else by a unique word prefix
  ('Hsed.Spec.Readers.Pins.pinTarget': Opal's @Number of Locking SP Admin
  Authorities@ names Table 6's @… Supported@): an exact pin makes its field a
  constant of the layout, any other value is what the spec allows the field
  ('allowanceOf': @VU@, a version @or any version …@, a minimum @N or
  larger@, or the text as printed);
* the general format its header conforms to: the table a sentence of the spec
  names in @\<descriptors\> SHALL conform to the general format defined in
  Table \<n\>@, its subject the end of the descriptor's caption (Core 3.3.6.3:
  @All feature descriptors SHALL conform to the general format defined in Table
  41.@), or, where the spec prints no such sentence (Opal), the base's, the
  whole lookup run in the base Context (@hsedc --base@, Core's for an Opal
  run). The template's fixed fields are the descriptor's header: its own
  fields over those bytes lie at the template's fields' bits, each named as
  the template's or with a name holding it (Opal Table 6's @Feature Descriptor
  Version Number@ over Table 41's @Version@), or over bits the template
  reserves (Table 6's @SSC Minor Version Number@, a Note); its fields at the
  template's fields besides reserved ones are, in order, the code, the
  version and the length of "Hsed.Core.Feature".'Hsed.Core.Feature.Feature',
  by the descriptor's own names;
* its code: its pin's, else the constant its code field's cell prints
  (Opal's @Feature Code (0x0404)@), else that of the row of a code table (a
  table the @codes@ rule reads, its code column headed as the code field) that
  names the descriptor's section (Core Table 40: @0001h | TPer feature | See
  3.3.6.4@); where the spec's code tables name no section of it, a row of the
  base's that lists its code alone (Core Table 40 for Opal's 0x0001, 0x0002);
* its length: its pin's, else the count of its bytes after the header, which
  the layout then pins (Opal Table 8: 36 - 4, a Note).

Checks (besides the readers' @C-LAYOUT-EXTENT@, @C-LAYOUT-ENDIAN@,
@N-LAYOUT-BITS@, @W-LAYOUT-BITS-UNUSED@, @C-LAYOUT-TILE@, @C-GRID-NAME@,
@W-PINS-NONE@ and @C-PINS-WORDS@, each by the criterion every grid rule
shares):

* @C-FEATURE-TEMPLATE@ (Error): no sentence names the general format of the
  descriptor, the table it names reads as no byte grid whose fixed part has a
  code, a version and a length, or the descriptor's fields over the
  template's bytes are not the template's (by bits, and by name where the
  template names them); @N-FEATURE-HEADER@ (Note): a field of the
  descriptor's over bits the template reserves;
* @C-FEATURE-CODE@ (Error): the code its pin or its cell gives and the code a
  code table gives its section differ, its pin and its cell differ, two code
  rows give it two codes, or none gives one; @W-FEATURE-UNLISTED@ (Warning):
  no code table lists its section, nor, with a base, its code alone;
* @C-FEATURE-LENGTH@ (Error): its length pin is not the count of its bytes
  after the header; @N-FEATURE-LENGTH@ (Note): no pin gives its length, which
  the layout pins to that count;
* @C-FEATURE-PIN@ (Error): a field pinned to two values; @W-FEATURE-PIN@
  (Warning): a pin of its section that names no field of it (Opal 3.1.1.2's
  @ACK/NACK Supported@ named Table 3's @ACK/NAK Supported@ by neither rule,
  PDF 20, until a reviewed spelling read it so,
  'Hsed.Spec.Readers.Pins.reviewedSpellings');
* @C-FEATURE-FIELD@ (Error): a variable part, which a descriptor has none of
  (two fields a record cannot tell apart are @C-GRID-NAME@).

Compiled into 'featuresModule', for each descriptor: its layout (@layoutDecl@,
@tPerFeatureLayout :: Layout@, the pins as 'Hsed.Core.Layout.Pinned'
fields); and, when it tiles, its record over the layout as @byteGrid@'s are
printed (@TPerFeature@, one field for each field neither pinned nor reserved,
typed by its width, a flag @Bool@, named after the record and the field:
@tPerFeatureSyncSupported@, with its instance of
"Hsed.Core.Record".@LayoutRecord@), and its instance of
"Hsed.Core.Feature".'Hsed.Core.Feature.Feature': its code, version and length,
its header's size and length field, which say where a descriptor ends, and,
where the spec lists any, what it allows its fields
('Hsed.Core.Feature.featureAllowances', by field name: Opal's).
-}
module Hsed.Spec.Rules.Feature
  ( featureRule
  , featuresModule
  , featureModule
    -- * Reading the bound tables
  , FeatureRead (..)
  , Template (..)
  , Header (..)
  , frLayout
  , featureReads
  , readFeature
  , templateOf
  , conformingSubject
  , listedCodes
  , codeRowsOf
  , allowanceOf
  , featureStructureName
  ) where

import qualified Data.Text                as DT
import qualified RIO.Char                 as C
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import           RIO

import qualified Hsed.Core.Feature        as F
import           Hsed.Core.Layout         (Field (..), FieldKind (..), Layout (..), Tail (..))
import           Hsed.Spec.Anchor         (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common         (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.DSL            (SomeBinding (..), feature)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (CellIR (..), SpecIR (..), TableIR (..), XrefIR (..))
import           Hsed.Spec.Readers.Grid   (BitPos (..), Grid (..), GridField (..), GridProblem (..), fieldConstant,
                                           gridFindings, gridOf, readGrid)
import           Hsed.Spec.Readers.Layout (gridFieldKind, gridFields, gridTiling, hexAtWidth, layoutDecl, tileFindings,
                                           untiledNote)
import           Hsed.Spec.Readers.Pins   (Pin (..), PinValue (..), pinTarget, pinWord, pinsFindings, tablePins)
import           Hsed.Spec.Readers.Record (Structure (..), declScope, earlierNames, layoutValue, nameFindings, recordDecl,
                                           structure, typeName)
import           Hsed.Spec.Readers.Text   (Number (..), nameKey, numberExpr, parseNumber)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.Codes    (CodeCell (..), CodeRead (..), CodeRowRead (..), codeReads)
import           Hsed.Spec.Shape          (Shape (..), captionTitle, shapeMismatch)


-- | The rule.
featureRule :: RuleDef
featureRule = RuleDef
  { ruleName    = RuleName "feature"
  , bindsTo     = SomeBinding feature
  , exemplar    = (SpecId "core-2.01", TableId 42)
  , refinesRule = Just (RuleName "byteGrid")
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch Feature form)
  , ruleIdent   = const Nothing
  , ruleModule  = featuresModule
  , ruleCodes   =
      [ CodeDef "C-FEATURE-TEMPLATE" [Error]   (Defect "a feature descriptor whose header is not the general format the spec defines for it, or whose general format is no byte grid of a code, a version and a length")
      , CodeDef "N-FEATURE-HEADER"   [Note]    Bookkeeping
      , CodeDef "C-FEATURE-CODE"     [Error]   (Defect "a feature descriptor's code: its pin, its cell and the code table disagree, or none gives one")
      , CodeDef "W-FEATURE-UNLISTED" [Warning] (Suspect "a feature descriptor whose section no code table lists")
      , CodeDef "C-FEATURE-LENGTH"   [Error]   (Defect "a feature descriptor's length pin is not the count of its bytes after the header")
      , CodeDef "N-FEATURE-LENGTH"   [Note]    Bookkeeping
      , CodeDef "C-FEATURE-PIN"      [Error]   (Defect "a field of a feature descriptor pinned to two values")
      , CodeDef "W-FEATURE-PIN"      [Warning] (Suspect "a pin of a feature descriptor's section naming no field of the descriptor")
      , CodeDef "C-FEATURE-FIELD"    [Error]   (Defect "a feature descriptor with a variable part") ]
  , ruleChecks  = \ctx bts -> let frs = map (readFeature ctx) bts in concatMap (checkFeature ctx frs) frs
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.Features@.
featuresModule :: Gen.GenModule
featuresModule = Gen.GenModule
  { Gen.leaf        = "Features"
  , Gen.description = "Feature descriptors: a record per descriptor, its code, version, length and layout"
  , Gen.notes       = [ "Each descriptor is a Layout (Hsed.Core.Layout), its header and every field the spec pins"
                      , "constants of it, and a record of its other fields over it (Hsed.Core.Record), flags as Bool,"
                      , "with an instance of Feature (Hsed.Core.Feature): its code, version, length and header." ] }

-- | The runtime module of the records' class, which this rule alone imports.
featureModule :: Text
featureModule = "Hsed.Core.Feature"


-- * Reading the bound tables

-- | The general format a sentence of a spec defines for a kind of descriptor.
data Template = Template
  { tmSpec     :: !SpecId        -- ^ the spec whose table it is: the descriptor's, or its base's
  , tmNumber   :: !Int           -- ^ its table's number
  , tmSentence :: !XrefIR        -- ^ the sentence naming it
  , tmFields   :: ![GridField]   -- ^ its fixed fields, in order: the header
  , tmSize     :: !Int           -- ^ the header's bytes
  }
  deriving (Eq, Show)

-- | A descriptor's header fields, by the descriptor's own names: those at
--   the template's code, version and length, and those over bits the
--   template reserves.
data Header = Header
  { hdCode    :: !Text
  , hdVersion :: !Text
  , hdLength  :: !Text
  , hdOwn     :: ![Text]   -- ^ fields over the template's reserved bits (Opal Table 6's SSC Minor Version Number)
  }
  deriving (Eq, Show)

-- | One descriptor read under the errata set.
data FeatureRead = FeatureRead
  { frBound      :: !BoundTable
  , frSection    :: !(Maybe Text)
  , frGrid       :: !Grid
  , frPins       :: !(Either Text [Pin])       -- ^ the pins of its section ('tablePins')
  , frMatched    :: ![(Text, Pin)]             -- ^ each pin naming a field of it ('pinTarget'), with the field's name
  , frPinned     :: !(Map Text [Pin])          -- ^ the exact pins of each field, by its name's key
  , frUnmatched  :: ![Pin]                     -- ^ pins naming no field of it
  , frTemplate   :: !(Either Text Template)
  , frHeader     :: !(Either Text Header)      -- ^ the header, when it conforms to the template
  , frListed     :: ![(CodeRead, CodeRowRead, Number)]  -- ^ the code rows naming its section
  , frListedBase :: ![(CodeRead, CodeRowRead, Number)]  -- ^ where none does, the base's code rows listing its code
  , frCellCode   :: !(Maybe Number)            -- ^ the code its code field's cell prints (Opal's @Feature Code (0x0404)@)
  , frCode       :: !(Maybe Number)            -- ^ the code: its pin's, else its cell's, else the code table's
  , frLength     :: !(Maybe Integer)           -- ^ the length the layout pins where no pin gives one: its bytes after the header
  , frAllowances :: ![(Text, F.Allowance)]     -- ^ what the spec allows each field it lists a value of other than an exact one
  , frStructure  :: !Structure                 -- ^ its layout, its pins (and its code and length) as constants, and its record's name
  }

-- | A descriptor's layout.
frLayout :: FeatureRead -> Layout
frLayout = stLayout . frStructure

-- | Every descriptor bound to the rule's shape.
featureReads :: Context -> [FeatureRead]
featureReads ctx = map (readFeature ctx) (boundTables (Just Feature) ctx)

-- | Read one descriptor.
readFeature :: Context -> BoundTable -> FeatureRead
readFeature ctx bt = FeatureRead
  { frBound      = bt
  , frSection    = sec
  , frGrid       = grid
  , frPins       = pins
  , frMatched    = matched
  , frPinned     = pinned
  , frUnmatched  = [ p | p <- allPins, isNothing (pinTarget names p) ]
  , frTemplate   = template
  , frHeader     = header
  , frListed     = listed
  , frListedBase = listedBase
  , frCellCode   = cellCode
  , frCode       = code
  , frLength     = derived
  , frAllowances = allowances
  , frStructure  = st { stConstants = constants }
  }
  where
    title = fromMaybe "" ((btRead bt).caption >>= captionTitle)
    sec = sectionOf ctx bt
    grid = gridOf bt
    pins = tablePins ctx bt
    allPins = fromRight [] pins
    -- the layout's names of the grid's fields, which a pin names
    names = L.nub [ fieldName f | (f, _) <- gridFields grid ]
    matched = [ (n, p) | p <- allPins, Just n <- [pinTarget names p] ]
    pinned = Map.fromListWith (flip (<>)) [ (nameKey n, [p]) | (n, p) <- matched, PinExact _ <- [pinValue p] ]
    template = templateOf ctx title
    header = template >>= conforms (cxSpec ctx) grid
    listed = case (header, sec) of
      (Right h, Just s) -> listedCodes ctx (hdCode h) s
      _                 -> []
    pinOf = agreedPin pinned
    gridField k = L.find ((== nameKey k) . nameKey . gfBase) (gFields grid)
    -- the constant the code field's cell prints after its name, as printed
    cellCode = do
      h <- either (const Nothing) Just header
      gf <- gridField (hdCode h)
      v <- fieldConstant gf
      pure (fromMaybe (Number v Nothing) (mfilter ((== v) . nValue) (gfNote gf >>= parseNumber)))
    code = case header of
      Right h -> pinOf (hdCode h) <|> cellCode <|> case L.nubBy ((==) `on` nValue) [ n | (_, _, n) <- listed ] of
        [n] -> Just n
        _   -> Nothing
      Left _  -> Nothing
    -- where the spec's code tables name no section of it, the base's rows that list its code alone
    listedBase = case (listed, cxBase ctx, template, code) of
      ([], Just base, Right tm, Just n) -> maybe [] (\k -> codeRowsOf base k (nValue n)) (templateCode tm)
      _                                 -> []
    codeKey = either (const Nothing) (Just . nameKey . hdCode) header
    lengthKey = either (const Nothing) (Just . nameKey . hdLength) header
    -- a length no pin of the section's (read) pins gives and the cell does not print: the bytes after the header
    -- (Opal Table 8: 36 - 4); where the pins cannot be read, none is derived (C-PINS-WORDS)
    derived = case (header, template) of
      (Right h, Right tm) | isRight pins, isNothing (pinOf (hdLength h)), isNothing (gridField (hdLength h) >>= fieldConstant)
                         , all ((/= Reserved) . gridFieldKind) (gridField (hdLength h)) -> Just (toInteger (gSize grid - tmSize tm))
      _ -> Nothing
    st = structure bt (fromMaybe (featureStructureName title) (mLayout (btMods bt))) title grid pin
    pin (f, _) = case (fieldKind f, pinOf (fieldName f)) of
      (Reserved, _)  -> f
      (_, Just n)    -> f { fieldKind = Pinned (nValue n) }
      _ | Just (nameKey (fieldName f)) == codeKey, Just n <- code     -> f { fieldKind = Pinned (nValue n) }
        | Just (nameKey (fieldName f)) == lengthKey, Just n <- derived -> f { fieldKind = Pinned n }
        | otherwise  -> f
    -- each record field (neither pinned nor reserved) a pin lists a value of other than an exact one, in the layout's order
    allowances = [ (fieldName f, a) | (f, _) <- stFields st, recordField (fieldKind f)
                                    , a : _ <- [ mapMaybe allowanceOf [ p | (n, p) <- matched, n == fieldName f ] ] ]
    recordField = \case
      Reserved -> False
      Pinned _ -> False
      _        -> True
    -- where each constant comes from: a pin of the section, the code table, the bytes after the header, or the cell
    constants = [ "the layout pins " <> T.intercalate ", " ps | let ps = mapMaybe source (stFields st), not (null ps) ]
                <> [ "the constants the table prints are the layout's: " <> T.intercalate ", " ps
                   | let ps = [ gfName gf | (f, gf) <- stFields st, Pinned _ <- [fieldKind f], isNothing (source (f, gf)) ], not (null ps) ]
    source (f, _) = case (fieldKind f, Map.findWithDefault [] (nameKey (fieldName f)) pinned) of
      (Pinned _, p : _) -> Just (fieldName f <> " to " <> pinText p <> " (" <> pinSection p <> ", PDF " <> tshow (pinPage p + 1) <> ")")
      (Pinned v, [])
        | Just (nameKey (fieldName f)) == codeKey, isNothing cellCode ->
            listToMaybe [ fieldName f <> " to " <> codeCell cr r <> " (" <> codeRowRef cr r <> ")" | (cr, r, _) <- listed ]
        | Just (nameKey (fieldName f)) == lengthKey, isJust derived ->
            Just (fieldName f <> " to " <> hexAtWidth (fieldWidth f) v <> " (its bytes after the header, which no pin gives)")
      _ -> Nothing

-- | The template a descriptor's caption title names: the table of the first
--   sentence of the spec saying that descriptors of a kind its title ends
--   with conform to the general format a table defines, read as a byte grid;
--   its fixed fields are the header. Where the spec prints no such sentence,
--   the base's (Core's for an Opal run), the whole lookup run in the base
--   Context: its sentences, its tables, its binding of the template. Left why
--   there is none.
templateOf :: Context -> Text -> Either Text Template
templateOf ctx title = fromMaybe (Left none) (templateIn ctx <|> (cxBase ctx >>= templateIn))
  where
    none = "no sentence of the spec" <> maybe "" (\b -> " or of its base, " <> specIdText (cxSpec b) <> ",") (cxBase ctx)
           <> " says which general format a " <> title <> " conforms to"
    templateIn c = case [ x | x <- (cxIR c).xrefs, Just subject <- [conformingSubject x], subject `T.isSuffixOf` T.toLower title ] of
      [] -> Nothing
      x : _ -> Just $ case Map.lookup (TableId x.number) (cxTables c) of
        Nothing -> Left (sentence c x <> " names no numbered table")
        Just t ->
          let g = maybe (readGrid [] t) gridOf (L.find ((== TableId x.number) . btAnchor) (boundTables (Just ByteGrid) c))
              fixed = inGridOrder [ f | f <- gFields g, isJust (gfLast f) ]
          in case [ gpMessage p | p <- gProblems g, gpSeverity p == Error ] of
               m : _ -> Left (sentence c x <> ", which does not read as a byte grid: " <> m)
               [] | null fixed -> Left (sentence c x <> ", which has no fixed fields")
                  | otherwise  -> Right (Template (cxSpec c) x.number x fixed (gSize g))
    sentence :: Context -> XrefIR -> Text
    sentence c x = (if cxSpec c == cxSpec ctx then "" else specIdText (cxSpec c) <> " ")
                   <> "PDF " <> tshow (x.page + 1) <> " says " <> tshow x.sentence

-- | The template's table as a descriptor of a spec names it: @Table 41@, or
--   @core-2.01 Table 41@ from another spec (its base).
templateRef :: SpecId -> Template -> Text
templateRef sid tm = (if tmSpec tm == sid then "" else specIdText (tmSpec tm) <> " ") <> "Table " <> tshow (tmNumber tm)

-- | The template's name of its code field: its first field besides reserved ones.
templateCode :: Template -> Maybe Text
templateCode tm = listToMaybe [ gfBase f | f <- tmFields tm, gridFieldKind f /= Reserved ]

-- | The kind of descriptor a sentence says conforms to the general format of
--   a table, lowered and singular: @All feature descriptors SHALL conform to
--   the general format defined in Table 41.@ gives @feature descriptor@.
conformingSubject :: XrefIR -> Maybe Text
conformingSubject x = case DT.breakOn conforming x.sentence of
  (before, after) | not (T.null after), T.takeWhile C.isDigit (T.drop (T.length conforming) after) == tshow x.number ->
    case dropWhile (== "all") (T.words (T.toLower before)) of
      [] -> Nothing
      ws -> let (front, end) = L.splitAt (length ws - 1) ws in Just (T.unwords (front <> map singular end))
  _ -> Nothing
  where
    conforming = " SHALL conform to the general format defined in Table "
    singular w = fromMaybe w (T.stripSuffix "s" w)

-- | The header of a descriptor of a spec: its fields over the template's
--   bytes lie at the bits of the template's fields, each named as the
--   template's field or with a name holding its words (Opal Table 6's
--   @Feature Descriptor Version Number@ at Table 41's @Version@), or over bits
--   the template reserves, any field (Table 6's @SSC Minor Version Number@,
--   which the header lists as the descriptor's own). The code, version and
--   length are the descriptor's fields at the template's fields besides
--   reserved ones, in order, by the descriptor's names.
conforms :: SpecId -> Grid -> Template -> Either Text Header
conforms sid g tm
  | map bits own /= map bits (tmFields tm) || not (all fits pairs) =
      Left ("its fields over bytes 0 to " <> tshow (tmSize tm - 1) <> " are " <> describe own
            <> ", not those of " <> templateRef sid tm <> ", " <> describe (tmFields tm))
  | otherwise = case [ gfBase f | (f, t) <- pairs, gridFieldKind t /= Reserved ] of
      [c, v, l] -> Right (Header c v l [ gfBase f | (f, t) <- pairs, gridFieldKind t == Reserved, gridFieldKind f /= Reserved ])
      _         -> Left (templateRef sid tm <> "'s header has " <> tshow (length named)
                         <> " fields besides reserved ones (" <> T.intercalate ", " named <> "), not a code, a version and a length")
  where
    own = inGridOrder [ f | f <- gFields g, Just l <- [gfLast f], bpByte l < tmSize tm ]
    pairs = zip own (tmFields tm)
    named = [ gfBase t | t <- tmFields tm, gridFieldKind t /= Reserved ]
    bits f = (gfFirst f, gfLast f)
    -- a field at a template field's bits: any over reserved bits; else no reserved one, its name holding the template's
    fits (f, t)
      | gridFieldKind t == Reserved = True
      | otherwise = gridFieldKind f /= Reserved && wordsOf (gfBase t) `L.isInfixOf` wordsOf (gfBase f)
    wordsOf = T.words . T.toLower
    describe fs = T.intercalate ", " [ gfBase f <> " " <> span' f | f <- fs ]
    span' f = pos (gfFirst f) <> maybe "" (("-" <>) . pos) (gfLast f)
    pos (BitPos b i) = tshow b <> "." <> tshow i

-- | The code rows that give a section a code: a row of a table the @codes@
--   rule reads, whose code column is headed by a field's name, one value in
--   it, and one of whose other cells names the section (@See 3.3.6.4@).
listedCodes :: Context -> Text -> Text -> [(CodeRead, CodeRowRead, Number)]
listedCodes ctx codeField sec =
  [ (cr, r, n) | Right cr <- codeReads ctx, headedBy codeField cr, r <- crRows cr, CodeOne n <- [rrCode r], any (names . T.words) (rrCells r) ]
  where names = any ((== sec) . T.dropWhileEnd (`elem` (".,;:)" :: String)))

-- | The code rows that list a code alone: a row of a table the @codes@ rule
--   reads, whose code column is headed by a field's name, holding that one
--   value (Core Table 40's @0001h | TPer feature@ lists Opal's TPer
--   descriptor's code, which Opal's tables do not).
codeRowsOf :: Context -> Text -> Integer -> [(CodeRead, CodeRowRead, Number)]
codeRowsOf ctx codeField v =
  [ (cr, r, n) | Right cr <- codeReads ctx, headedBy codeField cr, r <- crRows cr, CodeOne n <- [rrCode r], nValue n == v ]

-- | The code rows that give a code only within a range or above a bound
--   (Core Table 40's @0003h – 00FFh | Reserved@ for Opal's 0x0003).
codeRangesOf :: Context -> Text -> Integer -> [(CodeRead, CodeRowRead)]
codeRangesOf ctx codeField v =
  [ (cr, r) | Right cr <- codeReads ctx, headedBy codeField cr, r <- crRows cr, within (rrCode r) ]
  where
    within = \case
      CodeBetween a b -> nValue a <= v && v <= nValue b
      CodeAbove a     -> v > nValue a
      _               -> False

-- | Is a code table's code column headed by a field's name?
headedBy :: Text -> CodeRead -> Bool
headedBy codeField cr = maybe False ((== nameKey codeField) . nameKey) (listToMaybe (drop (crColumn cr) (crHeader cr)))

-- | What a pin allows its field where its value is not exact: vendor unique,
--   a version the device chooses, a minimum, or the text as printed (the
--   feature's state, @**@; a reference, @See section 3.1.1.3.1@). Nothing for
--   an exact pin, which makes the field a constant of the layout instead.
allowanceOf :: Pin -> Maybe F.Allowance
allowanceOf p = case pinValue p of
  PinExact _      -> Nothing
  PinVU           -> Just F.VendorUnique
  PinAnyVersion v -> Just (F.AnyVersion v)
  PinAtLeast v    -> Just (F.Minimum v)
  PinOther        -> Just (F.AsPrinted (pinText p))

-- | The structure a descriptor's caption title names: the title without its
--   last word (the @Descriptor@ the shape's captions end with): @TPer Feature
--   Descriptor@ gives @TPer Feature@, its record @TPerFeature@.
featureStructureName :: Text -> Text
featureStructureName name = case T.words name of
  ws@(_ : _ : _) -> T.unwords (take (length ws - 1) ws)
  _              -> name

-- | The value the exact pins of a field agree on, as printed (Nothing when
--   none pins it, or two disagree).
agreedPin :: Map Text [Pin] -> Text -> Maybe Number
agreedPin pinned k = case L.nub [ v | PinExact v <- map pinValue pins ] of
  [v] -> listToMaybe (mapMaybe (parseNumber . pinWord . pinText) pins) <|> Just (Number v Nothing)
  _   -> Nothing
  where pins = Map.findWithDefault [] (nameKey k) pinned

-- | A row of a code table: @Table 40 row 2 (PDF 69)@.
codeRowRef :: CodeRead -> CodeRowRead -> Text
codeRowRef cr r = "Table " <> maybe "?" tshow (btTable (crBound cr)).tableNumber <> " row " <> tshow (rrIndex r + 1)
                  <> maybe "" (\pg -> " (PDF " <> tshow (pg + 1) <> ")") (listToMaybe (mapMaybe (\c -> c.page) (rrRow r)))

-- | A code row's code as printed: @0001h@.
codeCell :: CodeRead -> CodeRowRead -> Text
codeCell cr r = maybe "" (\c -> T.strip c.text) (listToMaybe (drop (crColumn cr) (rrRow r)))

-- | Fields in the order the grid prints them: byte 0 first, bit 7 first in
--   each byte.
inGridOrder :: [GridField] -> [GridField]
inGridOrder = L.sortOn (\f -> (bpByte (gfFirst f), negate (bpBit (gfFirst f))))


-- * Checks

-- | A descriptor's findings, given every descriptor the rule reads (whose
--   names come before its own, and whose declarations' imports are its
--   module's).
checkFeature :: Context -> [FeatureRead] -> FeatureRead -> [Finding]
checkFeature ctx frs fr =
  gridFindings ctx bt (frGrid fr)
  <> tileFindings ctx bt (frGrid fr) (frLayout fr)
  <> nameFindings ctx (earlierNames (map frStructure frs) (frStructure fr))
       (declScope (concatMap (decls ctx (any (not . null . frAllowances) frs)) frs)) (frStructure fr)
  <> pinsFindings ctx bt name (length (frUnmatched fr) < length (fromRight [] (frPins fr)))
  <> map (atTable ctx bt) (template <> code <> lengthPin <> pinned <> fields <> notes)
  <> unmatched <> unlisted
  where
    bt = frBound fr
    name = layoutName (frLayout fr)
    secText = fromMaybe "its section" (frSection fr)
    template = case frHeader fr of
      Left why -> [ finding Error "C-FEATURE-TEMPLATE" (name <> ": " <> why) ]
      Right _  -> []
    listedValues = L.nub [ nValue n | (_, _, n) <- frListed fr ]
    code = case frHeader fr of
      Left _ -> []
      Right h
        | length listedValues > 1 ->
            [ finding Error "C-FEATURE-CODE" (name <> ": the code tables give " <> secText <> " the codes " <> T.intercalate " and " (map listedRef (frListed fr))) ]
        | Just (how, n) <- given h, [v] <- listedValues, nValue n /= v ->
            [ finding Error "C-FEATURE-CODE" (name <> ": its " <> hdCode h <> " is " <> how <> " " <> numberExpr n <> ", but "
                                              <> T.intercalate ", " (map listedRef (frListed fr))) ]
        | Just n <- agreedPin (frPinned fr) (hdCode h), Just c <- frCellCode fr, nValue n /= nValue c ->
            [ finding Error "C-FEATURE-CODE" (name <> ": its " <> hdCode h <> " is pinned to " <> numberExpr n
                                              <> ", but its cell prints " <> numberExpr c) ]
        | isNothing (frCode fr) ->
            [ finding Error "C-FEATURE-CODE" (name <> ": no pin gives its " <> hdCode h <> ", its cell prints none, and no code table lists " <> secText) ]
        | otherwise -> []
    -- the code the descriptor gives itself, and how
    given h = ((,) "pinned to" <$> agreedPin (frPinned fr) (hdCode h)) <|> ((,) "printed in its cell as" <$> frCellCode fr)
    lengthPin = case frHeader fr of
      Right h | Just f <- L.find ((== nameKey (hdLength h)) . nameKey . fieldName) (layoutFields (frLayout fr))
              , Pinned v <- fieldKind f, Right tm <- frTemplate fr
              , v /= toInteger (layoutSize (frLayout fr) - tmSize tm) ->
        [ finding Error "C-FEATURE-LENGTH" (name <> ": its " <> hdLength h <> " is pinned to " <> tshow v <> ", but "
                                            <> tshow (layoutSize (frLayout fr) - tmSize tm) <> " of its " <> tshow (layoutSize (frLayout fr))
                                            <> " bytes follow the " <> tshow (tmSize tm) <> "-byte header") ]
      _ -> []
    pinned =
      [ finding Error "C-FEATURE-PIN" (name <> ": " <> T.intercalate " and " [ pinField p <> " = " <> pinText p <> " (" <> pinSection p <> ")" | p <- ps ])
      | ps <- Map.elems (frPinned fr), length (L.nub [ v | p <- ps, PinExact v <- [pinValue p] ]) > 1 ]
    fields =
      [ finding Error "C-FEATURE-FIELD" (name <> ": the variable part " <> tshow (tailName t) <> " is no field of a descriptor's record")
      | Just t <- [layoutTail (frLayout fr)] ]
    notes =
      [ finding Note "N-FEATURE-HEADER" (name <> ": its " <> f <> " lies over bits " <> templateRef (cxSpec ctx) tm <> " reserves")
      | Right h <- [frHeader fr], Right tm <- [frTemplate fr], f <- hdOwn h ]
      <> [ finding Note "N-FEATURE-LENGTH" (name <> ": no pin gives its " <> hdLength h <> ": the layout pins it to " <> tshow n
                                            <> ", its " <> tshow (layoutSize (frLayout fr)) <> " bytes less the "
                                            <> tshow (tmSize tm) <> "-byte header")
         | Right h <- [frHeader fr], Right tm <- [frTemplate fr], Just n <- [frLength fr] ]
    unmatched =
      [ locatedAt (cxSpec ctx) (renderAnchorId (btAnchor bt)) (pinPage p + 1)
          (finding Warning "W-FEATURE-PIN" (name <> ": " <> pinSection p <> " pins " <> pinField p <> " (" <> pinText p <> "), no field of it"))
      | p <- frUnmatched fr ]
    unlisted = case (frHeader fr, frListed fr, frListedBase fr) of
      (Right _, [], []) | Just n <- frCode fr ->
        [ atTable ctx bt (finding Warning "W-FEATURE-UNLISTED" (name <> ": no code table lists " <> secText <> inBase n)) ]
      _ -> []
    -- with a base: no code table of the base lists the code alone either, and the range one gives it within, if any
    inBase n = case (cxBase ctx, frTemplate fr >>= maybe (Left "") Right . templateCode) of
      (Just b, Right k) ->
        ", nor does one of " <> specIdText (cxSpec b) <> " list its code " <> numberExpr n <> " alone"
        <> T.concat [ " (" <> codeRowRef cr r <> " gives it only within " <> codeCell cr r
                      <> T.concat [ ", " <> T.unwords (T.words c) | c <- take 1 (filter (not . T.null . T.strip) (rrCells r)) ] <> ")"
                    | (cr, r) <- take 1 (codeRangesOf b k (nValue n)) ]
      _ -> ""
    listedRef (cr, r, n) = codeRowRef cr r <> " gives " <> numberExpr n


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = concatMap one frs
  where
    frs = map (readFeature ctx) bts
    failures fr = L.nub [ Cm.code f | f <- checkFeature ctx frs fr, Cm.isError f ]
    -- whether an instance the module prints defines featureAllowances, which its import then names
    allowing = any (\fr -> null (failures fr) && stTiles (frStructure fr) && not (null (frAllowances fr))) frs
    one fr = case failures fr of
      []     -> map Emit (decls ctx allowing fr)
      failed -> [ Excluded (btAnchor (frBound fr)) (T.intercalate ", " failed <> " failed") ]

-- | A descriptor's declarations: its layout; when it tiles, its record over
--   it and its instance of @Feature@, given whether an instance of the module
--   defines @featureAllowances@ (which every one's import then names).
decls :: Context -> Bool -> FeatureRead -> [Gen.Decl]
decls ctx allowing fr =
  layoutDecl (boundProvenance ctx (frBound fr)) (layoutValue st) (frLayout fr) (untiledNote (gridTiling (frGrid fr) (frLayout fr)))
  : (if stTiles st then [recordDecl ctx st, featureDecl ctx allowing fr] else [])
  where st = frStructure fr

-- | A descriptor's instance of @Feature@, after its record: its code,
--   version and length, each at its header field's width as the layout
--   prints its pins (@0x0001@, @0x1@, @0x0c@), its header, and what the spec
--   allows its fields where it lists any (@featureAllowances@, by field name,
--   a number at its field's width), citing where they come from in comments
--   made ASCII.
featureDecl :: Context -> Bool -> FeatureRead -> Gen.Decl
featureDecl ctx allowing fr = (Gen.decl p name body)
  { Gen.declRank    = 2
  , Gen.declImports = [ Gen.ImportSome featureModule (["Feature (" <> T.intercalate ", " methods <> ")", "FeatureHeader (..)"]
                                                      <> [ "Allowance (..)" | allowing ])
                      , Gen.ImportSome "Prelude" ["Maybe (..)"] ] }
  where
    p = boundProvenance ctx (frBound fr)
    name = typeName (frStructure fr)
    methods = ["featureCode", "featureVersion", "featureLength", "featureHeader"] <> [ "featureAllowances" | allowing ]
    header = either (const Nothing) Just (frHeader fr)
    body = [ Gen.provenanceComment p <> ": its header" ] <> map Gen.ascii provenance <> [ "instance Feature " <> name <> " where" ] <> constants
    headerPins = [ (pinField q, q) | Just h <- [header], k <- [hdCode h, hdVersion h, hdLength h]
                                   , q <- take 1 (Map.findWithDefault [] (nameKey k) (frPinned fr)) ]
    provenance =
      [ "-- the header of " <> name <> (if null headerPins then ": no pin" else " as pinned by "
          <> T.intercalate ", " (L.nub [ pinSection q <> " (PDF " <> tshow (pinPage q + 1) <> ")" | (_, q) <- headerPins ])
          <> ": " <> T.intercalate ", " [ f <> " = " <> pinText q | (f, q) <- headerPins ]) ]
      <> [ "-- the code " <> codeRowRef cr r <> " gives " <> fromMaybe "its section" (frSection fr) <> ": " <> codeCell cr r
         | (cr, r, _) <- frListed fr ]
      <> [ "-- its code, as " <> specIdText (cxSpec b) <> " " <> codeRowRef cr r <> " lists it: " <> codeCell cr r
         | Just b <- [cxBase ctx], (cr, r, _) <- frListedBase fr ]
      <> [ "-- its " <> hdCode h <> " as its cell prints it: " <> numberExpr n
         | Just h <- [header], Just n <- [frCellCode fr], isNothing (agreedPin (frPinned fr) (hdCode h)) ]
      <> [ "-- its " <> hdLength h <> " the count of its bytes after the header, which no pin gives: " <> tshow n
         | Just h <- [header], Just n <- [frLength fr] ]
      <> [ "-- its general format " <> templateRef (cxSpec ctx) tm <> " (PDF " <> tshow ((tmSentence tm).page + 1) <> "): "
           <> tshow (tmSize tm) <> " bytes, " <> hdLength h <> " the count of the bytes after them"
         | Right tm <- [frTemplate fr], Just h <- [header] ]
      <> [ "-- what " <> T.intercalate ", " (L.nub [ pinSection q <> " (PDF " <> tshow (pinPage q + 1) <> ")" | (n, q) <- frMatched fr, n `elem` map fst (frAllowances fr) ])
           <> " allows the fields it lists a value of other than an exact one"
         | not (null (frAllowances fr)) ]
    -- a constant at the width of the header field it is (as printed when the layout has no such field)
    printedAt k n = maybe (numberExpr n) (\f -> hexAtWidth (fieldWidth f) (nValue n)) (L.find ((== nameKey k) . nameKey . fieldName) (layoutFields (frLayout fr)))
    pinned k = (,) k <$> agreedPin (frPinned fr) k
    justOr = maybe "Nothing" (\(k, n) -> "Just " <> printedAt k n)
    lengthValue = (header >>= pinned . hdLength) <|> ((\h n -> (hdLength h, Number n Nothing)) <$> header <*> frLength fr)
    constants =
      [ "  featureCode _    = " <> maybe "0" (printedAt (maybe "" hdCode header)) (frCode fr)
      , "  featureVersion _ = " <> justOr (header >>= pinned . hdVersion)
      , "  featureLength _  = " <> justOr lengthValue
      , "  featureHeader _  = FeatureHeader " <> either (const "0") (tshow . tmSize) (frTemplate fr) <> " " <> tshow (maybe "" hdLength header) ]
      <> case frAllowances fr of
           []  -> []
           as' -> "  featureAllowances _ =" : Gen.listLines "    " [ "(" <> tshow k <> ", " <> allowanceExpr k a <> ")" | (k, a) <- as' ]
    allowanceExpr k = \case
      F.VendorUnique -> "VendorUnique"
      F.AnyVersion v -> "AnyVersion " <> printedAt k (Number v Nothing)
      F.Minimum v    -> "Minimum " <> printedAt k (Number v Nothing)
      F.AsPrinted t  -> "AsPrinted " <> tshow t
