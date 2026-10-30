{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.Feature
Description : The feature rule: a feature descriptor's record, its code, version, length and layout
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E8. @feature@ fires on a byte grid captioned
@… Feature Descriptor@ (the @Feature@ shape; Core Tables 42 TPer Feature
Descriptor and 43 Locking Feature Descriptor, Table 41's @Feature Descriptor
Template Format@ staying @byteGrid@'s) and refines @byteGrid@. The caption
picks out those two across Core, so the rule rides on the @doc@ anchor (design
13.24). A descriptor is read with the shared readers:

* its grid ("Hsed.Spec.Readers.Grid": the extent rule, a @bits k [...]@
  modifier as its fallback) and its layout ("Hsed.Spec.Readers.Layout");
* its pins ("Hsed.Spec.Readers.Pins", @The \<Field\> field SHALL be set to
  \<V\>.@ in its section and their subsections), matched to its fields by
  name: an exact pin makes its field a constant of the layout;
* the general format its header conforms to: the table a sentence of the spec
  names in @\<descriptors\> SHALL conform to the general format defined in
  Table \<n\>@, its subject the end of the descriptor's caption (Core 3.3.6.3:
  @All feature descriptors SHALL conform to the general format defined in Table
  41.@). The template's fixed fields are the descriptor's header; its fields
  besides reserved ones are, in order, the code, the version and the length of
  "Hsed.Core.Feature".'Hsed.Core.Feature.Feature';
* the code a code table gives the descriptor's section: a row of a table the
  @codes@ rule reads ("Hsed.Spec.Rules.Codes") whose code column is headed as
  the template's code field and one of whose cells names the section (Core
  Table 40: @0001h | TPer feature | See 3.3.6.4@).

Checks (besides the readers' @C-LAYOUT-EXTENT@, @C-LAYOUT-ENDIAN@,
@N-LAYOUT-BITS@, @W-LAYOUT-BITS-UNUSED@, @C-LAYOUT-TILE@ and @W-PINS-NONE@):

* @C-FEATURE-TEMPLATE@ (Error): no sentence names the general format of the
  descriptor, the table it names reads as no byte grid whose fixed part has a
  code, a version and a length, or the descriptor's fields over the
  template's bytes are not the template's (by name and bits);
* @C-FEATURE-CODE@ (Error): the code its pin gives and the code a code table
  gives its section differ, two code rows give it two codes, or neither gives
  one; @W-FEATURE-UNLISTED@ (Warning): no code table lists its section;
* @C-FEATURE-LENGTH@ (Error): its length pin is not the count of its bytes
  after the header;
* @C-FEATURE-PIN@ (Error): a field pinned to two values; @W-FEATURE-PIN@
  (Warning): a pin of its section that names no field of it;
* @C-FEATURE-FIELD@ (Error): two fields a record cannot tell apart (one name,
  or one identifier), or a variable part.

Compiled into 'featuresModule', for each descriptor: its layout (@layoutDecl@,
@tPerFeatureDescriptor :: Layout@, the pins as 'Hsed.Core.Layout.Pinned'
fields), and a record named after the layout without its last word
(@TPerFeature@), one field for each field neither pinned nor reserved (a flag
@Bool@, a number @Natural@, bytes @ByteString@, named after the record and the
field: @tPerFeatureSyncSupported@), with its instance of
"Hsed.Core.Feature".'Hsed.Core.Feature.Feature'. The @layout "Name"@ modifier
renames the layout, and so the record.
-}
module Hsed.Spec.Rules.Feature
  ( featureRule
  , featuresModule
  , featureModule
    -- * Reading the bound tables
  , FeatureRead (..)
  , Template (..)
  , Header (..)
  , RecordField (..)
  , FieldType (..)
  , featureReads
  , readFeature
  , templateOf
  , conformingSubject
  , listedCodes
  , featureTypeName
  ) where

import qualified Data.Text              as DT
import qualified RIO.Char               as C
import qualified RIO.List               as L
import qualified RIO.Map                as Map
import qualified RIO.Text               as T
import           RIO

import           Hsed.Core.Layout       (Field (..), FieldKind (..), Layout (..), Tail (..), bitPosition, fieldEnd)
import           Hsed.Spec.Anchor       (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common       (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common       as Cm
import           Hsed.Spec.DSL          (SomeBinding (..), feature)
import qualified Hsed.Spec.Gen          as Gen
import           Hsed.Spec.IR           (CellIR (..), SpecIR (..), TableIR (..), XrefIR (..))
import           Hsed.Spec.Readers.Grid (BitPos (..), Grid (..), GridField (..), GridProblem (..), gridFindings, gridOf,
                                         readGrid)
import           Hsed.Spec.Readers.Layout (gridFieldKind, gridLayout, layoutDecl, tileFindings)
import           Hsed.Spec.Readers.Pins (Pin (..), PinValue (..), tablePins)
import           Hsed.Spec.Readers.Text (lowerIdent, titleIdent)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.Codes  (CodeCell (..), CodeRead (..), CodeRowRead (..), Number (..), codeReads,
                                         numberExpr, parseNumber)
import           Hsed.Spec.Shape        (Shape (..), captionTitle, shapeMismatch)


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
      , CodeDef "C-FEATURE-CODE"     [Error]   (Defect "a feature descriptor's code: its pin and the code table disagree, or neither gives one")
      , CodeDef "W-FEATURE-UNLISTED" [Warning] (Suspect "a feature descriptor whose section no code table lists")
      , CodeDef "C-FEATURE-LENGTH"   [Error]   (Defect "a feature descriptor's length pin is not the count of its bytes after the header")
      , CodeDef "C-FEATURE-PIN"      [Error]   (Defect "a field of a feature descriptor pinned to two values")
      , CodeDef "W-FEATURE-PIN"      [Warning] (Suspect "a pin of a feature descriptor's section naming no field of the descriptor")
      , CodeDef "C-FEATURE-FIELD"    [Error]   (Defect "fields of a feature descriptor a record cannot tell apart, or a variable part") ]
  , ruleChecks  = \ctx bts -> concatMap (checkFeature ctx . readFeature ctx) bts
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.Features@.
featuresModule :: Gen.GenModule
featuresModule = Gen.GenModule
  { Gen.leaf        = "Features"
  , Gen.description = "Feature descriptors: a record per descriptor, its code, version, length and layout"
  , Gen.notes       = [ "Each descriptor is a Layout (Hsed.Core.Layout), its header and every field the spec pins"
                      , "constants of it, and a record of its other fields, flags as Bool, with an instance of"
                      , "Feature (Hsed.Core.Feature) that encodes and decodes it." ] }

-- | The runtime module of the records' class, which this rule alone imports.
featureModule :: Text
featureModule = "Hsed.Core.Feature"


-- * Reading the bound tables

-- | The general format a sentence of the spec defines for a kind of descriptor.
data Template = Template
  { tmNumber   :: !Int           -- ^ its table's number
  , tmSentence :: !XrefIR        -- ^ the sentence naming it
  , tmFields   :: ![GridField]   -- ^ its fixed fields, in order: the header
  , tmSize     :: !Int           -- ^ the header's bytes
  }
  deriving (Eq, Show)

-- | A descriptor's header fields as the template names them.
data Header = Header
  { hdCode    :: !Text
  , hdVersion :: !Text
  , hdLength  :: !Text
  }
  deriving (Eq, Show)

-- | What a record field holds.
data FieldType = FlagField | NumberField | OctetsField
  deriving (Eq, Show)

-- | A field of the record: a field of the layout neither pinned nor reserved.
data RecordField = RecordField
  { rfName  :: !Text       -- ^ its name in the layout
  , rfIdent :: !Text       -- ^ the record's field
  , rfType  :: !FieldType
  , rfField :: !Field
  }
  deriving (Eq, Show)

-- | One descriptor read under the errata set.
data FeatureRead = FeatureRead
  { frBound     :: !BoundTable
  , frName      :: !Text                      -- ^ the layout's name: its @layout@ modifier's, else the caption's title
  , frSection   :: !(Maybe Text)
  , frGrid      :: !Grid
  , frPins      :: !(Either Text [Pin])       -- ^ the pins of its section ('tablePins')
  , frPinned    :: !(Map Text [Pin])          -- ^ the exact pins of each field, by its name's key
  , frUnmatched :: ![Pin]                     -- ^ pins naming no field of it
  , frTemplate  :: !(Either Text Template)
  , frHeader    :: !(Either Text Header)      -- ^ the header, when it conforms to the template
  , frListed    :: ![(CodeRead, CodeRowRead, Number)]  -- ^ the code rows naming its section
  , frCode      :: !(Maybe Number)            -- ^ the code: its pin's, else the code table's
  , frLayout    :: !Layout                    -- ^ its grid's layout, its pins (and its code) as constants
  , frFields    :: ![RecordField]
  }

-- | Every descriptor bound to the rule's shape.
featureReads :: Context -> [FeatureRead]
featureReads ctx = map (readFeature ctx) (boundTables (Just Feature) ctx)

-- | Read one descriptor.
readFeature :: Context -> BoundTable -> FeatureRead
readFeature ctx bt = FeatureRead
  { frBound     = bt
  , frName      = name
  , frSection   = sec
  , frGrid      = grid
  , frPins      = pins
  , frPinned    = pinned
  , frUnmatched = [ p | p <- allPins, key (pinField p) `notElem` fieldKeys ]
  , frTemplate  = template
  , frHeader    = header
  , frListed    = listed
  , frCode      = code
  , frLayout    = layout
  , frFields    = recordFields (featureTypeName name) layout
  }
  where
    title = fromMaybe "" ((btRead bt).caption >>= captionTitle)
    name = fromMaybe title (mLayout (btMods bt))
    sec = sectionOf ctx bt
    grid = gridOf bt
    pins = tablePins ctx bt
    allPins = fromRight [] pins
    fieldKeys = map (key . gfBase) (gFields grid) <> map (key . gfName) (gFields grid)
    pinned = Map.fromListWith (flip (<>)) [ (key (pinField p), [p]) | p <- allPins, PinExact _ <- [pinValue p], key (pinField p) `elem` fieldKeys ]
    template = templateOf ctx title
    header = template >>= conforms grid
    listed = case (header, sec) of
      (Right h, Just s) -> listedCodes ctx (hdCode h) s
      _                 -> []
    pinOf = agreedPin pinned
    code = case header of
      Right h -> pinOf (hdCode h) <|> case L.nubBy ((==) `on` nValue) [ n | (_, _, n) <- listed ] of
        [n] -> Just n
        _   -> Nothing
      Left _  -> Nothing
    codeKey = either (const Nothing) (Just . key . hdCode) header
    -- in the grid's order, bit 7 of byte 0 first, however the reader ordered a byte its bits modifier read
    layout = let l0 = gridLayout name grid in l0 { layoutFields = map pin (L.sortOn fieldStart (layoutFields l0)) }
    pin f = case (fieldKind f, pinOf (fieldName f)) of
      (Reserved, _)  -> f
      (_, Just n)    -> f { fieldKind = Pinned (nValue n) }
      _ | Just (key (fieldName f)) == codeKey, Just n <- code -> f { fieldKind = Pinned (nValue n) }
        | otherwise  -> f

-- | The template a descriptor's caption title names: the table of the first
--   sentence of the spec saying that descriptors of a kind its title ends
--   with conform to the general format a table defines, read as a byte grid;
--   its fixed fields are the header. Left why there is none.
templateOf :: Context -> Text -> Either Text Template
templateOf ctx title = case [ x | x <- (cxIR ctx).xrefs, Just subject <- [conformingSubject x], subject `T.isSuffixOf` T.toLower title ] of
  [] -> Left ("no sentence of the spec says which general format a " <> title <> " conforms to")
  x : _ -> case Map.lookup (TableId x.number) (cxTables ctx) of
    Nothing -> Left (sentence x <> " names no numbered table")
    Just t ->
      let g = maybe (readGrid [] t) gridOf (L.find ((== TableId x.number) . btAnchor) (boundTables (Just ByteGrid) ctx))
          fixed = inGridOrder [ f | f <- gFields g, isJust (gfLast f) ]
      in case [ gpMessage p | p <- gProblems g, gpSeverity p == Error ] of
           m : _ -> Left (sentence x <> ", which does not read as a byte grid: " <> m)
           [] | null fixed -> Left (sentence x <> ", which has no fixed fields")
              | otherwise  -> Right (Template x.number x fixed (gSize g))
  where
    sentence :: XrefIR -> Text
    sentence x = "PDF " <> tshow (x.page + 1) <> " says " <> tshow x.sentence

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

-- | The header of a descriptor, its fields over the template's bytes being
--   the template's fields (by name and bits): the names of its code, version
--   and length, the template's fields besides reserved ones, in order.
conforms :: Grid -> Template -> Either Text Header
conforms g tm
  | map shape own /= map shape (tmFields tm) =
      Left ("its fields over bytes 0 to " <> tshow (tmSize tm - 1) <> " are " <> describe own
            <> ", not those of Table " <> tshow (tmNumber tm) <> ", " <> describe (tmFields tm))
  | otherwise = case [ gfBase f | f <- tmFields tm, gridFieldKind f /= Reserved ] of
      [c, v, l] -> Right (Header c v l)
      named     -> Left ("Table " <> tshow (tmNumber tm) <> "'s header has " <> tshow (length named)
                         <> " fields besides reserved ones (" <> T.intercalate ", " named <> "), not a code, a version and a length")
  where
    own = inGridOrder [ f | f <- gFields g, Just l <- [gfLast f], bpByte l < tmSize tm ]
    shape f = (key (gfBase f), gfFirst f, gfLast f)
    describe fs = T.intercalate ", " [ gfBase f <> " " <> span' f | f <- fs ]
    span' f = pos (gfFirst f) <> maybe "" (("-" <>) . pos) (gfLast f)
    pos (BitPos b i) = tshow b <> "." <> tshow i

-- | The code rows that give a section a code: a row of a table the @codes@
--   rule reads, whose code column is headed by a field's name, one value in
--   it, and one of whose other cells names the section (@See 3.3.6.4@).
listedCodes :: Context -> Text -> Text -> [(CodeRead, CodeRowRead, Number)]
listedCodes ctx codeField sec =
  [ (cr, r, n)
  | Right cr <- codeReads ctx, maybe False ((== key codeField) . key) (listToMaybe (drop (crColumn cr) (crHeader cr)))
  , r <- crRows cr, CodeOne n <- [rrCode r], any (names . T.words) (rrCells r) ]
  where names = any ((== sec) . T.dropWhileEnd (`elem` (".,;:)" :: String)))

-- | The record of a layout's name: the name without its last word (the
--   @Descriptor@ the shape's captions end with), as an identifier:
--   @TPer Feature Descriptor@ gives @TPerFeature@.
featureTypeName :: Text -> Text
featureTypeName name = case T.words name of
  ws@(_ : _ : _) -> titleIdent (T.unwords (take (length ws - 1) ws))
  _              -> titleIdent name

-- | The record's fields: the layout's flags, numbers and bytes, in order.
recordFields :: Text -> Layout -> [RecordField]
recordFields typeName l =
  [ RecordField (fieldName f) (lowerIdent typeName <> titleIdent (fieldName f)) ty f
  | f <- layoutFields l, Just ty <- [typeOf (fieldKind f)] ]
  where
    typeOf = \case
      Flag     -> Just FlagField
      Unsigned -> Just NumberField
      Bytes    -> Just OctetsField
      _        -> Nothing

-- | The value the exact pins of a field agree on, as printed (Nothing when
--   none pins it, or two disagree).
agreedPin :: Map Text [Pin] -> Text -> Maybe Number
agreedPin pinned k = case L.nub [ v | PinExact v <- map pinValue pins ] of
  [v] -> listToMaybe (mapMaybe (parseNumber . pinText) pins) <|> Just (Number v Nothing)
  _   -> Nothing
  where pins = Map.findWithDefault [] (key k) pinned

-- | A row of a code table: @Table 40 row 2 (PDF 69)@.
codeRowRef :: CodeRead -> CodeRowRead -> Text
codeRowRef cr r = "Table " <> maybe "?" tshow (btTable (crBound cr)).tableNumber <> " row " <> tshow (rrIndex r + 1)
                  <> maybe "" (\pg -> " (PDF " <> tshow (pg + 1) <> ")") (listToMaybe (mapMaybe (\c -> c.page) (rrRow r)))

-- | Fields in the order the grid prints them: byte 0 first, bit 7 first in
--   each byte.
inGridOrder :: [GridField] -> [GridField]
inGridOrder = L.sortOn (\f -> (bpByte (gfFirst f), negate (bpBit (gfFirst f))))

-- | A name compared whitespace-free and case-blind.
key :: Text -> Text
key = T.toLower . T.filter (not . C.isSpace)


-- * Checks

checkFeature :: Context -> FeatureRead -> [Finding]
checkFeature ctx fr =
  gridFindings ctx bt (frGrid fr) <> tileFindings ctx bt (frLayout fr) <> map (atTable ctx bt) (none <> template <> code <> lengthPin <> pinned <> fields)
  <> unmatched <> unlisted
  where
    bt = frBound fr
    secText = fromMaybe "its section" (frSection fr)
    none = case frPins fr of
      Left why -> [ finding Warning "W-PINS-NONE" (frName fr <> ": no pins read: " <> why) ]
      Right [] -> [ finding Warning "W-PINS-NONE" (frName fr <> ": no sentence of " <> secText <> " or its subsections pins a field") ]
      Right _  -> []
    template = case frHeader fr of
      Left why -> [ finding Error "C-FEATURE-TEMPLATE" (frName fr <> ": " <> why) ]
      Right _  -> []
    listedValues = L.nub [ nValue n | (_, _, n) <- frListed fr ]
    code = case frHeader fr of
      Left _ -> []
      Right h
        | length listedValues > 1 ->
            [ finding Error "C-FEATURE-CODE" (frName fr <> ": the code tables give " <> secText <> " the codes " <> T.intercalate " and " (map listedRef (frListed fr))) ]
        | Just n <- agreedPin (frPinned fr) (hdCode h), [v] <- listedValues, nValue n /= v ->
            [ finding Error "C-FEATURE-CODE" (frName fr <> ": its " <> hdCode h <> " is pinned to " <> numberExpr n <> ", but "
                                              <> T.intercalate ", " (map listedRef (frListed fr))) ]
        | isNothing (frCode fr) ->
            [ finding Error "C-FEATURE-CODE" (frName fr <> ": no pin gives its " <> hdCode h <> " and no code table lists " <> secText) ]
        | otherwise -> []
    lengthPin = case frHeader fr of
      Right h | Just f <- L.find ((== key (hdLength h)) . key . fieldName) (layoutFields (frLayout fr))
              , Pinned v <- fieldKind f, Right tm <- frTemplate fr
              , v /= toInteger (layoutSize (frLayout fr) - tmSize tm) ->
        [ finding Error "C-FEATURE-LENGTH" (frName fr <> ": its " <> hdLength h <> " is pinned to " <> tshow v <> ", but "
                                            <> tshow (layoutSize (frLayout fr) - tmSize tm) <> " of its " <> tshow (layoutSize (frLayout fr))
                                            <> " bytes follow the " <> tshow (tmSize tm) <> "-byte header") ]
      _ -> []
    pinned =
      [ finding Error "C-FEATURE-PIN" (frName fr <> ": " <> T.intercalate " and " [ pinField p <> " = " <> pinText p <> " (" <> pinSection p <> ")" | p <- ps ])
      | ps <- Map.elems (frPinned fr), length (L.nub [ v | p <- ps, PinExact v <- [pinValue p] ]) > 1 ]
    fields =
      [ finding Error "C-FEATURE-FIELD" (frName fr <> ": " <> tshow (length ns) <> " fields, " <> T.intercalate " and " (map tshow (L.nub ns))
                                         <> ", give the record one field, " <> i)
      | (i, ns) <- Map.toList (Map.fromListWith (flip (<>)) [ (rfIdent f, [rfName f]) | f <- frFields fr ]), length ns > 1 ]
      <> [ finding Error "C-FEATURE-FIELD" (frName fr <> ": the variable part " <> tshow (tailName t) <> " is no field of a record")
         | Just t <- [layoutTail (frLayout fr)] ]
    unmatched =
      [ locatedAt (cxSpec ctx) (renderAnchorId (btAnchor bt)) (pinPage p + 1)
          (finding Warning "W-FEATURE-PIN" (frName fr <> ": " <> pinSection p <> " pins " <> pinField p <> " (" <> pinText p <> "), no field of it"))
      | p <- frUnmatched fr ]
    unlisted = case (frHeader fr, frListed fr) of
      (Right _, []) | isJust (frCode fr) ->
        [ atTable ctx bt (finding Warning "W-FEATURE-UNLISTED" (frName fr <> ": no code table lists " <> secText)) ]
      _ -> []
    listedRef (cr, r, n) = codeRowRef cr r <> " gives " <> numberExpr n


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx = concatMap (one . readFeature ctx)
  where
    one fr = case L.nub [ Cm.code f | f <- checkFeature ctx fr, Cm.isError f ] of
      []     -> [ Emit (layoutDecl (boundProvenance ctx (frBound fr)) (lowerIdent (frName fr)) (frLayout fr)), Emit (featureDecl ctx fr) ]
      failed -> [ Excluded (btAnchor (frBound fr)) (T.intercalate ", " failed <> " failed") ]

-- | The record of a descriptor and its instance of @Feature@, after its layout.
featureDecl :: Context -> FeatureRead -> Gen.Decl
featureDecl ctx fr = (Gen.decl p typeName body) { Gen.declRank = 1, Gen.declImports = imports }
  where
    p = boundProvenance ctx (frBound fr)
    typeName = featureTypeName (frName fr)
    fs = frFields fr
    header = either (const Nothing) Just (frHeader fr)
    body = [ Gen.provenanceComment p <> ": the feature, its fields neither pinned nor reserved" ]
           <> record <> [""] <> provenance <> [ "instance Feature " <> typeName <> " where" ] <> constants <> values <> fromValues
    -- the record
    width = foldl' max 0 [ T.length (rfIdent f) | f <- fs ]
    typeWidth = foldl' max 0 [ T.length (typeText (rfType f)) | f <- fs ]
    fieldLine f = rfIdent f <> T.replicate (width - T.length (rfIdent f)) " " <> " :: " <> typeText (rfType f)
                  <> T.replicate (typeWidth - T.length (typeText (rfType f))) " "
                  <> "  -- ^ " <> rfName f <> ", " <> bitsText (rfField f)
    record = case fs of
      []       -> [ "data " <> typeName <> " = " <> typeName, "  deriving (Eq, Show)" ]
      f : more -> [ "data " <> typeName <> " = " <> typeName, "  { " <> fieldLine f ] <> map (("  , " <>) . fieldLine) more
                  <> [ "  }", "  deriving (Eq, Show)" ]
    -- where the constants come from
    headerPins = [ (pinField q, q) | Just h <- [header], k <- [hdCode h, hdVersion h, hdLength h]
                                   , q <- take 1 (Map.findWithDefault [] (key k) (frPinned fr)) ]
    provenance =
      [ "-- the header of " <> typeName <> (if null headerPins then ": no pin" else " as pinned by "
          <> T.intercalate ", " (L.nub [ pinSection q <> " (PDF " <> tshow (pinPage q + 1) <> ")" | (_, q) <- headerPins ])
          <> ": " <> T.intercalate ", " [ f <> " = " <> pinText q | (f, q) <- headerPins ]) ]
      <> [ "-- the code " <> codeRowRef cr r <> " gives " <> fromMaybe "its section" (frSection fr) <> ": " <> codeCell cr r
         | (cr, r, _) <- frListed fr ]
    codeCell cr r = maybe "" (\c -> T.strip c.text) (listToMaybe (drop (crColumn cr) (rrRow r)))
    -- the instance
    pinNumber = agreedPin (frPinned fr)
    justOr = maybe "Nothing" (("Just " <>) . numberExpr)
    constants =
      [ "  featureCode       = " <> maybe "0" numberExpr (frCode fr)
      , "  featureVersion    = " <> justOr (header >>= pinNumber . hdVersion)
      , "  featureLength     = " <> justOr (header >>= pinNumber . hdLength)
      , "  featureLayout     = " <> lowerIdent (frName fr) ]
    values = case fs of
      [] -> [ "  featureValues _ = []" ]
      _  -> "  featureValues f =" : Gen.listLines "    " [ put x <> " " <> tshow (rfName x) <> " (" <> rfIdent x <> " f)" | x <- fs ]
    fromValues = case fs of
      []       -> [ "  featureFromValues _ = Right " <> typeName ]
      x : more -> ("  featureFromValues vs = " <> typeName) : ("    <$> " <> get x) : map (("    <*> " <>) . get) more
    get x = put x <> "Of " <> tshow (rfName x) <> " vs"
    put x = case rfType x of
      FlagField   -> "flag"
      NumberField -> "number"
      OctetsField -> "octets"
    has t = any ((== t) . rfType) fs
    imports =
      [ Gen.ImportSome featureModule (["Feature (..)"] <> concat [ [n, n <> "Of"] | (t, n) <- [(FlagField, "flag"), (NumberField, "number"), (OctetsField, "octets")], has t ])
      , Gen.ImportSome "Prelude" (["Eq", "Maybe (..)", "Show"] <> [ "Bool" | has FlagField ] <> [ "(<$>)" | not (null fs) ]
                                  <> [ "(<*>)" | length fs > 1 ] <> [ "Either (..)" | null fs ]) ]
      <> [ Gen.ImportSome "Numeric.Natural" ["Natural"] | has NumberField ]
      <> [ Gen.ImportSome "Data.ByteString" ["ByteString"] | has OctetsField ]

-- | A record field's type.
typeText :: FieldType -> Text
typeText = \case
  FlagField   -> "Bool"
  NumberField -> "Natural"
  OctetsField -> "ByteString"

-- | Where a field lies: @byte 4 bit 6@, @byte 2 bits 7-4@, @byte 0 bit 7 to byte 1 bit 0@.
bitsText :: Field -> Text
bitsText f
  | (b0, i0) == (b1, i1) = "byte " <> tshow b0 <> " bit " <> tshow i0
  | b0 == b1             = "byte " <> tshow b0 <> " bits " <> tshow i0 <> "-" <> tshow i1
  | otherwise            = "byte " <> tshow b0 <> " bit " <> tshow i0 <> " to byte " <> tshow b1 <> " bit " <> tshow i1
  where
    (b0, i0) = bitPosition (fieldStart f)
    (b1, i1) = bitPosition (fieldEnd f - 1)
