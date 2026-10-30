{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.ByteGrid
Description : The byteGrid rule: a structure printed as a grid of bytes and bits, its layout and its record
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E7. @byteGrid@ fires on a table whose first
row is a corner cell and the bit columns @7 … 0@ and whose caption does not
end @Feature Descriptor@ (the @ByteGrid@ shape; those are @feature@'s): in Core
2.01 Tables 17 ComPacket Format, 18 Packet Format, 20 Data SubPacket Format,
21 Credit Control Subpacket, 38 Level 0 Discovery Response Data Format, 39
Level 0 Discovery Header Format, 41 Feature Descriptor Template Format, 44
Common SSC Information and 212 GUDID Column Contents Description. The shape
picks them out across the document, so the rule rides on the @doc@ anchor
(design 13.24).

Each table is read by the shared grid reader ("Hsed.Spec.Readers.Grid": rows,
bits by the extent rule, fields from @(MSB)@ to @(LSB)@, a variable tail) and
made a 'Layout' by "Hsed.Spec.Readers.Layout".'gridLayout', under the name
the caption gives (@layout \"Name\"@ overrides it). The sentences of its
section that pin a field to the value a TPer SHALL send
("Hsed.Spec.Readers.Pins"; Table 39's two @The value of this field SHALL be
…@) are matched to its fields by name and make them pinned.

Checks:

* what reading the grid finds (@C-LAYOUT-EXTENT@, @C-LAYOUT-ENDIAN@, each at
  its row's page; @N-LAYOUT-BITS@, @W-LAYOUT-BITS-UNUSED@);
* @C-LAYOUT-TILE@ (Error), on a grid that reads: every bit of the fixed part
  covered by exactly one field, the fixed part's size the last byte the
  grid prints plus one, and a variable tail starting right after it; each
  at the page of the row the problem lies in;
* @C-GRID-NAME@ (Error): every field that carries a value has a name that
  makes an identifier, no two fields of the grid or generated names of two
  grids are alike;
* @W-PINS-NONE@ (Warning, not an Error): no pin names a field of the grid,
  where pins are in use: its section states pins of other fields (Table 38
  shares section 3.3.6.2 with Table 39), or the sections beside it state
  pins (Table 41 in 3.3.6.3 and Table 44 in 3.3.6.6, beside 3.3.6.2, 3.3.6.4
  and 3.3.6.5). Where no section around a grid pins anything (the ComPacket
  and GUDID grids) its having none is no finding.

Compiled into the shared layouts module ('layoutsModule', which the
@byteFieldValue@ rule writes too), for each table: its 'Layout' as
@\<name\>Layout@; a record of the fields that carry a value, named from the
caption less a trailing @Format@ (Table 17 ComPacket Format gives
@ComPacket@), each field @\<name\>\<Field\>@ typed by its width ('Word8' to
'Word64', 'Natural' beyond, 'Bool' for a one-bit flag, 'ByteString' for a
run of more than eight whole bytes), the reserved bits, the pins and the
tail's length field left to the layout; and its instance of
"Hsed.Core.Record".@LayoutRecord@ ("Hsed.Spec.Gen".'Gen.recordModule'). A
grid with a variable tail gives two records, as example E7 has them: the
fixed part's (@ComPacketHeader@) and the whole (@ComPacket@: the header and
the tail's bytes), the whole being the instance; a tail of reserved bytes is
the layout's, as reserved bits are ('recordTail'). The @byteFieldValue@ rule
names and prints its layouts and records with this module's functions
('layoutValue', 'recordDecl', 'nameFindings'), so the two rules' declarations
in the one module follow one convention.
-}
module Hsed.Spec.Rules.ByteGrid
  ( byteGridRule
    -- * Reading the bound tables
  , GridRead (..)
  , gridReads
  , readGridTable
  , structureName
  , applyPins
    -- * Records
  , RecordField (..)
  , recordFieldsOf
  , recordTail
  , fieldType
  , typeName
  , layoutValue
  , recordNames
  , recordDecl
    -- * Checks
  , nameFindings
  ) where

import qualified RIO.Char                 as C
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import           RIO

import           Hsed.Core.Layout         (Field (..), FieldKind (..), Layout (..), Tail (..), TileProblem (..), bitPosition,
                                           fieldEnd, tiling)
import           Hsed.Spec.Anchor         (AnchorId (..))
import           Hsed.Spec.Common         (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.DSL            (SomeBinding (..), byteGrid)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (CellIR, SpecIR (..), TableIR (..))
import           Hsed.Spec.Readers.Grid   (BitPos (..), ByteRow (..), ByteSpan (..), Grid (..), GridField (..),
                                           GridProblem (..), gridFindings, gridOf, reservedName)
import           Hsed.Spec.Readers.Layout (describeTileProblem, gridLayout, layoutDecl, layoutsModule)
import           Hsed.Spec.Readers.Pins   (Pin (..), PinValue (..), pinsInSection, tablePins)
import           Hsed.Spec.Readers.Text   (lowerIdent, titleIdent)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Scope          (Section (..), covers)
import           Hsed.Spec.Shape          (Shape (..), captionTitle, shapeMismatch)


-- | The rule.
byteGridRule :: RuleDef
byteGridRule = RuleDef
  { ruleName    = RuleName "byteGrid"
  , bindsTo     = SomeBinding byteGrid
  , exemplar    = (SpecId "core-2.01", TableId 17)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch ByteGrid form)
  , ruleIdent   = const Nothing
  , ruleModule  = layoutsModule
  , ruleCodes   =
      [ CodeDef "C-GRID-NAME" [Error] (Defect "a byte grid's field that carries a value has no name that makes an identifier, or two of its fields, or two grids, are named alike") ]
  , ruleChecks  = \ctx bts -> let grs = readGrids ctx bts in concatMap (checkGrid ctx grs) grs
  , ruleCompile = compile
  }

-- * Reading the bound tables

-- | One byte grid read under the errata set.
data GridRead = GridRead
  { grBound  :: !BoundTable
  , grGrid   :: !Grid
  , grTitle  :: !Text                 -- ^ the caption's title after errata, normalised
  , grName   :: !Text                 -- ^ the structure's name ('structureName', or @layout "Name"@)
  , grLayout :: !Layout               -- ^ its fields, the pins applied
  , grPins   :: !(Either Text [Pin])  -- ^ the pins of its section
  , grPinned :: ![(Text, Pin)]        -- ^ those naming one of its fields, by the field's name
  }

-- | Every table bound to the rule's shape, read under the errata set.
gridReads :: Context -> [GridRead]
gridReads ctx = readGrids ctx (boundTables (Just ByteGrid) ctx)

readGrids :: Context -> [BoundTable] -> [GridRead]
readGrids ctx = map (readGridTable ctx)

-- | One bound table: its grid, its names, its layout with its pins.
readGridTable :: Context -> BoundTable -> GridRead
readGridTable ctx bt = GridRead
  { grBound  = bt
  , grGrid   = g
  , grTitle  = title
  , grName   = fromMaybe (structureName title) (mLayout (btMods bt))
  , grLayout = pinned
  , grPins   = pins
  , grPinned = matched }
  where
    g = gridOf bt
    title = fromMaybe "" ((btRead bt).caption >>= captionTitle)
    pins = tablePins ctx bt
    (pinned, matched) = applyPins (fromRight [] pins) (gridLayout (fromMaybe title (mLayout (btMods bt))) g)

-- | The name of the structure a grid's caption describes: the title less a
--   trailing @Format@, which names the table as a format of it (@ComPacket
--   Format@ gives @ComPacket@, @Level 0 Discovery Header Format@ gives @Level 0
--   Discovery Header@; @Common SSC Information@ stays).
structureName :: Text -> Text
structureName title = case T.stripSuffix " Format" title of
  Just s | not (T.null (T.strip s)) -> T.strip s
  _                                 -> title

-- | The pins naming a field of the layout (whitespace-free and case-blind,
--   the field's name without its parenthesis), the first for each field; a
--   pin of an exact value makes its field 'Pinned' (a vendor-unique or
--   minimum value names the field and pins nothing).
applyPins :: [Pin] -> Layout -> (Layout, [(Text, Pin)])
applyPins pins l = (l { layoutFields = map pin (layoutFields l) }, matched)
  where
    matched = [ (fieldName f, p) | f <- layoutFields l, fieldKind f /= Reserved
              , p <- take 1 [ p | p <- pins, key (pinField p) == key (fieldName f) ] ]
    pin f = case [ v | (n, p) <- matched, n == fieldName f, PinExact v <- [pinValue p] ] of
      v : _ -> f { fieldKind = Pinned v }
      []    -> f

-- | A name compared whitespace-free and case-blind.
key :: Text -> Text
key = T.toLower . T.filter (not . C.isSpace)


-- * Records

-- | A field of the record: a field of the layout that carries a value.
data RecordField = RecordField
  { rfName    :: !Text   -- ^ the layout's name for it, the key of its value
  , rfIdent   :: !Text   -- ^ the record field: @comPacketComID@
  , rfType    :: !Text   -- ^ @Word16@, @Bool@, @ByteString@
  , rfKind    :: !FieldKind
  , rfWhere   :: !Text   -- ^ @bytes 4 to 5@
  , rfPrinted :: !Text   -- ^ its name as the grid prints it, the marks removed
  , rfCells   :: ![CellIR]
  }

-- | The tail a record carries: the layout's, unless its name says its bytes
--   are reserved (@byteFieldValue@'s runs to the end of the transfer, @46 to
--   TRNSFLEN - 1 | Reserved@), which, like reserved bits, is the layout's:
--   encoding writes none of it, decoding skips it.
recordTail :: Layout -> Maybe Tail
recordTail l = mfilter (not . reservedName . tailName) (layoutTail l)

-- | The fields of a grid's record, in order: every field of its layout but the
--   reserved ones, the pinned ones and the one that gives its tail's length.
recordFieldsOf :: GridRead -> [RecordField]
recordFieldsOf gr =
  [ RecordField (fieldName f) (fieldIdent gr (fieldName f)) (fieldType f) (fieldKind f) (spanText f) (gfName gf) (gfCells gf)
  | (f, gf) <- zip (layoutFields l) [ gf | gf <- gFields (grGrid gr), isJust (gfLast gf) ]
  , carriesValue (fieldKind f)
  , Just (fieldName f) /= (layoutTail l >>= tailLength) ]
  where
    l = grLayout gr
    carriesValue = \case
      Reserved -> False
      Pinned _ -> False
      _        -> True

-- | The Haskell type of a field's value: a flag a 'Bool', a byte string a
--   'ByteString', an unsigned number the narrowest of @Word8@ to @Word64@ that
--   holds it, 'Natural' when none does.
fieldType :: Field -> Text
fieldType f = case fieldKind f of
  Flag  -> "Bool"
  Bytes -> "ByteString"
  _ | w <= 8    -> "Word8"
    | w <= 16   -> "Word16"
    | w <= 32   -> "Word32"
    | w <= 64   -> "Word64"
    | otherwise -> "Natural"
  where w = fieldWidth f

-- | Where a field lies: @byte 3@, @bytes 4 to 5@, @byte 2 bits 7-4@, @byte 4
--   bit 3 to byte 7 bit 4@.
spanText :: Field -> Text
spanText f
  | h == 7, l == 0, b0 == b1 = "byte " <> tshow b0
  | h == 7, l == 0           = "bytes " <> tshow b0 <> " to " <> tshow b1
  | b0 == b1                 = "byte " <> tshow b0 <> (if h == l then " bit " <> tshow h else " bits " <> tshow h <> "-" <> tshow l)
  | otherwise                = "byte " <> tshow b0 <> " bit " <> tshow h <> " to byte " <> tshow b1 <> " bit " <> tshow l
  where
    (b0, h) = bitPosition (fieldStart f)
    (b1, l) = bitPosition (fieldEnd f - 1)

-- | The text an identifier is made from: parentheses dropped, their letters
--   kept (@Feature Descriptor(s)@ reads @Feature Descriptors@).
identText :: Text -> Text
identText = T.filter (`notElem` ("()" :: String))

-- | The record type's name: @ComPacket@.
typeName :: GridRead -> Text
typeName gr = titleIdent (identText (grName gr))

-- | The prefix of its values' names: @comPacket@.
valuePrefix :: GridRead -> Text
valuePrefix gr = lowerIdent (identText (grName gr))

-- | The record field of a layout field: @comPacketComID@.
fieldIdent :: GridRead -> Text -> Text
fieldIdent gr n = valuePrefix gr <> titleIdent (identText n)

-- | The layout's value: @comPacketLayout@.
layoutValue :: GridRead -> Text
layoutValue gr = valuePrefix gr <> "Layout"

-- | The fixed part's record where the layout has a tail: @ComPacketHeader@,
--   and the whole's field holding it, @comPacketHeader@.
headerType, headerField :: GridRead -> Text
headerType gr = typeName gr <> "Header"
headerField gr = valuePrefix gr <> "Header"

-- | Every name a grid's declarations define: the layout, the record types and
--   their constructors (one name each), and the record fields.
recordNames :: GridRead -> [Text]
recordNames gr =
  [layoutValue gr, typeName gr]
  <> concat [ [headerType gr, headerField gr, fieldIdent gr (tailName t)] | Just t <- [recordTail (grLayout gr)] ]
  <> map rfIdent (recordFieldsOf gr)


-- * Checks

checkGrid :: Context -> [GridRead] -> GridRead -> [Finding]
checkGrid ctx grs gr =
  gridFindings ctx (grBound gr) (grGrid gr)
  <> (if reads then tileFindingsAtRows ctx gr <> sizeFindings ctx gr else [])
  <> nameFindings ctx grs gr
  <> pinFindings ctx gr
  where reads = not (any ((== Error) . gpSeverity) (gProblems (grGrid gr)))

-- | The cells of the row a byte is printed in (none when no row is that byte
--   or a range holding it).
rowOfByte :: Grid -> Int -> [CellIR]
rowOfByte g b = concat (take 1 [ brRow r | r <- gRows g, holds (brSpan r) ])
  where
    holds = \case
      Just (OneByte n)     -> n == b
      Just (ByteRange a c) -> a <= b && b <= c
      _                    -> False

-- | The cells a field of the layout is printed in.
cellsOfField :: Grid -> Text -> [CellIR]
cellsOfField g n = concat (take 1 [ gfCells f | f <- gFields g, gfBase f == n || gfName f == n ])

-- | Locate a finding at cells, or at the table when there are none.
atCellsOr :: Context -> BoundTable -> [CellIR] -> Finding -> Finding
atCellsOr ctx bt cells = if null cells then atTable ctx bt else atCells ctx bt cells

-- | @C-LAYOUT-TILE@: each reason the layout's fields do not tile its fixed
--   part, at the row of the byte it lies in (the field's cells for a
--   problem of one field).
tileFindingsAtRows :: Context -> GridRead -> [Finding]
tileFindingsAtRows ctx gr =
  [ atCellsOr ctx (grBound gr) (cellsOf p) (finding Error "C-LAYOUT-TILE" (layoutName l <> ": " <> describeTileProblem p))
  | p <- tiling l ]
  where
    l = grLayout gr
    g = grGrid gr
    cellsOf = \case
      Overlap _ _ o -> rowOfByte g (fst (bitPosition o))
      Gap o _       -> rowOfByte g (fst (bitPosition o))
      Outside n     -> cellsOfField g n
      Unaligned n   -> cellsOfField g n
      Unfit n _     -> cellsOfField g n
      WideFlag n    -> cellsOfField g n
      NoLength n    -> cellsOfField g n
      BadAlign _    -> []

-- | @C-LAYOUT-TILE@: the fixed part's size is the last byte the grid prints
--   plus one, and a variable tail starts right after it.
sizeFindings :: Context -> GridRead -> [Finding]
sizeFindings ctx gr =
  [ atCellsOr ctx bt cells (bad ("the grid prints bytes to " <> tshow b <> ", its fields end at byte " <> tshow (size - 1)))
  | (b, cells) <- take 1 (L.sortOn (negate . fst) printed), b + 1 /= size ]
  <> [ atCellsOr ctx bt (gfCells f) (bad (tshow (gfName f) <> " starts at byte " <> tshow (bpByte (gfFirst f)) <> ", the fixed part ends at byte " <> tshow (size - 1)))
     | f <- gFields g, isNothing (gfLast f), bpByte (gfFirst f) /= size || bpBit (gfFirst f) /= 7 ]
  where
    bt = grBound gr
    g = grGrid gr
    size = layoutSize (grLayout gr)
    bad = finding Error "C-LAYOUT-TILE" . ((layoutName (grLayout gr) <> ": ") <>)
    printed = [ (b, brRow r) | r <- gRows g, Just b <- [lastByte (brSpan r)] ]
    lastByte = \case
      Just (OneByte n)     -> Just n
      Just (ByteRange _ c) -> Just c
      _                    -> Nothing

-- | @C-GRID-NAME@: a field that carries a value without a name that makes an
--   identifier; two fields of the grid named alike; a name the declarations
--   of a read before it in the list define first (the @byteFieldValue@ rule
--   passes this rule's reads, then its own, so that each name of the one
--   layouts module is defined once).
nameFindings :: Context -> [GridRead] -> GridRead -> [Finding]
nameFindings ctx grs gr = unnamed <> twice <> clashing
  where
    bt = grBound gr
    bad cells = atCellsOr ctx bt cells . finding Error "C-GRID-NAME"
    fields = recordFieldsOf gr
    -- a field's name, after the grid's prefix, needs a letter or a digit; the grid's, a capital first
    unnamed = [ bad (rfCells f) (rfWhere f <> ": the field " <> tshow (rfPrinted f) <> " has no name that makes an identifier")
              | f <- fields, noIdent (rfName f) ]
              <> [ bad [] ("the tail " <> tshow (tailName t) <> " has no name that makes an identifier")
                 | Just t <- [recordTail (grLayout gr)], noIdent (tailName t) ]
              <> [ bad [] ("the name " <> tshow (grName gr) <> " makes no type name") | not (typeIdent (typeName gr)) ]
    twice = [ bad (concatMap rfCells fs) ("the fields " <> T.intercalate " and " (map (\f -> tshow (rfPrinted f) <> " (" <> rfWhere f <> ")") fs)
                                          <> " are named alike: " <> n)
            | (n, fs@(_ : _ : _)) <- Map.toList (Map.fromListWith (flip (<>)) [ (rfIdent f, [f]) | f <- fields ]) ]
    earlier = concatMap recordNames (takeWhile ((/= btAnchor bt) . btAnchor . grBound) grs)
    clashing = [ atTable ctx bt (finding Error "C-GRID-NAME" (n <> " is also a name of an earlier grid's declarations"))
               | n <- L.nub (recordNames gr), n `elem` earlier ]
    noIdent n = not (T.any C.isAlphaNum (titleIdent (identText n)))
    typeIdent t = maybe False (\(c, cs) -> C.isUpper c && T.all (\x -> C.isAlphaNum x || x == '_') cs) (T.uncons t)

-- | @W-PINS-NONE@: no pin names a field of the grid, where pins are in use:
--   its section's pins name other fields, or the sections beside it (under
--   the same parent, word-complete) state pins.
pinFindings :: Context -> GridRead -> [Finding]
pinFindings ctx gr = case grPins gr of
  Left why -> [ warn ("its section's pins cannot be read: " <> why) ]
  Right ps
    | not (null (grPinned gr)) -> []
    | not (null ps)            -> [ warn ("the pins of " <> sec <> " (" <> T.intercalate ", " (L.nub (map pinField ps)) <> ") name none of its fields") ]
    | not (null beside)        -> [ warn ("no sentence of " <> sec <> " or its subsections pins one of its fields, while "
                                          <> T.intercalate ", " beside <> " pin fields of the grids beside it") ]
    | otherwise                -> []
  where
    bt = grBound gr
    warn = atTable ctx bt . finding Warning "W-PINS-NONE" . ((layoutName (grLayout gr) <> ": ") <>)
    section = sectionOf ctx bt
    sec = fromMaybe "its section" section
    ir = cxIR ctx
    beside = case section >>= parentSection of
      Nothing -> []
      Just parent -> L.nub
        [ pinSection p
        | s <- ir.wordComplete, covers (Subtree parent) (Just s), not (covers (Subtree sec) (Just s))
        , Right ps <- [pinsInSection ir s], p <- ps, not (covers (Subtree sec) (Just (pinSection p))) ]
    parentSection s = case L.initMaybe (T.split (== '.') s) of
      Just parts@(_ : _) -> Just (T.intercalate "." parts)
      _                  -> Nothing


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = concatMap one grs
  where
    grs = readGrids ctx bts
    one gr = case L.nub [ Cm.code f | f <- checkGrid ctx grs gr, Cm.isError f ] of
      []    -> [ Emit (layoutDecl (boundProvenance ctx (grBound gr)) (layoutValue gr) (grLayout gr)), Emit (recordDecl ctx gr) ]
      codes -> [ Excluded (btAnchor (grBound gr)) (T.intercalate ", " codes <> " failed") ]

-- | The record(s) of a grid, or of a @byteFieldValue@ table read as one, and
--   the instance over its layout.
recordDecl :: Context -> GridRead -> Gen.Decl
recordDecl ctx gr = (Gen.decl p (typeName gr) body)
  { Gen.declRank    = 1
  , Gen.declImports =
      [ Gen.ImportSome Gen.recordModule (["LayoutRecord (..)"] <> L.nub (concatMap helpers fields) <> [ h | isJust tl, h <- ["octets", "octetsField"] ])
      , Gen.ImportSome "Prelude" (["Eq", "Show", "pure"] <> [ "(<*>)" | not (null fields) || isJust tl ]) ]
      <> [ Gen.ImportSome "Data.Word" ws | let ws = L.nub [ t | f <- fields, t <- [rfType f], "Word" `T.isPrefixOf` t ], not (null ws) ]
      <> [ Gen.ImportSome "Data.ByteString" ["ByteString"] | isJust tl || any ((== "ByteString") . rfType) fields ]
      <> [ Gen.ImportSome "Numeric.Natural" ["Natural"] | any ((== "Natural") . rfType) fields ] }
  where
    p = boundProvenance ctx (grBound gr)
    l = grLayout gr
    tl = recordTail l
    fields = recordFieldsOf gr
    doc = Gen.provenanceComment p
    helpers f = case rfKind f of
      Flag  -> ["flag", "flagField"]
      Bytes -> ["octets", "octetsField"]
      _     -> ["number", "numberField"]
    put f = case rfKind f of
      Flag  -> "flag"
      Bytes -> "octets"
      _     -> "number"
    get f = put f <> "Field"
    fixedType = maybe (typeName gr) (const (headerType gr)) tl
    -- the layout's constants and what it leaves out, said once
    leftOut = T.intercalate "; " $
      [ "the layout's reserved bits are zeros" | any ((== Reserved) . fieldKind) (layoutFields l) ]
      <> [ "the constants the table prints are the layout's: " <> T.intercalate ", " printed | not (null printed) ]
      <> [ "the layout pins " <> T.intercalate ", " byPins | not (null byPins) ]
      <> [ n <> " is the length of " <> tailName t | Just t <- [tl], Just n <- [tailLength t] ]
      <> [ "the " <> T.toLower (tailName t) <> " bytes from byte " <> tshow (layoutSize l) <> " to the end are the layout's, written as none and skipped when read"
         | Just t <- [layoutTail l], isNothing tl ]
    pinned = [ (fieldName f, v) | f <- layoutFields l, Pinned v <- [fieldKind f] ]
    -- a constant printed as the field's name (0x02) or after it (NAA (0x05))
    printed = [ if hexText v == T.toLower n then n else n <> " " <> hexText v | (n, v) <- pinned, isNothing (L.lookup n (grPinned gr)) ]
    byPins = [ n <> " to " <> pinText pn <> " (" <> pinSection pn <> ", PDF " <> tshow (pinPage pn + 1) <> ")"
             | (n, _) <- pinned, Just pn <- [L.lookup n (grPinned gr)] ]
    fixedDoc = (doc <> ":") : map ("--   " <>) (Gen.wrapText 96 (ascii (
                 (if isJust tl then "the fixed part's fields" else "the fields") <> " that carry a value"
                 <> (if T.null leftOut then "." else "; " <> leftOut <> "."))))
    record name fs =
      [ "data " <> name <> " = " <> name ]
      <> fieldLines [ (rfIdent f, "!" <> rfType f, Just (rfWhere f <> printedNote f)) | f <- fs ]
      <> [ "  deriving (Eq, Show)" ]
    printedNote f = if rfPrinted f /= rfName f then ": " <> ascii (rfPrinted f) else ""
    fieldLines = \case
      [] -> []
      fs -> let w = foldl' max 0 [ T.length n | (n, _, _) <- fs ]
                tw = foldl' max 0 [ T.length t | (_, t, _) <- fs ]
            in [ (if i == 0 then "  { " else "  , ") <> T.justifyLeft w ' ' n <> " :: " <> maybe t (\c -> T.justifyLeft tw ' ' t <> "  -- ^ " <> c) note
               | (i, (n, t, note)) <- zip [0 :: Int ..] fs ] <> ["  }"]
    whole = case tl of
      Nothing -> []
      Just t ->
        [ ""
        , doc <> ": the whole, its fixed part and its " <> ascii (tailName t)
        , "data " <> typeName gr <> " = " <> typeName gr ]
        <> fieldLines [ (headerField gr, "!" <> headerType gr, Nothing)
                      , (fieldIdent gr (tailName t), "!ByteString", Just (tailNote t)) ]
        <> [ "  deriving (Eq, Show)" ]
    tailNote t = "from byte " <> tshow (layoutSize l) <> ", " <> maybe "to the end" (<> " bytes") (tailLength t)
                 <> (if tailAlign t > 1 then ", padded to a multiple of " <> tshow (tailAlign t) else "")
    -- the instance: the whole's fields through the header's
    viaHeader f = if isJust tl then "(" <> rfIdent f <> " (" <> headerField gr <> " r))" else "(" <> rfIdent f <> " r)"
    values = [ "(" <> tshow (rfName f) <> ", " <> put f <> " " <> viaHeader f <> ")" | f <- fields ]
             <> [ "(" <> tshow (tailName t) <> ", octets (" <> fieldIdent gr (tailName t) <> " r))" | Just t <- [tl] ]
    applied fs = [ "<*> " <> get f <> " " <> tshow (rfName f) <> " vs" | f <- fs ]
    fromLines = case tl of
      Nothing -> map ("    " <>) (applied fields)
      Just t  -> case applied fields of
        []       -> ["    <*> pure " <> headerType gr, "    <*> octetsField " <> tshow (tailName t) <> " vs"]
        a : rest -> ["    <*> (pure " <> headerType gr, "           " <> a] <> map ("           " <>) rest
                    <> ["        )", "    <*> octetsField " <> tshow (tailName t) <> " vs"]
    used = not (null values)
    inst =
      [ ""
      , "instance LayoutRecord " <> typeName gr <> " where"
      , "  recordLayout _ = " <> layoutValue gr ]
      <> (if used then "  recordFields r =" : Gen.listLines "    " values else ["  recordFields _ = []"])
      <> [ "  fromFields " <> (if used then "vs" else "_") <> " = pure " <> typeName gr ]
      <> fromLines
    body = fixedDoc <> record fixedType fields <> whole <> inst

-- | @0x0c@, @0x0001@: a pinned value in hex, an even count of digits.
hexText :: Integer -> Text
hexText v = "0x" <> T.justifyRight (n + n `mod` 2) '0' digits
  where
    digits = T.pack (showHexInteger v)
    n = T.length digits
    showHexInteger x
      | x < 16    = [hexDigit x]
      | otherwise = showHexInteger (x `div` 16) <> [hexDigit (x `mod` 16)]
    hexDigit d = T.index "0123456789abcdef" (fromInteger d)

-- | Text as printed made ASCII for a comment (a generated module stays ASCII,
--   docs/adding-a-rule.md): dashes as @-@, an ellipsis as @...@, anything
--   else not ASCII as @?@.
ascii :: Text -> Text
ascii = T.concatMap $ \c -> if C.isAscii c then T.singleton c else case c of
  '\x2013' -> "-"
  '\x2014' -> "-"
  '\x2026' -> "..."
  _        -> "?"
