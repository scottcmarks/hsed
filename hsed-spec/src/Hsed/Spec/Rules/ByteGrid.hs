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
bits by the extent rule, fields from @(MSB)@ to @(LSB)@, a variable tail) as a
"Hsed.Spec.Readers.Record".'Structure': its layout named by the caption's
title, its record by the structure the title names ('structureName'; @layout
\"Name\"@ names the record as written). The sentences of its section that pin
a field to the value a TPer SHALL send ("Hsed.Spec.Readers.Pins"; Table 39's
two @The value of this field SHALL be …@) are matched to its fields by name
and make them pinned, as do the constants the grid prints as a field's name
(Table 212's @0x02@, @NAA (0x05)@).

Checks:

* what reading the grid finds (@C-LAYOUT-EXTENT@, @C-LAYOUT-ENDIAN@, each at
  its row's page; @N-LAYOUT-BITS@, @W-LAYOUT-BITS-UNUSED@);
* @C-LAYOUT-TILE@, on a grid that reads, by the one policy of the grid rules
  ("Hsed.Spec.Readers.Layout".'tileFindings'): bits no field names a Warning
  (the layout generated as printed, without a record), any other tiling
  problem an Error; each at the page of the row it lies in;
* @C-GRID-NAME@ ("Hsed.Spec.Readers.Record".'nameFindings'): every field that
  carries a value has a name that makes an identifier, no two fields of the
  grid or names of two grids' declarations are alike, none is a name the
  module imports;
* @W-PINS-NONE@ and @C-PINS-WORDS@ ("Hsed.Spec.Readers.Pins".'pinsFindings'):
  no pin names a field of the grid where pins are in use around it (Tables
  38, 41, 44), or its section's pins cannot be read. Core's ComPacket family
  states constants for Kind and Length (3.2.3.4.1.1, 3.2.3.4.2.1: @For
  Credit Control Subpackets, this field is set to 0x8001.@, @This is always
  0x00000004 …@) in sentences none of the three schemas reads, so they pin
  nothing and their grids get no W-PINS-NONE (design 13.26).

Compiled into the shared layouts module ('layoutsModule', which the
@byteFieldValue@ rule writes too), for each table: its 'Layout' as
@\<name\>Layout@ (its comment naming the bytes no field covers, if any); and,
for a layout that tiles, its record and instance of
"Hsed.Core.Record".@LayoutRecord@ ('recordDecl'), the record named from the
caption less a trailing @Format@ (Table 17 ComPacket Format gives
@ComPacket@), two records for a grid with a variable tail, as example E7 has
them.
-}
module Hsed.Spec.Rules.ByteGrid
  ( byteGridRule
    -- * Reading the bound tables
  , GridRead (..)
  , gridReads
  , gridStructures
  , gridDecls
  , readGridTable
  , structureName
  ) where

import qualified RIO.List                 as L
import qualified RIO.Text                 as T
import           RIO

import           Hsed.Core.Layout         (Field (..), FieldKind (..), Layout (..))
import           Hsed.Spec.Anchor         (AnchorId (..))
import           Hsed.Spec.Common         (Finding, RuleName (..), Severity (..), SpecId (..))
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.DSL            (SomeBinding (..), byteGrid)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (TableIR (..))
import           Hsed.Spec.Readers.Grid   (Grid (..), GridField (..), GridProblem (..), gridFindings, gridOf)
import           Hsed.Spec.Readers.Layout (gridFields, gridTiling, layoutDecl, layoutsModule, tileFindings, untiledNote)
import           Hsed.Spec.Readers.Pins   (Pin (..), PinValue (..), pinsFindings, tablePins)
import           Hsed.Spec.Readers.Record (Structure (..), declScope, earlierNames, layoutValue, nameFindings, recordDecl,
                                           structure)
import           Hsed.Spec.Readers.Text   (nameKey)
import           Hsed.Spec.RuleDef
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
  , ruleCodes   = []
  , ruleChecks  = \ctx bts -> let grs = map (readGridTable ctx) bts in concatMap (checkGrid ctx grs) grs
  , ruleCompile = compile
  }

-- * Reading the bound tables

-- | One byte grid read under the errata set.
data GridRead = GridRead
  { grStructure :: !Structure            -- ^ its layout, the pins applied, and its record's name
  , grGrid      :: !Grid
  , grPins      :: !(Either Text [Pin])  -- ^ the pins of its section
  , grPinned    :: ![(Text, Pin)]        -- ^ those naming one of its fields, by the field's name
  }

-- | Every table bound to the rule's shape, read under the errata set.
gridReads :: Context -> [GridRead]
gridReads ctx = map (readGridTable ctx) (boundTables (Just ByteGrid) ctx)

-- | The structures of 'gridReads': what the @byteFieldValue@ rule's names in
--   the one layouts module must not repeat.
gridStructures :: Context -> [Structure]
gridStructures = map grStructure . gridReads

-- | The declarations of 'gridReads', whose imports the @byteFieldValue@
--   rule's names in the one layouts module must not meet.
gridDecls :: Context -> [Gen.Decl]
gridDecls ctx = concatMap (decls ctx) (gridReads ctx)

-- | One bound table: its grid, its pins, its structure with its pins applied
--   and the comment saying where its constants come from.
readGridTable :: Context -> BoundTable -> GridRead
readGridTable ctx bt = GridRead
  { grStructure = st { stConstants = printedNote <> pinsNote }
  , grGrid      = g
  , grPins      = pins
  , grPinned    = matched }
  where
    g = gridOf bt
    title = fromMaybe "" ((btRead bt).caption >>= captionTitle)
    pins = tablePins ctx bt
    st = structure bt (fromMaybe (structureName title) (mLayout (btMods bt))) title g pinned
    -- the pins naming a field (whitespace-free and case-blind), the first for each field; an exact one pins it
    matched = [ (fieldName f, p) | (f, _) <- gridFields g, fieldKind f /= Reserved
              , p <- take 1 [ p | p <- fromRight [] pins, nameKey (pinField p) == nameKey (fieldName f) ] ]
    pinned (f, _) = case [ v | (n, p) <- matched, n == fieldName f, PinExact v <- [pinValue p] ] of
      v : _ -> f { fieldKind = Pinned v }
      []    -> f
    pinnedFields = [ (f, gf) | (f, gf) <- stFields st, Pinned _ <- [fieldKind f] ]
    -- a constant printed as the field's name (0x02) or after it (NAA (0x05)), and the pins
    printedNote = [ "the constants the table prints are the layout's: " <> T.intercalate ", " ps
                  | let ps = [ gfName gf | (f, gf) <- pinnedFields, isNothing (L.lookup (fieldName f) matched) ], not (null ps) ]
    pinsNote = [ "the layout pins " <> T.intercalate ", " ps
               | let ps = [ fieldName f <> " to " <> pinText p <> " (" <> pinSection p <> ", PDF " <> tshow (pinPage p + 1) <> ")"
                          | (f, _) <- pinnedFields, Just p <- [L.lookup (fieldName f) matched] ]
               , not (null ps) ]

-- | The name of the structure a grid's caption describes: the title less a
--   trailing @Format@, which names the table as a format of it (@ComPacket
--   Format@ gives @ComPacket@, @Level 0 Discovery Header Format@ gives @Level 0
--   Discovery Header@; @Common SSC Information@ stays).
structureName :: Text -> Text
structureName title = case T.stripSuffix " Format" title of
  Just s | not (T.null (T.strip s)) -> T.strip s
  _                                 -> title


-- * Checks

-- | A grid's findings, given every grid the rule reads (whose names come
--   before its own, and whose declarations' imports are its module's).
checkGrid :: Context -> [GridRead] -> GridRead -> [Finding]
checkGrid ctx grs gr =
  gridFindings ctx bt (grGrid gr)
  <> (if reads then tileFindings ctx bt (grGrid gr) l else [])
  <> nameFindings ctx (earlierNames (map grStructure grs) st) (declScope (concatMap (decls ctx) grs)) st
  <> pinsFindings ctx bt (layoutName l) (not (null (grPinned gr)))
  where
    st = grStructure gr
    bt = stBound st
    l = stLayout st
    reads = not (any ((== Error) . gpSeverity) (gProblems (grGrid gr)))


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = concatMap one grs
  where
    grs = map (readGridTable ctx) bts
    one gr = case L.nub [ Cm.code f | f <- checkGrid ctx grs gr, Cm.isError f ] of
      []    -> map Emit (decls ctx gr)
      codes -> [ Excluded (btAnchor (stBound (grStructure gr))) (T.intercalate ", " codes <> " failed") ]

-- | A grid's declarations: its layout, and its record when the layout tiles.
decls :: Context -> GridRead -> [Gen.Decl]
decls ctx gr =
  layoutDecl (boundProvenance ctx (stBound st)) (layoutValue st) (stLayout st) (untiledNote (gridTiling (grGrid gr) (stLayout st)))
  : [ recordDecl ctx st | stTiles st ]
  where st = grStructure gr
