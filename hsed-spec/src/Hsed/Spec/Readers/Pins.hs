{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Readers.Pins
Description : The sentences that pin a byte grid's fields to the values a TPer SHALL send
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The pins reader of design example E8, which the @byteGrid@ and @feature@
rules share and no comment names. The prose of the section a byte grid lies
in (and of its subsections) states the values some of its fields SHALL have,
in one of three schemas:

* 'CoreSet': @The \<Field\> field SHALL be set to \<V\>.@ (Core 3.3.6.4 and
  3.3.6.5, Tables 42 and 43: @The Feature Code field SHALL be set to 0x0001.@);
* 'CoreValue': @The value of this field SHALL be \<V\>.@, the field being the
  one its subsection's heading names (Core 3.3.6.2.2 Data Structure Major
  Version, Table 39: @The value of this field SHALL be 0x0000.@), or the
  numbered list item before it in its section, where the PDF numbers a
  field's heading deeper than its outline goes (Core 3.2.3.4.2.1.2 Kind, a
  list item under 3.2.3.4.2.1);
* 'OpalBullets': @\<Field\> = \<V\>@, the top-level bullets after a paragraph
  saying the device @SHALL return the following@ (Opal 3.1.1.1-3.1.1.6, Tables
  2-8: @Feature Code = 0x0001@, @Sync Supported = 1@, @Base ComID = VU@).

A value reads as a number ('PinExact': @0x0001@, @0001h@, @0x0000_0001@, @1@,
as "Hsed.Spec.Readers.Text".'Hsed.Spec.Readers.Text.parseNumber' reads it),
as vendor unique (@VU@, or @0x1 or any version that supports the defined
features in this SSC@: 'PinVU'), as a minimum (@4 or larger@: 'PinAtLeast'),
or not ('PinOther', @**@, @See section 3.1.1.3.1@), the text as printed kept
beside it. Only these sentences pin: Core's @For Credit Control Subpackets,
this field is set to 0x8001.@ and @This is always 0x00000004 …@ (3.2.3.4.2.1,
PDF 45) are read by none of the schemas (design 13.26). The reader returns
the pins of a table's section ('tablePins', 'pinsInSection'), in document
order, as data; matching them to the grid's fields is the rules', and
'pinsFindings' the one criterion of its codes, whichever rule asks. It reads
the words of the paragraphs (IR @words@, design section 5.1 step 7), which the
extractor gives for every paragraph of a section a byte grid lies in and of
its subsections (IR @wordComplete@); a section without them is a Left, and
@C-PINS-WORDS@, an Error, since the IR breaks its own contract.
-}
module Hsed.Spec.Readers.Pins
  ( -- * Pins
    Pin (..)
  , PinSchema (..)
  , PinValue (..)
  , pinsInSection
  , tablePins
  , readPinValue
  , pinsFindings
    -- * Codes
  , pinsCodes
  ) where

import qualified Data.Text              as DT
import qualified RIO.List               as L
import qualified RIO.Map                as Map
import qualified RIO.Text               as T
import           RIO

import           Hsed.Spec.Common       (CodeDef (..), Finding, Reports (..), Severity (..), finding)
import           Hsed.Spec.IR           (HeadingIR (..), ParaIR (..), ParaKind (..), SpecIR (..))
import           Hsed.Spec.Readers.Text (readNumber)
import           Hsed.Spec.RuleDef      (BoundTable, Context (..), atTable, sectionOf)
import           Hsed.Spec.Scope        (Section (..), covers)


-- | The three sentence schemas of design example E8.
data PinSchema = CoreSet | CoreValue | OpalBullets
  deriving (Eq, Show, Enum, Bounded)

-- | What a pin's value says.
data PinValue
  = PinExact !Integer    -- ^ @0x0001@, @1@
  | PinVU                -- ^ @VU@, or a value "or any version …" the device chooses
  | PinAtLeast !Integer  -- ^ @0x0001 or larger@
  | PinOther             -- ^ anything else (@**@, @See section 3.1.1.3.1@): see 'pinText'
  deriving (Eq, Show)

-- | One pin: the field as the sentence names it, its value, and where it is.
data Pin = Pin
  { pinSchema  :: !PinSchema
  , pinField   :: !Text      -- ^ @Feature Code@; for 'CoreValue', its section's heading title
  , pinValue   :: !PinValue
  , pinText    :: !Text      -- ^ the value as printed
  , pinSection :: !Text
  , pinPage    :: !Int       -- ^ 0-based PDF page of its paragraph
  }
  deriving (Eq, Show)

-- | The pins of a bound table: those of the section it lies in and of its
--   subsections (none when it lies in no numbered section). Left when the IR
--   carries no words for a paragraph there.
tablePins :: Context -> BoundTable -> Either Text [Pin]
tablePins ctx bt = maybe (Right []) (pinsInSection (cxIR ctx)) (sectionOf ctx bt)

-- | The pins of a section and its subsections, in document order.
pinsInSection :: SpecIR -> Text -> Either Text [Pin]
pinsInSection ir sec = case [ p | p <- paras, isNothing p.words ] of
  p : _ -> Left ("section " <> fromMaybe "" p.section <> " has a paragraph without its words in the IR (wordComplete)")
  []    -> Right (concat (zipWith pinsOf [0 ..] paras))
  where
    paras = [ p | p <- ir.paras, covers (Subtree sec) p.section ]
    titles = Map.fromList [ (s, h.title) | h <- ir.headings, Just s <- [h.section] ]
    text :: ParaIR -> Text
    text p = T.unwords (fromMaybe [] p.words)
    pinsOf :: Int -> ParaIR -> [Pin]
    pinsOf i p = map (\(sch, f, v) -> mkPin sch f v p) (sentencePins p <> bulletPins i p)
    mkPin :: PinSchema -> Text -> Text -> ParaIR -> Pin
    mkPin sch f v p = Pin sch f (readPinValue v) v (fromMaybe "" p.section) p.page
    sentencePins :: ParaIR -> [(PinSchema, Text, Text)]
    sentencePins p =
      [ (CoreSet, f, v) | (f, v) <- sentences "The " " field SHALL be set to " (text p) ]
      <> [ (CoreValue, title, v) | ("", v) <- sentences "" "The value of this field SHALL be " (text p)
                                 , Just title <- [fieldHeading p <|> (p.section >>= (`Map.lookup` titles))] ]
    -- the list item before a paragraph in its section that numbers a heading deeper than the outline goes
    -- (3.2.3.4.2.1.2 Kind under 3.2.3.4.2.1): its title
    fieldHeading :: ParaIR -> Maybe Text
    fieldHeading p = L.lastMaybe
      [ T.unwords rest
      | q <- takeWhile (/= p) paras, q.section == p.section, q.kind == LI
      , Just (n : rest@(_ : _)) <- [q.words], Just s <- [p.section], (s <> ".") `T.isPrefixOf` n ]
    -- a top-level bullet "<Field> = <V>" among the list items right after a paragraph that says
    -- the device SHALL return the following, in the same section
    bulletPins :: Int -> ParaIR -> [(PinSchema, Text, Text)]
    bulletPins i p = case (p.kind, p.item) of
      (LI, Just item) | DT.count "•" item == 1, follows i p -> case DT.breakOn " = " (text p) of
        (f, v) | not (T.null v), not (T.null f) -> [(OpalBullets, T.strip f, T.dropWhileEnd (`elem` (".,;" :: String)) (T.strip (T.drop 3 v)))]
        _                                       -> []
      _ -> []
    follows :: Int -> ParaIR -> Bool
    follows i p = case dropWhile (\q -> q.kind == LI && q.section == p.section) (reverse (take i paras)) of
      q : _ -> q.kind == P && q.section == p.section && "SHALL return the following" `T.isInfixOf` text q
      []    -> False

-- | Each sentence of a text that is @\<lead\>\<Field\>\<marker\>\<V\>.@: the
--   field (the words between the lead, at the start of a sentence, and the
--   marker) and the value (the word after the marker, its full stop dropped).
sentences :: Text -> Text -> Text -> [(Text, Text)]
sentences lead marker t =
  [ (f, v)
  | (before, after) <- DT.breakOnAll marker t
  , Just f <- [T.stripPrefix lead (lastSentence before)]
  , Just v <- [T.stripSuffix "." =<< listToMaybe (T.words (T.drop (T.length marker) after))] ]
  where
    -- the text after the last sentence end before the marker
    lastSentence before = maybe before (T.drop 2 . snd) (L.lastMaybe (DT.breakOnAll ". " before))

-- | A pin's value as printed: a number, vendor unique, a minimum, or other.
readPinValue :: Text -> PinValue
readPinValue t
  | ws == ["VU"]                                       = PinVU
  | Just n <- readNumber t                             = PinExact n
  | "or any version" `T.isInfixOf` t                   = PinVU
  | [w, "or", "larger"] <- ws, Just n <- readNumber w  = PinAtLeast n
  | otherwise                                          = PinOther
  where ws = T.words t


-- * Findings

-- | The pins' findings at a bound grid, given its name and whether any pin
--   of its section names one of its fields: @C-PINS-WORDS@ (an Error) where
--   its section's pins cannot be read; @W-PINS-NONE@ (a Warning) where none
--   names a field of it while pins are in use around it, its section's pins
--   naming other fields (Core Table 38, which shares 3.3.6.2 with Table 39)
--   or the word-complete sections beside it (under the same parent) stating
--   pins (Tables 41 and 44 in 3.3.6.3 and 3.3.6.6, beside 3.3.6.2, 3.3.6.4
--   and 3.3.6.5). Where no section around a grid states a pin by one of the
--   three schemas (the ComPacket family's, T212's) its having none is no
--   finding.
pinsFindings :: Context -> BoundTable -> Text -> Bool -> [Finding]
pinsFindings ctx bt name matched = case tablePins ctx bt of
  Left why -> [ at (finding Error "C-PINS-WORDS" (name <> ": its section's pins cannot be read: " <> why)) ]
  Right ps
    | matched           -> []
    | not (null ps)     -> [ warn ("the pins of " <> sec <> " (" <> T.intercalate ", " (L.nub (map pinField ps)) <> ") name none of its fields") ]
    | not (null beside) -> [ warn ("no sentence of " <> sec <> " or its subsections pins one of its fields, while "
                                   <> T.intercalate ", " beside <> " pin fields of the grids beside it") ]
    | otherwise         -> []
  where
    at = atTable ctx bt
    warn = at . finding Warning "W-PINS-NONE" . ((name <> ": ") <>)
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


-- * Codes

-- | The pins' codes, whichever rule reads them.
pinsCodes :: [CodeDef]
pinsCodes =
  [ CodeDef "W-PINS-NONE" [Warning] Bookkeeping
  , CodeDef "C-PINS-WORDS" [Error] Bookkeeping ]
