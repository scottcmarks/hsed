{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Spec.Anchor
Description : Anchor ids, their annotation kinds and the typed anchor record
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The primary key of an annotation is @(spec, id)@ (design section 3.2): @T46@
for a table, @5.1.3@ for a heading, @doc@ for the document, @3.3.10.2.1#3a@
for a list item or paragraph, @5.3.3.6/sig@ for a method signature block. The
annotation kind follows from the id and the payload (docs/ir-v1.md). An
'Anchor' carries the id with the shape its binding asserts, so that 'Hsed.Spec.DSL.at'
can pair it only with a binding of the same shape.
-}
module Hsed.Spec.Anchor
  ( -- * Ids
    AnchorId (..)
  , parseAnchorId
  , renderAnchorId
  , anchorIdent
  , annotKind
  , naturalKey
  , anchorTarget
    -- * Anchors
  , Anchor (..)
  , anchor
  , anchorLocation
  , locationText
  , withLocation
  ) where

import qualified Data.Text        as DT
import qualified RIO.Char         as C
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Common (Finding, SpecId (..))
import qualified Hsed.Spec.Common as C
import qualified Hsed.Spec.IR     as IR
import           Hsed.Spec.IR     (HeadingIR (..), ParaIR (..), ParaKind (..), PageRect (..), Rect (..),
                                   SpecIR (..), StructPath (..), TableIR (..))
import           Hsed.Spec.Shape  (Shape)


-- * Ids

-- | An anchor id, design section 3.2.
data AnchorId
  = TableId Int          -- ^ @T46@
  | SectionId Text       -- ^ @5.1.3@: a heading
  | DocId                -- ^ @doc@: the document
  | ItemId Text Text     -- ^ @3.3.10.2.1#3a@: a list item, or @#p3@ for the third paragraph
  | SigId Text           -- ^ @5.3.3.6/sig@: a method signature block
  deriving stock (Eq, Ord, Show)

-- | Read an id as the sidecar and the IR spell it.
parseAnchorId :: Text -> Maybe AnchorId
parseAnchorId t
  | t == "doc" = Just DocId
  | Just digits <- T.stripPrefix "T" t, isNumber digits = TableId <$> readMaybe (T.unpack digits)
  | Just sec <- T.stripSuffix "/sig" t, isSection sec = Just (SigId sec)
  | (sec, rest) <- T.break (== '#') t, Just item <- T.stripPrefix "#" rest
  , isSection sec, isItem item = Just (ItemId sec item)
  | isSection t = Just (SectionId t)
  | otherwise = Nothing
  where
    isNumber s = not (T.null s) && T.all C.isDigit s
    isSection s = not (T.null s) && all isNumber (T.split (== '.') s)
    isItem s = not (T.null s) && T.all C.isAlphaNum s

renderAnchorId :: AnchorId -> Text
renderAnchorId (TableId n)     = "T" <> tshow n
renderAnchorId (SectionId s)   = s
renderAnchorId DocId           = "doc"
renderAnchorId (ItemId s item) = s <> "#" <> item
renderAnchorId (SigId s)       = s <> "/sig"

-- | The id as a Haskell identifier fragment: @T46@, @5_1_3@, @doc@,
--   @3_3_10_2_1_i_3a@, @5_3_3_6_sig@. Distinct ids give distinct fragments: a
--   section's is digits and underscores, an item's alone holds @_i_@, a
--   signature block's alone ends @_sig@ without one, so heading 3.3.3.1 and item
--   3.3.3#1 do not meet in one name.
anchorIdent :: AnchorId -> Text
anchorIdent = \case
  TableId n     -> "T" <> tshow n
  SectionId s   -> dotted s
  DocId         -> "doc"
  ItemId s item -> dotted s <> "_i_" <> item
  SigId s       -> dotted s <> "_sig"
  where dotted = T.map (\c -> if c == '.' then '_' else c)

-- | The natural order of ids (@T9@ before @T10@, @5.9@ before @5.10@): runs of
--   digits compare as numbers, the rest as text, exactly as
--   @tools/extract/hsed_annot.py@'s @natural_key@ orders them, starting with a
--   (possibly empty) text run so that positions always compare like with like.
naturalKey :: Text -> [Either Text Integer]
naturalKey t = zipWith piece [0 :: Int ..] runs
  where
    groups = T.groupBy (\a b -> C.isDigit a == C.isDigit b) t
    runs   = case groups of
      g : _ | T.any C.isDigit (T.take 1 g) -> "" : groups
      _                                    -> groups
    piece i r
      | odd i     = Right (fromMaybe 0 (readMaybe (T.unpack r)))
      | otherwise = Left r

-- | What an anchor id names in an IR: the structure path, 0-based page and rect
--   'Hsed.Spec.Sidecar' places its annotation at (a table on its first part's
--   first rect, a heading, a paragraph or list item, @doc@ in the top-left inch
--   of page 1), or why it names nothing. A signature block names nothing: it is
--   rule-bound and carries no annotation.
anchorTarget :: SpecIR -> AnchorId -> Either Text (StructPath, Int, Rect)
anchorTarget ir = \case
  DocId -> Right (StructPath "", 0, Rect 36 720 108 756)
  TableId n -> case [ t | t <- ir.tables, t.id == Just ("T" <> tshow n) ] of
    t : _ | PageRect p r : _ <- t.rects -> Right (t.structPath, p, r)
    _ -> Left ("no table T" <> tshow n <> " in the IR")
  SectionId s -> case [ h | h <- ir.headings, h.section == Just s ] of
    [h] -> Right (h.structPath, h.page, h.rect)
    []  -> Left ("no heading numbered " <> s <> " in the IR")
    hs  -> Left (tshow (length hs) <> " headings numbered " <> s <> " in the IR")
  ItemId s item -> case paraHits s item of
    [p] -> Right (p.structPath, p.page, p.rect)
    []  -> Left ("no paragraph " <> s <> "#" <> item <> " in the IR")
    ps  -> Left (tshow (length ps) <> " paragraphs labelled " <> s <> "#" <> item <> " in the IR")
  SigId s -> Left ("signature blocks are rule-bound and carry no annotation: " <> s <> "/sig")
  where
    paraHits s item =
      let inSection = [ p | p <- ir.paras, p.section == Just s ]
      in case DT.stripPrefix "p" item >>= readMaybe . T.unpack of
           Just k  -> take 1 (drop (k - 1) [ p | k >= (1 :: Int), p <- inSection, p.kind == P ])
           Nothing -> [ p | p <- inSection, p.item == Just item ]

-- | The annotation kind of an id with its payload (docs/ir-v1.md): @doc@ is
--   'IR.Doc', a section is 'IR.Rule', a table is 'IR.Bind', an item is 'IR.Law';
--   a section or an item whose payload starts with @behaviour@ or @fails@ is
--   'IR.Behaviour'. A signature block is rule-bound and would only ever carry a 'IR.Bind'.
annotKind :: AnchorId -> Text -> IR.AnnotKind
annotKind anchorId payload = case anchorId of
  DocId       -> IR.Doc
  TableId _   -> IR.Bind
  SigId _     -> IR.Bind
  SectionId _ -> if behaviour then IR.Behaviour else IR.Rule
  ItemId _ _  -> if behaviour then IR.Behaviour else IR.Law
  where
    -- the first word past any opening parentheses, as tools/extract/hsed_annot.py's first_word reads it
    behaviour = case T.words (T.dropWhile (\c -> c == '(' || C.isSpace c) payload) of
      w : _ -> w `elem` ["behaviour", "fails"]
      []    -> False


-- * Anchors

-- | An anchor with the shape its binding asserts; the rest of the anchor record
--   (ordinal, label, header, words hash, fragment rects) lives in the manifest.
data Anchor (s :: Shape) = Anchor
  { aId     :: !AnchorId
  , aSpec   :: !SpecId
  , aStruct :: !StructPath   -- ^ the @/K@ index path, @0/1288@
  , aPage   :: !Int          -- ^ 1-based PDF page
  }
  deriving stock (Eq, Show)

-- | The anchor of a generated binding, @anchor \@'TypeRow "core-2.01" "T75" "0/1429" 107@.
--   The id must parse: generated modules only ever print ids that were parsed,
--   so a failure here is a bug in whatever wrote the module, and it is reported as one.
anchor :: forall s. Text -> Text -> Text -> Int -> Anchor s
anchor sid idText struct page = Anchor
  { aId     = fromMaybe (error ("Hsed.Spec.Anchor.anchor: not an anchor id: " <> T.unpack idText)) (parseAnchorId idText)
  , aSpec   = SpecId sid
  , aStruct = StructPath struct
  , aPage   = page
  }

-- | @core-2.01.pdf#page=100&T46@: where a finding points, in the form
--   'Hsed.Spec.Common.renderFinding' also produces and the LINE pragmas of the generated bindings cite.
anchorLocation :: Anchor s -> Text
anchorLocation a = locationText (aSpec a) (aPage a) (aId a)

locationText :: SpecId -> Int -> AnchorId -> Text
locationText sid page anchorId = specIdText sid <> ".pdf#page=" <> tshow page <> "&" <> renderAnchorId anchorId

-- | Give a finding without a location the anchor's.
withLocation :: Anchor s -> Finding -> Finding
withLocation a f = f { C.spec = Just (aSpec a), C.anchor = Just (renderAnchorId (aId a)), C.page = Just (aPage a) }
