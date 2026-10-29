{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
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
    -- * Anchors
  , Anchor (..)
  , anchor
  , anchorLocation
  , locationText
  , withLocation
  ) where

import qualified RIO.Char         as C
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Common (Finding, SpecId (..))
import qualified Hsed.Spec.Common as C
import qualified Hsed.Spec.IR     as IR
import           Hsed.Spec.IR     (StructPath (..))
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

-- | The id as a Haskell identifier fragment: @T46@, @5_1_3@, @doc@, @3_3_10_2_1_3a@, @5_3_3_6_sig@.
anchorIdent :: AnchorId -> Text
anchorIdent = T.map (\c -> if C.isAlphaNum c then c else '_') . renderAnchorId

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
    behaviour = case T.words payload of
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
