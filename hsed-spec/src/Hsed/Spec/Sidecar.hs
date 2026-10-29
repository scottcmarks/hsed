{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Sidecar
Description : The annotation sidecar (.hsa) read and resolved against an IR
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@spec\/annot\/\<spec\>.hsa@ holds one anchor per line, @spec | id | payload@, with @#@
comments and blank lines ignored (docs/ir-v1.md, design section 5.3). Resolving a
line against an IR gives the 'AnnotIR' the extractor would read back from a PDF
annotated with it, the same way @tools/extract/annotate.py@ places it: a table on
its first part's first rect, a heading on its rect, a paragraph or list item on
its rect, @doc@ in the top-left inch of page 1. So @hsedc@ can check a sidecar
(the negative fixtures, a proposed edit) without writing and re-extracting a PDF.
-}
module Hsed.Spec.Sidecar
  ( SidecarLine (..)
  , parseSidecar
  , resolveLine
  , resolveSidecar
  , withSidecar
  ) where

import qualified Data.Text        as DT
import qualified RIO.List         as L
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Anchor (AnchorId (..), annotKind, parseAnchorId, renderAnchorId)
import           Hsed.Spec.Common (Finding, Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common as C
import           Hsed.Spec.IR


-- | One line of a sidecar, with its 1-based line number.
data SidecarLine = SidecarLine
  { slLine    :: !Int
  , slSpec    :: !Text
  , slId      :: !Text
  , slPayload :: !Text
  }
  deriving (Eq, Show)

-- | The anchor lines of a sidecar; a malformed line is an @E-SIDECAR@ finding naming it.
parseSidecar :: Text -> ([Finding], [SidecarLine])
parseSidecar = partitionEithers . mapMaybe line . zip [1 ..] . T.lines
  where
    line (n, raw)
      | T.null stripped || "#" `T.isPrefixOf` stripped = Nothing
      | (s, rest1) <- DT.breakOn "|" stripped, not (T.null rest1)
      , (i, rest2) <- DT.breakOn "|" (T.drop 1 rest1), not (T.null rest2)
      = Just (Right (SidecarLine n (T.strip s) (T.strip i) (T.strip (T.drop 1 rest2))))
      | otherwise = Just (Left (finding Error "E-SIDECAR" ("line " <> tshow (n :: Int) <> ": expected spec | id | payload, got " <> tshow raw)))
      where stripped = T.strip raw

-- | The annotation a line stands for in this IR, or a finding naming the line.
resolveLine :: SpecIR -> SidecarLine -> Either Finding AnnotIR
resolveLine ir l
  | slSpec l /= ir.spec = Left (bad ("spec is " <> slSpec l <> ", the IR is " <> ir.spec))
  | otherwise = case parseAnchorId (slId l) of
      Nothing  -> Left (bad ("malformed anchor id " <> tshow (slId l)))
      Just aid -> annot aid <$> place aid
  where
    bad msg = (finding Error "E-SIDECAR" ("line " <> tshow (slLine l) <> ": " <> msg)) { C.spec = Just (SpecId ir.spec) }
    annot aid (struct, page, rect) = AnnotIR
      { kind = annotKind aid (slPayload l), spec = ir.spec, id = renderAnchorId aid, struct = struct
      , v = 1, page = page, rect = rect, flags = 6, contents = slPayload l }
    place = \case
      DocId -> Right (StructPath "", 0, Rect 36 720 108 756)
      TableId n -> case [ t | t <- ir.tables, t.id == Just ("T" <> tshow n) ] of
        t : _ | PageRect p r : _ <- t.rects -> Right (t.structPath, p, r)
        _ -> Left (bad ("no table T" <> tshow n <> " in the IR"))
      SectionId s -> case [ h | h <- ir.headings, h.section == Just s ] of
        [h] -> Right (h.structPath, h.page, h.rect)
        []  -> Left (bad ("no heading numbered " <> s <> " in the IR"))
        hs  -> Left (bad (tshow (length hs) <> " headings numbered " <> s <> " in the IR"))
      ItemId s item -> case paraHits s item of
        [p] -> Right (p.structPath, p.page, p.rect)
        []  -> Left (bad ("no paragraph " <> s <> "#" <> item <> " in the IR"))
        ps  -> Left (bad (tshow (length ps) <> " paragraphs labelled " <> s <> "#" <> item <> " in the IR"))
      SigId s -> Left (bad ("signature blocks are rule-bound and carry no annotation: " <> s <> "/sig"))
    paraHits s item =
      let inSection = [ p | p <- ir.paras, p.section == Just s ]
      in case T.stripPrefix "p" item >>= readMaybe . T.unpack of
           Just k  -> take 1 (drop (k - 1) [ p | k >= (1 :: Int), p <- inSection, p.kind == P ])
           Nothing -> [ p | p <- inSection, p.item == Just item ]

-- | Every line resolved; the failures as findings.
resolveSidecar :: SpecIR -> Text -> ([Finding], [AnnotIR])
resolveSidecar ir text = (bad <> lefts resolved, rights resolved)
  where
    (bad, ls) = parseSidecar text
    resolved  = map (resolveLine ir) ls

-- | The IR with its annotations replaced by a sidecar's, in canonical order
--   (kind order, then the anchor id's natural order).
withSidecar :: SpecIR -> Text -> ([Finding], SpecIR)
withSidecar ir text = (fs, ir { annots = L.sortOn order as })
  where
    (fs, as) = resolveSidecar ir text
    order :: AnnotIR -> (Int, Maybe AnchorId)
    order a = (fromEnum a.kind, parseAnchorId a.id)
