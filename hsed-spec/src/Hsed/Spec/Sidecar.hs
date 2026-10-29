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
comments and blank lines ignored (docs/ir-v1.md, design section 5.3); an anchor
listed twice is refused, as @tools/extract/hsed_annot.py@ refuses it. Resolving a
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
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Anchor (annotKind, anchorTarget, naturalKey, parseAnchorId, renderAnchorId)
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

-- | The anchor lines of a sidecar; a malformed line, and every line after the
--   first of an anchor listed more than once, is an @E-SIDECAR@ finding naming it.
parseSidecar :: Text -> ([Finding], [SidecarLine])
parseSidecar text = (bad <> repeated, firsts)
  where
    (bad, ls) = partitionEithers (mapMaybe line (zip [1 ..] (T.lines text)))
    firstLine = Map.fromListWith min [ ((slSpec l, slId l), slLine l) | l <- ls ]
    isFirst l = Map.lookup (slSpec l, slId l) firstLine == Just (slLine l)
    firsts    = filter isFirst ls
    repeated  = [ finding Error "E-SIDECAR" ("line " <> tshow (slLine l) <> ": anchor " <> slSpec l <> " | " <> slId l
                    <> " is listed more than once (first at line " <> maybe "?" tshow (Map.lookup (slSpec l, slId l) firstLine) <> ")")
                | l <- ls, not (isFirst l) ]
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
      Just aid -> bimap bad (annot aid) (anchorTarget ir aid)
  where
    bad msg = (finding Error "E-SIDECAR" ("line " <> tshow (slLine l) <> ": " <> msg)) { C.spec = Just (SpecId ir.spec) }
    annot aid (struct, page, rect) = AnnotIR
      { kind = annotKind aid (slPayload l), spec = ir.spec, id = renderAnchorId aid, struct = struct
      , v = 1, page = page, rect = rect, flags = 6, contents = slPayload l }

-- | Every line resolved; the failures as findings.
resolveSidecar :: SpecIR -> Text -> ([Finding], [AnnotIR])
resolveSidecar ir text = (bad <> lefts resolved, rights resolved)
  where
    (bad, ls) = parseSidecar text
    resolved  = map (resolveLine ir) ls

-- | The IR with its annotations replaced by a sidecar's, in canonical order
--   (kind order, then the anchor id's natural order), the order annotate.py
--   writes them in and the extractor reads them back.
withSidecar :: SpecIR -> Text -> ([Finding], SpecIR)
withSidecar ir text = (fs, ir { annots = L.sortOn order as })
  where
    (fs, as) = resolveSidecar ir text
    order :: AnnotIR -> (Int, [Either Text Integer])
    order a = (fromEnum a.kind, naturalKey a.id)
