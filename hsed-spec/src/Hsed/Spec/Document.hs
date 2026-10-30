{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Document
Description : The checks of the document as a whole: its List of Tables and its table references
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Two checks of design section 4.2's catalogue that read no table's cells but
the document around them (IR @lot@ and @xrefs@, docs/ir-v1.md). Both report
the PDF, never hsed, so both are Warnings; @hsedc compile@ lists them in
@build/report/ERRATA.md@ ("Hsed.Spec.Report").

* @LOT-VS-BODY@: the List of Tables of the front matter against the captions
  in the body. Every numbered table has exactly one entry, every entry numbers
  a table, and the entry gives the caption's title (as printed, compared word
  by word, since either may wrap) and the printed page the table begins on. A
  missing entry is located at the table, every other difference at the entry.
  An IR without a List of Tables (a document that has none, or a file written
  before M2b) has nothing to compare.

* @XREF-RESOLVES@: every @Table N@ in the prose (a paragraph or a list item;
  a reference in a table's cell is out of scope, as the IR's @xrefs@ are)
  against the table it names. The number must name a numbered table, and the
  PDF's own link over it, when it has one, must land on a page of that table.
  Where the text says which table it means, it must be that one:

    * a name printed just before the reference, @Special Purpose UIDs (Table
      239)@, must be the caption's title, its words in order, a word of the
      caption left out allowed (@Single Row Table UIDs@ for @Single Row Table
      Row UIDs@); the name is the words after the last comma, semicolon or
      colon before it;
    * in a section that defines an identifier (its heading is the identifier
      and it holds the table captioned with it alone, as a type's section in
      Core 5.1.3 does), a reference names one of that identifier's tables, a
      table whose caption starts with it. The finding names the tables that
      do, and another reference to the same table whose sentence names the
      identifier, which may make the reference right after all (the Secure
      column's values are messaging_type's, PDF 167); the finding quotes the
      reference's own sentence first, then that other reference by its page.

  A reference followed by @of [k]@ or @in [k]@ names a table of another
  document and is not checked. Each finding is located at the reference's
  page and section (@core-2.01.pdf#page=116&5.1.3.55@) and quotes its sentence.
-}
module Hsed.Spec.Document
  ( documentChecks
  , lotChecks
  , xrefChecks
  ) where

import qualified Data.Text         as DT
import qualified RIO.Char          as C
import qualified RIO.List          as L
import qualified RIO.Map           as Map
import qualified RIO.Text          as T
import           RIO

import           Hsed.Spec.Anchor  (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common  (Finding (Finding), Severity (..), SpecId (..))
import           Hsed.Spec.IR      (HeadingIR (..), LotIR (..), SpecIR (..), TableIR (..), XrefIR (..))
import           Hsed.Spec.RuleDef (numberedTables, tablePage)
import           Hsed.Spec.Scope   (tableSections)
import           Hsed.Spec.Shape   (normaliseCaption, rawCaptionTitle)


-- | Both checks of the document.
documentChecks :: SpecIR -> [Finding]
documentChecks ir = lotChecks ir <> xrefChecks ir

-- | A Warning at a 1-based PDF page and, when there is one, an anchor.
warnAt :: SpecIR -> Text -> Int -> Maybe Text -> Text -> Finding
warnAt ir c pdf aid msg = Finding Warning c (Just (SpecId ir.spec)) aid (Just pdf) msg

-- | The title of a caption as printed: Table 112's @MethodID_object _ref@ keeps its space.
printedTitle :: TableIR -> Text
printedTitle t = fromMaybe "" (t.caption >>= rawCaptionTitle)

-- | A text in double quotes, as printed (no escapes: the PDF's dashes and quotes stay readable).
quote :: Text -> Text
quote t = "\"" <> t <> "\""

-- | The words of a text, its spacing and line breaks ignored.
sameWords :: Text -> Text -> Bool
sameWords a b = T.words a == T.words b


-- * LOT-VS-BODY

-- | The List of Tables against the captions; nothing when the IR has no List of Tables.
lotChecks :: SpecIR -> [Finding]
lotChecks ir
  | null ir.lot = []
  | otherwise   = missing <> repeated <> unknown <> concatMap compareEntry (Map.toList entries)
  where
    tables  = numberedTables ir
    entries = Map.fromListWith (flip (<>)) [ (TableId e.number, [e]) | e <- ir.lot ]
    atEntry :: LotIR -> Text -> Finding
    atEntry e = warnAt ir "LOT-VS-BODY" (e.page + 1) (Just ("T" <> tshow e.number))
    missing =
      [ warnAt ir "LOT-VS-BODY" pdf (Just (renderAnchorId aid))
          ("Table " <> tshow n <> " (" <> printedTitle t <> ", PDF " <> tshow pdf <> ") is not in the List of Tables")
      | (aid@(TableId n), t) <- Map.toList tables, Map.notMember aid entries, let pdf = fromMaybe 1 (tablePage t) ]
    repeated =
      [ atEntry e ("the List of Tables lists Table " <> tshow e.number <> " " <> tshow (length es) <> " times")
      | es@(_ : e : _) <- Map.elems entries ]
    unknown =
      [ atEntry e ("the List of Tables lists Table " <> tshow e.number <> " (" <> e.title <> "), which no caption in the body numbers")
      | e <- ir.lot, Map.notMember (TableId e.number) tables ]
    compareEntry :: (AnchorId, [LotIR]) -> [Finding]
    compareEntry (aid, e : _) = case Map.lookup aid tables of
      Nothing -> []
      Just t  ->
        [ atEntry e ("the List of Tables calls Table " <> tshow e.number <> " " <> quote e.title
                     <> "; its caption on PDF " <> tshow pdf <> " reads " <> quote (printedTitle t))
        | not (sameWords e.title (printedTitle t)) ]
        <> [ atEntry e ("the List of Tables gives page " <> tshow e.printed <> " for Table " <> tshow e.number
                        <> ", which begins on page " <> tshow p <> " (PDF " <> tshow pdf <> ")")
           | Just p <- [printedPage t], p /= e.printed ]
        where pdf = fromMaybe 1 (tablePage t)
    compareEntry (_, []) = []
    printedPage :: TableIR -> Maybe Int
    printedPage t = listToMaybe t.printedPages <|> fmap (subtract ir.labelOffset) (tablePage t)


-- * XREF-RESOLVES

-- | Every table reference in the prose against the table it names.
xrefChecks :: SpecIR -> [Finding]
xrefChecks ir = concatMap check (filter (not . elsewhere) ir.xrefs)
  where
    tables   = numberedTables ir
    sections = tableSections ir
    headings = Map.fromListWith (\_ firstOne -> firstOne) [ (s, h.title) | h <- ir.headings, Just s <- [h.section] ]
    sectionOfTable :: TableIR -> Maybe Text
    sectionOfTable t = join (Map.lookup t.structPath sections)
    pagesOf :: Int -> [Int]
    pagesOf n = L.nub [ p | t <- ir.tables, t.id == Just ("T" <> tshow n), p <- t.pages ]
    title :: TableIR -> Text
    title t = normaliseCaption (printedTitle t)
    firstWord = fromMaybe "" . listToMaybe . T.words

    here :: XrefIR -> Text -> Finding
    here x = warnAt ir "XREF-RESOLVES" (x.page + 1) x.section
    quoted, ref :: XrefIR -> Text
    quoted x = ": " <> quote x.sentence
    ref x = "\"Table " <> tshow x.number <> "\""

    check :: XrefIR -> [Finding]
    check x = case Map.lookup (TableId x.number) tables of
      Nothing -> [here x (ref x <> " names no numbered table" <> quoted x)]
      Just t  -> linked x <> namedBefore x t <> subject x t

    -- the PDF's link over the number lands on the table
    linked :: XrefIR -> [Finding]
    linked x = case x.target of
      Just p | p `notElem` pagesOf x.number ->
        [here x (ref x <> " links to PDF " <> tshow (p + 1) <> ", but Table " <> tshow x.number <> " lies on PDF "
                   <> T.intercalate ", " (map (tshow . (+ 1)) (pagesOf x.number)) <> quoted x)]
      _ -> []

    -- "<name> (Table N)": the name is the caption's title
    namedBefore, subject :: XrefIR -> TableIR -> [Finding]
    namedBefore x t = case nameBefore x of
      Just name | not (wordsWithin (wordsOf name) (wordsOf (printedTitle t))) ->
        [here x ("the text calls Table " <> tshow x.number <> " " <> quote name <> ", its caption " <> quote (printedTitle t) <> quoted x)]
      _ -> []

    -- a section defining an identifier refers to that identifier's tables
    subject x t = case definedIn x of
      Just (ident, defining) | firstWord (title t) /= ident ->
        [here x (T.concat
          [ ref x, " in ", quote x.sentence, " names ", title t, sectionText (sectionOfTable t), ", but section "
          , fromMaybe "" x.section, " defines ", ident, " (Table ", tshow (tableNo defining), ") and this is no table of it; "
          , candidates ident defining, corroborated x ident ])]
      _ -> []
    definedIn :: XrefIR -> Maybe (Text, TableIR)
    definedIn x = do
      sec <- x.section
      ident <- normaliseCaption <$> Map.lookup sec headings
      guard (length (T.words ident) == 1)
      defining <- listToMaybe [ t | t <- Map.elems tables, sectionOfTable t == Just sec, title t == ident ]
      pure (ident, defining)
    candidates :: Text -> TableIR -> Text
    candidates ident defining = case [ t | t <- Map.elems tables, firstWord (title t) == ident, t /= defining ] of
      [] -> "no other table's caption starts with " <> ident
      ts -> "its own: " <> T.intercalate ", " [ "Table " <> tshow (tableNo c) <> " " <> title c <> sectionText (sectionOfTable c) | c <- ts ]
    corroborated :: XrefIR -> Text -> Text
    corroborated x ident = T.concat
      [ "; yet the reference to Table " <> tshow x.number <> " on PDF " <> tshow (o.page + 1) <> sectionText o.section
          <> " names " <> ident <> ", which may make this one right"
      | o <- take 1 [ o | o <- ir.xrefs, o.number == x.number, o /= x, T.toLower ident `elem` wordsOf o.sentence ] ]
    sectionText = maybe "" (\s -> " (section " <> s <> ")")
    tableNo :: TableIR -> Int
    tableNo t = fromMaybe 0 t.tableNumber

-- | A reference followed by @of [k]@ or @in [k]@ names a table of another document.
elsewhere :: XrefIR -> Bool
elsewhere x = case afterRefs x.number x.sentence of
  []    -> False
  rests -> all (\rest -> any (`T.isPrefixOf` T.stripStart rest) ["of [", "in ["]) rests

-- | The text after each @Table N@ of a sentence, the number as printed (@Table 03@ is 3).
afterRefs :: Int -> Text -> [Text]
afterRefs n sentence =
  [ T.drop (T.length digits) rest
  | (_, at') <- DT.breakOnAll "Table" sentence
  , let rest   = T.stripStart (T.drop 5 at')
        digits = T.takeWhile C.isDigit rest
  , readMaybe (T.unpack digits) == Just n ]

-- | The name a sentence prints just before @(Table N)@: the words after the
--   last comma, semicolon or colon before it, or from the sentence's start.
nameBefore :: XrefIR -> Maybe Text
nameBefore x = listToMaybe
  [ T.strip (T.takeWhileEnd (`notElem` (",;:" :: String)) before)
  | (before, rest) <- DT.breakOnAll "(Table " x.sentence
  , let digits = T.takeWhile C.isDigit (T.drop 7 rest)
  , readMaybe (T.unpack digits) == Just x.number
  , ")" `T.isPrefixOf` T.drop (7 + T.length digits) rest
  , not (null (wordsOf (T.takeWhileEnd (`notElem` (",;:" :: String)) before))) ]

-- | Words for comparing names: runs of letters, digits and underscores, case folded.
wordsOf :: Text -> [Text]
wordsOf = filter (not . T.null) . T.split (\c -> not (C.isAlphaNum c || c == '_')) . T.toLower

-- | The first list's words in order within the second's, others between them allowed.
wordsWithin :: [Text] -> [Text] -> Bool
wordsWithin = L.isSubsequenceOf
