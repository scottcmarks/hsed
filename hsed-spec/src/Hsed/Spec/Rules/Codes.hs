{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.Codes
Description : The codes rule: a field's code values and what each row says they mean
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4. @codes@ fires on a table of two or three columns whose
first header cell is code-like (the @Codes@ shape: @… Code@, @ID@, @ComID@,
@… Type@, @… Value@, @Bit@, @… Version Number@, read without a parenthesised
type after it; no enumeration or typed-record table), and, the rule's own
conditions, whose caption does not name a type's enumeration (@\<type\>
Enumeration Values@, which @enumValues@ reads: Table 87, three columns, is one
the shape accepts) and one of whose columns holds a code in every row: the
first such column is the code column. A code is a value in hex (@0x07FF@, @0003h@) or in
decimal, a range (@a-b@, @a – b@, @a to b@, @a..b@), a lower bound (@>0x00@:
every greater value) or @All others@. The design's condition, "every first
cell" a code, is too strict for today's extraction: Table 19 prints its codes
in its second column (@Subpacket Type | Kind Field Value@) and Table 25 ends
with @All others@; reading the first column that is codes throughout binds
both and changes nothing for the other six. A row of one cell in a wider
table is the table's notes (Opal Table 10), not a code. In Core 2.01 the rule
rides on the @doc@ anchor and binds Tables 15, 16, 19, 25, 26, 40, 179 and 235;
it fires on no other table, Table 87 included, which its comment binds as an
enumeration.

Checks:

* @C-CODE-VALUE@ (Error): the table has a code column, every row is as wide
  as the table (or a single note cell), every range runs upwards, and at
  most one row is @All others@;
* @W-CODE-OVERLAP@ (Warning): no value is assigned by two rows (the first is
  kept, as 'Hsed.Core.Code.codeRow' reads the table);
* @C-CODE-TYPE@ (Error): a code column's header that names its values' type
  in parentheses (@Start Transaction Status Code (uinteger)@) names a Type
  object of the spec (a table "Hsed.Spec.Rules.TypeRow" binds).

Compiled into 'codesModule': for each table @c\<Title\> :: [CodeRow]@ (the
caption's words joined, @cComIDAssignments@), one 'Hsed.Core.Code.CodeRow' per
row with its span (numbers as printed: hex stays hex) and its other cells in
column order, as printed. The @statusCodes@ rule ("Hsed.Spec.Rules.StatusCodes")
writes the same module.
-}
module Hsed.Spec.Rules.Codes
  ( codesRule
  , codesModule
    -- * Code cells
  , Number (..)
  , CodeCell (..)
  , parseCodeCell
  , parseNumber
  , cellSpan
  , codeCellExpr
  , numberExpr
  , codeColumn
    -- * Reading the bound tables
  , CodeRead (..)
  , CodeRowRead (..)
  , codeReads
    -- * Declarations
  , titleIdent
  , codeRowsDecl
  , listLines
  ) where

import qualified Data.Char         as DC
import qualified Data.Text         as DT
import qualified RIO.Char          as C
import qualified RIO.List          as L
import qualified RIO.Text          as T
import           RIO
import           Numeric           (showHex)

import           Hsed.Core.Code    (Span (..))
import           Hsed.Spec.Anchor  (AnchorId (..))
import           Hsed.Spec.Common  (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common  as Cm
import           Hsed.Spec.DSL     (SomeBinding (..), codes)
import qualified Hsed.Spec.Gen     as Gen
import           Hsed.Spec.IR      (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TypeRow (typeEnvFrom)
import           Hsed.Spec.Shape   (CaptionKind (..), Form (..), Shape (..), captionTitle, normaliseCaption, shapeMismatch)


-- | The rule.
codesRule :: RuleDef
codesRule = RuleDef
  { ruleName    = RuleName "codes"
  , bindsTo     = SomeBinding codes
  , exemplar    = (SpecId "core-2.01", TableId 25)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ t form -> isNothing (shapeMismatch Codes form) && not (enumeration form) && isJust (codeColumn t)
  , ruleIdent   = const Nothing
  , ruleModule  = codesModule
  , ruleChecks  = \ctx bts -> concatMap (either pure (checkCodes ctx)) (readCodes ctx bts)
  , ruleCompile = compile
  }

-- | A caption naming a type's enumeration (@gen_status Enumeration Values@):
--   the values of a Type object, which @enumValues@ reads, not a code table.
enumeration :: Form -> Bool
enumeration form = case fCaptionKind form of
  CaptionEnumValues _ -> True
  _                   -> False

-- | @Hsed.Gen.\<Spec\>.Codes@, which the @statusCodes@ rule writes too.
codesModule :: Gen.GenModule
codesModule = Gen.GenModule
  { Gen.leaf        = "Codes"
  , Gen.description = "Code tables: the values each row of a table assigns, and what its other cells say they mean"
  , Gen.notes       = [ "Each table is a list of CodeRow (Hsed.Core.Code), its numbers as printed (hex stays"
                      , "hex), its other cells in column order as printed; codeRow finds the row assigning a value." ] }


-- * Code cells

-- | A number as printed: its value, and how many hex digits it was printed with (Nothing in decimal).
data Number = Number
  { nValue     :: !Integer
  , nHexDigits :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | A code cell read: the values it assigns, its numbers as printed.
data CodeCell
  = CodeOne !Number
  | CodeBetween !Number !Number
  | CodeAbove !Number
  | CodeOthers
  deriving (Eq, Show)

-- | A code cell: @0x0001@, @0003h@, @10@, @0x0002-0x07FF@, @0003h – 00FFh@,
--   @10 to 23@, @1..65534@, @>0x00@ or @All others@.
parseCodeCell :: Text -> Maybe CodeCell
parseCodeCell cell
  | T.toLower (T.unwords (T.words t)) == "all others" = Just CodeOthers
  | Just rest <- T.stripPrefix ">" t                  = CodeAbove <$> parseNumber (T.strip rest)
  | [a, "to", b] <- T.words t                         = CodeBetween <$> parseNumber a <*> parseNumber b
  | otherwise = case pieces of
      [a]    -> CodeOne <$> parseNumber a
      [a, b] -> CodeBetween <$> parseNumber a <*> parseNumber b
      _      -> Nothing
  where
    t = T.strip (T.map (\c -> if c `elem` ['–', '—'] then '-' else c) cell)
    squeezed = T.filter (not . C.isSpace) t
    pieces = if ".." `T.isInfixOf` squeezed then DT.splitOn ".." squeezed else T.split (== '-') squeezed

-- | A number in hex (@0x07FF@, @0003h@) or in decimal (@10@).
parseNumber :: Text -> Maybe Number
parseNumber n
  | Just h <- T.stripPrefix "0x" lower, isHex h = Just (Number (hexValue h) (Just (T.length h)))
  | Just h <- T.stripSuffix "h" lower, isHex h  = Just (Number (hexValue h) (Just (T.length h)))
  | not (T.null n), T.all C.isDigit n           = Number <$> readMaybe (T.unpack n) <*> pure Nothing
  | otherwise                                   = Nothing
  where
    lower = T.toLower n
    isHex h = not (T.null h) && T.all C.isHexDigit h
    hexValue = T.foldl' (\acc c -> acc * 16 + toInteger (DC.digitToInt c)) 0

-- | The values a code cell assigns.
cellSpan :: CodeCell -> Span
cellSpan = \case
  CodeOne a       -> One (nValue a)
  CodeBetween a b -> Between (nValue a) (nValue b)
  CodeAbove a     -> Above (nValue a)
  CodeOthers      -> Others

-- | The 'Span' a code cell gives, as Haskell, its numbers as printed: @Between 0x0002 0x07ff@.
codeCellExpr :: CodeCell -> Text
codeCellExpr = \case
  CodeOne a       -> "One " <> numberExpr a
  CodeBetween a b -> "Between " <> numberExpr a <> " " <> numberExpr b
  CodeAbove a     -> "Above " <> numberExpr a
  CodeOthers      -> "Others"

-- | A number as Haskell, as printed: hex with its digits (@0x07ff@), or decimal.
numberExpr :: Number -> Text
numberExpr (Number v hex) = case hex of
  Just w  -> let digits = T.pack (showHex v "") in "0x" <> T.replicate (w - T.length digits) "0" <> digits
  Nothing -> tshow v

-- | The code column of a table: the first column every row of which holds a
--   code, one of them a value or a range (not only @All others@). The table's
--   rows are as wide as it is, but for a row of one cell in a wider table (its
--   notes), which is not read.
codeColumn :: TableIR -> Maybe Int
codeColumn t
  | null full || any (\r -> length r /= t.nCols && not (isNote r)) t.rows = Nothing
  | otherwise = L.find holdsCodes [0 .. t.nCols - 1]
  where
    full = filter ((== t.nCols) . length) t.rows
    isNote r = t.nCols > 1 && length r == 1
    holdsCodes i = let cells = map (parseCodeCell . cellText i) full
                   in all isJust cells && any (\c -> c /= Just CodeOthers) cells
    cellText :: Int -> [CellIR] -> Text
    cellText i r = maybe "" (.text) (listToMaybe (drop i r))


-- * Reading the bound tables

-- | One code table read under the errata set.
data CodeRead = CodeRead
  { crBound  :: !BoundTable
  , crTitle  :: !Text               -- ^ the caption's title after errata, normalised
  , crColumn :: !Int                -- ^ the code column
  , crHeader :: ![Text]             -- ^ the header cells after errata
  , crRows   :: ![CodeRowRead]
  }

-- | One row of a code table.
data CodeRowRead = CodeRowRead
  { rrIndex :: !Int                 -- ^ 0-based among the data rows
  , rrCode  :: !CodeCell
  , rrCells :: ![Text]              -- ^ the row's other cells, in column order
  }

-- | Every table bound to the rule's shape, read under the errata set; a table
--   with no code column is a finding.
codeReads :: Context -> [Either Finding CodeRead]
codeReads ctx = readCodes ctx (boundTables (Just Codes) ctx)

readCodes :: Context -> [BoundTable] -> [Either Finding CodeRead]
readCodes ctx = map readOne
  where
    readOne bt = case codeColumn t of
      Nothing -> Left (atTable ctx bt (finding Error "C-CODE-VALUE"
                   ("no column holds a code (a value, a range, >value or All others) in every row, or a row is"
                    <> " neither as wide as the table nor a single note cell")))
      Just i  -> Right CodeRead
        { crBound  = bt
        , crTitle  = fromMaybe "" (t.caption >>= captionTitle)
        , crColumn = i
        , crHeader = maybe [] (map (.text)) (listToMaybe t.headerRows)
        , crRows   = [ CodeRowRead n code (map (.text) (dropAt i r))
                     | (n, r) <- zip [0 ..] (filter ((== t.nCols) . length) t.rows)
                     , Just code <- [parseCodeCell (maybe "" (.text) (listToMaybe (drop i r)))] ] }
      where t = btRead bt
    dropAt i r = take i r <> drop (i + 1) r


-- * Checks

checkCodes :: Context -> CodeRead -> [Finding]
checkCodes ctx cr = map (atTable ctx (crBound cr)) (reversed <> others <> overlaps <> typeNamed)
  where
    rowLabel r = "row " <> tshow (rrIndex r + 1)
    reversed = [ finding Error "C-CODE-VALUE" (rowLabel r <> ": the range " <> codeCellExpr (rrCode r) <> " runs downwards")
               | r <- crRows cr, CodeBetween a b <- [rrCode r], nValue a > nValue b ]
    others = case [ r | r <- crRows cr, rrCode r == CodeOthers ] of
      _ : extra@(_ : _) -> [ finding Error "C-CODE-VALUE" (rowLabel r <> ": a second All others row") | r <- extra ]
      _                 -> []
    overlaps =
      [ finding Warning "W-CODE-OVERLAP" (rowLabel b <> " (" <> codeCellExpr (rrCode b) <> ") assigns values "
          <> rowLabel a <> " (" <> codeCellExpr (rrCode a) <> ") assigns too; the first is kept")
      | (a, rest) <- zip (crRows cr) (drop 1 (L.tails (crRows cr))), b <- rest
      , intersects (interval (rrCode a)) (interval (rrCode b)) ]
    interval = \case
      CodeOne a       -> Just (nValue a, Just (nValue a))
      CodeBetween a b -> Just (nValue a, Just (nValue b))
      CodeAbove a     -> Just (nValue a + 1, Nothing)
      CodeOthers      -> Nothing
    intersects (Just (lo1, hi1)) (Just (lo2, hi2)) = maybe True (max lo1 lo2 <=) (minHi hi1 hi2)
    intersects _ _                                 = False
    minHi (Just a) (Just b) = Just (min a b)
    minHi a b               = a <|> b
    -- C-CODE-TYPE: "Start Transaction Status Code (uinteger)" names a Type object
    types = map fst (typeEnvFrom ctx)
    typeNamed = case parenthesised =<< listToMaybe (drop (crColumn cr) (crHeader cr)) of
      Just ty | ty `notElem` types ->
        [ finding Error "C-CODE-TYPE" ("the code column's header gives its values the type " <> tshow ty
                                       <> ", which names no Type object of the spec") ]
      _ -> []
    parenthesised h = case DT.breakOnEnd "(" (T.strip h) of
      (before, inside) | not (T.null before), Just ty <- T.stripSuffix ")" inside -> Just (T.strip ty)
      _                                                                           -> Nothing


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = map one (rights (readCodes ctx bts))
  where
    one cr
      | any Cm.isError (checkCodes ctx cr) = Excluded (btAnchor (crBound cr)) "C-CODE-VALUE or C-CODE-TYPE failed"
      | otherwise = Emit (codeRowsDecl (provenance cr) ("c" <> titleIdent (crTitle cr)) (header cr) 0
                            [ (rrCode r, rrCells r) | r <- crRows cr ])
    provenance cr = let bt = crBound cr in provenanceOf ctx (btAnchor bt) (btTable bt) (normaliseCaption (fromMaybe "" (btRead bt).caption))
    header cr = case L.splitAt (crColumn cr) (crHeader cr) of
      (before, code : after) -> (code, before <> after)
      _                      -> ("", [])

-- | An identifier from a caption's title: its words, each capitalised, joined,
--   every character but a letter, a digit or an underscore dropped:
--   @ComID Assignments@ gives @ComIDAssignments@, @GET_COMID Command Block@
--   gives @GET_COMIDCommandBlock@.
titleIdent :: Text -> Text
titleIdent = T.concat . map capitalise . T.split (\c -> not (C.isAlphaNum c || c == '_'))
  where capitalise w = maybe w (\(c, cs) -> T.cons (C.toUpper c) cs) (T.uncons w)

-- | @\<name\> :: [CodeRow]@: one 'Hsed.Core.Code.CodeRow' per row, its span and
--   its other cells; the Haddock names the code column and the others.
codeRowsDecl :: Gen.Provenance -> Text -> (Text, [Text]) -> Int -> [(CodeCell, [Text])] -> Gen.Decl
codeRowsDecl p name (codeHeader, otherHeaders) rank rows = (Gen.decl p name body)
  { Gen.declRank    = rank
  , Gen.declImports = [Gen.ImportSome Gen.codeModule ["CodeRow (..)", "Span (..)"]] }
  where
    body =
      [ Gen.provenanceComment p <> ": by " <> T.unwords (T.words codeHeader)
          <> (if null otherHeaders then "" else "; " <> T.intercalate ", " (map (T.unwords . T.words) otherHeaders))
      , name <> " :: [CodeRow]" ]
      <> listLines (name <> " =") [ "CodeRow " <> atom (codeCellExpr code) <> " [" <> T.intercalate ", " (map tshow cells) <> "]" | (code, cells) <- rows ]
    atom e = if T.any C.isSpace e then "(" <> e <> ")" else e

-- | A list, one item to a line: @name =@, then @  [ a@, @  , b@, @  ]@; @name = []@ when empty.
listLines :: Text -> [Text] -> [Text]
listLines lhs = \case
  []    -> [lhs <> " []"]
  items -> lhs : zipWith (\i x -> (if i == (0 :: Int) then "  [ " else "  , ") <> x) [0 ..] items <> ["  ]"]
