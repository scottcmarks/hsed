{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.UidTable
Description : The uidTable rule: the assigned UIDs of section 6.3, each table's rows named where it names them
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 and example E6. @uidTable@ fires on a table whose first
header cell names a UID (@UID@, @UID of Row@, @UID in \<T\> Table@, @Method
UID@) and that is no type table, no table of table UIDs and no
preconfiguration ("Hsed.Spec.Shape"): in Core 2.01 the nine tables of section
6.3 besides Table 240, so the rule rides on the @doc@ anchor (design 13.24).
The first column is the UID; the other headers say what a row is:

* a header ending @Table Name@ names the table the row lies in (@Single Row
  Table Name@, @Table Name@);
* any other header ending @Name@ gives the row its name (@Method Name@,
  @Authority Name@, @Row Name@, @Template Name@);
* @Template@ gives the template the row belongs to (read by the ranges rule,
  "Hsed.Spec.Rules.Ranges");
* the others (@Purpose@, @Table@, @ColumnNumber@) describe the row.

A UID cell is eight bytes, or a pattern whose last bytes are @xx@ (@00 00 00
00 00 00 FF xx@, the UIDs of the Session Manager methods). The table a UID
lies in is the one whose half-UID is its upper half, among the tables the
@tableUIDs@ rule binds (Table 240, "Hsed.Spec.Rules.TableUIDs"); a header
@UID in \<T\> Table@, a caption @\<T\> Table UIDs@ or a @… Table Name@ cell
claims a table for it.

Checks, each with the code its findings carry:

* @C-UID-TABLE@: every UID cell is eight bytes or a pattern; every table a
  header, caption or cell claims is one Table 240 names, and the UID's upper
  half is that table's half-UID; a pattern whose row cites a table (@see
  Table 241@) covers every UID of that table;
* @C-UID-UNIQUE@: no two rows of one table print one UID;
* @C-UID-NAME@: no two rows give one generated name;
* @N-UID-UNNAMED@: the rows of a table that name no definition (a placeholder
  such as @Reserved for SSC Usage@ or @OBSOLETE *@, a pattern, or a table whose
  rows carry a purpose and no name), listed once per table.

Compiled into 'uidsModule': one @u…:: UID@ per named row, @u\<T\>_\<Name\>@
for a row in table @T@ (@uMethodID_Get@, @uAuthority_SID@, @uC_PIN_SID@,
@uLocking_GlobalRange@), @u\<T\>_Row@ for the row of a single-row table
(@uSPInfo_Row@), @u\<T\>_\<cells\>@ for a row named by its cells alone
(@uSecretProtect_C_PIN_0x03@), and @u\<Name\>@ for a UID that lies in no table
(the Session Manager methods, @uProperties@, as hsed's hand-written
@SpecialPurposeUIDs@ names them); and per table the list of its UIDs with
their rows' text (@methodIDUIDs :: [(UID, String)]@), named after its caption.
A name is a cell of one word, or of several capitalised words joined
(@Global Range@ gives @GlobalRange@).
-}
module Hsed.Spec.Rules.UidTable
  ( uidTableRule
  , uidsModule
    -- * Reading the bound tables
  , UidRow (..)
  , UidValue (..)
  , uidRows
  , uidHeaders
  , rowText
  , tablesByHalf
    -- * Names
  , asName
  , lowerIdent
  ) where

import qualified Data.Char        as DC
import qualified RIO.Char         as C
import qualified RIO.List         as L
import qualified RIO.Map          as Map
import qualified RIO.Text         as T
import           RIO

import           Hsed.Spec.Anchor (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import           Hsed.Spec.DSL    (SomeBinding (..), uidTable)
import qualified Hsed.Spec.Gen    as Gen
import           Hsed.Spec.IR     (CellIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Rules.TableUIDs (TableUIDRow (..), tableUIDRows)
import           Hsed.Spec.Shape  (Shape (..), captionTitle, normHeader, normaliseCaption, shapeMismatch)


-- | The rule.
uidTableRule :: RuleDef
uidTableRule = RuleDef
  { ruleName    = RuleName "uidTable"
  , bindsTo     = SomeBinding uidTable
  , exemplar    = (SpecId "core-2.01", TableId 242)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch UidTable form)
  , ruleIdent   = const Nothing
  , ruleModule  = uidsModule
  , ruleChecks  = checks
  , ruleCompile = compile
  }

-- | @Hsed.Gen.\<Spec\>.UIDs@.
uidsModule :: Gen.GenModule
uidsModule = Gen.GenModule
  { Gen.leaf        = "UIDs"
  , Gen.description = "Assigned UIDs: special purpose, method, authority and row UIDs"
  , Gen.notes       = [ "Each row the PDF names gives one UID, u\\<T\\>_\\<Name\\> for a row of table T (u\\<Name\\>"
                      , "for a UID in no table); each table gives the list of its UIDs with its rows' text." ] }


-- * Reading the bound tables

-- | A UID cell.
data UidValue
  = Exact ![Word8]          -- ^ eight bytes
  | Pattern ![Word8] !Int   -- ^ the fixed leading bytes and the number of @xx@ bytes after them
  | Unreadable
  deriving (Eq, Show)

-- | One row of a UID table, read under the errata set.
data UidRow = UidRow
  { urBound    :: !BoundTable
  , urRow      :: !Int               -- ^ 0-based among the data rows
  , urCells    :: ![CellIR]
  , urUIDText  :: !Text
  , urValue    :: !UidValue
  , urTable    :: !(Maybe Text)      -- ^ the table its UID lies in: the Table 240 table whose half-UID is its upper half
  , urClaimed  :: !(Maybe Text)      -- ^ the table a header, the caption or a cell says it lies in
  , urName     :: !(Maybe Text)      -- ^ the cell of its name column, as printed
  , urTemplate :: !(Maybe Text)      -- ^ the cell of its Template column, when not empty
  , urIdent    :: !(Maybe Text)      -- ^ the name it is generated under
  }

-- | What a column of a UID table holds, by its header.
data Role = UidCol | TableNameCol | NameCol | TemplateCol | OtherCol
  deriving (Eq, Show)

roles :: [Text] -> [Role]
roles hs = zipWith role [0 :: Int ..] hs
  where
    firstName = L.findIndex isName' (zip [0 :: Int ..] hs)
    isName' (i, h) = i > 0 && "name" `T.isSuffixOf` h && not ("tablename" `T.isSuffixOf` h)
    role i h
      | i == 0                         = UidCol
      | h == "template"                = TemplateCol
      | "tablename" `T.isSuffixOf` h   = TableNameCol
      | Just i == firstName            = NameCol
      | otherwise                      = OtherCol

-- | The header cells of a bound table as printed, and normalised.
uidHeaders :: BoundTable -> ([Text], [Text])
uidHeaders bt = (raw, map normHeader raw)
  where raw = maybe [] (map (\c -> T.unwords (T.words c.text))) (listToMaybe (btRead bt).headerRows)

-- | Every row of every table bound to the rule's shape.
uidRows :: Context -> [UidRow]
uidRows ctx = concatMap (tableRows ctx) (boundTables (Just UidTable) ctx)

-- | The Table 240 tables by half-UID: the tables bound by @tableUIDs@.
tablesByHalf :: Context -> Map [Word8] Text
tablesByHalf ctx = Map.fromList [ (take 4 t, tuName r) | Right r <- tableUIDRows ctx, Just t <- [tuTable r] ]

tableRows :: Context -> BoundTable -> [UidRow]
tableRows ctx bt = zipWith row [0 ..] (btRead bt).rows
  where
    (raw, hs) = uidHeaders bt
    rs        = roles hs
    halves    = tablesByHalf ctx
    hasName   = NameCol `elem` rs
    hasTable  = TableNameCol `elem` rs
    claimed   = headerClaim raw <|> captionClaim bt
    row i cells = UidRow
      { urBound = bt, urRow = i, urCells = cells, urUIDText = uidText, urValue = value
      , urTable = case value of
          Exact bs -> Map.lookup (take 4 bs) halves
          _        -> Nothing
      , urClaimed = cellOf TableNameCol <|> claimed
      , urName = name
      , urTemplate = mfilter (not . T.null) (cellOf TemplateCol)
      , urIdent = ident }
      where
        uidText = maybe "" (.text) (listToMaybe cells)
        value   = readUID uidText
        cellOf r = listToMaybe [ T.unwords (T.words c.text) | (r', c) <- zip rs cells, r' == r ]
        name    = cellOf NameCol
        others  = [ T.unwords (T.words c.text) | (OtherCol, c) <- zip rs cells ]
        piece
          | hasName   = name >>= asName
          | hasTable  = Just "Row"
          | not (null others) = T.intercalate "_" <$> traverse asName others
          | otherwise = Nothing
        ident = case (value, piece) of
          (Exact bs, Just l) -> Just ("u" <> maybe l (<> ("_" <> l)) (Map.lookup (take 4 bs) halves))
          _                  -> Nothing

-- | @UID in MethodID Table@ claims MethodID.
headerClaim :: [Text] -> Maybe Text
headerClaim raw = case T.words <$> listToMaybe raw of
  Just ("UID" : "in" : rest@(_ : _)) | Just ws <- L.stripSuffix ["Table"] rest, not (null ws) -> Just (T.unwords ws)
  _ -> Nothing

-- | @Template Table UIDs@ claims Template.
captionClaim :: BoundTable -> Maybe Text
captionClaim bt = do
  title <- (btRead bt).caption >>= captionTitle
  t <- T.stripSuffix " Table UIDs" title
  guard (not (T.null t) && T.all (not . C.isSpace) t)
  pure t

-- | A UID cell: sixteen hex digits, or a pattern ending in @xx@ bytes.
readUID :: Text -> UidValue
readUID cell
  | Just bs <- parseUIDCell cell = Exact bs
  | length ws == 8, (fixed, wild@(_ : _)) <- L.span (/= "xx") (map T.toLower ws), all (== "xx") wild
  , Just bs <- traverse hexByte fixed = Pattern bs (length wild)
  | otherwise = Unreadable
  where
    ws = T.words cell
    hexByte w = case T.unpack w of
      [a, b] | C.isHexDigit a, C.isHexDigit b -> Just (fromIntegral (DC.digitToInt a * 16 + DC.digitToInt b))
      _                                       -> Nothing

-- | A cell that is a name: one word, or several capitalised words, each of
--   letters, digits and underscores; joined into one identifier piece.
asName :: Text -> Maybe Text
asName cell = case T.words cell of
  [w] | wordOk w -> Just w
  ws@(_ : _ : _) | all wordOk ws, all capital ws -> Just (T.concat ws)
  _ -> Nothing
  where
    wordOk w = not (T.null w) && T.all (\c -> C.isAlphaNum c || c == '_') w
    capital w = maybe False (\(c, _) -> C.isUpper c || C.isDigit c) (T.uncons w)

-- | An identifier from a title: its words joined, the leading capitals lowered
--   (the last kept when a lower-case letter follows): @SPTemplates Table UIDs@
--   gives @spTemplatesTableUIDs@, @HMACInit@ gives @hmacInit@.
lowerIdent :: Text -> Text
lowerIdent title = lowered <> rest
  where
    joined = T.concat [ capitalise (T.filter (\c -> C.isAlphaNum c || c == '_') w) | w <- T.words title ]
    capitalise w = maybe w (\(c, cs) -> T.cons (C.toUpper c) cs) (T.uncons w)
    (caps, after) = T.span C.isUpper joined
    (lowered, rest)
      | T.length caps > 1, maybe False (C.isLower . fst) (T.uncons after) = (T.toLower (T.dropEnd 1 caps), T.takeEnd 1 caps <> after)
      | otherwise = (T.toLower caps, after)

-- | A row's cells after its UID, the empty ones left out, as one line: @Get | Base@.
rowText :: UidRow -> Text
rowText r = T.intercalate " | " (filter (not . T.null) [ T.unwords (T.words c.text) | c <- drop 1 (urCells r) ])


-- * Checks

checks :: Context -> [BoundTable] -> [Finding]
checks ctx bts = concatMap tableCheck (L.groupBy sameTable rows) <> patternChecks <> nameChecks <> noTables
  where
    rows = concatMap (tableRows ctx) bts
    halves = tablesByHalf ctx
    named = Map.fromList [ (n, h) | (h, n) <- Map.toList halves ]
    sameTable a b = btAnchor (urBound a) == btAnchor (urBound b)
    at r = atTable ctx (urBound r)
    noTables = [ at r (finding Error "C-UID-TABLE" "no table bound by tableUIDs (T240's header) to resolve the tables the UID tables name against")
               | Map.null halves, r : _ <- [filter (isJust . urClaimed) rows] ]

    tableCheck trs = concatMap rowCheck trs <> uniqueness trs <> unnamed trs
    rowCheck r = map (at r . finding Error "C-UID-TABLE" . (label r <>)) $ case (urValue r, urClaimed r) of
      (Unreadable, _) -> ["the UID cell " <> tshow (urUIDText r) <> " is neither eight bytes nor a pattern ending xx"]
      (Exact bs, Just t) | not (Map.null halves) -> case Map.lookup t named of
        Nothing -> ["it lies in the " <> t <> " table, which no row of Table 240 names"]
        Just h | h /= take 4 bs -> ["the upper half " <> hexUID (take 4 bs) <> " is not " <> hexUID h <> ", the half-UID of the "
                                     <> t <> " table it lies in" <> maybe "" (\o -> " (it is " <> o <> "'s)") (urTable r)]
        _ -> []
      _ -> []
    uniqueness trs =
      [ at r (finding Error "C-UID-UNIQUE" (label r <> "the UID " <> hexUID bs <> " is also that of row " <> tshow (urRow r0 + 1)))
      | (bs, r0 : rest) <- Map.toList (Map.fromListWith (flip (<>)) [ (bs, [r]) | r <- trs, Exact bs <- [urValue r] ])
      , r <- rest ]
    unnamed trs = case [ r | r <- trs, isNothing (urIdent r) ] of
      [] -> []
      rs@(r : _) -> [ at r (finding Note "N-UID-UNNAMED"
                       ("rows that name no definition (listed in " <> listName (urBound r) <> " where their UID is eight bytes): "
                         <> T.intercalate "; " [ tshow (urRow x + 1) <> " \"" <> rowText x <> "\"" | x <- rs ])) ]

    -- a pattern whose row cites a table covers that table's UIDs
    patternChecks =
      [ at r (finding Error "C-UID-TABLE"
          ("row " <> tshow (urRow r + 1) <> " of " <> renderAnchorId (btAnchor (urBound r)) <> ": the UID " <> hexUID bs
            <> " does not match the pattern " <> urUIDText p <> " that " <> renderAnchorId (btAnchor (urBound p))
            <> " row " <> tshow (urRow p + 1) <> " gives for the UIDs of Table " <> tshow n))
      | p <- rows, Pattern fixed _ <- [urValue p], n <- citedTables p
      , r <- rows, btAnchor (urBound r) == TableId n, Exact bs <- [urValue r], take (length fixed) bs /= fixed ]

    nameChecks =
      [ at r (finding Error "C-UID-NAME" (label r <> "it is generated as " <> i <> ", as " <> renderAnchorId (btAnchor (urBound r0))
                                            <> " row " <> tshow (urRow r0 + 1) <> " is"))
      | (i, r0 : rest) <- Map.toList (Map.fromListWith (flip (<>)) [ (i, [r]) | r <- rows, Just i <- [urIdent r] ])
      , r <- rest ]

-- | The tables a row's text cites, @see Table 241@.
citedTables :: UidRow -> [Int]
citedTables r = [ n | (w, next) <- zip ws (drop 1 ws), w == "Table", Just n <- [readMaybe (T.unpack (T.filter C.isDigit next))]
                    , T.any C.isDigit next ]
  where ws = T.words (T.map (\c -> if C.isAlphaNum c then c else ' ') (rowText r))

label :: UidRow -> Text
label r = "row " <> tshow (urRow r + 1) <> " \"" <> rowText r <> "\": "


-- * Declarations

-- | The name of a table's list: its caption's title as an identifier, @methodIDUIDs@.
listName :: BoundTable -> Text
listName bt = lowerIdent (fromMaybe (renderAnchorId (btAnchor bt)) ((btRead bt).caption >>= captionTitle))

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = concatMap table bts
  where
    rows  = concatMap (tableRows ctx) bts
    -- a name two rows give (C-UID-NAME) defines neither
    clash = Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [ (i, 1) | r <- rows, Just i <- [urIdent r] ]))
    table bt = case [ r | r <- rows, btAnchor (urBound r) == btAnchor bt ] of
      [] -> []
      trs@(r0 : _) ->
        [ if i `elem` clash
            then Excluded (btAnchor bt) (label r <> "C-UID-NAME failed")
            else Emit (rowDecl ctx r i bs)
        | r <- trs, Just i <- [urIdent r], Exact bs <- [urValue r] ]
        <> [ Emit (listDecl (provenance ctx r0) (length trs) (listName bt) trs) | any (isExact . urValue) trs ]
    isExact = \case
      Exact _ -> True
      _       -> False

provenance :: Context -> UidRow -> Gen.Provenance
provenance ctx r = let bt = urBound r in provenanceOf ctx (btAnchor bt) (btTable bt) (normaliseCaption (fromMaybe "" (btRead bt).caption))

-- | One named row's UID, citing the page it is printed on.
rowDecl :: Context -> UidRow -> Text -> [Word8] -> Gen.Decl
rowDecl ctx r n bs = (Gen.decl p n body)
  { Gen.declRank    = urRow r
  , Gen.declImports = [Gen.ImportSome Gen.uidModule ["UID", "uid"]] }
  where
    p    = atRow ctx (urCells r) (provenance ctx r)
    body = [ Gen.provenanceComment p <> ", row " <> tshow (urRow r + 1) <> ": " <> rowText r
           , n <> " :: UID", n <> " = " <> Gen.uidExpr bs ]

-- | A table's UIDs with its rows' text, after its rows.
listDecl :: Gen.Provenance -> Int -> Text -> [UidRow] -> Gen.Decl
listDecl p rank n rows = (Gen.decl p n body)
  { Gen.declRank    = rank
  , Gen.declImports = [Gen.ImportSome "Prelude" ["String"], Gen.ImportSome Gen.uidModule ["UID", "uid"]] }
  where
    entries = [ "(" <> Gen.uidExpr bs <> ", " <> tshow (rowText r) <> ")" | r <- rows, Exact bs <- [urValue r] ]
    body = [ Gen.provenanceComment p <> ": every UID the table prints, with its row's other cells"
           , n <> " :: [(UID, String)]", n <> " =" ]
           <> zipWith (\i e -> (if i == (0 :: Int) then "  [ " else "  , ") <> e) [0 ..] entries <> ["  ]"]
