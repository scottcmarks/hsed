{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.StatusCodes
Description : The statusCodes rule: the method status codes, a type with a pattern per value
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4. @statusCodes@ fires on a table headed @[Name, Value]@
(the @StatusCodes@ shape) every row of which gives one value (the rule's own
condition): in Core 2.01 that is Table 166 alone (section 5.1.5, Method Status
Codes), so the rule rides on the @doc@ anchor (design 13.24).

Checks:

* @C-STATUS-CODE@ (Error): every row is a name and one value; every name can
  name a pattern (an identifier starting with a capital letter); no value is
  given twice; no two patterns get one name;
* @N-STATUS-NAME-SUFFIXED@ (Note): a name the table gives several values
  (@OBSOLETE@: 0x02, 0x0D, 0x0E) names each of them with the value as a
  suffix, in decimal, the first included (@OBSOLETE_2@, @OBSOLETE_13@,
  @OBSOLETE_14@), as the enumerations' constructors do
  ("Hsed.Spec.Rules.EnumValues", design section 13.15); the Note says the
  suffix is decimal, its values being printed in hex;
* the subsections of the table's section are the statuses described one by
  one (Core 5.1.5.1 SUCCESS to 5.1.5.16 FAIL): a subsection titled with an
  identifier that names no status of the table is @W-STATUS-SECTION@
  (Warning), and a status no subsection describes is @N-STATUS-UNDESCRIBED@
  (Note: OBSOLETE, the retired values, in Core).

Compiled into the codes module ("Hsed.Spec.Rules.Codes".'codesModule'): a
@newtype@ over 'Numeric.Natural.Natural' named after the caption (Table 166
Status Codes gives @StatusCode@), its field @statusValue@ the value it
carries, which is any status the wire does (the codec's @MethodResponse@
status is a bare uinteger, and SIIS adds values Core does not, design section
8); a bidirectional pattern per row, named as the
table names the value (@SUCCESS@, @NOT_AUTHORIZED@); and the table as
@c\<Title\> :: [CodeRow]@ like every code table, for a value's name.
-}
module Hsed.Spec.Rules.StatusCodes
  ( statusCodesRule
    -- * Reading the bound tables
  , StatusRead (..)
  , StatusRow (..)
  , statusReads
  , statusTypeName
  , patternNames
  ) where

import qualified RIO.Char             as C
import qualified RIO.List             as L
import qualified RIO.Map              as Map
import qualified RIO.Text             as T
import           RIO

import           Hsed.Spec.Anchor     (AnchorId (..))
import           Hsed.Spec.Common     (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Common     as Cm
import           Hsed.Spec.DSL        (SomeBinding (..), statusCodes)
import qualified Hsed.Spec.Gen        as Gen
import           Hsed.Spec.IR         (CellIR (..), HeadingIR (..), SpecIR (..), TableIR (..))
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Readers.Text (titleIdent)
import           Hsed.Spec.Rules.Codes (CodeCell (..), Number (..), codeRowsDecl, codesModule, numberExpr,
                                        parseCodeCell)
import           Hsed.Spec.Shape      (Shape (..), captionTitle, shapeMismatch)


-- | The rule.
statusCodesRule :: RuleDef
statusCodesRule = RuleDef
  { ruleName    = RuleName "statusCodes"
  , bindsTo     = SomeBinding statusCodes
  , exemplar    = (SpecId "core-2.01", TableId 166)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ t form -> isNothing (shapeMismatch StatusCodes form) && all oneValue t.rows
  , ruleIdent   = const Nothing
  , ruleModule  = codesModule
  , ruleCodes   =
      [ CodeDef "C-STATUS-CODE"          [Error]          (Defect "a status row is not one name and one value, or gives a value twice")
      , CodeDef "N-STATUS-NAME-SUFFIXED" [Note]           (AsPrinted "a status name given to several values, each suffixed with its value")
      , CodeDef "W-STATUS-SECTION"       [Warning]        (Suspect "a subsection of the status table's section names no status of it")
      , CodeDef "N-STATUS-UNDESCRIBED"   [Note]           (Suspect "a status no subsection of its section describes") ]
  , ruleChecks  = \ctx bts -> concatMap (checkStatus ctx) (readStatus bts)
  , ruleCompile = compile
  }
  where
    oneValue = \case
      [_, value] -> isOne (parseCodeCell value.text)
      _          -> False
    isOne = \case
      Just (CodeOne _) -> True
      _                -> False


-- * Reading the bound tables

-- | One table of status codes read under the errata set.
data StatusRead = StatusRead
  { srBound :: !BoundTable
  , srTitle :: !Text            -- ^ the caption's title after errata, normalised
  , srRows  :: ![StatusRow]
  }

-- | One row: a name and its value.
data StatusRow = StatusRow
  { stIndex :: !Int             -- ^ 0-based among the data rows
  , stName  :: !Text
  , stCell  :: !Text            -- ^ the value cell
  , stValue :: !(Maybe Number)  -- ^ when the cell is one value
  , stWidth :: !Int             -- ^ the row's cells
  , stRow   :: ![CellIR]        -- ^ the row as printed, which locates a finding about it
  }

-- | Every table bound to the rule's shape, read under the errata set.
statusReads :: Context -> [StatusRead]
statusReads ctx = readStatus (boundTables (Just StatusCodes) ctx)

readStatus :: [BoundTable] -> [StatusRead]
readStatus = map readOne
  where
    readOne bt = StatusRead
      { srBound = bt
      , srTitle = fromMaybe "" ((btRead bt).caption >>= captionTitle)
      , srRows  = zipWith row [0 ..] (btRead bt).rows }
    row :: Int -> [CellIR] -> StatusRow
    row i cells = StatusRow
      { stIndex = i
      , stName  = maybe "" (T.strip . (.text)) (listToMaybe cells)
      , stCell  = maybe "" (.text) (listToMaybe (drop 1 cells))
      , stValue = case parseCodeCell (maybe "" (.text) (listToMaybe (drop 1 cells))) of
          Just (CodeOne n) -> Just n
          _                -> Nothing
      , stWidth = length cells
      , stRow   = cells }

-- | The type of a table's statuses: the caption's title as an identifier, its
--   last word made singular by dropping an @s@ that does not end @ss@ or @us@
--   (@Status Codes@ gives @StatusCode@).
statusTypeName :: Text -> Text
statusTypeName title = titleIdent (T.unwords (initWords <> map singular lastWord))
  where
    (initWords, lastWord) = L.splitAt (length (T.words title) - 1) (T.words title)
    singular w
      | any (`T.isSuffixOf` w) ["ss", "us"] = w
      | otherwise = fromMaybe w (T.stripSuffix "s" w >>= \w' -> if T.null w' then Nothing else Just w')

-- | Each row's pattern name: the row's name, and a name given to several
--   values each suffixed with its value in decimal, the first included
--   (@OBSOLETE_2@, @OBSOLETE_13@, @OBSOLETE_14@), as the enumerations'
--   constructors are (design section 13.15).
patternNames :: [StatusRow] -> [(StatusRow, Text)]
patternNames rows = [ (r, name r) | r <- rows ]
  where
    given = Map.fromListWith (+) [ (stName r, 1 :: Int) | r <- rows ]
    name r
      | Map.lookup (stName r) given == Just 1 = stName r
      | otherwise = stName r <> "_" <> maybe (tshow (stIndex r + 1)) (tshow . nValue) (stValue r)


-- * Checks

checkStatus :: Context -> StatusRead -> [Finding]
checkStatus ctx sr = shapeOfRows <> names <> values <> twice <> map (atTable ctx (srBound sr)) (clashes <> suffixed)
                     <> sections ctx sr
  where
    rows = srRows sr
    rowLabel r = "row " <> tshow (stIndex r + 1)
    bad = finding Error "C-STATUS-CODE"
    at r = atCells ctx (srBound sr) (stRow r)
    shapeOfRows = [ at r (bad (rowLabel r <> " has " <> tshow (stWidth r) <> " cells, not a name and a value")) | r <- rows, stWidth r /= 2 ]
    names = [ at r (bad (rowLabel r <> ": the name " <> tshow (stName r) <> " cannot name a pattern (an identifier starting with a capital letter)"))
            | r <- rows, stWidth r == 2, not (isPatternName (stName r)) ]
    values = [ at r (bad (rowLabel r <> " " <> stName r <> ": the value " <> tshow (stCell r) <> " is not one value"))
             | r <- rows, stWidth r == 2, isNothing (stValue r) ]
    twice = concat [ [ at r (bad (rowLabel r <> " " <> stName r <> ": the value " <> numberExpr' r <> " is also that of "
                                  <> rowLabel f <> " " <> stName f)) | r <- rest ]
                   | f : rest <- Map.elems (Map.fromListWith (flip (<>)) [ (nValue v, [r]) | r <- rows, Just v <- [stValue r] ]) ]
    clashes = [ bad ("two rows get the pattern name " <> n) | (n, k) <- Map.toList (Map.fromListWith (+) [ (n, 1 :: Int) | (_, n) <- patternNames rows ]), k > 1 ]
    suffixed =
      [ finding Note "N-STATUS-NAME-SUFFIXED"
          ("the name " <> name <> " is given to " <> T.intercalate ", " (map numberExpr' rs) <> ": the patterns are "
            <> T.intercalate ", " [ n | (r, n) <- patternNames rows, stIndex r `elem` map stIndex rs ] <> ", each value's suffix in decimal")
      | (name, rs) <- Map.toList (Map.fromListWith (flip (<>)) [ (stName r, [r]) | r <- rows ]), length rs > 1 ]
    numberExpr' r = maybe (tshow (stCell r)) numberExpr (stValue r)

isPatternName :: Text -> Bool
isPatternName n = case T.uncons n of
  Just (c, cs) -> C.isUpper c && T.all (\x -> C.isAlphaNum x || x == '_' || x == '\'') cs
  Nothing      -> False

-- | The subsections of the table's section against its statuses.
sections :: Context -> StatusRead -> [Finding]
sections ctx sr = case sectionOf ctx (srBound sr) of
  Nothing  -> []
  Just sec ->
    let subs = [ h | h <- (cxIR ctx).headings, Just s <- [h.section], Just n <- [T.stripPrefix (sec <> ".") s]
                   , not (T.null n), T.all C.isDigit n, isPatternName (T.strip h.title) ]
        statuses = L.nub [ stName r | r <- srRows sr ]
        titles = map (T.strip . (.title)) subs
    in [ locatedAt (cxSpec ctx) (fromMaybe "" h.section) (h.page + 1)
           (finding Warning "W-STATUS-SECTION" ("section " <> fromMaybe "" h.section <> " " <> T.strip h.title
              <> " describes a status " <> renderTable <> " does not give"))
       | h <- subs, T.strip h.title `notElem` statuses ]
       <> [ atTable ctx (srBound sr) (finding Note "N-STATUS-UNDESCRIBED"
              ("no subsection of " <> sec <> " describes " <> n <> " (" <> T.intercalate ", " (valuesOf n) <> ")"))
          | n <- statuses, n `notElem` titles ]
  where
    renderTable = "Table " <> maybe "?" tshow (btTable (srBound sr)).tableNumber
    valuesOf n = [ maybe (tshow (stCell r)) numberExpr (stValue r) | r <- srRows sr, stName r == n ]


-- * Declarations

compile :: Context -> [BoundTable] -> [Emitted]
compile ctx bts = map one (readStatus bts)
  where
    one sr
      | any Cm.isError (checkStatus ctx sr) = Excluded (btAnchor (srBound sr)) "C-STATUS-CODE failed"
      | otherwise = Emit (statusDecl ctx sr)

-- | The type, a pattern per row and the table's rows, as one declaration.
statusDecl :: Context -> StatusRead -> Gen.Decl
statusDecl ctx sr = (Gen.decl p typeName body)
  { Gen.declImports    = [Gen.ImportSome "Numeric.Natural" ["Natural"], Gen.ImportSome "Prelude" ["Eq", "Ord", "Show"]]
                         <> rowsDecl.declImports
  , Gen.declExtensions = ["PatternSynonyms"] }
  where
    bt = srBound sr
    p = boundProvenance ctx bt
    typeName = statusTypeName (srTitle sr)
    doc = Gen.provenanceComment p
    rowsDecl = codeRowsDecl p ("c" <> titleIdent (srTitle sr)) (header 1, [header 0]) 0
                 [ (CodeOne v, [stName r]) | r <- srRows sr, Just v <- [stValue r] ]
    header i = maybe "" (.text) (listToMaybe (drop i (concat (take 1 (btRead bt).headerRows))))
    body =
      [ doc <> ": any status the wire carries; a pattern names each value the table gives"
      , "newtype " <> typeName <> " = " <> typeName <> " { statusValue :: Natural }"
      , "  deriving (Eq, Ord, Show)" ]
      <> concat [ [ "", doc <> ", row " <> tshow (stIndex r + 1) <> ": " <> stName r
                  , "pattern " <> n <> " :: " <> typeName
                  , "pattern " <> n <> " = " <> typeName <> " " <> numberExpr v ]
                | (r, n) <- patternNames (srRows sr), Just v <- [stValue r] ]
      <> ("" : rowsDecl.declBody)
