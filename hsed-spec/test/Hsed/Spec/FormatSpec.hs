{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.FormatSpec
Description : Test specifications for Hsed.Spec.Format
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The corpus is @dev/Formats.txt@ (the 82 distinct Format strings as hsed's hand
transcription has them) and @test/fixtures/core-2.01-type-rows.txt@ (the 100
Format cells as the PDF prints them, Table 75's @Fractoin@ included), resolved
against @dev/Types.txt@ and the T240 table names.
-}
module Hsed.Spec.FormatSpec (spec) where

import           Data.Char        (isSpace)
import qualified Data.Text        as T
import           RIO
import           RIO.List         (find, sort)
import           Test.Hspec

import           Hsed.Core.Types
import           Hsed.Spec.Common hiding (spec)
import           Hsed.Spec.Errata
import           Hsed.Spec.CoreIR   (withCore)
import           Hsed.Spec.Fixtures
import           Hsed.Spec.Format
import           Hsed.Spec.IR       (CellIR (..), TableIR (..))
import qualified Hsed.Spec.IR       as IR
import           Hsed.Spec.Shape    (normHeader)


-- | A row's Format cell under an errata set: under scott, corrected as Table 75's
--   annotation corrects it (spec/annot/core-2.01.hsa), the one Format cell with an erratum.
cell :: ErrataSet -> Row -> Text
cell es r = fst (applyErrata corrections (Site core ("T" <> tshow (rowTable r)) (DataCell 0 2 "Format") Nothing) (rowFormat r))
  where corrections = [ Erratum core "T75" "Fractoin" "Fraction" "annotation" | es == Scott ]

-- | The sixteen codes, one example each, over a small environment.
smallEnv :: Env
smallEnv = mkEnv [ (n, Nothing) | n <- ["bytes", "uinteger", "integer_2", "bytes_16", "bytes_32", "ACE_expression", "name", "fraction_enum", "Seconds", "Fraction"] ]
                 ["MBR", "Log", "LogList", "C_AES_128", "C_AES_256"]

examples :: [(Text, TypeDef)]
examples =
  [ ("Base_Type", BaseType)
  , ("Simple_Type, uinteger, 2", SimpleType (Ref "uinteger") (Lit 2))
  , ("Enumeration_Type, 0, 7", EnumType [(Lit 0, Lit 7)])
  , ("Alternative_Type, bytes_16, bytes_32", AltType [Ref "bytes_16", Ref "bytes_32"])
  , ("List_Type, *, ACE_expression", ListType Star (Ref "ACE_expression"))
  , ("Restricted_Reference_Type{5}, ref{MBRTableUID}", RestrictedRef ByteTableRow [Table "MBR"])
  , ("Restricted_Reference_Type{6}, uidref{LogTableUID}, *", RestrictedRef ObjectTableUID [Table "Log", TableStar])
  , ("General_Reference_Type{7}", GeneralRef AnyByteTableRow)
  , ("General_Reference_Type{8}", GeneralRef AnyObjectUID)
  , ("General_Reference_Type{9}", GeneralRef AnyTableUID)
  , ("General_Reference_Table_Type, 1", GeneralRefTable ObjectTable)
  , ("Named_Value_Name_Type, Key, name", NamedValueName "Key" (Ref "name"))
  , ("Name_Value_Integer_Type, -1, integer_2", NamedValueInteger (-1) (Ref "integer_2"))
  , ("Name_Value_Uinteger_Type, 6, fraction_enum", NamedValueUinteger 6 (Ref "fraction_enum"))
  , ("Struct_Type, Seconds, Fraction", StructType [Ref "Seconds", Ref "Fraction"])
  , ("Set_Type, 0, *, 8, 15", SetType [(Lit 0, Star), (Lit 8, Lit 15)])
  ]

codeOf :: Either Finding a -> Text
codeOf = either code (const "ok")

spec :: Spec
spec = do
  rows   <- runIO readRows
  tables <- runIO (readLines "test/fixtures/core-2.01-table-names.txt")
  corpus <- runIO (readLines "../dev/Formats.txt")
  names  <- runIO (readLines "../dev/Types.txt")
  let env = mkEnv [ (rowName r, Just (rowTable r)) | r <- rows ] tables

  describe "the fixtures" $ do
    it "hold the 100 published rows, 46 table names, 82 corpus strings and 100 type names" $
      (length rows, length tables, length corpus, length names) `shouldBe` (100, 46, 82, 100)
    it "name the same Type objects as dev/Types.txt" $
      sort (map rowName rows) `shouldBe` sort names

  describe "tokenize" $ do
    it "ignores the spacing the printed tables vary" $ do
      tokenize "uidref {C_PINTableUID}" `shouldBe` tokenize "uidref{C_PINTableUID}"
      tokenize "General_Reference_Type {7}" `shouldBe` tokenize "General_Reference_Type{7}"
      tokenize "Simple_Type,uinteger, 2" `shouldBe` tokenize "Simple_Type, uinteger, 2"
      tokenize "List_Type,  *,  ACE_expression" `shouldBe` Right [TName "List_Type", TComma, TStar, TComma, TName "ACE_expression"]
    it "lexes braces, numbers and names" $
      tokenize "Restricted_Reference_Type{6}, uidref { C_AES_*TableUID }, 12"
        `shouldBe` Right [TName "Restricted_Reference_Type", TBraced "6", TComma, TName "uidref", TBraced "C_AES_*TableUID", TComma, TNum 12]
    it "rejects what no Format cell contains" $ do
      codeOf (tokenize "List_Type, *, {ACE") `shouldBe` "E-FMT-SYNTAX"
      codeOf (tokenize "List_Type; *") `shouldBe` "E-FMT-SYNTAX"

  describe "parseFormat" $ do
    it "builds every one of the sixteen Format codes" $
      map (parseFormat smallEnv . fst) examples `shouldBe` map (Right . snd) examples
    it "reads the suffix forms and the table-name spellings of the corpus" $ do
      parseFormat env "Restricted_Reference_Type {6}, uidref{LogTableUID}, *"
        `shouldBe` Right (RestrictedRef ObjectTableUID [Table "Log", TableStar])
      parseFormat env "Restricted_Reference_Type{6}, uidref {ColumnTable_UID}"
        `shouldBe` Right (RestrictedRef ObjectTableUID [Table "Column"])
      parseFormat env "Restricted_Reference_Type{6}, uidref {C_PINTableUID}, uidref {C_AES_*TableUID}, uidref{C_HMAC_*TableUID}"
        `shouldBe` Right (RestrictedRef ObjectTableUID [Table "C_PIN", TableGlob "C_AES_*", TableGlob "C_HMAC_*"])
    it "names the nearest known type on an unresolved name" $
      parseFormat env "Struct_Type, Year, Fractoin"
        `shouldBe` Left (finding Error "E-FMT-RESOLVE" "unresolved type name Fractoin (nearest: Fraction, T84)")
    it "names the nearest known table on an unresolved uidref" $ do
      parseFormat env "Restricted_Reference_Type{6}, uidref{LogsTableUID}"
        `shouldBe` Left (finding Error "E-FMT-RESOLVE" "unresolved table name Logs (nearest: Log)")
      codeOf (parseFormat env "Restricted_Reference_Type{6}, uidref{Z_*TableUID}") `shouldBe` "E-FMT-RESOLVE"
    it "reports an unknown Format code with its nearest spelling" $
      parseFormat env "Struct_Typo, Year"
        `shouldBe` Left (finding Error "E-FMT-CODE" "unknown Format code Struct_Typo (nearest: Struct_Type)")
    it "reports the wrong number or kind of values" $ do
      map (codeOf . parseFormat env)
        [ "Simple_Type, bytes", "Enumeration_Type, 0", "Alternative_Type, bytes_16", "General_Reference_Type{7}, 1"
        , "Restricted_Reference_Type{6}", "General_Reference_Table_Type, 3", "Set_Type", "Struct_Type", "Base_Type, 1", "List_Type, name, *" ]
        `shouldBe` replicate 10 "E-FMT-ARITY"
      map (codeOf . parseFormat env) [ "Simple_Type, bytes, bytes", "Struct_Type, *", "Struct_Type, Year," ]
        `shouldBe` replicate 3 "E-FMT-SYNTAX"
    it "accepts both spellings of the Named value codes" $
      parseFormat env "Named_Value_Uinteger_Type, 6, fraction_enum" `shouldBe` parseFormat env "Name_Value_Uinteger_Type, 6, fraction_enum"

  describe "locate" $
    it "cites the page and table as design example E3 does" $
      case find ((== 75) . rowTable) rows of
        Nothing  -> expectationFailure "no T75 in the fixture"
        Just t75 -> either
          (\f -> renderFinding (locate core "T75" (Just 107) f)
                   `shouldBe` "E-FMT-RESOLVE core-2.01.pdf#page=107&T75: unresolved type name Fractoin (nearest: Fraction, T84)")
          (const (expectationFailure "Fractoin resolved under the published set"))
          (parseFormat env (cell Published t75))

  describe "the corpus dev/Formats.txt" $ do
    it "parses entirely under the scott errata set" $
      [ (s, f) | s <- corpus, Left f <- [parseFormat env s] ] `shouldBe` []
    it "fails under the published set exactly on the strings that contain Fractoin" $
      [ s | s <- corpus, isLeft (parseFormat env s) ] `shouldBe` filter (T.isInfixOf "Fractoin") corpus

  describe "the published Format cells" $ do
    it "parse entirely under the scott errata set" $
      [ (rowTable r, f) | r <- rows, Left f <- [parseFormat env (cell Scott r)] ] `shouldBe` []
    it "fail under the published set exactly on the cells that contain Fractoin, with E-FMT-RESOLVE" $ do
      let failures = [ (rowTable r, f) | r <- rows, Left f <- [parseFormat env (cell Published r)] ]
      map fst failures `shouldBe` [ rowTable r | r <- rows, "Fractoin" `T.isInfixOf` rowFormat r ]
      map (code . snd) failures `shouldBe` ["E-FMT-RESOLVE"]
    it "parse to the Format code the cell names" $
      [ (rowTable r, formatName d, spelledCode r) | r <- rows, Right d <- [parseFormat env (cell Scott r)], formatName d /= spelledCode r ]
        `shouldBe` []
    it "refer only to Type objects of the corpus" $
      [ (rowTable r, x) | r <- rows, Right d <- [parseFormat env (cell Scott r)], Ref x <- references d, x `notElem` names ]
        `shouldBe` []

  describe "nearest" $ do
    it "picks the fewest edits, then the alphabetically first" $ do
      nearest ["Fracture", "Fraction", "Year"] "Fractoin" `shouldBe` Just "Fraction"
      nearest ["b", "a"] "c" `shouldBe` Just "a"
      nearest [] "x" `shouldBe` Nothing

  describe "globMatch" $
    it "lets * stand for any run of characters" $ do
      globMatch "C_AES_*" "C_AES_128" `shouldBe` True
      globMatch "C_AES_*" "C_RSA_1024" `shouldBe` False
      globMatch "C_*_1*" "C_RSA_1024" `shouldBe` True
      globMatch "*" "" `shouldBe` True
      globMatch "Log" "LogList" `shouldBe` False

  describe "formatNormalisations" $ do
    it "lists a name spaced from its braces and T76's Table_UID, each as printed and as read" $ do
      formatNormalisations "Restricted_Reference_Type{6}, uidref {ColumnTable_UID}"
        `shouldBe` [("uidref {ColumnTable_UID}", "uidref{ColumnTable_UID}"), ("ColumnTable_UID", "ColumnTableUID")]
      formatNormalisations "General_Reference_Type {7}" `shouldBe` [("General_Reference_Type {7}", "General_Reference_Type{7}")]
      formatNormalisations "Restricted_Reference_Type{6}, uidref{LogTableUID}, *" `shouldBe` []
      formatNormalisations "Simple_Type, uinteger, 2" `shouldBe` []
    it "lists spaces inside the braces and a plain Table suffix too, and not a bare table name" $ do
      formatNormalisations "Restricted_Reference_Type{6}, uidref{ ColumnTableUID }"
        `shouldBe` [("uidref{ ColumnTableUID }", "uidref{ColumnTableUID}")]
      formatNormalisations "Restricted_Reference_Type{6}, uidref{ColumnTable}" `shouldBe` [("ColumnTable", "ColumnTableUID")]
      formatNormalisations "Restricted_Reference_Type{6}, uidref{Column}" `shouldBe` []
    it "lists something for every spelling at a brace that reads as the canonical cell but is printed otherwise" $ do
      let canonical = "Restricted_Reference_Type{6}, uidref{LogTableUID}"
          variants  = [ "Restricted_Reference_Type {6}, uidref{LogTableUID}", "Restricted_Reference_Type{ 6 }, uidref{LogTableUID}"
                      , "Restricted_Reference_Type{6}, uidref {LogTableUID}", "Restricted_Reference_Type{6}, uidref{ LogTableUID}"
                      , "Restricted_Reference_Type{6}, uidref{LogTable_UID}", "Restricted_Reference_Type{6}, uidref{LogTable}"
                      , "Restricted_Reference_Type{6}, uidref { LogTable }" ]
      formatNormalisations canonical `shouldBe` []
      parseFormat smallEnv canonical `shouldSatisfy` isRight
      forM_ variants $ \v -> do
        parseFormat smallEnv v `shouldBe` parseFormat smallEnv canonical
        (v, formatNormalisations v) `shouldNotSatisfy` (null . snd)
    it "finds them in the fourteen printed cells that have them" $
      [ rowTable r | r <- rows, not (null (formatNormalisations (rowFormat r))) ]
        `shouldBe` [57, 62, 72, 76, 77, 104, 105, 111, 112, 119, 140, 141, 142, 144]

  -- the fixtures are copies of spec/ir/core-2.01.json: a regenerated IR that changed them fails here
  withCore $ \ir -> describe "the fixtures against spec/ir/core-2.01.json" $ do
    let typeTables = [ t | t <- ir.tables, Just n <- [t.tableNumber], t.part `elem` [Nothing, Just 1]
                         , map (normHeader . (.text)) (concat (take 1 t.headerRows)) == ["uid", "name", "format"]
                         , [_, _, _] : _ <- [t.rows], n >= 46, n <= 165 ]
    it "hold every Type table row as the IR prints it" $
      [ Row n (cellText 1 r) (cellText 0 r) (cellText 2 r) | t <- typeTables, Just n <- [t.tableNumber], r : _ <- [t.rows] ]
        `shouldBe` rows
    it "hold the Table Name column of the IR's T240" $
      [ T.strip c.text | t <- ir.tables, t.id == Just "T240", r <- t.rows, c <- take 1 (drop 2 r), not (T.null (T.strip c.text)) ]
        `shouldBe` tables

  describe "tableName" $
    it "strips the TableUID suffix in each of its spellings" $
      map tableName ["C_PINTableUID", "ColumnTable_UID", "LogTableUID", "C_AES_*TableUID", "Foo"]
        `shouldBe` ["C_PIN", "Column", "Log", "C_AES_*", "Foo"]
  where
    spelledCode r = T.filter (not . isSpace) (T.takeWhile (/= ',') (rowFormat r))
    cellText :: Int -> [IR.CellIR] -> Text
    cellText i r = maybe "" (.text) (listToMaybe (drop i r))
