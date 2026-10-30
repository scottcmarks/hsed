{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.RangesSpec
Description : Test specifications for the ranges rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The ranges rule on the committed Core extraction with the committed
annotations (design section 3.4, M2b): it binds Tables 237 and 238 by the doc
anchor's @rule ranges@; C-UID-RANGE passes against Table 237 for every row of
Table 240 and of the MethodID and Authority UIDs that names a template, and
against Table 238 for every type row, and fails on each kind of edit; the
range tables themselves are checked for four-byte values, order and overlap;
the generated module lists both tables' ranges.
-}
module Hsed.Spec.Rules.RangesSpec (spec) where

import qualified RIO.Map               as Map
import qualified RIO.Text              as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor      (AnchorId (..))
import           Hsed.Spec.Check       (checkIR, mkContext)
import           Hsed.Spec.Common      (ErrataSet (..), RuleName (..), isError, renderFinding)
import qualified Hsed.Spec.Common      as Cm
import           Hsed.Spec.Compile     (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR          (SpecIR)
import           Hsed.Spec.RuleDef     (BoundTable (..), Classification (..))
import           Hsed.Spec.Rules       (classify, explain)
import           Hsed.Spec.Rules.Ranges


-- | The C-UID-RANGE Errors of an IR.
rangeErrors :: SpecIR -> [Text]
rangeErrors = map renderFinding . filter (\f -> isError f && Cm.code f == "C-UID-RANGE") . checkIR Scott

spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx = mkContext Scott ir
      rows = rangeRows ctx
      of' n = [ r | r <- rows, btAnchor (rgBound r) == TableId n ]
      scott = checkIR Scott ir

  describe "the ranges rule" $ do
    it "binds Tables 237 and 238, and no other table, by the doc anchor's rule token" $ do
      [ aid | (aid, RuleBound (RuleName "ranges") _ _) <- Map.toList (classify ir) ] `shouldBe` [TableId 237, TableId 238]
      explain ir (TableId 238) `shouldBe` "ranges (exemplar core T238)"
    it "reads each table's categories by its first header" $
      map (\n -> (rangeKey . rgBound) <$> listToMaybe (of' n)) [237, 238] `shouldBe` [Just "Template", Just "Type Format"]
    it "reads Table 237's ten ranges" $
      [ (rgName r, rgStart r, rgEnd r) | r <- of' 237 ] `shouldBe`
        [ ("Unassigned", Just 0, Just 0), ("Base", Just 0x1, Just 0x200), ("Admin", Just 0x201, Just 0x400)
        , ("Clock", Just 0x401, Just 0x600), ("Crypto", Just 0x601, Just 0x800), ("Locking", Just 0x801, Just 0xA00)
        , ("Log", Just 0xA01, Just 0xC00), ("Unassigned", Just 0xC01, Just 0x10000)
        , ("Non-reserved non-assigned", Just 0x10001, Just 0xFFFEFFFF), ("Vendor unique (upper 64K)", Just 0xFFFF0000, Just 0xFFFFFFFF) ]
    it "reads Table 238's sixteen, the last one cell spanning Start and End" $ do
      length (of' 238) `shouldBe` 16
      [ (rgName r, rgStart r, rgEnd r, rgOneCell r) | r <- drop 14 (of' 238) ] `shouldBe`
        [ ("Unassigned", Just 0x1C01, Just 0x10000, False), ("Unassigned", Just 0, Just 0, True) ]
    it "takes the LSB of a UID" $
      lsb [0, 0, 0, 6, 0, 0, 0x06, 0x0F] `shouldBe` 0x060F

  describe "C-UID-RANGE against Table 237 (acceptance)" $ do
    it "passes: every row of T240, T242 and T243 that names a template lies in its template's range" $ do
      [ renderFinding f | f <- scott, Cm.code f == "C-UID-RANGE" ] `shouldBe` []
    it "lists the MethodID UIDs that name no template, with the range each lies in" $
      [ renderFinding f | f <- scott, Cm.code f == "N-UID-RANGE-NO-TEMPLATE" ] `shouldBe`
        [ "N-UID-RANGE-NO-TEMPLATE core-2.01.pdf#page=303&T242: rows that name no template, with the range of T237 their LSB lies in: "
          <> "row 6 \"OBSOLETE *\" in Base; row 7 \"OBSOLETE *\" in Base; row 12 \"OBSOLETE *\" in Base; row 17 \"Reserved for SSC Usage\" in Base; "
          <> "row 24 \"Reserved for SSC Usage\" in Admin; row 25 \"Reserved for SSC Usage\" in Admin; row 54 \"Reserved for SSC Usage\" in Locking" ]
    it "fails on a Table 240 row whose descriptor object lies outside its template's range" $
      rangeErrors (setCell "T240" 30 3 "Base" ir) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=302&T240: row 31 CryptoSuite: the LSB 00 00 02 03 lies outside 00 00 00 01 to 00 00 02 00, which T237 gives Base" ]
    it "fails on a MethodID UID outside its template's range" $
      rangeErrors (setCell "T242" 19 2 "Admin" ir) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=304&T242: row 20 \"Get | Admin\": the LSB 00 00 00 16 lies outside 00 00 02 01 to 00 00 04 00, which T237 gives Admin" ]
    it "fails on an authority outside its template's range, and on a template Table 237 does not name" $ do
      rangeErrors (setCell "T243" 9 2 "Base" ir) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=305&T243: row 10 \"Issuers | Base\": the LSB 00 00 02 01 lies outside 00 00 00 01 to 00 00 02 00, which T237 gives Base" ]
      rangeErrors (setCell "T243" 0 2 "Basic" ir) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=304&T243: row 1 \"Anybody | Basic\": the template Basic is none T237 gives a range" ]

  describe "C-UID-RANGE against Table 238" $ do
    it "passes for the 100 type rows, reading two range names' spaces as underscores" $
      [ renderFinding f | f <- scott, Cm.anchor f == Just "T238" ] `shouldBe`
        [ "N-RANGE-NAME-NORMALISED core-2.01.pdf#page=301&T238: row 13 Struct Type: read as Struct_Type, the name 3 type rows' Format gives (T75, T78, T99)"
        , "N-RANGE-NAME-NORMALISED core-2.01.pdf#page=301&T238: row 14 Set Type: read as Set_Type, the name 5 type rows' Format gives (T47, T53, T86, T123, T127)"
        , "N-RANGE-ONE-CELL core-2.01.pdf#page=301&T238: row 16 Unassigned: one cell spans Start and End: the range is 00 00 00 00 alone" ]
    it "fails on a type row outside its format's range" $
      rangeErrors (setCell "T46" 0 0 "00 00 00 05 00 00 04 01" ir) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=100&T46: AC_element: the LSB 00 00 04 01 of 00 00 00 05 00 00 04 01 lies outside 00 00 08 01 to 00 00 0A 00, which T238 gives List_Type" ]
    it "finds General_Reference_Type{8} under General_Reference_Type, and fails where no range name matches" $ do
      rangeErrors (setCell "T119" 0 0 "00 00 00 05 00 00 10 02" ir) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=117&T119: object_ref: the LSB 00 00 10 02 of 00 00 00 05 00 00 10 02 lies outside 00 00 0F 01 to 00 00 10 00, which T238 gives General_Reference_Type{8}" ]
      rangeErrors (setCell "T238" 7 0 "General Reference" ir) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=" <> p <> ": " <> n <> ": its Format names " <> fmt <> ", which is no type format T238 gives a range"
        | (p, n, fmt) <- [ ("104&T62", "byte_row_ref", "General_Reference_Type{7}"), ("117&T119", "object_ref", "General_Reference_Type{8}")
                         , ("122&T141", "table_ref", "General_Reference_Type{9}") ] ]

  describe "C-UID-RANGE on the range tables" $ do
    it "fails on two ranges that overlap" $
      filter ("&T237" `T.isInfixOf`) (rangeErrors (setCell "T237" 1 2 "00 00 02 01" ir)) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=300&T237: row 3 Admin: the range 00 00 02 01 to 00 00 04 00 overlaps row 2 Base, 00 00 00 01 to 00 00 02 01" ]
    it "fails on a range that ends before it starts" $
      filter ("&T238" `T.isInfixOf`) (rangeErrors (setCell "T238" 0 1 "00 00 03 00" ir)) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=300&T238: row 1 Base_Type: the range starts at 00 00 03 00, after its end 00 00 02 00" ]
    it "fails on a value that is not four bytes" $
      filter ("&T237" `T.isInfixOf`) (rangeErrors (setCell "T237" 3 1 "00 00 04" ir)) `shouldBe`
        [ "C-UID-RANGE core-2.01.pdf#page=300&T237: row 4 Clock: the start \"00 00 04\" is not four bytes" ]

  describe "the generated module" $ do
    cr <- runIO (either (throwString . show) pure (compileIR Scott ir))
    let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.UIDRanges" (crModules cr)))
    it "lists each table's ranges, named after its first header" $ do
      lns `shouldContain` [ "templateRanges :: [(String, (Word32, Word32))]", "templateRanges =", "  [ (\"Unassigned\", (0x00000000, 0x00000000))"
                          , "  , (\"Base\", (0x00000001, 0x00000200))" ]
      lns `shouldContain` [ "  , (\"Set Type\", (0x00001a01, 0x00001c00))", "  , (\"Unassigned\", (0x00001c01, 0x00010000))"
                          , "  , (\"Unassigned\", (0x00000000, 0x00000000))", "  ]" ]
      length [ l | l <- lns, " :: [(String, (Word32, Word32))]" `T.isSuffixOf` l ] `shouldBe` 2
