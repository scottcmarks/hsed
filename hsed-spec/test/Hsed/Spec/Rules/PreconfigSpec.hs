{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.Rules.PreconfigSpec
Description : Test specifications for the preconfig rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The target a preconfiguration caption names, Core's and Opal's; then the rule
(design section 3.4, example E12) on the committed Core extraction with the
committed annotations: Tables 209 and 216, the default authorities of the
Base and Admin templates, bound from the doc anchor's rule token, both rows of
the Authority table (T178), their 17 rows read as the description types their
columns; C-PRECONF-HEADER, C-UID-TABLE, C-UID-UNIQUE and C-PRECONF-CELL made to
fail by one edited cell; an informative column dropped, a group row read as
context, a template row left out; the generated rows. Opal's Table 24, the
design's exemplar, fires too.
-}
module Hsed.Spec.Rules.PreconfigSpec (spec) where

import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO
import           RIO.Directory             (doesFileExist)
import           Test.Hspec

import           Hsed.Spec.Anchor          (AnchorId (..))
import           Hsed.Spec.Check           (checkIR, mkContext)
import           Hsed.Spec.Common          (ErrataSet (..), RuleName (..), SpecId (..), isError, renderFinding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.Compile         (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR              (SpecIR (..), TableIR (..), readIR)
import           Hsed.Spec.RuleDef         (BoundTable (..), Classification (..), RuleDef (..))
import           Hsed.Spec.Rules           (classify, explain)
import           Hsed.Spec.Rules.Preconfig
import           Hsed.Spec.Rules.TableDesc (DescRead (..))
import           Hsed.Spec.Shape           (measure)


errorsOf :: SpecIR -> [Text]
errorsOf = map renderFinding . filter isError . checkIR Scott

-- | The generated module of an IR.
generated :: SpecIR -> IO [Text]
generated ir = do
  cr <- either (throwString . show) pure (compileIR Scott ir)
  pure (T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Preconfig" (crModules cr))))

-- | The lines of one declaration.
block :: Text -> [Text] -> [Text]
block n = takeWhile (/= "") . dropWhile (/= (n <> " :: [Pre Row]"))

spec :: Spec
spec = do
  describe "targetNoun" $ do
    it "reads Opal's <SP> - <T> Table Preconfiguration and Core's Default … <Ts>" $ do
      targetNoun "Admin SP - Authority Table Preconfiguration" `shouldBe` Just "Authority"
      targetNoun "Admin SP – DataRemovalMechansim Table Preconfiguration" `shouldBe` Just "DataRemovalMechansim"
      targetNoun "Default Base Template Authorities" `shouldBe` Just "Authorities"
      targetNoun "Authority Table Description" `shouldBe` Nothing
    it "names the generated rows after the caption" $ do
      preDeclName "Default Base Template Authorities" `shouldBe` "defaultBaseTemplateAuthorities"
      preDeclName "Admin SP - Authority Table Preconfiguration" `shouldBe` "adminSPAuthorityTablePreconfiguration"

  describe "Opal Table 24, the design's exemplar" $ do
    let path = "../spec/ir/opal-2.30.json"
    present <- runIO (doesFileExist path)
    if not present then it "needs the committed Opal IR" (pendingWith path) else do
      opal <- runIO (readIR path >>= either throwString pure)
      it "fires the rule" $
        [ fires preconfigRule (SpecId "opal-2.30") t (measure (SpecId "opal-2.30") t) | t <- opal.tables, t.id == Just "T24" ]
          `shouldBe` [True]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx = mkContext Scott ir
        ps  = preReads ctx

    describe "the preconfig rule" $ do
      it "binds Tables 209 and 216, the default authorities, by the doc anchor's rule token" $ do
        [ aid | (aid, RuleBound (RuleName "preconfig") _ _) <- Map.toList (classify ir) ] `shouldBe` [TableId 209, TableId 216]
        explain ir (TableId 209) `shouldBe` "preconfig (exemplar core T209)"
      it "reads both as rows of the Authority table, its description T178, for the Base and Admin templates" $ do
        [ (btAnchor (pBound p), either (const Nothing) (Just . btAnchor . drBound) (pTarget p), captionTemplate p) | p <- ps ]
          `shouldBe` [ (TableId 209, Just (TableId 178), Just "Base"), (TableId 216, Just (TableId 178), Just "Admin") ]
      it "gives the 17 default authorities' names and UIDs, for the table of authority UIDs to agree with" $ do
        let rows = preRowsFor ctx "Authority"
        length rows `shouldBe` 17
        take 1 rows `shouldBe` [("Anybody", [0, 0, 0, 9, 0, 0, 0, 1])]
        drop 16 rows `shouldBe` [("Reserve3", [0, 0, 0, 9, 0, 0, 2, 8])]
      it "finds no Error on the committed annotations" $
        errorsOf ir `shouldBe` []

    describe "C-PRECONF-HEADER" $
      it "fails on a header naming no column of the target, and leaves the table out" $ do
        let broken = setHeaderCell "T209" 0 5 "Operations" ir
        errorsOf broken `shouldBe`
          [ "C-PRECONF-HEADER core-2.01.pdf#page=211&T209: the header \"Operations\" names no column of T178 Authority" ]
        newlyExcluded ir broken `shouldReturn`
          [(TableId 209, "C-PRECONF-HEADER, C-UID-TABLE, C-UID-UNIQUE or C-PRECONF-CELL failed")]

    describe "C-UID-TABLE and C-UID-UNIQUE" $ do
      it "fail on a UID outside the Authority table" $
        errorsOf (setCell "T209" 0 1 "00 00 00 0A 00 00 00 01" ir) `shouldBe`
          [ "C-UID-TABLE core-2.01.pdf#page=211&T209: row 1 Anybody: the UID 00 00 00 0A 00 00 00 01 does not lie in the Authority table, whose half-UID is 00 00 00 09" ]
      it "fail on a UID two rows give, across the tables" $
        errorsOf (setCell "T216" 0 1 "00 00 00 09 00 00 00 01" ir) `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=236&T216: row 1 Issuers: the UID 00 00 00 09 00 00 00 01 is also that of T209 row 1 Anybody" ]

    describe "C-PRECONF-CELL" $ do
      it "fails on a label of no value of the column's enumeration" $
        errorsOf (setCell "T209" 0 5 "Sing" ir) `shouldBe`
          [ "C-PRECONF-CELL core-2.01.pdf#page=211&T209: row 1 Anybody: Operation: \"Sing\" is no label of auth_method's enumeration (None, Password, Exchange, Sign, SymK, HMAC, TPerSign, TPerExchange, Reserved)" ]
      it "fails on a reference naming no preconfigured row of the table it refers to" $
        errorsOf (setCell "T209" 3 4 "Maker" ir) `shouldBe`
          [ "C-PRECONF-CELL core-2.01.pdf#page=211&T209: row 4 MakerSymK: Class: \"Maker\" names no row preconfigured for Authority" ]

    describe "informative columns, group rows and template rows" $ do
      it "drops a column whose header says it is informative" $ do
        let informative = setHeaderCell "T209" 0 5 "Operation - informative text" ir
        errorsOf informative `shouldBe` []
        [ pDropped p | p <- preReads (mkContext Scott informative) ] `shouldBe` [["Operation - informative text"], []]
        lns <- generated informative
        filter ("(0x09," `T.isInfixOf`) (block "defaultBaseTemplateAuthorities" lns) `shouldBe` []
      it "reads a row whose first cell alone is printed as the context of the rows below it" $ do
        let grouped = foldr (\c -> setCell "T209" 0 c "") ir [1 .. 5]
        errorsOf grouped `shouldBe` []
        length (preRowsFor (mkContext Scott grouped) "Authority") `shouldBe` 16
        lns <- generated grouped
        take 1 (drop 2 (block "defaultBaseTemplateAuthorities" lns)) `shouldBe` ["  [ Pre  -- row 2 Admins (group Anybody)"]
      it "leaves a template row out, with a Warning" $ do
        let templated = setCell "T216" 7 1 "00 00 00 09 00 00 02 08 (+XX)" ir
        errorsOf templated `shouldBe` []
        map renderFinding (filter ((== "W-PRECONF-TEMPLATE") . Cm.code) (checkIR Scott templated)) `shouldBe`
          [ "W-PRECONF-TEMPLATE core-2.01.pdf#page=236&T216: row 8 Reserve3: a template row ((+XX)), not generated: the expand modifier is to expand it" ]
        lns <- generated templated
        length (filter ("Pre  -- " `T.isInfixOf`) (block "defaultAdminTemplateAuthorities" lns)) `shouldBe` 7

    describe "the generated module" $ do
      lns <- runIO (generated ir)
      it "declares the rows of each table" $ do
        length (filter ("Pre  -- " `T.isInfixOf`) (block "defaultBaseTemplateAuthorities" lns)) `shouldBe` 9
        length (filter ("Pre  -- " `T.isInfixOf`) (block "defaultAdminTemplateAuthorities" lns)) `shouldBe` 8
      it "gives each row's cells by column number, typed by the Authority table's description" $
        take 9 (drop 2 (block "defaultBaseTemplateAuthorities" lns)) `shouldBe`
          [ "  [ Pre  -- row 1 Anybody"
          , "      [ (0x00, UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x01))"
          , "      , (0x01, TextCell \"Anybody\")"
          , "      , (0x02, TextCell \"Anybody\")"
          , "      , (0x03, EnumCell \"False\" 0)"
          , "      , (0x09, EnumCell \"Sign\" 3)"
          , "      ]"
          , "      []"
          , "  , Pre  -- row 2 Admins" ]
      it "resolves a Class by the name of a preconfigured authority" $
        block "defaultBaseTemplateAuthorities" lns `shouldContain`
          ["      , (0x04, RefCell \"Admins\" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02))"]
      it "cites the table and its target" $
        lns `shouldSatisfy` any (\l -> "-- | core-2.01 T216 Default Admin Template Authorities (PDF 236 / p.219, ord 218, sha256:" `T.isPrefixOf` l
                                         && "): rows of the Authority table (T178)" `T.isSuffixOf` l)
