{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.TableUIDsSpec
Description : Test specifications for the tableUIDs rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The tableUIDs rule on the committed Core extraction with the committed
annotations (design example E6, M2a): it fires on Table 240 alone, from the
doc anchor's @rule tableUIDs@; its 46 table names are the names a
@uidref{...}@ may name; C-T240-HALVES passes on all 46 rows and fails on one
edited cell, each of its relations in turn (the descriptor's low half and the
table's high half, the table's zero low half, the descriptor in the table of
tables); C-T240-NAME refuses a name that is no identifier or that two rows
give; the generated module has the hand-written module's shapes, each row
citing the page it is printed on. Since M5a, Opal's typed half-UIDs from the
Kind column of an SP's Table table (Table 20, PDF 36-37; Table 36, PDF
53-54): every row read (@Obje ct@, a note marker after a UID), the kinds
held to agree (C-TABLE-KIND), and @oth\<T\>@, @bth\<T\>@ and @byteTables@
generated in Opal's TableUIDs module, under the M4b sidecar (the Admin SP's
twelve object tables) and the M5a sidecar (MBR and DataStore byte tables).
-}
module Hsed.Spec.Rules.TableUIDsSpec (spec) where

import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor          (AnchorId (..))
import           Hsed.Spec.Check           (checkIR, mkContext, mkContextWith)
import           Hsed.Spec.Common          (ErrataSet (..), RuleName (..), SpecId (..), isError, renderFinding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.Compile         (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import qualified Hsed.Spec.Gen             as Gen
import           Hsed.Spec.Registry        (registry)
import           Hsed.Spec.RuleDef         (BoundTable (..), Classification (..), Context (..))
import           Hsed.Spec.Rules           (classify, explain)
import           Hsed.Spec.Rules.TableUIDs


spec :: Spec
spec = do
  coreSpec
  opalSpec

-- | Opal's typed half-UIDs, under the M4b and the M5a sidecars, over Core.
opalSpec :: Spec
opalSpec = do
  withM4bOpal $ \base opal -> do
    let kinds = tableKinds (mkContextWith registry (Just base) Scott opal)
        errorsOf ir' = [ renderFinding f | f <- checkIR (Just base) Scott ir', Cm.code f == "C-TABLE-KIND" ]
    describe "tableKinds, Opal under the M4b sidecar (Table 20, PDF 36-37)" $ do
      it "reads the Admin SP's twelve tables, each an object table, Obje ct read as Object" $ do
        [ (r.tkName, r.tkKind) | Right r <- kinds ] `shouldBe`
          [ (n, ObjectTable) | n <- [ "Table", "SPInfo", "SPTemplates", "MethodID", "AccessControl", "ACE", "Authority", "C_PIN"
                                    , "TPerInfo", "Template", "SP", "DataRemovalMechanism" ] ]
        [ r.tkHalf | Right r <- kinds, r.tkName == "DataRemovalMechanism" ] `shouldBe` [[0, 0, 0x11, 1]]
        errorsOf opal `shouldBe` []
      it "is C-TABLE-KIND for a Kind neither Object nor Byte" $
        errorsOf (setCell "T20" 0 4 "VU" opal) `shouldBe`
          ["C-TABLE-KIND opal-2.30.pdf#page=36&T20: row 1 Table: the Kind \"VU\" is neither Object nor Byte"]
      it "generates oth<T> for each in Opal's TableUIDs module, and byteTables empty" $ do
        cr <- either (throwString . show) pure (compileIR (Just base) Scott opal)
        let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Opal.TableUIDs" (crModules cr)))
        length [ l | l <- lns, " :: Object_Table_HalfUID" `T.isSuffixOf` l ] `shouldBe` 12
        lns `shouldContain` ["othACE :: Object_Table_HalfUID", "othACE = otHalfUID 0x00 0x00 0x00 0x08"]
        lns `shouldContain` ["byteTables :: [String]", "byteTables = []"]
  withM5aOpal $ \base opal -> do
    let kinds = tableKinds (mkContextWith registry (Just base) Scott opal)
        errorsOf ir' = [ renderFinding f | f <- checkIR (Just base) Scott ir', Cm.code f == "C-TABLE-KIND" ]
    describe "tableKinds, Opal under the M5a sidecar (Tables 20 and 36, PDF 36-37 and 53-54)" $ do
      it "reads the Locking SP's sixteen tables too, MBR and DataStore byte tables, K_AES_128's UID before its note marker" $ do
        length [ () | Right r <- kinds, btAnchor r.tkBound == TableId 36 ] `shouldBe` 16
        [ r.tkName | Right r <- kinds, r.tkKind == ByteTable ] `shouldBe` ["MBR", "DataStore"]
        [ r.tkHalf | Right r <- kinds, r.tkName == "K_AES_128" ] `shouldBe` [[0, 0, 8, 5]]
        errorsOf opal `shouldBe` []
      it "is C-TABLE-KIND where two SPs give one table two kinds, at the later row" $
        errorsOf (setCell "T36" 0 4 "Byte" opal) `shouldBe`
          ["C-TABLE-KIND opal-2.30.pdf#page=53&T36: row 1 Table: Byte, where T20 row 1 gives Object"]
      it "generates each table once, bth<T> for the byte tables, and names them in byteTables" $ do
        cr <- either (throwString . show) pure (compileIR (Just base) Scott opal)
        let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Opal.TableUIDs" (crModules cr)))
        length [ l | l <- lns, " :: Object_Table_HalfUID" `T.isSuffixOf` l ] `shouldBe` 18
        lns `shouldContain` ["bthMBR :: Byte_Table_HalfUID", "bthMBR = btHalfUID 0x00 0x00 0x08 0x04"]
        lns `shouldContain` ["byteTables :: [String]", "byteTables =", "  [ \"MBR\"", "  , \"DataStore\"", "  ]"]

coreSpec :: Spec
coreSpec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx  = mkContext Scott ir
      rows = rights (tableUIDRows ctx)
      errorsOf ir' = map renderFinding (filter isError (checkIR Nothing Scott ir'))

  describe "allTableUIDRows" $ do
    it "reads Core alone as its own Table 240's rows" $
      map (tuName . snd) (allTableUIDRows ctx) `shouldBe` map tuName rows
    withOpal $ \opalPristine -> do
      opal  <- runIO (annotated opalPristine m4bOpalHsa)
      ohsa  <- runIO committedOpalHsa
      opal0 <- runIO (annotated opalPristine (withoutLine "4.2" m4bOpalHsa))
      let octx = opalOver ctx opal
          alls = allTableUIDRows octx
          drm  = [ r | (c, r) <- alls, cxSpec c == SpecId "opal-2.30" ]
      it "gives Opal, under the M4b sidecar, the one table Table 240 does not name, from the Admin SP's Table table (T20 row 12, PDF 37), then Core's rows" $ do
        map tuName drm `shouldBe` ["DataRemovalMechanism"]
        [ (btAnchor (tuBound r), tuRow r + 1, tuObject r, tuTable r) | r <- drm ]
          `shouldBe` [(TableId 20, 12, Just [0, 0, 0, 1, 0, 0, 0x11, 0x01], Just [0, 0, 0x11, 0x01, 0, 0, 0, 0])]
        map (tuName . snd) (drop 1 alls) `shouldBe` map tuName rows
        [ cxSpec c | (c, _) <- drop 1 alls ] `shouldSatisfy` all (== core)
      it "prints its row's declarations as Table 240's are printed (rowDecl), citing T20's page 37 and row 12" $ do
        let body = [ l | (c, r) <- take 1 alls, Just o <- [tuObject r], Just t <- [tuTable r], l <- (rowDecl c r o t).declBody ]
        body `shouldContain` ["uDataRemovalMechanismTableObject :: UID", "uDataRemovalMechanismTableObject = uid 0x00 0x00 0x00 0x01 0x00 0x00 0x11 0x01"]
        body `shouldContain` ["uDataRemovalMechanismTable :: UID", "uDataRemovalMechanismTable = uid 0x00 0x00 0x11 0x01 0x00 0x00 0x00 0x00"]
        body `shouldContain` ["hDataRemovalMechanism :: HalfUID", "hDataRemovalMechanism = halfUID 0x00 0x00 0x11 0x01"]
        take 1 body `shouldBe` ["-- | opal-2.30 T20 - Admin SP - Table Table Preconfiguration (PDF 37 / p.36, ord 22, sha256:98a412fc6777), row 12: DataRemovalMechanism"]
      it "is the committed sidecar's since M5a's integration: the M5a sidecar, line for line, is the committed one" $
        L.sort (T.lines ohsa) `shouldBe` L.sort (T.lines m5aOpalHsa)
      it "gives Opal under a sidecar that binds no preconfiguration (the M4b sidecar without its 4.2 line), Core's rows alone" $
        map (\(c, r) -> (cxSpec c, tuName r)) (allTableUIDRows (opalOver ctx opal0)) `shouldBe` map (\r -> (core, tuName r)) rows

  describe "the tableUIDs rule" $ do
    it "binds Table 240, and no other table, by the doc anchor's rule token" $ do
      [ aid | (aid, RuleBound (RuleName "tableUIDs") _ _) <- Map.toList (classify ir) ] `shouldBe` [TableId 240]
      explain ir (TableId 240) `shouldBe` "tableUIDs (exemplar core T240)"
    it "reads the 46 rows, every one of them whole" $ do
      length rows `shouldBe` 46
      lefts (tableUIDRows ctx) `shouldBe` []
      take 3 (map tuName rows) `shouldBe` ["Table", "SPInfo", "SPTemplates"]
    it "gives the 46 table names a uidref{...} may name" $
      length (tableNames ctx) `shouldBe` 46

  describe "C-T240-HALVES" $ do
    it "passes on all 46 rows: the descriptor object's low half is the table's high half" $ do
      [ tuName r | r <- rows, fmap (drop 4) (tuObject r) /= fmap (take 4) (tuTable r) ] `shouldBe` []
      all (\r -> isJust (tuObject r) && isJust (tuTable r)) rows `shouldBe` True
      errorsOf ir `shouldBe` []
    it "fails on a row whose halves disagree, and leaves that row out of the module" $ do
      let broken = setCell "T240" 6 1 "00 00 00 17 00 00 00 00" ir
      errorsOf broken `shouldBe`
        [ "C-T240-HALVES core-2.01.pdf#page=302&T240: row 7 AccessControl: the low half 00 00 00 07 of the descriptor object's UID is not the high half 00 00 00 17 of the table's UID" ]
      newlyExcluded ir broken `shouldReturn` [(TableId 240, "row 7 AccessControl: C-T240-HALVES or C-T240-NAME failed")]
    it "fails on a UID cell that is not eight bytes" $
      errorsOf (setCell "T240" 0 0 "00 00 00 01 00 00 01" ir) `shouldBe`
        [ "C-T240-HALVES core-2.01.pdf#page=302&T240: row 1 Table: the descriptor object's UID cell \"00 00 00 01 00 00 01\" is not eight bytes" ]
    it "fails on a descriptor object outside the table of tables and a table UID whose low half is not zero (the middle halves agreeing)" $ do
      let moved = setCell "T240" 1 1 "00 00 00 02 00 00 00 07" (setCell "T240" 1 0 "00 00 00 09 00 00 00 02" ir)
      errorsOf moved `shouldBe`
        [ "C-T240-HALVES core-2.01.pdf#page=302&T240: row 2 SPInfo: the high half 00 00 00 09 of the descriptor object's UID is not 00 00 00 01, the half-UID of the table of tables (row 1 Table)"
        , "C-T240-HALVES core-2.01.pdf#page=302&T240: row 2 SPInfo: the low half 00 00 00 07 of the table's UID is not zero" ]
      newlyExcluded ir moved `shouldReturn` [(TableId 240, "row 2 SPInfo: C-T240-HALVES or C-T240-NAME failed")]
    it "fails when no row, or more than one, is the table of tables" $ do
      errorsOf (setCell "T240" 0 1 "00 00 00 3F 00 00 00 00" (setCell "T240" 0 0 "00 00 00 01 00 00 00 3F" ir))
        `shouldSatisfy` elem "C-T240-HALVES core-2.01.pdf#page=302&T240: no row's descriptor object lies in its own table, so no row is the table of tables"
      errorsOf (setCell "T240" 1 0 "00 00 00 02 00 00 00 02" ir)
        `shouldSatisfy` elem "C-T240-HALVES core-2.01.pdf#page=302&T240: rows 1 Table and 2 SPInfo each have their descriptor object in their own table"

  describe "C-T240-NAME" $
    it "refuses a name that is no identifier, and one an earlier row gives" $
      filter ("C-T240-NAME" `T.isPrefixOf`) (errorsOf (setCell "T240" 2 2 "SP Templates" (setCell "T240" 1 2 "Table" ir))) `shouldBe`
        [ "C-T240-NAME core-2.01.pdf#page=302&T240: row 2 Table: the table name is also that of row 1"
        , "C-T240-NAME core-2.01.pdf#page=302&T240: row 3 SP Templates: the table name is not an identifier" ]

  describe "the generated module" $ do
    cr <- runIO (either (throwString . show) pure (compileIR Nothing Scott ir))
    let text  = fromMaybe "" (lookup "Hsed.Gen.Core.TableUIDs" (crModules cr))
        lns   = T.lines text
    it "declares u<T>TableObject, u<T>Table and h<T> for every row, as the hand transcription (the oracle's fixture) names them" $ do
      length [ l | l <- lns, " :: UID" `T.isSuffixOf` l ] `shouldBe` 92
      length [ l | l <- lns, " :: HalfUID" `T.isSuffixOf` l ] `shouldBe` 46
      lns `shouldContain` ["uAccessControlTableObject :: UID", "uAccessControlTableObject = uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x07"]
      lns `shouldContain` ["hMBR :: HalfUID", "hMBR = halfUID 0x00 0x00 0x08 0x04"]
    it "maps every half-UID and UID to its name, as nameHalfUID and nameUID do" $ do
      lns `shouldContain` ["nameHalfUID :: Map HalfUID String", "nameHalfUID = fromList", "  [ (hTable, \"Table\")"]
      lns `shouldContain` ["nameUID :: Map UID String", "nameUID = fromList", "  [ (uTableTable, \"Table Table\")", "  , (uTableTableObject, \"Table Table Object\")"]
    it "declares no typed half-UID: Core does not say which tables are byte tables" $
      filter (\l -> any (`T.isPrefixOf` l) ["oth", "bth"]) lns `shouldBe` []
    it "cites the page each row is printed on: rows 1-31 on PDF 302, rows 32-46 on PDF 303" $ do
      lns `shouldContain` ["-- | core-2.01 T240 Table UIDs (PDF 302 / p.285, ord 242, sha256:2cd1f420bb70), row 31: CryptoSuite"]
      lns `shouldContain` ["-- | core-2.01 T240 Table UIDs (PDF 303 / p.286, ord 242, sha256:2cd1f420bb70), row 32: Template"]
