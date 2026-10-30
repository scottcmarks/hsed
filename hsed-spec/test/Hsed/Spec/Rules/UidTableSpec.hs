{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.UidTableSpec
Description : Test specifications for the uidTable rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The uidTable rule on the committed Core extraction with the committed
annotations (design section 3.4, example E6, M2b): it binds the nine UID
tables of section 6.3 besides Table 240 by the doc anchor's @rule uidTable@;
Table 243 yields its 17 authorities; every UID lies in the table its upper
half names, and in the one a header, caption or cell claims; the Special
Purpose UIDs' pattern covers Table 241; C-UID-TABLE, C-UID-UNIQUE and
C-UID-NAME fail on edited cells; the one UID two of the tables print is a
Note (N-UID-SHARED); the generated module names every named row and lists
every table. Whether Table 243 gives the authorities Tables 209 and 216
preconfigure is the preconfig rule's check (PreconfigSpec).
-}
module Hsed.Spec.Rules.UidTableSpec (spec) where

import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor          (AnchorId (..))
import           Hsed.Spec.Check           (checkIR, mkContext)
import           Hsed.Spec.Common          (ErrataSet (..), RuleName (..), isError, renderFinding)
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.Compile         (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR              (SpecIR)
import           Hsed.Spec.RuleDef         (BoundTable (..), Classification (..))
import           Hsed.Spec.Rules           (classify, explain)
import           Hsed.Spec.Rules.UidTable


-- | The Errors of a run, but the preconfig rule's C-PRECONF-UID, which an
--   edited row of Table 243 also fails (its own tests are PreconfigSpec's).
errorsOf :: SpecIR -> [Text]
errorsOf = map renderFinding . filter (\f -> isError f && Cm.code f /= "C-PRECONF-UID") . checkIR Nothing Scott

spec :: Spec
spec = do
  describe "asName" $
    it "takes one word, or several capitalised words joined, and nothing else" $
      map asName ["Get", "Global Range", "C_RSA_1024", "0x0A", "Reserved for SSC Usage", "OBSOLETE *", ""]
        `shouldBe` [Just "Get", Just "GlobalRange", Just "C_RSA_1024", Just "0x0A", Nothing, Nothing, Nothing]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx  = mkContext Scott ir
        rows = uidRows ctx
        of' n = [ r | r <- rows, btAnchor (urBound r) == TableId n ]

    describe "the uidTable rule" $ do
      it "binds T239 and T241-T248, and no other table, by the doc anchor's rule token" $ do
        [ aid | (aid, RuleBound (RuleName "uidTable") _ _) <- Map.toList (classify ir) ]
          `shouldBe` map TableId (239 : [241 .. 248])
        explain ir (TableId 242) `shouldBe` "uidTable (exemplar core T242)"
      it "reads every row of the nine tables" $
        [ length (of' n) | n <- 239 : [241 .. 248] ] `shouldBe` [5, 6, 54, 17, 4, 5, 7, 7, 30]
      it "yields Table 243's 17 authorities, each a UID of the Authority table" $ do
        map urName (of' 243) `shouldBe` map Just
          [ "Anybody", "Admins", "Makers", "MakerSymK", "MakerPuK", "SID", "TPerSign", "TPerExch", "AdminExch"
          , "Issuers", "Editors", "Deleters", "Servers", "Reserve0", "Reserve1", "Reserve2", "Reserve3" ]
        L.nub (map urTable (of' 243)) `shouldBe` [Just "Authority"]
        mapMaybe urIdent (of' 243) `shouldSatisfy` (\is -> length is == 17 && all ("uAuthority_" `T.isPrefixOf`) is)
        map urTemplate (of' 243) `shouldBe` replicate 9 (Just "Base") <> replicate 8 (Just "Admin")
      it "finds the table every UID lies in by its upper half, and the table the header, caption or cell claims" $ do
        [ (n, L.nub (map urTable (of' n)), L.nub (map urClaimed (of' n))) | n <- [242, 246, 247, 248] ]
          `shouldBe` [ (242, [Just "MethodID"], [Just "MethodID"]), (246, [Just "Template"], [Just "Template"])
                     , (247, [Just "SPTemplates"], [Just "SPTemplates"]), (248, [Just "SecretProtect"], [Just "SecretProtect"]) ]
        [ (urTable r, urClaimed r) | r <- of' 244 ] `shouldBe`
          [ (Just t, Just t) | t <- ["SPInfo", "TPerInfo", "LockingInfo", "MBRControl"] ]
        L.nub (map urTable (of' 241)) `shouldBe` [Nothing]
        map urTable (of' 239) `shouldBe` [Nothing, Nothing, Nothing, Nothing, Just "C_PIN"]
      it "reads the Special Purpose UIDs' fourth row as a pattern of one byte" $
        map urValue (take 1 (drop 3 (of' 239))) `shouldBe` [Pattern [0, 0, 0, 0, 0, 0, 0xFF] 1]

    describe "C-UID-TABLE" $ do
      it "passes on every row, and the pattern covers the six UIDs of Table 241" $
        [ f | f <- map renderFinding (checkIR Nothing Scott ir), any (`T.isPrefixOf` f) ["C-UID-TABLE", "C-UID-UNIQUE", "C-UID-NAME"]
            , not ("T149" `T.isInfixOf` f) ] `shouldBe` []
      it "fails on a UID whose upper half is not that of the table its header names" $
        errorsOf (setCell "T242" 0 0 "00 00 00 09 00 00 00 01" ir) `shouldBe`
          [ "C-UID-TABLE core-2.01.pdf#page=303&T242: row 1 \"DeleteSP | Base\": the upper half 00 00 00 09 is not 00 00 00 06, the half-UID of the MethodID table it lies in (it is Authority's)" ]
      it "fails on a UID outside the table its caption names" $
        errorsOf (setCell "T246" 0 0 "00 00 02 05 00 00 00 01" ir) `shouldBe`
          [ "C-UID-TABLE core-2.01.pdf#page=305&T246: row 1 \"Base\": the upper half 00 00 02 05 is not 00 00 02 04, the half-UID of the Template table it lies in (it is SP's)" ]
      it "fails on a row of a single-row table whose UID is another table's" $
        errorsOf (setCell "T244" 1 1 "LockingInfo" ir) `shouldBe`
          [ "C-UID-TABLE core-2.01.pdf#page=305&T244: row 2 \"LockingInfo\": the upper half 00 00 02 01 is not 00 00 08 01, the half-UID of the LockingInfo table it lies in (it is TPerInfo's)" ]
      it "fails on a UID the pattern of Table 239 does not cover" $
        errorsOf (setCell "T241" 0 0 "00 00 00 00 00 00 FE 01" ir) `shouldBe`
          [ "C-UID-TABLE core-2.01.pdf#page=303&T241: row 1 of T241: the UID 00 00 00 00 00 00 FE 01 does not match the pattern 00 00 00 00 00 00 FF xx that T239 row 4 gives for the UIDs of Table 241" ]
      it "fails on a UID cell that is neither eight bytes nor a pattern" $
        errorsOf (setCell "T247" 2 0 "00 00 00 03 00 00 03" ir) `shouldBe`
          [ "C-UID-TABLE core-2.01.pdf#page=306&T247: row 3 \"Clock\": the UID cell \"00 00 00 03 00 00 03\" is neither eight bytes nor a pattern ending xx" ]

    describe "C-UID-UNIQUE and C-UID-NAME" $ do
      it "fail on two rows of one table with one UID" $
        errorsOf (setCell "T243" 1 0 "00 00 00 09 00 00 00 01" ir) `shouldBe`
          [ "C-UID-UNIQUE core-2.01.pdf#page=304&T243: row 2 \"Admins | Base\": the UID 00 00 00 09 00 00 00 01 is also that of row 1" ]
      it "fail on two rows that give one name, and leave both out of the module" $ do
        let twice = setCell "T243" 1 1 "Anybody" ir
        errorsOf twice `shouldBe`
          [ "C-UID-NAME core-2.01.pdf#page=304&T243: row 2 \"Anybody | Base\": it is generated as uAuthority_Anybody, as T243 row 1 is" ]
        newlyExcluded ir twice `shouldReturn`
          [ (TableId 243, "row 1 \"Anybody | Base\": C-UID-NAME failed"), (TableId 243, "row 2 \"Anybody | Base\": C-UID-NAME failed") ]

    describe "N-UID-SHARED" $ do
      it "finds the one UID two of the tables print: Table 239's GenKey CharSet value is Table 245's C_PIN SID, a Note at the later row" $
        [ renderFinding f | f <- checkIR Nothing Scott ir, Cm.code f == "N-UID-SHARED" ] `shouldBe`
          [ "N-UID-SHARED core-2.01.pdf#page=305&T245: row 1 \"C_PIN | SID\": the UID 00 00 00 0B 00 00 00 01 is also that of T239 row 5 (PDF 302), "
            <> "\"Used in the C_PIN table's CharSet column to indicate that the GenKey character set is not restricted (all byte values are legal).\"" ]
      it "finds another, and leaves one UID printed twice in one table to C-UID-UNIQUE" $ do
        [ renderFinding f | f <- checkIR Nothing Scott (setCell "T246" 0 0 "00 00 00 09 00 00 00 01" ir), Cm.code f == "N-UID-SHARED"
                          , Cm.anchor f == Just "T246" ] `shouldBe`
          [ "N-UID-SHARED core-2.01.pdf#page=305&T246: row 1 \"Base\": the UID 00 00 00 09 00 00 00 01 is also that of T243 row 1 (PDF 304), \"Anybody | Base\"" ]
        [ Cm.code f | f <- checkIR Nothing Scott (setCell "T243" 1 0 "00 00 00 09 00 00 00 01" ir), Cm.code f `elem` ["N-UID-SHARED", "C-UID-UNIQUE"]
                    , Cm.anchor f == Just "T243" ] `shouldBe` ["C-UID-UNIQUE"]

    describe "N-UID-UNNAMED" $
      it "lists the rows that name no definition, once per table" $
        [ renderFinding f | f <- checkIR Nothing Scott ir, Cm.code f == "N-UID-UNNAMED", Cm.anchor f == Just "T242" ] `shouldBe`
          [ "N-UID-UNNAMED core-2.01.pdf#page=303&T242: rows uidTable gives no name (listed in methodIDUIDs where their UID is eight bytes): "
            <> "6 \"OBSOLETE *\"; 7 \"OBSOLETE *\"; 12 \"OBSOLETE *\"; 17 \"Reserved for SSC Usage\"; 24 \"Reserved for SSC Usage\"; "
            <> "25 \"Reserved for SSC Usage\"; 54 \"Reserved for SSC Usage\"" ]

    describe "the generated module" $ do
      cr <- runIO (either (throwString . show) pure (compileIR Nothing Scott ir))
      let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.UIDs" (crModules cr)))
          uids = [ T.takeWhile (/= ' ') l | l <- lns, " :: UID" `T.isSuffixOf` l ]
      it "names every named row: 6 + 47 + 17 + 4 + 5 + 6 + 6 + 30 UIDs, beside the sig rule's two invoking IDs" $ do
        length uids `shouldBe` 121 + 2
        filter (`elem` uids) ["uProperties", "uCloseSession", "uMethodID_DeleteSP"] `shouldBe` ["uProperties", "uCloseSession", "uMethodID_DeleteSP"]
        filter (`elem` uids) [ "uMethodID_Get", "uAuthority_SID", "uSPInfo_Row", "uC_PIN_SID", "uLocking_GlobalRange"
                             , "uTemplate_Locking", "uSPTemplates_Base", "uSecretProtect_C_PIN_0x03" ]
          `shouldBe` [ "uMethodID_Get", "uAuthority_SID", "uSPInfo_Row", "uC_PIN_SID", "uLocking_GlobalRange"
                     , "uTemplate_Locking", "uSPTemplates_Base", "uSecretProtect_C_PIN_0x03" ]
      it "gives each UID the page its row is printed on" $ do
        lns `shouldContain`
          [ "-- | core-2.01 T242 MethodID UIDs (PDF 303 / p.286, ord 244, sha256:e895ef45fc8d), row 16: GenKey | Base"
          , "uMethodID_GenKey :: UID", "uMethodID_GenKey = uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x10" ]
        lns `shouldContain`
          [ "-- | core-2.01 T242 MethodID UIDs (PDF 304 / p.287, ord 244, sha256:e895ef45fc8d), row 18: GetPackage | Base"
          , "uMethodID_GetPackage :: UID", "uMethodID_GetPackage = uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x12" ]
      it "lists every table's UIDs with its rows' text, the pattern left out" $ do
        [ T.takeWhile (/= ' ') l | l <- lns, " :: [(UID, String)]" `T.isSuffixOf` l ] `shouldBe`
          [ "specialPurposeUIDs", "sessionManagerMethodUIDs", "methodIDUIDs", "authorityUIDs", "singleRowTableRowUIDs"
          , "tableDefaultRows", "templateTableUIDs", "spTemplatesTableUIDs", "secretProtectTableUIDs" ]
        lns `shouldContain` ["  [ (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00, \"Used to represent null uid\")"]
        length [ l | l <- lns, "(uid 0x00 0x00 0x00 0x06 " `T.isInfixOf` l ] `shouldBe` 54
