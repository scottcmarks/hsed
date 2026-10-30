{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.SigSpec
Description : Test specifications for the sig rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The sig rule on the committed Core extraction with the committed annotations
(design section 3.4, example E9, M2b): it binds the 53 signature blocks by the
doc anchor's @rule sig@ and parses every one (the heads, required parameters
positional, optional ones Named, the response; the Session Manager's
invocations and references); it joins 52 of them to their UIDs in the
Session Manager Method UIDs and the MethodID UIDs by name, and the one no row
names (5.6.4.2 Stir) is a C-SIG-METHOD Warning with its page; the generated
module has a record per method, its signature and, when joined, its UID.
-}
module Hsed.Spec.Rules.SigSpec (spec) where

import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor         (AnchorId (..))
import           Hsed.Spec.Check          (checkIR, mkContext)
import           Hsed.Spec.Common         (ErrataSet (..), RuleName (..), Severity (..), renderFinding)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.Compile        (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL            (SomeBinding (..), sig)
import           Hsed.Spec.IR             (SigIR (..), SpecIR (..))
import           Hsed.Spec.RuleDef        (BoundSig (..), Classification (..), Context)
import           Hsed.Spec.Rules          (classifySignatures, explain)
import           Hsed.Spec.Rules.Sig
import           Hsed.Spec.Rules.UidTable (UidRow (..))


-- | The IR with one signature block's text replaced.
withSigText :: Text -> Text -> SpecIR -> SpecIR
withSigText section text ir = ir { sigs = [ if s.section == section then s { text = text } else s | s <- ir.sigs ] }

-- | The IR with one signature block's heads and text replaced.
withSig :: Text -> [Text] -> Text -> SpecIR -> SpecIR
withSig section heads text ir = ir { sigs = [ if s.section == section then s { heads = heads, text = text } else s | s <- ir.sigs ] }

-- | The block of a section, read.
readOf :: Context -> Text -> Maybe SigRead
readOf ctx section = listToMaybe [ sr | Right sr <- sigReads ctx, bsAnchor (srBound sr) == SigId section ]

sigFindings :: SpecIR -> [Text]
sigFindings = map renderFinding . filter (\f -> any (`T.isPrefixOf` Cm.code f) ["C-SIG", "N-SIG"]) . checkIR Nothing Scott

spec :: Spec
spec = do
  describe "parseBlock" $ do
    it "reads two heads, each with its own bracket, and one list" $
      fmap fst (parseBlock "TableUID.Get [\nObjectUID.Get [\nCellblock : cell_block ]\n=>\n[ Result : bytes ]")
        `shouldBe` Right ( Part [("TableUID", "Get"), ("ObjectUID", "Get")] (Params [SigParam "Cellblock" "cell_block"] [])
                         , Just (Part [] (Params [SigParam "Result" "bytes"] [])) )
    it "reads a head without a bracket, optional parameters, and commas inside braces" $
      fmap (fst . fst) (parseBlock "CredentialObjectUID.Sign\nHashObjectUID.Sign[\nInput = typeOr { Data : bytes, Buffer : cell_block },\nBufferOut = cell_block ]")
        `shouldBe` Right (Part [("CredentialObjectUID", "Sign"), ("HashObjectUID", "Sign")]
                               (Params [] [SigParam "Input" "typeOr { Data : bytes, Buffer : cell_block }", SigParam "BufferOut" "cell_block"]))
    it "reads a response that invokes a method, and a list that is a reference" $
      fmap (snd . fst) (parseBlock "SMUID.StartSession [\nHostSessionID : uinteger ]\n=>\nSMUID.SyncSession [ see SyncSession definition in 5.2.3.2]")
        `shouldBe` Right (Just (Part [("SMUID", "SyncSession")] (SeeRef "SyncSession" (Just "5.2.3.2"))))
    it "reads an unclosed list as closed at =>, a Warning, and a comma before ] as a Note" $ do
      fmap snd (parseBlock "C.GetPackage [\nPurpose : package_purpose,\nLog = bytes\n=>\n[ Result : package ]")
        `shouldBe` Right [(Warning, "C-SIG-PARSE", "the invocation's parameter list is not closed before =>: read as closed there")]
      fmap snd (parseBlock "C.SetPackage [\nValue : package,\n]\n=>\n[ ]")
        `shouldBe` Right [(Note, "N-SIG-NORMALISED", "the invocation's parameter list ends with a comma before ]: read without it")]
    it "refuses a parameter that is not Name : type or Name = type, and a required one after an optional one" $ do
      fmap fst (parseBlock "ThisSP.X [ Count uinteger ]") `shouldBe` Left "the invocation: \"Count uinteger\" is not Name : type or Name = type"
      fmap fst (parseBlock "ThisSP.X [ A = bytes, B : bytes ]")
        `shouldBe` Left "the invocation: the required parameter B follows an optional one (3.2.4.1)"

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ctx = mkContext Scott ir
        reads = sigReads ctx
        rows = rights reads
        joined sr = [ urRow r + 1 | (n, r) <- methodRows ctx, n == srMethod sr ]

    describe "the sig rule" $ do
      it "binds the 53 signature blocks by the doc anchor's rule token" $ do
        L.nub (Map.elems (fst (classifySignatures ir))) `shouldBe` [RuleBound (RuleName "sig") [] (SomeBinding sig)]
        Map.size (fst (classifySignatures ir)) `shouldBe` 53
        explain ir (SigId "5.3.3.6") `shouldBe` "sig (exemplar core 5.3.3.6/sig)"
      it "parses every one" $ do
        lefts reads `shouldBe` []
        length rows `shouldBe` 53
      it "names the method each block defines, the one its heading names" $
        map srMethod (take 6 rows) `shouldBe` ["Properties", "StartSession", "SyncSession", "StartTrustedSession", "SyncTrustedSession", "CloseSession"]
      it "reads Get as E9 does: two invokers, one required parameter, a typeOr result" $
        fmap (\sr -> (srInvokers sr, srRequired sr, srOptional sr, srResponse sr)) (readOf ctx "5.3.3.6") `shouldBe` Just
          ( ["TableUID", "ObjectUID"], [SigParam "Cellblock" "cell_block"], []
          , SResults [SigParam "Result" "typeOr { Bytes : bytes, RowValues : list [ ColumnNumber = Value ... ] }"] [] )
      it "reads StartSession's three required and nine optional parameters, answered by SyncSession" $
        fmap (\sr -> (length (srRequired sr), length (srOptional sr), srResponse sr)) (readOf ctx "5.2.3.1")
          `shouldBe` Just (3, 9, SInvocationOf "SyncSession")
      it "reads SyncSession from the response of its block, the answer to StartSession" $
        fmap (\sr -> (map spName (srRequired sr), srResponse sr, srAnswers sr)) (readOf ctx "5.2.3.2")
          `shouldBe` Just (["HostSessionID", "SPSessionID"], SNoResponse, Just "StartSession")
      it "reads Properties' response, an invocation of Properties, and CloseSession's absence of one" $ do
        fmap srResponse (readOf ctx "5.2.2.1") `shouldBe` Just
          (SInvocation "Properties" [SigParam "Properties" "list [ name = value ... ]"] [SigParam "HostProperties" "list [ name = value ... ]"])
        fmap srResponse (readOf ctx "5.2.3.5") `shouldBe` Just SNoResponse

    describe "C-SIG-METHOD (acceptance: signatures joined to T242/T241)" $ do
      it "joins 52 blocks, each to one row: 6 of T241 and 46 of T242" $ do
        length [ sr | sr <- rows, [_] <- [joined sr] ] `shouldBe` 52
        L.sort (L.nub [ Cm.code f | f <- checkIR Nothing Scott ir, Cm.code f == "C-SIG-METHOD", Cm.severity f == Error ]) `shouldBe` []
      it "reports the one block no row names with its page, and the one method row no block defines" $
        filter ("C-SIG-METHOD" `T.isPrefixOf`) (sigFindings ir) `shouldBe`
          [ "C-SIG-METHOD core-2.01.pdf#page=252&5.6.4.2/sig: Stir has no UID: no row of T241 or T242 names it"
          , "C-SIG-METHOD core-2.01.pdf#page=304&T242: T242 row 34, Salt, has no signature block" ]
      it "is an Error when two rows name one method" $
        [ renderFinding f | f <- checkIR Nothing Scott (setCell "T242" 33 1 "Get" ir), Cm.code f == "C-SIG-METHOD", Cm.severity f == Error ]
          `shouldBe` ["C-SIG-METHOD core-2.01.pdf#page=201&5.3.3.6/sig: Get is named by T242 row 20 and T242 row 34"]

    describe "C-SIG-PARSE" $ do
      it "reads GetPackage's unclosed list and SetPackage's last comma, and nothing else is amiss" $
        filter (not . ("C-SIG-METHOD" `T.isPrefixOf`)) (sigFindings ir) `shouldBe`
          [ "C-SIG-PARSE core-2.01.pdf#page=208&5.3.3.17/sig: the invocation's parameter list is not closed before =>: read as closed there"
          , "N-SIG-NORMALISED core-2.01.pdf#page=209&5.3.3.18/sig: the invocation's parameter list ends with a comma before ]: read without it" ]
      it "is an Error at the block when a block does not parse, and the block is left out" $ do
        let broken = withSigText "5.3.3.8" "TableUID.Next [\nWhere uidref ]\n=>\n[ Result : list [ uidref ... ] ]" ir
        [ renderFinding f | f <- checkIR Nothing Scott broken, Cm.code f == "C-SIG-PARSE", Cm.severity f == Error ] `shouldBe`
          ["C-SIG-PARSE core-2.01.pdf#page=203&5.3.3.8/sig: the invocation: \"Where uidref\" is not Name : type or Name = type"]
      it "is an Error when the heads read are not the IR's, or when the heading names neither method of the block" $ do
        [ renderFinding f | f <- checkIR Nothing Scott (withSigText "5.5.4.2" "ClockTimeTableUID.Reset [ ]\n=>\n[ ]" ir), Cm.code f == "C-SIG-PARSE", Cm.severity f == Error ]
          `shouldBe` ["C-SIG-PARSE core-2.01.pdf#page=240&5.5.4.2/sig: the heads read are [(\"ClockTimeTableUID\",\"Reset\")], the IR's [\"ClockTimeTableUID.ResetClock\"]"]
        [ renderFinding f | f <- checkIR Nothing Scott (withSig "5.5.4.2" ["ClockTimeTableUID.Reset"] "ClockTimeTableUID.Reset [ ]\n=>\n[ ]" ir), Cm.code f == "C-SIG-PARSE", Cm.severity f == Error ]
          `shouldBe` ["C-SIG-PARSE core-2.01.pdf#page=240&5.5.4.2/sig: the heading \"ResetClock (Table Method)\" names neither Reset"]

    describe "the invoking IDs" $
      it "finds SMUID and ThisSP word for word in the Special Purpose UIDs, and no other invoker" $ do
        [ (i, map (\r -> urRow r + 1) (invokerRows ctx i)) | i <- ["SMUID", "ThisSP", "TableUID", "ObjectUID"] ]
          `shouldBe` [("SMUID", [3]), ("ThisSP", [2]), ("TableUID", []), ("ObjectUID", [])]

    describe "the generated module" $ do
      cr <- runIO (either (throwString . show) pure (compileIR Nothing Scott ir))
      let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Methods" (crModules cr)))
      it "has a record per block, a signature instance per block, and a Method instance per joined block" $ do
        length [ l | l <- lns, "data " `T.isPrefixOf` l ] `shouldBe` 53
        length [ l | l <- lns, "instance MethodSignature " `T.isPrefixOf` l ] `shouldBe` 53
        length [ l | l <- lns, "instance Method " `T.isPrefixOf` l ] `shouldBe` 52
        lns `shouldContain` ["-- no instance Method Stir: no one row of the UID tables names it (C-SIG-METHOD)"]
      it "gives Get its field and its UID as E9 does" $ do
        lns `shouldContain` [ "-- | core-2.01 5.3.3.6/sig TableUID.Get, ObjectUID.Get (PDF 201 / p.184, ord 12, sha256:9d67d740e4cb)"
                            , "data Get = Get", "  { getCellblock :: Value", "  }", "  deriving (Eq, Show)" ]
        lns `shouldContain` [ "    , sigInvokers = [\"TableUID\", \"ObjectUID\"]", "    , sigRequired = [Param \"Cellblock\" \"cell_block\"]" ]
        lns `shouldContain` [ "instance Method Get where", "  methodUID = uMethodID_Get" ]
      it "makes optional parameters Maybe fields, in the signature's order" $
        lns `shouldContain` [ "data Set = Set", "  { setWhere :: Maybe Value", "  , setValues :: Maybe Value", "  }" ]
      it "says which method a block defines when its heads name another (SyncSession's, headed StartSession)" $ do
        lns `shouldSatisfy` any ("-- | core-2.01 5.2.3.2/sig SMUID.StartSession => SyncSession (PDF " `T.isPrefixOf`)
        lns `shouldSatisfy` any ("-- | core-2.01 5.3.3.6/sig TableUID.Get, ObjectUID.Get (PDF " `T.isPrefixOf`)
      it "gives the invoking IDs SMUID and ThisSP as constants, from Table 239, in the UIDs module beside its other rows" $ do
        let uids = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.UIDs" (crModules cr)))
        uids `shouldContain` ["uThisSP :: UID", "uThisSP = uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x01"]
        uids `shouldContain` ["uSMUID :: UID", "uSMUID = uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0xff"]
        filter (\l -> "uThisSP" `T.isPrefixOf` l || "uSMUID" `T.isPrefixOf` l) lns `shouldBe` []
