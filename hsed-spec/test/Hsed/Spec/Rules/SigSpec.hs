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
Since M4a (the plan's answer 2) the type join: every parameter's type text
parses, its leaves resolve against the 100 type rows, the 18 abstract types
read from 5.1.4.2's headings and Table 240's 46 tables, the ten braced
placeholders nothing defines are C-SIG-TYPE Warnings at their blocks and
the five types the spec leaves to context (Properties' @value@, Get's
@Value@, Set's @\<type of column\>@) N-SIG-TYPE-CONTEXT Notes, each with its
page; and nothing generated changes.
-}
module Hsed.Spec.Rules.SigSpec (spec) where

import qualified Data.Text                as DT
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

-- | The type join's findings alone, of one code.
typeFindings :: Text -> SpecIR -> [Text]
typeFindings code = map renderFinding . filter ((== code) . Cm.code) . checkIR Nothing Scott

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

  describe "parseType" $ do
    it "reads a name, uidref with and without a table, a list, a Named list, typeOr and a type in angle brackets, the spacing free" $ do
      parseType "uinteger" `shouldBe` Right (TName "uinteger")
      parseType "uidref" `shouldBe` Right (TUidref Nothing)
      parseType "uidref {SPObjectUID}" `shouldBe` Right (TUidref (Just "SPObjectUID"))
      parseType "uidref { SP/table/object }" `shouldBe` Right (TUidref (Just "SP/table/object"))
      parseType "list [ uidref ... ]" `shouldBe` Right (TList Nothing (TUidref Nothing))
      parseType "list [ name = value ... ]" `shouldBe` Right (TList (Just "name") (TName "value"))
      parseType "typeOr { Data : bytes, Buffer : cell_block}" `shouldBe` Right (TOr [("Data", TName "bytes"), ("Buffer", TName "cell_block")])
      parseType "typeOr { Bytes : bytes, RowValues : list [ ColumnNumber = <type of column> ... ] }"
        `shouldBe` Right (TOr [("Bytes", TName "bytes"), ("RowValues", TList (Just "ColumnNumber") (TContext "type of column"))])
    it "refuses what is no type" $ do
      parseType "list [ uidref ]" `shouldSatisfy` isLeft
      parseType "typeOr { Data bytes }" `shouldSatisfy` isLeft
      parseType "uinteger extra" `shouldSatisfy` isLeft
      parseType "" `shouldSatisfy` isLeft
    it "lists a type's leaves with their roles: a name, a uidref's table, a Named list's value type, a type said in words" $ do
      typeLeaves (TOr [("Bytes", TName "bytes"), ("RowValues", TList (Just "ColumnNumber") (TName "Value"))]) `shouldBe` [(NameLeaf, "bytes"), (ElementLeaf, "Value")]
      typeLeaves (TList Nothing (TUidref Nothing)) `shouldBe` [(NameLeaf, "uidref")]
      typeLeaves (TList (Just "n") (TContext "type of column")) `shouldBe` [(ContextLeaf, "type of column")]
      typeLeaves (TUidref (Just "ByteTable")) `shouldBe` [(TargetLeaf, "ByteTable")]

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
      it "reads GetPackage's unclosed list and SetPackage's last comma, and nothing else is amiss (the type join's findings aside)" $
        filter (\l -> not (any (`T.isPrefixOf` l) ["C-SIG-METHOD", "C-SIG-TYPE", "N-SIG-TYPE-CONTEXT"])) (sigFindings ir) `shouldBe`
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

    describe "the type join (C-SIG-TYPE, N-SIG-TYPE-CONTEXT; the M4a plan's answer 2)" $ do
      let env = typeEnv ctx
          typeTexts = L.nub [ spType p | sr <- rows, (_, p) <- paramsOf sr ]
      it "resolves against the 100 type rows, the 18 abstract types read from 5.1.4.2's headings (PDF 128-136), and Table 240's 46 tables" $ do
        length (teTypeRows env) `shouldBe` 100
        teAbstract env `shouldBe` [ "access_control_list", "boolean", "cell_block", "clock_kind", "clock_time", "columns", "date", "hash_protocol"
                                  , "key_size", "lag", "name", "package", "package_purpose", "row_address", "row_data", "table_kind", "table_sizes", "uidref" ]
        length (teTables env) `shouldBe` 46
        teTablesAt env `shouldBe` ["T240"]
      it "parses every distinct type text of the 53 blocks: 39, over 147 parameters and results" $ do
        length typeTexts `shouldBe` 39
        length [ p | sr <- rows, (_, p) <- paramsOf sr ] `shouldBe` 147
        [ (t, why) | t <- typeTexts, Left why <- [parseType t] ] `shouldBe` []
      it "lets a type row win over an abstract type of the same name, joins a uidref's table less its suffix, and leaves a Named list's value type to the context" $ do
        resolveLeaf env NameLeaf "boolean" `shouldBe` Resolved ATypeRow
        resolveLeaf env NameLeaf "cell_block" `shouldBe` Resolved AnAbstractType
        resolveLeaf env NameLeaf "uidref" `shouldBe` Resolved AnAbstractType
        resolveLeaf env TargetLeaf "SPObjectUID" `shouldBe` Resolved (ATable "SP")
        resolveLeaf env TargetLeaf "MethodID" `shouldBe` Resolved (ATable "MethodID")
        resolveLeaf env TargetLeaf "ByteTable" `shouldBe` Unresolved "no row of T240 names a table \"ByteTable\""
        resolveLeaf env ElementLeaf "value" `shouldBe` ContextTyped
        resolveLeaf env ElementLeaf "uinteger" `shouldBe` Resolved ATypeRow
        resolveLeaf env ContextLeaf "type of column" `shouldBe` ContextTyped
        resolveLeaf env NameLeaf "TemplateObjectUID" `shouldBe` Unresolved "no type row and no abstract type (5.1.4.2) is named \"TemplateObjectUID\""
      it "gives exactly the ten C-SIG-TYPE Warnings, at their blocks with their pages: the braced placeholders and IssueSP's Templates" $
        typeFindings "C-SIG-TYPE" ir `shouldBe`
          [ "C-SIG-TYPE core-2.01.pdf#page=205&5.3.3.11/sig: the parameter InvokingID: the type \"uidref { SP/table/object }\" names \"SP/table/object\", which nothing defines: no row of T240 names a table \"SP/table/object\""
          , "C-SIG-TYPE core-2.01.pdf#page=206&5.3.3.13/sig: the parameter InvokingID: the type \"uidref { SP/table/object }\" names \"SP/table/object\", which nothing defines: no row of T240 names a table \"SP/table/object\""
          , "C-SIG-TYPE core-2.01.pdf#page=206&5.3.3.14/sig: the parameter InvokingID: the type \"uidref { SP/table/object }\" names \"SP/table/object\", which nothing defines: no row of T240 names a table \"SP/table/object\""
          , "C-SIG-TYPE core-2.01.pdf#page=207&5.3.3.15/sig: the parameter InvokingID: the type \"uidref { SP/table/object }\" names \"SP/table/object\", which nothing defines: no row of T240 names a table \"SP/table/object\""
          , "C-SIG-TYPE core-2.01.pdf#page=208&5.3.3.17/sig: the parameter SigningKey: the type \"uidref { CredentialObjectUID }\" names \"CredentialObjectUID\", which nothing defines: no row of T240 names a table \"Credential\" (\"CredentialObjectUID\" less its suffix)"
          , "C-SIG-TYPE core-2.01.pdf#page=208&5.3.3.17/sig: the parameter WrappingKey: the type \"uidref { CredentialObjectUID }\" names \"CredentialObjectUID\", which nothing defines: no row of T240 names a table \"Credential\" (\"CredentialObjectUID\" less its suffix)"
          , "C-SIG-TYPE core-2.01.pdf#page=209&5.3.3.18/sig: the parameter SigningKey: the type \"uidref { CredentialObjectUID }\" names \"CredentialObjectUID\", which nothing defines: no row of T240 names a table \"Credential\" (\"CredentialObjectUID\" less its suffix)"
          , "C-SIG-TYPE core-2.01.pdf#page=209&5.3.3.18/sig: the parameter WrappingKey: the type \"uidref { CredentialObjectUID }\" names \"CredentialObjectUID\", which nothing defines: no row of T240 names a table \"Credential\" (\"CredentialObjectUID\" less its suffix)"
          , "C-SIG-TYPE core-2.01.pdf#page=234&5.4.3.1/sig: the parameter Templates: the type \"list [ TemplateObjectUID ... ]\" names \"TemplateObjectUID\", which nothing defines: no type row and no abstract type (5.1.4.2) is named \"TemplateObjectUID\""
          , "C-SIG-TYPE core-2.01.pdf#page=263&5.6.4.17/sig: the parameter PatternInput: the type \"uidref {ByteTable}\" names \"ByteTable\", which nothing defines: no row of T240 names a table \"ByteTable\"" ]
      it "gives exactly the five N-SIG-TYPE-CONTEXT Notes: Properties' value three times, Get's Value, Set's <type of column>" $
        typeFindings "N-SIG-TYPE-CONTEXT" ir `shouldBe`
          [ "N-SIG-TYPE-CONTEXT core-2.01.pdf#page=140&5.2.2.1/sig: the parameter HostProperties: the type of each named value, \"value\", is the context's, given by the name beside it; no type row or abstract type is so named"
          , "N-SIG-TYPE-CONTEXT core-2.01.pdf#page=140&5.2.2.1/sig: the response's parameter HostProperties: the type of each named value, \"value\", is the context's, given by the name beside it; no type row or abstract type is so named"
          , "N-SIG-TYPE-CONTEXT core-2.01.pdf#page=140&5.2.2.1/sig: the response's parameter Properties: the type of each named value, \"value\", is the context's, given by the name beside it; no type row or abstract type is so named"
          , "N-SIG-TYPE-CONTEXT core-2.01.pdf#page=201&5.3.3.6/sig: the result Result: the type of each named value, \"Value\", is the context's, given by the name beside it; no type row or abstract type is so named"
          , "N-SIG-TYPE-CONTEXT core-2.01.pdf#page=202&5.3.3.7/sig: the parameter Values: the type <type of column> is the context's, said in words rather than named" ]
      it "is a Warning at the block when a type text does not read as a type" $
        typeFindings "C-SIG-TYPE" (withSigText "5.3.3.8" "TableUID.Next [\nWhere = uidref,\nCount = list [ uinteger ] ]\n=>\n[ Result : list [ uidref ... ] ]" ir)
          `shouldSatisfy` any (T.isPrefixOf "C-SIG-TYPE core-2.01.pdf#page=203&5.3.3.8/sig: the parameter Count: the type \"list [ uinteger ]\" does not read as a type: list without ... at")
      it "joins each block's method to its UID: Get's uMethodID_Get, none for Stir" $ do
        (readOf ctx "5.3.3.6" >>= joinedUID ctx) `shouldBe` Just "uMethodID_Get"
        (readOf ctx "5.6.4.2" >>= joinedUID ctx) `shouldBe` Nothing

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
        lns `shouldContain` [ "    , sigInvokers = [\"TableUID\", \"ObjectUID\"]", "    , sigRequired = [Param \"Cellblock\" (TypeName \"cell_block\" Nothing)]" ]
        lns `shouldContain` [ "  fromValues [r1] [] = Just (Get r1)", "  fromValues _ _ = Nothing" ]
        lns `shouldContain` [ "instance Method Get where", "  methodUID = uMethodID_Get" ]
      it "types each parameter by name, with its Type object's UID where the whole text is a type row's name: 90 of 147" $ do
        let text = fromMaybe "" (lookup "Hsed.Gen.Core.Methods" (crModules cr))
        DT.count "Param \"" text `shouldBe` 147
        DT.count "\" (Just u" text `shouldBe` 90
        DT.count "\" Nothing)" text `shouldBe` 57
        lns `shouldContain` [ "        [ Param \"HostSessionID\" (TypeName \"uinteger\" (Just uuintegerType))"
                            , "        , Param \"SPID\" (TypeName \"uidref {SPObjectUID}\" Nothing)" ]
        lns `shouldContain` [ "import           Hsed.Core.TypeName  (TypeName (..))" ]
        lns `shouldSatisfy` any (\l -> "import           Hsed.Gen.Core.Types" `T.isPrefixOf` l && "ubooleanType, ubytesType" `T.isInfixOf` l && "uuintegerType" `T.isInfixOf` l)
      it "gives every record fromValues, the record from values of its signature's arity, a wrapped line where it is long" $ do
        length [ l | l <- lns, "  fromValues [" `T.isPrefixOf` l ] `shouldBe` 53
        length [ l | l <- lns, l == "  fromValues _ _ = Nothing" ] `shouldBe` 53
        lns `shouldContain` [ "  fromValues [] [] = Just DeleteSP" ]
        lns `shouldContain` [ "  fromValues [r1, r2, r3] [o1, o2, o3, o4, o5, o6, o7, o8, o9] =", "    Just (StartSession r1 r2 r3 o1 o2 o3 o4 o5 o6 o7 o8 o9)" ]
      it "lists the 52 methods with a UID, Stir left out, under the first block's provenance, last in the module" $ do
        lns `shouldContain` [ "methods :: [SomeMethod]", "methods =", "  [ SomeMethod (Proxy :: Proxy Properties)", "  , SomeMethod (Proxy :: Proxy StartSession)" ]
        length [ l | l <- lns, "SomeMethod (Proxy :: Proxy " `T.isInfixOf` l ] `shouldBe` 52
        filter ("Proxy Stir" `T.isInfixOf`) lns `shouldBe` []
        lns `shouldSatisfy` any (\l -> "-- | core-2.01 5.2.2.1/sig SMUID.Properties (PDF 140" `T.isPrefixOf` l && "every signature block after it" `T.isInfixOf` l)
        L.lastMaybe [ l | l <- lns, " :: " `T.isInfixOf` l, not (" " `T.isPrefixOf` l) ] `shouldBe` Just "methods :: [SomeMethod]"
        lns `shouldContain` [ "import           Data.Proxy          (Proxy (..))" ]
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
