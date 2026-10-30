{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Hsed.Spec.Rules.PropsReqSpec
Description : Test specifications for the propsReq reader
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The propsReq reader on the committed Opal extraction, Table 17 bound in
memory and read over Core's Context, the base (design example E14; M4a):
the 11 rows, each property Core Table 167's, the TPer requirements' minima
and the one VU, the six host requirements with their initial assumptions,
the one (N) and the four N/A; the cell grammar on its own; the checks, each
made to fire by an edit of a cell and the table then left out; read alone,
without the base, every row names no property; and the generated module,
@Hsed.Gen.Opal.Properties@, with Core Table 168's assumptions patched by
name (2048, 2028, 1992 over 1024, 1004, 968; the 1s kept; the host's row as
it stands) and the 11 requirements, type-checked under @-Wall -Werror@.
-}
module Hsed.Spec.Rules.PropsReqSpec (spec) where

import qualified Data.Text               as DT
import qualified RIO.Map                 as Map
import qualified RIO.Text                as T
import           RIO
import           RIO.Directory           (createDirectoryIfMissing)
import           RIO.FilePath            (takeDirectory, (</>))
import           System.Environment      (lookupEnv)
import           System.Process          (readProcessWithExitCode)
import           Test.Hspec

import           Hsed.Core.Properties    (HostRequirement (..), Requirement (..), TPerRequirement (..))
import           Hsed.Spec.Anchor        (AnchorId (..))
import           Hsed.Spec.Check         (checkIR, mkContext, mkContextWith)
import           Hsed.Spec.Common        (ErrataSet (..), renderFinding)
import qualified Hsed.Spec.Common        as Cm
import           Hsed.Spec.Compile       (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL           (SomeBinding (..), propsReq)
import           Hsed.Spec.IR            (SpecIR (..))
import           Hsed.Spec.Registry      (registry)
import           Hsed.Spec.RuleDef       (BoundTable (..), Classification (..), Context, RuleDef (..))
import           Hsed.Spec.Rules         (classify)
import           Hsed.Spec.Rules.PropsReq


-- | The codes the reader gives: its own, and the Props reader's W-PROP-NAME-CASE.
ownCodes :: [Text]
ownCodes = map Cm.codeName (ruleCodes propsReqRule) <> ["W-PROP-NAME-CASE"]

-- | @ghc -fno-code -Wall -Werror@ on a printed module against hsed-core (as FeatureSpec does).
compiles :: Text -> Text -> Expectation
compiles modName text = withSystemTempDirectory "hsed-propsreq" $ \dir -> do
  let file = dir </> T.unpack (DT.replace "." "/" modName) <> ".hs"
      ghcArgs = ["-fno-code", "-Wall", "-Werror", "-package", "hsed-core", "-i" <> dir, file]
  createDirectoryIfMissing True (takeDirectory file)
  writeFileUtf8 file text
  underStack <- isJust <$> lookupEnv "GHC_PACKAGE_PATH"
  let (prog, args) = if underStack then ("ghc", ghcArgs) else ("stack", ["exec", "--", "ghc"] <> ghcArgs)
  result <- try (readProcessWithExitCode prog args "")
  case result of
    Left (e :: IOException)         -> pendingWith ("cannot run " <> prog <> " here: " <> show e)
    Right (ExitSuccess, _, _)       -> pure ()
    Right (ExitFailure _, out, err) -> expectationFailure (prog <> " rejected the module:\n" <> out <> err)

spec :: Spec
spec = do
  describe "the cell grammar" $ do
    it "reads a TPer cell: a requirement and its minimum, VU, or N/A" $ do
      readTPerCell "(M) 2048 minimum" `shouldBe` Right (Just (TPerRequirement Mandatory (Just 2048)))
      readTPerCell "(O) 2 minimum" `shouldBe` Right (Just (TPerRequirement Optional (Just 2)))
      readTPerCell "(M) VU" `shouldBe` Right (Just (TPerRequirement Mandatory Nothing))
      readTPerCell "N/A \x2013 not a TPer property" `shouldBe` Right Nothing
      readTPerCell "(M) 2048 maximum" `shouldBe` Left "expected a minimum (2048 minimum) or VU after (M)"
      readTPerCell "2048 minimum" `shouldBe` Left "expected (M), (O) or (N) first, or N/A"
    it "reads a host cell: a requirement with its initial assumption and bounds, (N), or N/A" $ do
      readHostCell "(M) Initial Assumption: 2048 Minimum allowed: 2048 Maximum allowed: VU"
        `shouldBe` Right (Just (HostRequirement Mandatory (Just 2048) (Just 2048) Nothing))
      readHostCell "(O) Initial Assumption: 1 Minimum allowed: 1 Maximum allowed: 4"
        `shouldBe` Right (Just (HostRequirement Optional (Just 1) (Just 1) (Just 4)))
      readHostCell "(N) Although this is a legal host property, the TPer MAY ignore it."
        `shouldBe` Right (Just (HostRequirement NotRequired Nothing Nothing Nothing))
      readHostCell "N/A \x2013 not a host property" `shouldBe` Right Nothing
      readHostCell "(M) Initial Assumption: 2048 Maximum allowed: VU"
        `shouldBe` Left "expected Initial Assumption: n Minimum allowed: n Maximum allowed: VU|n after (M)"
      readHostCell "(M) Initial Assumption: 2048 Minimum allowed: 2048 Maximum allowed: VU and more"
        `shouldBe` Left "expected Initial Assumption: n Minimum allowed: n Maximum allowed: VU|n after (M)"
    it "reads the requirement word and the labelled values" $ do
      readRequirementWord "(N) rest of it" `shouldBe` Just (NotRequired, "rest of it")
      readRequirementWord "(X) rest" `shouldBe` Nothing
      labelled ["a:", "b:"] "a: 1 b: 2" `shouldBe` Just ["1", "2"]
      labelled ["a:", "b:"] "b: 2 a: 1" `shouldBe` Nothing
      labelled ["a:"] "a: 1 2" `shouldBe` Just ["1 2"]

  withCore $ \corePristine -> withOpal $ \opalPristine -> do
    hsa  <- runIO committedHsa
    ohsa <- runIO committedOpalHsa
    coreIR <- runIO (annotated corePristine hsa)
    -- the committed Opal sidecar with Table 17's line, whether it has it or not
    opal <- runIO (annotated opalPristine (withoutLine "T17" ohsa <> "opal-2.30 | T17 | propsReq\n"))
    let base = mkContext Scott coreIR
        overCore :: SpecIR -> Context
        overCore = mkContextWith registry (Just base) Scott
        octx = overCore opal
        reads' = requirementReads octx
        findings ir' = [ renderFinding f | f <- checkIR (Just base) Scott ir', Cm.code f `elem` ownCodes ]
        excludedOf ir' = do
          cr <- either (throwString . show) pure (compileIR (Just base) Scott ir')
          pure [ (aid, why) | (aid, why) <- crExcluded cr, aid == TableId 17 ]
        moduleOf ir' = do
          cr <- either (throwString . show) pure (compileIR (Just base) Scott ir')
          pure (fromMaybe "" (lookup "Hsed.Gen.Opal.Properties" (crModules cr)))

    describe "reading Table 17 over Core" $ do
      it "is bound by its comment, and read as the one propsReq table" $ do
        Map.lookup (TableId 17) (classify opal) `shouldBe` Just (CommentBound (SomeBinding propsReq))
        map (btAnchor . rqBound) (take 1 reads') `shouldBe` [TableId 17]
      it "reads its 11 rows, the properties in Table 17's order, each one of Core Table 167's" $ do
        map rqName reads' `shouldBe`
          [ "MaxComPacketSize", "MaxResponseComPacketSize", "MaxPacketSize", "MaxIndTokenSize", "MaxPackets", "MaxSubpackets"
          , "MaxMethods", "MaxSessions", "MaxAuthentications", "MaxTransactionLimit", "DefSessionTimeout" ]
        map rqRow reads' `shouldBe` [1 .. 11]
      it "reads the TPer requirements: every one mandatory, its minimum as printed, DefSessionTimeout's VU" $
        map rqTPer reads' `shouldBe` map (Right . Just . TPerRequirement Mandatory)
          [ Just 2048, Just 2048, Just 2028, Just 1992, Just 1, Just 1, Just 1, Just 1, Just 2, Just 1, Nothing ]
      it "reads the host requirements: six mandatory with their initial assumption and minimum, the maximum VU; one (N); four N/A" $ do
        let mandatory n = Right (Just (HostRequirement Mandatory (Just n) (Just n) Nothing))
        map rqHost reads' `shouldBe`
          [ mandatory 2048, Right (Just (HostRequirement NotRequired Nothing Nothing Nothing)), mandatory 2028, mandatory 1992
          , mandatory 1, mandatory 1, mandatory 1, Right Nothing, Right Nothing, Right Nothing, Right Nothing ]
        [ rqHostText r | r <- reads', rqHost r == Right Nothing ] `shouldBe` replicate 4 "N/A \x2013 not a host property"

    describe "the checks" $ do
      it "find nothing on Table 17 read over Core" $
        findings opal `shouldBe` []
      it "C-PROPSREQ-ROW: a TPer cell or a host cell that does not read, at the row's page, and the table left out" $ do
        let badTPer = setCell "T17" 0 1 "(M) 2048 maximum" opal
            badHost = setCell "T17" 7 2 "(M) Initial Assumption: 1 Maximum allowed: VU" opal
        findings badTPer `shouldBe`
          [ "C-PROPSREQ-ROW opal-2.30.pdf#page=33&T17: row 1 MaxComPacketSize: the TPer cell \"(M) 2048 maximum\" does not read:"
            <> " expected a minimum (2048 minimum) or VU after (M)" ]
        findings badHost `shouldBe`
          [ "C-PROPSREQ-ROW opal-2.30.pdf#page=34&T17: row 8 MaxSessions: the host cell \"(M) Initial Assumption: 1 Maximum allowed: VU\" does not read:"
            <> " expected Initial Assumption: n Minimum allowed: n Maximum allowed: VU|n after (M)" ]
        excludedOf badTPer `shouldReturn` [(TableId 17, "C-PROPSREQ-ROW failed")]
      it "C-PROPSREQ-NAME: a name Core Table 167 does not give, a requirement of a communicator the property does not describe, N/A for one it does" $ do
        findings (setCell "T17" 0 0 "MaxFoo" opal) `shouldBe`
          [ "C-PROPSREQ-NAME opal-2.30.pdf#page=33&T17: row 1 MaxFoo: names no property of core-2.01 Table 167" ]
        findings (setCell "T17" 7 2 "(M) Initial Assumption: 1 Minimum allowed: 1 Maximum allowed: VU" opal) `shouldBe`
          [ "C-PROPSREQ-NAME opal-2.30.pdf#page=34&T17: row 8 MaxSessions: a host requirement, but core-2.01 Table 167 says MaxSessions does not describe the Host" ]
        findings (setCell "T17" 0 2 "N/A - not a host property" opal) `shouldBe`
          [ "C-PROPSREQ-NAME opal-2.30.pdf#page=33&T17: row 1 MaxComPacketSize: N/A, but core-2.01 Table 167 says MaxComPacketSize describes the Host" ]
        findings (setCell "T17" 1 0 "MaxComPacketSize" opal) `shouldBe`
          [ "C-PROPSREQ-NAME opal-2.30.pdf#page=33&T17: row 2 MaxComPacketSize: the property is also row 1" ]
        excludedOf (setCell "T17" 0 0 "MaxFoo" opal) `shouldReturn` [(TableId 17, "C-PROPSREQ-NAME failed")]
      it "W-PROP-NAME-CASE: a name that is Core Table 167's only ignoring case is a Warning, and the property's own spelling is generated" $ do
        let folded = setCell "T17" 0 0 "maxcompacketsize" opal
        findings folded `shouldBe`
          [ "W-PROP-NAME-CASE opal-2.30.pdf#page=33&T17: row 1 maxcompacketsize: the property is MaxComPacketSize in core-2.01 Table 167, which is read" ]
        lns <- T.lines <$> moduleOf folded
        lns `shouldSatisfy` any (\l -> "  [ PropertyRequirement \"MaxComPacketSize\"  -- " `T.isPrefixOf` l && "; printed \"maxcompacketsize\"" `T.isSuffixOf` l)
        excludedOf folded `shouldReturn` []
      it "C-PATCH-MONOTONE: an initial assumption below Core Table 168's, the value it patches" $ do
        let low = setCell "T17" 2 2 "(M) Initial Assumption: 1000 Minimum allowed: 1000 Maximum allowed: VU" opal
        findings low `shouldBe`
          [ "C-PATCH-MONOTONE opal-2.30.pdf#page=33&T17: row 3 MaxPacketSize: Initial Assumption 1000 is below core-2.01 Table 168's 1004"
            <> " (Initial TPer Assumption About Host), the value it patches" ]
        excludedOf low `shouldReturn` [(TableId 17, "C-PATCH-MONOTONE failed")]
        findings (setCell "T17" 2 2 "(M) Initial Assumption: 1004 Minimum allowed: 1004 Maximum allowed: VU" opal) `shouldBe` []
      it "read alone, without the base, finds no Properties table to name the properties" $ do
        let alone = [ renderFinding f | f <- checkIR Nothing Scott opal, Cm.code f `elem` ownCodes ]
        length alone `shouldBe` 11
        take 1 alone `shouldBe` [ "C-PROPSREQ-NAME opal-2.30.pdf#page=33&T17: row 1 MaxComPacketSize: no propsNames table of opal-2.30 names the properties" ]

    describe "the generated module" $ do
      text <- runIO (moduleOf opal)
      let lns = T.lines text
      it "is Hsed.Gen.Opal.Properties, beside the features, under Table 17's provenance" $ do
        cr <- either (throwString . show) pure (compileIR (Just base) Scott opal)
        map fst (crModules cr) `shouldBe` ["Hsed.Gen.Opal.Features", "Hsed.Gen.Opal.Properties"]
        lns `shouldSatisfy` any ("-- | opal-2.30 T17 - Properties Requirements (PDF 33 / p.32, ord 19, sha256:" `T.isPrefixOf`)
        lns `shouldContain` [ "import           Hsed.Core.Properties (Assumptions (..), Communicator (..), HostRequirement (..), PropertyRequirement (..), Requirement (..), TPerRequirement (..))" ]
      it "declares Core Table 168's assumptions, the host's as they stand, the TPer's about the host patched by name, each value sourced" $ do
        lns `shouldContain`
          [ "initialAssumptions :: [Assumptions]"
          , "initialAssumptions ="
          , "  [ Assumptions Host TPer  -- core-2.01 Table 168 \"Initial Host Assumption About TPer\", as it stands"
          , "      [ (\"MaxSubpackets\", 1)  -- core-2.01 Table 168"
          , "      , (\"MaxPacketSize\", 1004)  -- core-2.01 Table 168" ]
        lns `shouldContain`
          [ "  , Assumptions TPer Host  -- core-2.01 Table 168 \"Initial TPer Assumption About Host\", patched by Table 17"
          , "      [ (\"MaxSubpackets\", 1)  -- Table 17 row 6 \"Initial Assumption: 1\" (core-2.01 Table 168: 1)"
          , "      , (\"MaxPacketSize\", 2028)  -- Table 17 row 3 \"Initial Assumption: 2028\" (core-2.01 Table 168: 1004)"
          , "      , (\"MaxPackets\", 1)  -- Table 17 row 5 \"Initial Assumption: 1\" (core-2.01 Table 168: 1)"
          , "      , (\"MaxComPacketSize\", 2048)  -- Table 17 row 1 \"Initial Assumption: 2048\" (core-2.01 Table 168: 1024)"
          , "      , (\"MaxIndTokenSize\", 1992)  -- Table 17 row 4 \"Initial Assumption: 1992\" (core-2.01 Table 168: 968)"
          , "      , (\"MaxAggTokenSize\", 968)  -- core-2.01 Table 168"
          , "      , (\"MaxMethods\", 1)  -- Table 17 row 7 \"Initial Assumption: 1\" (core-2.01 Table 168: 1)"
          , "      , (\"ContinuedTokens\", 0)  -- core-2.01 Table 168" ]
        length [ l | l <- lns, "      , (\"" `T.isPrefixOf` l || "      [ (\"" `T.isPrefixOf` l ] `shouldBe` 22
      it "declares the 11 requirements, each with its cells as printed, N/A as Nothing, VU as Nothing" $ do
        lns `shouldContain`
          [ "propertyRequirements :: [PropertyRequirement]"
          , "propertyRequirements ="
          , "  [ PropertyRequirement \"MaxComPacketSize\"  -- \"(M) 2048 minimum\"; \"(M) Initial Assumption: 2048 Minimum allowed: 2048 Maximum allowed: VU\""
          , "      (Just (TPerRequirement Mandatory (Just 2048)))"
          , "      (Just (HostRequirement Mandatory (Just 2048) (Just 2048) Nothing))" ]
        lns `shouldContain`
          [ "      (Just (TPerRequirement Mandatory (Just 2048)))"
          , "      (Just (HostRequirement NotRequired Nothing Nothing Nothing))" ]
        lns `shouldContain`
          [ "  , PropertyRequirement \"MaxSessions\"  -- \"(M) 1 minimum\"; \"N/A - not a host property\""
          , "      (Just (TPerRequirement Mandatory (Just 1)))"
          , "      Nothing" ]
        lns `shouldContain`
          [ "  , PropertyRequirement \"DefSessionTimeout\"  -- \"(M) VU\"; \"N/A - not a host property\""
          , "      (Just (TPerRequirement Mandatory Nothing))"
          , "      Nothing" ]
        length [ l | l <- lns, "PropertyRequirement \"" `T.isInfixOf` l ] `shouldBe` 11
        lns `shouldSatisfy` any (\l -> "  , PropertyRequirement \"MaxResponseComPacketSize\"  -- \"(M) 2048 minimum\"; \"(N) Although this is a legal host property" `T.isPrefixOf` l && "...\"" `T.isSuffixOf` l)
      it "type-checks under -Wall -Werror" $
        compiles "Hsed.Gen.Opal.Properties" text
