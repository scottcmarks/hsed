{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Rules.PropsSpec
Description : Test specifications for the Props reader
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Props reader on the committed Core extraction with the committed
annotations (design section 3.4 and example E14; M2b): one reader for the two
comments, @propsNames@ on Core Table 167 and @propsInitial@ on Table 168,
told apart by the binding. Table 167 gives 23 properties, each typed by a
type row (@uinteger@, @boolean@) and describing the host, the TPer or both;
Table 168 gives what each communicator assumes of the other's 11 properties,
a boolean as its enumeration value and a chain of arithmetic checked
(@1024 - 20 - 24 - 12 = 968@). Table 168's @AckNAK@ is Table 167's @AckNak@ only
ignoring case: W-PROP-NAME-CASE, once, and the property's own spelling is
generated. C-PROP-NAME, C-PROP-TYPE and C-PROP-INITIAL each refuse an edited
cell, and the table is then left out.
-}
module Hsed.Spec.Rules.PropsSpec (spec) where

import qualified RIO.List                   as L
import qualified RIO.Map                    as Map
import qualified RIO.Text                   as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Properties       (Communicator (..))
import           Hsed.Spec.Anchor           (AnchorId (..))
import           Hsed.Spec.Check            (checkIR, mkContext)
import           Hsed.Spec.Common           (ErrataSet (..), isError, renderFinding)
import qualified Hsed.Spec.Common           as Cm
import           Hsed.Spec.Compile          (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL              (SomeBinding (..), propsInitial, propsNames)
import           Hsed.Spec.RuleDef          (Classification (..), boundBinding)
import           Hsed.Spec.Rules            (classify)
import           Hsed.Spec.Rules.Props
import           Hsed.Spec.Rules.TypeRow    (TypeRowRead (..))


spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx   = mkContext Scott ir
      props = rights (propertyReads ctx)
      cols  = assumptionsReads ctx
      propFindings ir' = L.sort [ renderFinding f | f <- checkIR Scott ir', any (`T.isInfixOf` Cm.code f) ["-PROP-"] ]

  describe "the Props reader" $ do
    it "reads Tables 167 and 168, which their comments bind, telling them apart by the binding" $ do
      [ (n, propsKind sb) | (TableId n, c) <- Map.toList (classify ir), Just sb <- [boundBinding c], n `elem` [167, 168] ]
        `shouldBe` [(167, Just Names), (168, Just Initial)]
      Map.lookup (TableId 168) (classify ir) `shouldBe` Just (CommentBound (SomeBinding propsInitial))
      Map.lookup (TableId 167) (classify ir) `shouldBe` Just (CommentBound (SomeBinding propsNames))
    it "reads Table 167's 23 properties, each typed by a type row and describing the host, the TPer or both" $ do
      length props `shouldBe` 23
      lefts (propertyReads ctx) `shouldBe` []
      [ (prName p, fmap trName (prType p), prApplies p) | p <- take 2 props <> drop 6 (take 7 props) <> drop 21 props ] `shouldBe`
        [ ("MaxMethods", Just "uinteger", [Host, TPer]), ("MaxSubpackets", Just "uinteger", [Host, TPer])
        , ("MaxSessions", Just "uinteger", [TPer]), ("AckNak", Just "boolean", [Host, TPer])
        , ("Asynchronous", Just "boolean", [Host, TPer]) ]
      length [ p | p <- props, fmap trName (prType p) == Just "boolean" ] `shouldBe` 4
    it "reads Table 168's two columns: the host's assumptions about the TPer, the TPer's about the host" $ do
      map arWho cols `shouldBe` [Just (Host, TPer), Just (TPer, Host)]
      map (length . arValues) cols `shouldBe` [11, 11]
    it "reads a value by its property's type: a number, the end of an arithmetic chain, a boolean's label" $ do
      readValue Nothing "1024" `shouldBe` Right 1024
      readValue Nothing "1024 \x2013 (ComPacket Header size) = 1004" `shouldBe` Right 1004
      readValue Nothing "1024 - (ComPacket Header Size) - (Packet Header Size) - (Subpacket Header Size) = 1024 - 20 - 24 - 12 = 968"
        `shouldBe` Right 968
      readValue Nothing "1024 - 20 = 968" `shouldBe` Left "does not come to 968"
      readValue Nothing "False" `shouldBe` Left "is not a number"
      readValue (Just [("False", [0]), ("True", [1])]) "False" `shouldBe` Right 0
      readValue (Just [("False", [0]), ("True", [1])]) "Maybe" `shouldBe` Left "is no label of the type's enumeration (False, True)"
    it "names the communicators of a cell in the order it names them" $ do
      communicatorsIn "Host Property and TPer Property" `shouldBe` [Host, TPer]
      communicatorsIn "Initial TPer Assumption About Host" `shouldBe` [TPer, Host]
      communicatorsIn "TPer Property" `shouldBe` [TPer]

  describe "the checks" $ do
    it "find on the committed tables only Table 168's AckNAK, which is AckNak ignoring case: a Warning, once" $
      propFindings ir `shouldBe`
        [ "W-PROP-NAME-CASE core-2.01.pdf#page=144&T168: row 10 AckNAK: the property is AckNak in Table 167, which is read" ]
    it "C-PROP-NAME: refuses an assumption about no property of Table 167, and a property named twice (whose row's own name then names none)" $ do
      filter ("C-" `T.isPrefixOf`) (propFindings (setCell "T168" 0 0 "MaxSubPacket" ir)) `shouldBe`
        [ "C-PROP-NAME core-2.01.pdf#page=144&T168: row 1 MaxSubPacket: names no property of Table 167" ]
      filter ("C-" `T.isPrefixOf`) (propFindings (setCell "T167" 1 0 "MaxMethods" ir)) `shouldBe`
        [ "C-PROP-NAME core-2.01.pdf#page=141&T167: the property MaxMethods is also row 1"
        , "C-PROP-NAME core-2.01.pdf#page=144&T168: row 1 MaxSubpackets: names no property of Table 167" ]
    it "C-PROP-TYPE: refuses a Type that names no type row, and an Applicable To that names neither communicator" $ do
      filter ("C-" `T.isPrefixOf`) (propFindings (setCell "T167" 0 1 "uintegr" ir)) `shouldBe`
        [ "C-PROP-TYPE core-2.01.pdf#page=141&T167: row 1 MaxMethods: the Type \"uintegr\" names no type row" ]
      filter ("C-" `T.isPrefixOf`) (propFindings (setCell "T167" 6 3 "Drive Property" ir)) `shouldBe`
        [ "C-PROP-TYPE core-2.01.pdf#page=141&T167: row 7 MaxSessions: Applicable To \"Drive Property\" names neither communicator" ]
    it "C-PROP-INITIAL: refuses a value that does not read, a sum that does not add up, and a property of the other communicator" $ do
      filter ("C-" `T.isPrefixOf`) (propFindings (setCell "T168" 7 1 "Maybe" ir)) `shouldBe`
        [ "C-PROP-INITIAL core-2.01.pdf#page=144&T168: row 8 ContinuedTokens: Initial Host Assumption About TPer: \"Maybe\" is no label of the type's enumeration (False, True)" ]
      filter ("C-" `T.isPrefixOf`) (propFindings (setCell "T168" 4 2 "1024 - 20 - 24 - 12 = 966" ir)) `shouldBe`
        [ "C-PROP-INITIAL core-2.01.pdf#page=144&T168: row 5 MaxIndTokenSize: Initial TPer Assumption About Host: \"1024 - 20 - 24 - 12 = 966\" does not come to 966" ]
      filter ("C-" `T.isPrefixOf`) (propFindings (setCell "T167" 0 3 "TPer Property" ir)) `shouldBe`
        [ "C-PROP-INITIAL core-2.01.pdf#page=144&T168: row 7 MaxMethods: Initial TPer Assumption About Host: Table 167 says MaxMethods does not describe the Host" ]
    it "leave a table with an Error out of the module" $ do
      let broken = setCell "T168" 7 1 "Maybe" ir
      newlyExcluded ir broken `shouldReturn` [(TableId 168, "C-PROP-NAME, C-PROP-TYPE or C-PROP-INITIAL failed")]
      filter isError (checkIR Scott ir) `shouldBe` []

  describe "the generated module" $ do
    cr <- runIO (either (throwString . show) pure (compileIR Scott ir))
    let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Properties" (crModules cr)))
    it "declares the properties, each typed by its type row's UID" $ do
      lns `shouldContain` ["properties :: [Property]", "properties =", "  [ Property \"MaxMethods\" uuintegerType [Host, TPer]"]
      lns `shouldContain` ["  , Property \"AckNak\" ubooleanType [Host, TPer]"]
      length [ l | l <- lns, "Property \"" `T.isInfixOf` l ] `shouldBe` 23
      lns `shouldContain` ["import           Hsed.Gen.Core.Types  (ubooleanType, uuintegerType)"]
    it "declares the initial assumptions, the property's own spelling and each value's printed form kept" $ do
      lns `shouldContain` ["initialAssumptions :: [Assumptions]", "initialAssumptions =", "  [ Assumptions Host TPer  -- \"Initial Host Assumption About TPer\""]
      lns `shouldContain` ["      , (\"AckNak\", 0)  -- \"False\""]
      lns `shouldContain` ["      , (\"MaxPacketSize\", 1004)  -- \"1024 \\8211 (ComPacket Header size) = 1004\""]
      length [ l | l <- lns, "(\"AckNAK\"" `T.isInfixOf` l ] `shouldBe` 0
    it "leaves neither table out: both are generated, no longer W-GEN-EXCLUDED" $
      [ aid | (aid, _) <- crExcluded cr, aid `elem` [TableId 167, TableId 168] ] `shouldBe` []
