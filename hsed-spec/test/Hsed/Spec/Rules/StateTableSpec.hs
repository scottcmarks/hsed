{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Rules.StateTableSpec
Description : Test specifications for the stateTable reader
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The stateTable reader on the committed Core extraction with the committed
annotations (design example E10, section 6.1; M2b): Core Table 45 is bound by
its comment, @stateTable "Sync.table45"@, and read: three fields (Length,
OutstandingData, MinTransfer) set in six states, each a constant or a quantity
in words; C-STATE-TABLE refuses a name that is not qualified, a state named
twice, a missing setting and a row naming no state, and the table is then
left out; the generated value records the name it is bound to, for M3.
-}
module Hsed.Spec.Rules.StateTableSpec (spec) where

import qualified RIO.Map                    as Map
import qualified RIO.Text                   as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor           (AnchorId (..))
import           Hsed.Spec.Check            (checkIR, mkContext)
import           Hsed.Spec.Common           (ErrataSet (..), renderFinding)
import qualified Hsed.Spec.Common           as Cm
import           Hsed.Spec.Compile          (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.DSL              (SomeBinding (..), stateTable)
import           Hsed.Spec.RuleDef          (Classification (..))
import           Hsed.Spec.Rules            (classify, explain)
import           Hsed.Spec.Rules.StateTable


spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx = mkContext Scott ir
      stateFindings ir' = [ renderFinding f | f <- checkIR Scott ir', Cm.code f == "C-STATE-TABLE" ]
      withComment payload = annotated pristine (withoutLine "T45" hsa <> "core-2.01 | T45 | " <> payload <> "\n")

  describe "the stateTable reader" $ do
    it "reads Table 45, which its comment binds, and no other table" $ do
      Map.lookup (TableId 45) (classify ir) `shouldBe` Just (CommentBound (SomeBinding (stateTable "Sync.table45")))
      explain ir (TableId 45) `shouldBe` "comment: stateTable \"Sync.table45\""
      map stQName (stateTableReads ctx) `shouldBe` ["Sync.table45"]
    it "names the generated value by the comment's name's last part" $
      map stName (stateTableReads ctx) `shouldBe` ["table45"]
    it "reads three fields from the header, less the words their cells all end with" $ do
      map stHeaders (stateTableReads ctx) `shouldBe` [["Length Field Value", "OutstandingData Field Value", "MinTransfer Field Value"]]
      map stFields (stateTableReads ctx) `shouldBe` [["Length", "OutstandingData", "MinTransfer"]]
      fieldNames ["Value"] `shouldBe` ["Value"]
      fieldNames ["A Value", "Value"] `shouldBe` ["A Value", "Value"]
    it "reads the six states, each field a constant or a quantity in words" $
      [ (s, map readSetting cells) | r <- stateTableReads ctx, (s, cells) <- stRows r ] `shouldBe`
        [ ("Response(s) to come, no Response(s) available", [Constant 0 "0x00", Constant 1 "0x01", Constant 0 "0x00"])
        , ("Response ready, insufficient transfer length request"
          , [Constant 0 "0x00", Quantity "Total bytes currently available", Quantity "The minimum request required to transfer a packet"])
        , ("Response, additional Response(s) available"
          , [ Quantity "Data Length", Quantity "Additional bytes available, not including the data transferred in the current ComPacket."
            , Quantity "The minimum request required to transfer the next packet" ])
        , ("Response, additional Response(s) to come, no Response(s) available", [Quantity "Data Length", Constant 1 "0x01", Constant 0 "0x00"])
        , ("Response, all Response(s) returned \x2013 no further data", [Quantity "Data Length", Constant 0 "0x00", Constant 0 "0x00"])
        , ("All Response(s) returned \x2013 no further data", [Constant 0 "0x00", Constant 0 "0x00", Constant 0 "0x00"]) ]

  describe "C-STATE-TABLE" $ do
    it "passes on Table 45" $
      stateFindings ir `shouldBe` []
    it "refuses a name that is not qualified, and leaves the table out" $ do
      ir' <- withComment "stateTable \"table45\""
      stateFindings ir' `shouldBe`
        [ "C-STATE-TABLE core-2.01.pdf#page=83&T45: the comment's name \"table45\" is not a qualified name, Module.name" ]
      newlyExcluded ir ir' `shouldReturn` [(TableId 45, "C-STATE-TABLE failed")]
    it "refuses a state named twice, a missing setting, and a row that names no state" $ do
      stateFindings (setCell "T45" 5 0 "Response(s) to come, no Response(s) available" ir) `shouldBe`
        [ "C-STATE-TABLE core-2.01.pdf#page=83&T45: the state \"Response(s) to come, no Response(s) available\" is named by rows 1 and 6" ]
      stateFindings (setCell "T45" 0 2 " " ir) `shouldBe`
        [ "C-STATE-TABLE core-2.01.pdf#page=83&T45: row 1 (Response(s) to come, no Response(s) available) gives no setting for OutstandingData" ]
      stateFindings (setCell "T45" 3 0 "" ir) `shouldBe`
        [ "C-STATE-TABLE core-2.01.pdf#page=83&T45: row 4 names no state" ]

  describe "the generated module" $ do
    cr <- runIO (either (throwString . show) pure (compileIR Scott ir))
    let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.StateTables" (crModules cr)))
    it "declares table45, recording the code its comment binds it to, for M3" $ do
      lns `shouldContain` ["table45 :: StateTable", "table45 = StateTable", "  { boundTo = \"Sync.table45\""
                          , "  , fields  = [\"Length\", \"OutstandingData\", \"MinTransfer\"]"]
      lns `shouldContain` ["-- | core-2.01 T45 IF-RECV ComPacket Field Values Summary (PDF 83 / p.66, ord 47, sha256:498ea00e3c6a)"]
    it "gives every state its settings, constants as printed and quantities in words" $ do
      length [ l | l <- lns, "StateRow \"" `T.isInfixOf` l ] `shouldBe` 6
      lns `shouldContain` ["      , StateRow \"Response ready, insufficient transfer length request\""
                          , "            [ Constant 0x00", "            , Quantity \"Total bytes currently available\""]
    it "leaves no table out: Table 45 is generated, no longer W-GEN-EXCLUDED" $
      (filter ((== TableId 45) . fst) (crExcluded cr), [ f | f <- crFindings cr, Cm.anchor f == Just "T45" ]) `shouldBe` ([], [])
