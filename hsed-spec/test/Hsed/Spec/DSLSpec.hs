{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Spec.DSLSpec
Description : Test specifications for Hsed.Spec.DSL
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The smart constructors type-check here with the combinations the design
writes; the run-time 'applyModifier' agrees with them; 'bindingAccepts' sees
the modifiers.
-}
module Hsed.Spec.DSLSpec (spec) where

import           RIO              hiding ((&))
import           Test.Hspec

import qualified Hsed.Spec.Common as C
import           Hsed.Spec.Common (Severity (..))
import           Hsed.Spec.DSL
import           Hsed.Spec.IR     (HeaderSource (..))
import           Hsed.Spec.Shape


-- | A form with the given columns and normalised header, cited as core T87 on page 109.
form :: [Text] -> Form
form hs = Form
  { fSpec = SpecId "core-2.01", fId = Just "T87", fPage = Just 109, fCols = length hs
  , fHeaderSource = LeadingTH, fHeaderNorm = hs, fGrid8 = False
  , fCaptionKind = CaptionEnumValues "gen_status", fCaption = Just "gen_status Enumeration Values", fRagged = False }

-- | The design's example bindings, as the generated module would spell them.
examples :: [Bound]
examples =
  [ at (anchor @'Doc "core-2.01" "doc" "0" 1) (doc core201 & rule codes & rule fieldValue & rule fieldType & rule byteFieldValue & rule byteGrid & rule terminology)
  , at (anchor @'Heading "core-2.01" "5.1.3" "0/1278" 100) (rule typeRow & rule enumValues)
  , at (anchor @'TypeRow "core-2.01" "T46" "0/1288" 100) typeRow
  , at (anchor @'TypeRow "core-2.01" "T75" "0/1429" 107) (erratum scott ["Fractoin" ~> "Fraction"] typeRow)
  , at (anchor @'EnumValues "core-2.01" "T87" "0/1488" 109) (rangeWords ["to"] (cols 0 1 enumValues))
  , at (anchor @'ColumnsAddition "opal-2.30" "T50" "0/852" 98) (extends (core 171))
  , at (anchor @'Feature "opal-2.30" "T3" "0/195" 20) (refines (core 42))
  , at (anchor @'Codes "opal-2.30" "T14" "0/322" 30) (narrows (core 26))
  , at (anchor @'EnumValues "opal-2.30" "T49" "0/842" 98) (enumInDescription (replaces [core 102, core 103]))
  , at (anchor @'Legend "opal-2.30" "T1" "0/59" 12) legend
  , at (anchor @'PropsReq "opal-2.30" "T17" "0/360" 33) propsReq
  , at (anchor @'StateTable "core-2.01" "T45" "0/1061" 83) (stateTable "Sync.table45")
  , at (anchor @'Sentence "core-2.01" "3.3.10.2.1#3a" "0/1058/2/0/1/0" 82) (law "Sync.ifSendWhileBusy")
  , at (anchor @'Method "core-2.01" "5.3.3.6" "0/3379" 201) (behaviour "Base.get")
  , at (anchor @'Method "core-2.01" "5.3.3.6.3" "0/3395" 201) (fails "Base.getFails")
  , at (anchor @'Prose "core-2.01" "T2" "0/199" 28) prose
  , at (anchor @'Codes "core-2.01" "T25" "0/589" 53) (as @'Codes codes)
  , at (anchor @'Feature "core-2.01" "T43" "0/861" 71) (bits 5 [(0, "KeyChangeZone"), (1, "IdScope")] feature)
  , at (anchor @'Preconfig "opal-2.30" "T24" "0/455" 48) (expand preconfig)
  , at (anchor @'Heading "core-2.01" "3.2.2" "0/200" 30) (rule tokenEncoding & rule sig)
  ]

spec :: Spec
spec = do
  describe "the vocabulary" $ do
    it "type-checks the design's examples and knows their shapes" $
      [ bindingShape b | Bound _ b <- examples ] `shouldBe`
        [ Doc, Heading, TypeRow, TypeRow, EnumValues, ColumnsAddition, Feature, Codes, EnumValues, Legend
        , PropsReq, StateTable, Sentence, Method, Method, Prose, Codes, Feature, Preconfig, Heading ]

    it "names every rule of design section 3.4" $ do
      length knownRules `shouldBe` 19
      [ruleNameOf typeRow, ruleNameOf enumValues] `shouldBe` take 2 knownRules
      ruleNameOf tokenEncoding `shouldBe` RuleName "tokenEncoding"
      ruleNameOf byteGrid `shouldBe` RuleName "byteGrid"

    it "names the specifications" $ do
      map specName [core201, opal230] `shouldBe` ["core201", "opal230"]
      specByName "core201" `shouldBe` Just core201
      specByName "core-2.01" `shouldBe` Nothing

  describe "applyModifier" $ do
    it "accepts what the smart constructors accept" $ do
      applyModifier (Cols 0 1) enumValues `shouldBe` Right (cols 0 1 enumValues)
      applyModifier (RangeWords ["to"]) (cols 0 1 enumValues) `shouldBe` Right (rangeWords ["to"] (cols 0 1 enumValues))
      applyModifier (Erratum Scott ["Fractoin" ~> "Fraction"]) typeRow `shouldBe` Right (erratum scott ["Fractoin" ~> "Fraction"] typeRow)
      applyModifier (As Codes) codes `shouldBe` Right (as @'Codes codes)
      applyModifier Expand preconfig `shouldBe` Right (expand preconfig)
      applyModifier EnumInDescription enumValues `shouldBe` Right (enumInDescription enumValues)
      applyModifier (Waive "C-UID-UNIQUE" "T94 00 00 00 05 00 00 02 15" "r") typeRow
        `shouldBe` Right (waive "C-UID-UNIQUE" "T94 00 00 00 05 00 00 02 15" "r" typeRow)

    it "rejects what they reject, naming both shapes" $ do
      let wrong = [ fmap bindingShape (applyModifier (Cols 0 1) typeRow)
                  , fmap bindingShape (applyModifier (As Codes) typeRow)
                  , fmap bindingShape (applyModifier Expand enumValues)
                  , fmap bindingShape (applyModifier (RangeWords ["to"]) codes)
                  , fmap bindingShape (applyModifier (Waive "C-UID-UNIQUE" "T51 00 00 00 05 00 00 08 01" "r") enumValues) ]
      map (either C.code (const "accepted")) wrong `shouldBe` replicate 5 "E-MODIFIER"
      map (either C.message (const "")) wrong `shouldBe`
        [ "cols 0 1 applies to a EnumValues binding, not to a TypeRow one"
        , "as @'Codes applies to a Codes binding, not to a TypeRow one"
        , "expand applies to a Preconfig binding, not to a EnumValues one"
        , "rangeWords applies to a EnumValues binding, not to a Codes one"
        , "waive applies to a TypeRow binding, not to a EnumValues one" ]
    it "refuses a second layout, and a second bits for one byte, which nothing would read (E-MODIFIER)" $ do
      let twice = [ either C.message (const "accepted") (applyModifier (LayoutName "B") (layout "A" byteGrid))
                  , either C.message (const "accepted") (applyModifier (Bits 4 [(0, "Y")]) (layout "A" (bits 4 [(0, "X")] feature))) ]
      twice `shouldBe`
        [ "a second layout name, \"B\" beside \"A\": a binding takes one"
        , "a second bits modifier for byte 4: a binding takes one for each byte" ]
      applyModifier (Bits 5 [(0, "Y")]) (bits 4 [(0, "X")] feature) `shouldBe` Right (bits 5 [(0, "Y")] (bits 4 [(0, "X")] feature))

  describe "SomeBinding" $ do
    it "compares across shapes" $ do
      SomeBinding typeRow `shouldBe` SomeBinding typeRow
      SomeBinding typeRow `shouldNotBe` SomeBinding enumValues
      SomeBinding (refines (core 42) :: Binding 'Feature) `shouldNotBe` PolyBinding (refines (core 42))
      PolyBinding (refines (core 42)) `shouldBe` PolyBinding (refines (core 42))
      someBindingShape (PolyBinding (narrows (core 26))) `shouldBe` Nothing

    it "fixes the shape of a polymorphic binding through a shaped modifier" $ do
      applyModifierSome (Cols 0 1) (PolyBinding (refines (core 42))) `shouldBe` Right (SomeBinding (cols 0 1 (refines (core 42))))
      applyModifierSome (Erratum Scott []) (PolyBinding (refines (core 42))) `shouldBe` Right (PolyBinding (erratum scott [] (refines (core 42))))
      applyModifierSome (As Feature) (PolyBinding (refines (core 42))) `shouldBe` Right (SomeBinding (as @'Feature (refines (core 42))))
      fmap someBindingShape (applyModifierSome Expand (PolyBinding (replaces [core 1]))) `shouldBe` Right (Just Preconfig)
      fmap someBindingShape (applyModifierSome (Waive "C-UID-UNIQUE" "x" "y") (PolyBinding (refines (core 46)))) `shouldBe` Right (Just TypeRow)

    it "instantiates at a shape" $ do
      atShape @'Feature (PolyBinding (refines (core 42))) `shouldBe` Just (refines (core 42))
      atShape @'Feature (SomeBinding typeRow) `shouldBe` Nothing
      atShape @'TypeRow (SomeBinding typeRow) `shouldBe` Just typeRow

  describe "bindingAccepts" $ do
    let t87 = form ["columnvalue", "associatedvalue", "meaning"]
        t49 = form ["uid", "name", "format", "size", "description"]
    it "sees cols on a wider enumeration table" $ do
      fmap C.severity (bindingAccepts enumValues t87) `shouldBe` Just Error
      bindingAccepts (rangeWords ["to"] (cols 0 1 enumValues)) t87 `shouldBe` Nothing
      fmap C.message (bindingAccepts (cols 0 7 enumValues) t87) `shouldBe` Just "cols 0 7: no column 7 in 3"

    it "demotes under as and cites the page" $ do
      let f = bindingAccepts (as @'EnumValues enumValues) t87
      fmap C.severity f `shouldBe` Just Warning
      fmap (\x -> (C.spec x, C.anchor x, C.page x)) f `shouldBe` Just (Just (SpecId "core-2.01"), Just "T87", Just 109)

    it "asks enumInDescription only for a Description column" $ do
      bindingAccepts (enumInDescription (replaces [core 102, core 103])) t49 `shouldBe` Nothing
      fmap C.severity (bindingAccepts (enumInDescription enumValues) t87) `shouldBe` Just Error

    it "checks the shape of a cross-spec binding at its resolved shape" $ do
      bindingAccepts (narrows (core 26) :: Binding 'Codes) (form ["comid", "description"]) `shouldBe` Nothing
      fmap C.severity (bindingAccepts (narrows (core 26) :: Binding 'TypeRow) (form ["comid", "description"])) `shouldBe` Just Error
