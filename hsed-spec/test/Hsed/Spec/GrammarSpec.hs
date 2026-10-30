{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Spec.GrammarSpec
Description : Test specifications for Hsed.Spec.Grammar
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Every payload of design sections 3.5 and 3.6 parses to the typed binding the
DSL spells, the printer is the inverse of the parser on arbitrary payloads,
and errors carry their position.
-}
module Hsed.Spec.GrammarSpec (spec) where

import qualified RIO.Text          as T
import           RIO               hiding ((&))
import           Test.Hspec
import           Test.QuickCheck

import qualified Hsed.Spec.Common  as C
import           Hsed.Spec.DSL
import           Hsed.Spec.Grammar


-- | The design's payloads, the typed binding each means, and its canonical text.
examples :: [(Text, Payload, Text)]
examples =
  [ ( "doc core201 & rule codes & rule fieldValue"
    , Full (SomeBinding (doc core201 & rule codes & rule fieldValue))
    , "doc core201 & rule codes & rule fieldValue" )
  , ( "rule typeRow & rule enumValues"
    , Full (SomeBinding (rule typeRow & rule enumValues :: Binding 'Heading))
    , "rule typeRow & rule enumValues" )
  , ( "erratum scott [\"Fractoin\" ~> \"Fraction\"]"
    , Modifiers [Erratum Scott ["Fractoin" ~> "Fraction"]]
    , "erratum scott [\"Fractoin\" ~> \"Fraction\"]" )
  , ( "rangeWords [\"to\"] (enumValues (cols 0 1))"
    , Full (SomeBinding (rangeWords ["to"] (cols 0 1 enumValues)))
    , "rangeWords [\"to\"] (cols 0 1 enumValues)" )
  , ( "rangeWords [\"to\"] (cols 0 1 enumValues)"
    , Full (SomeBinding (rangeWords ["to"] (cols 0 1 enumValues)))
    , "rangeWords [\"to\"] (cols 0 1 enumValues)" )
  , ( "extends (core 171)"
    , Full (SomeBinding (extends (core 171)))
    , "extends (core 171)" )
  , ( "refines (core 42)"
    , Full (PolyBinding (refines (core 42)))
    , "refines (core 42)" )
  , ( "narrows (core 26)"
    , Full (PolyBinding (narrows (core 26)))
    , "narrows (core 26)" )
  , ( "replaces [core 127, core 128]"
    , Full (PolyBinding (replaces [core 127, core 128]))
    , "replaces [core 127, core 128]" )
  , ( "enumInDescription (replaces [core 102, core 103])"
    , Full (SomeBinding (enumInDescription (replaces [core 102, core 103])))
    , "enumInDescription (replaces [core 102, core 103])" )
  , ( "legend", Full (SomeBinding legend), "legend" )
  , ( "propsReq", Full (SomeBinding propsReq), "propsReq" )
  , ( "stateTable \"Sync.table45\"", Full (SomeBinding (stateTable "Sync.table45")), "stateTable \"Sync.table45\"" )
  , ( "law \"Sync.ifSendWhileBusy\"", Full (SomeBinding (law "Sync.ifSendWhileBusy")), "law \"Sync.ifSendWhileBusy\"" )
  , ( "behaviour \"Base.get\"", Full (SomeBinding (behaviour "Base.get")), "behaviour \"Base.get\"" )
  , ( "prose", Full (SomeBinding prose), "prose" )
  , ( "as @'Codes", Modifiers [As Codes], "as @'Codes" )
  , ( "as @'Codes codes", Full (SomeBinding (as @'Codes codes)), "as @'Codes codes" )
  , ( "erratum scott [\"Fractoin\" ~> \"Fraction\"] typeRow"
    , Full (SomeBinding (erratum scott ["Fractoin" ~> "Fraction"] typeRow))
    , "erratum scott [\"Fractoin\" ~> \"Fraction\"] typeRow" )
  , ( "erratum scott [fractoin ~> fraction] cols 0 1 enumValues"
    , Full (SomeBinding (erratum scott ["fractoin" ~> "fraction"] (cols 0 1 enumValues)))
    , "erratum scott [\"fractoin\" ~> \"fraction\"] (cols 0 1 enumValues)" )
  , ( "waive \"C-UID-UNIQUE\" \"T94 00 00 00 05 00 00 02 15\" \"no erratum says which is wrong\""
    , Modifiers [Waive "C-UID-UNIQUE" "T94 00 00 00 05 00 00 02 15" "no erratum says which is wrong"]
    , "waive \"C-UID-UNIQUE\" \"T94 00 00 00 05 00 00 02 15\" \"no erratum says which is wrong\"" )
  , ( "waive \"C-UID-UNIQUE\" \"x\" \"y\" typeRow"
    , Full (SomeBinding (waive "C-UID-UNIQUE" "x" "y" typeRow))
    , "waive \"C-UID-UNIQUE\" \"x\" \"y\" typeRow" )
  , ( "bits 5 [(0, \"KeyChangeZone\"), (1, \"IdScope\")]"
    , Modifiers [Bits 5 [(0, "KeyChangeZone"), (1, "IdScope")]]
    , "bits 5 [(0, \"KeyChangeZone\"), (1, \"IdScope\")]" )
  , ( "bits 5 [(0, \"KeyChangeZone\"), (1, \"IdScope\")] feature"
    , Full (SomeBinding (bits 5 [(0, "KeyChangeZone"), (1, "IdScope")] feature))
    , "bits 5 [(0, \"KeyChangeZone\"), (1, \"IdScope\")] feature" )
  , ( "layout \"ComPacketHeader\"", Modifiers [LayoutName "ComPacketHeader"], "layout \"ComPacketHeader\"" )
  , ( "layout \"TPerFeature\" (bits 4 [(0, \"Sync\")] feature)"
    , Full (SomeBinding (layout "TPerFeature" (bits 4 [(0, "Sync")] feature)))
    , "layout \"TPerFeature\" (bits 4 [(0, \"Sync\")] feature)" )
  , ( "layout \"GetComIDPayload\" byteFieldValue"
    , Full (SomeBinding (layout "GetComIDPayload" byteFieldValue))
    , "layout \"GetComIDPayload\" byteFieldValue" )
  , ( "replaces [§ \"5.1.3\", opal 0x10]"
    , Full (PolyBinding (replaces [section "5.1.3", opal 16]))
    , "replaces [section \"5.1.3\", opal 16]" )
  , ( "rule typeRow & (rule enumValues & rule tableDesc)"
    , Full (SomeBinding (rule typeRow & (rule enumValues & rule tableDesc) :: Binding 'Heading))
    , "rule typeRow & (rule enumValues & rule tableDesc)" )
  , ( "expand", Modifiers [Expand], "expand" )
  , ( "rangeWords [\"to\"] (cols 0 1)", Modifiers [Cols 0 1, RangeWords ["to"]], "rangeWords [\"to\"] (cols 0 1)" )
  ]

-- | Payloads that must not parse, with the start of the message.
failures :: [(Text, Text, Text)]
failures =
  [ ("typeRwo", "E-GRAMMAR", "1:1: unknown name typeRwo")
  , ("cols 0 1 typeRow", "E-MODIFIER", "cols 0 1 applies to a EnumValues binding, not to a TypeRow one")
  , ("rule typeRwo", "E-GRAMMAR", "1:1: unknown rule typeRwo")
  , ("doc core199", "E-GRAMMAR", "1:1: unknown specification core199")
  , ("erratum scott", "E-GRAMMAR", "1:1: erratum: expected erratum scott [\"old\" ~> \"new\", ...]")
  , ("erratum bob [\"a\" ~> \"b\"]", "E-GRAMMAR", "1:9: unknown errata set bob")
  , ("erratum published [\"Fractoin\" ~> \"Fraction\"]", "E-ERRATUM-PUBLISHED",
     "1:9: erratum published: the published set is the text as printed and takes no corrections; write erratum scott")
  , ("erratum scott [\"a\" ~> \"b\"] (erratum published [\"a\" ~> \"b\"] typeRow)", "E-ERRATUM-PUBLISHED", "1:37: erratum published")
  , ("waive \"C-ENUM-RANGE\" \"x\" \"y\"", "E-GRAMMAR", "1:7: waive: hsedc waives only C-UID-UNIQUE, not C-ENUM-RANGE")
  , ("waive \"C-UID-UNIQUE\" \"x\"", "E-GRAMMAR", "1:1: waive: expected waive \"C-UID-UNIQUE\" \"subject\" \"reason\"")
  , ("as @'Codex", "E-GRAMMAR", "1:4: unknown shape Codex")
  , ("typeRow & rule codes", "E-GRAMMAR", "1:1: only doc and rule tokens join with &")
  , ("rule codes & doc core201 & typeRow", "E-GRAMMAR", "1:28: only doc and rule tokens join with &")
  , ("law \"x\" (cols 0 1)", "E-MODIFIER", "cols 0 1 applies to a EnumValues binding, not to a Sentence one")
  , ("refines core 42", "E-GRAMMAR", "1:9: expected a reference: core N, opal N or section \"x.y\"")
  , ("[\"to\"", "E-SYNTAX", "1:6:")
  , ("cols 0 1 \"x\"", "E-GRAMMAR", "1:10: expected a binding or a modifier")
  , ("layout \"X\" typeRow", "E-MODIFIER", "layout applies to a ByteGrid, Feature, ByteFieldValue binding, not to a TypeRow one")
  , ("bits 4 [(0, \"A\")] byteFieldValue", "E-MODIFIER", "bits 4 applies to a ByteGrid, Feature binding, not to a ByteFieldValue one")
  , ("bits 4 [0]", "E-GRAMMAR", "1:9: expected (BIT, \"Field\")")
  , ("layout \"X\" (refines (core 39))", "E-MODIFIER", "layout applies to a binding whose shape is fixed")
  , ("featureNamed \"X\"", "E-GRAMMAR", "1:1: unknown name featureNamed")
  ]

spec :: Spec
spec = do
  describe "parsePayload" $ do
    it "reads every payload of the design" $
      [ (t, parsePayload t) | (t, _, _) <- examples ] `shouldBe` [ (t, Right p) | (t, p, _) <- examples ]

    it "reports what it cannot read, with the position" $
      [ (t, either (\f -> (C.code f, T.take (T.length m) (C.message f))) (const ("accepted", "")) (parsePayload t))
      | (t, _, m) <- failures ]
        `shouldBe` [ (t, (c, m)) | (t, c, m) <- failures ]

    it "ignores surrounding whitespace" $
      parsePayload "  erratum  scott\n [ \"a\" ~> \"b\" ]  typeRow  " `shouldBe` Right (Full (SomeBinding (erratum scott ["a" ~> "b"] typeRow)))

  describe "printPayload" $ do
    it "prints the canonical form of every example" $
      [ printPayload p | (_, p, _) <- examples ] `shouldBe` [ c | (_, _, c) <- examples ]

    it "is inverted by parsePayload on arbitrary payloads" $
      property $ \(ArbPayload p) -> parsePayload (printPayload p) === Right p

    it "names every word a payload may start with in its vocabulary" $
      vocabulary `shouldSatisfy` \v -> all (`elem` v) ["doc", "rule", "typeRow", "law", "behaviour", "erratum", "waive", "as", "refines"]

    it "prints Haskell string literals the grammar reads back" $
      property $ \(ArbText t) -> parsePayload ("law " <> haskellString t) === Right (Full (SomeBinding (law t)))


-- * Generators

newtype ArbText = ArbText Text
  deriving stock (Show)

instance Arbitrary ArbText where
  arbitrary = ArbText . T.pack <$> listOf (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " _.\"\\~>&()[]"))
  shrink (ArbText t) = ArbText . T.pack <$> shrink (T.unpack t)

newtype ArbPayload = ArbPayload Payload
  deriving stock (Show)

instance Arbitrary ArbPayload where
  arbitrary = ArbPayload <$> oneof
    [ Full <$> genSomeBinding
    , Modifiers <$> listOf1 genModifier ]

text :: Gen Text
text = (\(ArbText t) -> t) <$> arbitrary

small :: Gen Int
small = chooseInt (0, 300)

genRef :: Gen Ref
genRef = oneof [core <$> small, opal <$> small, section <$> dotted]
  where dotted = T.intercalate "." <$> listOf1 (tshow <$> chooseInt (1, 20))

-- | Scott's: the grammar refuses an erratum of the published set.
genErrataSet :: Gen ErrataSet
genErrataSet = pure Scott

genModifier :: Gen Modifier
genModifier = oneof
  [ Erratum <$> genErrataSet <*> listOf ((,) <$> text <*> text)
  , Cols <$> small <*> small
  , RangeWords <$> listOf text
  , pure EnumInDescription
  , pure Expand
  , As <$> elements [minBound .. maxBound]
  , Waive <$> elements waivableChecks <*> text <*> text
  , LayoutName <$> text
  , Bits <$> small <*> listOf ((,) <$> small <*> text) ]

-- | A fixed-shape table or prose binding without modifiers.
genCore :: Gen SomeBinding
genCore = oneof
  [ elements
      [ SomeBinding typeRow, SomeBinding enumValues, SomeBinding tableDesc, SomeBinding columnsAddition
      , SomeBinding tableUIDs, SomeBinding uidTable, SomeBinding statusCodes, SomeBinding codes
      , SomeBinding fieldValue, SomeBinding fieldType, SomeBinding byteFieldValue, SomeBinding byteGrid
      , SomeBinding feature, SomeBinding constValues, SomeBinding ranges, SomeBinding preconfig
      , SomeBinding tokens, SomeBinding propsNames, SomeBinding propsInitial, SomeBinding propsReq
      , SomeBinding legend, SomeBinding decision, SomeBinding sig, SomeBinding prose ]
  , SomeBinding . stateTable <$> text
  , SomeBinding . behaviour <$> text
  , SomeBinding . fails <$> text
  , SomeBinding . law <$> text
  , SomeBinding . extends <$> genRef ]

-- | A cross-spec binding, shape still open.
genPoly :: Gen SomeBinding
genPoly = oneof
  [ (\r -> PolyBinding (refines r)) <$> genRef
  , (\r -> PolyBinding (narrows r)) <$> genRef
  , (\rs -> PolyBinding (replaces rs)) <$> listOf genRef ]

-- | Rule tokens joined by @&@, on a heading or on the document.
genRules :: Gen SomeBinding
genRules = oneof [SomeBinding <$> sized (rules @'Heading), SomeBinding <$> sized docRules]
  where
    -- the document's chain carries its doc token somewhere
    docRules :: Int -> Gen (Binding 'Doc)
    docRules n = do
      d <- DocB <$> elements [core201, opal230]
      oneof [pure d, Also d <$> rules n, (`Also` d) <$> rules n]

-- | A left- or right-nested chain of rule tokens.
rules :: forall t. RuleBearing t => Int -> Gen (Binding t)
rules n
  | n <= 1    = leaf
  | otherwise = oneof [leaf, Also <$> rules (n `div` 2) <*> rules (n `div` 2)]
  where leaf = RuleB <$> elements knownRules

-- | A binding with modifiers the shape allows, applied one by one.
genSomeBinding :: Gen SomeBinding
genSomeBinding = do
  base <- frequency [(6, genCore), (2, genPoly), (2, genRules)]
  mods <- listOf genModifier
  pure (foldl' (\sb m -> either (const sb) id (applyModifierSome m sb)) base mods)
