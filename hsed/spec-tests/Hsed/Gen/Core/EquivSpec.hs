{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Hsed.Gen.Core.EquivSpec
Description : The generated Core types beside hsed's hand-written TypeUIDs, at run time
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design docs/spec-as-source.md section 5.5 and M1 acceptance (d): the module
@hsedc compile@ generated from the PDF (@gen/Hsed/Gen/Core/Types.hs@, imported
as @G@) against the hand transcription (@System.SED.MCTP.Common.TypeUIDs@, as
@H@). First the M1 slice by name (Tables 46-56 and 75); then every generated
definition with a hand-written counterpart ("Hsed.Gen.Core.EquivTH"), where the
set of definitions that differ must be exactly 'expectedDifferences': a known
difference that disappears fails the test as surely as a new one.
-}
module Hsed.Gen.Core.EquivSpec (spec) where

import           Data.List                       (sort)
import           Test.Hspec

import           Hsed.Core.Types                 (Enumerated (..), enumValue)
import           Hsed.Gen.Core.EquivTH
import qualified Hsed.Gen.Core.Types             as G
import qualified System.SED.MCTP.Common.TypeUIDs as H


-- | Every generated definition compared, and the generated names the hand-written module lacks.
equiv :: ([Comparison], [String])
equiv = $(comparisons "gen/Hsed/Gen/Core/Types.hs" "G" "H")

-- | The differences the hand transcription is known to have, each with its reason
--   (design section 5.5; docs/evidence/2026-10-25/extract/regen_report.txt).
expectedDifferences :: [(String, String)]
expectedDifferences =
  -- the four UIDs hsed transcribed wrongly (design section 1)
  [ ("uSecondsType",       "T129: hsed has all zeros; the PDF prints 00 00 00 05 00 00 14 06")
  , ("ufeedback_sizeType", "T83: hsed carries date's 18 04; the PDF prints 02 14")
  , ("ufraction_enumType", "T85: hsed carries date's 18 04; the PDF prints 04 1C")
  , ("ugen_statusType",    "T86: hsed carries date's 18 04; the PDF prints 1A 02")
    -- whitespace inside a Format value: the PDF's spacing kept, hsed's ASCII tables closed it up
  , ("fAuthority_object_refType",    "T57: uidref {AuthorityTableUID} in the PDF, uidref{...} in hsed")
  , ("fCertificates_object_refType", "T72: uidref {CertificatesTableUID} in the PDF")
  , ("fTemplate_object_refType",     "T142: uidref {TemplateTableUID} in the PDF")
  , ("fbyte_row_refType",            "T62: General_Reference_Type {7} in the PDF")
  , ("fobject_refType",              "T119: General_Reference_Type {8} in the PDF")
  , ("ftable_refType",               "T141: General_Reference_Type {9} in the PDF")
  , ("flog_row_refType",             "T105: Restricted_Reference_Type {6} in the PDF; hsed split the cell differently")
  , ("fcred_object_uidrefType",      "T77: the PDF spaces three uidrefs and closes two; hsed spaces all five")
    -- a hand-formatting artefact of hsed's table
  , ("fuinteger_2Type",              "T149: hsed's line break left Simple_Type,uinteger, 2")
    -- enumerations
  , ("Core_keys_avail_conds",        "T98: the PDF's label reads ReadLocked, WriteLocked (a space after the comma), hsed's ReadLocked,WriteLocked")
  , ("Core_reencrypt_state",         "T126: the range starts at 1 (T125 Enumeration_Type, 1, 16); dEnum starts at 0 and adds _NA")
  , ("Core_table_kind",              "T138: the range starts at 1 (T137 Enumeration_Type, 1, 8); dEnum starts at 0 and adds _NA")
  ]

spec :: Spec
spec = do
  describe "the M1 slice, Core Tables 46-56 and 75 (acceptance (d))" $ do
    it "has the hand-written UIDs" $ do
      G.uAC_elementType     `shouldBe` H.uAC_elementType
      G.uACE_columnsType    `shouldBe` H.uACE_columnsType
      G.uACE_expressionType `shouldBe` H.uACE_expressionType
      G.uACE_object_refType `shouldBe` H.uACE_object_refType
      G.uACLType            `shouldBe` H.uACLType
      G.uadv_key_modeType   `shouldBe` H.uadv_key_modeType
      G.uattr_flagsType     `shouldBe` H.uattr_flagsType
      G.uauth_methodType    `shouldBe` H.uauth_methodType
      G.uclock_timeType     `shouldBe` H.uclock_timeType
    it "has the hand-written Format strings (T75 under the scott errata)" $ do
      G.fAC_elementType     `shouldBe` H.fAC_elementType
      G.fACE_columnsType    `shouldBe` H.fACE_columnsType
      G.fACE_expressionType `shouldBe` H.fACE_expressionType
      G.fACE_object_refType `shouldBe` H.fACE_object_refType
      G.fACLType            `shouldBe` H.fACLType
      G.fadv_key_modeType   `shouldBe` H.fadv_key_modeType
      G.fattr_flagsType     `shouldBe` H.fattr_flagsType
      G.fauth_methodType    `shouldBe` H.fauth_methodType
      G.fclock_timeType     `shouldBe` H.fclock_timeType
    it "has Core_adv_key_mode's constructors in the hand-written order (T52)" $ do
      map show [minBound .. maxBound :: G.Core_adv_key_mode] `shouldBe` map show [minBound .. maxBound :: H.Core_adv_key_mode]
      map fromEnum [minBound .. maxBound :: G.Core_adv_key_mode] `shouldBe` map fromEnum [minBound .. maxBound :: H.Core_adv_key_mode]
    it "ties Core_adv_key_mode to its Type object and range" $ do
      typeUID @G.Core_adv_key_mode `shouldBe` H.uadv_key_modeType
      range @G.Core_adv_key_mode `shouldBe` (0, 7)
    it "has the hand-written enumerations of Tables 54 and 56" $ do
      map show [minBound .. maxBound :: G.Core_attr_flags] `shouldBe` map show [minBound .. maxBound :: H.Core_attr_flags]
      map show [minBound .. maxBound :: G.Core_auth_method] `shouldBe` map show [minBound .. maxBound :: H.Core_auth_method]

  describe "every generated definition with a hand-written counterpart" $ do
    let (compared, unmatched) = equiv
    it "covers the 100 type rows twice (UID, Format) and the 19 enumerations" $ do
      length compared `shouldBe` 219
      unmatched `shouldBe` []
    it "differs from the hand transcription exactly where it is known to" $
      sort (map cName (filter differs compared)) `shouldBe` sort (map fst expectedDifferences)
    it "agrees on the value of every constructor where only the range's start differs (T126, T138)" $ do
      [ (show x, enumValue x) | x <- [minBound .. maxBound :: G.Core_reencrypt_state] ]
        `shouldBe` [ (show x, fromEnum x) | x <- [minBound .. maxBound :: H.Core_reencrypt_state], x /= H.Core_reencrypt_state_NA ]
      [ (show x, enumValue x) | x <- [minBound .. maxBound :: G.Core_table_kind] ]
        `shouldBe` [ (show x, fromEnum x) | x <- [minBound .. maxBound :: H.Core_table_kind], x /= H.Core_table_kind_NA ]
