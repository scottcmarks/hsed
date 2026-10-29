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
difference that disappears fails the test as surely as a new one. Each listed
difference is also checked to be the one it is listed as ('Expected'), so a
listed definition whose difference changes fails too.
-}
module Hsed.Gen.Core.EquivSpec (spec) where

import           Data.Char                       (isSpace)
import           Data.List                       (isSuffixOf, sort)
import           Data.Word                       (Word8)
import           Test.Hspec

import           Hsed.Core.Types                 (Enumerated (..), enumValue)
import           Hsed.Gen.Core.EquivTH
import qualified Hsed.Gen.Core.Types             as G
import qualified System.SED.MCTP.Common.TypeUIDs as H
import           System.SED.MCTP.Common.UID      (uid)


-- | Every generated definition compared, and the generated names the hand-written module lacks.
equiv :: ([Comparison], [String])
equiv = $(comparisons "gen/Hsed/Gen/Core/Types.hs" "G" "H")

-- | What a listed difference is.
data Expected
  = PdfUID [Word8]   -- ^ the generated UID is this, the one the PDF prints
  | Spacing          -- ^ the two agree once spaces (and the underscores a label's spaces become) are dropped
  | RangeFromOne     -- ^ the same constructors once the hand-written @_NA@ at 0 is dropped

-- | The differences the hand transcription is known to have, each with what it
--   is and its reason (design section 5.5; docs/evidence/2026-10-25/extract/regen_report.txt).
expectedDifferences :: [(String, Expected, String)]
expectedDifferences =
  -- the four UIDs hsed transcribed wrongly (design section 1)
  [ ("uSecondsType",       PdfUID [0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x14, 0x06], "T129: hsed has all zeros")
  , ("ufeedback_sizeType", PdfUID [0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x02, 0x14], "T83: hsed carries date's 18 04")
  , ("ufraction_enumType", PdfUID [0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x04, 0x1C], "T85: hsed carries date's 18 04")
  , ("ugen_statusType",    PdfUID [0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x1A, 0x02], "T86: hsed carries date's 18 04")
    -- whitespace inside a Format value: the PDF's spacing kept, hsed's ASCII tables closed it up
  , ("fAuthority_object_refType",    Spacing, "T57: uidref {AuthorityTableUID} in the PDF, uidref{...} in hsed")
  , ("fCertificates_object_refType", Spacing, "T72: uidref {CertificatesTableUID} in the PDF")
  , ("fTemplate_object_refType",     Spacing, "T142: uidref {TemplateTableUID} in the PDF")
  , ("fbyte_row_refType",            Spacing, "T62: General_Reference_Type {7} in the PDF")
  , ("fobject_refType",              Spacing, "T119: General_Reference_Type {8} in the PDF")
  , ("ftable_refType",               Spacing, "T141: General_Reference_Type {9} in the PDF")
  , ("flog_row_refType",             Spacing, "T105: Restricted_Reference_Type {6} in the PDF; hsed split the cell differently")
  , ("fcred_object_uidrefType",      Spacing, "T77: the PDF spaces three uidrefs and closes two; hsed spaces all five")
    -- a hand-formatting artefact of hsed's table
  , ("fuinteger_2Type",              Spacing, "T149: hsed's line break left Simple_Type,uinteger, 2")
    -- enumerations
  , ("Core_keys_avail_conds",        Spacing, "T98: the PDF's label reads ReadLocked, WriteLocked (a space after the comma), hsed's ReadLocked,WriteLocked")
  , ("Core_reencrypt_state",         RangeFromOne, "T126: the range starts at 1 (T125 Enumeration_Type, 1, 16); dEnum starts at 0 and adds _NA")
  , ("Core_table_kind",              RangeFromOne, "T138: the range starts at 1 (T137 Enumeration_Type, 1, 8); dEnum starts at 0 and adds _NA")
  ]

-- | Is a comparison the difference it is listed as?
isAsExpected :: Expected -> Comparison -> Bool
isAsExpected expected c = case expected of
  PdfUID [a, b, d, e, f, g, h, i] -> cGenerated c == show (uid a b d e f g h i)
  PdfUID _                        -> False
  Spacing                         -> squeeze (cGenerated c) == squeeze (cHandWritten c)
  RangeFromOne                    -> names (cGenerated c) == filter (not . ("_NA" `isSuffixOf`)) (names (cHandWritten c))
  where
    squeeze = filter (\x -> not (isSpace x) && x /= '_')
    names s = map fst (read s :: [(String, Int)])

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
    it "covers the 100 type rows twice (UID, Format) and the 19 enumerations; T87's is new" $ do
      length compared `shouldBe` 219
      unmatched `shouldBe` ["Core_gen_status"]
    it "differs from the hand transcription exactly where it is known to" $
      sort (map cName (filter differs compared)) `shouldBe` sort [ n | (n, _, _) <- expectedDifferences ]
    it "differs there in exactly the way each difference is listed as" $
      [ cName c | c <- compared, (n, expected, _) <- expectedDifferences, n == cName c, not (isAsExpected expected c) ]
        `shouldBe` []
    it "agrees on the value of every constructor where only the range's start differs (T126, T138)" $ do
      [ (show x, enumValue x) | x <- [minBound .. maxBound :: G.Core_reencrypt_state] ]
        `shouldBe` [ (show x, fromEnum x) | x <- [minBound .. maxBound :: H.Core_reencrypt_state], x /= H.Core_reencrypt_state_NA ]
      [ (show x, enumValue x) | x <- [minBound .. maxBound :: G.Core_table_kind] ]
        `shouldBe` [ (show x, fromEnum x) | x <- [minBound .. maxBound :: H.Core_table_kind], x /= H.Core_table_kind_NA ]
