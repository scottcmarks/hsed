{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Rules.EnumValuesSpec
Description : Test specifications for the enumValues rule's own pieces
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The value cells an enumeration reads and the constructor names it prints;
the rule on the committed Core extraction is tested through hsedc's checks
and compilation ("Hsed.Spec.CheckSpec", "Hsed.Spec.CompileSpec").
-}
module Hsed.Spec.Rules.EnumValuesSpec (spec) where

import qualified RIO.Text                   as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Rules.EnumValues (constructorNames, parseEnumValue)


spec :: Spec
spec = do
  describe "parseEnumValue" $
    it "reads a value, a dash or en-dash range, and a declared range word" $ do
      parseEnumValue [] "2" `shouldBe` Just [2]
      parseEnumValue [] "2-7" `shouldBe` Just [2 .. 7]
      parseEnumValue [] "14–15" `shouldBe` Just [14, 15]
      parseEnumValue [] "7 to 31" `shouldBe` Nothing
      parseEnumValue ["to"] "7 to 31" `shouldBe` Just [7 .. 31]
      parseEnumValue [] "7-2" `shouldBe` Nothing

  describe "constructorNames" $ do
    it "names adv_key_mode as today's dEnum does: first value of a label bare, later ones suffixed" $
      map fst (constructorNames "adv_key_mode" (0, 7) advKeyMode)
        `shouldBe` [ "Core_adv_key_mode_Wait_for_AdvKey_Req", "Core_adv_key_mode_Auto_advance_keys", "Core_adv_key_mode_Reserved"
                   , "Core_adv_key_mode_Reserved_3", "Core_adv_key_mode_Reserved_4", "Core_adv_key_mode_Reserved_5"
                   , "Core_adv_key_mode_Reserved_6", "Core_adv_key_mode_Reserved_7" ]
    it "treats a repeated label as one label (padding_type's two None rows)" $
      take 2 (constructorNames "padding_type" (0, 15) paddingType)
        `shouldBe` [("Core_padding_type_None", 0), ("Core_padding_type_None_1", 1)]
    it "keeps a label's values in table order across its rows (life_cycle_state's Unassigned)" $
      filter (T.isInfixOf "Unassigned" . fst) (constructorNames "life_cycle_state" (0, 15) lifeCycleState)
        `shouldBe` [ ("Core_life_cycle_state_Unassigned", 5), ("Core_life_cycle_state_Unassigned_6", 6), ("Core_life_cycle_state_Unassigned_7", 7)
                   , ("Core_life_cycle_state_Unassigned_14", 14), ("Core_life_cycle_state_Unassigned_15", 15) ]
    it "fills the gaps of the range with _NA from the range's lower bound" $ do
      constructorNames "x" (1, 4) [("Idle", 1), ("Active", 3)]
        `shouldBe` [("Core_x_Idle", 1), ("Core_x_NA", 2), ("Core_x_Active", 3), ("Core_x_NA_4", 4)]
      constructorNames "x" (0, 3) [("A", 2)]
        `shouldBe` [("Core_x_NA", 0), ("Core_x_NA_1", 1), ("Core_x_A", 2), ("Core_x_NA_3", 3)]
      map fst (constructorNames "reencrypt_state" (1, 16) reencryptState) `shouldSatisfy` notElem "Core_reencrypt_state_NA"
      map snd (constructorNames "reencrypt_state" (1, 16) reencryptState) `shouldBe` [1 .. 16]
    it "scrubs a label to an identifier: commas dropped, spaces and dashes to underscores" $
      map fst (constructorNames "e" (0, 3) [("RSAES-PKCS1-v1_5", 0), ("SHA 1", 1), ("Issued-Disabled-Frozen", 2), ("ReadLocked, WriteLocked or (none)", 3)])
        `shouldBe` ["Core_e_RSAES_PKCS1_v1_5", "Core_e_SHA_1", "Core_e_Issued_Disabled_Frozen", "Core_e_ReadLocked_WriteLocked_or__none_"]
  where
    advKeyMode     = [("Wait for AdvKey_Req", 0), ("Auto-advance keys", 1)] <> [ ("Reserved", v) | v <- [2 .. 7] ]
    paddingType    = [("None", 0), ("None", 1), ("RSAES-PKCS1-v1_5", 2), ("RSAES-OAEP", 3), ("RSASSA-PKCS1-v1_5", 4)] <> [ ("Reserved", v) | v <- [5 .. 15] ]
    lifeCycleState = [("Issued", 0), ("Issued-Disabled", 1), ("Issued-Frozen", 2), ("Issued-Disabled-Frozen", 3), ("Issued-Failed", 4)]
                     <> [ ("Unassigned", v) | v <- [5 .. 7] ] <> [ ("Reserved for SSC Usage", v) | v <- [8 .. 13] ] <> [ ("Unassigned", v) | v <- [14, 15] ]
    reencryptState = [("Idle", 1), ("Pending", 2), ("Active", 3), ("Completed", 4), ("Paused", 5)] <> [ ("Reserved", v) | v <- [6 .. 16] ]
