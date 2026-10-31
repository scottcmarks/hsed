{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Core.PreconfigSpec
Description : Test specifications for Hsed.Core.Preconfig
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A template row's instances, on Opal 2.30's own templates: 'instanceUID' adds
the instance's number to the bytes the placeholder numbers, giving the rows
the tables print where they print an instance (Table 40's Admin1-Admin4 and
Table 41's C_PIN_Admin1-C_PIN_Admin4 from @(+XX XX)@, PDF 82-84), and nothing
past those bytes; 'instantiate' replaces the placeholder's letters in names
and steps the UID and the references in the columns it is given, in a list
only the elements that carry the letters (Table 24's AdminXX, PDF 48; Table
39's ACE_C_PIN_UserMMMM_Set_PIN, PDF 79). The column numbers are Core's
descriptions': Authority, Table 178 (PDF 165-166); ACE, Table 177 (PDF
164-165); C_PIN, Table 181 (PDF 170).
-}
module Hsed.Core.PreconfigSpec (spec) where

import qualified GHC.Exts              as Exts (toList)
import qualified RIO.List              as L
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Hsed.Core.Preconfig
import           Hsed.Core.Table       (Cell (..), ColNum)
import           Hsed.Core.UID         (UID, uNull, uid)


-- | A placeholder numbering so many bytes.
bytes :: Int -> Placeholder
bytes = Placeholder "XX"

-- | The number a UID's last bytes hold.
lastValue :: Int -> UID -> Natural
lastValue k = L.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 . L.drop (8 - k) . Exts.toList

-- | A UID's bytes before its last ones.
firstBytes :: Int -> UID -> [Word8]
firstBytes k = L.take (8 - k) . Exts.toList

-- | An instance's number, up to twice what the placeholder's bytes hold.
numberFor :: Int -> Gen Natural
numberFor k = fromInteger <$> chooseInteger (0, 2 * 256 ^ k)

-- | Opal 2.30 Table 24's AdminXX row (PDF 48): UID, Name, IsClass, Class,
--   Enabled, Operation, Credential, ResponseSign; the placeholder in the UID,
--   the Name and the Credential.
adminXX :: [(ColNum, PreCell)]
adminXX =
  [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00)))
  , (0x01, Given (TextCell "AdminXX"))
  , (0x03, Given (EnumCell "False" 0))
  , (0x04, Given (RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)))
  , (0x05, Given (EnumCell "False" 0))
  , (0x09, Given (EnumCell "Password" 1))
  , (0x0A, Given (RefCell "C_PIN_AdminXX" (uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x02 0x00)))
  , (0x0B, Given (RefCell "Null" uNull)) ]

-- | Opal 2.30 Table 39's ACE_C_PIN_UserMMMM_Set_PIN row (PDF 79): UID, Name,
--   BooleanExpr @Admins OR UserMMMM@ in postfix (Core 5.3.4.3.2-3, PDF
--   226-227; Or is boolean_ACE's 1, Table 61, PDF 104), Columns PIN; the
--   placeholder in the UID, the Name and the BooleanExpr.
aceUserMMMM :: [(ColNum, PreCell)]
aceUserMMMM =
  [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xA8 0x00)))
  , (0x01, Given (TextCell "ACE_C_PIN_UserMMMM_Set_PIN"))
  , (0x03, Given (ListCell [ RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)
                           , RefCell "UserMMMM" (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x00), EnumCell "Or" 1 ]))
  , (0x04, Given (ListCell [UIntCell 3])) ]

spec :: Spec
spec = do
  describe "instanceUID" $ do
    it "numbers Table 24's AdminXX, (+XX) on 00 00 00 09 00 00 02 00: at 2 it is …02 02, at 1 the Admin1 the table prints (PDF 48)" $ do
      instanceUID (Placeholder "XX" 1) (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00) 2
        `shouldBe` Just (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x02)
      instanceUID (Placeholder "XX" 1) (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00) 1
        `shouldBe` Just (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x01)
    it "numbers Table 40's AdminXXXX and Table 41's C_PIN_AdminXXXX, (+XX XX): at 1 it is …00 01, and 1-4 are the rows the tables print (PDF 82-84)" $ do
      instanceUID (Placeholder "XXXX" 2) (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x00) 1
        `shouldBe` Just (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x01)
      map (instanceUID (Placeholder "XXXX" 2) (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x00)) [1 .. 4]
        `shouldBe` map Just [ uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 b | b <- [0x01 .. 0x04] ]
      map (instanceUID (Placeholder "XXXX" 2) (uid 0x00 0x00 0x00 0x0B 0x00 0x01 0x00 0x00)) [1 .. 4]
        `shouldBe` map Just [ uid 0x00 0x00 0x00 0x0B 0x00 0x01 0x00 b | b <- [0x01 .. 0x04] ]
    it "adds the number to the placeholder's bytes, not over them: Table 39's (+MMMM) on …A8 00 at 1 is …A8 01 (PDF 79); XX XX on …02 00 at 1 is …02 01" $ do
      instanceUID (Placeholder "MMMM" 2) (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xA8 0x00) 1
        `shouldBe` Just (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xA8 0x01)
      instanceUID (Placeholder "XXXX" 2) (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00) 1
        `shouldBe` Just (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x01)
    it "gives Nothing past the placeholder's bytes: XX holds 255 more on …02 00, not 256; MMMM 0x57FF more on …A8 00, not 0x5800" $ do
      instanceUID (Placeholder "XX" 1) (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00) 255
        `shouldBe` Just (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0xFF)
      instanceUID (Placeholder "XX" 1) (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00) 256 `shouldBe` Nothing
      instanceUID (Placeholder "MMMM" 2) (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xA8 0x00) 0x57FF
        `shouldBe` Just (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xFF 0xFF)
      instanceUID (Placeholder "MMMM" 2) (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xA8 0x00) 0x5800 `shouldBe` Nothing
    it "reads a width past eight bytes as the whole UID, and one below one byte as none, which holds no number but 0" $ do
      instanceUID (bytes 9) (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0xFF) 1 `shouldBe` Just (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x01 0x00)
      instanceUID (bytes 0) (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00) 0 `shouldBe` Just (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00)
      instanceUID (bytes (-1)) (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00) 1 `shouldBe` Nothing
    prop "keeps the bytes before the placeholder's and adds the number to theirs, or gives Nothing where the sum does not fit" $
      forAll (chooseInt (1, 8)) $ \k -> forAll arbitrary $ \u -> forAll (numberFor k) $ \n ->
        if lastValue k u + n < 256 ^ k
          then fmap (firstBytes k) (instanceUID (bytes k) u n) === Just (firstBytes k u)
                 .&&. fmap (lastValue k) (instanceUID (bytes k) u n) === Just (lastValue k u + n)
          else instanceUID (bytes k) u n === Nothing
    prop "is the base itself at 0" $
      forAll (chooseInt (0, 8)) $ \k -> forAll arbitrary $ \u -> instanceUID (bytes k) u 0 === Just u
    prop "numbers in steps: a + b is b past a's instance" $
      forAll (chooseInt (1, 3)) $ \k -> forAll arbitrary $ \u -> forAll (numberFor k) $ \a -> forAll (numberFor k) $ \b ->
        instanceUID (bytes k) u (a + b) === (instanceUID (bytes k) u a >>= \v -> instanceUID (bytes k) v b)

  describe "instantiate" $ do
    it "makes Table 24's AdminXX at 2 Admin2: its UID …02 02, its Name, and its Credential C_PIN_Admin2, C_PIN_AdminXX's base plus 2 (PDF 48-49)" $
      instantiate (Placeholder "XX" 1) [0x00, 0x01, 0x0A] 2 adminXX `shouldBe`
        [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x02)))
        , (0x01, Given (TextCell "Admin2"))
        , (0x03, Given (EnumCell "False" 0))
        , (0x04, Given (RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)))
        , (0x05, Given (EnumCell "False" 0))
        , (0x09, Given (EnumCell "Password" 1))
        , (0x0A, Given (RefCell "C_PIN_Admin2" (uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x02 0x02)))
        , (0x0B, Given (RefCell "Null" uNull)) ]
    it "changes in a list only the elements whose names carry the letters: Table 39's BooleanExpr Admins OR UserMMMM at 3 (PDF 79)" $
      instantiate (Placeholder "MMMM" 2) [0x00, 0x01, 0x03] 3 aceUserMMMM `shouldBe`
        [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xA8 0x03)))
        , (0x01, Given (TextCell "ACE_C_PIN_User3_Set_PIN"))
        , (0x03, Given (ListCell [ RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)
                                 , RefCell "User3" (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x03), EnumCell "Or" 1 ]))
        , (0x04, Given (ListCell [UIntCell 3])) ]
    it "leaves the columns it is not given as printed, whatever their names carry" $
      instantiate (Placeholder "XX" 1) [0x00] 2 adminXX `shouldBe`
        (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x02))) : L.drop 1 adminXX
    it "leaves a cell the TPer chooses as printed, in a column it is given too" $
      instantiate (Placeholder "XX" 1) [0x00, 0x03] 2 [(0x00, Given (UIDCell uNull)), (0x03, VendorUnique "VU")]
        `shouldBe` [(0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x02))), (0x03, VendorUnique "VU")]
    it "steps a reference in a column it is given whatever its name, and renames only a name that carries the letters" $
      instantiate (Placeholder "XX" 1) [0x01] 2 [(0x01, Given (RefCell "Admin" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00)))]
        `shouldBe` [(0x01, Given (RefCell "Admin" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x02)))]
    it "leaves a uid as printed where the placeholder's bytes cannot hold the number (the caller numbers only the instances instanceUID gives)" $
      instantiate (Placeholder "XX" 1) [0x00, 0x01] 256 (L.take 2 adminXX)
        `shouldBe` [(0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00))), (0x01, Given (TextCell "Admin256"))]
    it "replaces no letters for a placeholder of none" $
      instantiate (Placeholder "" 1) [0x01] 2 [(0x01, Given (TextCell "AdminXX"))] `shouldBe` [(0x01, Given (TextCell "AdminXX"))]
    prop "changes nothing when given no column" $
      forAll (chooseInt (1, 2)) $ \k -> forAll (numberFor k) $ \n ->
        instantiate (bytes k) [] n adminXX === adminXX
