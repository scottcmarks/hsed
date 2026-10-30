{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Hsed.Core.TypesSpec
Description : Test specifications for Hsed.Core.Types
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental
-}
module Hsed.Core.TypesSpec (spec) where

import           RIO
import qualified RIO.Set                    as Set
import           Test.Hspec

import           Hsed.Core.Types
import           Hsed.Core.UID              (UID, uid)


-- | Core Table 52 adv_key_mode as the generator would print it: range 0..7, every
--   value a constructor.
data AdvKeyMode = WaitForAdvKeyReq | AutoAdvanceKeys | Reserved | Reserved_3 | Reserved_4
                | Reserved_5 | Reserved_6 | Reserved_7
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Enumerated AdvKeyMode where
  typeUID = uadv_key_modeType
  range   = (0, 7)

-- | Core Table 126 reencrypt_state, whose Format starts at 1: no filler at 0.
data ReencryptState = Idle | Pending | Active | Completed | Paused
                    | Reserved6 | Reserved7
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Enumerated ReencryptState where
  typeUID = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x04 0x14
  range   = (1, 7)

uadv_key_modeType :: UID
uadv_key_modeType = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x04 0x0F

-- | One definition per Format code, as the Core corpus spells them.
oneOfEach :: [(Int, TypeDef)]
oneOfEach =
  [ (0, BaseType)
  , (1, SimpleType (Ref "uinteger") (Lit 2))
  , (2, EnumType [(Lit 0, Lit 7)])
  , (3, AltType [Ref "bytes_16", Ref "bytes_32"])
  , (4, ListType Star (Ref "ACE_expression"))
  , (5, RestrictedRef ByteTableRow [Table "MBR"])
  , (6, RestrictedRef ObjectTableUID [Table "Log", TableStar])
  , (7, GeneralRef AnyByteTableRow)
  , (8, GeneralRef AnyObjectUID)
  , (9, GeneralRef AnyTableUID)
  , (10, GeneralRefTable ByteTable)
  , (11, NamedValueName "Name" (Ref "name"))
  , (12, NamedValueInteger (-1) (Ref "integer_2"))
  , (13, NamedValueUinteger 6 (Ref "fraction_enum"))
  , (14, StructType [Ref "Seconds", Ref "Fraction"])
  , (15, SetType [(Lit 0, Star)])
  ]

spec :: Spec
spec = do
  describe "formatCode" $
    it "assigns the sixteen codes of Core 5.1.1" $
      map (formatCode . snd) oneOfEach `shouldBe` map fst oneOfEach

  describe "formatName" $ do
    it "spells the reference codes with their suffix" $
      map (formatName . snd) (filter ((`elem` [5 .. 9]) . fst) oneOfEach)
        `shouldBe` [ "Restricted_Reference_Type{5}", "Restricted_Reference_Type{6}"
                   , "General_Reference_Type{7}", "General_Reference_Type{8}", "General_Reference_Type{9}" ]
    it "names every code distinctly" $
      Set.size (Set.fromList (map (formatName . snd) oneOfEach)) `shouldBe` 16

  describe "references" $
    it "lists the Type objects a definition names, in Format order" $ do
      references (StructType [Ref "Year", Ref "Month", Ref "Day"]) `shouldBe` [Ref "Year", Ref "Month", Ref "Day"]
      references (ListType Star (Ref "name")) `shouldBe` [Ref "name"]
      references (SimpleType (Ref "bytes") (Lit 8)) `shouldBe` [Ref "bytes"]
      references (RestrictedRef ObjectTableUID [Table "ACE"]) `shouldBe` []
      references (EnumType [(Lit 0, Lit 1)]) `shouldBe` []

  describe "Enumerated" $ do
    it "reaches the Type object's UID and range by type application" $ do
      typeUID @AdvKeyMode `shouldBe` uadv_key_modeType
      range @AdvKeyMode `shouldBe` (0, 7)
    it "values a constructor from the range's lower bound" $ do
      map enumValue [minBound .. maxBound :: AdvKeyMode] `shouldBe` [0 .. 7]
      map enumValue [minBound .. maxBound :: ReencryptState] `shouldBe` [1 .. 7]
    it "finds the constructor of a value inside the range and nothing outside it" $ do
      enumFromValue 3 `shouldBe` Just Reserved_3
      enumFromValue 1 `shouldBe` Just Idle
      enumFromValue 0 `shouldBe` (Nothing :: Maybe ReencryptState)
      enumFromValue 8 `shouldBe` (Nothing :: Maybe AdvKeyMode)
      map (enumFromValue . enumValue) [minBound .. maxBound :: AdvKeyMode]
        `shouldBe` map Just [minBound .. maxBound :: AdvKeyMode]
