{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.TokenEncodingSpec
Description : Test specifications for the tokenEncoding rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The tokenEncoding rule on the committed Core extraction with the committed
annotations (design section 3.4, class D; M2b): it binds Tables 4-14, and only
those, from the anchor at Core 3.2.2; it reads Table 4's 17 rows, and the
table equals the codec's constants byte for byte (the M2b acceptance: every
token row's byte decodes alone to the codec's token its Meaning names and the
codec encodes it as that byte, every reserved byte is refused, every atom range
starts atoms the codec writes with the row's header length); an edited row is
reported, by C-TOKEN-TABLE when the table disagrees with itself and by
C-TOKEN-CODEC when it disagrees with the codec; the generated module records
the eleven tables with their SpecRefs and provenance, the 13 nested tables
counted.
-}
module Hsed.Spec.Rules.TokenEncodingSpec (spec) where

import qualified RIO.List                     as L
import qualified RIO.Map                      as Map
import qualified RIO.Text                     as T
import           RIO
import           Test.Hspec

import           Hsed.Spec.Anchor             (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Check              (checkIR, mkContext)
import           Hsed.Spec.Common             (ErrataSet (..), RuleName (..), renderFinding)
import qualified Hsed.Spec.Common             as Cm
import           Hsed.Spec.Compile            (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.RuleDef            (BoundTable (..), Classification (..))
import           Hsed.Spec.Rules              (classify, explain)
import           Hsed.Spec.Rules.TokenEncoding
import           System.SED.MCTP.Common.Token (Token (..))


spec :: Spec
spec = withCore $ \pristine -> do
  hsa <- runIO committedHsa
  ir  <- runIO (annotated pristine hsa)
  let ctx  = mkContext Scott ir
      rows = concatMap (rights . snd) (tokenRows ctx)
      tokenFindingsOf ir' = L.sort [ renderFinding f | f <- checkIR Scott ir', "C-TOKEN-" `T.isPrefixOf` Cm.code f ]

  describe "the tokenEncoding rule" $ do
    it "binds Tables 4-14, and no other table, by the rule token at Core 3.2.2" $ do
      [ n | (TableId n, RuleBound (RuleName "tokenEncoding") _ _) <- Map.toList (classify ir) ] `shouldBe` [4 .. 14]
      explain ir (TableId 4) `shouldBe` "tokenEncoding (exemplar core T4), binding tokens"
    it "reads Table 4, the one token table, row by row: 17 rows, every one whole" $ do
      [ (renderAnchorId (btAnchor bt), length rs, lefts rs) | (bt, rs) <- tokenRows ctx ]
        `shouldBe` [("T4", 17, [])]
      map tkHex rows `shouldBe`
        [ "00..7F", "80..BF", "C0..DF", "E0..E3", "E4..EF", "F0", "F1", "F2", "F3", "F4..F7"
        , "F8", "F9", "FA", "FB", "FC", "FD..FE", "FF" ]
    it "reads each row's header from its bit pattern: tiny and short atoms one byte, medium two, long four" $
      [ (tkMeaning r, tkBits r) | r <- take 4 rows ] `shouldBe`
        [ ("Tiny atom", ["0", "S", "d<5..0>"]), ("Short atom", ["1", "0", "B", "S", "n<3..0>"])
        , ("Medium atom", ["1", "1", "0", "B", "S", "n<10..0>"])
        , ("Long atom", ["1", "1", "1", "0", "0", "0", "B", "S", "n<23..16>", "n<15..8>", "n<7..0>"]) ]

  describe "Table 4 against the codec (System.SED.MCTP.Common.Token), the M2b acceptance" $ do
    it "finds nothing: the token table equals the codec's constants" $
      tokenFindingsOf ir `shouldBe` []
    it "has the codec's single-byte tokens: each Acronym row's byte decodes alone to the token its Meaning names" $
      [ (tkHex r, tkAcronym r, tkMeaning r, codecTag (tkLo r)) | r <- rows, isJust (tkAcronym r) ] `shouldBe`
        [ ("F0", Just "SL", "Start List", Whole StartList), ("F1", Just "EL", "End List", Whole EndList)
        , ("F2", Just "SN", "Start Name", Whole StartName), ("F3", Just "EN", "End Name", Whole EndName)
        , ("F8", Just "CALL", "Call", Whole Call), ("F9", Just "EOD", "End of Data", Whole EndOfData)
        , ("FA", Just "EOS", "End of session", Whole EndOfSession), ("FB", Just "ST", "Start transaction", Whole StartTransaction)
        , ("FC", Just "ET", "End transaction", Whole EndTransaction), ("FF", Just "MT", "Empty atom", Whole Empty) ]
    it "has the codec's atoms and reserved bytes: what the codec does with every byte of each other row" $
      [ (tkHex r, tkMeaning r, L.nub (map codecTag [tkLo r .. tkHi r])) | r <- rows, isNothing (tkAcronym r) ] `shouldBe`
        [ ("00..7F", "Tiny atom", [Atom [1]])
        , ("80..BF", "Short atom", [Refused, Atom [1]])      -- 80, 90, B0: a zero-length integer or continued value (Table 8)
        , ("C0..DF", "Medium atom", [Atom [2]])
        , ("E0..E3", "Long atom", [Atom [4]])
        , ("E4..EF", "TCG Reserved", [Refused]), ("F4..F7", "TCG Reserved", [Refused]), ("FD..FE", "TCG Reserved", [Refused]) ]
    it "is refused exactly where the codec refuses a zero-length short atom" $
      [ b | b <- [0x80 .. 0xBF], codecTag b == Refused ] `shouldBe` [0x80, 0x90, 0xB0]
    it "probes the empty continued byte sequence, which the codec writes as no atom (none may encode it), and every other atom round-trips" $ do
      (ContinuedBytes mempty, 0) `elem` probe `shouldBe` True
      roundTrips `shouldBe` []

  describe "an edited Table 4" $ do
    it "reports a token's byte moved into a reserved range: the table (pattern, coverage) and the codec" $
      tokenFindingsOf (setCell "T4" 5 9 "F5" ir) `shouldBe`
        [ "C-TOKEN-CODEC core-2.01.pdf#page=32&T4: row 6 F5 SL Start List: the codec refuses F5"
        , "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: F5 is in rows 6 and 10"
        , "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: no row covers F0"
        , "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: row 6 F5 SL Start List: the bit pattern's leading fixed bits 11110000 give F0, not F5" ]
    it "reports a Meaning that names another token" $
      tokenFindingsOf (setCell "T4" 5 11 "End List" ir) `shouldBe`
        [ "C-TOKEN-CODEC core-2.01.pdf#page=32&T4: row 6 F0 SL End List: the codec decodes F0 alone as StartList" ]
    it "reports an atom range its bit pattern does not give, and the bytes it leaves uncovered" $
      tokenFindingsOf (setCell "T4" 0 4 "00..3F" ir) `shouldBe`
        [ "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: no row covers 40..7F"
        , "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: row 1 00..3F Tiny atom: the bit pattern's leading fixed bits 0 give 00..7F, not 00..3F" ]
    it "reports a bit pattern that is not whole bytes" $
      tokenFindingsOf (setCell "T4" 2 5 "n<11..0>" ir) `shouldBe`
        [ "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: row 3 C0..DF Medium atom: the bit pattern comes to 17 bits, not whole bytes" ]
    it "reports an atom header of another length than the codec's" $
      tokenFindingsOf (setCell "T4" 3 10 "" ir) `shouldBe`
        [ "C-TOKEN-CODEC core-2.01.pdf#page=32&T4: row 4 E0..E3 Long atom: the codec writes atoms with 4-byte headers starting with E0..E3; the row's header is 3 bytes" ]
    it "reports a reserved range that holds bytes the codec writes" $
      tokenFindingsOf (setCell "T4" 4 1 "E0..EF" ir) `shouldBe`
        [ "C-TOKEN-CODEC core-2.01.pdf#page=32&T4: row 5 E0..EF TCG Reserved: the codec writes atoms with 4-byte headers starting with E0..E3"
        , "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: E0 is in rows 4 and 5"
        , "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: E1 is in rows 4 and 5"
        , "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: E2 is in rows 4 and 5"
        , "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: E3 is in rows 4 and 5" ]
    it "reports a row without a Hex cell" $
      tokenFindingsOf (setCell "T4" 4 1 "" ir) `shouldBe`
        [ "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: no row covers E4..EF"
        , "C-TOKEN-TABLE core-2.01.pdf#page=32&T4: row 5 has no Hex cell: [\"\",\"\",\"TCG Reserved\"]" ]

  describe "the generated module" $ do
    cr <- runIO (either (throwString . show) pure (compileIR Scott ir))
    let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Tokens" (crModules cr)))
    it "records the eleven tables the codec implements, each with its SpecRef and title" $ do
      lns `shouldContain` ["codecTables :: [(Text, Text)]", "codecTables ="]
      [ l | l <- lns, "  [ (" `T.isPrefixOf` l || "  , (" `T.isPrefixOf` l ] `shouldBe`
        [ "  [ (\"core-2.01:T4\", \"Token Types\")", "  , (\"core-2.01:T5\", \"Tiny Atom Description\")"
        , "  , (\"core-2.01:T6\", \"Tiny Atom Encoding\")", "  , (\"core-2.01:T7\", \"Short Atom Description\")"
        , "  , (\"core-2.01:T8\", \"Short Atom Encoding\")", "  , (\"core-2.01:T9\", \"0-Length Byte Encoding\")"
        , "  , (\"core-2.01:T10\", \"Medium Atom Description\")", "  , (\"core-2.01:T11\", \"Medium Atom Encoding\")"
        , "  , (\"core-2.01:T12\", \"Long Atom Description\")", "  , (\"core-2.01:T13\", \"Long Atom Encoding\")"
        , "  , (\"core-2.01:T14\", \"Empty Atom Description\")" ]
    it "puts each under its provenance, counting the 13 tables nested in the encodings' cells" $ do
      lns `shouldContain` ["-- | core-2.01 T4 Token Types (PDF 32 / p.15, ord 5, sha256:cd3ca596f683)"]
      sum [ n | l <- lns, Just rest <- [T.stripSuffix " nested tables" l <|> T.stripSuffix " nested table" l]
              , Just n <- [readMaybe (T.unpack (T.takeWhileEnd (/= ' ') rest))] ] `shouldBe` (13 :: Int)
      length [ l | l <- lns, "    -- core-2.01 T" `T.isPrefixOf` l ] `shouldBe` 11
