{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Rules.ByteFieldValueSpec
Description : Test specifications for the byteFieldValue rule
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The VALUE cells the rule reads; then the rule on the committed Core extraction
with the committed annotations (design section 3.4, example E7): the eight
tables it binds from the doc anchor; the first row of Tables 31 and 37, whose
bytes the PDF's tags put in the header; the layouts, each tiling its bytes but
Table 236's, which leaves byte 8 to no row (a Warning, the layout generated as
printed and saying so, until an erratum names the byte, as one does here); the
constants pinned from the VALUE
column, checked against what the PDF prints of them; the findings of edited
tables; the declarations of the layouts module.
-}
module Hsed.Spec.Rules.ByteFieldValueSpec (spec) where

import qualified RIO.ByteString               as B
import qualified RIO.List                     as L
import qualified RIO.Map                      as Map
import qualified RIO.Text                     as T
import           RIO
import           RIO.Directory                (doesFileExist)
import           Test.Hspec

import           Hsed.Core.Layout             (Content (..), Decoded (..), Field (..), FieldKind (..), Layout (..), Tail (..),
                                               TileProblem (..), bitPosition, decodeStrict, encode, fieldEnd, tiling)
import           Hsed.Spec.Anchor             (AnchorId (..))
import           Hsed.Spec.Check              (checkIR, mkContext)
import           Hsed.Spec.Common             (ErrataSet (..), RuleName (..), Severity (..), SpecId (..), renderFinding)
import qualified Hsed.Spec.Common             as Cm
import           Hsed.Spec.Compile            (CompileResult (..), compileIR)
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR                 (CellIR (..), SpecIR (..), TableIR (..), readIR)
import           Hsed.Spec.RuleDef            (BoundTable (..), Classification (..), RuleDef (..), numberedTables)
import           Hsed.Spec.Rules              (classify, explain)
import           Hsed.Spec.Rules.ByteFieldValue
import           Hsed.Spec.Shape              (measure)


-- | The tables a rule fires on anywhere in an IR, by number, whatever its scope.
firesOn :: RuleDef -> SpecIR -> [Int]
firesOn r ir = [ n | (TableId n, t) <- Map.toList (numberedTables ir), fires r sid t (measure sid t) ]
  where sid = SpecId ir.spec

-- | The rule's reading of one table.
readOf :: SpecIR -> Int -> BytesRead
readOf ir n = fromMaybe (error ("T" <> show n <> " is not bound to byteFieldValue"))
  (L.find ((== TableId n) . btAnchor . bfBound) (bytesReads (mkContext Scott ir)))

-- | A field as the grid prints it: its name, its first and last (byte, bit), its kind.
printed :: Field -> (Text, (Int, Int), (Int, Int), FieldKind)
printed f = (fieldName f, bitPosition (fieldStart f), bitPosition (fieldEnd f - 1), fieldKind f)

-- | The IR with one data row of a table replaced.
setRow :: Text -> Int -> ([CellIR] -> [CellIR]) -> SpecIR -> SpecIR
setRow tid i edit ir = ir { tables = map table ir.tables }
  where
    table t
      | t.id == Just tid = t { rows = zipWith (\j r -> if j == i then edit r else r) [0 ..] t.rows }
      | otherwise        = t

-- | The tables the rule binds, by number.
boundTo :: SpecIR -> [Int]
boundTo ir = [ n | (TableId n, RuleBound (RuleName "byteFieldValue") _ _) <- Map.toList (classify ir) ]

spec :: Spec
spec = do
  describe "readValue" $ do
    it "reads hex bytes as many as the field's as a number, and a 0x or decimal number" $
      map (readValue (Just 4)) ["00 00 00 01", "00 00 00 00", "0x0004", "1024"] `shouldBe` map Exact [1, 0, 4, 1024]
    it "reads hex bytes as wide as a two-byte field" $
      map (readValue (Just 2)) ["00 22", "00 04"] `shouldBe` [Exact 0x22, Exact 4]
    it "reads hex bytes of another count as a misfit, never as decimal" $
      map (readValue (Just 4)) ["00 00", "12", "00 00 00 00 01"] `shouldBe` [Misfit 2, Misfit 1, Misfit 5]
    it "reads hex bytes for the run to the end of the transfer as its bytes" $
      map (readValue Nothing) ["00", "00 ff"] `shouldBe` [Fill [0], Fill [0, 0xff]]
    it "reads anything else as printed, its spacing squeezed" $
      map (readValue (Just 4)) ["Allocated  ComID", "00 00 00 00/ 00 00 00 01", "zero", "0x", ""]
        `shouldBe` map Printed ["Allocated ComID", "00 00 00 00/ 00 00 00 01", "zero", "0x", ""]

  describe "the rule's codes" $
    it "claims C-BYTES-ROW, C-BYTES-VALUE and N-BYTES-IN-HEADER, and none of the readers'" $
      map Cm.codeName (ruleCodes byteFieldValueRule) `shouldBe` ["C-BYTES-ROW", "C-BYTES-VALUE", "N-BYTES-IN-HEADER"]

  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let ours = ["C-BYTES-ROW", "C-BYTES-VALUE", "N-BYTES-IN-HEADER", "C-LAYOUT-TILE", "C-LAYOUT-EXTENT"]
        findings ir' = [ renderFinding f | f <- checkIR Scott ir', Cm.code f `elem` ours ]
        severities ir' = [ Cm.severity f | f <- checkIR Scott ir', Cm.code f `elem` ours ]

    describe "the byteFieldValue rule" $ do
      it "binds T28, T31-T33, T35-T37 and T236, and no other table, by the doc anchor's rule token" $ do
        boundTo ir `shouldBe` [28, 31, 32, 33, 35, 36, 37, 236]
        explain ir (TableId 33) `shouldBe` "byteFieldValue (exemplar core T33)"
      it "fires on no other table of Core" $
        firesOn byteFieldValueRule ir `shouldBe` [28, 31, 32, 33, 35, 36, 37, 236]
      it "fires on no table of Opal" $ do
        let path = "../spec/ir/opal-2.30.json"
        present <- doesFileExist path
        if not present then pendingWith (path <> " is not checked out") else do
          opal <- readIR path >>= either throwString pure
          firesOn byteFieldValueRule opal `shouldBe` []

    describe "reading the rows" $ do
      it "reads each row's bytes, field and value" $
        [ ((bwLabel r).text, (bwField r).text, bwValue r) | r <- take 2 (bfRows (readOf ir 33)) ] `shouldBe`
          [ ("0 to 3", "Extended ComID", Just "Requested ComID"), ("4 to 7", "Request Code", Just "00 00 00 01") ]
      it "reads Table 236's two columns as bytes and a field, with no value" $
        [ ((bwLabel r).text, bwValue r) | r <- bfRows (readOf ir 236) ] `shouldBe` [("0 to 7", Nothing), ("9 to 15", Nothing), ("16 to 17", Nothing)]
      it "reads the first row of T31 and T37 with the bytes the header cell's later lines print (N-BYTES-IN-HEADER)" $ do
        [ ((bwLabel r).text, (bwField r).text) | r <- take 1 (bfRows (readOf ir 31)) ] `shouldBe` [("0 to 3", "Extended ComID")]
        [ (Cm.code f, Cm.severity f) | f <- checkIR Scott ir, Cm.code f == "N-BYTES-IN-HEADER" ] `shouldBe` replicate 2 ("N-BYTES-IN-HEADER", Note)
        filter ("N-BYTES-IN-HEADER" `T.isPrefixOf`) (findings ir) `shouldBe`
          [ "N-BYTES-IN-HEADER core-2.01.pdf#page=63&T31: row 1 has no bytes cell: its bytes \"0 to 3\" are read from the header cell \"VALUE 0 to 3\", whose later lines print them"
          , "N-BYTES-IN-HEADER core-2.01.pdf#page=66&T37: row 1 has no bytes cell: its bytes \"0 to 3\" are read from the header cell \"VALUE 0 to 3\", whose later lines print them" ]

    describe "the layouts" $ do
      it "gives each its fixed part's size and the run to the end of the transfer as its tail" $ do
        [ (n, layoutSize l, layoutTail l) | n <- [28, 31, 32, 33, 35, 36, 37], let l = bfLayout (readOf ir n) ] `shouldBe`
          [ (28, 4, Nothing) ] <> [ (n, s, Just (Tail "Reserved" Nothing 1)) | (n, s) <- [(31, 12), (32, 8), (33, 46), (35, 8), (36, 16), (37, 12)] ]
      it "T33: 46 bytes, the numbers, the reserved bytes, the pinned constants and the three 10-byte times (PDF 64)" $
        map printed (layoutFields (bfLayout (readOf ir 33))) `shouldBe`
          [ ("Extended ComID", (0, 7), (3, 0), Unsigned)
          , ("Request Code", (4, 7), (7, 0), Pinned 1)
          , ("Reserved", (8, 7), (9, 0), Reserved)
          , ("Available Data Length in bytes", (10, 7), (11, 0), Pinned 0x22)
          , ("Current state of Extended ComID", (12, 7), (15, 0), Unsigned)
          , ("Absolute time of allocation relative to last reset of TPer", (16, 7), (25, 0), Bytes)
          , ("Absolute time of expiry relative to last reset of TPer", (26, 7), (35, 0), Bytes)
          , ("Time since last reset of TPer", (36, 7), (45, 0), Bytes) ]
      it "pins the Request Code of each request, response and pending payload (PDF 63-66)" $
        [ (n, [ k | f <- layoutFields (bfLayout (readOf ir n)), fieldName f == "Request Code", let k = fieldKind f ]) | n <- [31, 32, 33, 35, 36, 37] ]
          `shouldBe` [(31, [Pinned 0]), (32, [Pinned 1]), (33, [Pinned 1]), (35, [Pinned 2]), (36, [Pinned 2]), (37, [Pinned 2])]
      it "pins each Available Data Length to the bytes that follow it in the fixed part, as the PDF prints them (00 00, 00 22, 00 04, 00 00)" $
        [ (n, v, toInteger (layoutSize l - 12)) | n <- [31, 33, 36, 37], let l = bfLayout (readOf ir n)
                                                  , f <- layoutFields l, fieldName f == "Available Data Length in bytes", Pinned v <- [fieldKind f] ]
          `shouldBe` [(31, 0, 0), (33, 0x22, 0x22), (36, 4, 4), (37, 0, 0)]
      it "C-LAYOUT-TILE passes for T28, T31-T33 and T35-T37 (T33: 46 bytes)" $ do
        [ (n, tiling (bfLayout (readOf ir n))) | n <- [28, 31, 32, 33, 35, 36, 37] ] `shouldBe` [ (n, []) | n <- [28, 31, 32, 33, 35, 36, 37] ]
        [ f | f <- findings ir, "C-LAYOUT-TILE" `T.isPrefixOf` f, not ("&T236:" `T.isInfixOf` f) ] `shouldBe` []
      it "finds byte 8 of T236 in no row, a Warning at the row after it (PDF 298), and generates the layout as printed" $ do
        let l = bfLayout (readOf ir 236)
        (layoutSize l, map printed (layoutFields l)) `shouldBe`
          ( 18, [ ("The invoking uid of the method", (0, 7), (7, 0), Unsigned)
                , ("The uid of the method involved in the operation. If there is no method, these bytes SHALL be zeroes.", (9, 7), (15, 0), Unsigned)
                , ("Status code for the operation.", (16, 7), (17, 0), Unsigned) ] )
        tiling l `shouldBe` [Gap 64 71]
        filter ("C-LAYOUT-TILE" `T.isPrefixOf`) (findings ir) `shouldBe`
          [ "C-LAYOUT-TILE core-2.01.pdf#page=298&T236: System Log Entry Structure: no field covers byte 8 bit 7 to byte 8 bit 0" ]
        [ s | f <- checkIR Scott ir, Cm.code f == "C-LAYOUT-TILE", let s = Cm.severity f ] `shouldBe` [Warning]
        cr <- either (throwString . show) pure (compileIR Scott ir)
        [ e | e@(TableId 236, _) <- crExcluded cr ] `shouldBe` []
      it "gives no Error on the committed run" $
        filter (== Error) (severities ir) `shouldBe` []

    describe "Table 236 under an erratum naming byte 8" $ do
      fixed <- runIO (annotated pristine (hsa <> "core-2.01 | T236 | erratum scott [\"9 to 15\" ~> \"8 to 15\"]\n"))
      it "tiles 18 bytes, each field named by its Values cell, a parenthesis apart, and says nothing of a gap" $ do
        let l = bfLayout (readOf fixed 236)
        map printed (layoutFields l) `shouldBe`
          [ ("The invoking uid of the method", (0, 7), (7, 0), Unsigned)
          , ("The uid of the method involved in the operation. If there is no method, these bytes SHALL be zeroes.", (8, 7), (15, 0), Unsigned)
          , ("Status code for the operation.", (16, 7), (17, 0), Unsigned) ]
        tiling l `shouldBe` []
        filter ("C-LAYOUT-TILE" `T.isPrefixOf`) (findings fixed) `shouldBe` []
        cr <- either (throwString . show) pure (compileIR Scott fixed)
        let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Layouts" (crModules cr)))
        lns `shouldContain` ["      , spanning \"The uid of the method involved in the operation. If there is no method, these bytes SHALL be zeroes.\" (8, 7) (15, 0) Unsigned"]
        filter ("-- It does not tile" `T.isPrefixOf`) lns `shouldBe` []

    describe "the layout modifier" $ do
      named <- runIO (annotated pristine (hsa <> "core-2.01 | T28 | layout \"GetComIDPayload\"\n"))
      it "names the layout and its declaration" $ do
        layoutName (bfLayout (readOf named 28)) `shouldBe` "GetComIDPayload"
        cr <- either (throwString . show) pure (compileIR Scott named)
        T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Layouts" (crModules cr))) `shouldContain` ["getComIDPayloadLayout :: Layout"]
        T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Layouts" (crModules cr))) `shouldContain` ["data GetComIDPayload = GetComIDPayload"]

    describe "the findings of edited tables" $ do
      it "C-BYTES-VALUE: hex bytes not as many as the field's, a reserved field's value not zero; the table is left out" $ do
        let broken = setCell "T33" 2 2 "00 01" (setCell "T33" 1 2 "00 01" ir)
        filter ("C-BYTES-VALUE" `T.isPrefixOf`) (findings broken) `shouldBe`
          [ "C-BYTES-VALUE core-2.01.pdf#page=64&T33: row 2: \"Request Code\" (4 to 7) prints 2 bytes for a field of 4"
          , "C-BYTES-VALUE core-2.01.pdf#page=64&T33: row 3: \"Reserved\" (8 to 9) is reserved, its value not zero" ]
        newlyExcluded ir broken `shouldReturn` [(TableId 33, "C-BYTES-VALUE failed")]
      it "C-BYTES-VALUE: a reserved run to the end of the transfer whose value is not zero" $
        filter ("C-BYTES-VALUE" `T.isPrefixOf`) (findings (setCell "T35" 2 2 "01" ir)) `shouldBe`
          [ "C-BYTES-VALUE core-2.01.pdf#page=65&T35: row 3: \"Reserved\" (8 to TRNSFLEN - 1) is reserved, its value not zero" ]
      it "C-LAYOUT-TILE: rows that overlap are an Error, and the table is left out" $ do
        let broken = setCell "T33" 1 2 "00 00 00 00 01" (setCell "T33" 1 0 "4 to 8" ir)
        [ (Cm.severity f, renderFinding f) | f <- checkIR Scott broken, Cm.code f == "C-LAYOUT-TILE", Cm.anchor f == Just "T33" ] `shouldBe`
          [ (Error, "C-LAYOUT-TILE core-2.01.pdf#page=64&T33: VERIFY_COMID_VALID Command Response: \"Reserved\" starts at byte 8 bit 7, inside \"Request Code\"") ]
        newlyExcluded ir broken `shouldReturn` [(TableId 33, "C-LAYOUT-TILE failed")]
      it "C-LAYOUT-TILE: a run to the end of the transfer that starts past the fixed part leaves a gap, a Warning" $
        [ (Cm.severity f, renderFinding f) | f <- checkIR Scott (setCell "T33" 8 0 "47 to TRNSFLEN - 1" ir), Cm.code f == "C-LAYOUT-TILE", Cm.anchor f == Just "T33" ]
          `shouldBe` [ (Warning, "C-LAYOUT-TILE core-2.01.pdf#page=64&T33: VERIFY_COMID_VALID Command Response: no field covers byte 46 bit 7 to byte 46 bit 0") ]
      it "C-BYTES-ROW: a second run to the end of the transfer" $
        filter ("C-BYTES-ROW" `T.isPrefixOf`) (findings (setCell "T33" 7 0 "36 to TRNSFLEN - 1" ir)) `shouldBe`
          [ "C-BYTES-ROW core-2.01.pdf#page=64&T33: a second run to an end the text gives, \"46 to TRNSFLEN - 1\": only one can follow the fixed part" ]
      it "C-BYTES-ROW: a row without its value, and one of two cells, which leaves its bytes to no field" $ do
        let broken = setRow "T33" 4 (take 2) (setCell "T33" 3 2 " " ir)
        filter (\f -> any (`T.isPrefixOf` f) ["C-BYTES-ROW", "C-LAYOUT-TILE"] && "&T33:" `T.isInfixOf` f) (findings broken) `shouldBe`
          [ "C-BYTES-ROW core-2.01.pdf#page=64&T33: row 4: the field \"Available Data Length in bytes\" has no value"
          , "C-BYTES-ROW core-2.01.pdf#page=64&T33: row 5 has 2 cells, not its bytes, a field and a value"
          , "C-LAYOUT-TILE core-2.01.pdf#page=64&T33: VERIFY_COMID_VALID Command Response: no field covers byte 12 bit 7 to byte 15 bit 0" ]
      it "C-LAYOUT-EXTENT: a row whose bytes do not read, as the grid reader reports it" $
        filter ("C-LAYOUT-EXTENT" `T.isPrefixOf`) (findings (setCell "T28" 0 0 "zero to three" ir)) `shouldBe`
          [ "C-LAYOUT-EXTENT core-2.01.pdf#page=61&T28: row 1: the byte number \"zero to three\" does not read" ]

    describe "the generated layouts module" $ do
      cr <- runIO (either (throwString . show) pure (compileIR Scott ir))
      let lns = T.lines (fromMaybe "" (lookup "Hsed.Gen.Core.Layouts" (crModules cr)))
      it "declares a layout for each table, named after its caption as byteGrid names its layouts" $
        [ T.takeWhile (/= ' ') l | l <- lns, " :: Layout" `T.isSuffixOf` l, any (`T.isPrefixOf` l) ["get_", "noR", "verify", "stack", "system"] ] `shouldBe`
          [ "get_COMIDPayloadLayout", "noResponseAvailableLayout", "verify_COMID_VALIDRequestLayout", "verify_COMID_VALIDCommandResponseLayout"
          , "stack_RESETCommandRequestLayout", "stack_RESETCommandResponseLayout", "stack_RESETPendingLayout", "systemLogEntryStructureLayout" ]
      it "declares a record for each table whose layout tiles, as byteGrid's records, its reserved run to the end the layout's; none for Table 236" $ do
        [ l | l <- lns, "data " `T.isPrefixOf` l, any (`T.isInfixOf` l) ["_COMID", "_RESET", "NoResponse", "SystemLog"] ] `shouldBe`
          [ "data GET_COMIDPayload = GET_COMIDPayload", "data NoResponseAvailable = NoResponseAvailable"
          , "data VERIFY_COMID_VALIDRequest = VERIFY_COMID_VALIDRequest", "data VERIFY_COMID_VALIDCommandResponse = VERIFY_COMID_VALIDCommandResponse"
          , "data STACK_RESETCommandRequest = STACK_RESETCommandRequest", "data STACK_RESETCommandResponse = STACK_RESETCommandResponse"
          , "data STACK_RESETPending = STACK_RESETPending" ]
        lns `shouldContain`
          [ "data STACK_RESETCommandRequest = STACK_RESETCommandRequest"
          , "  { stack_RESETCommandRequestExtendedComID :: !Word32  -- ^ bytes 0 to 3"
          , "  }"
          , "  deriving (Eq, Show)"
          , ""
          , "instance LayoutRecord STACK_RESETCommandRequest where"
          , "  recordLayout _ = stack_RESETCommandRequestLayout" ]
      it "says in the comment of Table 236's layout which bytes no field covers" $
        lns `shouldContain`
          [ "-- It does not tile (C-LAYOUT-TILE): Hsed.Core.Layout refuses to encode or decode it, and it has"
          , "-- no record, until an erratum names these bytes:", "--", "-- * no field covers byte 8 bit 7 to byte 8 bit 0", "systemLogEntryStructureLayout :: Layout" ]
      it "prints the fields, the pins, the tail, and the values the layout does not hold, as printed and in ASCII" $ do
        lns `shouldContain`
          [ "      , spanning \"Request Code\" (4, 7) (7, 0) (Pinned 0x01)"
          , "      , spanning \"Reserved\" (8, 7) (9, 0) Reserved"
          , "      , spanning \"Available Data Length in bytes\" (10, 7) (11, 0) (Pinned 0x22)" ]
        lns `shouldContain` ["  , layoutTail   = Just (Tail \"Reserved\" Nothing 1)"]
        lns `shouldContain` ["-- > \"16 to 25\" | \"Absolute time of allocation relative to last reset of TPer\" | \"10 byte format \\8211 see Table 34.\""]
        lns `shouldContain` ["-- > \"12 to 15\" | \"Success/Failure\" | \"00 00 00 00/ 00 00 00 01\""]
        filter (T.any (> '\DEL')) lns `shouldBe` []

    describe "the runtime" $
      it "encodes T33 with its pins and reserved bytes, and decodes it strictly" $ do
        let l = bfLayout (readOf ir 33)
            time k = Octets (B.pack (map fromIntegral [k .. k + 9 :: Int]))
            values = [ ("Extended ComID", Number 0x07FE0000), ("Current state of Extended ComID", Number 2)
                     , ("Absolute time of allocation relative to last reset of TPer", time 1)
                     , ("Absolute time of expiry relative to last reset of TPer", time 11)
                     , ("Time since last reset of TPer", time 21) ]
        bytes <- either (throwString . show) pure (encode l values)
        B.length bytes `shouldBe` 46
        B.unpack (B.take 12 bytes) `shouldBe` [0x07, 0xFE, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x22]
        fmap decodedFields (decodeStrict l bytes) `shouldBe` Right
          ( take 1 values <> [("Request Code", Number 1), ("Available Data Length in bytes", Number 0x22)]
            <> drop 1 values <> [("Reserved", Octets "")] )
