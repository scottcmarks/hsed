{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Readers.CellSpec
Description : Test specifications for the column reader's M4b kinds and an SSC's cell conventions
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The column reader as M4b extends it for an SSC's preconfiguration tables:
the kinds read over Core's types (password, table_or_object_ref, ACL and
SSC, AC_element over ACE_expression, ACE_columns, byte_table_ref, date,
@\<t\>_def_\<v\>@), what still is not read; each kind's cells read
('readCellWith': labels as printed, by key and by their first letters,
remarked; names over line breaks; Null; lists; an expression into postfix,
Opal Table 23's and Core Table 210's, and the ways one does not parse; an
ACE's column names); the old 'readCell' over it; each kind's default
('zeroValue'); a list printed; and a printed cell's conventions on cells as
Opal prints them (PDF 35-52): footnote markers, @(O)@, a placeholder and the
digit after it, quotes, an empty pair, VU and MSID, an edited text over its
IR lines. TableDescSpec holds the M2b kinds. Since M5a, the Locking SP's
readings (Opal Tables 34-48, PDF 52-89): a Set_Type a list of its
enumeration's labels (Table 45's LockOnReset, PDF 86), a misspelt type read
as the type (Table 43's uniteger_8, PDF 85), an ACE's column read by the end
of its name (Table 39's DoneOnReset, PDF 81), a bare NN NN template UID
(Tables 45, 47, 48, PDF 87-89), SID or MSID (Table 41, PDF 84), a least value
(Table 36, PDF 54), one of two names (Table 45's ActiveKey, PDF 86), and the
reviewed cells whose note's number is joined to the value (Tables 41 and 44,
PDF 84, 86).
-}
module Hsed.Spec.Readers.CellSpec (spec) where

import           Data.Aeson                (fromJSON, object, (.=))
import qualified Data.Aeson                as A
import qualified RIO.Text                  as T
import           RIO
import           Test.Hspec

import           Hsed.Core.Types           (Operand (..), TableRef (..), TypeDef (..), TypeRef (..))
import           Hsed.Spec.Check           (mkContext)
import           Hsed.Spec.Anchor          (AnchorId (..))
import           Hsed.Spec.Common          (ErrataSet (..), SpecId (..))
import qualified Hsed.Spec.Common          as Cm
import           Hsed.Spec.CoreIR
import           Hsed.Spec.IR              (CellIR)
import           Hsed.Spec.Readers.Cell
import           Hsed.Spec.Rules.TableDesc (typeEnv)


-- | A data cell printed over lines, its IR text the lines joined by a space,
--   as the extractor writes it; or with a text of its own, as an erratum or
--   a test's edit leaves it.
cell :: [Text] -> CellIR
cell ls = cellWith (T.unwords ls) ls

cellWith :: Text -> [Text] -> CellIR
cellWith t ls = case fromJSON (object ["kind" .= ("TD" :: Text), "text" .= t, "lines" .= ls]) of
  A.Success c -> c
  A.Error e   -> error ("cellWith: " <> e)

admins, sid, anybody, user1, aceSPSID, aceAdmin, nullUID :: [Word8]
admins   = [0, 0, 0, 9, 0, 0, 0, 2]
sid      = [0, 0, 0, 9, 0, 0, 0, 6]
anybody  = [0, 0, 0, 9, 0, 0, 0, 1]
user1    = [0, 0, 0, 9, 0, 3, 0, 1]
aceSPSID = [0, 0, 0, 8, 0, 3, 0, 2]
aceAdmin = [0, 0, 0, 8, 0, 0, 0, 2]
nullUID  = replicate 8 0

-- | A few authorities and ACEs by name, and two of C_PIN's columns, by
--   name or by number (the reader asks the resolver for both since the M4b
--   review, so that a number too is held to the ACE's table).
resolver :: Resolver
resolver = Resolver
  { rvRef    = \n _ -> maybe (Left (tshow n <> " names no row")) (\u -> Right (n, u)) (lookup n rows)
  , rvColumn = \n -> case lookup n [("UID", 0), ("PIN", 3), ("0x00", 0), ("0x03", 3)] of
                      Just c -> Right (c, Nothing)
                      Nothing
                        | n == "DoneOnReset" -> Right (3, Just "MBRDoneOnReset")
                        | otherwise          -> Left (tshow n <> " names no column") }
  where rows = [ ("Admins", admins), ("SID", sid), ("Anybody", anybody), ("User1", user1)
               , ("ACE_SP_SID", aceSPSID), ("ACE_Admin", aceAdmin) ]

boolean, tableKind, lifeCycle, authMethod :: ColKind
boolean    = KEnum "boolean" [(0, 1)] [("False", 0), ("True", 1)]
tableKind  = KEnum "table_kind" [(1, 8)] [("Object", 1), ("Byte", 2), ("Reserved", 3)]
lifeCycle  = KEnum "life_cycle_state" [(0, 15)] [("manufactured-inactive", 8), ("manufactured", 9)]
authMethod = KEnum "auth_method" [(0, 23)] [("None", 0), ("Password", 1), ("Exchange", 2), ("Sign", 3), ("SymK", 4)]

-- | An AC_element's kind: boolean_ACE's operators (Core Table 61, PDF 104) over authorities.
expr :: ColKind
expr = KExpr "boolean_ACE" [("And", 0), ("Or", 1), ("Not", 2)] [Table "Authority"]

readWith :: ColKind -> Text -> Either Problem (CellValue, [Remark])
readWith = readCellWith resolver

spec :: Spec
spec = do
  withCore $ \pristine -> do
    hsa <- runIO committedHsa
    ir  <- runIO (annotated pristine hsa)
    let kind = colKind (typeEnv (mkContext Scott ir))
    describe "colKind, M4b's kinds over Core's types" $ do
      it "reads password (Table 122, PDF 118) as a credential's bytes and table_or_object_ref (Table 139, PDF 122) as a UID" $ do
        kind "password" `shouldBe` Right (KPassword 32)
        kind "table_or_object_ref" `shouldBe` Right KUIDRef
      it "reads a List_Type as a list of its element's kind: ACL (Table 50, PDF 101) of ACE references, SSC (Table 132) of names" $ do
        kind "ACL" `shouldBe` Right (KList (KRef [Table "ACE"]))
        kind "SSC" `shouldBe` Right (KList (KText 32))
      it "reads AC_element (Table 46) over ACE_expression (Table 48) as an expression of authorities and boolean_ACE's operators" $
        kind "AC_element" `shouldBe` Right expr
      it "reads ACE_columns (Table 47) as column names, byte_table_ref (Table 63) as a byte table's reference, date (Table 78) as a struct" $ do
        kind "ACE_columns" `shouldBe` Right KColumns
        kind "byte_table_ref" `shouldBe` Right KByteTableRef
        kind "date" `shouldBe` Right (KStruct "date")
      it "reads <t>_def_<v>, which names no Type object, as t defaulting to v (Table 215's boolean_def_false, PDF 232-233)" $ do
        kind "boolean_def_false" `shouldBe` Right (KDefault "false" boolean)
        kind "boolean_def_true" `shouldBe` Right (KDefault "true" boolean)
        kind "int_1_def_0" `shouldBe` Left "int_1_def_0 names no Type object"
      it "still refuses what it does not read: an alternative that is not of references" $ do
        kind "ACE_expression" `shouldBe` Left "a column of type ACE_expression (Alternative_Type) is not read from a table cell"
        kind "log_kind" `shouldBe` Left "log_kind names no Type object"
      it "reads a Set_Type as a list of its range's values, labelled by its enumeration (M5a: reset_types, Core Tables 127-128, PDF 119)" $ do
        fmap (\case KList (KEnum n rs ls) -> (n, rs, take 3 ls); k -> (tshow k, [], [])) (kind "reset_types")
          `shouldBe` Right ("reset_types", [(0, 31)], [("Power Cycle", 0), ("Hardware", 1), ("HotPlug", 2)])
        fmap (\case KList (KEnum n rs ls) -> (n, rs, take 2 ls); k -> (tshow k, [], [])) (kind "gen_status")
          `shouldBe` Right ("gen_status", [(0, 63)], [("None", 0), ("pending_tper_error", 1)])
        kind "protect_types" `shouldBe` Right (KList (KEnum "protect_types" [(0, 255)] []))
        setKind (typeEnv (mkContext Scott ir)) "reset_types" `shouldBe` Just "reset_types"
        setKind (typeEnv (mkContext Scott ir)) "protect_types" `shouldBe` Nothing
      it "reads a type cellTypos lists as the type it misspells (M5a: Opal Table 43's uniteger_8, PDF 85)" $ do
        cellTypos `shouldBe` [("uniteger_8", "uinteger_8")]
        kind "uniteger_8" `shouldBe` kind "uinteger_8"
        kind "uinteger_8" `shouldBe` Right (KUInt 8)
      it "reads Opal Table 45's LockOnReset, Power Cycle (PDF 86), as a set of one label, its default the empty set" $ do
        let resetTypes = either (const KUID) id (kind "reset_types")
        readWith resetTypes "Power Cycle" `shouldBe` Right (CvList [CvEnum "Power Cycle" 0], [])
        readWith resetTypes "Power Cycle, HotPlug" `shouldBe` Right (CvList [CvEnum "Power Cycle" 0, CvEnum "HotPlug" 2], [])
        fmap (renderCell . fst) (readWith resetTypes "Power Cycle") `shouldBe` Right "ListCell [EnumCell \"Power Cycle\" 0]"
        zeroValue resetTypes `shouldBe` Just (CvList [])

  describe "readCellWith: labels" $ do
    it "reads a label as printed, a number its ranges hold, and no remark" $ do
      readWith boolean "False" `shouldBe` Right (CvEnum "False" 0, [])
      readWith boolean "1" `shouldBe` Right (CvEnum "1" 1, [])
    it "reads a label in another case or spacing by its key, and one letter by the one label it begins, each remarked (Opal T18-T29)" $ do
      readWith boolean "FALSE" `shouldBe` Right (CvEnum "FALSE" 0, [LabelRead "boolean" "FALSE" "False" False])
      readWith boolean "T" `shouldBe` Right (CvEnum "T" 1, [LabelRead "boolean" "T" "True" True])
      readWith lifeCycle "Manufactured" `shouldBe` Right (CvEnum "Manufactured" 9, [LabelRead "life_cycle_state" "Manufactured" "manufactured" False])
    it "joins a label's line break with nothing where that is the label (Opal T20's Obje|ct, T29's Manufactured-|Inactive, PDF 36, 50)" $ do
      readWith tableKind "Obje\nct" `shouldBe` Right (CvEnum "Object" 1, [])
      readWith lifeCycle "Manufactured-\nInactive"
        `shouldBe` Right (CvEnum "Manufactured-Inactive" 8, [LabelRead "life_cycle_state" "Manufactured-Inactive" "manufactured-inactive" False])
    it "refuses a text no label is, nor begins alone" $ do
      readWith boolean "Maybe" `shouldBe` Left (Unread "\"Maybe\" is no label of boolean's enumeration (False, True)")
      readWith authMethod "S" `shouldBe` Left (Unread "\"S\" is no label of auth_method's enumeration (None, Password, Exchange, Sign, SymK)")
    it "matches a label by key, else by the one label it begins" $ do
      matchLabel [("False", 0), ("True", 1)] "t" `shouldBe` Just ("True", 1, True)
      matchLabel [("False", 0), ("True", 1)] "TRUE" `shouldBe` Just ("True", 1, False)
      matchLabel [("Sign", 3), ("SymK", 4)] "S" `shouldBe` Nothing
      matchLabel [("False", 0)] "" `shouldBe` Nothing

  describe "readCellWith: texts, references, passwords and UIDs" $ do
    it "joins a name's line break with nothing (Opal T24's C_PIN_Admin|1, PDF 48)" $
      readWith (KText 32) "C_PIN_Admin\n1" `shouldBe` Right (CvText "C_PIN_Admin1", [])
    it "keeps a text longer than its type, remarked (Opal T23's 40-byte ACE name, PDF 47)" $
      readWith (KText 32) "ACE_TPerInfo_Set_ProgrammaticResetEnable"
        `shouldBe` Right (CvText "ACE_TPerInfo_Set_ProgrammaticResetEnable", [OverLength "ACE_TPerInfo_Set_ProgrammaticResetEnable" 40 32])
    it "reads Null in a reference column as the NULL UID (Core PDF 52, 96), a name through the resolver, or why not" $ do
      readWith (KRef [Table "Authority"]) "Null" `shouldBe` Right (CvRef "Null" nullUID, [])
      readWith (KRef [Table "Authority"]) "Admins" `shouldBe` Right (CvRef "Admins" admins, [])
      readWith (KRef [Table "Authority"]) "Makers" `shouldBe` Left (Unresolved "\"Makers\" names no row")
    it "reads a password as its bytes, an empty pair as none (Opal T25, PDF 49), and refuses one longer than its type" $ do
      readWith (KPassword 32) "" `shouldBe` Right (CvBytes [], [])
      readWith (KPassword 32) "MSID" `shouldBe` Right (CvBytes [0x4D, 0x53, 0x49, 0x44], [])
      readWith (KPassword 4) "secret" `shouldBe` Left (Unread "\"secret\" is longer than 4 bytes")
    it "reads an invoking ID as a UID over its lines, or Null" $ do
      readWith KUIDRef "00 00 00 0B\n00 00 00 01" `shouldBe` Right (CvUID [0, 0, 0, 0x0B, 0, 0, 0, 1], [])
      readWith KUIDRef "Null" `shouldBe` Right (CvRef "Null" nullUID, [])
      readWith KUIDRef "C_PIN_SID" `shouldBe` Left (Unread "\"C_PIN_SID\" is not the UID of a table or an object, eight bytes in hex")
    it "reads a byte table's reference as Null alone, and a struct blank alone" $ do
      readWith KByteTableRef "Null" `shouldBe` Right (CvRef "Null" nullUID, [])
      readWith KByteTableRef "Default" `shouldSatisfy` isLeft
      readWith (KStruct "date") "2026-10-31"
        `shouldBe` Left (Unread "\"2026-10-31\" is not read: a column of type date (Struct_Type) is read blank, its default an empty struct")
    it "reads <t>_def_<v> as t" $
      readWith (KDefault "false" boolean) "FALSE" `shouldBe` readWith boolean "FALSE"

  describe "readCellWith: lists" $ do
    it "reads an ACL's names over a line break (Opal T22's ACE_SP_SID, ACE_Admin, PDF 45)" $ do
      readWith (KList (KRef [Table "ACE"])) "ACE_SP_SID,\nACE_Admin"
        `shouldBe` Right (CvList [CvRef "ACE_SP_SID" aceSPSID, CvRef "ACE_Admin" aceAdmin], [])
      readWith (KList (KRef [Table "ACE"])) "ACE_Nobody" `shouldBe` Left (Unresolved "\"ACE_Nobody\" names no row")
    it "reads a bracketed list (Opal T27's [“Opal”], its quotes removed, PDF 50)" $
      readWith (KList (KText 32)) "[Opal]" `shouldBe` Right (CvList [CvText "Opal"], [])
    it "gives a list's items, joined as the element kind joins its lines" $ do
      listItems (KRef [Table "ACE"]) "ACE_SP_SID,\nACE_Admin" `shouldBe` ["ACE_SP_SID", "ACE_Admin"]
      listItems (KText 32) "[Opal]" `shouldBe` ["Opal"]
      listItems (KRef []) "" `shouldBe` []

  describe "readCellWith: an ACE's BooleanExpr, infix to postfix (Core 5.3.4.3.2, PDF 226-227)" $ do
    it "reads Opal T23's Admins OR SID, printed over two lines (PDF 47), as Admins SID Or, remarking OR" $
      readWith expr "Admins\nOR SID"
        `shouldBe` Right (CvList [CvRef "Admins" admins, CvRef "SID" sid, CvEnum "OR" 1], [LabelRead "boolean_ACE" "OR" "Or" False])
    it "binds Not before And before Or, left to right" $
      fmap (map renderCell . (\(v, _) -> case v of CvList vs -> vs; _ -> [])) (readWith expr "Admins And Not SID Or Anybody")
        `shouldBe` Right [ "RefCell \"Admins\" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)", "RefCell \"SID\" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x06)"
                         , "EnumCell \"Not\" 2", "EnumCell \"And\" 0", "RefCell \"Anybody\" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x01)"
                         , "EnumCell \"Or\" 1" ]
    it "groups by parentheses: Table 210's ((a AND b) OR c) AND d is a b AND c OR d AND (PDF 226-227)" $
      fmap (map labelOf . (\(v, _) -> case v of CvList vs -> vs; _ -> [])) (readWith expr "((Admins AND SID) OR Anybody) AND User1")
        `shouldBe` Right ["Admins", "SID", "AND", "Anybody", "OR", "User1", "AND"]
    it "joins an authority's name a line break parts, but not a name and an operator" $
      fmap (map labelOf . (\(v, _) -> case v of CvList vs -> vs; _ -> [])) (readWith expr "Admins Or Use\nr1")
        `shouldBe` Right ["Admins", "User1", "Or"]
    it "refuses an expression that does not parse (C-ACL-EXPR), and an authority it does not know (C-ACL-REFS)" $ do
      readWith expr "Admins XOR SID" `shouldBe` Left (BadExpr "\"XOR\" follows an authority where an operator (And, Or, Not) is expected")
      readWith expr "Admins OR" `shouldBe` Left (BadExpr "the expression ends where an authority is expected")
      readWith expr "OR SID" `shouldBe` Left (BadExpr "\"OR\" has no authority before it")
      readWith expr "(Admins OR SID" `shouldBe` Left (BadExpr "a parenthesis opens a group no parenthesis closes")
      readWith expr "Admins OR SID)" `shouldBe` Left (BadExpr "a closing parenthesis closes no group")
      readWith expr "Admins Not SID" `shouldBe` Left (BadExpr "\"Not\", which takes one operand, follows an authority")
      readWith expr "Admins OR Nobody" `shouldBe` Left (Unresolved "\"Nobody\" names no row")

  describe "colKind through a Format that names its own type (the M4b review)" $
    it "refuses it, naming the types it went through, where it once read for ever" $ do
      let env' = TypeEnv (`lookup` [ ("loop", Right (ListType (Lit 4) (Ref "loop")))
                                   , ("a", Right (ListType (Lit 4) (Ref "b"))), ("b", Right (ListType (Lit 2) (Ref "a"))) ]) (const [])
      colKind env' "loop" `shouldBe` Left "loop's Format names its own type, through loop -> loop"
      colKind env' "a" `shouldBe` Left "a's Format names its own type, through a -> b -> a"

  describe "readCellWith: an ACE's Columns (Core 5.3.2.9.5, PDF 165)" $ do
    it "reads All as the empty set, every column" $
      readWith KColumns "All" `shouldBe` Right (CvList [], [])
    it "reads column names and numbers, joined over line breaks, as the numbers the column resolver gives, which holds a number to the table too" $ do
      readWith KColumns "UID,\nPIN" `shouldBe` Right (CvList [CvUInt 0 Nothing, CvUInt 3 Nothing], [])
      readWith KColumns "0x03" `shouldBe` Right (CvList [CvUInt 3 Nothing], [])
      readWith KColumns "Colour" `shouldBe` Left (Unresolved "\"Colour\" names no column")
      readWith KColumns "0x63" `shouldBe` Left (Unresolved "\"0x63\" names no column")
    it "remarks a column the resolver reads by the end of its name (M5a: Opal Table 39's DoneOnReset, PDF 81)" $
      readWith KColumns "UID, DoneOnReset" `shouldBe` Right (CvList [CvUInt 0 Nothing, CvUInt 3 Nothing], [ColumnRead "DoneOnReset" "MBRDoneOnReset"])

  describe "readCell, the reader without remarks" $
    it "reads as readCellWith does, references by the caller's function" $ do
      readCell (\_ _ -> Left "no references here") boolean "FALSE" `shouldBe` Right (CvEnum "FALSE" 0)
      readCell (\_ _ -> Right admins) (KRef [Table "Authority"]) "Admins" `shouldBe` Right (CvRef "Admins" admins)
      readCell (\_ _ -> Left "no references here") (KRef [Table "Authority"]) "Admins" `shouldBe` Left "no references here"
      readCell (\_ _ -> Left "no references here") KColumns "PIN" `shouldSatisfy` isLeft
      readCell (\_ _ -> Left "no references here") KColumns "0x03" `shouldBe` Right (CvList [CvUInt 3 Nothing])

  describe "zeroValue, a column's default (design 13.31 (a))" $ do
    it "is 0, an empty text or password, zero bytes, the NULL UID for a reference" $ do
      map zeroValue [KUID, KBytes 2, KText 32, KUInt 4, KInt 1, KPassword 32]
        `shouldBe` map Just [CvUID nullUID, CvBytes [0, 0], CvText "", CvUInt 0 Nothing, CvInt 0, CvBytes []]
      map zeroValue [KRef [Table "Authority"], KByteTableRef, KUIDRef]
        `shouldBe` map Just [CvRef "Null" nullUID, CvRef "Null" nullUID, CvUID nullUID]
    it "is an enumeration's lowest value with its label, an empty list, set or struct" $ do
      map zeroValue [boolean, tableKind, KEnum "x" [(0, 3)] []] `shouldBe` map Just [CvEnum "False" 0, CvEnum "Object" 1, CvEnum "0" 0]
      map zeroValue [KList (KText 32), expr, KColumns, KStruct "date"] `shouldBe` replicate 4 (Just (CvList []))
    it "is <t>_def_<v>'s v, with its label as the enumeration gives it (Frozen False, Core PDF 233)" $ do
      map zeroValue [KDefault "false" boolean, KDefault "true" boolean, KDefault "0" (KInt 1)]
        `shouldBe` map Just [CvEnum "False" 0, CvEnum "True" 1, CvInt 0]
      zeroValue (KDefault "maybe" boolean) `shouldBe` Nothing

  describe "renderCell" $
    it "prints a list as a ListCell of its items" $ do
      renderCell (CvList [CvRef "Admins" admins, CvEnum "OR" 1])
        `shouldBe` "ListCell [RefCell \"Admins\" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02), EnumCell \"OR\" 1]"
      renderCell (CvList [CvUInt 0 Nothing, CvUInt 3 Nothing]) `shouldBe` "ListCell [UIntCell 0, UIntCell 3]"
      renderCell (CvList []) `shouldBe` "ListCell []"

  describe "printedCell, an SSC's cell conventions (Opal 2.30, PDF 35-52)" $ do
    it "keeps a template's placeholder, the digit printed after it and (O) apart from the UID (Table 24's AdminXX, PDF 48)" $
      printedCell (cell ["00 00 00 09", "00 00 02 00", "(+XX)1", "(O)"])
        `shouldBe` PrintedCell ["00 00 00 09", "00 00 02 00"] False ["1"] True (Just "XX") False
    it "keeps a footnote marker apart, a wildcard's TT TT TT TT in the value (Table 22's *AC1, PDF 39)" $
      printedCell (cell ["00 00 00 01", "TT TT TT TT", "*AC1"]) `shouldBe` PrintedCell ["00 00 00 01", "TT TT TT TT"] False ["*AC1"] False Nothing False
    it "reads M5's two-byte placeholders as printed (Table 39's (+NN NN), PDF 79)" $
      pcPlaceholder (printedCell (cell ["00 00 00 08", "00 04 40 00", "(+NN NN)"])) `shouldBe` Just "NN NN"
    it "reads a UID printing its placeholder's letters for its last bytes as that template at its base (M5a: Tables 45, 47, 48, PDF 87-89)" $ do
      printedCell (cell ["00 00 08 05 00 03 NN NN (O)"]) `shouldBe` PrintedCell ["00 00 08 05 00 03 00 00"] False [] True (Just "NN NN") True
      printedCell (cell ["00 00 08 02", "00 03 NN NN"]) `shouldBe` PrintedCell ["00 00 08 02", "00 03 00 00"] False [] False (Just "NN NN") True
      map (pcBare . printedCell . cell) [["00 00 00 01 TT TT TT TT"], ["00 00 08 02 00 03 NN MM"], ["NN NN"], ["00 00 08 02 00 03 AA AA"]]
        `shouldBe` [False, False, False, False]
    it "removes quotes, an unbalanced one too (Table 20 row 12, PDF 37), and keeps an empty pair as an empty value (Table 25, PDF 49)" $ do
      joinedText (Just (KText 32)) (printedCell (cell ["DataRemovalMecha", "nism\x201D"])) `shouldBe` "DataRemovalMechanism"
      let empty = printedCell (cell ["\x201C\x201D"])
      (pcLines empty, pcQuoted empty, hasValue empty, printedText empty) `shouldBe` ([], True, True, "")
      pcLines (printedCell (cell ["[\x201COpal\x201D]", "*TP3"])) `shouldBe` ["[Opal]"]
    it "gives no value for a cell of markers alone (Table 23's *ACE1, PDF 47)" $ do
      hasValue (printedCell (cell ["*ACE1"])) `shouldBe` False
      pcNotes (printedCell (cell ["*ACE1"])) `shouldBe` ["*ACE1"]
    it "reads VU and MSID, unquoted and alone, as values the TPer chooses (Table 1, PDF 12; Table 25, PDF 48)" $
      map (vendorMarker . printedCell . cell) [["VU", "*TP1"], ["MSID"], ["\"VU\""], ["VU VU"]] `shouldBe` [Just "VU", Just "MSID", Nothing, Nothing]
    it "reads SID or MSID as a value the TPer chooses (M5a: the Locking SP's C_PIN_Admin1, Opal Table 41 note 1, PDF 84)" $
      map (vendorMarker . printedCell . cell) [["SID or", "MSID"], ["SID or MSID1"], ["\"SID or MSID\""]] `shouldBe` [Just "SID or MSID", Nothing, Nothing]
    it "reads a least value before min as one the TPer chooses (M5a: Opal Table 36's MBR and DataStore Rows, PDF 54)" $ do
      map (vendorAtLeast . printedCell . cell) [["0x08000000 min"], ["0x00A00000", "min"]]
        `shouldBe` [Just (0x08000000, "0x08000000 min"), Just (0x00A00000, "0x00A00000 min")]
      map (vendorAtLeast . printedCell . cell) [["0x08000000"], ["\"0x08000000 min\""], ["VU min"]] `shouldBe` [Nothing, Nothing, Nothing]
    it "reads a name with bracketed characters as two names (M5a: Opal Table 45's ActiveKey, PDF 86)" $ do
      alternativeNames "K_AES_128[256]_GlobalRange_Key" `shouldBe` Just ["K_AES_128_GlobalRange_Key", "K_AES_256_GlobalRange_Key"]
      map alternativeNames ["Admins", "[256]", "K_AES_128[256]_[1]"] `shouldBe` [Nothing, Nothing, Nothing]
    it "splits the note's number off the two reviewed cells, and off no other (M5a: Opal Tables 41 and 44, PDF 84, 86)" $ do
      let opal = SpecId "opal-2.30"
          noted t r h = fmap (\(k, pc) -> (k, pcLines pc)) (noteMarked opal (TableId t) r h (printedCell (cell [h'])))
            where h' = fromMaybe "" (listToMaybe [ x | (_, TableId t', r', hd, x, _) <- noteMarkerCells, t' == t, r' == r, hd == h ])
      noteMarkerCells `shouldBe` [(opal, TableId 41, 1, "PIN", "SID or MSID1", 1), (opal, TableId 44, 1, "MaxRanges", "81", 1)]
      noted 41 1 "PIN" `shouldBe` Just (1, ["SID or MSID"])
      noted 44 1 "MaxRanges" `shouldBe` Just (1, ["8"])
      noteMarked opal (TableId 44) 2 "MaxRanges" (printedCell (cell ["81"])) `shouldBe` Nothing
      noteMarked opal (TableId 44) 1 "MaxRanges" (printedCell (cell ["8"])) `shouldBe` Nothing
      noteMarked (SpecId "core-2.01") (TableId 44) 1 "MaxRanges" (printedCell (cell ["81"])) `shouldBe` Nothing
    it "reads an edited text, not the IR lines it no longer joins" $
      pcLines (printedCell (cellWith "ACE_Nobody" ["ACE_C_PIN_SID_Get_NOPIN"])) `shouldBe` ["ACE_Nobody"]
    it "joins its lines as a kind does: an identifier's with nothing, anything else's with a space" $ do
      joinedText (Just (KText 32)) (printedCell (cell ["C_PIN_Admin", "1"])) `shouldBe` "C_PIN_Admin1"
      joinedText Nothing (printedCell (cell ["Admins", "OR SID"])) `shouldBe` "Admins OR SID"
      printedText (printedCell (cell ["Admins", "OR SID"])) `shouldBe` "Admins\nOR SID"

  describe "cellCodes" $
    it "claims the cell reader's remarks' codes" $
      map Cm.codeName cellCodes `shouldBe` ["N-PRECONF-LABEL", "N-PRECONF-NOTE", "N-CELL-NORMALISED", "W-PRECONF-NAME-LENGTH"]
  where
    labelOf = \case
      CvRef n _  -> n
      CvEnum l _ -> l
      other      -> tshow other
