{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Hsed.Gen.Core.EquivSpec
Description : The generated Core modules beside the hand transcription of TypeUIDs, TableUIDs and SpecialPurposeUIDs, at run time
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design docs/spec-as-source.md section 5.5, the regenerate-and-compare oracle
(M2a): every definition @hsedc compile@ generated from the PDF (every module
under @gen/Hsed/Gen/Core@, each imported here as @G@) beside its
counterpart in the hand transcription (@Hand.{TypeUIDs,TableUIDs,SpecialPurposeUIDs}@,
the test fixture hsed/fixture/Hand, as @H@), definition by definition and
table caption by table caption
("Hsed.Gen.Core.EquivTH"). The differences must be exactly the lines of
@spec/expected-diff.txt@: a missing line fails the test as surely as an extra
one, and each line is checked by value, its kind holding of the two sides and
its value column equal to what this module computes for the difference
('summary'):

* @pdf-uid@: the generated UID, the one the PDF prints (hsed transcribed another);
* @spacing@: the same once spaces and underscores are dropped; the generated value;
* @range-from-1@: an enumeration whose range starts at 1, the hand-written one
  with @_NA@ fillers from 0; the constructors only on either side;
* @typed-half-uid@: a hand-written @oth\<T\>@ or @bth\<T\>@ with no generated
  counterpart, its bytes those of the generated @h\<T\>@; the hand-written value;
* @new@: a generated definition the hand transcription lacks; its value (an
  enumeration's size, range and a digest of its constructor names, so that a
  binding that names other constructors cannot pass);
* @unnamed@: a hand-written UID with no generated counterpart of its name,
  from a table whose rows the PDF names nowhere (Table 239's Purpose column),
  its value among the UIDs the generated list of that table holds; the
  hand-written value;
* @caption@: a table whose caption the two sides print differently (spacing
  aside); @generated / hand-written@.

Two rules of M2b have a hand counterpart other than a definition: the
method status codes (Core Table 166, @statusCodes@) are what hsed-core's codec
carries as a bare uinteger in a response's status list, so every one of them
is encoded and decoded by the codec here and the values the table leaves
unassigned are listed; and each field of a typed record (Tables 22, 23 and 34,
@fieldType@) names a Type object whose UID the hand transcription has.

The M1 slice is kept by name above it (M1 acceptance (d), less its Format
strings, retired with @f\<Name\>Type@ at M2d on both sides). The
hand-written side is the 2019 transcription, kept since M2d as the test
fixture hsed/fixture/Hand and compiled only by this test suite; hsed's
library re-exports the generated modules in its place, so 'handDefs' reads
the fixture (design sections 5.5 and 11, M2d).
-}
module Hsed.Gen.Core.EquivSpec (spec) where

import           Data.Bits                                 (xor)
import           Data.Char                                 (isDigit, isSpace, ord)
import           Data.List                                 (foldl', intercalate, isPrefixOf, nub, sort, stripPrefix, (\\))
import qualified Data.Map                                  as Map
import           Data.Maybe                                (isJust, isNothing, mapMaybe)
import           Data.Word                                 (Word32, Word64)
import           GHC.Exts                                  (toList)
import           Numeric                                   (showHex)
import           Test.Hspec

import qualified RIO.ByteString                            as B
import qualified RIO.Text                                  as T

import qualified Hand.SpecialPurposeUIDs                   as H
import qualified Hand.TableUIDs                            as H
import qualified Hand.TypeUIDs                             as H
import           Hsed.Core.Code                            (CodeRow (..), Span (..))
import           Hsed.Core.Field                           (FieldDef (..))
import           Hsed.Core.Types                           (Enumerated (..), enumValue)
import qualified Hsed.Gen.Core.Codes                       as G
import qualified Hsed.Gen.Core.Enums                       as G
import           Hsed.Gen.Core.EquivTH
import qualified Hsed.Gen.Core.Fields                      as G
import qualified Hsed.Gen.Core.TableUIDs                   as G
import qualified Hsed.Gen.Core.Types                       as G
import qualified Hsed.Gen.Core.UIDRanges                   as G
import qualified Hsed.Gen.Core.UIDs                        as G
import           System.SED.MCTP.Common.Call               (MethodResponse (..))
import           System.SED.MCTP.Common.StreamItem         (StreamItem (..))
import           System.SED.MCTP.Common.UID                (UID, uidLower)


-- | Every generated definition a hand-written module could have.
generated :: [Def]
generated = $(generatedDefs "gen/Hsed/Gen/Core" "G")

-- | Every generated list of a table's UIDs: the table, and each UID shown.
generatedLists :: [(String, [String])]
generatedLists = $(generatedUIDLists "gen/Hsed/Gen/Core" "G")

-- | Every hand-written definition the fixture's quasiquoters make, and its special purpose UIDs.
handWritten :: [Def]
handWritten = $(handDefs "fixture/Hand/TypeUIDs.hs" "fixture/Hand/TableUIDs.hs" "fixture/Hand/SpecialPurposeUIDs.hs" "H")


-- * Differences

-- | A difference between the two sides: of one definition (either side may
--   lack it), or of the caption of one generated table.
data Difference = Difference
  { subject       :: String                  -- ^ the definition's name, or the table's id
  , generatedSide :: Maybe Def
  , handSide      :: Maybe Def
  , captions      :: Maybe (String, String)  -- ^ a caption difference: generated, hand-written
  }

byName :: [Def] -> Map.Map String Def
byName ds = Map.fromList [ (dName d, d) | d <- ds ]

differences :: [Difference]
differences = valueDifferences ++ captionDifferences
  where
    gens  = byName generated
    hands = byName handWritten
    valueDifferences =
      [ Difference n g h Nothing
      | n <- Map.keys (Map.union gens hands), let g = Map.lookup n gens; h = Map.lookup n hands
      , fmap dValue g /= fmap dValue h ]
    -- each generated table's caption against those of its definitions' hand-written counterparts
    tables = Map.fromListWith (\(c, hs) (_, hs') -> (c, hs' ++ hs))
      [ (dTable g, (dCaption g, [dCaption h])) | (n, g) <- Map.toList gens, Just h <- [Map.lookup n hands] ]
    captionDifferences =
      [ Difference t Nothing Nothing (Just (gc, hc))
      | (t, (gc, hcs)) <- Map.toList tables, hc <- nub (map normal hcs), normal gc /= hc ]

normal :: String -> String
normal = unwords . words

-- | The value column of a difference's line.
summary :: Difference -> String
summary d = case (captions d, generatedSide d, handSide d) of
  (Just (g, h), _, _)  -> normal g ++ " / " ++ normal h
  (_, Just g, Just h)  -> case (dValue g, dValue h) of
    (Entries a, Entries b) -> "+" ++ show (a \\ b) ++ " -" ++ show (b \\ a)
    (a, _)                 -> value a
  (_, Just g, Nothing) -> value (dValue g)
  (_, Nothing, Just h) -> value (dValue h)
  _                    -> ""
  where
    value (Scalar s)   = s
    value (Entries es) = show (length es) ++ " constructors, values " ++ show (minimum (map snd es)) ++ ".." ++ show (maximum (map snd es))
                         ++ ", names fnv1a64:" ++ digest (intercalate "," (map fst es))

-- | FNV-1a over a string's characters, 64 bits, as sixteen hex digits: a short,
--   stable digest of an enumeration's constructor names for its @new@ line.
digest :: String -> String
digest s = let h = foldl' (\acc c -> (acc `xor` fromIntegral (ord c)) * 0x100000001b3) (0xcbf29ce484222325 :: Word64) s
               hex = showHex h ""
           in replicate (16 - length hex) '0' ++ hex

-- | Does a difference have the kind a line names?
holds :: String -> Difference -> Bool
holds kind d = case (kind, captions d, dValue <$> generatedSide d, dValue <$> handSide d) of
  ("caption", Just _, _, _) -> True
  ("pdf-uid", Nothing, Just (Scalar a), Just (Scalar b)) -> all ("uid " `isPrefixOf`) [a, b] && a /= b
  ("spacing", Nothing, Just a, Just b) -> squeeze a == squeeze b
  ("range-from-1", Nothing, Just (Entries a), Just (Entries b)) -> filter (not . isNA . fst) b == a && minimum (map snd a) >= 1
  ("typed-half-uid", Nothing, Nothing, Just (Scalar b)) ->
    case mapMaybe (`stripPrefix` subject d) ["oth", "bth"] of
      [t] | Just g <- Map.lookup ("h" ++ t) (byName generated), Scalar a <- dValue g -> drop 1 (words a) == drop 1 (words b)
      _   -> False
  ("new", Nothing, Just _, Nothing) -> True
  ("unnamed", Nothing, Nothing, Just (Scalar b)) ->
    any (\h -> b `elem` concat [ us | (t, us) <- generatedLists, t == dTable h ]) (handSide d)
  _ -> False
  where
    squeeze (Scalar s)   = Scalar (dropSpacing s)
    squeeze (Entries es) = Entries [ (dropSpacing c, v) | (c, v) <- es ]
    dropSpacing = filter (\x -> not (isSpace x) && x /= '_')
    -- Core_x_NA, Core_x_NA_4: dEnum's fillers
    isNA c = case reverse (splitOn "_" c) of
      "NA" : _                       -> True
      n : "NA" : _ | all isDigit n   -> True
      _                              -> False

-- | The lower four bytes of a UID.
lsb :: UID -> Word32
lsb = foldl' (\acc b -> acc * 256 + fromIntegral b) 0 . toList . uidLower

-- | One line of spec/expected-diff.txt: subject | kind | value | reason.
data Expected = Expected { eSubject :: String, eKind :: String, eValue :: String, eReason :: String }
  deriving (Show)

parseExpected :: String -> [Expected]
parseExpected = map line . filter (not . ignorable) . lines
  where
    ignorable l = all isSpace l || "#" `isPrefixOf` dropWhile isSpace l
    line l = case splitOn " | " l of
      s : k : v : r@(_ : _) -> Expected s k v (intercalate " | " r)
      _                     -> error ("spec/expected-diff.txt: not subject | kind | value | reason: " ++ l)

splitOn :: String -> String -> [String]
splitOn sep = go ""
  where
    go acc s | Just rest <- stripPrefix sep s = reverse acc : go "" rest
    go acc (c : cs) = go (c : acc) cs
    go acc []       = [reverse acc]

kinds :: [String]
kinds = ["pdf-uid", "spacing", "range-from-1", "typed-half-uid", "new", "unnamed", "caption"]


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
    it "has Core_adv_key_mode's constructors in the hand-written order (T52)" $ do
      map show [minBound .. maxBound :: G.Core_adv_key_mode] `shouldBe` map show [minBound .. maxBound :: H.Core_adv_key_mode]
      map fromEnum [minBound .. maxBound :: G.Core_adv_key_mode] `shouldBe` map fromEnum [minBound .. maxBound :: H.Core_adv_key_mode]
    it "ties Core_adv_key_mode to its Type object and range" $ do
      typeUID @G.Core_adv_key_mode `shouldBe` H.uadv_key_modeType
      range @G.Core_adv_key_mode `shouldBe` (0, 7)
    it "has the hand-written enumerations of Tables 54 and 56" $ do
      map show [minBound .. maxBound :: G.Core_attr_flags] `shouldBe` map show [minBound .. maxBound :: H.Core_attr_flags]
      map show [minBound .. maxBound :: G.Core_auth_method] `shouldBe` map show [minBound .. maxBound :: H.Core_auth_method]

  describe "every generated definition beside its hand-written counterpart (design 5.5)" $ do
    expected <- runIO (parseExpected <$> readFile "../spec/expected-diff.txt")
    let gens  = byName generated
        hands = byName handWritten
        matching e = [ d | d <- differences, subject d == eSubject e ]
    it "reads 383 generated definitions and 315 hand-written ones: 267 on both sides, 116 new, 48 the generated lack" $ do
      (Map.size gens, Map.size hands) `shouldBe` (383, 315)
      Map.size (Map.intersection gens hands) `shouldBe` 267
      -- new: Core_gen_status, and the UIDs of the tables hsed does not transcribe (Tables 242-248)
      [ n | (n, g) <- Map.toList (gens Map.\\ hands), dTable g `notElem` map (('T' :) . show) [242 .. 248 :: Int] ]
        `shouldBe` ["Core_gen_status"]
      Map.size (gens Map.\\ hands) `shouldBe` 116
      -- lacked: the 46 typed half-UIDs, and the two special purpose UIDs the PDF names nowhere
      Map.size (hands Map.\\ gens) `shouldBe` 48
    it "has the hand-written Session Manager method UIDs and invoking IDs (Tables 241 and 239)" $ do
      [G.uProperties, G.uStartSession, G.uSyncSession, G.uStartTrustedSession, G.uSyncTrustedSession, G.uCloseSession]
        `shouldBe` [H.uProperties, H.uStartSession, H.uSyncSession, H.uStartTrustedSession, H.uSyncTrustedSession, H.uCloseSession]
      (G.uThisSP, G.uSMUID) `shouldBe` (H.uThisSP, H.uSMUID)
      map (show . fst) G.specialPurposeUIDs `shouldContain` [show H.uNULL]
    it "finds every generated MethodID UID that names a template in that template's generated range (C-UID-RANGE)" $ do
      let templated = [ (u, template) | (u, row) <- G.methodIDUIDs, _ : template : _ <- [splitOn " | " row] ]
          inRange u t = or [ lo <= lsb u && lsb u <= hi | (n, (lo, hi)) <- G.templateRanges, n == t ]
      length templated `shouldBe` 47
      [ (show u, t) | (u, t) <- templated, not (inRange u t) ] `shouldBe` []
    it "differs from the hand transcription exactly on the lines of spec/expected-diff.txt (extra, missing)" $ do
      let actual = sort (map subject differences)
          listed = sort (map eSubject expected)
      (actual \\ listed, listed \\ actual) `shouldBe` ([], [])
    it "lists each difference once, with a kind it knows and a reason" $ do
      let listed = map eSubject expected
      listed \\ nub listed `shouldBe` []
      [ eSubject e ++ ": " ++ eKind e | e <- expected, eKind e `notElem` kinds ] `shouldBe` []
      [ eSubject e | e <- expected, all isSpace (eReason e) ] `shouldBe` []
    it "differs there in the kind each line names" $
      [ eSubject e ++ " is not " ++ eKind e | e <- expected, d <- matching e, not (holds (eKind e) d) ] `shouldBe` []
    it "differs there by the value each line records" $
      [ (eSubject e, eValue e, summary d) | e <- expected, d <- matching e, summary d /= eValue e ] `shouldBe` []
    it "agrees on the value of every constructor where only the range's start differs (T126, T138)" $ do
      [ (show x, enumValue x) | x <- [minBound .. maxBound :: G.Core_reencrypt_state] ]
        `shouldBe` [ (show x, fromEnum x) | x <- [minBound .. maxBound :: H.Core_reencrypt_state], x /= H.Core_reencrypt_state_NA ]
      [ (show x, enumValue x) | x <- [minBound .. maxBound :: G.Core_table_kind] ]
        `shouldBe` [ (show x, fromEnum x) | x <- [minBound .. maxBound :: H.Core_table_kind], x /= H.Core_table_kind_NA ]
    it "finds every typed half-UID's table among the generated half-UIDs" $
      [ dName h | h <- Map.elems (hands Map.\\ gens), any (`isPrefixOf` dName h) ["oth", "bth"], isNothing (lookup' h) ] `shouldBe` []

  describe "the status codes beside the codec's status list (T166, design M2b)" $ do
    let statuses = [ (v, n) | CodeRow (One v) [n] <- G.cStatusCodes ]
        wire v = B.unpack (generate (MethodResponse [] (fromInteger v)))
        readBack v = _resultStatus <$> (parseByteString (generate (MethodResponse [] (fromInteger v))) :: Either String MethodResponse)
    it "gives 19 values, each a tiny atom the codec puts first in a response's status list" $ do
      length statuses `shouldBe` 19
      [ (n, wire v) | (v, n) <- statuses ] `shouldBe` [ (n, [0xF0, 0xF1, 0xF9, 0xF0, fromInteger v, 0x00, 0x00, 0xF1]) | (v, n) <- statuses ]
    it "reads every one back through the codec" $
      [ n | (v, n) <- statuses, readBack v /= Right (fromInteger v) ] `shouldBe` []
    it "names with its patterns the values the codec carries bare" $ do
      [G.SUCCESS, G.NOT_AUTHORIZED, G.OBSOLETE_13, G.FAIL] `shouldBe` map G.StatusCode [0x00, 0x01, 0x0d, 0x3f]
      [ n | (v, n) <- statuses, n == T.pack "INVALID_PARAMETER", G.StatusCode (fromInteger v) /= G.INVALID_PARAMETER ] `shouldBe` []
    it "differs from the codec as listed: the codec names no value, and reads every tiny atom the table leaves unassigned" $
      [ v | v <- [0 .. 0x3f], v `notElem` map fst statuses, readBack v == Right (fromInteger v) ] `shouldBe` (0x0b : [0x13 .. 0x3e])

  describe "the typed records' fields beside the hand-written Type objects (T22, T23, T34, design M2b)" $
    it "gives each field the UID the hand transcription gives the Type object it names" $ do
      let hands  = byName handWritten
          fields = G.rSecureMessagingPacketPayloadField ++ G.rSecureMessagingPacketPayloadSecureDataField ++ G.rDateValues
      length fields `shouldBe` 14
      [ (fdName f, fdTypeName f) | f <- fields
                                 , fmap dValue (Map.lookup ("u" ++ T.unpack (fdTypeName f) ++ "Type") hands) /= Just (Scalar (show (fdType f))) ]
        `shouldBe` []
  where
    lookup' h = case mapMaybe (`stripPrefix` dName h) ["oth", "bth"] of
      [t] | isJust (Map.lookup ("h" ++ t) (byName generated)) -> Just t
      _   -> Nothing
