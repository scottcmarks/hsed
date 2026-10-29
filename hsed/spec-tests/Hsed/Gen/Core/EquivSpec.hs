{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Hsed.Gen.Core.EquivSpec
Description : The generated Core modules beside hsed's hand-written TypeUIDs and TableUIDs, at run time
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design docs/spec-as-source.md section 5.5, the regenerate-and-compare oracle
(M2a): every definition @hsedc compile@ generated from the PDF (every module
under @gen/Hsed/Gen/Core@, each imported here as @G@) beside its
counterpart in the hand transcription (@System.SED.MCTP.Common.{TypeUIDs,TableUIDs}@,
as @H@), definition by definition and table caption by table caption
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
* @caption@: a table whose caption the two sides print differently (spacing
  aside); @generated / hand-written@.

A rule of M2b has a hand counterpart other than a definition: the method
status codes (Core Table 166, @statusCodes@) are what hsed-core's codec carries
as a bare uinteger in a response's status list, so every one of them is
encoded and decoded by the codec here and the values the table leaves
unassigned are listed.

The M1 slice is kept by name above it (M1 acceptance (d)). The hand-written
side is read where it lives, in hsed's library; when M2d makes those modules
re-exports of the generated ones, the hand transcription is kept as a test
fixture and 'handDefs' points at it (design section 11, M2d).
-}
module Hsed.Gen.Core.EquivSpec (spec) where

import           Data.Bits                        (xor)
import           Data.Char                        (isDigit, isSpace, ord)
import           Data.List                        (foldl', intercalate, isPrefixOf, nub, sort, stripPrefix, (\\))
import qualified Data.Map                         as Map
import           Data.Maybe                       (isJust, isNothing, mapMaybe)
import           Data.Word                        (Word64)
import           Numeric                          (showHex)
import           Test.Hspec

import qualified RIO.ByteString                   as B
import qualified RIO.Text                         as T

import           Hsed.Core.Code                   (CodeRow (..), Span (..))
import           Hsed.Core.Types                  (Enumerated (..), enumValue)
import qualified Hsed.Gen.Core.Codes              as G
import qualified Hsed.Gen.Core.Enums              as G
import           Hsed.Gen.Core.EquivTH
import qualified Hsed.Gen.Core.TableUIDs          as G
import qualified Hsed.Gen.Core.Types              as G
import           System.SED.MCTP.Common.Call      (MethodResponse (..))
import           System.SED.MCTP.Common.StreamItem (StreamItem (..))
import qualified System.SED.MCTP.Common.TableUIDs as H
import qualified System.SED.MCTP.Common.TypeUIDs  as H


-- | Every generated definition a hand-written module could have.
generated :: [Def]
generated = $(generatedDefs "gen/Hsed/Gen/Core" "G")

-- | Every hand-written definition the quasiquoters make.
handWritten :: [Def]
handWritten = $(handDefs "src/System/SED/MCTP/Common/TypeUIDs.hs" "src/System/SED/MCTP/Common/TableUIDs.hs" "H")


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
kinds = ["pdf-uid", "spacing", "range-from-1", "typed-half-uid", "new", "caption"]


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

  describe "every generated definition beside its hand-written counterpart (design 5.5)" $ do
    expected <- runIO (parseExpected <$> readFile "../spec/expected-diff.txt")
    let gens  = byName generated
        hands = byName handWritten
        matching e = [ d | d <- differences, subject d == eSubject e ]
    it "reads 360 generated definitions and 405 hand-written ones: 359 on both sides, 1 new, 46 the generated lack" $ do
      (Map.size gens, Map.size hands) `shouldBe` (360, 405)
      Map.size (Map.intersection gens hands) `shouldBe` 359
      Map.keys (gens Map.\\ hands) `shouldBe` ["Core_gen_status"]
      Map.size (hands Map.\\ gens) `shouldBe` 46
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
      [ dName h | h <- Map.elems (hands Map.\\ gens), isNothing (lookup' h) ] `shouldBe` []

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
  where
    lookup' h = case mapMaybe (`stripPrefix` dName h) ["oth", "bth"] of
      [t] | isJust (Map.lookup ("h" ++ t) (byName generated)) -> Just t
      _   -> Nothing
