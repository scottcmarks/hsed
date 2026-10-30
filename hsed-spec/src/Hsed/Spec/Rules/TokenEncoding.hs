{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Hsed.Spec.Rules.TokenEncoding
Description : The tokenEncoding rule: the token table checked against the codec, the atom diagrams recorded
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 3.4 (class D). @tokenEncoding@ binds, with the @tokens@ binding,
the token table of Core 3.2.2.3 (Table 4 Token Types, headed @[Byte, Hex,
Acronym, Meaning]@) and the atom diagrams after it (Tables 5-14, the first
header cell naming the atom's @Header@, or two columns and no header row),
anchored at Core 3.2.2: across the document the shape would also take Table 24.
The codec that encodes and decodes tokens is hand-written
("System.SED.MCTP.Common.Token"); the rule reads the token table with the
@tokens@ reader ('tokenRows') and checks the codec against it on every run.
The diagrams, with the 13 tables nested in their cells, are the codec's by
hand: they are recorded, not read.

The token table, read row by row: the last cell is the Meaning, the Hex cell
(@F0@, @00..7F@) the one before it or before an Acronym, and the cells before
the Hex cell the bit pattern of the token's header (@1 1 0 B S n\<10..0\>@:
single bits and @x\<h..l\>@ fields). A row with an Acronym is one token, a
row with a bit pattern and no Acronym an atom whose header is the pattern's
bytes, a row with neither reserved.

Checks, each with the code its findings carry:

* @C-TOKEN-TABLE@: every row has a Hex cell; a bit pattern comes to whole
  bytes and its leading fixed bits give exactly the Hex range; a row with an
  Acronym is one byte; the rows cover every byte 00..FF once;
* @C-TOKEN-CODEC@: the codec agrees with every row ('codecTag'): a token's
  byte alone decodes to the codec's token whose name the Meaning starts with
  (@Start List@, @StartList@), which encodes as that byte; every byte of a
  reserved range is refused by the codec's parser and starts nothing it
  writes; the codec writes atoms starting with bytes of each atom range, each
  with the row's header length, and no byte of an atom range decodes alone to
  a token; every atom of a probe (every tiny integer, values of every length
  up to 17 and around every multiple of 256 to 2049, and the empty byte
  sequences, continued or not) the codec writes decodes back to itself, and
  the one it writes as nothing is the empty continued byte sequence, which
  adds nothing to the value it continues and which no atom may encode (Core
  3.2.2.3.1.2 permits length 0 only for a byte sequence that is not
  continued).

Compiled into 'tokensModule': @codecTables@, the SpecRef and title of every
table bound with the shape, each under its provenance, so that a change of a
diagram in the PDF changes the generated module (and asks for a look at the
hand-written codec).
-}
module Hsed.Spec.Rules.TokenEncoding
  ( tokenEncodingRule
  , tokensModule
    -- * The token table
  , TokenRow (..)
  , RowKind (..)
  , tokenRows
  , readTokenRow
  , isTokenTable
    -- * The codec
  , CodecTag (..)
  , codecTag
  , tokenFindings
  , probe
  , roundTrips
  ) where

import           Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString            as B
import qualified Data.Text                  as DT
import qualified RIO.Char                   as C
import qualified RIO.List                   as L
import qualified RIO.Map                    as Map
import qualified RIO.Text                   as T
import           RIO

import           Hsed.Spec.Anchor           (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Common           (Finding, RuleName (..), Severity (..), SpecId (..), finding)
import           Hsed.Spec.DSL              (SomeBinding (..), tokens)
import qualified Hsed.Spec.Gen              as Gen
import           Hsed.Spec.IR               (CellIR (..), SpecIR (..), StructPath (..), TableIR (..))
import           Hsed.Spec.Readers.Text     (readHexByte)
import           Hsed.Spec.RuleDef
import           Hsed.Spec.Shape            (Form (..), Shape (..), captionTitle, measure, shapeMismatch)
import           System.SED.MCTP.Common.StreamItem (StreamItem (..))
import           System.SED.MCTP.Common.Token      (Token (..))


-- | The rule.
tokenEncodingRule :: RuleDef
tokenEncodingRule = RuleDef
  { ruleName    = RuleName "tokenEncoding"
  , bindsTo     = SomeBinding tokens
  , exemplar    = (SpecId "core-2.01", TableId 4)
  , refinesRule = Nothing
  , joinsTo     = Nothing
  , fires       = \_ _ form -> isNothing (shapeMismatch Tokens form)
  , ruleIdent   = const Nothing
  , ruleModule  = tokensModule
  , ruleChecks  = \ctx bts -> concat [ map (atTable ctx bt) (tokenFindings rows) | (bt, rows) <- readTables ctx bts ]
  , ruleCompile = \ctx bts -> case bts of
      first' : _ -> [Emit (codecTablesDecl ctx first' bts)]
      []         -> []
  }

-- | @Hsed.Gen.\<Spec\>.Tokens@.
tokensModule :: Gen.GenModule
tokensModule = Gen.GenModule
  { Gen.leaf        = "Tokens"
  , Gen.description = "The token tables the hand-written codec implements"
  , Gen.notes       = [ "The codec is System.SED.MCTP.Common.Token: hsedc checks it against the token table"
                      , "on every run (C-TOKEN-CODEC); the atom diagrams are its by hand." ] }


-- * The token table

-- | One row of a token table, read under the errata set.
data TokenRow = TokenRow
  { tkRow     :: !Int            -- ^ 1-based among the data rows
  , tkHex     :: !Text           -- ^ the Hex cell, @00..7F@
  , tkLo      :: !Word8
  , tkHi      :: !Word8
  , tkBits    :: ![Text]         -- ^ the bit pattern, its empty cells dropped
  , tkAcronym :: !(Maybe Text)
  , tkMeaning :: !Text
  }
  deriving (Eq, Show)

-- | What a row is.
data RowKind
  = OneToken          -- ^ it has an Acronym: one byte, one token
  | AtomHeader !Int   -- ^ a bit pattern and no Acronym: an atom whose header is this many bytes
  | Reserved          -- ^ neither
  deriving (Eq, Show)

-- | A table the rule binds is the token table when its header starts as Table 4's does.
isTokenTable :: Context -> BoundTable -> Bool
isTokenTable ctx bt = take 4 (fHeaderNorm (measure (cxSpec ctx) (btRead bt))) == ["byte", "hex", "acronym", "meaning"]

-- | Every token table bound with the shape, each row read or why it cannot be.
tokenRows :: Context -> [(BoundTable, [Either Text TokenRow])]
tokenRows ctx = readTables ctx (boundTables (Just Tokens) ctx)

readTables :: Context -> [BoundTable] -> [(BoundTable, [Either Text TokenRow])]
readTables ctx bts =
  [ (bt, zipWith readTokenRow [1 ..] (map (map (T.strip . (.text))) (btRead bt).rows)) | bt <- bts, isTokenTable ctx bt ]

-- | One row of the token table from its cells: the Meaning last, the Hex cell
--   before it or before the Acronym, the bit pattern before that.
readTokenRow :: Int -> [Text] -> Either Text TokenRow
readTokenRow i cells = case reverse cells of
  meaning : h : rest | Just (lo, hi) <- hexRange h -> Right (row h lo hi Nothing rest meaning)
  meaning : acronym : h : rest | Just (lo, hi) <- hexRange h ->
    Right (row h lo hi (if T.null acronym then Nothing else Just acronym) rest meaning)
  _ -> Left ("row " <> tshow i <> " has no Hex cell: " <> tshow cells)
  where
    row h lo hi acronym rest meaning = TokenRow
      { tkRow = i, tkHex = h, tkLo = lo, tkHi = hi, tkBits = filter (not . T.null) (reverse rest)
      , tkAcronym = acronym, tkMeaning = meaning }

-- | @F0@ or @00..7F@.
hexRange :: Text -> Maybe (Word8, Word8)
hexRange cell = case DT.splitOn ".." cell of
  [a]    -> (\x -> (x, x)) <$> readHexByte a
  [a, b] -> do
    lo <- readHexByte a
    hi <- readHexByte b
    if lo <= hi then Just (lo, hi) else Nothing
  _      -> Nothing

-- | How many bits a cell of a bit pattern stands for: one for a single
--   character, @h - l + 1@ for a field @x\<h..l\>@.
bitWidth :: Text -> Maybe Int
bitWidth cell
  | T.length cell == 1 = Just 1
  | otherwise = do
      inner <- T.stripSuffix ">" (T.drop 1 (T.dropWhile (/= '<') cell))
      guard (T.any (== '<') cell)
      case DT.splitOn ".." inner of
        [h, l] -> do
          hi <- readMaybe (T.unpack h)
          lo <- readMaybe (T.unpack l)
          if hi >= lo then Just (hi - lo + 1) else Nothing
        _ -> Nothing

-- | What a row is, by its Acronym and bit pattern, with how many bytes the
--   pattern comes to (Nothing when a cell is no bit or field).
rowKind :: TokenRow -> RowKind
rowKind r = case (tkAcronym r, tkBits r) of
  (Just _, _) -> OneToken
  (_, [])     -> Reserved
  (_, bits)   -> AtomHeader (maybe 0 sum (traverse bitWidth bits) `div` 8)

-- | The bytes of a row's Hex range.
bytesOf :: TokenRow -> [Word8]
bytesOf r = [tkLo r .. tkHi r]


-- * The codec

-- | What the codec does with a token's first byte.
data CodecTag
  = Whole !Token    -- ^ the byte alone decodes to this token, which carries no value
  | Atom ![Int]     -- ^ the codec writes atoms starting with it, their headers this many bytes long
  | Refused         -- ^ it writes nothing starting with it, and its parser refuses it whatever follows
  | ReadOnly        -- ^ it writes nothing starting with it, and its parser reads it
  deriving (Eq, Show)

-- | The codec's view of a byte as the first of a token.
codecTag :: Word8 -> CodecTag
codecTag b
  | Right t <- parseOnly (parser @Token) (B.singleton b), not (carriesValue t) = Whole t
  | Just ws <- Map.lookup b written = Atom ws
  | isLeft (parseOnly (parser @Token) (B.cons b following)) = Refused
  | otherwise = ReadOnly

-- | What follows a first byte when the codec is asked whether it refuses it:
--   0x01 bytes, enough for any header and the longest value three length bytes
--   of 0x01 announce.
following :: ByteString
following = B.replicate 0x10200 0x01

-- | Every atom the probe has the codec write, by its first byte: the lengths
--   of their headers (the bytes before the value), sorted.
written :: Map Word8 [Int]
written = Map.map (L.sort . L.nub) (Map.fromListWith (<>)
  [ (B.head enc, [B.length enc - payload]) | (t, payload) <- probe, let enc = generate t, not (B.null enc) ])

-- | The atoms the probe has the codec write, each with the length of its value:
--   every integer a tiny atom holds, integers, byte sequences and continued
--   byte sequences of every length up to 17 and around every multiple of 256
--   up to 2049, and the empty byte sequence and the empty continued one.
probe :: [(Token, Int)]
probe =
  [ (Unsigned n, 0) | n <- [0 .. 63] ] <> [ (Signed i, 0) | i <- [-32 .. 31] ]
  <> concat [ [ (Unsigned (2 ^ (8 * k - 1)), k), (Signed (negate (2 ^ (8 * k - 1))), k)
              , (Bytes (B.replicate k 0), k), (ContinuedBytes (B.replicate k 0), k) ] | k <- lengths ]
  <> [ (Bytes B.empty, 0), (ContinuedBytes B.empty, 0) ]
  where lengths = [1 .. 17] <> concat [ [256 * j - 1, 256 * j, 256 * j + 1] | j <- [1 .. 8] ]

-- | A token that carries a value: an atom.
carriesValue :: Token -> Bool
carriesValue = \case
  Unsigned _       -> True
  Signed _         -> True
  Bytes _          -> True
  ContinuedBytes _ -> True
  _                -> False

-- | Does a Meaning name a token: does it start with the token's name, both
--   compared without case, spaces or punctuation (@Start List@ and @StartList@,
--   @Empty atom@ and @Empty@)?
names :: Text -> Token -> Bool
names meaning t = squash (tshow t) `T.isPrefixOf` squash meaning
  where squash = T.toLower . T.filter C.isAlphaNum


-- * The checks

-- | What C-TOKEN-TABLE and C-TOKEN-CODEC find in one token table's rows.
tokenFindings :: [Either Text TokenRow] -> [Finding]
tokenFindings reads =
  map (finding Error "C-TOKEN-TABLE") (lefts reads <> concatMap rowProblems rows <> coverage rows)
  <> map (finding Error "C-TOKEN-CODEC") (concatMap codecProblems rows <> roundTrips)
  where rows = rights reads

label :: TokenRow -> Text
label r = "row " <> tshow (tkRow r) <> " " <> tkHex r <> " " <> T.unwords (catMaybes [tkAcronym r, Just (tkMeaning r)]) <> ": "

-- | C-TOKEN-TABLE on one row: its bit pattern against its Hex range.
rowProblems :: TokenRow -> [Text]
rowProblems r = map (label r <>) $
  [ "an Acronym names one token, and " <> tkHex r <> " is a range" | isJust (tkAcronym r), tkLo r /= tkHi r ]
  <> case traverse bitWidth (tkBits r) of
       _ | null (tkBits r) -> []
       Nothing -> [ "the bit pattern " <> tshow (tkBits r) <> " has a cell that is no bit and no field" ]
       Just widths
         | sum widths `mod` 8 /= 0 -> [ "the bit pattern comes to " <> tshow (sum widths) <> " bits, not whole bytes" ]
         | (lo, hi) /= (tkLo r, tkHi r) ->
             [ "the bit pattern's leading fixed bits " <> T.concat fixed <> " give " <> hexText lo hi <> ", not " <> tkHex r ]
         | otherwise -> []
  where
    fixed  = take 8 (takeWhile (`elem` ["0", "1"]) (tkBits r))
    value  = foldl' (\n b -> n * 2 + (if b == "1" then 1 else 0)) 0 fixed :: Int
    free   = 8 - length fixed
    lo     = fromIntegral (value * 2 ^ free) :: Word8
    hi     = fromIntegral (value * 2 ^ free + 2 ^ free - 1) :: Word8

-- | C-TOKEN-TABLE on the whole table: every byte in exactly one row.
coverage :: [TokenRow] -> [Text]
coverage rows =
  [ "no row covers " <> byteList missing | let missing = [ b | b <- [minBound .. maxBound], b `Map.notMember` owners ], not (null missing) ]
  <> [ byteList [b] <> " is in rows " <> T.intercalate " and " (map tshow rs) | (b, rs) <- Map.toList owners, length rs > 1 ]
  where owners = Map.fromListWith (flip (<>)) [ (b, [tkRow r]) | r <- rows, b <- bytesOf r ]

-- | C-TOKEN-CODEC on one row.
codecProblems :: TokenRow -> [Text]
codecProblems r = map (label r <>) $ case rowKind r of
  OneToken -> case codecTag (tkLo r) of
    Whole t
      | not (names (tkMeaning r) t) -> [ sentence (Whole t) [tkLo r] ]
      | generate t /= B.singleton (tkLo r) -> [ "the codec encodes " <> tshow t <> " as " <> bytesText (generate t) ]
      | otherwise -> []
    other -> [ sentence other [tkLo r] ]
  Reserved ->
    [ sentence c bs | (c, bs) <- grouped [ (c, b) | b <- bytesOf r, let c = codecTag b, c /= Refused ] ]
  AtomHeader w ->
    [ sentence c bs <> (if isAtom c then "; the row's header is " <> tshow w <> " bytes" else "")
    | (c, bs) <- grouped [ (c, b) | b <- bytesOf r, let c = codecTag b, wrong w c ] ]
    <> [ "the codec writes no atom starting with a byte of the range" | not (any (isAtom . codecTag) (bytesOf r)) ]
  where
    wrong w = \case
      Whole _  -> True
      Atom ws  -> ws /= [w]
      _        -> False
    isAtom = \case
      Atom _ -> True
      _      -> False

-- | What the codec does with some bytes, as a sentence.
sentence :: CodecTag -> [Word8] -> Text
sentence c bs = case c of
  Whole t  -> "the codec decodes " <> byteList bs <> " alone as " <> tshow t
  Atom ws  -> "the codec writes atoms with " <> T.intercalate " and " [ tshow w <> "-byte" | w <- ws ] <> " headers starting with " <> byteList bs
  Refused  -> "the codec refuses " <> byteList bs
  ReadOnly -> "the codec writes nothing starting with " <> byteList bs <> " but decodes it"

-- | C-TOKEN-CODEC on the codec alone: every atom of the probe it writes
--   decodes back to itself, and it writes nothing only for an atom that adds
--   nothing to a value, a continued byte sequence with no bytes.
roundTrips :: [Text]
roundTrips = mapMaybe (\(t, _) -> problem t (generate t)) probe
  where
    problem t enc
      | B.null enc = if addsNothing t then Nothing else Just ("the codec writes nothing for " <> short t)
      | otherwise = case parseOnly (parser @Token) enc of
          Right back | back == t -> Nothing
          back -> Just ("the codec writes " <> short t <> " as " <> bytesText (B.take 4 enc) <> "..., which it decodes as "
                        <> either T.pack short back)
    addsNothing = \case
      ContinuedBytes bs -> B.null bs
      _                 -> False
    short = T.take 60 . tshow

-- | Bytes grouped by what the codec does with them, in the order first met.
grouped :: [(CodecTag, Word8)] -> [(CodecTag, [Word8])]
grouped xs = [ (c, [ b | (c', b) <- xs, c' == c ]) | c <- L.nub (map fst xs) ]

-- | Bytes as hex, runs as ranges: @80, 90@, @E4..EF@.
byteList :: [Word8] -> Text
byteList = T.intercalate ", " . map run . runs . L.sort
  where
    runs = foldr (\b acc -> case acc of
                      (lo, hi) : rest | fromIntegral b + 1 == (fromIntegral lo :: Int) -> (b, hi) : rest
                      _ -> (b, b) : acc) []
    run (lo, hi) = if lo == hi then hexByte lo else hexText lo hi

hexText :: Word8 -> Word8 -> Text
hexText lo hi = if lo == hi then hexByte lo else hexByte lo <> ".." <> hexByte hi

hexByte :: Word8 -> Text
hexByte w = T.pack [digit (w `div` 16), digit (w `mod` 16)]
  where digit d = T.index "0123456789ABCDEF" (fromIntegral d)

bytesText :: ByteString -> Text
bytesText = T.unwords . map hexByte . B.unpack


-- * Declarations

-- | @codecTables@: the SpecRef and title of every table bound with the shape,
--   in anchor order, each under its provenance (with the number of tables
--   nested in its cells).
codecTablesDecl :: Context -> BoundTable -> [BoundTable] -> Gen.Decl
codecTablesDecl ctx first' bts = (Gen.decl (provenance first') "codecTables" body)
  { Gen.declImports = [Gen.ImportSome "Data.Text" ["Text"]] }
  where
    -- the caption's title, which the provenance prints whatever the caption's number looks like (Table 04)
    provenance bt = provenanceOf ctx (btAnchor bt) (btTable bt) (title bt)
    body =
      [ Gen.provenanceComment (provenance first')
      , "--   The tables of " <> Gen.specLabel (cxSpec ctx) <> within <> " the hand-written codec (System.SED.MCTP.Common.Token)"
      , "--   implements, each with its SpecRef and title under its provenance: the token table"
      , "--   is checked against the codec on every hsedc run (C-TOKEN-CODEC), the atom"
      , "--   diagrams are the codec's by hand."
      , "codecTables :: [(Text, Text)]"
      , "codecTables =" ]
      <> concat (zipWith element [0 :: Int ..] bts)
      <> ["  ]"]
    element i bt =
      [ "    -- " <> fromMaybe "" (T.stripPrefix "-- | " (Gen.provenanceComment (provenance bt))) <> nestedNote bt
      , "  " <> (if i == 0 then "[ " else ", ") <> "(" <> tshow (specRef bt) <> ", " <> tshow (title bt) <> ")" ]
    -- the section every table lies in: the longest common prefix of their sections
    within = case [ DT.splitOn "." s | bt <- bts, Just s <- [sectionOf ctx bt] ] of
      s : ss | common@(_ : _) <- foldl' (\a b -> map fst (takeWhile (uncurry (==)) (zip a b))) s ss -> " " <> T.intercalate "." common
      _ -> ""
    specRef bt = specIdText (cxSpec ctx) <> ":" <> renderAnchorId (btAnchor bt)
    title bt = fromMaybe "" ((btRead bt).caption >>= captionTitle)
    nestedNote bt = case nestedCount (btTable bt) of
      0 -> ""
      1 -> ", 1 nested table"
      n -> ", " <> tshow n <> " nested tables"
    nestedCount :: TableIR -> Int
    nestedCount t = let StructPath sp = t.structPath in length
      [ () | n <- (cxIR ctx).tables, Just (StructPath p) <- [n.nestedIn], p == sp || (sp <> "/") `T.isPrefixOf` p ]
