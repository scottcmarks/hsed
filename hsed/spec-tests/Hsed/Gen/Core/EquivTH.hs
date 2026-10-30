{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hsed.Gen.Core.EquivTH
Description : Every generated definition and every hand-written one, with its value and its table's caption
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The splices behind "Hsed.Gen.Core.EquivSpec" (design docs/spec-as-source.md
section 5.5, the regenerate-and-compare oracle). Both read source at compile
time and build, for each definition, a 'Def' whose value is computed at run
time:

* 'generatedDefs' reads every module @hsedc compile@ wrote for the spec (all
  of @gen/Hsed/Gen/Core@, so that a new generated module cannot escape the
  comparison: one not imported by "Hsed.Gen.Core.EquivSpec" stops the
  compilation): every signature @name :: T@ whose type the hand-written
  modules also have (@UID@, @HalfUID@ and the two name maps; a @TypeDef@ has
  no hand-written counterpart, and since M2d no Format @String@ is generated
  or read) and every @data Core_\<enum\>@ (another data type, a record, is
  no enumeration), each with the caption of the table its provenance line
  names (a definition under the provenance of a signature block,
  @5.3.3.6/sig@, names no table and is not read);
* 'generatedUIDLists' reads the same modules' lists of a table's UIDs
  (@name :: [(UID, String)]@), each with its table: what a hand-written UID of
  a table whose rows the PDF names nowhere is found in;
* 'handDefs' reads the hand transcription, the test fixture hsed/fixture/Hand
  (hsed's library until M2d): every @ttype@ quasiquote (@u\<Name\>Type@;
  its @f\<Name\>Type@ is not read), every @tenum@ quasiquote
  (@Core_\<enum\>@), the @t240@ quasiquote's rows (@h\<T\>@, @u\<T\>Table@,
  @u\<T\>TableObject@, and whichever of the typed @oth\<T\>@ or @bth\<T\>@
  the module defines; then @nameHalfUID@ and @nameUID@), each with the
  caption the quasiquote prints, and every @name :: UID@ of
  @SpecialPurposeUIDs@ with the caption of the transcribed table above it
  (@Table 239 Special Purpose UIDs@, @Table 241 Session Manager Method
  UIDs@). A name the source promises and the module does not define stops the
  compilation.

A value is 'Scalar' ('show' of it) or, for an enumeration, 'Entries': each
constructor with the value the spec assigns it (the generated module's
'Hsed.Core.Types.enumValue'; the hand-written module's 'fromEnum', since its
enumerations start at 0). The fixture's constructors are compared under the
names the generator gives them ('asGenerated', design section 13.15): its
@dEnum@ left the first value of a label that names several bare.
-}
module Hsed.Gen.Core.EquivTH
  ( Def (..)
  , Value (..)
  , generatedDefs
  , generatedUIDLists
  , handDefs
  , asGenerated
  ) where

import           Control.Monad              (forM)
import           Data.Char                  (isDigit, isSpace)
import           Data.List                  (isInfixOf, isPrefixOf, sort, stripPrefix, transpose)
import           Data.Maybe                 (catMaybes)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (addDependentFile)
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath            (takeExtension, (</>))

import           Hsed.Core.Types            (enumValue)


-- | A definition's value, as the comparison sees it.
data Value
  = Scalar String          -- ^ 'show' of the value
  | Entries [(String, Int)] -- ^ an enumeration: each constructor and the value the spec gives it
  deriving (Eq, Show)

-- | One definition of one side.
data Def = Def
  { dName    :: String
  , dTable   :: String   -- ^ the table it comes from, @T46@
  , dCaption :: String   -- ^ that table's caption, @Table 46 AC_element@, its spacing as written
  , dValue   :: Value
  }
  deriving (Eq, Show)


-- * The generated modules

-- | @$(generatedDefs "gen/Hsed/Gen/Core" "G")@: every module under the
--   directory, at any depth, all imported qualified as @G@.
generatedDefs :: FilePath -> String -> Q Exp
generatedDefs dir g = do
  paths <- runIO (haskellFiles dir)
  mapM_ addDependentFile paths
  srcs <- runIO (mapM readFile paths)
  defs <- fmap concat (forM srcs (\src -> forM [ d | d@(_, _, _, kind) <- generatedNames (lines src), kind /= UIDList ] one))
  listE (map pure defs)
  where
    one (n, table, caption, kind) = do
      let isEnum = kind == Enumeration
      found <- (if isEnum then lookupTypeName else lookupValueName) (g ++ "." ++ n)
      case found of
        Nothing -> fail ("the generated module declares " ++ n ++ " but " ++ g ++ " does not export it")
        Just nm
          | isEnum    -> [| Def n table caption (Entries [ (show x, enumValue x) | x <- [minBound .. maxBound :: $(conT nm)] ]) |]
          | otherwise -> [| Def n table caption (Scalar (show $(varE nm))) |]

-- | @$(generatedUIDLists "gen/Hsed/Gen/Core" "G")@: each list of a table's
--   UIDs, @[(UID, String)]@, as its table and the 'show' of every UID in it.
generatedUIDLists :: FilePath -> String -> Q Exp
generatedUIDLists dir g = do
  paths <- runIO (haskellFiles dir)
  mapM_ addDependentFile paths
  srcs <- runIO (mapM readFile paths)
  lists <- fmap concat $ forM srcs $ \src -> forM [ (n, table) | (n, table, _, UIDList) <- generatedNames (lines src) ] $ \(n, table) -> do
    found <- lookupValueName (g ++ "." ++ n)
    case found of
      Nothing -> fail ("the generated module declares " ++ n ++ " but " ++ g ++ " does not export it")
      Just nm -> [| (table, map (show . fst) $(varE nm)) |]
  listE (map pure lists)

-- | What a generated declaration is to the comparison.
data DeclKind = Scalar' | Enumeration | UIDList
  deriving (Eq)

-- | Every @.hs@ file under a directory, sorted.
haskellFiles :: FilePath -> IO [FilePath]
haskellFiles d = do
  entries <- map (d </>) . sort <$> listDirectory d
  fmap concat $ forM entries $ \e -> do
    isDir <- doesDirectoryExist e
    if isDir then haskellFiles e else pure [ e | takeExtension e == ".hs" ]

-- | The definitions of a generated module with the table and caption of the
--   provenance line above each: @-- | core-2.01 T46 AC_element (PDF 100 / ...@;
--   a provenance line that names no table (a signature block's) ends the
--   table before it.
generatedNames :: [String] -> [(String, String, String, DeclKind)]
generatedNames = go Nothing
  where
    go _ [] = []
    go prov (l : ls)
      | Just rest <- stripPrefix "-- | " l, " (PDF " `isInfixOf` rest = go (provenance rest) ls
      | Just (table, caption) <- prov, Just n <- signature l = (n, table, caption, Scalar') : go prov ls
      | Just (table, caption) <- prov, Just n <- uidList l = (n, table, caption, UIDList) : go prov ls
      | Just (table, caption) <- prov, Just n <- stripPrefix "data " l, "Core_" `isPrefixOf` n = (n, table, caption, Enumeration) : go prov ls
      | otherwise = go prov ls
    uidList l = case words l of
      [n, "::", "[(UID,", "String)]"] -> Just n
      _                               -> Nothing
    -- "core-2.01 T46 AC_element (PDF 100 / p.83, ...": the table and its caption
    provenance rest = case words rest of
      _spec : table@('T' : number) : more | all isDigit number ->
        Just (table, unwords (["Table", number] ++ takeWhile (/= "(PDF") more))
      _ -> Nothing
    signature l = case words l of
      n : "::" : ty | unwords ty `elem` handTypes -> Just n
      _ -> Nothing
    handTypes = ["UID", "HalfUID", "Map HalfUID String", "Map UID String"]


-- * The hand-written modules

-- | @$(handDefs "fixture/Hand/TypeUIDs.hs" "fixture/Hand/TableUIDs.hs" "fixture/Hand/SpecialPurposeUIDs.hs" "H")@,
--   the fixture's three modules, all imported qualified as @H@.
handDefs :: FilePath -> FilePath -> FilePath -> String -> Q Exp
handDefs typeUIDs tableUIDs specialUIDs h = do
  mapM_ addDependentFile [typeUIDs, tableUIDs, specialUIDs]
  types   <- runIO (readFile typeUIDs)
  tables  <- runIO (readFile tableUIDs)
  special <- runIO (readFile specialUIDs)
  typeDefs    <- forM (quasiquotes "ttype" (lines types)) typeRow
  enumDefs    <- forM (quasiquotes "tenum" (lines types)) enumeration
  tableDefs   <- forM (quasiquotes "t240" (lines tables)) t240
  specialDefs <- forM (captionedUIDs (lines special)) (\(caption, n) -> scalar caption n)
  listE (map pure (concat typeDefs ++ enumDefs ++ concat tableDefs ++ specialDefs))
  where
    value n = maybe (fail (h ++ " does not define " ++ n ++ ", which its source promises")) pure
                =<< lookupValueName (h ++ "." ++ n)
    scalar caption n = do
      nm <- value n
      [| Def n (tableOf caption) caption (Scalar (show $(varE nm))) |]
    -- a Type table row: its Name cell names u<Name>Type (the fixture's f<Name>Type, a
    -- Format string, has had no generated counterpart since M2d)
    typeRow (caption, body) = case drop 1 (tableRows body) of
      [_, name, _] : _ -> mapM (scalar caption) ["u" ++ name ++ "Type"]
      _                -> fail ("no Type table row under " ++ caption)
    -- an enumeration: its caption names the type, Core_<type>
    enumeration (caption, _) = case words caption of
      _ : _ : ident : _ -> do
        let n = "Core_" ++ ident
        found <- lookupTypeName (h ++ "." ++ n)
        nm <- maybe (fail (h ++ " does not define " ++ n ++ ", which its source promises")) pure found
        [| Def n (tableOf caption) caption (Entries (asGenerated [ (show x, fromEnum x) | x <- [minBound .. maxBound :: $(conT nm)] ])) |]
      _ -> fail ("no enumeration named by " ++ caption)
    -- Table 240: three UIDs per row, the typed half-UID the module has, then the two maps
    t240 (caption, body) = do
      rows <- fmap concat $ forM [ name | [_, _, name, _] <- drop 1 (tableRows body) ] $ \name -> do
        typed <- catMaybes <$> mapM (\p -> fmap (const (p ++ name)) <$> lookupValueName (h ++ "." ++ p ++ name)) ["oth", "bth"]
        mapM (scalar caption) (["u" ++ name ++ "TableObject", "u" ++ name ++ "Table", "h" ++ name] ++ typed)
      maps <- mapM (scalar caption) ["nameHalfUID", "nameUID"]
      pure (rows ++ maps)

-- | An enumeration of the fixture, each constructor with its value, named as
--   the generator names it (design section 13.15, M2d): every value of a label
--   that names several is suffixed with the value, the first included. The
--   fixture's @dEnum@ left that first value bare, so a bare @C@ is renamed
--   @C_\<v\>@ when a sibling @C_\<w\>@ carries its own value @w@; a name that
--   merely ends in a number (@Core_hash_protocol_SHA_1@, value 1, beside
--   @SHA_256@, value 2) has no such sibling and stays as it is.
asGenerated :: [(String, Int)] -> [(String, Int)]
asGenerated es = [ (if any (\(d, w) -> d == c ++ "_" ++ show w) es then c ++ "_" ++ show v else c, v) | (c, v) <- es ]

-- | Every top-level @name :: UID@ of a module with the caption of the last
--   transcribed table above it, a line @Table N …@ in a comment.
captionedUIDs :: [String] -> [(String, String)]
captionedUIDs = go Nothing
  where
    go _ [] = []
    go caption (l : ls)
      | ("Table" : n : _) <- words l, all isDigit n, not (null n) = go (Just (trim l)) ls
      | Just c <- caption, [name, "::", "UID"] <- words l, not (isSpace (head' l)) = (c, name) : go caption ls
      | otherwise = go caption ls
    head' l = case l of
      c : _ -> c
      []    -> ' '

-- | @Table 46 AC_element@ gives @T46@.
tableOf :: String -> String
tableOf caption = case words caption of
  _ : number : _ -> 'T' : number
  _              -> "T?"

-- | Each quasiquote @[name|@ at the start of a line, to its @|]@: its caption
--   (the first non-blank line) and its lines.
quasiquotes :: String -> [String] -> [(String, [String])]
quasiquotes name = go
  where
    go ls = case break (("[" ++ name ++ "|") `isPrefixOf`) ls of
      (_, _ : rest) -> let (body, after) = break ("|]" `isPrefixOf`) rest
                       in case dropWhile (all isSpace) body of
                            caption : _ -> (trim caption, body) : go after
                            []          -> go after
      _ -> []

-- | The rows of a quasiquoted ASCII table, the header row first: the @|...|@
--   lines between its @+---+@ separators, each row's cells joined across its
--   lines as the hand-written quasiquoters join them (trimmed pieces concatenated).
tableRows :: [String] -> [[String]]
tableRows = map joinRow . filter (not . null) . groups . filter (\l -> isCell l || isSeparator l)
  where
    lead = dropWhile isSpace
    isCell l = "|" `isPrefixOf` lead l
    isSeparator l = "+" `isPrefixOf` lead l
    groups ls = case break isSeparator ls of
      (row, _ : rest) -> row : groups rest
      (row, [])       -> [row]
    joinRow ls = map concat (transpose [ map trim (splitOn '|' (drop 1 (lead l))) | l <- ls ])

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : rest) -> a : splitOn c rest
  (a, [])       -> [a | not (all isSpace a)]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
