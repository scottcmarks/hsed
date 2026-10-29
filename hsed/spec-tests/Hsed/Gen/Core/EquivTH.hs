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

* 'generatedDefs' reads the modules @hsedc compile@ wrote: every signature
  @name :: T@ whose type the hand-written modules also have (@UID@, @String@,
  @HalfUID@ and the two name maps; a @TypeDef@ has no hand-written
  counterpart) and every @data Core_\<enum\>@, each with the caption of the
  table its provenance line names;
* 'handDefs' reads hsed's hand-written modules: every @[ttype|@ quasiquote
  (@u\<Name\>Type@, @f\<Name\>Type@), every @[tenum|@ (@Core_\<enum\>@) and the
  @[t240|@ rows (@h\<T\>@, @u\<T\>Table@, @u\<T\>TableObject@, and whichever of
  the typed @oth\<T\>@ or @bth\<T\>@ the module defines; then @nameHalfUID@ and
  @nameUID@), each with the caption the quasiquote prints. A name the source
  promises and the module does not define stops the compilation.

A value is 'Scalar' ('show' of it) or, for an enumeration, 'Entries': each
constructor with the value the spec assigns it (the generated module's
'Hsed.Core.Types.enumValue'; the hand-written module's 'fromEnum', since its
enumerations start at 0).
-}
module Hsed.Gen.Core.EquivTH
  ( Def (..)
  , Value (..)
  , generatedDefs
  , handDefs
  ) where

import           Control.Monad              (forM)
import           Data.Char                  (isDigit, isSpace)
import           Data.List                  (isPrefixOf, stripPrefix, transpose)
import           Data.Maybe                 (catMaybes)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (addDependentFile)

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

-- | @$(generatedDefs ["gen/Hsed/Gen/Core/Types.hs", ...] "G")@, the modules imported qualified as @G@.
generatedDefs :: [FilePath] -> String -> Q Exp
generatedDefs paths g = do
  mapM_ addDependentFile paths
  srcs <- runIO (mapM readFile paths)
  defs <- fmap concat (forM srcs (\src -> forM (generatedNames (lines src)) one))
  listE (map pure defs)
  where
    one (n, table, caption, isEnum) = do
      found <- (if isEnum then lookupTypeName else lookupValueName) (g ++ "." ++ n)
      case found of
        Nothing -> fail ("the generated module declares " ++ n ++ " but " ++ g ++ " does not export it")
        Just nm
          | isEnum    -> [| Def n table caption (Entries [ (show x, enumValue x) | x <- [minBound .. maxBound :: $(conT nm)] ]) |]
          | otherwise -> [| Def n table caption (Scalar (show $(varE nm))) |]

-- | The definitions of a generated module with the table and caption of the
--   provenance line above each: @-- | core-2.01 T46 AC_element (PDF 100 / ...@.
generatedNames :: [String] -> [(String, String, String, Bool)]
generatedNames = go Nothing
  where
    go _ [] = []
    go prov (l : ls)
      | Just rest <- stripPrefix "-- | " l, Just p <- provenance rest = go (Just p) ls
      | Just (table, caption) <- prov, Just n <- signature l = (n, table, caption, False) : go prov ls
      | Just (table, caption) <- prov, Just n <- stripPrefix "data " l = (n, table, caption, True) : go prov ls
      | otherwise = go prov ls
    -- "core-2.01 T46 AC_element (PDF 100 / p.83, ...": the table and its caption
    provenance rest = case words rest of
      _spec : table@('T' : number) : more | all isDigit number ->
        Just (table, unwords (["Table", number] ++ takeWhile (/= "(PDF") more))
      _ -> Nothing
    signature l = case words l of
      n : "::" : ty | unwords ty `elem` handTypes -> Just n
      _ -> Nothing
    handTypes = ["UID", "String", "HalfUID", "Map HalfUID String", "Map UID String"]


-- * The hand-written modules

-- | @$(handDefs "src/.../TypeUIDs.hs" "src/.../TableUIDs.hs" "H")@, both imported qualified as @H@.
handDefs :: FilePath -> FilePath -> String -> Q Exp
handDefs typeUIDs tableUIDs h = do
  mapM_ addDependentFile [typeUIDs, tableUIDs]
  types  <- runIO (readFile typeUIDs)
  tables <- runIO (readFile tableUIDs)
  typeDefs  <- forM (quasiquotes "ttype" (lines types)) typeRow
  enumDefs  <- forM (quasiquotes "tenum" (lines types)) enumeration
  tableDefs <- forM (quasiquotes "t240" (lines tables)) t240
  listE (map pure (concat typeDefs ++ enumDefs ++ concat tableDefs))
  where
    value n = maybe (fail (h ++ " does not define " ++ n ++ ", which its source promises")) pure
                =<< lookupValueName (h ++ "." ++ n)
    scalar caption n = do
      nm <- value n
      [| Def n (tableOf caption) caption (Scalar (show $(varE nm))) |]
    -- a Type table row: its Name cell names u<Name>Type and f<Name>Type
    typeRow (caption, body) = case drop 1 (tableRows body) of
      [_, name, _] : _ -> mapM (scalar caption) ["u" ++ name ++ "Type", "f" ++ name ++ "Type"]
      _                -> fail ("no Type table row under " ++ caption)
    -- an enumeration: its caption names the type, Core_<type>
    enumeration (caption, _) = case words caption of
      _ : _ : ident : _ -> do
        let n = "Core_" ++ ident
        found <- lookupTypeName (h ++ "." ++ n)
        nm <- maybe (fail (h ++ " does not define " ++ n ++ ", which its source promises")) pure found
        [| Def n (tableOf caption) caption (Entries [ (show x, fromEnum x) | x <- [minBound .. maxBound :: $(conT nm)] ]) |]
      _ -> fail ("no enumeration named by " ++ caption)
    -- Table 240: three UIDs per row, the typed half-UID the module has, then the two maps
    t240 (caption, body) = do
      rows <- fmap concat $ forM [ name | [_, _, name, _] <- drop 1 (tableRows body) ] $ \name -> do
        typed <- catMaybes <$> mapM (\p -> fmap (const (p ++ name)) <$> lookupValueName (h ++ "." ++ p ++ name)) ["oth", "bth"]
        mapM (scalar caption) (["u" ++ name ++ "TableObject", "u" ++ name ++ "Table", "h" ++ name] ++ typed)
      maps <- mapM (scalar caption) ["nameHalfUID", "nameUID"]
      pure (rows ++ maps)

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
