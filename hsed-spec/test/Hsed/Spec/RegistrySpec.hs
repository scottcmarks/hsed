{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.RegistrySpec
Description : The registry of rules, and the generic modules that iterate it
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

docs/adding-a-rule.md: every rule is one module under @src/Hsed/Spec/Rules/@,
named after the rule (@typeRow@ in @TypeRow.hs@), and one line of
"Hsed.Spec.Registry"; no two rules share a name or a shape, nor claim one
finding code (nor a rule a code the generic modules or a shared reader
claim: the catalogue is assembled from all three); and the generic
modules name no rule, no rule's shape and no rule's module, those of the
registry and those still to come (every rule token of "Hsed.Spec.DSL", every
shape a table can be bound with), so that a rule added later edits no generic
module. Adding a rule touches its own files, the registry's two lines, and
files @make gen@ rewrites (the sidecar's renderings, the bindings, the
generated modules, @docs/LEDGER.md@).
-}
module Hsed.Spec.RegistrySpec (spec) where

import qualified RIO.Char          as C
import           RIO.Directory     (listDirectory)
import           RIO.FilePath      (takeBaseName, takeExtension, (</>))
import qualified RIO.List          as L
import qualified Data.Text         as T
import           RIO
import           Test.Hspec

import qualified RIO.Map           as Map
import           Hsed.Spec.Common  (CodeDef (..), RuleName (..))
import           Hsed.Spec.DSL     (knownRules)
import           Hsed.Spec.Readers (readerCodes)
import           Hsed.Spec.Registry (registry)
import           Hsed.Spec.Report  (genericCodes)
import           Hsed.Spec.RuleDef (RuleDef (..), ruleShape)
import           Hsed.Spec.Shape   (Shape (..), shapeName)


-- | The modules that must know no rule: they iterate the registry.
genericModules :: [FilePath]
genericModules =
  map ("src/Hsed/Spec" </>)
    ["Rules.hs", "Scope.hs", "Check.hs", "Compile.hs", "Gen.hs", "Cli.hs", "RuleDef.hs", "Report.hs", "Bind.hs", "Document.hs", "Readers.hs"]
  <> ["app/Main.hs"]

-- | Haskell source without its comments: @{- ... -}@ blocks and @--@ to the end of a line.
withoutComments :: Text -> Text
withoutComments = T.unlines . map lineComment . T.lines . blocks
  where
    blocks t = case T.breakOn "{-" t of
      (code, rest) | T.null rest -> code
                   | otherwise   -> code <> blocks (T.drop 2 (snd (T.breakOn "-}" rest)))
    lineComment l = fst (T.breakOn "--" l)

identifiers :: Text -> [Text]
identifiers = filter (not . T.null) . T.split (\c -> not (C.isAlphaNum c || c == '_' || c == '\''))

capitalise :: Text -> Text
capitalise t = maybe t (\(c, cs) -> T.cons (C.toUpper c) cs) (T.uncons t)

spec :: Spec
spec = describe "the registry" $ do
  let names = map (ruleNameText . ruleName) registry
  it "names no rule twice" $
    L.nub names `shouldBe` names
  it "gives no two rules one shape, and every rule a shape" $ do
    let shapes = map ruleShape registry
    all isJust shapes `shouldBe` True
    L.nub shapes `shouldBe` shapes
  it "lets no two rules claim one finding code, nor a rule a code the generic modules or a shared reader claim" $ do
    let claims = [ ("generic", c) | c <- genericCodes ] <> [ ("readers", c) | c <- readerCodes ]
                 <> [ (ruleNameText (ruleName r), c) | r <- registry, c <- ruleCodes r ]
        owners = Map.fromListWith (flip (<>)) [ (codeName c, [owner]) | (owner, c) <- claims ]
    [ (code, os) | (code, os@(_ : _ : _)) <- Map.toList owners ] `shouldBe` []
  it "lists every module under src/Hsed/Spec/Rules/, each named after its rule, and nothing else" $ do
    files <- listDirectory "src/Hsed/Spec/Rules"
    L.sort [ T.pack (takeBaseName f) | f <- files, takeExtension f == ".hs" ] `shouldBe` L.sort (map capitalise names)
  it "is known to no generic module: no rule's name, shape or module appears in their code" $ do
    let banned = names <> map capitalise names <> map (<> "Rule") names <> mapMaybe (fmap shapeName . ruleShape) registry
    forM_ genericModules $ \path -> do
      code <- withoutComments <$> readFileUtf8 path
      (path, L.nub [ w | w <- identifiers code, w `elem` banned ]) `shouldBe` (path, [])
    -- and the search finds them where they belong
    registryCode <- withoutComments <$> readFileUtf8 "src/Hsed/Spec/Registry.hs"
    length (filter (`elem` banned) (identifiers registryCode)) `shouldSatisfy` (>= 2 * length registry)
  it "is known to no generic module before it is written: no rule token of the vocabulary, no table shape" $ do
    let tokens = map ruleNameText knownRules
        shapes = [ shapeName s | s <- [minBound .. maxBound], s `notElem` [Doc, Heading] ]
        banned = tokens <> map capitalise tokens <> map (<> "Rule") tokens <> shapes
    forM_ genericModules $ \path -> do
      code <- withoutComments <$> readFileUtf8 path
      (path, L.nub [ w | w <- identifiers code, w `elem` banned ]) `shouldBe` (path, [])
