{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Hsed.CapabilitySpec
Description : Test specifications for the transports' capability: 'ReadOnly is not 'ReadWrite, and only the transports build either
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The type-level half of the jig's safety against the attached drive (design
9.3.1): a drive's transport is @'ReadOnly@, and IF-SEND and events take a
@'ReadWrite@ one. That holds only while no module can make one of the
other: so the signatures say it ("Hsed.Capability.TH" reads them); GHC
refuses a @'ReadOnly@ transport where a @'ReadWrite@ one is asked, by
'coerce' too, the role of 'Transport' being nominal ("Hsed.Capability.Refused",
its type errors deferred to run time); the constructor, "Hsed.Wire.Internal",
is imported only by "Hsed.Wire" and the modules that build or decorate
transports (@Hsed.Transport.*@, "Hsed.Jig.Guard", "Hsed.Jig.Transcript"),
and "Hsed.Wire" exports 'Transport' without it; and no module of the
library or the executables names unsafeCoerce. The sources are read when the
suite runs (stack runs it in the package's directory).
-}
module Hsed.CapabilitySpec (spec) where

import           Control.Exception       (TypeError (..))
import qualified RIO.List                as L
import           RIO
import           RIO.Directory           (doesDirectoryExist, doesFileExist, listDirectory)
import           RIO.FilePath            (dropExtension, splitDirectories, takeExtension, (</>))
import qualified RIO.Text                as T
import           Test.Hspec

import           Hsed.Capability.Refused (coercedToReadWrite, readOnlyAsReadWrite)
import           Hsed.Capability.TH      (transportCapability)
import           Hsed.Wire               (closeTransport, describeTransport, ifRecv, ifSend, inject, unitOf)


spec :: Spec
spec = do
  describe "a transport's capability, in the types" $ do
    it "IF-SEND and events take a 'ReadWrite transport" $ do
      ($(transportCapability 'ifSend) :: String) `shouldBe` "ReadWrite"
      ($(transportCapability 'inject) :: String) `shouldBe` "ReadWrite"
    it "IF-RECV, the unit, the description and closing take one of any capability" $
      [ $(transportCapability 'ifRecv), $(transportCapability 'unitOf)
      , $(transportCapability 'describeTransport), $(transportCapability 'closeTransport) ]
        `shouldBe` (["any", "any", "any", "any"] :: [String])
    it "refuses a 'ReadOnly transport where a 'ReadWrite one is asked" $
      evaluate readOnlyAsReadWrite `shouldThrow` capabilityMismatch
    it "refuses to coerce a 'ReadOnly transport to a 'ReadWrite one (Transport's role is nominal)" $
      evaluate coercedToReadWrite `shouldThrow` capabilityMismatch

  describe "Transport's constructor" $ do
    it "is imported from Hsed.Wire.Internal only by Hsed.Wire, the transports, the Guard and the Transcript" $ do
      sources <- concat <$> traverse modulesUnder sourceDirs
      [ name | (name, text) <- sources, internal `elem` importsOf text, not (mayImportInternal name) ]
        `shouldBe` []
    it "is exported by Hsed.Wire with Transport abstract" $ do
      exports <- exportList . decodeUtf8Lenient <$> readFileBinary ("src" </> "Hsed" </> "Wire.hs")
      exports `shouldSatisfy` (not . T.null)
      filter (`T.isInfixOf` exports) ["MkTransport", "Transport (..)", "Transport(..)", internal] `shouldBe` []

  describe "unsafeCoerce" $
    it "is named by no module of the library, the generated code or the executables" $ do
      sources <- concat <$> traverse modulesUnder sourceDirs
      [ name | (name, text) <- sources, "unsafeCoerce" `T.isInfixOf` text ] `shouldBe` []

-- | A deferred type error that says a 'ReadOnly transport is no 'ReadWrite one.
capabilityMismatch :: TypeError -> Bool
capabilityMismatch (TypeError msg) = all (`L.isInfixOf` msg) ["ReadOnly", "ReadWrite"]

-- | The module of the transport's constructor.
internal :: Text
internal = "Hsed.Wire.Internal"

-- | The modules allowed to see the transport's constructor: the module that
--   wraps it, the transports, and the two jig modules that decorate or send
--   on a drive's transport (design 9.3.1). The test suite may too.
mayImportInternal :: Text -> Bool
mayImportInternal name =
  name `elem` [internal, "Hsed.Wire", "Hsed.Jig.Guard", "Hsed.Jig.Transcript"]
  || "Hsed.Transport." `T.isPrefixOf` name

-- | The library's, the generated code's and the executables' source directories.
sourceDirs :: [FilePath]
sourceDirs = ["src", "gen", "app"]

-- | Every Haskell module under a directory, at any depth: its name (from its
--   path) and its text.
modulesUnder :: FilePath -> IO [(Text, Text)]
modulesUnder root = map named <$> files root
  where
    named (path, text) = (T.intercalate "." (map T.pack (drop 1 (splitDirectories (dropExtension path)))), text)
    files dir = do
      entries <- map (dir </>) . L.sort <$> listDirectory dir
      here    <- filterM (\p -> (takeExtension p == ".hs" &&) <$> doesFileExist p) entries
      below   <- filterM doesDirectoryExist entries
      texts   <- traverse (\p -> (,) p . decodeUtf8Lenient <$> readFileBinary p) here
      (texts <>) . concat <$> traverse files below

-- | The modules a source's import declarations name.
importsOf :: Text -> [Text]
importsOf = mapMaybe imported . T.lines
  where
    imported line = case T.words line of
      "import" : rest -> listToMaybe (map moduleName (dropWhile (`elem` ["qualified", "safe", "{-#", "SOURCE", "#-}"]) rest))
      _               -> Nothing
    moduleName = T.takeWhile (\c -> c /= '(' && c /= ' ')

-- | A module's export list: from its @module@ line to its @where@.
exportList :: Text -> Text
exportList text = T.unlines (takeWhile (not . ("where" `T.isInfixOf`)) (drop 1 header) <> take 1 (dropWhile (not . ("where" `T.isInfixOf`)) header))
  where header = dropWhile (not . ("module " `T.isPrefixOf`)) (T.lines text)
