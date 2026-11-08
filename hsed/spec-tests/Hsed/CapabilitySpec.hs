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
library or the executables names unsafeCoerce. A drive reaches a run only
behind the Guard (design 13.29 (l), M4c): the drive's transports, the kext's
("Hsed.Transport.MacKext") and the replay's ("Hsed.Transport.Replay"), are
imported by "Hsed.Jig.Target" alone, which puts each behind the Guard as
it opens it, and "Hsed.Jig.Guard" exports 'Hsed.Jig.Guard.GuardedDrive'
abstract, without its constructor; and no module of the library, the
generated code or the executables imports a function of the portable C
(cbits/hsed_cdb.c, compiled into every build), which hsed's doctests,
interpreting the library's sources, could not link. The sources are read
when the suite runs (stack runs it in the package's directory).

Since M5c (design 11 M5c, the LAN leg) two more. hsed-sim, which serves the
simulator to other processes, reaches no module of the jig and none of a
drive's: the imports of its modules (@app/hsed-sim@) followed through every
module of @src@ and @gen@ they reach, to a fixed point, name no
@Hsed.Jig.*@ module, nor "Hsed.Transport.Kext", "Hsed.Transport.MacKext",
"Hsed.Transport.Cdb", "Hsed.Transport.Drive" or "Hsed.Transport.Replay" (the
walk shown transitive by hsed-jig's Main, which reaches the kext's transport
through modules it imports, never directly). And nothing listens but through
"Hsed.Transport.Mhtx.Conn", whose one bind takes a port alone and binds
127.0.0.1 (D6; Scott's answer Q1): no other module of @src@, @app@ or
@lan-tests@ names a function that binds a listening socket, Network.Socket's
bind or listen (a listen unbound binds the wildcard address), warp's run,
runEnv or runSettings (whose default host is every IPv4 address; warp's
runSettingsSocket, which serves a socket already bound, is not one), or
websockets' runServer, runServerWith, runServerWithOptions or
makeListenSocket; a module names one where its import of the function's
module lists it, brings it into scope unqualified (an open import, or a
hiding list that leaves it), or writes it qualified by the import's alias or
module; and the wildcard address 0.0.0.0 is written in none of them.

And two of group G4's (M5c). The jig reads the simulator's state through
its view alone ("Hsed.Sim.View"'s queries, "Hsed.Jig.Script".@peekSim@;
Option A), in process as over the LAN leg: no module under @src/Hsed/Jig@
names the state's type (@TPerState@), the in-process control's reader of
it (@scPeek@), or the scaffold's transitional accessor
(@simStateInProc@), comments included. And hsed-test opens no socket, a
rule rather than a discipline (the lead's L4; the socket suites are
hsed-lan-test's): no module of its sources (@spec-tests@, @fixture@)
names, by the import rule above, a function of 'listeners' nor one that
dials or serves a socket bound (network's connect, websockets' clients,
warp's runSettingsSocket), nor one of the LAN suite's support
(Hsed.Lan.Support's withHub, withHubOn, probe, rawHttp); nor writes in its
code (its comments and literals left out, so that prose and these
examples' own names are not code) one of hsed's own that listens, dials
or serves: Hsed.Transport.Mhtx.Conn's bindLoopback and dialLoopback,
Hsed.Transport.Mhtx.Hub's runHub, the support's withHub, withHubOn and
rawHttp. A client opened with "Hsed.Transport.Mhtx.Client"'s own dialer
(@clientOpts@'s @coDial@) dials too; the specs in hsed-test give theirs
(over "Hsed.Transport.Mhtx.Conn".@memoryPair@), which no rule of names can
see.
-}
module Hsed.CapabilitySpec (spec) where

import           Control.Exception       (TypeError (..))
import           Prelude                 (until)
import qualified RIO.Char                as C
import qualified RIO.List                as L
import qualified RIO.Map                 as Map
import           RIO
import           RIO.Directory           (doesDirectoryExist, doesFileExist, listDirectory)
import           RIO.FilePath            (dropExtension, splitDirectories, takeExtension, (</>))
import qualified RIO.Set                 as Set
import qualified RIO.Text                as T
import qualified RIO.Text.Partial        as T'
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

  describe "a drive's transports (design 13.29 (l), M4c)" $ do
    it "are imported under src, gen and app by Hsed.Jig.Target alone: Hsed.Transport.MacKext and Hsed.Transport.Replay" $ do
      sources <- concat <$> traverse modulesUnder sourceDirs
      [ (name, m) | (name, text) <- sources, m <- importsOf text, m `elem` driveTransports ]
        `shouldBe` [ ("Hsed.Jig.Target", m) | m <- driveTransports ]
    it "reach a run as Hsed.Jig.Guard's GuardedDrive, which its export list names without (..)" $ do
      exports <- exportList . decodeUtf8Lenient <$> readFileBinary ("src" </> "Hsed" </> "Jig" </> "Guard.hs")
      [ w | w <- T.words (T.map (\c -> if c == ',' then ' ' else c) exports), "GuardedDrive" `T.isPrefixOf` w ] `shouldBe` ["GuardedDrive"]
      filter (`T.isInfixOf` exports) ["GuardedDrive (..)", "GuardedDrive(..)", "GuardedDrive ("] `shouldBe` []

  describe "the portable C (cbits/hsed_cdb.c)" $
    it "is foreign-imported by no module of the library, the generated code or the executables" $ do
      sources <- concat <$> traverse modulesUnder sourceDirs
      [ (name, line) | (name, text) <- sources, line <- T.lines text, "foreign import" `T.isPrefixOf` T.stripStart line
                     , "hsed_cdb" `T.isInfixOf` line ] `shouldBe` []

  describe "unsafeCoerce" $
    it "is named by no module of the library, the generated code or the executables" $ do
      sources <- concat <$> traverse modulesUnder sourceDirs
      [ name | (name, text) <- sources, "unsafeCoerce" `T.isInfixOf` text ] `shouldBe` []

  describe "hsed-sim (design 11 M5c)" $
    it "imports, through every module of src and gen its imports reach, no Hsed.Jig module and none of a drive's (Kext, MacKext, Cdb, Drive, Replay)" $ do
      library <- Map.fromList . concat <$> traverse modulesUnder ["src", "gen"]
      -- the walk is transitive: hsed-jig's Main reaches the kext's transport through the modules it imports
      jigMain <- decodeUtf8Lenient <$> readFileBinary ("app" </> "hsed-jig" </> "Main.hs")
      (importsOf jigMain, Set.member "Hsed.Transport.MacKext" (importClosure library (importsOf jigMain)))
        `shouldSatisfy` (\(direct, reached) -> "Hsed.Transport.MacKext" `notElem` direct && reached)
      simModules <- modulesUnder ("app" </> "hsed-sim")
      filter outsideSim (Set.toList (importClosure library (concatMap (importsOf . snd) simModules))) `shouldBe` []

  describe "listening (design 11 M5c; D6 and Scott's answer Q1: 127.0.0.1 alone)" $
    it "is Hsed.Transport.Mhtx.Conn's alone: no other module of src, app or lan-tests names a function that binds a listening socket, and none writes 0.0.0.0" $ do
      sources <- concat <$> traverse modulesUnder ["src", "app", "lan-tests"]
      [ (name, why) | (name, text) <- sources, name /= "Hsed.Transport.Mhtx.Conn", why <- listenersNamed text ] `shouldBe` []
      [ name | (name, text) <- sources, "0.0.0.0" `T.isInfixOf` text ] `shouldBe` []

  describe "the simulator's state, seen from the jig (design 11 M5c, Option A)" $
    it "is read through the simulator's view alone: no module under src/Hsed/Jig names TPerState, scPeek or simStateInProc" $ do
      jig <- modulesUnder ("src" </> "Hsed" </> "Jig")
      jig `shouldSatisfy` any ((== "Hsed.Jig.Script") . fst)
      [ (name, word) | (name, text) <- jig, word <- ["TPerState", "scPeek", "simStateInProc"], word `T.isInfixOf` text, word `standsIn` text ]
        `shouldBe` []

  describe "hsed-test's sockets (design 11 M5c; the lead's L4: the socket suites are hsed-lan-test's)" $
    it "are none: no module of spec-tests or fixture names a function that listens, dials or serves a socket, nor writes in its code one of hsed's own (bindLoopback, dialLoopback, runHub, withHub, withHubOn, rawHttp)" $ do
      sources <- concat <$> traverse modulesUnder ["spec-tests", "fixture"]
      sources `shouldSatisfy` any ((== "Hsed.CapabilitySpec") . fst)
      [ (name, why) | (name, text) <- sources, any (`T.isInfixOf` text) ["Network.", "Hsed.Lan."], why <- namesFrom openers text ] `shouldBe` []
      [ (name, f) | (name, text) <- sources, let code = codeOf text, f <- ownOpeners, f `T.isInfixOf` text, f `standsIn` code ] `shouldBe` []
      -- the rule sees a use however it is written, and no name that prose or a literal holds
      [ f | f <- ownOpeners, f `standsIn` codeOf ("x <- Conn." <> f <> " 0 -- a comment naming " <> f) ] `shouldBe` ownOpeners
      [ f | f <- ownOpeners, f `standsIn` codeOf ("{- " <> f <> " {- nested -} " <> f <> " -}\nx = \"" <> f <> "\\\"\" <> y -- " <> f) ]
        `shouldBe` []

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

-- | The modules of the drive's transports, which Hsed.Jig.Target alone
--   imports, in the order it imports them.
driveTransports :: [Text]
driveTransports = ["Hsed.Transport.MacKext", "Hsed.Transport.Replay"]

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

-- | The modules a list of imports reaches, walked to a fixed point through
--   the sources a map holds by module name: each module named, and in turn
--   the modules each one the map holds imports (one of another package, or
--   of hsed-core, ends its branch).
importClosure :: Map Text Text -> [Text] -> Set Text
importClosure sources roots = fst (until (Set.null . snd) step (Set.empty, Set.fromList roots))
  where
    step (seen, frontier) =
      let seen' = seen <> frontier
      in (seen', Set.fromList [ m | name <- Set.toList frontier, Just text <- [Map.lookup name sources], m <- importsOf text ]
                   `Set.difference` seen')

-- | A module hsed-sim may not reach: the jig's, and a drive's transports and
--   what they are built on.
outsideSim :: Text -> Bool
outsideSim name =
  "Hsed.Jig." `T.isPrefixOf` name
  || name `elem` ["Hsed.Transport.Kext", "Hsed.Transport.MacKext", "Hsed.Transport.Cdb", "Hsed.Transport.Drive", "Hsed.Transport.Replay"]

-- | The functions that bind a listening socket, by the module that exports
--   them (and the modules under it): network's bind, and its listen, which
--   on a socket not bound binds the wildcard address; warp's runners but
--   runSettingsSocket, which serves a socket already bound; websockets'
--   servers (s1-sources, read from the packages' sources at M5c's S1.0).
listeners :: [(Text, [Text])]
listeners =
  [ ("Network.Socket", ["bind", "listen"])
  , ("Network.Wai.Handler.Warp", ["run", "runEnv", "runSettings"])
  , ("Network.WebSockets", ["runServer", "runServerWith", "runServerWithOptions", "makeListenSocket"])
  ]

-- | Each function of 'listeners' a source names, and how ('namesFrom').
listenersNamed :: Text -> [Text]
listenersNamed = namesFrom listeners

-- | Each function of a list of them by module a source names, and how:
--   listed by an import of its module (not as hidden), in scope unqualified
--   by an import with no list or a hiding list that leaves it, or written
--   qualified by the import's alias or by the module's name.
namesFrom :: [(Text, [Text])] -> Text -> [Text]
namesFrom functions text =
  [ idModule d <> "." <> f <> ": " <> how
  | d <- importDecls text
  , (family, fs) <- functions
  , idModule d == family || (family <> ".") `T.isPrefixOf` idModule d
  , f <- fs
  , Just how <- [naming d f]
  ]
  where
    naming d f
      | not (idHiding d), f `elem` fromMaybe [] (idList d)                         = Just "listed by its import"
      | not (idQualified d), maybe True (\l -> idHiding d && f `notElem` l) (idList d) = Just "in scope unqualified"
      | any (\q -> (q <> "." <> f) `standsIn` text) (maybe [] pure (idAlias d) <> [idModule d]) = Just "written qualified"
      | otherwise                                                                  = Nothing

-- | Whether a name is written in a text as a word of its own, no longer
--   name's part (the last part of a qualified one counts:
--   @Hsed.Transport.InProc.scPeek@ writes @scPeek@).
standsIn :: Text -> Text -> Bool
standsIn name text = any (\(front, back) -> not (endsIdent front) && not (startsIdent (T.drop (T.length name) back))) (T'.breakOnAll name text)
  where
    endsIdent   = T.any identChar . T.takeEnd 1
    startsIdent = T.any identChar . T.take 1

-- | A character of a Haskell name.
identChar :: Char -> Bool
identChar c = C.isAlphaNum c || c == '_' || c == '\''

-- | What opens a socket, by the module that exports it: 'listeners', and
--   network's connect, websockets' clients, warp's server of a socket bound
--   (runSettingsSocket), and the LAN suite's support, which only
--   hsed-lan-test's sources hold (lan-tests/Hsed/Lan/Support.hs).
openers :: [(Text, [Text])]
openers = listeners <>
  [ ("Network.Socket", ["connect"])
  , ("Network.WebSockets", ["runClient", "runClientWith", "runClientWithSocket", "runClientWithStream"])
  , ("Network.Wai.Handler.Warp", ["runSettingsSocket"])
  , ("Hsed.Lan.Support", ["withHub", "withHubOn", "probe", "rawHttp"])
  ]

-- | hsed's own functions that listen on a loopback socket, dial one, or
--   serve or start a hub that does: Hsed.Transport.Mhtx.Conn's
--   bindLoopback and dialLoopback, Hsed.Transport.Mhtx.Hub's runHub, the LAN
--   suite's withHub, withHubOn and rawHttp (its probe is a word other code
--   uses: the import rule alone holds it, 'openers').
ownOpeners :: [Text]
ownOpeners = ["bindLoopback", "dialLoopback", "runHub", "withHub", "withHubOn", "rawHttp"]

-- | A source's code: its comments (line comments, and block comments,
--   nested, the Haddock and the pragmas among them), its string literals
--   and its character literals each left out as a space, so that a name
--   prose or a literal holds is not one the code writes (Haskell 2010 2.3
--   and 2.6: two dashes or more begin a comment unless a symbol follows
--   them, @-->@ being an operator; a quote after a name's character is part
--   of the name).
codeOf :: Text -> Text
codeOf = T.pack . code ' ' . T.unpack
  where
    code prev s = case s of
      []                                                      -> []
      '{' : '-' : rest                                        -> ' ' : block (1 :: Int) rest
      '-' : '-' : rest | dashes rest                          -> code ' ' (dropWhile (/= '\n') rest)
      '"' : rest                                              -> ' ' : code ' ' (string rest)
      '\'' : rest | not (identChar prev), Just beyond <- character rest -> ' ' : code ' ' beyond
      c : rest                                                -> c : code c rest
    block depth s = case s of
      []               -> []
      '{' : '-' : rest -> block (depth + 1) rest
      '-' : '}' : rest -> if depth == 1 then ' ' : code ' ' rest else block (depth - 1) rest
      _ : rest         -> block depth rest
    dashes rest = case dropWhile (== '-') rest of
      c : _ -> c `notElem` ("!#$%&*+./<=>?@\\^|~:" :: String)
      []    -> True
    string s = case s of
      []              -> []
      '\\' : _ : rest -> string rest
      '"' : rest      -> rest
      _ : rest        -> string rest
    character s = case s of
      '\\' : _ : rest -> case break (== '\'') rest of
        (_, _ : beyond) -> Just beyond
        _               -> Nothing
      c : '\'' : beyond | c /= '\'' -> Just beyond
      _                               -> Nothing

-- | An import declaration: the module, whether qualified, its alias, whether
--   its list hides, and the names its list holds (none without a list).
data ImportDecl = ImportDecl
  { idModule    :: !Text
  , idQualified :: !Bool
  , idAlias     :: !(Maybe Text)
  , idHiding    :: !Bool
  , idList      :: !(Maybe [Text])
  }

-- | A source's import declarations, each read whole: a line that begins
--   @import@ and the indented lines after it, their comments left out.
importDecls :: Text -> [ImportDecl]
importDecls = mapMaybe (importDecl . T.unwords) . declarations . map (fst . T'.breakOn "--") . T.lines
  where
    declarations ls = case dropWhile (not . ("import " `T.isPrefixOf`)) ls of
      []         -> []
      (l : rest) -> let (more, others) = span (maybe False (C.isSpace . fst) . T.uncons) rest in (l : more) : declarations others
    importDecl line = case filter (`notElem` ["safe", "{-#", "SOURCE", "#-}"]) (T.words (T.takeWhile (/= '(') line)) of
      "import" : ws | m : more <- filter (/= "qualified") ws -> Just ImportDecl
        { idModule    = m
        , idQualified = "qualified" `elem` ws
        , idAlias     = case dropWhile (/= "as") more of
                          _ : a : _ -> Just a
                          _         -> Nothing
        , idHiding    = "hiding" `elem` more
        , idList      = case T'.breakOn "(" line of
                          (_, list) | T.null list -> Nothing
                                    | otherwise   -> Just (T.words (T.map (\c -> if c `elem` ("()," :: String) then ' ' else c) list))
        }
      _ -> Nothing

-- | A module's export list: from its @module@ line to its @where@.
exportList :: Text -> Text
exportList text = T.unlines (takeWhile (not . ("where" `T.isInfixOf`)) (drop 1 header) <> take 1 (dropWhile (not . ("where" `T.isInfixOf`)) header))
  where header = dropWhile (not . ("module " `T.isPrefixOf`)) (T.lines text)
