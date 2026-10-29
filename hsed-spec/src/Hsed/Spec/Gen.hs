{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.Gen
Description : The deterministic printer of generated modules
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Assembles one generated module from the declarations the rules emit for it
(docs/spec-as-source.md section 4.2; docs/adding-a-rule.md). A rule prints its
own declarations ('Decl': its lines, the imports they need, where it comes
from, the language extensions they need) and names the module they go to
('GenModule'); this module knows nothing of what any rule generates. It writes
the header (the pragmas, every declaration's extensions united with the two
every module has; the module's Haddock with the description and notes),
merges the imports of every declaration (a whole-module import wins over an
import list; lists are united and sorted), and prints the declarations sorted
by table number, then rank within the table, then name. The same declarations
print the same text in any order. The runtime modules the declarations import
are named once here ('uidModule', 'coreTypesModule', 'coreTableModule',
'methodModule', 'codeModule', 'fieldModule', 'syncModule', 'propertiesModule').

Every definition carries a Haddock line naming its table, PDF and printed
page, structure ordinal and the hash of its cells ('provenanceComment',
design example E2).
-}
module Hsed.Spec.Gen
  ( -- * Where a definition comes from
    Provenance (..)
  , provenanceComment
  , tableNumber
    -- * Declarations and the modules they go to
  , GenModule (..)
  , Import (..)
  , Decl (..)
  , decl
  , specLabel
  , moduleName
  , specModuleDir
  , modulePath
    -- * The runtime modules generated code imports
  , uidModule
  , coreTypesModule
  , coreTableModule
  , methodModule
  , codeModule
  , fieldModule
  , syncModule
  , propertiesModule
    -- * Printing
  , renderModule
    -- * Pieces for the rules' printers
  , uidExpr
  , bytesExpr
  , hexByte
  , wrapText
  ) where

import qualified Data.Text        as T
import qualified Data.Text.Read   as TR
import           RIO
import qualified RIO.Char         as C
import           RIO.FilePath     ((</>))
import           RIO.List         (sortOn)
import qualified RIO.Map          as Map
import qualified RIO.Set          as Set

import           Hsed.Spec.Common (SpecId (..))


-- | Where a definition comes from, printed on its Haddock line.
data Provenance = Provenance
  { spec        :: !SpecId
  , tableId     :: !Text   -- ^ anchor id, @T46@
  , caption     :: !Text   -- ^ @Table 46 AC_element@
  , pdfPage1    :: !Int    -- ^ 1-based PDF page
  , printedPage :: !Int    -- ^ the page label printed on it
  , ordinal     :: !Int    -- ^ among the top-level tables of the structure tree
  , structPath  :: !Text   -- ^ @0/1288@
  , cellsHash   :: !Text   -- ^ sha256 of the table's cells, hex
  }
  deriving (Eq, Show)

-- | A generated module, named by its last component under @Hsed.Gen.\<Spec\>@.
data GenModule = GenModule
  { leaf        :: !Text     -- ^ @Types@: the module is @Hsed.Gen.Core.Types@ for @core-2.01@
  , description :: !Text     -- ^ the Haddock Description, before @, generated from \<spec\>@
  , notes       :: ![Text]   -- ^ sentences appended to the module's Haddock paragraph
  }
  deriving (Eq, Show)

-- | What a declaration needs imported.
data Import
  = ImportAll !Text           -- ^ @import M@
  | ImportSome !Text ![Text]  -- ^ @import M (a, b)@
  deriving (Eq, Show)

-- | One declaration as a rule printed it.
data Decl = Decl
  { declProvenance :: !Provenance  -- ^ the table it comes from: the first sort key, its number
  , declRank       :: !Int         -- ^ its place within the table (a row's index), the second
  , declName       :: !Text        -- ^ the name it defines first, the third
  , declBody       :: ![Text]      -- ^ its lines, Haddock included
  , declImports    :: ![Import]
  , declExtensions :: ![Text]      -- ^ the LANGUAGE extensions its lines need (@DataKinds@)
  }
  deriving (Eq, Show)

-- | A declaration of rank 0 with no imports and no extensions; set the rest by record update.
decl :: Provenance -> Text -> [Text] -> Decl
decl p n body = Decl { declProvenance = p, declRank = 0, declName = n, declBody = body, declImports = [], declExtensions = [] }

-- | The spec's name in generated identifiers: @Core@ for @core-2.01@.
specLabel :: SpecId -> Text
specLabel sid = capitalise (T.takeWhile C.isAlpha (specIdText sid))
  where capitalise t = maybe t (\(c, cs) -> T.cons (C.toUpper c) (T.toLower cs)) (T.uncons t)

-- | @Hsed.Gen.Core.Types@ for @core-2.01@ and a module whose leaf is @Types@.
moduleName :: SpecId -> GenModule -> Text
moduleName sid gm = "Hsed.Gen." <> specLabel sid <> "." <> gm.leaf

-- | The directory of a spec's generated modules under an output directory:
--   @DIR/Hsed/Gen/Core@; @hsedc compile@ owns it.
specModuleDir :: FilePath -> SpecId -> FilePath
specModuleDir dir sid = dir </> "Hsed" </> "Gen" </> T.unpack (specLabel sid)

-- | Where a module goes under an output directory: @DIR/Hsed/Gen/Core/Types.hs@.
modulePath :: FilePath -> Text -> FilePath
modulePath dir name = dir </> T.unpack (T.replace "." "/" name) <> ".hs"


-- * The runtime modules generated code imports

-- | @UID@, @uid@, @HalfUID@, @halfUID@.
uidModule :: Text
uidModule = "System.SED.MCTP.Common.UID"

-- | @TypeDef@ and its constructors, the @Enumerated@ class.
coreTypesModule :: Text
coreTypesModule = "Hsed.Core.Types"

-- | @TableDesc@, @Col@, the cells and rows of the tables of values.
coreTableModule :: Text
coreTableModule = "Hsed.Core.Table"

-- | The @Method@ and @MethodSignature@ classes a method record instantiates, @Signature@, @Param@, @Response@, @Value@.
methodModule :: Text
methodModule = "Hsed.Core.Method"

-- | @Span@ and @CodeRow@: the rows of a code table.
codeModule :: Text
codeModule = "Hsed.Core.Code"

-- | @FieldValue@, @FieldConst@ and @FieldDef@: the rows of a record table.
fieldModule :: Text
fieldModule = "Hsed.Core.Field"

-- | A state table's runtime form: @StateTable@, @StateRow@, @Setting@.
syncModule :: Text
syncModule = "Hsed.Core.Sync"

-- | The Properties method's runtime form: @Property@, @Communicator@, @Assumptions@.
propertiesModule :: Text
propertiesModule = "Hsed.Core.Properties"


-- * Printing

-- | The whole module: header, imports, then the declarations in order.
renderModule :: Text -> GenModule -> [Decl] -> Text
renderModule modName gm decls = T.unlines (header <> importLines (concatMap (.declImports) decls) <> concatMap (("" :) . (.declBody)) sorted)
  where
    sorted = sortOn (\d -> (tableNumber d.declProvenance, d.declRank, d.declName)) decls
    specs  = Set.toList (Set.fromList (map (\d -> specIdText d.declProvenance.spec) sorted))
    extensions = Set.toList (Set.fromList (["NoImplicitPrelude", "OverloadedStrings"] <> concatMap (.declExtensions) decls))
    header =
      [ "{-# LANGUAGE " <> x <> " #-}" | x <- extensions ]
      <>
      [ ""
      , "{-|"
      , "Module      : " <> modName
      , "Description : " <> gm.description <> ", generated from " <> T.intercalate ", " specs
      , "Copyright   : (c) Magnolia Heights R&D, 2026"
      , "License     : BSD3"
      , "Maintainer  : scott@magnolia-heights.com"
      , "" ]
      <> wrapText 80 (T.unwords (generatedNote : gm.notes))
      <> [ "-}"
         , "module " <> modName <> " where"
         , "" ]
    generatedNote = T.unwords
      [ "Generated by hsedc: do not edit. Each definition names its table, its PDF and"
      , "printed page, its ordinal in the structure tree and the sha256 of its cells"
      , "(docs/spec-as-source.md section 3.5). The tables are TCG's text: see"
      , "tcg/TCG-NOTICE.txt." ]

-- | The import lines: one per module, in module order, a whole-module import
--   winning over a list; the lists united, sorted case-insensitively, and
--   aligned on the longest module name that has one.
importLines :: [Import] -> [Text]
importLines is = map line (Map.toList merged)
  where
    merged = Map.fromListWith combine [ entry i | i <- is ]
    entry (ImportAll m)       = (m, Nothing)
    entry (ImportSome m xs)   = (m, Just (Set.fromList xs))
    combine a b = (<>) <$> a <*> b
    width = 1 + foldl' max 0 [ T.length m | (m, Just _) <- Map.toList merged ]
    line (m, Nothing) = "import           " <> m
    line (m, Just xs) = "import           " <> m <> T.replicate (width - T.length m) " "
                        <> "(" <> T.intercalate ", " (sortOn (\x -> (T.toLower x, x)) (Set.toList xs)) <> ")"

-- | @uid 0x00 0x00 0x00 0x05 0x00 0x00 0x08 0x01@
uidExpr :: [Word8] -> Text
uidExpr = bytesExpr "uid"

-- | A function applied to bytes: @bytesExpr "halfUID" [0, 0, 0, 7]@ is @halfUID 0x00 0x00 0x00 0x07@.
bytesExpr :: Text -> [Word8] -> Text
bytesExpr fn bytes = T.unwords (fn : map hexByte bytes)

-- | @0x0f@
hexByte :: Word8 -> Text
hexByte b = "0x" <> T.pack [digit (b `div` 16), digit (b `mod` 16)]
  where digit d = T.index "0123456789abcdef" (fromIntegral d)

-- | Greedy word wrap: as many words to a line as fit in the width.
wrapText :: Int -> Text -> [Text]
wrapText width = reverse . map T.unwords . foldl' place [] . T.words
  where
    place [] w = [[w]]
    place (current : done) w
      | T.length (T.unwords (current <> [w])) <= width = (current <> [w]) : done
      | otherwise = [w] : current : done

-- | @-- | core-2.01 T46 AC_element (PDF 100 \/ p.83, ord 48, sha256:3f1a2b3c4d5e)@
provenanceComment :: Provenance -> Text
provenanceComment p = T.concat
  [ "-- | ", specIdText p.spec, " ", p.tableId, " ", title
  , " (PDF ", tshow p.pdfPage1, " / p.", tshow p.printedPage, ", ord ", tshow p.ordinal
  , ", sha256:", T.take 12 p.cellsHash, ")" ]
  where
    title = fromMaybe p.caption (T.stripPrefix ("Table " <> T.drop 1 p.tableId <> " ") p.caption)

-- | The number in an anchor id (@T46@ -> 46); an id without one sorts last.
tableNumber :: Provenance -> Int
tableNumber p = either (const maxBound) fst (TR.decimal (T.drop 1 p.tableId) :: Either String (Int, Text))
