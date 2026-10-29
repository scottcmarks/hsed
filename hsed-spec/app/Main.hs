{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Main
Description : hsedc, the hsed spec compiler
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The command line of the spec compiler (docs/spec-as-source.md sections 4.2 and
5.2). Every command reads an IR v1 file, by default the annotated extraction
@build/core-2.01.hsed.json@ (@make ir-annotated@); @--hsa FILE@ replaces the
IR's annotations with a sidecar's, resolved against the IR, so that a sidecar
can be checked without annotating and re-extracting the PDF. Findings print in
'renderFinding' form, errors first; @check@, @bind@ and @compile@ exit non-zero
on any Error.
-}
module Main (main) where

import           Options.Applicative
import           RIO
import           RIO.Directory            (createDirectoryIfMissing)
import           RIO.FilePath             (takeDirectory, (</>))
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T

import           Hsed.Spec.Anchor         (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Bind           (bindModule)
import           Hsed.Spec.Check          (checkIR, sortFindings)
import           Hsed.Spec.Common         (ErrataSet (..), Finding, RuleName (..), SpecId (..), errataSetName,
                                           isError, parseErrataSet, renderFinding)
import           Hsed.Spec.Compile
import           Hsed.Spec.IR
import           Hsed.Spec.Report
import           Hsed.Spec.Rules          (Classification (..), Section (..), classify, explain,
                                           numberedTables, scopeOf, tablePage)
import           Hsed.Spec.Shape          (captionTitle)
import           Hsed.Spec.Sidecar        (withSidecar)


-- | Where the IR comes from: a file, and optionally a sidecar replacing its annotations.
data Input = Input FilePath (Maybe FilePath)

data Command
  = CmdIrSummary FilePath
  | CmdBind Input FilePath
  | CmdCheck Input ErrataSet
  | CmdCompile Input ErrataSet Text FilePath FilePath
  | CmdExplain Text Int Input
  | CmdOrphans Input
  | CmdLedger [FilePath] (Maybe FilePath)
  | CmdNotImplemented Text [Text]   -- ^ the subcommand and whatever followed it

main :: IO ()
main = execParser hsedc >>= run

hsedc :: ParserInfo Command
hsedc = info (commands <**> helper)
  ( fullDesc
 <> header "hsedc - the hsed spec compiler"
 <> progDesc "Reads the IR v1 JSON written by the PDF extractor (docs/ir-v1.md)." )

defaultIR :: FilePath
defaultIR = "build/core-2.01.hsed.json"

commands :: Parser Command
commands = hsubparser $ mconcat
  [ command "ir-summary" (info (CmdIrSummary <$> strArgument (metavar "FILE" <> help "an IR v1 JSON file"))
      (progDesc "Count the tables, headings, paras and annots in an IR file"))
  , command "bind" (info (CmdBind <$> input <*> outFile)
      (progDesc "Write the bindings module of the IR's annotations (hsed-spec/bindings/Hsed/Bindings/<Spec>.hs)"))
  , command "check" (info (CmdCheck <$> input <*> errataOpt)
      (progDesc "Classify every numbered table and run the checks; non-zero exit on any Error"))
  , command "compile" (info (CmdCompile <$> input <*> errataOpt <*> targetOpt <*> outDir <*> reportDir)
      (progDesc "Check, then write DIR/Hsed/Gen/<Spec>/Types.hs and the reports findings.md, coverage.md, ledger.md"))
  , command "explain" (info (CmdExplain <$> strArgument (metavar "SPEC" <> help "core or opal")
                                     <*> argument auto (metavar "N" <> help "the table number")
                                     <*> input)
      (progDesc "Say how a table is bound: hsedc explain core 46"))
  , command "orphans" (info (CmdOrphans <$> input) (progDesc "List the numbered tables nothing binds"))
  , command "ledger" (info (CmdLedger <$> many (strArgument (metavar "IR...")) <*> hsaOpt)
      (progDesc "Count rule-bound, comment-bound and unbound tables per spec"))
  , command "dump" (info (CmdNotImplemented "dump" <$> many (strArgument (metavar "ARG")))
      (progDesc "Dump the annotations of a PDF as a sidecar (not implemented: tools/extract/dump.py)"))
  ]
  where
    input     = Input <$> strArgument (metavar "IR" <> value defaultIR <> showDefault <> help "an IR v1 JSON file") <*> hsaOpt
    errataOpt = option (maybeReader (parseErrataSet . T.pack))
                  (long "errata" <> metavar "SET" <> value Scott <> showDefaultWith (T.unpack . errataSetName)
                   <> help "scott (the corrections) or published (the text as printed)")
    targetOpt = strOption (long "target" <> metavar "TARGET" <> value "core" <> showDefault <> help "the spec to generate (M1: core)")
    outFile   = strOption (short 'o' <> long "output" <> metavar "FILE" <> help "the module to write")
    outDir    = strOption (short 'o' <> long "output" <> metavar "DIR" <> help "the source directory to generate into")
    reportDir = strOption (long "report" <> metavar "DIR" <> value "build/report" <> showDefault <> help "where the reports go")

hsaOpt :: Parser (Maybe FilePath)
hsaOpt = optional (strOption (long "hsa" <> metavar "FILE" <> help "a sidecar whose annotations replace the IR's"))

run :: Command -> IO ()
run = \case
  CmdIrSummary path -> readIR path >>= either (failWith . fromString) (say stdout . summary)
  CmdBind inp out -> do
    (ir, sideFs) <- load inp
    let (fs, result) = bindModule ir
    report (sideFs <> fs)
    case result of
      Left f     -> report [f] >> exitFailure
      Right text -> do
        createDirectoryIfMissing True (takeDirectory out)
        writeFileUtf8 out text
        say stderr ("hsedc bind: wrote " <> fromString out <> "\n")
        exitOnErrors (sideFs <> fs)
  CmdCheck inp es -> do
    (ir, sideFs) <- load inp
    let fs = sortFindings (sideFs <> checkIR es ir)
    report fs
    say stderr ("hsedc check: " <> display ir.spec <> ", errata " <> display (errataSetName es) <> ": " <> display (findingCounts fs) <> "\n")
    exitOnErrors fs
  CmdCompile inp es target out reportTo -> do
    (ir, sideFs) <- load inp
    unless (target `T.isPrefixOf` ir.spec) $
      failWith ("hsedc compile: --target " <> display target <> " does not match the IR's spec " <> display ir.spec)
    case compileIR es ir of
      Left f -> report [f] >> exitFailure
      Right cr -> do
        let fs   = sortFindings (sideFs <> crFindings cr)
            sid  = SpecId ir.spec
            path = generatedModulePath out sid
        createDirectoryIfMissing True (takeDirectory path)
        writeFileUtf8 path (crModuleText cr)
        createDirectoryIfMissing True reportTo
        writeFileUtf8 (reportTo </> "findings.md") (findingsMarkdown sid es fs)
        writeFileUtf8 (reportTo </> "coverage.md") (coverageMarkdown ir)
        writeFileUtf8 (reportTo </> "ledger.md") (ledgerMarkdown [ledger ir])
        report fs
        say stderr ("hsedc compile: wrote " <> fromString path <> " (" <> display (length (crDecls cr)) <> " declarations, "
                    <> display (length (crExcluded cr)) <> " tables left out) and " <> fromString reportTo
                    <> "/{findings,coverage,ledger}.md; " <> display (findingCounts fs) <> "\n")
        exitOnErrors fs
  CmdExplain specName n inp -> do
    (ir, _) <- load inp
    unless (specName `T.isPrefixOf` ir.spec) $
      failWith ("hsedc explain: the IR is " <> display ir.spec <> ", not " <> display specName)
    let aid = TableId n
    say stdout (display (explain ir aid) <> "\n")
    case (Map.lookup aid (classify ir), Map.lookup aid (numberedTables ir)) of
      (Just (RuleBound r _ _), Just t) -> say stdout ("  " <> where' ir aid t <> ": rule " <> display (ruleNameText r)
                                                      <> ", anchored at " <> display (T.intercalate ", " (map scope (scopeOf ir r))) <> "\n")
      (Just _, Just t) -> say stdout ("  " <> where' ir aid t <> "\n")
      _ -> exitFailure
  CmdOrphans inp -> do
    (ir, _) <- load inp
    let os = orphans ir
    say stdout (foldMap (\(_, f) -> display (renderFinding f) <> "\n") os)
    say stderr ("hsedc orphans: " <> display (length os) <> " of " <> display (Map.size (numberedTables ir))
                <> " numbered tables of " <> display ir.spec <> " are unbound\n")
  CmdLedger paths hsa -> do
    irs <- traverse (\p -> fst <$> load (Input p hsa)) (if null paths then [defaultIR] else paths)
    say stdout (display (renderLedger (map ledger irs)))
  CmdNotImplemented name _ -> failWith ("hsedc " <> display name <> ": not implemented")
  where
    scope WholeDocument = "the whole document"
    scope (Subtree s)   = "section " <> s
    where' :: SpecIR -> AnchorId -> TableIR -> Utf8Builder
    where' ir aid t = display ir.spec <> " " <> display (renderAnchorId aid) <> " "
                      <> display (fromMaybe "" (t.caption >>= captionTitle))
                      <> maybe "" (\p -> ", PDF " <> display p) (tablePage t)

-- | The IR, with a sidecar's annotations in place of its own when one is given.
load :: Input -> IO (SpecIR, [Finding])
load (Input path hsa) = do
  ir <- readIR path >>= either (failWith . fromString) pure
  case hsa of
    Nothing -> pure (ir, [])
    Just f  -> do
      text <- readFileUtf8 f
      let (fs, ir') = withSidecar ir text
      pure (ir', fs)

report :: [Finding] -> IO ()
report = say stdout . foldMap (\f -> display (renderFinding f) <> "\n")

exitOnErrors :: [Finding] -> IO ()
exitOnErrors fs = when (any isError fs) exitFailure

-- | The counts @ir-summary@ prints.
summary :: SpecIR -> Utf8Builder
summary ir = mconcat
  [ "spec:     " <> display ir.spec <> "\n"
  , "tables:   " <> count ir.tables
  , "headings: " <> count ir.headings
  , "paras:    " <> count ir.paras
  , "annots:   " <> count ir.annots
  ]
  where count xs = display (length xs) <> "\n"

-- | Write to a handle; stdout is flushed first, so that a summary on stderr
--   follows the findings on stdout when both go to one terminal or file.
say :: Handle -> Utf8Builder -> IO ()
say h msg = hFlush stdout >> hPutBuilder h (getUtf8Builder msg) >> hFlush h

failWith :: Utf8Builder -> IO b
failWith msg = say stderr (msg <> "\n") >> exitFailure
