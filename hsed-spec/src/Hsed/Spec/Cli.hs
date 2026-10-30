{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Cli
Description : What each hsedc command does, apart from reading its command line
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The commands of the spec compiler (docs/spec-as-source.md sections 4.2 and
5.2), here rather than in @app/Main.hs@ so that the tests can run them. Every
command reads an IR v1 file, by default the annotated extraction
@build/core-2.01.hsed.json@ (@make ir-annotated@); @--hsa FILE@ replaces the
IR's annotations with a sidecar's, resolved against the IR, so that a sidecar
can be checked without annotating and re-extracting the PDF. @ledger@ takes
several IRs and one sidecar: the sidecar replaces the annotations of the IRs
whose spec its lines name, and the others are read as they are.

A spec annotated only in part until its last binding milestone (Opal until
M5) is named by @--partial SPEC@ ('Options', repeatable, on every command that
reads an IR): each of its units nothing binds yet is reported as the Warning
@W-UNBOUND@ rather than the Error @E-UNBOUND@
("Hsed.Spec.Check".'Hsed.Spec.Check.partially', design 13.26 (k)). A spec read
over another (Opal over Core) names the other's IR by @--base IR@ (the
annotated extraction, @build/core-2.01.hsed.json@): @check@ and @compile@,
which run the rules, read it under the run's errata set as the base Context
of the run, read-only ("Hsed.Spec.Check".'Hsed.Spec.Check.mkContextWith',
design 13.26 (g)); the other commands take it too, so that one argument list
serves every command of a spec, and read nothing of it until a classification
resolves through it.

Findings print in 'renderFinding' form on stdout, errors first; a one-line
summary goes to stderr. @compile@ writes the reports of "Hsed.Spec.Report" beside
the modules: @findings.md@, @coverage.md@ (every numbered table, signature
block and anchor), @ERRATA.md@ (the corrections and the findings that report
the PDF) and @ledger.md@. @explain@ takes a table (@46@ or @T46@) or a
signature block (@5.3.3.6/sig@) and prints its Errors (and the sidecar's),
counting the others; @orphans@ and @ledger@ count both. Every command exits
non-zero when any Error was found, the sidecar's included (design 5.2). @bind@ and @compile@ write nothing then: an Error
leaves the committed bindings and generated module as they were.

@compile@ owns the directory of its spec's generated modules
(@DIR/Hsed/Gen/Core@ for Core): after writing the modules it removes every
other @.hs@ file under it, so that a module no rule emits any more, or one
renamed, disappears from the tree and @make gen-diff@ reports it.
-}
module Hsed.Spec.Cli
  ( Input (..)
  , Options (..)
  , noOptions
  , Command (..)
  , defaultIR
  , run
  , runWith
  , load
  ) where

import           RIO
import           RIO.Directory            (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeFile)
import           RIO.FilePath             (normalise, takeDirectory, takeExtension, (</>))
import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import qualified RIO.Text                 as T

import           Hsed.Spec.Anchor         (AnchorId (..), renderAnchorId)
import           Hsed.Spec.Bind           (bindModule)
import           Hsed.Spec.Check          (checkIR, mkContext, partially, sortFindings)
import qualified Hsed.Spec.Common         as Cm
import           Hsed.Spec.Common         (ErrataSet (..), Finding, RuleName (..), SpecId (..), errataSetName,
                                           isError, matchesSpec, renderFinding)
import           Hsed.Spec.Compile
import           Hsed.Spec.Gen            (modulePath, specModuleDir)
import           Hsed.Spec.IR
import           Hsed.Spec.Report
import           Hsed.Spec.RuleDef        (Context, signatureUnits)
import           Hsed.Spec.Rules          (Classification (..), Section (..), classify, classifySignatures, classifyWith,
                                           explain, numberedTables, scopeOf, tablePage)
import           Hsed.Spec.Shape          (captionTitle)
import           Hsed.Spec.Sidecar        (withSidecar, withSidecarOn)


-- | Where the IR comes from: a file, and optionally a sidecar replacing its annotations.
data Input = Input FilePath (Maybe FilePath)

-- | How a run reads its specs, besides the IR: the specs annotated only in
--   part (@--partial SPEC@, each named as @--target@ names one), whose units
--   nothing binds yet are Warnings; and the IR of the spec this one is read
--   over (@--base IR@), whose Context the rules fall back to.
data Options = Options
  { optPartial :: ![Text]
  , optBase    :: !(Maybe FilePath)
  }

-- | A run over a spec annotated in full and read alone (Core since M2c).
noOptions :: Options
noOptions = Options [] Nothing

data Command
  = CmdIrSummary FilePath
  | CmdBind Options Input FilePath
  | CmdCheck Options Input ErrataSet
  | CmdCompile Options Input ErrataSet Text FilePath FilePath
  | CmdExplain Options Text AnchorId Input
  | CmdOrphans Options Input
  | CmdLedger Options [FilePath] (Maybe FilePath)
  | CmdNotImplemented Text [Text]   -- ^ the subcommand and whatever followed it

defaultIR :: FilePath
defaultIR = "build/core-2.01.hsed.json"

run :: Command -> IO ()
run = runWith stdout stderr

-- | 'run' with the findings and answers written to the first handle and the
--   one-line summaries and refusals to the second.
runWith :: Handle -> Handle -> Command -> IO ()
runWith out err = \case
  CmdIrSummary path -> readIR path >>= either (failWith err . fromString) (say out . summary)
  CmdBind o inp file -> do
    (ir, sideFs) <- load err inp
    let (fs, result) = bindModule ir
        found = sortFindings (partially (optPartial o) (sideFs <> fs <> either pure (const []) result))
    report out found
    case result of
      Right text | not (any isError found) -> do
        createDirectoryIfMissing True (takeDirectory file)
        writeFileUtf8 file text
        say err ("hsedc bind: wrote " <> fromString file <> "; " <> display (findingCounts found) <> "\n")
      _ -> failWith err ("hsedc bind: " <> display (findingCounts found) <> "; " <> fromString file <> " not written")
  CmdCheck o inp es -> do
    (ir, sideFs) <- load err inp
    base <- loadBase err o es
    let fs = sortFindings (partially (optPartial o) (sideFs <> checkIR base es ir))
    report out fs
    say err ("hsedc check: " <> display ir.spec <> ", errata " <> display (errataSetName es) <> ": " <> display (findingCounts fs) <> "\n")
    exitOnErrors fs
  CmdCompile o inp es target dir reportTo -> do
    (ir, sideFs) <- load err inp
    unless (matchesSpec target (SpecId ir.spec)) $
      failWith err ("hsedc compile: --target " <> display target <> " does not name the IR's spec " <> display ir.spec)
    base <- loadBase err o es
    case compileIR base es ir of
      Left f -> report out (sideFs <> [f]) >> failWith err "hsedc compile: nothing written"
      Right cr -> do
        let fs    = sortFindings (partially (optPartial o) (sideFs <> crFindings cr))
            sid   = SpecId ir.spec
            paths = [ (modulePath dir name, text) | (name, text) <- crModules cr ]
        report out fs
        when (any isError fs) $
          failWith err ("hsedc compile: " <> display (findingCounts fs) <> "; nothing written")
        forM_ paths $ \(path, text) -> do
          createDirectoryIfMissing True (takeDirectory path)
          writeFileUtf8 path text
        existing <- haskellFiles (specModuleDir dir sid)
        let stale = L.sort (filter (`notElem` map (normalise . fst) paths) (map normalise existing))
        mapM_ removeFile stale
        createDirectoryIfMissing True reportTo
        writeFileUtf8 (reportTo </> "findings.md") (findingsMarkdown sid es fs)
        writeFileUtf8 (reportTo </> "coverage.md") (coverageMarkdown ir)
        writeFileUtf8 (reportTo </> "ERRATA.md") (errataMarkdown (mkContext es ir) fs)
        writeFileUtf8 (reportTo </> "ledger.md") (ledgerMarkdown es [(ledger ir, fs)])
        say err ("hsedc compile: wrote " <> display (T.intercalate ", " (map (T.pack . fst) paths)) <> " ("
                    <> display (length (crDecls cr)) <> " declarations, "
                    <> display (length (crExcluded cr)) <> " units left out)"
                    <> (if null stale then "" else ", removed " <> display (T.intercalate ", " (map T.pack stale)))
                    <> " and " <> fromString reportTo
                    <> "/{findings,coverage,ERRATA,ledger}.md; " <> display (findingCounts fs) <> "\n")
  CmdExplain o specName aid inp -> do
    (ir, sideFs) <- load err inp
    unless (matchesSpec specName (SpecId ir.spec)) $
      failWith err ("hsedc explain: the IR is " <> display ir.spec <> ", not " <> display specName)
    let (sigClasses, sigFs) = classifySignatures ir
        errs = sortFindings (sideFs <> filter isError (partially (optPartial o) (snd (classifyWith ir) <> sigFs)))
        -- the unit's own Errors (and the sidecar's), the rest counted: a partly annotated spec's other units
        -- would bury it (Opal's 53 E-UNBOUND before M3)
        (ours, others) = L.partition (\f -> f `elem` sideFs || Cm.anchor f == Just (renderAnchorId aid)) errs
        how = \case
          RuleBound r _ _ -> ": rule " <> display (ruleNameText r) <> ", anchored at "
                               <> display (T.intercalate ", " (map scope (scopeOf ir r)))
          _               -> ""
    report out ours
    unless (null others) $
      say err ("hsedc explain: " <> display (length others) <> " Errors at other units of " <> display ir.spec <> " (hsedc check lists them)\n")
    say out (display (explain ir aid) <> "\n")
    case aid of
      SigId _ -> case (Map.lookup aid sigClasses, Map.lookup aid (signatureUnits ir)) of
        (Just c, Just s) -> say out ("  " <> sigWhere ir aid s <> how c <> "\n")
        _                -> exitFailure
      _ -> case (Map.lookup aid (classify ir), Map.lookup aid (numberedTables ir)) of
        (Just c, Just t) -> say out ("  " <> where' ir aid t <> how c <> "\n")
        _                -> exitFailure
    exitOnErrors errs
  CmdOrphans o inp -> do
    (ir, sideFs) <- load err inp
    let os  = orphans ir
        (sigOs, tableOs) = L.partition (isSigId . fst) os
        fs  = sortFindings (partially (optPartial o) (sideFs <> map snd os <> otherErrors ir (map snd os)))
        n c = length [ () | (aid, _) <- tableOs, Map.lookup aid (classify ir) & maybe False c ]
    report out fs
    say err ("hsedc orphans: " <> display (length tableOs) <> " of " <> display (Map.size (numberedTables ir))
                <> " numbered tables of " <> display ir.spec <> " are bound by nothing ("
                <> display (n (== Unbound)) <> " unbound, " <> display (n isAmbiguous) <> " ambiguous, "
                <> display (n isInvalid) <> " invalid), and " <> display (length sigOs) <> " of "
                <> display (Map.size (signatureUnits ir)) <> " signature blocks\n")
    exitOnErrors fs
  CmdLedger o paths hsa -> do
    irs  <- traverse (readOrFail err) (if null paths then [defaultIR] else paths)
    side <- traverse readFileUtf8 hsa
    let (sideFs, irs') = maybe ([], irs) (withSidecarOn irs) side
        errs = sortFindings (sideFs <> concat [ filter isError (partially (optPartial o) (snd (classifyWith ir) <> snd (classifySignatures ir)))
                                              | ir <- irs' ])
    report out errs
    say out (display (renderLedger (map ledger irs')))
    exitOnErrors errs
  CmdNotImplemented name _ -> failWith err ("hsedc " <> display name <> ": not implemented")
  where
    scope WholeDocument = "the whole document"
    scope (Subtree s)   = "section " <> s
    where' :: SpecIR -> AnchorId -> TableIR -> Utf8Builder
    where' ir aid t = display ir.spec <> " " <> display (renderAnchorId aid) <> " "
                      <> display (fromMaybe "" (t.caption >>= captionTitle))
                      <> maybe "" (\p -> ", PDF " <> display p) (tablePage t)
    sigWhere :: SpecIR -> AnchorId -> SigIR -> Utf8Builder
    sigWhere ir aid s = display ir.spec <> " " <> display (renderAnchorId aid) <> " "
                        <> display (T.intercalate ", " s.heads) <> ", PDF " <> display (s.page + 1)
    isAmbiguous = \case
      Ambiguous _ -> True
      _           -> False
    isInvalid = \case
      Invalid _ -> True
      _         -> False
    isSigId = \case
      SigId _ -> True
      _       -> False
    -- the classification's Errors that are not about a numbered table (an annotation on a heading, say)
    otherErrors ir shown = [ f | f <- snd (classifyWith ir) <> snd (classifySignatures ir), isError f, f `notElem` shown ]

-- | The IR, with a sidecar's annotations in place of its own when one is given,
--   and what resolving the sidecar found.
load :: Handle -> Input -> IO (SpecIR, [Finding])
load err (Input path hsa) = do
  ir <- readOrFail err path
  case hsa of
    Nothing -> pure (ir, [])
    Just f  -> do
      text <- readFileUtf8 f
      let (fs, ir') = withSidecar ir text
      pure (ir', fs)

-- | The base Context of a run (@--base IR@): the IR read, under the run's
--   errata set; none without one.
loadBase :: Handle -> Options -> ErrataSet -> IO (Maybe Context)
loadBase err o es = traverse (fmap (mkContext es) . readOrFail err) (optBase o)

-- | Every @.hs@ file under a directory, at any depth; none when it does not exist.
haskellFiles :: FilePath -> IO [FilePath]
haskellFiles d = do
  exists <- doesDirectoryExist d
  if not exists then pure [] else do
    entries <- map (d </>) <$> listDirectory d
    concat <$> forM entries (\e -> do
      isDir <- doesDirectoryExist e
      if isDir then haskellFiles e else pure [ e | takeExtension e == ".hs" ])

readOrFail :: Handle -> FilePath -> IO SpecIR
readOrFail err path = readIR path >>= either (failWith err . fromString) pure

report :: Handle -> [Finding] -> IO ()
report h = say h . foldMap (\f -> display (renderFinding f) <> "\n")

exitOnErrors :: [Finding] -> IO ()
exitOnErrors fs = when (any isError fs) exitFailure

-- | The counts @ir-summary@ prints.
summary :: SpecIR -> Utf8Builder
summary ir = mconcat
  [ "spec:     " <> display ir.spec <> "\n"
  , "tables:   " <> count ir.tables
  , "headings: " <> count ir.headings
  , "paras:    " <> count ir.paras
  , "sigs:     " <> count ir.sigs
  , "lot:      " <> count ir.lot
  , "xrefs:    " <> count ir.xrefs
  , "annots:   " <> count ir.annots
  ]
  where count xs = display (length xs) <> "\n"

-- | Write to a handle; stdout is flushed first, so that a summary on stderr
--   follows the findings on stdout when both go to one terminal or file.
say :: Handle -> Utf8Builder -> IO ()
say h msg = hFlush stdout >> hPutBuilder h (getUtf8Builder msg) >> hFlush h

failWith :: Handle -> Utf8Builder -> IO b
failWith h msg = say h (msg <> "\n") >> exitFailure
