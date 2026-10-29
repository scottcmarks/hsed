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
5.2); what each command does is "Hsed.Spec.Cli". Every command reads an IR v1
file, by default the annotated extraction @build/core-2.01.hsed.json@ (@make
ir-annotated@); @--hsa FILE@ replaces the IR's annotations with a sidecar's.
Every command exits non-zero on any Error, and @bind@ and @compile@ then write
nothing.
-}
module Main (main) where

import           Options.Applicative
import           RIO
import qualified RIO.Text                 as T

import           Hsed.Spec.Cli
import           Hsed.Spec.Common         (ErrataSet (..), errataSetName, parseErrataSet)


main :: IO ()
main = execParser hsedc >>= run

hsedc :: ParserInfo Command
hsedc = info (commands <**> helper)
  ( fullDesc
 <> header "hsedc - the hsed spec compiler"
 <> progDesc "Reads the IR v1 JSON written by the PDF extractor (docs/ir-v1.md)." )

commands :: Parser Command
commands = hsubparser $ mconcat
  [ command "ir-summary" (info (CmdIrSummary <$> strArgument (metavar "FILE" <> help "an IR v1 JSON file"))
      (progDesc "Count the tables, headings, paras and annots in an IR file"))
  , command "bind" (info (CmdBind <$> input <*> outFile)
      (progDesc "Write the bindings module of the IR's annotations (hsed-spec/bindings/Hsed/Bindings/<Spec>.hs)"))
  , command "check" (info (CmdCheck <$> input <*> errataOpt)
      (progDesc "Classify every numbered table and run the checks; non-zero exit on any Error"))
  , command "compile" (info (CmdCompile <$> input <*> errataOpt <*> targetOpt <*> outDir <*> reportDir)
      (progDesc "Check, then write the generated modules DIR/Hsed/Gen/<Spec>/*.hs and the reports findings.md, coverage.md, ledger.md"))
  , command "explain" (info (CmdExplain <$> strArgument (metavar "SPEC" <> help "core or opal")
                                     <*> argument auto (metavar "N" <> help "the table number")
                                     <*> input)
      (progDesc "Say how a table is bound: hsedc explain core 46"))
  , command "orphans" (info (CmdOrphans <$> input) (progDesc "List the numbered tables nothing binds: unbound, ambiguous or with an annotation that does not bind"))
  , command "ledger" (info (CmdLedger <$> many (strArgument (metavar "IR...")) <*> hsaOpt)
      (progDesc "Count rule-bound, comment-bound and unbound tables per spec; --hsa applies to the IRs whose spec its lines name"))
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
