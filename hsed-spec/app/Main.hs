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

The command line of the spec compiler (docs/spec-as-source.md section 4.2).
Only @ir-summary@ works so far; the other subcommands are registered and say so.
-}
module Main (main) where

import           Options.Applicative
import           RIO

import           Hsed.Spec.IR


data Command
  = IrSummary FilePath
  | NotImplemented Text [Text]   -- ^ the subcommand and whatever followed it

main :: IO ()
main = execParser hsedc >>= run

hsedc :: ParserInfo Command
hsedc = info (commands <**> helper)
  ( fullDesc
 <> header "hsedc - the hsed spec compiler"
 <> progDesc "Reads the IR v1 JSON written by the PDF extractor (docs/ir-v1.md)." )

commands :: Parser Command
commands = hsubparser (irSummary <> foldMap stub stubs)
  where
    irSummary = command "ir-summary"
      (info (IrSummary <$> strArgument (metavar "FILE" <> help "an IR v1 JSON file"))
            (progDesc "Count the tables, headings, paras and annots in an IR file"))
    stub (name, desc) = command name
      (info (NotImplemented (fromString name) <$> many (strArgument (metavar "ARG")))
            (progDesc (desc <> " (not implemented)")))

-- | The subcommands of the design, registered ahead of their implementation.
stubs :: [(String, String)]
stubs =
  [ ("bind",    "Generate the bindings from the annotated spec")
  , ("check",   "Check the annotations against the IR")
  , ("compile", "Compile the bound tables to Haskell")
  , ("explain", "Explain how an anchor was bound")
  , ("orphans", "List the tables no annotation binds")
  , ("ledger",  "Write the binding ledger")
  , ("dump",    "Dump the annotations of a PDF as a sidecar")
  ]

run :: Command -> IO ()
run (IrSummary path) = readIR path >>= either (failWith . fromString) (say stdout . summary)
run (NotImplemented name _) = failWith ("hsedc " <> display name <> ": not implemented")

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

say :: Handle -> Utf8Builder -> IO ()
say h = hPutBuilder h . getUtf8Builder

failWith :: Utf8Builder -> IO b
failWith msg = say stderr (msg <> "\n") >> exitFailure
