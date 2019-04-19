{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Options.Applicative.Simple
import           RIO
import           RIO.Process

import qualified Paths_hsed

import           Hsed.App
import           System.SED.TPer.Run

main :: IO ()
main = do
  (opts, ()) <- simpleOptions
    $(simpleVersion Paths_hsed.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (opts ^. verbose)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = makeApp lf pc opts
     in runRIO app run
