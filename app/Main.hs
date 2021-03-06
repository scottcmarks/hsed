{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Options.Applicative.Simple
import           RIO
import           RIO.Process

import           GitHash.Extras.GitVersion          (gitVersion)
import           Hsed.App
import qualified Paths_hsed
import           System.SED.MCTP.Host.Run        as Host ()
import           System.SED.MCTP.TPer.Run        as TPer (run)

main :: IO ()
main = do
    (opts, ()) <- simpleOptions
        $(gitVersion Paths_hsed.version)
        "Header for command line arguments"
        "Program description, also for command line arguments"
        ( Options
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
      in runRIO app TPer.run
