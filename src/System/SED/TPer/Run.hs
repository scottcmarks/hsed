{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.SED.TPer.Run (run) where

import           Hsed.App
import           RIO

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
