{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.SED.Host.Run (run) where

import           RIO

import           Hsed.App
import           System.SED.Common.Import ()

run :: RIO App ()
run = do
  logInfo "We're inside the SED Host application!"
