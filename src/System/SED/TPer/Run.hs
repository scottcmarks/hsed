{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.SED.TPer.Run (run) where

import           System.SED.Common.Import

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
