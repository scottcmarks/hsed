{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hsed.TPer.Run (run) where

import           RIO

import           Hsed.App
import           Hsed.Import ()

run :: RIO App ()
run =
  logInfo "We're inside the hsed TPer application!"
