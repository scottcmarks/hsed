{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.SED.MCTP.Host.Run (run) where

import           RIO

import           Hsed.App
import           System.SED.MCTP.Common.Import ()

run :: RIO App ()
run =
  logInfo "We're inside the SED.MCTP.Host application!"
