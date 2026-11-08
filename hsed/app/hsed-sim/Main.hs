{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : hsed-sim, the simulator served over the LAN leg (M5c)
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The command line of the simulator's mh_transport hub (design 7 item 5, 11
M5c): the in-process simulator behind "Hsed.Transport.Mhtx.Hub", listening
on 127.0.0.1 alone (Scott's answer Q1). Not yet written: it says so and
exits 1.
-}
module Main (main) where

import           RIO


main :: IO ()
main = do
  hPutBuilder stderr "hsed-sim: not yet written: group G2 (app/hsed-sim/Main.hs: the hub served on 127.0.0.1)\n"
  exitWith (ExitFailure 1)
