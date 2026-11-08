{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Transport.Mhtx.ConnLoopbackSpec
Description : Loopback specifications for Hsed.Transport.Mhtx.Conn: a real socket on 127.0.0.1
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

On a socket bindLoopback 0 bound: its name is 127.0.0.1; text, binary and close 4000 with its
reason carried; a 1.4 MB text message passed; a message over the limit closed with the code
the fetched sources predict while other connections are served; invalid UTF-8 ignored (design
11 M5c, group G1's acceptance).

Not yet written: group G1's, one pending example until then (M5c's
scaffold).
-}
module Hsed.Transport.Mhtx.ConnLoopbackSpec (spec) where

import           RIO
import           Test.Hspec


spec :: Spec
spec = describe "Hsed.Transport.Mhtx.Conn" $
  it "carries text, binary and close codes over a socket bound to 127.0.0.1" $
    pendingWith "not yet written: group G1 (Hsed.Transport.Mhtx.ConnLoopbackSpec)"
