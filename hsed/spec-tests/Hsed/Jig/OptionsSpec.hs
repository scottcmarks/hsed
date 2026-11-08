{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.OptionsSpec
Description : Test specifications for Hsed.Jig.Options: the ws:// target's accepted and refused forms
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Every accepted and refused form of a ws:// target (design 11 M5c, group G3's acceptance):
@ws:\/\/127.0.0.1:PORT\/PATH@ alone, PORT 1 to 65535 in decimal and PATH a @/@ and then
letters, digits and @._~/-@, rendered back as it was given; @wss:\/\/@, another host
(@localhost@ too), no host, no port, port 0, a port out of range or not in decimal, user
info, a query or a fragment, another path, and a @ws:\/\/@ end in a pair (any @+@ with one,
@sim+ws:\/\/@ among them) refused, each with its reason; M5b's targets read as before. And
hsed-jig reads @--target@ through 'parseTarget' as optparse's reader, a usage error exiting
2, before any command runs: each refusal exits 2 with nothing opened (app/hsed-jig/Main.hs,
read as text, as "Hsed.CapabilitySpec" reads the sources).
-}
module Hsed.Jig.OptionsSpec (spec) where

import           RIO
import           RIO.FilePath     ((</>))
import qualified RIO.Text         as T
import           Test.Hspec

import           Hsed.Jig.Options (TargetSpec (..), WsUrl (..), parseTarget, renderWsUrl)


spec :: Spec
spec = describe "Hsed.Jig.Options" $ do
  describe "parseTarget's ws:// target (design 11 M5c)" $ do
    it "takes ws://127.0.0.1:PORT/PATH alone, PORT 1 to 65535 in decimal, PATH a / and letters, digits and ._~/-" $
      map parseTarget accepted `shouldBe` map (Right . TWs) acceptedUrls
    it "renders each URL it takes as it was given (renderWsUrl, the reports' options.target)" $
      map renderWsUrl acceptedUrls `shouldBe` accepted
    it "refuses wss://, another host, no host, no port, port 0, a port out of range or not in decimal, user info, a query or a fragment, and another path, each with its reason" $
      for_ refusals (uncurry refusedFor)
    it "refuses a ws:// end in a pair: sim+ws://, and any + with a ws:// end" $
      for_ pairs (`refusedFor` "a pair with a ws:// end")
    it "reads M5b's targets as before: sim, /dev/diskN, replay:FILE, sim+/dev/diskN, sim+replay:FILE" $ do
      map parseTarget ["sim", "/dev/disk99", "replay:x.bin", "sim+/dev/disk99", "sim+replay:x.bin"] `shouldBe`
        map Right [TSim, TDevice "/dev/disk99", TReplay "x.bin", TPair TSim (TDevice "/dev/disk99"), TPair TSim (TReplay "x.bin")]
      "nonsense" `refusedFor` "not sim, /dev/diskN, replay:FILE, sim+/dev/diskN, sim+replay:FILE or ws://127.0.0.1:PORT/PATH"

  describe "hsed-jig's --target (app/hsed-jig/Main.hs)" $
    it "is read through parseTarget as optparse's reader, a usage error exiting 2 before any command runs: a refused target exits 2, nothing opened" $ do
      main <- decodeUtf8Lenient <$> readFileBinary ("app" </> "hsed-jig" </> "Main.hs")
      main `shouldSatisfy` T.isInfixOf "main = execParser hsedJig >>= runCommand >>= exitWith"
      main `shouldSatisfy` T.isInfixOf "option (eitherReader (first T.unpack . parseTarget . T.pack))"
      main `shouldSatisfy` T.isInfixOf "(long \"target\""
      -- the parser's own failure code and each subcommand's (run's among them)
      length (filter (T.isInfixOf "failureCode 2") (T.lines main)) `shouldBe` 2

-- | The ws:// targets taken, and their URLs.
accepted :: [Text]
accepted = ["ws://127.0.0.1:41234/ws", "ws://127.0.0.1:1/", "ws://127.0.0.1:65535/a.b_c~d/e-F9"]

acceptedUrls :: [WsUrl]
acceptedUrls = [WsUrl 41234 "/ws", WsUrl 1 "/", WsUrl 65535 "/a.b_c~d/e-F9"]

-- | ws:// targets refused, each with what its reason says.
refusals :: [(Text, Text)]
refusals =
  [ ("wss://127.0.0.1:41234/ws",               "wss:// is TLS, M6's")
  , ("ws://localhost:41234/ws",                "host localhost: 127.0.0.1 alone")
  , ("ws://192.0.2.1:41234/ws",                "host 192.0.2.1: 127.0.0.1 alone (the cross-host run is M6's, D4")
  , ("ws://127.0.0.2:41234/ws",                "host 127.0.0.2: 127.0.0.1 alone")
  , ("ws://[::1]:41234/ws",                    "host [::1]: 127.0.0.1 alone")
  , ("ws://:41234/ws",                         "no host")
  , ("ws://127.0.0.1/ws",                      "no port")
  , ("ws://127.0.0.1:/ws",                     "no port")
  , ("ws://127.0.0.1:0/ws",                    "port 0: an ephemeral port is the hub's to choose")
  , ("ws://127.0.0.1:65536/ws",                "port 65536: 1 to 65535")
  , ("ws://127.0.0.1:99999999999999999999/ws", "port 99999999999999999999: 1 to 65535")
  , ("ws://127.0.0.1:041234/ws",               "port 041234: 1 to 65535 in decimal")
  , ("ws://127.0.0.1:0x50/ws",                 "port 0x50: 1 to 65535 in decimal")
  , ("ws://127.0.0.1:-80/ws",                  "port -80: 1 to 65535 in decimal")
  , ("ws://user@127.0.0.1:41234/ws",           "user info")
  , ("ws://user:secret@127.0.0.1:41234/ws",    "user info")
  , ("ws://127.0.0.1:41234/ws?token=abc",      "a query or a fragment")
  , ("ws://127.0.0.1:41234/ws#part",           "a query or a fragment")
  , ("ws://127.0.0.1:41234",                   "no path")
  , ("ws://127.0.0.1:41234/w s",               "path /w s: a / and then letters, digits and ._~/- alone")
  , ("ws://127.0.0.1:41234/ws%20",             "path /ws%20")
  ]

-- | Pairs with a ws:// end.
pairs :: [Text]
pairs =
  [ "sim+ws://127.0.0.1:41234/ws"
  , "sim+wss://127.0.0.1:41234/ws"
  , "ws://127.0.0.1:41234/ws+sim"
  , "replay:x.bin+ws://127.0.0.1:41234/ws"
  , "ws://127.0.0.1:+80/ws"
  ]

-- | A target refused, its reason the target's and saying what is given.
refusedFor :: Text -> Text -> Expectation
refusedFor target reason = case parseTarget target of
  Left why -> do
    why `shouldSatisfy` T.isPrefixOf ("--target " <> target <> ": ")
    why `shouldSatisfy` T.isInfixOf reason
  Right taken -> expectationFailure (T.unpack target <> ": taken as " <> show taken)
