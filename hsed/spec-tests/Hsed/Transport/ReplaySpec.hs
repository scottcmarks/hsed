{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.ReplaySpec
Description : Test specifications for Hsed.Transport.Replay
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A captured Level 0 response replayed as a drive: IF-RECV of Level 0
Discovery answered with the capture truncated or zero-padded to the length
asked (Core 3.3.6.2, PDF 67), of 512 and 2048 bytes and of 255 blocks; every
other request (another ComID, 0x1004 among them; another protocol; IF-SEND;
a length the kext could not carry) Unsupported, as the kext's transport
answers; the same answer every time; what it says it is. A file that is no
Level 0 response is refused. And the same on the disk6-like response
"Hsed.Level0.Sedutil" synthesizes from sedutil's printed query
(@spec/fixtures/disk6-query.txt@).

The transport's own exchange is called (its constructor's field, as the
suite may), so that these hold of the transport whatever the host's verbs
in "Hsed.Wire" do.
-}
module Hsed.Transport.ReplaySpec (spec) where

import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import           RIO
import           RIO.FilePath          ((</>))
import qualified RIO.Text              as T
import           Test.Hspec

import           Hsed.Core.Record      (encodeRecord)
import           Hsed.Gen.Core.Layouts (Level0DiscoveryHeader (..))
import           Hsed.Level0.Sedutil   (parseQuery, synthesize)
import           Hsed.Transport.Replay (openReplay)
import           Hsed.Version          (hsedVersion)
import           Hsed.Wire.Internal    (Cap (..), Describe (..), Dir (..), Event (..), IfStatus (..), Request (..),
                                        Response (..), Transport (..), Unit (..))


-- | A response of 600 bytes: Core Table 39's header, its Length of
--   Parameter Data 596 (the bytes after itself), then bytes counting up.
response :: ByteString
response = header <> B.pack (take (600 - B.length header) (L.cycle [1 .. 255]))
  where header = either (const "") id (encodeRecord (Level0DiscoveryHeader 596 (B.replicate 32 0)))

-- | An IF-RECV.
recv :: Word8 -> Word16 -> Word32 -> Request
recv p c n = Request { dir = IfRecv, proto = p, comId = c, lenBytes = n, payload = "" }

-- | Level 0's IF-RECV of a length.
level0 :: Word32 -> Request
level0 = recv 0x01 0x0001

-- | The first bytes of a response, zero-padded: what a TPer answers a length with.
padded :: Word32 -> ByteString -> ByteString
padded n bytes = B.take (fromIntegral n) (bytes <> B.replicate (fromIntegral n) 0)

-- | A replay of bytes written to a temporary file, and the file.
withReplay :: ByteString -> (FilePath -> Transport 'ReadOnly -> IO a) -> IO a
withReplay bytes act = withSystemTempDirectory "hsed-replay" $ \tmp -> do
  let file = tmp </> "level0.bin"
  writeFileBinary file bytes
  openReplay file >>= either (throwString . T.unpack) (act file)

spec :: Spec
spec = describe "Hsed.Transport.Replay" $ do
  describe "a replay of a captured response" $ do
    it "answers Level 0 IF-RECV of 512 bytes with its first 512 (truncated)" $ withReplay response $ \_ t ->
      tExchange t (level0 512) `shouldReturn` Response Good (B.take 512 response)
    it "answers Level 0 IF-RECV of 2048 bytes with it and zeros (padded)" $ withReplay response $ \_ t ->
      tExchange t (level0 2048) `shouldReturn` Response Good (response <> B.replicate (2048 - 600) 0)
    it "answers Level 0 IF-RECV of 255 blocks, the most the kext asks for" $ withReplay response $ \_ t ->
      tExchange t (level0 130560) `shouldReturn` Response Good (padded 130560 response)
    it "refuses IF-RECV on another ComID (0x1004, the drive's base) or protocol, and IF-SEND: Unsupported" $
      withReplay response $ \_ t -> do
        answers <- traverse (tExchange t)
          [ recv 0x01 0x1004 2048, recv 0x02 0x0001 2048, recv 0x02 0x1004 512
          , (level0 2048) { dir = IfSend, payload = B.replicate 2048 0 } ]
        answers `shouldBe` replicate 4 (Response (Unsupported "M3: Level 0 only") "")
    it "refuses a length the kext could not carry: none, part of a block, more than 255 blocks" $
      withReplay response $ \_ t -> do
        answers <- traverse (tExchange t . level0) [0, 100, 1000, 131072]
        [ a | a@(Response s p) <- answers, not (unsupported s) || not (B.null p) ] `shouldBe` []
    it "answers the same request the same, before and after others" $ withReplay response $ \_ t -> do
      first' <- tExchange t (level0 1024)
      _      <- tExchange t (recv 0x01 0x1004 2048)
      _      <- tExchange t (level0 512)
      tExchange t (level0 1024) `shouldReturn` first'
    it "is a drive's replay, in 512-byte blocks, of its file, causing no events" $ withReplay response $ \file t -> do
      tUnit t `shouldBe` UnitBlocks512
      let d = tDescribe t
      (dRole d, dUnits d, dDevice d) `shouldBe` ("drive-replay", UnitBlocks512, Just (T.pack file))
      dImpl d `shouldSatisfy` (hsedVersion `T.isInfixOf`)
      tInject t PowerCycle >>= (`shouldSatisfy` unsupported)
      tClose t

  describe "openReplay" $ do
    it "refuses a file it cannot read" $ withSystemTempDirectory "hsed-replay" $ \tmp ->
      void <$> openReplay (tmp </> "absent.bin") >>= (`shouldSatisfy` isLeft)
    it "refuses a file shorter than a Level 0 header (Core Table 39's 48 bytes), and takes one as long" $
      withSystemTempDirectory "hsed-replay" $ \tmp -> do
        writeFileBinary (tmp </> "short.bin") (B.take 47 response)
        writeFileBinary (tmp </> "header.bin") (B.take 48 response)
        void <$> openReplay (tmp </> "short.bin") >>= (`shouldSatisfy` isLeft)
        void <$> openReplay (tmp </> "header.bin") >>= (`shouldSatisfy` isRight)

  describe "a replay of the disk6-like response synthesized from sedutil's query" $
    it "answers Level 0 IF-RECV of 512 and 2048 bytes, truncated or padded, and refuses ComID 0x1004" $ do
      query <- readFileUtf8 "../spec/fixtures/disk6-query.txt"
      bytes <- either (throwString . T.unpack) (evaluate . synthesize) (parseQuery query)
      withReplay bytes $ \_ t -> do
        answers <- traverse (tExchange t) [level0 512, level0 2048, recv 0x01 0x1004 2048]
        answers `shouldBe`
          [ Response Good (padded 512 bytes), Response Good (padded 2048 bytes)
          , Response (Unsupported "M3: Level 0 only") "" ]

-- | Whether a status is a refusal to carry the command.
unsupported :: IfStatus -> Bool
unsupported = \case
  Unsupported _ -> True
  _             -> False
