{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Transport.Replay
Description : A captured Level 0 response as a 'ReadOnly drive transport
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A drive without the drive: a @'ReadOnly@ transport that answers IF-RECV of
Level 0 Discovery from a captured response (@spec/fixtures/disk6-level0.bin@,
or one "Hsed.Level0.Sedutil" synthesized), truncated or zero-padded to the
transfer length as a TPer pads it (Core 3.3.6.2, PDF 67), and refuses
every other command. The jig's drive-side runs, its Pair runner and the
fixture's tests run on it without root or hardware.

It stands in for the kext's transport ("Hsed.Transport.MacKext") command
for command: the same unit (512-byte blocks), the same one command carried
("Hsed.Transport.Cdb".'level0Blocks': IF-RECV on protocol 1, ComID 1, of 1
to 255 whole blocks), the same refusal of any other ('Unsupported'
@M3: Level 0 only@), so that a run that passes on a replay asks the drive
nothing the kext would refuse. It keeps no state: the same request has the
same answer.
-}
module Hsed.Transport.Replay
  ( openReplay
  ) where

import qualified RIO.ByteString        as B
import           RIO
import qualified RIO.Text              as T

import           Hsed.Core.Layout      (Layout (..))
import           Hsed.Gen.Core.Layouts (level0DiscoveryHeaderLayout)
import           Hsed.Transport.Cdb    (level0Blocks)
import           Hsed.Version          (hsedVersion)
import           Hsed.Wire.Internal    (Cap (..), Describe (..), IfStatus (..), Request (lenBytes),
                                        Response (..), Transport (..), Unit (..))


-- | A replay of a captured response, or why not: a file that cannot be
--   read, or one shorter than a Level 0 header (Core Table 39's layout).
openReplay :: FilePath -> IO (Either Text (Transport 'ReadOnly))
openReplay file = do
  contents <- tryIO (readFileBinary file)
  pure $ case contents of
    Left e -> Left ("replay " <> T.pack file <> ": " <> tshow e)
    Right bytes
      | B.length bytes < header -> Left ("replay " <> T.pack file <> ": " <> tshow (B.length bytes)
                                          <> " bytes, short of a Level 0 header's " <> tshow header)
      | otherwise -> Right (replay file bytes)
  where header = layoutSize level0DiscoveryHeaderLayout

-- | The transport over a captured response.
replay :: FilePath -> ByteString -> Transport 'ReadOnly
replay file bytes = MkTransport
  { tExchange = pure . answer
  , tInject   = \_ -> pure (Unsupported "a replay causes no events")
  , tUnit     = UnitBlocks512
  , tDescribe = Describe
      { dRole      = "drive-replay"
      , dImpl      = "hsed replay " <> hsedVersion
      , dUnits     = UnitBlocks512
      , dSsc       = Nothing
      , dBaseComId = Nothing
      , dNComIds   = Nothing
      , dDevice    = Just (T.pack file)
      }
  , tClose    = pure ()
  }
  where
    answer r = case level0Blocks r of
      Left why -> Response (Unsupported why) mempty
      Right _  -> Response Good (B.take n (bytes <> B.replicate n 0))
        where n = fromIntegral (lenBytes r)
