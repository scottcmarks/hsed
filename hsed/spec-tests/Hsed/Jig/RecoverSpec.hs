{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{-|
Module      : Hsed.Jig.RecoverSpec
Description : Test specifications for Hsed.Jig.Recover: --recover on the simulator, on it as a drive, on a replay, and on TPers that answer Failure or Pending
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsed-jig --recover@ (design 9.3, 11) as a run makes it ("Hsed.Jig.Run".
'runOpened' with @roRecover@): on the simulator in 512-byte blocks, a
read-write session to the Admin SP held open before it (so that a second
answers SP_BUSY), it issues Level 0, STACK_RESET's HANDLE_COMID_REQUEST
(protocol 2, 512 bytes) and GET_COMID_RESPONSE, the IF-RECV that finds the
ComID idle, then StartSession and its End of Session, each sent and
received, and exits 0, no session left (the ComID no longer Associated) and
its report's @recover.ok@ true. On the simulator as a drive (Support's
fakeDrive), behind the Guard as a run opens one: the drive bound to 0x1004
under the recovery's list, every command it was issued admitted (none
blocked, none outside it), recovered. On a replay: Level 0 alone, the bind
refused, nothing sent, exit 1. On a TPer that answers its
GET_COMID_RESPONSE Success/Failure 1: not recovered, nothing sent after
it, exit 1; on one that answers Pending twice, then Success: asked three
times, recovered, exit 0.

A drive's exit is M4c's rule's ("Hsed.Jig.Run".'Hsed.Jig.Run.runExit'):
the STACK_RESET and the session --recover issues on it are within its
policy, so it exits 0 recovered and 1 not, never 3, which is for a command
its policy does not admit.
-}
module Hsed.Jig.RecoverSpec (spec) where

import qualified Data.Aeson             as A
import qualified RIO.ByteString         as B
import qualified RIO.Map                as Map
import           RIO
import           RIO.Directory          (listDirectory)
import           RIO.FilePath           (takeExtension, (</>))
import qualified RIO.Text               as T
import           Test.Hspec

import           Hsed.Core.Record       (encodeRecord)
import           Hsed.Gen.Core.Codes    (pattern SP_BUSY)
import           Hsed.Gen.Core.Layouts  (STACK_RESETCommandResponse (..), STACK_RESETPending (..))
import           Hsed.Jig.Guard         (recoverMethods)
import           Hsed.Jig.Options       (RunOptions (..), TargetSpec (..), defaultRunOptions)
import           Hsed.Jig.Run           (driveAllowedFor, runOpened, selectedCases)
import           Hsed.Jig.Script        (RunEnv (..), Script, SyncAnswer (..), adminSPAsAnybody, emptyRunCtx, startSession)
import           Hsed.Jig.Support       (at, atPath, disk6Like, fakeDrive, onFakeDrive, silently)
import           Hsed.Jig.Target        (Target (..), closeTarget, openTarget)
import           Hsed.Jig.Transcript    (newTranscript)
import           Hsed.Sim.State         (lifeOf)
import           Hsed.Sim.Types         (ComIdLife (Associated), SimParams (..), TPerState (..), defaultSimParams)
import           Hsed.Transport.Drive   (DriveInfo, RawDrive (..))
import           Hsed.Transport.InProc  (InProcOpts (..), SimControl (..), newInProc)
import           Hsed.Wire              (newChannel)
import           Hsed.Wire.Internal     (Cap (..), Dir (..), IfStatus (..), Request (..), Response (..), Transport (..), Unit (..))


-- | A script on the simulator's transport as it is, outside any run: a
--   transcript, a channel and a context of its own.
onSim :: Transport 'ReadWrite -> SimControl -> Script a -> IO a
onSim tr control s = do
  t <- newTranscript
  channel <- newChannel
  ctx <- newIORef emptyRunCtx
  stop <- newIORef Nothing
  runRIO (RunEnv (SimTarget tr control) t channel ctx defaultRunOptions stop (mkLogFunc (\_ _ _ _ -> pure ()))) s

-- | The ComIDs a session holds in the simulator's state.
associated :: SimControl -> IO [Word16]
associated control = scPeek control <&> \st -> [ c | c <- Map.keys (tsComIds st), lifeOf c st == Just Associated ]

-- | The one JSON report a run wrote into a directory.
reportIn :: FilePath -> IO A.Value
reportIn reports = listDirectory reports >>= \fs -> case [ f | f <- fs, takeExtension f == ".json" ] of
  [f] -> readFileBinary (reports </> f) >>= either throwString pure . A.eitherDecodeStrict'
  _   -> throwString ("not one report in " <> reports <> ": " <> show fs)

-- | A JSON array's elements.
elements :: Maybe A.Value -> [A.Value]
elements = \case
  Just (A.Array xs) -> toList xs
  _                 -> []

-- | The commands a report's transcript issued, each its end, op, protocol,
--   ComID and length.
issued :: A.Value -> [(Maybe A.Value, Maybe A.Value, Maybe A.Value, Maybe A.Value, Maybe A.Value)]
issued json = [ (at "end" e, atPath ["request", "op"] e, atPath ["request", "proto"] e, atPath ["request", "comid"] e, atPath ["request", "len"] e)
              | e <- elements (at "transcript" json), at "entry" e == Just "issued" ]

-- | A command, as 'issued' gives it, on an end.
command :: Text -> Text -> Int -> Int -> Int -> (Maybe A.Value, Maybe A.Value, Maybe A.Value, Maybe A.Value, Maybe A.Value)
command end op p c len = (Just (A.String end), Just (A.String op), num p, num c, num len)
  where num = Just . A.Number . fromIntegral

-- | What --recover issues on an end in 512-byte blocks, in order: Level 0;
--   HANDLE_COMID_REQUEST and GET_COMID_RESPONSE of STACK_RESET on 0x1004
--   (protocol 2, a block each); the idle IF-RECV; StartSession and its
--   IF-RECV; End of Session and its IF-RECV.
recovering :: Text -> [(Maybe A.Value, Maybe A.Value, Maybe A.Value, Maybe A.Value, Maybe A.Value)]
recovering end =
  [ command end "if_recv" 1 0x0001 2048
  , command end "if_send" 2 0x1004 512, command end "if_recv" 2 0x1004 512
  , command end "if_recv" 1 0x1004 2048
  , command end "if_send" 1 0x1004 512, command end "if_recv" 1 0x1004 2048
  , command end "if_send" 1 0x1004 512, command end "if_recv" 1 0x1004 2048 ]

-- | Whether a JSON value is a string holding a text.
holding :: Text -> Maybe A.Value -> Bool
holding t = \case
  Just (A.String s) -> t `T.isInfixOf` s
  _                 -> False

-- | Support's fakeDrive with the GET_COMID_RESPONSEs its simulator would
--   answer replaced, in order, by these (the simulator's own past them),
--   each padded to the length asked; and how many were asked.
resetAnswering :: [ByteString] -> IO ((RawDrive, DriveInfo, SimControl), IORef Int)
resetAnswering answers = do
  (raw, di, control) <- fakeDrive defaultSimParams
  left <- newIORef answers
  asked <- newIORef 0
  let tr = rdTransport raw
      exchange q
        | dir q == IfRecv && proto q == 2 = do
            modifyIORef' asked (+ 1)
            atomicModifyIORef' left (\as -> (drop 1 as, listToMaybe as)) >>= \case
              Just bytes -> pure (Response Good (bytes <> B.replicate (fromIntegral (lenBytes q) - B.length bytes) 0))
              Nothing    -> tExchange tr q
        | otherwise = tExchange tr q
  pure ((raw { rdTransport = tr { tExchange = exchange } }, di, control), asked)

-- | 0x1004's Extended ComID, a static ComID's (Core 3.3.4.7, PDF 62).
extended :: Word32
extended = 0x10040000

-- | Table 36's response, Success/Failure 1 (Core PDF 66), and Table 37's
--   Pending.
failure1, pendingAnswer :: ByteString
failure1 = either (error . show) id (encodeRecord (STACK_RESETCommandResponse extended 1))
pendingAnswer = either (error . show) id (encodeRecord (STACK_RESETPending extended))

spec :: Spec
spec = describe "--recover (Hsed.Jig.Recover through Hsed.Jig.Run.runOpened)" $ do
  it "on the simulator, a session held: STACK_RESET, the ComID idle, a session opened and ended, none left open; exit 0" $
    withSystemTempDirectory "hsed-recover" $ \tmp -> do
      let params = defaultSimParams { spUnits = UnitBlocks512 }
          opts   = defaultRunOptions { roSim = params, roRecover = True, roReportDir = tmp </> "report" }
      (tr, control) <- newInProc params (InProcOpts UnitBlocks512 0)
      held <- onSim tr control (startSession adminSPAsAnybody)
      held `shouldSatisfy` either (const False) (\case Synced _ -> True; _ -> False)
      associated control `shouldReturn` [0x1004]
      onSim tr control (startSession adminSPAsAnybody) `shouldReturn` Right (SyncFailed SP_BUSY)
      t <- newTranscript
      code <- silently (runOpened opts t (SimTarget tr control))
      code `shouldBe` ExitSuccess
      associated control `shouldReturn` []
      json <- reportIn (tmp </> "report")
      atPath ["recover", "ok"] json `shouldBe` Just (A.Bool True)
      atPath ["recover", "text"] json `shouldSatisfy` holding "STACK_RESET on 0x1004: Success; the ComID idle; the Admin SP took a read-write session (TSN "
      issued json `shouldBe` recovering "tper"
      [ m | e <- elements (at "transcript" json), at "entry" e == Just "issued", p <- elements (atPath ["decode", "request", "packets"] e)
          , sp <- elements (at "subpackets" p)
          , m <- maybe [] pure (atPath ["method_call", "method"] sp) <> [ "EndOfSession" | i <- elements (at "items" sp), at "end_of_session" i == Just (A.Bool True) ] ]
        `shouldBe` ["StartSession", "EndOfSession"]
      [ () | e <- elements (at "transcript" json), at "entry" e == Just "note", holding "--recover: STACK_RESET on the base ComID 0x1004" (at "text" e) ]
        `shouldBe` [()]

  it "on the simulator as a drive (Support.fakeDrive): bound to 0x1004 under the recovery's list, every command admitted; exit 0" $ do
    drive <- fakeDrive defaultSimParams
    (code, json) <- onFakeDrive defaultRunOptions { roRecover = True } drive
    at "policies" json `shouldBe` Just (A.toJSON [A.object
      [ "end" A..= ("drive" :: Text), "policy" A..= ("allow_list" :: Text), "base_comid" A..= (0x1004 :: Int), "allowed" A..= recoverMethods ]])
    (at "blocked" json, at "outside_policy" json) `shouldBe` (Just (A.Number 0), Just (A.Array mempty))
    issued json `shouldBe` recovering "drive"
    atPath ["recover", "ok"] json `shouldBe` Just (A.Bool True)
    code `shouldBe` ExitSuccess

  it "on a replay: Level 0 alone, the bind refused, nothing sent; exit 1" $
    withSystemTempDirectory "hsed-recover" $ \tmp -> do
      let file = tmp </> "disk6-like-level0.bin"
          opts = defaultRunOptions { roTarget = TReplay file, roRecover = True, roReportDir = tmp </> "report" }
      disk6Like >>= writeFileBinary file
      chosen <- either (throwString . T.unpack) pure (selectedCases (roTests opts))
      t <- newTranscript
      tg <- openTarget opts (driveAllowedFor opts chosen) t >>= either (throwString . T.unpack) pure
      code <- silently (runOpened opts t tg) `finally` closeTarget tg
      code `shouldBe` ExitFailure 1
      json <- reportIn (tmp </> "report")
      atPath ["recover", "ok"] json `shouldBe` Just (A.Bool False)
      atPath ["recover", "text"] json `shouldSatisfy` holding "not bound: a replay answers Level 0 Discovery alone"
      issued json `shouldBe` [command "drive-replay" "if_recv" 1 0x0001 2048]
      at "policies" json `shouldBe` Just (A.toJSON [A.object ["end" A..= ("drive-replay" :: Text), "policy" A..= ("level0_only" :: Text)]])

  it "on a TPer answering Success/Failure 1: not recovered, nothing sent after the reset; exit 1" $ do
    (drive, asked) <- resetAnswering [failure1]
    (code, json) <- onFakeDrive defaultRunOptions { roRecover = True } drive
    atPath ["recover", "ok"] json `shouldBe` Just (A.Bool False)
    atPath ["recover", "text"] json `shouldSatisfy` holding "STACK_RESET on 0x1004: Success/Failure 1, the stack not reset (Core Table 36, PDF 66)"
    readIORef asked `shouldReturn` 1
    issued json `shouldBe` take 3 (recovering "drive")
    code `shouldBe` ExitFailure 1

  it "on a TPer answering Pending twice, then Success: asked three times, recovered; exit 0" $ do
    (drive, asked) <- resetAnswering [pendingAnswer, pendingAnswer]
    (code, json) <- onFakeDrive defaultRunOptions { roRecover = True } drive
    readIORef asked `shouldReturn` 3
    atPath ["recover", "ok"] json `shouldBe` Just (A.Bool True)
    issued json `shouldBe` take 2 (recovering "drive") <> replicate 2 (command "drive" "if_recv" 2 0x1004 512) <> drop 2 (recovering "drive")
    code `shouldBe` ExitSuccess
