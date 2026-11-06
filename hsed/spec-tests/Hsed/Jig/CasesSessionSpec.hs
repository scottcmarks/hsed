{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.CasesSessionSpec
Description : Test specifications for M5b's session cases on the simulator: ETC-20, ETC-21 as printed and ETC-07
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The three Locking SP cases that need nothing of the simulator beyond M5a's
sessions (design 11 M5b), run in process through @hsed-jig run@ in the
four configurations M4a's acceptance runs in (bytes and 512-byte blocks,
a response released at once or held for an IF-RECV): ETC-20 at the default
Original Factory State, Manufactured-Inactive, passes on SyncSession
INVALID_PARAMETER with no session open, and at @--locking-ofs manufactured@
is skipped on its own prerequisite; ETC-21 as printed, Admin1 to the
Locking SP with its PIN's last byte changed, passes at manufactured on
NOT_AUTHORIZED, C_PIN_Admin1's Tries unchanged at TryLimit 0, its
extension observing them, the PIN read from the Locking SP's C_PIN and
masked as the run's one secret; ETC-07 passes at manufactured, step 1's
IF-RECV (0, 0, 0) observed, step 2's session opened and ended by the jig,
no session left; and at the default OFS ETC-21 and ETC-07 are skipped on
LockingManufactured, saying what @--locking-ofs manufactured@ does. Its
helpers are its own.
-}
module Hsed.Jig.CasesSessionSpec (spec) where

import qualified Data.Aeson            as A
import qualified RIO.ByteString        as B
import qualified RIO.List              as L
import qualified RIO.Map               as Map
import           RIO
import           RIO.Directory         (listDirectory)
import           RIO.FilePath          (takeExtension, (</>))
import qualified RIO.Text              as T
import           Test.Hspec

import           Hsed.Jig.Cases.ETC21  (lastByteChanged)
import           Hsed.Jig.Catalogue    (CaseKey, parseCaseKeys)
import           Hsed.Jig.Options      (TargetSpec (..), defaultRunOptions)
import           Hsed.Jig.Run          (RunOptions (..), runJig, runOpened)
import           Hsed.Jig.Support      (at, atPath, silently)
import           Hsed.Jig.Target       (Target (..))
import           Hsed.Jig.Transcript   (newTranscript)
import           Hsed.Sim.Types        (LifeCycle (..), SimParams (..), TPerState (..), defaultSimParams)
import           Hsed.Transport.InProc (InProcOpts (..), SimControl (..))
import           Hsed.Transport.Opened (openedInProc)
import           Hsed.Wire.Internal    (Unit (..))


keys :: Text -> [CaseKey]
keys = either (error . T.unpack) id . parseCaseKeys

-- | The four configurations: bytes and blocks, a response released at once
--   or held for an IF-RECV.
configurations :: [(Unit, Int)]
configurations = [(UnitBytes, 0), (UnitBlocks512, 0), (UnitBytes, 1), (UnitBlocks512, 1)]

configured :: (Unit, Int) -> String
configured (u, hold) = show u <> (if hold > 0 then ", a response held for an IF-RECV" else "")

-- | Cases on the simulator at an Original Factory State, in a
--   configuration.
onSim :: LifeCycle -> (Unit, Int) -> Text -> RunOptions
onSim ofs (u, hold) ks = defaultRunOptions
  { roTarget = TSim
  , roTests  = keys ks
  , roSim    = defaultSimParams { spUnits = u, spLockingOFS = ofs }
  , roHold   = hold
  }

-- | A run into a temporary directory, quietly: its exit code and its JSON
--   report.
jig :: RunOptions -> IO (ExitCode, A.Value)
jig opts = withSystemTempDirectory "hsed-jig" $ \dir -> do
  let reports = dir </> "report"
  code <- silently (runJig opts { roReportDir = reports })
  files <- listDirectory reports
  case [ f | f <- files, takeExtension f == ".json" ] of
    [f] -> (,) code <$> (readFileBinary (reports </> f) >>= either throwString pure . A.eitherDecodeStrict')
    _   -> throwString ("not one JSON report in " <> reports <> ": " <> show files)

elements :: Maybe A.Value -> [A.Value]
elements = \case
  Just (A.Array xs) -> toList xs
  _                 -> []

str :: Text -> Maybe A.Value
str = Just . A.String

-- | Each case's key and verdict.
verdicts :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
verdicts json = [ (at "key" c, at "verdict" c) | c <- elements (at "cases" json) ]

-- | Each case's reasons: a failure's, or a skip's.
reasonsOf :: A.Value -> [(Text, [Text])]
reasonsOf json = [ (k, [ r | A.String r <- elements (at "reasons" c) ] <> [ r | Just (A.String r) <- [at "reason" c] ])
                 | c <- elements (at "cases" json), Just (A.String k) <- [at "key" c] ]

-- | Each extension's verdict and reason.
extensions :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
extensions json = [ (at "verdict" x, at "reason" x) | c <- elements (at "cases" json), x <- elements (at "extensions" c) ]

-- | A case's observations, by its key.
observations :: A.Value -> Text -> [(Text, Text)]
observations json key =
  [ (n, v) | c <- elements (at "cases" json), at "key" c == str key, o <- elements (at "observations" c)
           , Just (A.String n) <- [at "name" o], Just (A.String v) <- [at "value" o] ]

-- | Each case's waivers: its key, and each waiver's prerequisite and reason.
waiversOf :: A.Value -> [(Text, Text, Text)]
waiversOf json = [ (k, w, r) | c <- elements (at "cases" json), Just (A.String k) <- [at "key" c], x <- elements (at "waivers" c)
                             , Just (A.String w) <- [at "prereq" x], Just (A.String r) <- [at "reason" x] ]

-- | The transcript's notes.
notes :: A.Value -> [Text]
notes json = [ t | e <- elements (at "transcript" json), Just (A.String t) <- [at "text" e] ]

-- | The method calls decoded in the transcript's IF-RECV answers: each
--   method's name and status.
answeredCalls :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
answeredCalls json =
  [ (at "method" mc, at "status" mc)
  | e <- elements (at "transcript" json), p <- elements (atPath ["decode", "response", "packets"] e)
  , sp <- elements (at "subpackets" p), Just mc <- [at "method_call" sp] ]

-- | The methods the IF-SENDs of protocol 1 a report's transcript issued
--   carry, in order, a bare End of Session as @EndOfSession@.
sentMethods :: A.Value -> [Text]
sentMethods json =
  [ m | e <- elements (at "transcript" json), at "entry" e == str "issued", atPath ["request", "op"] e == str "if_send"
      , p <- elements (atPath ["decode", "request", "packets"] e), sp <- elements (at "subpackets" p)
      , m <- [ n | Just (A.String n) <- [atPath ["method_call", "method"] sp] ]
             <> [ "EndOfSession" | i <- elements (at "items" sp), at "end_of_session" i == Just (A.Bool True) ] ]

-- | How many IF-SENDs the transcript issued.
ifSends :: A.Value -> Int
ifSends json = length [ () | e <- elements (at "transcript" json), at "entry" e == str "issued", atPath ["request", "op"] e == str "if_send" ]

-- | The run's one secret, the MSID's 32 bytes, as a report prints it masked.
msidSecret :: A.Value
msidSecret = A.toJSON [A.object ["len" A..= (32 :: Int), "sha256" A..= ("a6c4ca3911e4" :: Text), "shown" A..= False]]

-- | Why a case needing the Locking SP Manufactured is skipped at the default
--   OFS.
manufacturedUnmet :: Text
manufacturedUnmet = "prerequisite unmet: LockingManufactured: the simulator's Locking SP is Manufactured-Inactive, its Original Factory State; "
                    <> "--locking-ofs manufactured folds it Manufactured (design 8.3, 9.1; M6's Activate)"

spec :: Spec
spec = do
  describe "ETC-20 (Test Cases PDF 91-92): StartSession on the inactive Locking SP" $ do
    for_ configurations $ \c ->
      it ("at manufactured-inactive, the default OFS, passes in " <> configured c <> ": SyncSession INVALID_PARAMETER, no session open, LockingManufactured waived, its own prerequisite established") $ do
        (code, json) <- jig (onSim ManufacturedInactive c "ETC-20")
        (code, verdicts json) `shouldBe` (ExitSuccess, [(str "ETC-20", str "pass")])
        answeredCalls json `shouldSatisfy` elem (str "SyncSession", Just (A.Number 12))
        observations json "ETC-20" `shouldSatisfy` any (\(n, v) -> n == "step 1" && "SyncSession INVALID_PARAMETER (12), no session open" `T.isInfixOf` v)
        waiversOf json `shouldBe`
          [("ETC-20", "LockingManufactured", "contradicted by the case's own prerequisite 1, the Locking SP Manufactured-Inactive (Test Cases ETC-20, PDF 91)")]
        map (at "prerequisites") (elements (at "cases" json)) `shouldBe` [str "established"]
        sentMethods json `shouldBe` ["StartSession"]
    it "at --locking-ofs manufactured is skipped on its own prerequisite, LockingManufacturedInactive, its common ones established before it: exit 1" $ do
      (code, json) <- jig (onSim Manufactured (UnitBytes, 0) "ETC-20")
      code `shouldBe` ExitFailure 1
      reasonsOf json `shouldBe`
        [("ETC-20", ["prerequisite unmet: LockingManufacturedInactive: the simulator's Locking SP is Manufactured (--locking-ofs manufactured)"])]
      sentMethods json `shouldBe` []

  describe "ETC-21 as printed (Test Cases PDF 92): Admin1 to the Locking SP with a wrong HostChallenge" $ do
    it "varies the PIN within its length, its last byte's lowest bit flipped (Core Table 122's 32 bytes, PDF 118); an empty PIN to one byte" $ do
      let msid = spMsid defaultSimParams
      lastByteChanged msid `shouldSatisfy` \c -> B.length c == B.length msid && c /= msid && B.take 31 c == B.take 31 msid
      lastByteChanged "" `shouldBe` "!"
    for_ configurations $ \c ->
      it ("at manufactured passes in " <> configured c <> ": NOT_AUTHORIZED, C_PIN_Admin1's Tries unchanged at TryLimit 0, its extension observing them, the PIN read from the Locking SP's C_PIN the run's one secret; --tests ETC-21, a whole test's key, selects the Admin SP variant too, which passes") $ do
        (code, json) <- jig (onSim Manufactured c "ETC-21")
        (code, verdicts json) `shouldBe` (ExitSuccess, [(str "ETC-21", str "pass"), (str "ETC-21/adminsp", str "pass")])
        extensions json `shouldBe` [(str "pass", Nothing), (str "pass", Nothing)]
        answeredCalls json `shouldSatisfy` elem (str "SyncSession", Just (A.Number 1))
        observations json "ETC-21" `shouldSatisfy` elem ("prerequisite", "C_PIN_Admin1 TryLimit 0, Tries 0 (Test Cases ETC-21 prerequisite 1, PDF 92)")
        observations json "ETC-21" `shouldSatisfy` any (\(n, v) -> "the Tries rule" `T.isPrefixOf` n && ": Tries" `T.isSuffixOf` n && v == "0 before, 0 after, TryLimit 0")
        observations json "ETC-21/adminsp" `shouldSatisfy` elem ("prerequisite", "C_PIN_SID TryLimit 0, Tries 0 (Test Cases ETC-21 prerequisite 1, PDF 92)")
        notes json `shouldSatisfy` elem ("CredentialsKnown: Admin1's PIN read from the simulator's state, 32 bytes, TryLimit 0, Tries 0 (masked in the reports "
                                          <> "unless --show-msid) (the Locking SP's C_PIN)")
        notes json `shouldSatisfy` elem "CredentialsKnown: SID's PIN read from the simulator's state, 32 bytes, TryLimit 0, Tries 0 (masked in the reports unless --show-msid)"
        at "secrets" json `shouldBe` Just msidSecret
        sentMethods json `shouldBe` replicate 4 "StartSession"

  describe "ETC-07 (Test Cases PDF 80): an EndList before StartSession's Call on the control session" $ do
    for_ configurations $ \c ->
      it ("at manufactured passes in " <> configured c <> ": step 1's IF-RECV (0, 0, 0) observed, step 2's session opened, SyncSession SUCCESS, and ended by the jig, noted") $ do
        (code, json) <- jig (onSim Manufactured c "ETC-07")
        (code, verdicts json) `shouldBe` (ExitSuccess, [(str "ETC-07", str "pass")])
        observations json "ETC-07" `shouldSatisfy` any (\(n, v) -> n == "step 1" && "(0, 0, 0)" `T.isInfixOf` v)
        observations json "ETC-07" `shouldSatisfy` any (\(n, v) -> n == "step 2" && "SyncSession SUCCESS (0)" `T.isInfixOf` v)
        answeredCalls json `shouldSatisfy` elem (str "SyncSession", Just (A.Number 0))
        notes json `shouldSatisfy` elem "ETC-07: step 2's session is ended by the jig's own End of Session: the Test Cases print no CLOSE_SESSION step (PDF 80)"
        ifSends json `shouldBe` 3
        sentMethods json `shouldSatisfy` \ms -> "StartSession" `elem` ms && L.lastMaybe ms == Just "EndOfSession"
    it "leaves no session open: the simulator's state after the run holds none" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let params = defaultSimParams { spLockingOFS = Manufactured }
        (sim, control) <- openedInProc params (InProcOpts UnitBytes 0)
        t <- newTranscript
        code <- silently (runOpened defaultRunOptions { roTests = keys "ETC-07", roSim = params, roReportDir = dir </> "report" } t (SimTarget sim control))
        code `shouldBe` ExitSuccess
        (Map.null . tsSessions <$> scPeek control) `shouldReturn` True

  describe "at manufactured-inactive, the default OFS" $
    it "ETC-21 and ETC-07 are skipped on LockingManufactured, saying what --locking-ofs manufactured does, ETC-21's extension with it; the Admin SP variant, selected with ETC-21, passes: exit 1" $ do
      (code, json) <- jig (onSim ManufacturedInactive (UnitBytes, 0) "ETC-07,ETC-21")
      code `shouldBe` ExitFailure 1
      verdicts json `shouldBe` [(str "ETC-07", str "skipped"), (str "ETC-21", str "skipped"), (str "ETC-21/adminsp", str "pass")]
      reasonsOf json `shouldBe` [("ETC-07", [manufacturedUnmet]), ("ETC-21", [manufacturedUnmet]), ("ETC-21/adminsp", [])]
      extensions json `shouldBe` [(str "skipped", str ("not run, the case skipped: " <> manufacturedUnmet)), (str "pass", Nothing)]
      sentMethods json `shouldBe` ["StartSession", "StartSession"]

  describe "the three together" $
    it "--tests ETC-07,ETC-20,ETC-21 at manufactured: ETC-07, ETC-21 and its Admin SP variant pass, ETC-20 skipped on its own prerequisite: exit 1" $ do
      (code, json) <- jig (onSim Manufactured (UnitBlocks512, 1) "ETC-07,ETC-20,ETC-21")
      code `shouldBe` ExitFailure 1
      verdicts json `shouldBe` [(str "ETC-07", str "pass"), (str "ETC-20", str "skipped"), (str "ETC-21", str "pass"), (str "ETC-21/adminsp", str "pass")]
