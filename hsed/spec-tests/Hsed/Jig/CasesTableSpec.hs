{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.CasesTableSpec
Description : Test specifications for the Locking SP's table and transaction cases of M5b: SPF-02:2, SPF-11, SPF-12:1/no-media, ETC-09, ETC-14
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

M5b's acceptance for the five cases that read and write the Locking SP's
tables on the simulator (design 11 M5b; group G4's), as @hsed-jig run@
makes it with @--locking-ofs manufactured@: the five pass in bytes and in
512-byte blocks, each with and without a response held for an IF-RECV,
every extension passing; SPF-02:2 observes Table 45's insufficient transfer
length, then the Get's 1024 bytes at the MinTransfer (Test Cases PDF 53);
SPF-11 observes the Start Transaction mirrored 0, the Set's SUCCESS,
STACK_RESET's Success/Failure 0 and User1's Enabled FALSE (PDF 65);
SPF-12:1/no-media observes ReadLocked and WriteLocked TRUE TRUE after
TPER_RESET (PDF 66) and says steps 6 and 7 are not run (Scott's answer Q2);
ETC-09 and ETC-14 accept the session abort, the TPer's CloseSession among
its answers, and their step 4 opens a session (PDF 82 and 87). With a
response held for three IF-RECVs, SPF-02:2 passes in both units, asking its
short IF-RECV again while responses are to come (Test Cases 4.1 a, PDF 26).
After a manufactured run of M4a's ten cases and M5b's seven, the simulator's
Locking table, its MBRControl row and its TPerInfo row equal the initial
state's cell for cell by value (a boolean the fold labels @F@ or @FALSE@
and a Set writes @False@: "Hsed.Sim.Cells"), and no session is open: the
restores of SPF-12:1/no-media hold (Scott's answer Q4), and so does its
extension's putting back of the locks its power cycle set on every range
listing Power Cycle. At the default Original Factory State,
Manufactured-Inactive, the five are skipped on LockingManufactured, saying
what @--locking-ofs manufactured@ does, nothing issued for them.

The cases are written in the jig's verbs ("Hsed.Jig.Script"), on the
simulator's Set ("Hsed.Sim.Base") and its one level of transactions
("Hsed.Sim.State", "Hsed.Sim.Session"), M5b's groups G3, G1 and G2 merged
into claude/m5b before this group's (G4). The host's polls are shortened
for the runs here ('quick'), the simulator answering in process.
-}
module Hsed.Jig.CasesTableSpec (spec) where

import qualified Data.Aeson             as A
import qualified RIO.List               as L
import qualified RIO.Map                as Map
import           RIO
import           RIO.Directory          (listDirectory)
import           RIO.FilePath           (takeExtension, (</>))
import qualified RIO.Text               as T
import           Test.Hspec

import           Hsed.Core.Table        (Cell (..), ColNum, Row)
import           Hsed.Gen.Core.TableUIDs (uTPerInfoTable)
import           Hsed.Gen.Core.UIDs     (uSP_Admin)
import           Hsed.Jig.Catalogue     (CaseKey, parseCaseKeys)
import           Hsed.Jig.Options       (RunOptions (..), TargetSpec (..), defaultRunOptions)
import           Hsed.Jig.Run           (runJig, runOpened)
import           Hsed.Jig.Support       (at, atPath, silently)
import           Hsed.Jig.Target        (Target (..))
import           Hsed.Jig.Transcript    (newTranscript)
import           Hsed.Sim.State         (lockingRows, mbrControl, tableRows)
import           Hsed.Sim.Types         (LifeCycle (..), SimParams (..), TPerState (..), defaultSimParams, initialState)
import           Hsed.Transport.InProc  (InProcOpts (..), SimControl (..))
import           Hsed.Transport.Opened  (openedInProc)
import           Hsed.Wire              (ExchangeOpts (..), Unit (..), defaultExchangeOpts)


keys :: Text -> [CaseKey]
keys = either (error . T.unpack) id . parseCaseKeys

-- | The five cases of this group, in the catalogue's order.
five :: [CaseKey]
five = keys fiveList

fiveList :: Text
fiveList = "SPF-02:2,SPF-11,SPF-12:1/no-media,ETC-09,ETC-14"

fiveNames :: [Text]
fiveNames = T.split (== ',') fiveList

-- | M4a's ten cases and M5b's seven, the cases a manufactured run selects
--   (Scott's answer Q1).
oldPlusSeven :: [CaseKey]
oldPlusSeven = keys "UCT-01,UCT-02,UCT-03:1a-1c,SPF-02:1,ETC-02,ETC-03,ETC-04,ETC-08,ETC-15,ETC-21/adminsp,SPF-02:2,SPF-11,SPF-12:1/no-media,ETC-07,ETC-09,ETC-14,ETC-21"

-- | The simulator's parameters with the Locking SP Manufactured
--   (@--locking-ofs manufactured@), in a unit.
manufactured :: Unit -> SimParams
manufactured u = defaultSimParams { spUnits = u, spLockingOFS = Manufactured }

-- | The five on the simulator at manufactured, in a unit, holding a
--   response for so many IF-RECVs; the host's polls shortened, as the
--   simulator in process answers at once and a hold counts IF-RECVs, not
--   time ("Hsed.Jig.Script".@drain@: the run's @roExchange@, the default
--   unless a test shortens it), so that the suite keeps its time budget
--   (design 14: hsed-test within 15 s).
onSim :: Unit -> Int -> RunOptions
onSim u hold = defaultRunOptions { roTarget = TSim, roTests = five, roSim = manufactured u, roHold = hold, roExchange = quick }

quick :: ExchangeOpts
quick = defaultExchangeOpts { xPollMicros = 1000 }

-- | The four configurations the acceptance runs in: bytes and blocks, a
--   response released at once or held for an IF-RECV.
configurations :: [(Unit, Int)]
configurations = [(UnitBytes, 0), (UnitBlocks512, 0), (UnitBytes, 1), (UnitBlocks512, 1)]

configured :: (Unit, Int) -> String
configured (u, hold) = show u <> (if hold > 0 then ", a response held for an IF-RECV" else "")

-- | A run into a temporary directory, quietly: its exit code, its JSON
--   report and its Markdown.
jig :: RunOptions -> IO (ExitCode, A.Value, Text)
jig opts = withSystemTempDirectory "hsed-jig" $ \dir -> do
  code <- silently (runJig opts { roReportDir = dir </> "report" })
  (,,) code <$> reportIn dir <*> markdownIn dir

-- | The one JSON report a run wrote under a directory's @report@.
reportIn :: FilePath -> IO A.Value
reportIn dir = reportFile dir ".json" >>= readFileBinary >>= either throwString pure . A.eitherDecodeStrict'

markdownIn :: FilePath -> IO Text
markdownIn dir = reportFile dir ".md" >>= readFileUtf8

reportFile :: FilePath -> String -> IO FilePath
reportFile dir ext = listDirectory (dir </> "report") >>= \fs -> case [ f | f <- fs, takeExtension f == ext ] of
  [f] -> pure (dir </> "report" </> f)
  _   -> throwString ("not one " <> ext <> " report in " <> dir)

-- | A JSON array's elements.
elements :: Maybe A.Value -> [A.Value]
elements = \case
  Just (A.Array xs) -> toList xs
  _                 -> []

str :: Text -> Maybe A.Value
str = Just . A.String

-- | Each case's key and verdict.
verdicts :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
verdicts json = [ (at "key" c, at "verdict" c) | c <- elements (at "cases" json) ]

-- | Each extension's verdict, by its case's key.
extensionVerdicts :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
extensionVerdicts json = [ (at "key" c, at "verdict" x) | c <- elements (at "cases" json), x <- elements (at "extensions" c) ]

-- | A case's observations, by its key.
observations :: A.Value -> Text -> [(Text, Text)]
observations json key =
  [ (n, v) | c <- elements (at "cases" json), at "key" c == str key, o <- elements (at "observations" c)
           , Just (A.String n) <- [at "name" o], Just (A.String v) <- [at "value" o] ]

-- | Whether a case observed a value under a name that says something.
observed :: A.Value -> Text -> Text -> Text -> Bool
observed json key name says = any (\(n, v) -> n == name && says `T.isInfixOf` v) (observations json key)

-- | Every reason the report gives: each case's, a failure's or a skip's,
--   and each extension's.
reasons :: A.Value -> [Text]
reasons json = concat [ reasonsOf c <> concatMap reasonsOf (elements (at "extensions" c)) | c <- elements (at "cases" json) ]

-- | The cases' own reasons, each case's failure's or skip's.
caseReasons :: A.Value -> [Text]
caseReasons json = concatMap reasonsOf (elements (at "cases" json))

reasonsOf :: A.Value -> [Text]
reasonsOf v = [ r | A.String r <- elements (at "reasons" v) ] <> [ r | Just (A.String r) <- [at "reason" v] ]

-- | The rows of Table 45 the IF-RECVs of the transcript were answered with.
t45s :: A.Value -> [Text]
t45s json = [ row | e <- elements (at "transcript" json), Just (A.String row) <- [atPath ["decode", "response", "t45"] e] ]

-- | The method calls decoded in the transcript's IF-RECV answers: each
--   method's name and status.
answeredCalls :: A.Value -> [(Maybe A.Value, Maybe A.Value)]
answeredCalls json =
  [ (at "method" mc, at "status" mc)
  | e <- elements (at "transcript" json), p <- elements (atPath ["decode", "response", "packets"] e)
  , sp <- elements (at "subpackets" p), Just mc <- [at "method_call" sp] ]

-- | The commands the transcript issued.
issued :: A.Value -> [A.Value]
issued json = [ e | e <- elements (at "transcript" json), at "entry" e == str "issued" ]

-- | Whether two cells are the same by value: an enumeration's value whatever
--   its label (the fold labels a boolean @F@ or @FALSE@, a Set @False@:
--   "Hsed.Sim.Cells"), a list's elements each, any other cell as it is.
sameCell :: Cell -> Cell -> Bool
sameCell a b = case (a, b) of
  (EnumCell _ x, EnumCell _ y) -> x == y
  (ListCell xs, ListCell ys)   -> length xs == length ys && and (zipWith sameCell xs ys)
  _                            -> a == b

-- | Where two tables' rows differ by value, the rows paired in order: the
--   row's number, the column, and the cell of each (none where a row lacks
--   the column, or a table the row).
differences :: [Row] -> [Row] -> [(Int, ColNum, Maybe Cell, Maybe Cell)]
differences was now =
  [ (i, c, x, y)
  | (i, (r1, r2)) <- zip [0 ..] (zipLonger was now)
  , c <- L.nub (map fst r1 <> map fst r2)
  , let x = L.lookup c r1, let y = L.lookup c r2
  , not (fromMaybe False (sameCell <$> x <*> y)) ]
  where
    zipLonger xs ys = zip (xs <> replicate (length ys - length xs) []) (ys <> replicate (length xs - length ys) [])

-- | The Admin SP's TPerInfo rows, its one (Opal 2.30 Table 27, PDF 50).
tperInfoRows :: TPerState -> [Row]
tperInfoRows = tableRows uSP_Admin uTPerInfoTable

spec :: Spec
spec = do
  describe ("--target sim --locking-ofs manufactured --tests " <> T.unpack fiveList) $ do
    for_ configurations $ \c@(u, hold) ->
      it ("passes in " <> configured c <> ", every extension passing: SPF-02:2 observing the insufficient transfer length then 1024 bytes, SPF-11 the mirror 0, SUCCESS, Success/Failure 0 and FALSE, SPF-12:1/no-media TRUE TRUE after TPER_RESET, ETC-09 and ETC-14 the abort and step 4's session") $ do
        (code, json, md) <- jig (onSim u hold)
        verdicts json `shouldBe` [ (str k, str "pass") | k <- fiveNames ]
        code `shouldBe` ExitSuccess
        extensionVerdicts json `shouldBe` [ (str k, str "pass") | k <- ["SPF-02:2", "SPF-11", "SPF-11", "SPF-12:1/no-media", "ETC-09", "ETC-14"] ]
        observed json "SPF-02:2" "step 2" "response ready, insufficient transfer length request" `shouldBe` True
        observed json "SPF-02:2" "step 3" "1024 bytes" `shouldBe` True
        t45s json `shouldSatisfy` elem "insufficient_length"
        observed json "SPF-11" "step 2" "mirrored it with status 0" `shouldBe` True
        observed json "SPF-11" "step 3" "SUCCESS (0)" `shouldBe` True
        observed json "SPF-11" "step 4" "Success/Failure 0" `shouldBe` True
        observed json "SPF-11" "step 6" "FALSE (0)" `shouldBe` True
        observed json "SPF-12:1/no-media" "step 2" "TPER_RESET" `shouldBe` True
        observed json "SPF-12:1/no-media" "step 4" "TRUE TRUE" `shouldBe` True
        observed json "SPF-12:1/no-media" "steps 6-7" "not run: no media I/O on the wire (design 8.5)" `shouldBe` True
        length (observations json "SPF-12:1/no-media") `shouldSatisfy` (>= 7)
        for_ ["ETC-09", "ETC-14"] $ \k -> do
          observed json k "step 3" "the session was aborted" `shouldBe` True
          observed json k "steps 4-5" "opened a session" `shouldBe` True
        answeredCalls json `shouldSatisfy` any ((== str "CloseSession") . fst)
        length [ () | (m, s) <- answeredCalls json, m == str "SyncSession", s == Just (A.Number 0) ] `shouldSatisfy` (>= 12)
        md `shouldSatisfy` T.isInfixOf "- steps 6-7: not run: no media I/O on the wire (design 8.5)"
    it "with a response held for three IF-RECVs, SPF-02:2 passes in both units, its short IF-RECV asked again while responses are to come (Test Cases 4.1 a, PDF 26)" $
      for_ [UnitBytes, UnitBlocks512] $ \u -> do
        (code, json, _) <- jig (onSim u 3) { roTests = keys "SPF-02:2" }
        verdicts json `shouldBe` [(str "SPF-02:2", str "pass")]
        code `shouldBe` ExitSuccess
        t45s json `shouldSatisfy` elem "responses_to_come"
        t45s json `shouldSatisfy` elem "insufficient_length"
        length (filter (== "responses_to_come") (t45s json)) `shouldSatisfy` (>= 3)

  describe "after a manufactured run of M4a's ten cases and M5b's seven" $
    it "the simulator's Locking table, its MBRControl row and its TPerInfo row equal the initial state's cell for cell by value, and no session is open: SPF-12:1/no-media's restores hold (Scott's answer Q4)" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        let params = manufactured UnitBytes
        (sim, control) <- openedInProc params (InProcOpts UnitBytes 0)
        t <- newTranscript
        code <- silently (runOpened defaultRunOptions { roTests = oldPlusSeven, roSim = params, roReportDir = dir </> "report", roExchange = quick } t (SimTarget sim control))
        json <- reportIn dir
        verdicts json `shouldSatisfy` all ((== str "pass") . snd)
        code `shouldBe` ExitSuccess
        st <- scPeek control
        let st0 = initialState params
        Map.keys (tsSessions st) `shouldBe` []
        differences (lockingRows st0) (lockingRows st) `shouldBe` []
        differences (maybeToList (mbrControl st0)) (maybeToList (mbrControl st)) `shouldBe` []
        differences (tperInfoRows st0) (tperInfoRows st) `shouldBe` []
        length (lockingRows st) `shouldBe` length (lockingRows st0)

  describe "at the default Original Factory State, Manufactured-Inactive" $
    it "skips the five on LockingManufactured, each saying so, nothing issued for them, exit 1" $ do
      (code, json, _) <- jig (onSim UnitBytes 0) { roSim = defaultSimParams }
      verdicts json `shouldBe` [ (str k, str "skipped") | k <- fiveNames ]
      caseReasons json `shouldBe` replicate 5 ("prerequisite unmet: LockingManufactured: the simulator's Locking SP is Manufactured-Inactive, its Original Factory State; "
                                               <> "--locking-ofs manufactured folds it Manufactured (design 8.3, 9.1; M6's Activate)")
      reasons json `shouldSatisfy` all ("prerequisite unmet: LockingManufactured: " `T.isInfixOf`)
      issued json `shouldBe` []
      at "if_send_issued" json `shouldBe` Just (A.Number 0)
      code `shouldBe` ExitFailure 1
