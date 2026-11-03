{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.PairPropertiesSpec
Description : Test specifications for Hsed.Jig.PairProperties
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Pair's Properties comparison (design 13.32 (6), M4c) on hand-written
exchanges: the simulator's answer to UCT-02's Properties (Opal 2.30 Table
17's minima, the host's 4096, 4076 and 4040 echoed) against a drive's
edited from it, each difference judged by Table 17 as generated (PDF
33-34). (a) Equal ends differ in nothing; (b) a TPer value above its
minimum is allowed, (c) one below it not, failing the comparison, and (d)
0, no limit, allowed; (e) the VU DefSessionTimeout takes any value; (f) a
mandatory property the drive lacks is not allowed; (g) one an end alone
reports is listed, failing nothing; (h) a host value answered within
Table 17's minimum and the value sent is allowed, one outside not; (i) a
host property not sent and answered differently is unchecked; (j) an end
without the exchange, or two ends sending different host properties, are
not compared, failing nothing. Then the rules beyond those ten: a
mandatory host property one end leaves out, sent or not; MaxResponseComPacketSize's
(N); a host value sent as 0 or below the initial assumption; a host
property whose row requires nothing of the host (N/A, MaxSessions), which
is unchecked; a TPer property outside the table; every TPer row by the minimum Table 17 prints
for it; and the generated host column holding what the host rule reads of
it. The rules follow the requirements they are given: one rebound, the
judgement follows. Then the JSON and the lines the reports print, and two
simulators run through UCT-02 as a pair, the second standing for a drive
that reports a larger MaxComPacketSize and a property more, the report
carrying the comparison.
-}
module Hsed.Jig.PairPropertiesSpec (spec) where

import qualified Data.Aeson               as A
import           RIO
import           RIO.Directory            (listDirectory)
import           RIO.FilePath             (takeExtension, (</>))
import qualified RIO.Text                 as T
import           Test.Hspec

import           Hsed.Core.Properties     (HostRequirement (..), PropertyRequirement (..), Requirement (..), TPerRequirement (..))
import           Hsed.Gen.Opal.Properties (propertyRequirements)
import           Hsed.Jig.Cases.UCT02     (hostProperties)
import           Hsed.Jig.Catalogue       (parseCaseKeys)
import           Hsed.Jig.Options         (RunOptions (..), TargetSpec (..), defaultRunOptions)
import           Hsed.Jig.Pair            (Judgement (..))
import           Hsed.Jig.PairProperties
import           Hsed.Jig.Run             (runOpened)
import           Hsed.Jig.Script          (PropertiesAnswer (..))
import           Hsed.Jig.Support         (at, silently)
import           Hsed.Jig.Target          (Target (..))
import           Hsed.Jig.Transcript      (newTranscript)
import           Hsed.Sim.Types           (SimParams (..), defaultSimParams)
import           Hsed.Transport.InProc    (InProcOpts (..))
import           Hsed.Transport.Opened    (openedInProc)
import           Hsed.Wire                (Unit (..))


-- * The exchanges

-- | The simulator's answer to UCT-02's Properties (Test Cases PDF 32), as
--   its default parameters give it: Opal 2.30 Table 17's eleven TPer
--   properties at their minima, DefSessionTimeout 0 (PDF 33-34); and the
--   six host properties it supports, the three UCT-02 sends echoed and the
--   others at Table 17's initial assumptions.
simAnswer :: PropertiesAnswer
simAnswer = PropertiesAnswer
  [ ("MaxComPacketSize", 2048), ("MaxResponseComPacketSize", 2048), ("MaxPacketSize", 2028), ("MaxIndTokenSize", 1992)
  , ("MaxPackets", 1), ("MaxSubpackets", 1), ("MaxMethods", 1), ("MaxSessions", 1), ("MaxAuthentications", 2)
  , ("MaxTransactionLimit", 1), ("DefSessionTimeout", 0) ]
  (Just [ ("MaxComPacketSize", 4096), ("MaxPacketSize", 4076), ("MaxIndTokenSize", 4040), ("MaxPackets", 1), ("MaxSubpackets", 1)
        , ("MaxMethods", 1) ])

-- | UCT-02's exchange: the host properties it sends (4096, 4076, 4040), and
--   an answer.
uct02 :: PropertiesAnswer -> Maybe ([(Text, Natural)], PropertiesAnswer)
uct02 answer = Just (hostProperties, answer)

-- | The simulator's exchange against a drive's answer, compared.
against :: PropertiesAnswer -> PropertiesPair
against drive = pairProperties (uct02 simAnswer) (uct02 drive)

-- | A property set to a value (added where the list lacks it), or removed.
setProperty :: Text -> Maybe Integer -> [(Text, Integer)] -> [(Text, Integer)]
setProperty name = \case
  Nothing -> filter ((/= name) . fst)
  Just v  -> \ps -> if any ((== name) . fst) ps then [ (n, if n == name then v else w) | (n, w) <- ps ] else ps <> [(name, v)]

-- | An answer with a TPer property set or removed.
tperSet :: Text -> Maybe Integer -> PropertiesAnswer -> PropertiesAnswer
tperSet name v answer = answer { paTPer = setProperty name v (paTPer answer) }

-- | An answer with a host property set or removed.
hostSet :: Text -> Maybe Integer -> PropertiesAnswer -> PropertiesAnswer
hostSet name v answer = answer { paHost = setProperty name v <$> paHost answer }

-- | A comparison's differences, each by its name, table, values and judgement.
judgements :: PropertiesPair -> [(Text, Side, Maybe Integer, Maybe Integer, Judgement)]
judgements = \case
  Compared ds _ _ -> [ (pdName d, pdSide d, pdSim d, pdDrive d, pdJudged d) | d <- ds ]
  NotCompared _   -> []

-- | The judgement of a comparison of one difference.
oneJudgement :: PropertiesPair -> Maybe Judgement
oneJudgement r = case judgements r of
  [(_, _, _, _, j)] -> Just j
  _                 -> Nothing

-- | A comparison's rules, a difference's each.
rules :: PropertiesPair -> [Text]
rules = \case
  Compared ds _ _ -> map pdRule ds
  NotCompared _   -> []

-- | The generated requirements with one property's row changed.
rebound :: Text -> (PropertyRequirement -> PropertyRequirement) -> [PropertyRequirement]
rebound name f = [ if reqName r == name then f r else r | r <- propertyRequirements ]

-- | A drive's answer edited from the simulator's in several places: a TPer
--   value above its minimum and one below, a property more, a host value
--   lowered within its range, and one not sent answered otherwise.
edited :: PropertiesAnswer
edited = tperSet "MaxComPacketSize" (Just 65536) . tperSet "MaxPacketSize" (Just 2000) . tperSet "MaxReadSessions" (Just 1)
         . hostSet "MaxComPacketSize" (Just 2048) . hostSet "MaxPackets" (Just 2) $ simAnswer


spec :: Spec
spec = do
  describe "pairProperties, the simulator's UCT-02 exchange against a drive's, by Opal 2.30 Table 17 (PDF 33-34)" $ do
    it "(a) finds nothing between equal ends, and fails nothing" $ do
      against simAnswer `shouldBe` Compared [] [] []
      propertiesFail (against simAnswer) `shouldBe` False

    it "(b) allows a drive's TPer MaxComPacketSize of 65536 against the simulator's 2048, at least Table 17's minimum" $ do
      let r = against (tperSet "MaxComPacketSize" (Just 65536) simAnswer)
      r `shouldBe` Compared [PropDiff "MaxComPacketSize" TPerSide (Just 2048) (Just 65536) "TPer minimum 2048 (Opal 2.30 Table 17, PDF 33-34)" Allowed] [] []
      propertiesFail r `shouldBe` False

    it "(c) does not allow a drive's MaxPacketSize of 2000, below Table 17's 2028, and the comparison fails" $ do
      let r = against (tperSet "MaxPacketSize" (Just 2000) simAnswer)
      r `shouldBe` Compared [PropDiff "MaxPacketSize" TPerSide (Just 2028) (Just 2000) "TPer minimum 2028 (Opal 2.30 Table 17, PDF 33-34)" NotAllowed] [] []
      propertiesFail r `shouldBe` True

    it "(d) allows a drive's MaxSessions of 0, no limit (Core Table 167, PDF 142)" $ do
      let r = against (tperSet "MaxSessions" (Just 0) simAnswer)
      r `shouldBe` Compared
        [PropDiff "MaxSessions" TPerSide (Just 1) (Just 0) "TPer minimum 1, 0 no limit (Opal 2.30 Table 17, PDF 33-34; Core Table 167, PDF 141-142)" Allowed]
        [] []
      propertiesFail r `shouldBe` False

    it "(e) allows any DefSessionTimeout, Table 17's VU" $ do
      let r = against (tperSet "DefSessionTimeout" (Just 30000) simAnswer)
      r `shouldBe` Compared [PropDiff "DefSessionTimeout" TPerSide (Just 0) (Just 30000) "TPer VU (Opal 2.30 Table 17, PDF 33-34)" Allowed] [] []
      oneJudgement (against (tperSet "DefSessionTimeout" (Just 1) simAnswer)) `shouldBe` Just Allowed

    it "(f) does not allow a drive that lacks MaxAuthentications, which Table 17 makes mandatory, and lists it as no one-sided property" $ do
      let r = against (tperSet "MaxAuthentications" Nothing simAnswer)
      r `shouldBe` Compared
        [PropDiff "MaxAuthentications" TPerSide (Just 2) Nothing "TPer (M), which every Opal TPer reports (Opal 2.30 Table 17, PDF 33-34)" NotAllowed]
        [] []
      propertiesFail r `shouldBe` True
      judgements (pairProperties (uct02 (tperSet "MaxAuthentications" Nothing simAnswer)) (uct02 simAnswer))
        `shouldBe` [("MaxAuthentications", TPerSide, Nothing, Just 2, NotAllowed)]

    it "(g) lists MaxReadSessions where the drive alone reports it, and where the simulator alone does, failing nothing (Core 5.2.2.2, PDF 141)" $ do
      let more = tperSet "MaxReadSessions" (Just 1) simAnswer
      against more `shouldBe` Compared [] [] ["MaxReadSessions"]
      propertiesFail (against more) `shouldBe` False
      pairProperties (uct02 more) (uct02 simAnswer) `shouldBe` Compared [] ["MaxReadSessions"] []

    it "(h) allows a host MaxComPacketSize answered 2048, within Table 17's 2048 and the 4096 sent, and neither 1024, below, nor 8192, above what was sent" $ do
      [ judgements (against (hostSet "MaxComPacketSize" (Just v) simAnswer)) | v <- [2048, 1024, 8192] ] `shouldBe`
        [ [("MaxComPacketSize", HostSide, Just 4096, Just v, j)] | (v, j) <- [(2048, Allowed), (1024, NotAllowed), (8192, NotAllowed)] ]
      rules (against (hostSet "MaxComPacketSize" (Just 8192) simAnswer)) `shouldBe` ["host within [2048, 4096 sent] (Opal 2.30 Table 17, PDF 33-34)"]
      map (propertiesFail . against . flip (hostSet "MaxComPacketSize") simAnswer . Just) [2048, 1024, 8192] `shouldBe` [False, True, True]

    it "(i) leaves unchecked a host property not sent and answered differently, in Table 17 or not" $ do
      let r = against (hostSet "MaxPackets" (Just 2) simAnswer)
      r `shouldBe` Compared [PropDiff "MaxPackets" HostSide (Just 1) (Just 2) "host not sent, its value what the ComID keeps (Core 5.2.2.3, PDF 143)" Unchecked] [] []
      propertiesFail r `shouldBe` False
      against (hostSet "MaxAggTokenSize" (Just 968) simAnswer)
        `shouldBe` Compared [PropDiff "MaxAggTokenSize" HostSide Nothing (Just 968) "host not in Opal 2.30 Table 17 (PDF 33-34)" Unchecked] [] []

    it "(j) compares nothing, failing nothing, where an end made no Properties exchange with the host's properties" $ do
      let shown = [ pairProperties (uct02 simAnswer) Nothing, pairProperties Nothing (uct02 simAnswer), pairProperties Nothing Nothing ]
      shown `shouldBe`
        [ NotCompared "no Properties with the host's properties (UCT-02 step 1) on the drive, only on the simulator"
        , NotCompared "no Properties with the host's properties (UCT-02 step 1) on the simulator, only on the drive"
        , NotCompared "no Properties with the host's properties (UCT-02 step 1) on either end" ]
      map propertiesFail shown `shouldBe` [False, False, False]

    it "(j) compares nothing where the ends sent different host properties, and the same ones in another order as any" $ do
      let other = Just ([("MaxComPacketSize", 2048)], simAnswer)
      pairProperties (uct02 simAnswer) other `shouldBe` NotCompared
        "the ends sent different host properties: the simulator [MaxComPacketSize = 4096, MaxPacketSize = 4076, MaxIndTokenSize = 4040], the drive [MaxComPacketSize = 2048]"
      propertiesFail (pairProperties (uct02 simAnswer) other) `shouldBe` False
      pairProperties (uct02 simAnswer) (Just (reverse hostProperties, tperSet "MaxPacketSize" (Just 2000) simAnswer))
        `shouldBe` against (tperSet "MaxPacketSize" (Just 2000) simAnswer)

  describe "pairProperties, beyond the plan's ten" $ do
    it "does not allow a host property Table 17 makes mandatory that the drive leaves out of its answer, sent or not (Core 5.2.2.3, PDF 144)" $ do
      let leftOut = "host (M), which an Opal TPer answers whenever the host sends its own (Core 5.2.2.3, PDF 144; Opal 2.30 Table 17, PDF 33-34)"
      against (hostSet "MaxPacketSize" Nothing simAnswer) `shouldBe` Compared [PropDiff "MaxPacketSize" HostSide (Just 4076) Nothing leftOut NotAllowed] [] []
      against (hostSet "MaxMethods" Nothing simAnswer) `shouldBe` Compared [PropDiff "MaxMethods" HostSide (Just 1) Nothing leftOut NotAllowed] [] []
      judgements (against simAnswer { paHost = Nothing })
        `shouldBe` [ (n, HostSide, Just v, Nothing, NotAllowed) | (n, v) <- fromMaybe [] (paHost simAnswer) ]
      propertiesFail (against simAnswer { paHost = Nothing }) `shouldBe` True

    it "allows MaxResponseComPacketSize, Table 17's (N), left out of one end's host answer, and leaves its value unchecked" $ do
      let answered v = hostSet "MaxResponseComPacketSize" (Just v) simAnswer
      against (answered 4096) `shouldBe`
        Compared [PropDiff "MaxResponseComPacketSize" HostSide Nothing (Just 4096) "host (N), which a TPer MAY leave out of its answer (Opal 2.30 Table 17, PDF 33-34)" Allowed] [] []
      judgements (pairProperties (uct02 (answered 2048)) (uct02 (answered 4096)))
        `shouldBe` [("MaxResponseComPacketSize", HostSide, Just 2048, Just 4096, Unchecked)]

    it "reads a host value sent as 0 as no limit: at least Table 17's minimum, or 0 (Core Table 167, PDF 141-142)" $ do
      let sentAs s v = Just ([("MaxComPacketSize", s)], simAnswer { paHost = Just [("MaxComPacketSize", v)] })
          judged0 v = pairProperties (sentAs 0 4096) (sentAs 0 v)
      map (oneJudgement . judged0) [0, 2048, 65536, 1024] `shouldBe` [Just Allowed, Just Allowed, Just Allowed, Just NotAllowed]
      rules (judged0 1024) `shouldBe` ["host at least 2048, or 0, the 0 sent no limit (Opal 2.30 Table 17, PDF 33-34; Core Table 167, PDF 141-142)"]

    it "reads a host value sent below Table 17's initial assumption as that assumption, which the TPer reports in its place (Core 5.2.2.4, PDF 145)" $ do
      let sentAs s v = Just ([("MaxComPacketSize", s)], simAnswer { paHost = Just [("MaxComPacketSize", v)] })
          low v = pairProperties (sentAs 1000 2048) (sentAs 1000 v)
      low 2048 `shouldBe` Compared [] [] []
      map (oneJudgement . low) [1000, 4096] `shouldBe` [Just NotAllowed, Just NotAllowed]
      rules (low 1000) `shouldBe` ["host 2048, the initial assumption, in place of the 1000 sent below it (Core 5.2.2.4, PDF 145; Opal 2.30 Table 17, PDF 33-34)"]

    it "leaves unchecked a host property whose Table 17 row requires nothing of the host (its host column N/A): MaxSessions, the TPer's own, answered by one end or by both" $ do
      let answered v = hostSet "MaxSessions" (Just v) simAnswer
          notHost = "no host requirement in Opal 2.30 Table 17, its host column N/A, not a host property (PDF 33-34)"
      [ (reqName r, isNothing (reqHost r)) | r <- propertyRequirements, reqName r == "MaxSessions" ] `shouldBe` [("MaxSessions", True)]
      against (answered 1) `shouldBe` Compared [PropDiff "MaxSessions" HostSide Nothing (Just 1) notHost Unchecked] [] []
      propertiesFail (against (answered 1)) `shouldBe` False
      pairProperties (uct02 (answered 1)) (uct02 (answered 2))
        `shouldBe` Compared [PropDiff "MaxSessions" HostSide (Just 1) (Just 2) notHost Unchecked] [] []
      propertiesFail (pairProperties (uct02 (answered 1)) (uct02 (answered 2))) `shouldBe` False

    it "leaves unchecked a TPer property outside Table 17 that the ends report differently" $
      pairProperties (uct02 (tperSet "MaxReadSessions" (Just 1) simAnswer)) (uct02 (tperSet "MaxReadSessions" (Just 2) simAnswer))
        `shouldBe` Compared [PropDiff "MaxReadSessions" TPerSide (Just 1) (Just 2) "TPer not in Opal 2.30 Table 17 (PDF 33-34)" Unchecked] [] []

    it "judges each of Table 17's eleven TPer rows by the minimum it prints (PDF 33-34): one more allowed, one less not, unless it is 0, no limit, as 0 is" $ do
      [ (reqName r, reqTPer r >>= tprMinimum) | r <- propertyRequirements ] `shouldBe`
        [ ("MaxComPacketSize", Just 2048), ("MaxResponseComPacketSize", Just 2048), ("MaxPacketSize", Just 2028), ("MaxIndTokenSize", Just 1992)
        , ("MaxPackets", Just 1), ("MaxSubpackets", Just 1), ("MaxMethods", Just 1), ("MaxSessions", Just 1), ("MaxAuthentications", Just 2)
        , ("MaxTransactionLimit", Just 1), ("DefSessionTimeout", Nothing) ]
      [ (n, [ oneJudgement (against (tperSet n (Just v) simAnswer)) | v <- [least + 1, least - 1, 0] ]) | (n, least) <- paTPer simAnswer, least > 0 ]
        `shouldBe`
        [ ("MaxComPacketSize", [Just Allowed, Just NotAllowed, Just Allowed]), ("MaxResponseComPacketSize", [Just Allowed, Just NotAllowed, Just Allowed])
        , ("MaxPacketSize", [Just Allowed, Just NotAllowed, Just Allowed]), ("MaxIndTokenSize", [Just Allowed, Just NotAllowed, Just Allowed])
        , ("MaxPackets", [Just Allowed, Just Allowed, Just Allowed]), ("MaxSubpackets", [Just Allowed, Just Allowed, Just Allowed])
        , ("MaxMethods", [Just Allowed, Just Allowed, Just Allowed]), ("MaxSessions", [Just Allowed, Just Allowed, Just Allowed])
        , ("MaxAuthentications", [Just Allowed, Just NotAllowed, Just Allowed]), ("MaxTransactionLimit", [Just Allowed, Just Allowed, Just Allowed]) ]

    it "reads of Table 17's host column what the generated rows hold (PDF 33-34): each row the TPer accepts with its initial assumption at its minimum and its maximum VU, MaxResponseComPacketSize (N), and every TPer row (M)" $ do
      [ (reqName r, hrRequired h, hrInitial h, hrMinimum h, hrMaximum h) | r <- propertyRequirements, Just h <- [reqHost r] ] `shouldBe`
        [ ("MaxComPacketSize", Mandatory, Just 2048, Just 2048, Nothing), ("MaxResponseComPacketSize", NotRequired, Nothing, Nothing, Nothing)
        , ("MaxPacketSize", Mandatory, Just 2028, Just 2028, Nothing), ("MaxIndTokenSize", Mandatory, Just 1992, Just 1992, Nothing)
        , ("MaxPackets", Mandatory, Just 1, Just 1, Nothing), ("MaxSubpackets", Mandatory, Just 1, Just 1, Nothing)
        , ("MaxMethods", Mandatory, Just 1, Just 1, Nothing) ]
      [ reqName r | r <- propertyRequirements, fmap tprRequired (reqTPer r) /= Just Mandatory ] `shouldBe` []

    it "reads the simulator's own answer as the hand-written one: its default TPer properties" $
      paTPer simAnswer `shouldBe` spTPerProperties defaultSimParams

  describe "pairPropertiesWith, the requirements an argument" $ do
    it "is pairProperties under the generated requirements" $
      pairPropertiesWith propertyRequirements (uct02 simAnswer) (uct02 edited) `shouldBe` against edited

    it "follows a minimum rebound: MaxPacketSize's at 1900, the drive's 2000 is allowed; the host MaxComPacketSize's at 3000, an answer of 2048 is not" $ do
      let tperLeast m r = r { reqTPer = (\t -> t { tprMinimum = Just m }) <$> reqTPer r }
          hostLeast m r = r { reqHost = (\h -> h { hrMinimum = Just m, hrInitial = Just m }) <$> reqHost r }
          drive2000     = uct02 (tperSet "MaxPacketSize" (Just 2000) simAnswer)
      oneJudgement (pairProperties (uct02 simAnswer) drive2000) `shouldBe` Just NotAllowed
      pairPropertiesWith (rebound "MaxPacketSize" (tperLeast 1900)) (uct02 simAnswer) drive2000
        `shouldBe` Compared [PropDiff "MaxPacketSize" TPerSide (Just 2028) (Just 2000) "TPer minimum 1900 (Opal 2.30 Table 17, PDF 33-34)" Allowed] [] []
      oneJudgement (against (hostSet "MaxComPacketSize" (Just 2048) simAnswer)) `shouldBe` Just Allowed
      let raised = pairPropertiesWith (rebound "MaxComPacketSize" (hostLeast 3000)) (uct02 simAnswer) (uct02 (hostSet "MaxComPacketSize" (Just 2048) simAnswer))
      (oneJudgement raised, rules raised) `shouldBe` (Just NotAllowed, ["host within [3000, 4096 sent] (Opal 2.30 Table 17, PDF 33-34)"])

    it "follows a requirement rebound: MaxAuthentications optional, the drive lacking it is listed, failing nothing" $ do
      let madeOptional r = r { reqTPer = (\t -> t { tprRequired = Optional }) <$> reqTPer r }
          r' = pairPropertiesWith (rebound "MaxAuthentications" madeOptional) (uct02 simAnswer) (uct02 (tperSet "MaxAuthentications" Nothing simAnswer))
      r' `shouldBe` Compared [] ["MaxAuthentications"] []
      propertiesFail r' `shouldBe` False

  describe "the JSON" $ do
    it "gives a comparison's differences, each with its table, values, rule and judgement, then the one-sided lists, the reason null" $
      A.toJSON (against edited) `shouldBe` A.object
        [ "compared" A..= True
        , "diffs" A..=
            [ A.object [ "name" A..= ("MaxComPacketSize" :: Text), "side" A..= ("tper" :: Text), "sim" A..= (2048 :: Int), "drive" A..= (65536 :: Int)
                       , "rule" A..= ("TPer minimum 2048 (Opal 2.30 Table 17, PDF 33-34)" :: Text), "judged" A..= ("allowed" :: Text) ]
            , A.object [ "name" A..= ("MaxPacketSize" :: Text), "side" A..= ("tper" :: Text), "sim" A..= (2028 :: Int), "drive" A..= (2000 :: Int)
                       , "rule" A..= ("TPer minimum 2028 (Opal 2.30 Table 17, PDF 33-34)" :: Text), "judged" A..= ("not_allowed" :: Text) ]
            , A.object [ "name" A..= ("MaxComPacketSize" :: Text), "side" A..= ("host" :: Text), "sim" A..= (4096 :: Int), "drive" A..= (2048 :: Int)
                       , "rule" A..= ("host within [2048, 4096 sent] (Opal 2.30 Table 17, PDF 33-34)" :: Text), "judged" A..= ("allowed" :: Text) ]
            , A.object [ "name" A..= ("MaxPackets" :: Text), "side" A..= ("host" :: Text), "sim" A..= (1 :: Int), "drive" A..= (2 :: Int)
                       , "rule" A..= ("host not sent, its value what the ComID keeps (Core 5.2.2.3, PDF 143)" :: Text)
                       , "judged" A..= ("unchecked" :: Text) ] ]
        , "sim_only" A..= ([] :: [Text])
        , "drive_only" A..= (["MaxReadSessions"] :: [Text])
        , "reason" A..= A.Null ]

    it "gives a value an end lacks as null" $
      [ (at "sim" d, at "drive" d) | A.Array ds <- maybeToList (at "diffs" (A.toJSON (against (tperSet "MaxAuthentications" Nothing simAnswer)))), d <- toList ds ]
        `shouldBe` [(Just (A.Number 2), Just A.Null)]

    it "gives a comparison not made with nothing in it but its reason" $
      A.toJSON (pairProperties (uct02 simAnswer) Nothing) `shouldBe` A.object
        [ "compared" A..= False, "diffs" A..= ([] :: [Text]), "sim_only" A..= ([] :: [Text]), "drive_only" A..= ([] :: [Text])
        , "reason" A..= ("no Properties with the host's properties (UCT-02 step 1) on the drive, only on the simulator" :: Text) ]

  describe "propertiesLines" $ do
    it "prints a line a difference, its table, name, values, judgement and rule, then the TPer properties one end alone reports" $
      propertiesLines (against edited) `shouldBe`
        [ "TPer MaxComPacketSize: simulator 2048, drive 65536, allowed: TPer minimum 2048 (Opal 2.30 Table 17, PDF 33-34)"
        , "TPer MaxPacketSize: simulator 2028, drive 2000, not allowed: TPer minimum 2028 (Opal 2.30 Table 17, PDF 33-34)"
        , "host MaxComPacketSize: simulator 4096, drive 2048, allowed: host within [2048, 4096 sent] (Opal 2.30 Table 17, PDF 33-34)"
        , "host MaxPackets: simulator 1, drive 2, unchecked: host not sent, its value what the ComID keeps (Core 5.2.2.3, PDF 143)"
        , "TPer properties the drive alone reports, no failure (Core 5.2.2.2, PDF 141): MaxReadSessions" ]

    it "prints a value an end lacks as none, and the simulator's one-sided list" $ do
      propertiesLines (against (tperSet "MaxAuthentications" Nothing simAnswer)) `shouldBe`
        ["TPer MaxAuthentications: simulator 2, drive none, not allowed: TPer (M), which every Opal TPer reports (Opal 2.30 Table 17, PDF 33-34)"]
      propertiesLines (pairProperties (uct02 (tperSet "MaxReadSessions" (Just 1) . tperSet "MaxSessionTimeout" (Just 0) $ simAnswer)) (uct02 simAnswer))
        `shouldBe` ["TPer properties the simulator alone reports, no failure (Core 5.2.2.2, PDF 141): MaxSessionTimeout, MaxReadSessions"]

    it "prints that nothing differs, and why the ends were not compared" $ do
      propertiesLines (against simAnswer) `shouldBe` ["no differences"]
      propertiesLines (pairProperties Nothing (uct02 simAnswer))
        `shouldBe` ["not compared: no Properties with the host's properties (UCT-02 step 1) on the simulator, only on the drive"]

  describe "a pair of two simulators through UCT-02, the second standing for a drive (Hsed.Jig.Run's wiring)" $
    it "compares the first end's exchange as the simulator's and the second's as the drive's, the report carrying the comparison: exit 0" $
      withSystemTempDirectory "hsed-jig" $ \dir -> do
        (sim, simControl) <- openedInProc defaultSimParams (InProcOpts UnitBytes 0)
        let roomier = defaultSimParams
              { spTPerProperties = setProperty "MaxAggTokenSize" (Just 65480) (setProperty "MaxComPacketSize" (Just 65536) (spTPerProperties defaultSimParams)) }
        (drive, driveControl) <- openedInProc roomier (InProcOpts UnitBytes 0)
        t <- newTranscript
        let reports = dir </> "report"
            opts    = defaultRunOptions { roTarget = TPair TSim TSim, roTests = either (error . T.unpack) id (parseCaseKeys "UCT-02"), roReportDir = reports }
        code <- silently (runOpened opts t (PairTarget (SimTarget sim simControl) (SimTarget drive driveControl)))
        code `shouldBe` ExitSuccess
        json <- listDirectory reports >>= \fs -> case [ f | f <- fs, takeExtension f == ".json" ] of
          [f] -> readFileBinary (reports </> f) >>= either throwString pure . A.eitherDecodeStrict'
          _   -> throwString ("not one report in " <> reports)
        at "pair_properties" json `shouldBe` Just (A.toJSON (Compared
          [PropDiff "MaxComPacketSize" TPerSide (Just 2048) (Just 65536) "TPer minimum 2048 (Opal 2.30 Table 17, PDF 33-34)" Allowed] [] ["MaxAggTokenSize"]))
        fmap length (at "properties" json >>= \case { A.Array es -> Just es; _ -> Nothing }) `shouldBe` Just 2
