{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.LawsSpec
Description : Every unit the law comments bind, held to the hand-written laws: names, hashes, properties and cases
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design example E10 and milestone M3 ("LawsSpec runs every Law"). The spec
compiler lists what the comments bind to hand-written laws
("Hsed.Gen.Core.Laws": the sentences of Core 3.3.10.2.1 item 3a, PDF 82, and
3.3.10.6 item d.c and 3.3.10.7 item a, PDF 86, each under its @law@ comment,
and Table 45, PDF 83, under its @stateTable@ comment), each entry referring to
its law by its qualified name, so that a law a comment names and the code
lacks fails to compile. This test holds the generated list and the
hand-written laws the comments bind ("Hsed.Law.Sync".@laws@) to each other:
by name, both ways; each entry's law is the law of its name, and states the
hash its paragraph or table has in the IR (the first 12 hex digits the entry
carries). The resets' laws ("Hsed.Law.Reset".@laws@, M5a) are bound by no
comment yet, their paragraphs unlabelled prose no anchor names (design 13.29
(j); the plan's D7): each is held to the paragraph its code cites, by name,
page and hash, and its binding pends. Then it runs each law's property under
QuickCheck, over the simulator, the resets' 1000 cases each, and finds each
case a law names in the jig's catalogue; a case a later rung's jig runs
pends with that rung (design section 11). A law without a property fails, as
does a case that neither the catalogue nor a later rung has. The states the laws
quantify over ("Hsed.Sim.Gen".@genState@) open at most one session per SP,
the Locking SP Manufactured where one is open to it and in either life
else, the shipping default Manufactured-Inactive among them, and start from
"Hsed.Sim.Types".@initialState@'s TPer for their parameters (@foldedAt@).
-}
module Hsed.LawsSpec (spec) where

import qualified RIO.List              as L
import qualified RIO.Map               as Map
import qualified RIO.Text              as T
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (conjoin, counterexample, forAll, property, vectorOf)

import           Hsed.Core.Table       (Cell (..), columnOf)
import           Hsed.Core.Types       (enumValue)
import           Hsed.Gen.Core.Enums   (Core_boolean (..))
import qualified Hsed.Gen.Core.Laws    as Generated
import           Hsed.Gen.Core.TableDescs (c_PINTableDesc, lockingTableDesc, mbrControlTableDesc)
import           Hsed.Gen.Core.TableUIDs (uC_PINTable, uLockingTable, uMBRControlTable)
import           Hsed.Gen.Opal.Preconfig.AdminSP (uSP_Locking)
import           Hsed.Jig.Catalogue    (CaseKey (..), CatalogueEntry (..), TestId (..), catalogue, renderCaseKey)
import           Hsed.Law              (Law (..), LawRef (..))
import qualified Hsed.Law.Reset        as Reset
import qualified Hsed.Law.Sync         as Sync
import           Hsed.Sim.Gen          (foldedAt, genState)
import           Hsed.Sim.Reset        (ResetKind (..), resetListed)
import           Hsed.Sim.Types        (LifeCycle (..), SPState (..), Session (..), SimParams (..), TPerState (..), defaultSimParams,
                                        initialComIds, initialState, maxComPacketSizeName)


-- | Every hand-written law: those of each module of "Hsed.Law" (M3: Sync's;
--   M5a: Reset's).
handWritten :: [Law]
handWritten = bound <> unbound

-- | The laws the comments bind (M3: Sync's).
bound :: [Law]
bound = Sync.laws

-- | The laws no comment binds yet, their paragraphs unlabelled prose
--   (design 13.29 (j)): the resets' (M5a).
unbound :: [Law]
unbound = Reset.laws

-- | The paragraph each unbound law cites, by its spec, section, PDF page
--   (1-based) and the first 12 hex digits of its hash in the IR
--   (@spec/ir/*.json@'s @paras@, the sha256 of the paragraph's normalised
--   words), as its comment would bind it.
cited :: [(Text, (Text, Int, Text))]
cited =
  [ ("Reset.lockOnReset",    ("core-2.01 5.7.2.2.10, its first paragraph", 275, "9594d522ccb0"))
  , ("Reset.mbrDoneOnReset", ("core-2.01 5.7.2.5.4, its second paragraph", 278, "7e253a3ed3e0"))
  , ("Reset.properties",     ("core-2.01 5.2.2.4.4", 148, "759d1a8f5208"))
  , ("Reset.tries",          ("core-2.01 5.3.4.1.1.2, its sixth paragraph", 210, "5431c0205778"))
  , ("Reset.comIdIssued",    ("opal-2.30 3.3.5.1 item 1", 31, "6c1ce772d278"))
  ]

-- | The cases a law names that a later rung's jig runs, with that rung
--   (design section 11; the M5 plan's rungs): SPF-12's first case whole at
--   M6, its steps 6 and 7 media I/O (its steps 1 to 5 are the jig's variant
--   SPF-12:1/no-media, in the catalogue since M5b, which the laws name too:
--   Scott's answer Q2); and at M6 what needs the medium, the MBR's shadowing
--   or a power cycle, which M5b's jig issues to the simulator alone, in an
--   extension of SPF-12:1/no-media.
later :: [(CaseKey, String)]
later =
  [ (CaseKey (SPF 12) (Just 1) Nothing Nothing, "M6")
  , (CaseKey (SPF 12) (Just 2) Nothing Nothing, "M6")
  , (CaseKey (UCT 10) Nothing Nothing Nothing, "M6")
  , (CaseKey (UCT 11) Nothing Nothing Nothing, "M6")
  , (CaseKey (SPF 5) Nothing Nothing Nothing, "M6")
  ]

-- | No name twice.
distinct :: [Text] -> Bool
distinct xs = L.nub xs == xs

-- | The keys of the jig's catalogue.
catalogueKeys :: [CaseKey]
catalogueKeys = map ceKey catalogue

-- | A law's property run, or a failure where it has none.
lawHolds :: Law -> Spec
lawHolds l = case lawProp l of
  Just p  -> it (T.unpack (lawName l) <> " holds") (property p)
  Nothing -> it (T.unpack (lawName l) <> " holds") $
    expectationFailure (T.unpack (lawName l) <> " has no property")

spec :: Spec
spec = do
  describe "what the comments bind to laws (Hsed.Gen.Core.Laws)" $ do
    it "is the hand-written laws the comments bind, by name, both ways" $ do
      [ lrName r | r <- Generated.laws, lrName r `notElem` map lawName bound ] `shouldBe` []
      [ lawName l | l <- bound, lawName l `notElem` map lrName Generated.laws ] `shouldBe` []
      map lawName handWritten `shouldSatisfy` distinct
      map lrName Generated.laws `shouldSatisfy` distinct
    it "refers each name to the law of that name" $
      [ (lrName r, lawName (lrLaw r)) | r <- Generated.laws, lawName (lrLaw r) /= lrName r ] `shouldBe` []
    it "gives each law the hash it states: the first 12 hex digits of its paragraph's or table's in the IR" $ do
      [ (lrName r, lrHash r, lawHash (lrLaw r)) | r <- Generated.laws, lrHash r /= lawHash (lrLaw r) ] `shouldBe` []
      [ lrName r | r <- Generated.laws, T.length (lrHash r) /= 12 ] `shouldBe` []
    it "binds Core 3.3.10.2.1#3a (PDF 82), 3.3.10.6#dc and 3.3.10.7#a (PDF 86), and Table 45 (PDF 83)" $
      [ (lrAnchor r, lrPage r) | r <- Generated.laws ] `shouldBe`
        [ ("core-2.01:3.3.10.2.1#3a", 82), ("core-2.01:3.3.10.6#dc", 86), ("core-2.01:3.3.10.7#a", 86), ("core-2.01:T45", 83) ]

  describe "the laws no comment binds yet (Hsed.Law.Reset, design 13.29 (j))" $ do
    it "are the five the M5 plan names, each citing its paragraph: Core 5.7.2.2.10 (PDF 275), 5.7.2.5.4 (PDF 278), 5.2.2.4.4 (PDF 148), 5.3.4.1.1.2 (PDF 210), Opal 3.3.5.1 item 1 (PDF 31)" $
      [ (lawName l, lawHash l) | l <- unbound ] `shouldBe` [ (n, h) | (n, (_, _, h)) <- cited ]
    it "are named in no comment, and none shares a hash with a bound law" $ do
      [ lawName l | l <- unbound, lawName l `elem` map lrName Generated.laws ] `shouldBe` []
      [ lawName l | l <- unbound, lawHash l `elem` map lawHash bound ] `shouldBe` []
      [ lawName l | l <- unbound, T.length (lawHash l) /= 12 ] `shouldBe` []
    forM_ cited $ \(n, (paragraph, page, _)) ->
      it (T.unpack n <> " (" <> T.unpack paragraph <> ", PDF " <> show page <> ") is bound by a comment") $
        pendingWith "the paragraph is unlabelled prose, which no anchor names yet (design 13.29 (j); the M5 plan's D7)"

  describe "each law's property, over the simulator" $ do
    forM_ bound lawHolds
    modifyMaxSuccess (const 1000) $ forM_ unbound lawHolds

  describe "each case a law names, in the jig's catalogue" $
    forM_ [ (l, k) | l <- handWritten, k <- lawCases l ] $ \(l, k) ->
      it (T.unpack (renderCaseKey k) <> ", which " <> T.unpack (lawName l) <> " names") $
        if k `elem` catalogueKeys then pure () else case lookup k later of
          Just m  -> pendingWith (T.unpack (renderCaseKey k) <> " is run by the jig of " <> m <> " (design section 11)")
          Nothing -> expectationFailure (T.unpack (renderCaseKey k) <> " is not in the jig's catalogue")

  describe "the states the laws quantify over (Hsed.Sim.Gen.genState)" $ modifyMaxSuccess (const 20) $ do
    prop "open at most one session per SP, the Locking SP Manufactured where one is open to it and in either life else, the shipping default Manufactured-Inactive among them" $
      forAll (vectorOf 60 genState) $ \sts ->
        let sps st         = map sesSP (Map.elems (tsSessions st))
            locking st     = spLockingOFS (tsParams st)
            onePerSP st    = L.nub (sps st) == sps st
            lockingOpen st = uSP_Locking `elem` sps st
        in counterexample (show [ (sps st, locking st) | st <- sts ])
             (all (\st -> onePerSP st && (not (lockingOpen st) || locking st == Manufactured)) sts
              && any ((== ManufacturedInactive) . locking) sts && any lockingOpen sts)
    prop "vary what the resets read and write: some Locking row lock-enabled and locked, some LockOnReset listing Programmatic (3, Opal Table 16, PDF 32), some MBRControl Done, some C_PIN Tries above 0, at either life" $
      forAll (vectorOf 60 genState) $ \sts ->
        let rows table st   = concat [ Map.findWithDefault [] table (spsTables sps) | sps <- Map.elems (tsSPs st) ]
            holds table p   = any (any p . rows table) sts
            cell desc name r = columnOf desc name >>= (`lookup` r)
            true desc name r = case cell desc name r of
              Just (EnumCell _ v) -> v == enumValue Core_boolean_True
              _                   -> False
        in conjoin
             [ counterexample "a Locking row ReadLockEnabled and ReadLocked" $
                 holds uLockingTable (\r -> true lockingTableDesc "ReadLockEnabled" r && true lockingTableDesc "ReadLocked" r)
             , counterexample "a LockOnReset listing Programmatic" $
                 holds uLockingTable (maybe False (resetListed OnTPerReset) . cell lockingTableDesc "LockOnReset")
             , counterexample "an MBRControl row Done" $ holds uMBRControlTable (true mbrControlTableDesc "Done")
             , counterexample "a C_PIN row's Tries above 0" $
                 holds uC_PINTable (\r -> case cell c_PINTableDesc "Tries" r of
                                           Just (UIntCell n) -> n > 0
                                           _                 -> False)
             , counterexample "both lives" $ length (L.nub (map (spLockingOFS . tsParams) sts)) == 2
             ]
    it "start from initialState's TPer for their parameters, the SPs folded once per OFS (foldedAt; the M5a review), whatever unit, ComIDs, discard and MaxComPacketSize" $
      forM_ [ (u, n, d, roomier, ofs) | u <- [minBound .. maxBound], n <- [1 .. 3], d <- [False, True], roomier <- [False, True]
                                      , ofs <- [ManufacturedInactive, Manufactured] ] $ \(u, n, d, roomier, ofs) -> do
        let p = defaultSimParams
                  { spUnits = u, spNComIds = n, spInactiveComIdDiscard = d, spLockingOFS = ofs
                  , spTPerProperties = [ (name, if roomier && name == maxComPacketSizeName then 2 * v else v)
                                       | (name, v) <- spTPerProperties defaultSimParams ] }
        (foldedAt ofs) { tsParams = p, tsComIds = initialComIds p } `shouldBe` initialState p
