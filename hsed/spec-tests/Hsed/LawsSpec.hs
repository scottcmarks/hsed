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
hand-written laws ("Hsed.Law.Sync".@laws@) to each other: by name, both ways;
each entry's law is the law of its name, and states the hash its paragraph or
table has in the IR (the first 12 hex digits the entry carries). Then it runs
each law's property under QuickCheck, over the simulator, and finds each case
a law names in the jig's catalogue; a case a later milestone's jig runs would
pend with that milestone (none since M4a's catalogue took ETC-04 and ETC-15,
design section 11). A law without a property fails, as does a case that
neither the catalogue nor a later milestone has. The states the laws
quantify over ("Hsed.Sim.Gen".@genState@) open at most one session per SP,
the Locking SP Manufactured where one is open to it and in either life
else, the shipping default Manufactured-Inactive among them.
-}
module Hsed.LawsSpec (spec) where

import qualified RIO.List              as L
import qualified RIO.Map               as Map
import qualified RIO.Text              as T
import           RIO
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (counterexample, forAll, property, vectorOf)

import qualified Hsed.Gen.Core.Laws    as Generated
import           Hsed.Jig.Catalogue    (CaseKey, CatalogueEntry (..), catalogue, renderCaseKey)
import           Hsed.Law              (Law (..), LawRef (..))
import qualified Hsed.Law.Sync         as Sync
import           Hsed.OpalUIDs         (uSP_Locking)
import           Hsed.Sim.Gen          (genState)
import           Hsed.Sim.Types        (LifeCycle (..), Session (..), SimParams (..), TPerState (..))


-- | Every hand-written law: those of each module of "Hsed.Law" (M3: Sync's).
handWritten :: [Law]
handWritten = Sync.laws

-- | The cases a law names that a later milestone's jig runs, with that
--   milestone (design section 11): none since M4a's catalogue.
later :: [(CaseKey, String)]
later = []

-- | No name twice.
distinct :: [Text] -> Bool
distinct xs = L.nub xs == xs

-- | The keys of the jig's catalogue.
catalogueKeys :: [CaseKey]
catalogueKeys = map ceKey catalogue

spec :: Spec
spec = do
  describe "what the comments bind to laws (Hsed.Gen.Core.Laws)" $ do
    it "is the hand-written laws, by name, both ways" $ do
      [ lrName r | r <- Generated.laws, lrName r `notElem` map lawName handWritten ] `shouldBe` []
      [ lawName l | l <- handWritten, lawName l `notElem` map lrName Generated.laws ] `shouldBe` []
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

  describe "each law's property, over the simulator" $
    forM_ handWritten $ \l -> case lawProp l of
      Just p  -> it (T.unpack (lawName l) <> " holds") (property p)
      Nothing -> it (T.unpack (lawName l) <> " holds") $
        expectationFailure (T.unpack (lawName l) <> " has no property (Hsed.Law.Sync)")

  describe "each case a law names, in the jig's catalogue" $
    forM_ [ (l, k) | l <- handWritten, k <- lawCases l ] $ \(l, k) ->
      it (T.unpack (renderCaseKey k) <> ", which " <> T.unpack (lawName l) <> " names") $
        if k `elem` catalogueKeys then pure () else case lookup k later of
          Just m  -> pendingWith (T.unpack (renderCaseKey k) <> " is run by the jig of " <> m <> " (design section 11)")
          Nothing -> expectationFailure (T.unpack (renderCaseKey k) <> " is not in the jig's catalogue")

  describe "the states the laws quantify over (Hsed.Sim.Gen.genState)" $ modifyMaxSuccess (const 20) $
    prop "open at most one session per SP, the Locking SP Manufactured where one is open to it and in either life else, the shipping default Manufactured-Inactive among them" $
      forAll (vectorOf 60 genState) $ \sts ->
        let sps st         = map sesSP (Map.elems (tsSessions st))
            locking st     = spLockingOFS (tsParams st)
            onePerSP st    = L.nub (sps st) == sps st
            lockingOpen st = uSP_Locking `elem` sps st
        in counterexample (show [ (sps st, locking st) | st <- sts ])
             (all (\st -> onePerSP st && (not (lockingOpen st) || locking st == Manufactured)) sts
              && any ((== ManufacturedInactive) . locking) sts && any lockingOpen sts)
