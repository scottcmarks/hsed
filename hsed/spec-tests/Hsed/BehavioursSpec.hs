{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.BehavioursSpec
Description : Every heading the behaviour and fails comments bind, held to the hand-written behaviours: names, hashes, methods, Fails items and cases
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 13.16 and milestone M4a. The spec compiler lists what the
comments bind to hand-written behaviours ("Hsed.Gen.Core.Behaviours": the
method headings of Core 5.2.2.1 Properties, PDF 140, 5.2.3.1 StartSession,
PDF 149, 5.2.3.5 CloseSession, PDF 153, and 5.3.3.6 Get, PDF 201, each under
its @behaviour@ comment, and Get's Fails list, 5.3.3.6.3, PDF 201, under its
@fails@ comment), each entry naming its behaviour at its method's generated
record, so that a behaviour a comment names and the code lacks, or one of
another method's record, fails to compile. This test holds the generated list
and the hand-written dispatch lists ("Hsed.Sim.SessionManager".@behaviours@
and "Hsed.Sim.Base".@behaviours@) to each other: by name, both ways; each
entry's behaviour is the behaviour of its name, states the hash its signature
block has in the IR (the first 12 hex digits the entry carries), and runs the
method of a record the generated methods list has; each Fails list's items
match the hand-written list's by label and hash, both ways. Then it runs
each Fails item's property under QuickCheck, over the simulator (pending
while the item has none: the properties are group G1's), and finds each case
a behaviour names in the jig's catalogue.
-}
module Hsed.BehavioursSpec (spec) where

import qualified RIO.List                 as L
import qualified RIO.Text                 as T
import           RIO
import           Test.Hspec
import           Test.QuickCheck          (property)

import           Hsed.Core.Method         (someMethodUID)
import qualified Hsed.Gen.Core.Behaviours as Generated
import qualified Hsed.Gen.Core.Methods    as Methods
import           Hsed.Jig.Catalogue       (CatalogueEntry (..), catalogue, renderCaseKey)
import qualified Hsed.Sim.Base            as Base
import           Hsed.Sim.Behaviour
import qualified Hsed.Sim.SessionManager  as SessionManager


-- | Every hand-written behaviour: the dispatch lists of the Session Manager and the Base Template.
handWritten :: [SomeBehaviour]
handWritten = SessionManager.behaviours <> Base.behaviours

-- | Every hand-written Fails list (M4a: Get's).
handFails :: [SomeFails]
handFails = [forFails Base.getFails]

-- | A Fails list's name and its items' labels and hashes.
failsOf :: SomeFails -> (Text, [(Text, Text)])
failsOf (SomeFails f) = (failsName f, [ (fiLabel i, fiHash i) | i <- failsItems f ])

-- | No name twice.
distinct :: [Text] -> Bool
distinct xs = L.nub xs == xs

spec :: Spec
spec = do
  describe "what the comments bind to behaviours (Hsed.Gen.Core.Behaviours)" $ do
    it "is the hand-written behaviours, by name, both ways" $ do
      [ brName r | r <- Generated.behaviours, brName r `notElem` map someName handWritten ] `shouldBe` []
      [ someName b | b <- handWritten, someName b `notElem` map brName Generated.behaviours ] `shouldBe` []
      map someName handWritten `shouldSatisfy` distinct
      map brName Generated.behaviours `shouldSatisfy` distinct
    it "refers each name to the behaviour of that name" $
      [ (brName r, someName (brBehaviour r)) | r <- Generated.behaviours, someName (brBehaviour r) /= brName r ] `shouldBe` []
    it "gives each behaviour the hash it states: the first 12 hex digits of its signature block's in the IR" $ do
      [ (brName r, brHash r, someHash (brBehaviour r)) | r <- Generated.behaviours, brHash r /= someHash (brBehaviour r) ] `shouldBe` []
      [ brName r | r <- Generated.behaviours, T.length (brHash r) /= 12 ] `shouldBe` []
    it "runs the method of a record the generated methods list has, the hand-written behaviour's own" $ do
      [ brName r | r <- Generated.behaviours, someUID (brBehaviour r) `notElem` map someMethodUID Methods.methods ] `shouldBe` []
      [ brName r | r <- Generated.behaviours, b <- handWritten, someName b == brName r, someUID b /= someUID (brBehaviour r) ] `shouldBe` []
    it "binds Core 5.2.2.1 (PDF 140), 5.2.3.1 (PDF 149), 5.2.3.5 (PDF 153) and 5.3.3.6 (PDF 201), Get's with its Fails list 5.3.3.6.3 (PDF 201)" $ do
      [ (brAnchor r, brPage r) | r <- Generated.behaviours ] `shouldBe`
        [ ("core-2.01:5.2.2.1", 140), ("core-2.01:5.2.3.1", 149), ("core-2.01:5.2.3.5", 153), ("core-2.01:5.3.3.6", 201) ]
      [ (brName r, frAnchor f, frPage f) | r <- Generated.behaviours, Just f <- [brFails r] ] `shouldBe` [("Base.get", "core-2.01:5.3.3.6.3", 201)]

  describe "what the fails comments bind (the Fails lists)" $ do
    it "is the hand-written Fails lists, by name, both ways" $ do
      let generated = [ f | r <- Generated.behaviours, Just f <- [brFails r] ]
      [ frName f | f <- generated, frName f `notElem` map (fst . failsOf) handFails ] `shouldBe` []
      [ fst (failsOf f) | f <- handFails, fst (failsOf f) `notElem` map frName generated ] `shouldBe` []
      map frName generated `shouldSatisfy` distinct
    it "refers each name to the Fails list of that name, whose items match by label and hash, both ways, in order" $
      [ (frName f, frItems f, snd (failsOf (frFails f))) | r <- Generated.behaviours, Just f <- [brFails r]
                                                         , fst (failsOf (frFails f)) /= frName f || frItems f /= snd (failsOf (frFails f)) ]
        `shouldBe` []

  describe "each Fails item's property, over the simulator" $
    forM_ [ (n, i) | sf <- handFails, let (n, _) = failsOf sf, SomeFails f <- [sf], i <- failsItems f ] $ \(n, i) ->
      let title = T.unpack n <> " item " <> T.unpack (fiLabel i) <> " fails with " <> show (fiStatus i)
      in case fiProp i of
           Just p  -> it title (property p)
           Nothing -> it title (pendingWith (T.unpack n <> " item " <> T.unpack (fiLabel i) <> " has no property yet (group G1's, Hsed.Sim.Base)"))

  describe "each case a behaviour names, in the jig's catalogue" $
    forM_ [ (behName b, k) | SomeBehaviour b <- handWritten, k <- behCases b ] $ \(n, k) ->
      it (T.unpack (renderCaseKey k) <> ", which " <> T.unpack n <> " names") $
        k `shouldSatisfy` (`elem` map ceKey catalogue)
