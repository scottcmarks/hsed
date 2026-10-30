{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Jig.CatalogueSpec
Description : Test specifications for Hsed.Jig.Catalogue and Hsed.Jig.Prereq
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The case keys @--tests@ takes and prints; the catalogue's four cases of M3
with their titles and pages (Test Cases 1.02: UCT-01 PDF 29, SPF-02 case 1
PDF 52, ETC-02 PDF 77, ETC-03 PDF 78), each with the case the runner runs
and the keys the laws name (ETC-02, SPF-02:1); the common prerequisites of
sections 5.2 (PDF 29), 6.2 (PDF 49) and 7.2 (PDF 76), and the section each
family's tests are in.
-}
module Hsed.Jig.CatalogueSpec (spec) where

import           RIO
import           Test.Hspec

import           Hsed.Jig.Case      (Case (..))
import           Hsed.Jig.Catalogue
import           Hsed.Jig.Prereq
import           Hsed.Jig.Run       (cases)


key :: TestId -> Maybe Int -> CaseKey
key t c = CaseKey t c Nothing Nothing

spec :: Spec
spec = do
  describe "parseCaseKeys and renderCaseKey" $ do
    it "read M3's four keys" $
      parseCaseKeys "UCT-01,ETC-02,ETC-03,SPF-02:1"
        `shouldBe` Right [key (UCT 1) Nothing, key (ETC 2) Nothing, key (ETC 3) Nothing, key (SPF 2) (Just 1)]
    it "round trip a step range and a variant" $ do
      fmap (map renderCaseKey) (parseCaseKeys "UCT-03:1a-1c") `shouldBe` Right ["UCT-03:1a-1c"]
      fmap (map renderCaseKey) (parseCaseKeys "ETC-21/adminsp") `shouldBe` Right ["ETC-21/adminsp"]
      parseCaseKeys "UCT-03:1a-1c,ETC-21/adminsp"
        `shouldBe` Right [CaseKey (UCT 3) Nothing (Just "1a-1c") Nothing, CaseKey (ETC 21) Nothing Nothing (Just "adminsp")]
    it "refuses what is no key" $
      map (isLeft . parseCaseKeys) ["", "UCT", "XYZ-01", "UCT-01:", "UCT-01/", "UCT-x1"] `shouldBe` replicate 6 True

  describe "catalogue" $ do
    it "holds M3's four cases in the Test Cases' order, at their pages (PDF 29, 52, 77, 78; printed 28, 51, 76, 77)" $
      [ (renderCaseKey (ceKey e), cePdf e, cePrinted e) | e <- catalogue ]
        `shouldBe` [("UCT-01", 29, 28), ("SPF-02:1", 52, 51), ("ETC-02", 77, 76), ("ETC-03", 78, 77)]
    it "gives the pages of UCT-01, ETC-02, ETC-03 and SPF-02:1 as 29, 77, 78 and 52" $
      [ cePdf e | Right ks <- [parseCaseKeys "UCT-01,ETC-02,ETC-03,SPF-02:1"], k <- ks, e <- catalogue, ceKey e == k ]
        `shouldBe` [29, 77, 78, 52]
    it "titles them as the Test Cases head them (ETC-02's dash an en dash)" $
      map ceTitle catalogue `shouldBe`
        [ "Level 0 Discovery"
        , "IF-RECV Behavior Tests, Case 1: an IF-RECV while the SD is in Awaiting IF-SEND"
        , "General \x2013 IF-SEND/IF-RECV Synchronous Protocol"
        , "Invalid IF-SEND Transfer Length" ]
    it "has a case for each entry, and an entry for each case, in its order" $
      map cKey cases `shouldBe` map ceKey catalogue
    it "holds the cases the synchronous protocol's laws name: ETC-02 and SPF-02 case 1" $
      [ k | k <- [key (ETC 2) Nothing, key (SPF 2) (Just 1)], k `elem` map ceKey catalogue ]
        `shouldBe` [key (ETC 2) Nothing, key (SPF 2) (Just 1)]
    it "puts each case in its family's section, whose list it waives LockingManufactured of" $ do
      [ cSection c | c <- cases ] `shouldBe` [ sectionOf (ckTest (cKey c)) | c <- cases ]
      [ LockingManufactured `elem` map wPrereq (cWaives c) | c <- cases ] `shouldBe` replicate 4 True

  describe "common" $ do
    it "lists 5.2's six prerequisites (PDF 29) and 6.2's, the same (PDF 49)" $ do
      common UseCases `shouldBe` zip [SyncIdle, LockingManufactured, CredentialsKnown, HostChallengeIsCurrentPin, ReadWriteSessions, NoSessions] [1 ..]
      common SpecificFunctionality `shouldBe` common UseCases
    it "lists 7.2's five, with no item about HostChallenge (PDF 76)" $
      common ErrorCases `shouldBe` zip [SyncIdle, LockingManufactured, CredentialsKnown, ReadWriteSessions, NoSessions] [1 ..]
    it "has each list at its page, each family in its section" $ do
      map listAt [minBound .. maxBound] `shouldBe` [("5.2", 29), ("6.2", 49), ("7.2", 76)]
      map sectionOf [UCT 1, SPF 2, ETC 2] `shouldBe` [UseCases, SpecificFunctionality, ErrorCases]
