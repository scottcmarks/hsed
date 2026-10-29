{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.NegativeSpec
Description : The negative fixtures: comments that must fail, citing the PDF page
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Design section 5.4 gate f and M1 acceptance (f). Each @test/negative/*.hsa@ is
resolved into the committed Core extraction (the anchor's structure path and
page come from the IR's tables) and must make the check logic, and the binding
step, fail with an Error whose text cites @core-2.01.pdf#page=@; the expected
text of each fixture is in 'expected', and a fixture without an entry there
fails the suite. Nothing here compiles generated code, so @stack build
hsed-spec@ stays green while the fixtures fail; GHC's own rejection of an
unknown name in a generated bindings module is tested in
"Hsed.Spec.Bindings.PrintSpec".
-}
module Hsed.Spec.NegativeSpec (spec) where

import qualified RIO.List         as L
import qualified RIO.Text         as T
import           RIO
import           RIO.Directory    (listDirectory)
import           RIO.FilePath     (takeExtension, (</>))
import           Test.Hspec

import           Hsed.Spec.Bind   (bindEntries)
import           Hsed.Spec.Check  (checkIR)
import           Hsed.Spec.Common (ErrataSet (..), isError, renderFinding)
import           Hsed.Spec.CoreIR


-- | What each fixture must fail with: the text its Error starts with, from
--   @hsedc check@ and from @hsedc bind@ (Nothing where bind has nothing to refuse).
expected :: [(FilePath, (Text, Maybe Text))]
expected =
  [ ("wrong-shape.hsa",  ( "E-SHAPE core-2.01.pdf#page=100&T46: Feature does not accept this table"
                         , Nothing ))
  , ("unknown-name.hsa", ( "E-GRAMMAR core-2.01.pdf#page=100&T46: 1:1: unknown name frobnicate"
                         , Just "E-GRAMMAR core-2.01.pdf#page=100&T46: 1:1: unknown name frobnicate" ))
  ]

dir :: FilePath
dir = "test/negative"

spec :: Spec
spec = withCore $ \ir -> do
  fixtures <- runIO (L.sort . filter ((== ".hsa") . takeExtension) <$> listDirectory dir)
  it "has an expected failure for every fixture" $
    fixtures `shouldBe` L.sort (map fst expected)
  forM_ fixtures $ \name -> describe name $ do
    text <- runIO (readFileUtf8 (dir </> name))
    let (sideFs, ir') = annotatedWith ir text
        checkErrors   = map renderFinding (filter isError (checkIR Scott ir'))
        bindErrors    = map renderFinding (filter isError (fst (bindEntries ir')))
    it "resolves against the IR" $
      sideFs `shouldBe` []
    it "fails hsedc check with an Error citing the page" $ do
      checkErrors `shouldSatisfy` any ("core-2.01.pdf#page=" `T.isInfixOf`)
      forM_ (lookup name expected) $ \(want, _) -> checkErrors `shouldSatisfy` any (want `T.isPrefixOf`)
    it "fails hsedc bind where bind has something to refuse" $
      forM_ (lookup name expected >>= snd) $ \want -> bindErrors `shouldSatisfy` any (want `T.isPrefixOf`)
