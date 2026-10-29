{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Spec.Rules.TypeRowSpec
Description : Test specifications for the typeRow rule's own pieces
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The Format string a type row prints for the M2a comparison; the rule on the
committed Core extraction is tested through hsedc's checks and compilation
("Hsed.Spec.CheckSpec", "Hsed.Spec.CompileSpec").
-}
module Hsed.Spec.Rules.TypeRowSpec (spec) where

import           RIO
import           Test.Hspec

import           Hsed.Spec.Rules.TypeRow (joinFormat)


spec :: Spec
spec =
  describe "joinFormat" $
    it "joins the pieces with commas and no spaces, as hsed's quasiquoter does" $
      map joinFormat [ "List_Type, *, ACE_expression", "Simple_Type,uinteger, 2", "Restricted_Reference_Type{6}, uidref {AuthorityTableUID}", "Base_Type" ]
        `shouldBe` [ "List_Type,*,ACE_expression", "Simple_Type,uinteger,2", "Restricted_Reference_Type{6},uidref {AuthorityTableUID}", "Base_Type" ]
