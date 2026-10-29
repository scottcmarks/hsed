{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Hsed.Spec.Bind
Description : The annotations of an IR resolved into the entries of the generated bindings module
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsedc bind@ (design section 5.2): every annotation of the IR becomes one
'Entry' of "Hsed.Spec.Bindings.Print", the @doc@ anchor and the rule anchors as
written, a table's full binding as written, and a table's bare modifier chain
applied to the default of the rule that binds the table, so that Table 75's
@erratum scott ["Fractoin" ~> "Fraction"]@ prints as @erratum scott
["Fractoin" ~> "Fraction"] typeRow@. A binding shaped by its target
(@refines (core 42)@) takes the target's shape ("Hsed.Spec.Rules"). A
rule-bound table without an annotation prints nothing: that is the point of
the rules. An annotation that cannot be resolved is a finding instead of an
entry: one that "Hsed.Spec.Scope".'parseAnnots' refuses (an id naming nothing,
two annotations on one anchor, a payload that does not parse, a binding of the
wrong kind for its anchor), so that @hsedc bind@ refuses exactly what @hsedc
check@ does.
-}
module Hsed.Spec.Bind
  ( bindEntries
  , bindModule
  ) where

import qualified RIO.List                 as L
import qualified RIO.Map                  as Map
import           RIO

import           Hsed.Spec.Anchor         (AnchorId (..), naturalKey)
import           Hsed.Spec.Bindings.Print (Entry, printBindingsModule)
import           Hsed.Spec.Common         (Finding, Severity (..), SpecId (..), finding)
import           Hsed.Spec.Grammar        (Payload (..))
import           Hsed.Spec.IR             (AnnotIR (..), SpecIR (..))
import           Hsed.Spec.Rules


-- | One entry per annotation that resolves, in canonical order (kind, then the
--   anchor id's natural order); the findings of those that do not.
bindEntries :: SpecIR -> ([Finding], [Entry])
bindEntries ir = (L.nub (lefts results), map snd (L.sortOn fst (rights results)))
  where
    sid              = SpecId ir.spec
    parsed           = parseAnnots ir
    classes          = classify ir
    results          = map entry parsed
    entry pa = do
      let a    = paAnnot pa
          page = a.page + 1
          here = locatedAt sid a.id page
      payload <- paPayload pa
      aid <- maybe (Left (here (finding Error "E-ANCHOR" ("not an anchor id: " <> a.id)))) Right (paAnchor pa)
      let done sb = Right ((fromEnum a.kind, naturalKey a.id), (sid, aid, page, a.struct, sb))
      case aid of
        TableId _ -> case Map.lookup aid classes of
          Just (CommentBound sb)  -> done sb
          Just (RuleBound _ _ sb) -> done sb
          Just (Invalid f)        -> Left f
          Just (Ambiguous _)      -> Left (here (finding Error "E-AMBIGUOUS" "the modifiers apply to the rule default, and more than one rule fires"))
          Just Unbound            -> Left (here (finding Error "E-ANCHOR" "an annotated table cannot be unbound"))
          Nothing                 -> Left (here (finding Error "E-ANCHOR" ("no table " <> a.id <> " in the IR")))
        _ -> case payload of
          Full sb     -> done sb
          Modifiers _ -> Left (here (finding Error "E-MODIFIER" "a bare modifier chain binds only a table"))   -- parseAnnots has refused it already

-- | The text of @Hsed.Bindings.\<Spec\>@ for an IR, or the finding that stops it,
--   with the findings of the annotations left out.
bindModule :: SpecIR -> ([Finding], Either Finding Text)
bindModule ir = (fs, printBindingsModule (SpecId ir.spec) entries)
  where (fs, entries) = bindEntries ir
