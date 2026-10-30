{-|
Module      : Hsed.Capability.TH
Description : The capability a function's first argument, a Transport, is at, read from its type
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

A splice for "Hsed.CapabilitySpec": the type GHC gives a function of a
transport, reified, its first argument's capability as a string, so that
the test holds 'Hsed.Wire.ifSend' and 'Hsed.Wire.inject' to @'ReadWrite@
transports and 'Hsed.Wire.ifRecv' to any.
-}
module Hsed.Capability.TH
  ( transportCapability
  ) where

import           Language.Haskell.TH


-- | The capability of the transport a function takes first: the promoted
--   constructor's name (@ReadWrite@), or @any@ for a type variable.
transportCapability :: Name -> Q Exp
transportCapability fn = do
  t <- reifyType fn
  case firstArgument (unquantified t) of
    Just (AppT (ConT tc) cap) | nameBase tc == "Transport" -> stringE (capability cap)
    other -> fail (show fn <> " takes no Transport first: " <> maybe (pprint t) pprint other)
  where
    unquantified (ForallT _ _ body) = unquantified body
    unquantified body               = body
    firstArgument (AppT (AppT ArrowT a) _) = Just a
    firstArgument _                        = Nothing
    capability (PromotedT c) = nameBase c
    capability (ConT c)      = nameBase c
    capability (VarT _)      = "any"
    capability (SigT c _)    = capability c
    capability other         = pprint other
