{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Hsed.Gen.Core.EquivTH
Description : One comparison per generated definition that has a hand-written counterpart
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

The splice behind "Hsed.Gen.Core.EquivSpec" (design docs/spec-as-source.md
section 5.5, the regenerate-and-compare oracle, M1 acceptance (d)). It reads
the generated module's source at compile time, takes every @u\<Name\>Type@,
@f\<Name\>Type@ and @data Core_\<enum\>@ it defines, and for each one the
hand-written module also defines builds a 'Comparison' of the two values as
'show' renders them (for an enumeration: each constructor's name with its
'fromEnum'). So the comparison grows with the generated module by itself, and
a definition with no counterpart is listed rather than dropped.
-}
module Hsed.Gen.Core.EquivTH
  ( Comparison (..)
  , differs
  , comparisons
  ) where

import           Control.Monad              (forM)
import           Data.List                  (isPrefixOf, isSuffixOf, stripPrefix)
import           Data.Maybe                 (mapMaybe)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (addDependentFile)


-- | A generated definition beside its hand-written counterpart, both shown.
data Comparison = Comparison
  { cName        :: String
  , cGenerated   :: String
  , cHandWritten :: String
  }
  deriving (Eq, Show)

differs :: Comparison -> Bool
differs c = cGenerated c /= cHandWritten c

-- | @$(comparisons "gen/Hsed/Gen/Core/Types.hs" "G" "H")@, with the generated
--   module imported qualified as @G@ and the hand-written one as @H@: an
--   expression of type @([Comparison], [String])@, the comparisons and the
--   generated names without a counterpart.
comparisons :: [FilePath] -> String -> String -> Q Exp
comparisons paths g h = do
  mapM_ addDependentFile paths
  srcs <- runIO (mapM readFile paths)
  let ls     = concatMap lines srcs
      values = mapMaybe valueName ls
      types  = mapMaybe (stripPrefix "data ") ls
  vs <- forM values $ \n -> pair n <$> lookupValueName (g ++ "." ++ n) <*> lookupValueName (h ++ "." ++ n)
  ts <- forM types $ \n -> pair n <$> lookupTypeName (g ++ "." ++ n) <*> lookupTypeName (h ++ "." ++ n)
  let compared = [ valueComparison n a b | (n, Just a, Just b) <- vs ] ++ [ enumComparison n a b | (n, Just a, Just b) <- ts ]
      unmatched = [ n | (n, _, Nothing) <- vs ++ ts ]
  tupE [listE compared, lift' unmatched]
  where
    pair n a b = (n, a, b)
    lift' = listE . map (litE . stringL)
    -- "uAC_elementType :: UID" and "fAC_elementType :: String"
    valueName l = case words l of
      [n, "::", ty] | ty `elem` ["UID", "String"], any (`isPrefixOf` n) ["u", "f"], "Type" `isSuffixOf` n -> Just n
      _ -> Nothing
    valueComparison n a b = [| Comparison n (show $(varE a)) (show $(varE b)) |]
    enumComparison n a b =
      [| Comparison n (show [ (show x, fromEnum x) | x <- [minBound .. maxBound :: $(conT a)] ])
                      (show [ (show x, fromEnum x) | x <- [minBound .. maxBound :: $(conT b)] ]) |]
