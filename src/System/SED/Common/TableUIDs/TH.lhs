\documentstyle{article}
\begin{document}
\chapter{TableUIDs Table 240 Template Haskell}

TableUIDs Template Haskell


\begin{code}
{-|
Module      : System.SED.Common.TableUIDS.TH
Description : SED table UIDs Template Haskell
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

TableUIDs Template Haskell.

-}

{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.SED.Common.TableUIDs.TH (t240)
where

import           Control.Applicative                  (many, (<$>), (<*),
                                                       (<*>), (*>))

import           Data.Attoparsec.ByteString           (Parser, parseOnly,
                                                       skipWhile, take,
                                                       (<?>))
import           Data.Attoparsec.ByteString.Char8     (endOfInput, endOfLine, skipSpace, string)


import           Data.ByteString                      (ByteString, length)
import qualified Data.ByteString                 as B (unpack)

import           Data.ByteString.Char8                (split)
import qualified Data.ByteString.Char8           as C (unpack)
import           Data.Either                          (either)
import           Data.Foldable                        (foldr, mapM_)
import           Data.List                            ((!!), concat, foldl,
                                                       init, map)
import           Data.Map                             (Map, fromList)
import           Data.String                          (fromString)

import           GHC.Base                             (Eq(..), Semigroup,
                                                       Monoid, Int, String,
                                                       Maybe(..), (.), (++),
                                                       (==), (<>), ($), id,
                                                       mconcat, mempty)
import           GHC.Err                              (error,undefined)
import           GHC.Real                             (toInteger)
import           GHC.Show                             (Show(..))


import           Language.Haskell.TH                  (mkName,
                                                       Body(..), Lit(..), Type(..),
                                                       Exp(..), Dec(..), Pat(..),
                                                       Name)
import           Language.Haskell.TH.Quote            (QuasiQuoter(..))
import           Language.Haskell.TH.Syntax           (returnQ)


import           Extras.Bytes(funpack)

import           System.SED.Common.Table              (TableName(..),TemplateName(..))
import           System.SED.Common.UID                (HalfUID(..),UID(..),
                                                       halfUID, uid,
                                                       uidUpper, uidLower)
import           System.SED.Common.Util               (hexUID, trimTrailingWhitespace)

-- | Bespoke Quasiquoter for Table 240
t240 :: QuasiQuoter
t240 = QuasiQuoter
    { quoteExp = undefined
    , quotePat = undefined
    , quoteDec = returnQ <$> t240Decs
    , quoteType = undefined
    }

t240Decs :: String -> [Dec]
t240Decs s =        concat [ us,
                             mapd (mkName "nameHalfUID") ''HalfUID ehs,
                             mapd (mkName "nameUID")     ''UID     eus]
    where
      -- | approx. [d| $n :: $t ; $n = $v |]
      mapd n t v =
                [ SigD n (AppT (AppT (ConT ''Map) (ConT t)) (ConT ''String))
                , ValD (VarP n) (NormalB (AppE (VarE 'fromList) (ListE v))) []
                ]
      (UIDRowDecs us ehs eus) =
                foldr gather mempty
              $ either error id
              $ parseOnly table240Parser
              $ fromString s
              where row `gather` decs = decs <> dUIDRow row
      table240Parser = skipSpace
              *> title
              *> rowSep
              *> header
              *> header
              *> rowSep
              *> many (uidRow <* rowSep)    <*    -- <-- the data
                 many (spaces <* endOfLine) <*
                 endOfInput
             <?> "Table 240"
      spaces = skipWhile (== 32)



data UIDRow = UIDRow UID UID HalfUID TableName TemplateName
    deriving (Eq)

uidRow :: Parser UIDRow
uidRow = string "    |"
      *> ( mkUIDRow
        <$> pTableObjectUID <* string "|"
        <*> pTableUID       <* string "|"
        <*> pTableName      <* string "|"
        <*> pTemplateName   <* string "|"
         )
     <*  endOfLine
     <?> "UID Row"

mkUIDRow :: UID -> UID -> TableName -> TemplateName -> UIDRow
mkUIDRow u1 u2 ta te = case u1 `matches` u2 of
                         Just h -> UIDRow u1 u2 h ta te
                         Nothing -> error $ show u1
                                         ++ " does not match "
                                         ++ show u2
    where matches :: UID -> UID -> Maybe HalfUID
          matches u1' u2' =
              let h = uidLower u1'
              in if h == uidUpper u2' then Just h else Nothing

pTableObjectUID :: Parser UID
pTableObjectUID =  pUIDField 1

pTableUID :: Parser UID
pTableUID =  pUIDField 2

pTableName    :: Parser TableName
pTableName    = TableName    <$> pTrimmedField 3

pTemplateName :: Parser TemplateName
pTemplateName = TemplateName <$> pTrimmedField 4

pUIDField :: Int -> Parser UID
pUIDField i = hexUID <$> pTrimmedField i

pTrimmedField :: Int -> Parser ByteString
pTrimmedField i = trimTrailingWhitespace <$> pField i

pField :: Int -> Parser ByteString
pField i = take (pieceLengths !! i)


title :: Parser ()
title = string "Table 240   Table UIDs" *> endOfLine

header :: Parser ()
header = mapM_ (\len -> take len *> string "|") pieceLengths *> endOfLine

rowSep :: Parser ()
rowSep = string rowSepString *> endOfLine

pieceLengths :: [Int]
pieceLengths = map length $ init . split '+' $ rowSepString

rowSepString :: ByteString
rowSepString = "    +------------------------+------------------------+--------------+--------+"


dSig :: Name -> Name -> Dec
dSig n t = SigD n (ConT t)

dVal :: Name -> Exp -> Dec
dVal n e = ValD (VarP n) (NormalB e) []

dUIDRow :: UIDRow -> UIDRowDecs
dUIDRow (UIDRow objectUID tableUID tableHalfUID (TableName tableName) (TemplateName _templateName)) =
    UIDRowDecs
    [ dSig o ''UID     , dVal o $ eUID     objectUID
    , dSig u ''UID     , dVal u $ eUID     tableUID
    , dSig h ''HalfUID , dVal h $ eHalfUID tableHalfUID
    ]
    [ eValP h $ table
    ]
    [ eValP u $ table ++ " Table"
    , eValP o $ table ++ " Table Object"
    ]
  where eHalfUID (HalfUID fb) = foldl arg (VarE 'halfUID) $ B.unpack $ funpack fb
          where arg e b = AppE e (LitE (IntegerL (toInteger b)))
        eUID     (UID     fb) = foldl arg (VarE 'uid    ) $ B.unpack $ funpack fb
          where arg e b = AppE e (LitE (IntegerL (toInteger b)))
        eValP hn ts =  TupE [VarE hn, LitE (StringL ts)]
        table = C.unpack tableName
        vn p t = mkName $ mconcat [ p, table, t]
        h = vn "h" ""
        u = vn "u" "Table"
        o = vn "u" "TableObject"

data UIDRowDecs = UIDRowDecs [Dec] [Exp] [Exp]
    deriving(Eq,Show)

instance Semigroup UIDRowDecs
  where (UIDRowDecs u1 eh1 eu1) <> (UIDRowDecs u2 eh2 eu2) =
            UIDRowDecs (u1<>u2) (eh1<>eh2) (eu1<>eu2)

instance Monoid UIDRowDecs
  where mempty = UIDRowDecs [] [] []


\end{code}
\end{document}
