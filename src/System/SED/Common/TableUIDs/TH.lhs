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

import           Control.Applicative                  (many, pure, (<$>),
                                                       (<*), (<*>), (*>))
import           Data.Attoparsec.ByteString           (Parser, (<?>),
                                                       parseOnly, take)
import           Data.Attoparsec.ByteString.Char8     (char, endOfInput, endOfLine,
                                                       hexadecimal, isSpace_w8,
                                                       skipSpace, string)
import           Data.ByteString                      (ByteString, spanEnd,
                                                       length, unpack)
import           Data.ByteString.Char8                (split)
import qualified Data.ByteString.Char8 as C           (unpack)
import           Data.Either                          (either)
import           Data.Foldable                        (foldr, mapM_)
import           Data.List                            ((!!), concat, foldl,
                                                       init, map)
import           Data.Map                             (Map, fromList)
import           Data.String                          (fromString)
import           Data.Tuple                           (fst,snd)
import           GHC.Base                             (Eq(..), Semigroup,
                                                       Monoid, Int, String,
                                                       Maybe(..), (.), (++),
                                                       (==), (<>), ($), id,
                                                       mconcat, mempty)
import           GHC.Err                              (error,undefined)
import           GHC.Real                             (toInteger)
import           GHC.Show                             (Show(..))
import           Language.Haskell.TH                  (loc_start, loc_filename,
                                                       location, mkName,
                                                       Body(..), Lit(..), Type(..),
                                                       Exp(..), Dec(..), Pat(..),
                                                       Name, DecsQ, Q)
import           Language.Haskell.TH.Quote            (QuasiQuoter(..))
import           Extras.Bytes                         (unwrap)
import           System.SED.Common.Table              (TableName(..),TemplateName(..))
import           System.SED.Common.UID                (HalfUID(..),UID(..),
                                                       halfUID, uid, fpack,
                                                       uidUpper, uidLower)

t240 :: QuasiQuoter
t240 = QuasiQuoter
    { quoteExp = undefined
    , quotePat = undefined
    , quoteDec = t240Decs
    , quoteType = undefined
    }

t240Decs :: String -> DecsQ
t240Decs s =
    do
        (_filename, _line, _column) <- getPosition
        let (UIDRowDecs us ehs eus) =
                foldr gather mempty
              $ either error id
              $ parseOnly table240Parser
              $ fromString s
              where row `gather` decs = decs <> dUIDRow row
            hmap = mkName "nameHalfUID"
            umap = mkName "nameUID"
            mapd n t v =
                [ SigD n (AppT (AppT (ConT ''Map) (ConT t)) (ConT ''String))
                , ValD (VarP n) (NormalB (AppE (VarE 'fromList) (ListE v))) []
                ]
        pure $ concat [ us, mapd hmap ''HalfUID ehs, mapd umap ''UID eus]



table240Parser :: Parser [UIDRow]
table240Parser = skipSpace
              *> title
              *> rowSep
              *> header
              *> header
              *> rowSep
              *> many (uidRow <* rowSep)
              <* many (many (char ' ') <* endOfLine)
              <* endOfInput
             <?> "Table 240"


data UIDRow = UIDRow UID UID HalfUID TableName TemplateName
    deriving (Eq)

uidRow :: Parser UIDRow
uidRow = string "    |"
      *> ( mkUIDRow
        <$> pUID1          <* string "|"
        <*> pUID2          <* string "|"
        <*> pTableName     <* string "|"
        <*> pTemplateName  <* string "|"
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

pUID1 :: Parser UID
pUID1 =  pUIDField 1

pUID2 :: Parser UID
pUID2 =  pUIDField 2

pTableName    :: Parser TableName
pTableName    = TableName    <$> pTrimmedField 3

pTemplateName :: Parser TemplateName
pTemplateName = TemplateName <$> pTrimmedField 4

pUIDField :: Int -> Parser UID
pUIDField i = hexUID <$> pField i
  where hexUID = UID
                 . fpack
                 . (map ((either error id) . (parseOnly hexadecimal)))
                 . init . split ' '

pTrimmedField :: Int -> Parser ByteString
pTrimmedField i = trimTrailingWhitespace <$> pField i
  where trimTrailingWhitespace s = case spanEnd isSpace_w8 s of (s',_) -> s'

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
    let table :: String
        table = C.unpack tableName
        vn :: String -> String -> Name
        vn p t = mkName $ mconcat [ p, table, t]
        h = vn "h" ""
        u = vn "u" "Table"
        uo = vn "u" "TableObject"
    in UIDRowDecs
           [ dSig uo ''UID
           , dVal uo $ eUID objectUID
           , dSig u ''UID
           , dVal u $ eUID tableUID
           , dSig h ''HalfUID
           , dVal h $ eHalfUID tableHalfUID
           ]
           [ eValP h $ table
           ]
           [ eValP u $ table ++ " Table"
           , eValP uo $ table ++ " Table Object"
           ]

  where eHalfUID (HalfUID fb) = foldl arg (VarE 'halfUID) $ unpack $ unwrap fb
          where arg e b = AppE e (LitE (IntegerL (toInteger b)))
        eUID (UID fb) = foldl arg (VarE 'uid) $ unpack $ unwrap fb
          where arg e b = AppE e (LitE (IntegerL (toInteger b)))
        eValP hn ts =
            TupE [VarE hn, LitE (StringL ts)]

data UIDRowDecs = UIDRowDecs [Dec] [Exp] [Exp]
    deriving(Eq,Show)

instance Semigroup UIDRowDecs
  where (UIDRowDecs u1 eh1 eu1) <> (UIDRowDecs u2 eh2 eu2) =
            UIDRowDecs (u1<>u2) (eh1<>eh2) (eu1<>eu2)

instance Monoid UIDRowDecs
  where mempty = UIDRowDecs [] [] []

getPosition :: Q (String, Int, Int) -- TODO: Use me or lose me
getPosition = transPos <$> location where
  transPos loc = (loc_filename loc,
                  fst (loc_start loc),
                  snd (loc_start loc))



\end{code}
\begin{code}
\end{code}
\end{document}
