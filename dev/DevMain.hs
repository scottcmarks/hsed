{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module DevMain where

import           Data.Attoparsec.ByteString       hiding (takeWhile)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  hiding (putStrLn, take,
                                                   takeWhile)
import           Data.Either                      (fromRight)
import           Data.Functor                     ((<$>))
import qualified Data.List.NonEmpty               as NE (fromList)
import           Data.Map.Internal                (Map (..), fromList)
import           Data.String                      (IsString (..))
import           Data.Version
import           GHC                              ()
import           GHC.Base                         (Int, Semigroup (..), String,
                                                   liftA2, many, mapM, pure,
                                                   undefined, ($), (*>), (++),
                                                   (<*), (<*>), (==))
import           GHC.List                         (zip, (!!))
import           GHC.Show                         (Show (..))
import           GHC.Tuple                        ()
import           Language.Haskell.TH
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.PprLib       hiding (char, (<>))
import           Language.Haskell.TH.Syntax
import           Text.PrettyPrint                 hiding (char, (<>))

import           Extras.Bytes                     hiding (take)
import           Extras.GitVersion                (gitVersion)
import           Extras.Hex
import           Extras.Integral                  hiding (char)
import qualified Paths_hsed
import           System.IO                        hiding (char8)
import           System.SED.Common.ColumnTypes.TH
import           System.SED.Common.TableUIDs
import           System.SED.Common.TableUIDs.TH
import           System.SED.Common.UID


dev :: IO ()
dev = putStrLn "dev"




typeTableParser :: Parser TypeTableRow
typeTableParser = do
    pieceLengths <- skipSpace *> title *> rowSep
    rows         <- header pieceLengths *> rowSep *> many1 (typeTableRow pieceLengths)
    _            <- rowSep *> blankLines *> endOfInput
    pure $ sconcat $ NE.fromList rows


typeTableRow :: [Int] -> Parser TypeTableRow
typeTableRow lengths =
    do
        [uidField, typeName, formatString] <- tableRowFields lengths
        pure $ TypeTableRow uidField typeName formatString

title :: Parser ByteString
title = string "Table 50 ACL" <* endOfLine
    <?> "Table title"

spaces :: Parser ByteString
spaces = takeWhile (== ' ')

rowSepFieldLengths :: Parser [Int]
rowSepFieldLengths = char8 '+' *> many (length <$> takeWhile (== '-') <*  char '+' )
    <?> "row separator fields"

rowSep :: Parser [Int]
rowSep = (:) <$> (length <$> spaces) <*> rowSepFieldLengths <* endOfLine
    <?> "row separator"

blankLines :: Parser ()
blankLines = many (spaces *> endOfLine) *> pure ()
    <?> "blank lines"

tableRowFields :: [Int] -> Parser [ByteString]
tableRowFields lengths =
    do
        [_leader, uidField, typeName, formatString] <- (mapM takeField lengths <* endOfLine)
        pure [uidField, typeName, formatString]
    where
       takeField len = take len <* char '|'




data TypeTableRow = TypeTableRow TypeUIDField TypeName FormatString
    deriving (Show)

instance Semigroup TypeTableRow where
    (TypeTableRow u1 n1 f1) <> (TypeTableRow _u2 _n2 f2) =
        (TypeTableRow u1 n1 (f1 <> f2))

type TypeUIDField = ByteString

type TypeName = ByteString

type FormatString = ByteString


header :: [Int] -> Parser ()
header lengths = tableRowFields lengths *> pure ()
    <?> ("header " ++ show lengths)
