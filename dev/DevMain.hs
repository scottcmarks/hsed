{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module DevMain where

import           Data.Attoparsec.ByteString       hiding (takeWhile)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  hiding (putStrLn, take,
                                                   takeWhile)
import           Data.Functor                     ((<$>))
import           Data.Map.Internal                (Map (..), fromList)
import           Data.String                      (IsString (..))
import           Data.Version
import           GHC                              ()
import           GHC.Base                         (Int, String, liftA2, many,
                                                   mapM, pure, undefined, ($),
                                                   (*>), (<*), (<*>), (==))
import           GHC.List                         (zip, (!!))
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

title :: Parser ByteString
title = string "Table 50 ACL"
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

data TypeTableRow = TypeTableRow TypeUIDField TypeName FormatString

type TypeUIDField = ByteString

type TypeName = ByteString

type FormatString = ByteString


tableRowFields :: [Int] -> Parser [ByteString]
tableRowFields lengths =
    do
        _leader:fields <- mapM takeField lengths
        pure fields
    where
       takeField len = take len <* char '|'

header :: [Int] -> Parser ()
header lengths = tableRowFields lengths *> pure ()

typeTableRow :: [Int] -> Parser TypeTableRow
typeTableRow lengths = do
    [_leader, uidField, typeName, formatString] <- tableRowFields lengths
    pure $ TypeTableRow uidField typeName formatString


typeTableParser :: Parser [TypeTableRow]
typeTableParser = do
    _ <- skipSpace
    _ <- title
    pieceLengths <- rowSep
    _ <- header pieceLengths
    _ <- rowSep
    rows <- many $ typeTableRow pieceLengths
    _ <- rowSep
    _ <- many $ spaces <* endOfLine
    _ <- endOfInput
    pure rows
