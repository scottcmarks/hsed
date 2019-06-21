{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module DevMain where

import           Data.Attoparsec.ByteString       hiding (takeWhile)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  hiding (putStrLn, takeWhile)
import           Data.Functor                     ((<$>))
import           Data.Map.Internal                (Map (..), fromList)
import           Data.String                      (IsString (..))
import           Data.Version
import           GHC                              ()
import           GHC.Base                         (String, liftA2, many, pure,
                                                   undefined, ($), (*>), (<*),
                                                   (<*>), (==))
import           GHC.List                         (zip, (!!))
import           GHC.Tuple                        ()
import           Language.Haskell.TH
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.PprLib       hiding (char, (<>))
import           Language.Haskell.TH.Syntax
-- import           RIO                              hiding (takeWhile)
import           Text.PrettyPrint                 hiding (char, (<>))

import           Extras.Bytes
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

qAC_element

-- qUIDRow uidrow


title :: Parser ByteString
title = string "Table 50 ACL" :: Parser ByteString

spaces :: Parser ByteString
spaces = takeWhile (== ' ') :: Parser ByteString

rowSepFields :: Parser [ByteString]
rowSepFields = char8 '+' *> many ( takeWhile (== '-') <* char '+' )
           <?> "row separator fields"

rowSep :: Parser [ByteString]
rowSep = (:)
     <$> spaces
     <*> rowSepFields <* endOfLine
     <?> "row separator"

blankLines :: Parser ()
blankLines = many (spaces *> endOfLine) *> pure () <?> "blank lines" :: Parser ()

header :: Parser ()
header = undefined :: Parser ()

data TypeTableRow

typeTableRow :: Parser TypeTableRow
typeTableRow = undefined :: Parser TypeTableRow


typeTableParser :: Parser [TypeTableRow]
typeTableParser = skipSpace
               *> title
               *> rowSep
               *> header
               *> rowSep
               *> many (typeTableRow)        <*    -- <-- the data
                  rowSep <*
                  many (spaces <* endOfLine) <*
                  endOfInput
              <?> "Table 240"
