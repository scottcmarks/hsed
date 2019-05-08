{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module DevMain where

import           Data.Attoparsec.ByteString
import           Data.Map.Internal                (Map (..), fromList)
import           Data.String                      (IsString (..))
import           Data.Version
import           GHC                              ()
import           GHC.Base                         (String)
import           GHC.List                         (zip, (!!))
import           GHC.Tuple                        ()
import           Language.Haskell.TH
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.PprLib
import           Language.Haskell.TH.Syntax
import           RIO
import           Text.PrettyPrint

import           Extras.Bytes
import           Extras.GitVersion                (gitVersion)
import           Extras.Hex
import           Extras.Integral
import qualified Paths_hsed
import           System.IO
import           System.SED.Common.ColumnTypes.TH
import           System.SED.Common.TableUIDs
import           System.SED.Common.TableUIDs.TH
import           System.SED.Common.UID


dev :: IO ()
dev = putStrLn "dev"

qAC_element

-- qUIDRow uidrow
