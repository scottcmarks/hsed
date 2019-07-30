{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- {-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wunticked-promoted-constructors #-}



module DevMain where

import           System.IO

import           Data.Foldable (foldr)
import           Data.Text     (Text, pack, replace, snoc, unpack)

import           GHC.Arr       (range)
import           GHC.Base      (String, flip, ($), (.), (<>))

import           Text.Heredoc  (here)


dev :: IO ()
dev = putStrLn "dev"



{-
(from RFC 5234)

   [T]he version of a ruleset included in a
   specification might need preprocessing to ensure that it can
   be interpreted by an ABNF parser.
-}

preprocessABNF :: Text -> Text
preprocessABNF = correctABNFFromTCGABNF

correctABNFFromTCGABNF :: Text -> Text
correctABNFFromTCGABNF = replace "_" "-"
                       . (flip (foldr ($)) [replace (snoc s i) (snoc (s <> "%d") i) | i <- range ('0','9'), s <- ["= ","/"]])

coreTypesTCGABNF :: Text
coreTypesTCGABNF = [here|
Type = Base_Type / Simple_Type / Enumeration_Type / Alternative_Type / List_Type / Restricted_Reference_Type / General_Reference_Type / Named_Value_Type / Struct_Type / Set_Type


table_kind = 1/2


Base_Type = 0

Simple_Type = 1 bytes_8 uinteger_2

Enumeration_Type = 2 1*(uinteger_2 uinteger_2)

Alternative_Type = 3 2*bytes_8

List_Type = 4 uinteger_2 bytes_8

Restricted_Reference_Type = 5/6 1*bytes_8

General_Reference_Type = 7/8/9

General_Reference_Table_Type = 10 table_kind

Named_Value_Name_Type = 11 1*32bytes bytes_8

Name_Value_Integer_Type = 12 integer_2 bytes_8

Name_Value_Uinteger_Type = 13 uinteger_2 bytes_8

Struct_Type = 14 1*bytes_8

Set_Type = 15 1*(uinteger_2 uinteger_2)

|]


correctABNFToTCGABNF :: String -> String
correctABNFToTCGABNF = unpack . replace "-" "_" . replace "%d" "" . pack

data (KnownNat n) => CoreType n where
    Base_Type :: CoreType 0
