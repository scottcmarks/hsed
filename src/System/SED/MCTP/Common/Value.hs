{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds         #-}
{-|
Module      : System.SED.MCTP.Common.Value
Copyright   : (c) Magnolia Heights R&D, 2020
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Parse and generate values used in method invocations and responses.

-}


module System.SED.MCTP.Common.Value where

import           RIO                               hiding (foldr, length, map,
                                                    mask, reverse, take)
import           Test.QuickCheck                   hiding (generate)

import           System.SED.MCTP.Common.StreamItem
import           System.SED.MCTP.Common.Token

{-

Due to the nature of method parameters and results, there are two additional constructs defined for
messaging that serve as grouping mechanisms for the basic types: Named values and List values.

  a. Named values. The name (a byte-string/integer/uinteger value) followed by its value (any
messaging type, i.e. byte-string values, N length signed or unsigned integer values, list values,
or Named values).
-}

newtype Datum = Datum Token
    deriving (Show,Eq)

class (Show v) => IsDatum v where
    datum :: v -> Datum
    datum = fromMaybe <$> error . ("Not a Datum: " <>) . show <*> mdatum

    mdatum :: v -> Maybe Datum
    mdatum = Just . datum

instance IsDatum Datum where
    datum = id

instance IsDatum Token where
    mdatum = (Datum <$>) . maybeDatumToken

maybeDatumToken ::Token -> Maybe Token
maybeDatumToken t@Unsigned{} = Just t
maybeDatumToken t@Signed  {} = Just t
maybeDatumToken t@Bytes   {} = Just t
maybeDatumToken _            = Nothing


isDatumToken :: Token -> Bool
isDatumToken = isJust . maybeDatumToken


instance IsToken Datum where
    token (Datum v) = v
    fromToken = mdatum

instance IsDatum Natural where
    datum = udatum

udatum :: Natural -> Datum
udatum = Datum . Unsigned

instance IsDatum Integer where
    datum = sdatum

sdatum :: Integer -> Datum
sdatum = Datum . Signed

instance IsDatum ByteString where
    datum = bdatum

bdatum :: ByteString -> Datum
bdatum = Datum . Bytes

instance IsDatum String where
    datum = bdatum . fromString

newtype Name = Name Datum
    deriving (Show,Eq)

class (Show d) => IsName d where
    name :: d -> Name
    name = fromMaybe <$> error . ("Not a Name: " <>) . show <*> mname

    mname :: d -> Maybe Name
    mname = Just . name

instance IsName Name where
    name = id

instance IsName Datum where
    name = Name

instance IsName Token where
    name = name . datum

instance IsName Natural where
    name = name . datum

instance IsName Integer where
    name = name . datum

instance IsName ByteString where
    name = name . datum

instance IsName String where
    name = name . (fromString::String->ByteString)

instance IsDatum Name where
    datum (Name d) = d

instance IsToken Name where
    token (Name d) = token d
    fromToken = mname


data NamedValue = NamedValue Name Value
    deriving (Show,Eq)

(.:) :: (IsName n, IsValue v) => n -> v -> NamedValue
n .: v = NamedValue (name n) (value v)
infixr 5 .:

instance IsName NamedValue where
    name (NamedValue n _) = n

newtype List = List [Value]
    deriving (Show,Eq)

data Value = D Datum
           | N NamedValue
           | L List
    deriving (Show,Eq)

class IsValue a where
    value :: a -> Value
    mvalue :: a -> Maybe Value
    mvalue = Just . value

instance IsValue Value where
    value = id

instance IsValue Datum where
    value = D

instance IsValue NamedValue where
    value = N

instance IsValue List where
    value = L

instance IsValue Token where
    value = value . datum

instance IsValue Natural where
    value = value . datum

instance IsValue Integer where
    value = value . datum

instance IsValue ByteString where
    value = value . datum

instance IsValue String where
    value = value . datum

instance IsToken Value where
    mtoken (D (Datum d)) = mtoken d
    mtoken _             = Nothing
    fromToken = mvalue


instance StreamItem Datum where
    parser    = datum <$> (parser::Parser Token)
    generate (Datum t) = generate t

instance StreamItem Name where
    parser    = name <$> (parser::Parser Datum)
    generate (Name d)  = generate d


instance StreamItem NamedValue where
    parser = require StartName
          *> (NamedValue <$> parser <*> parser) <*
             require EndName
    generate (NamedValue n v) =
        generate StartName
     <> generate n
     <> generate v
     <> generate EndName

instance StreamItem List where
    parser = require StartList
          *> (List <$> many parser) <*
             require EndList
    generate (List values) =
        generate StartList
     <> mconcat (generate <$> values)
     <> generate EndList

instance StreamItem Value where
    parser = D <$> parser
         <|> N <$> parser
         <|> L <$> parser
    generate (D d) = generate d
    generate (N n) = generate n
    generate (L l) = generate l


instance Arbitrary Datum where
    arbitrary = frequency
        [ (15, udatum <$> (arbitrary :: Gen Natural))
        , (10, sdatum <$> (arbitrary :: Gen Integer))
        , (20, bdatum <$> (arbitrary :: Gen ByteString))
        ]

        -- Datum <$> arbitrary

instance Arbitrary Name where
    arbitrary = Name <$> arbitrary

instance Arbitrary NamedValue where
    arbitrary = NamedValue <$> arbitrary <*> arbitrary

instance Arbitrary List where
    arbitrary = List <$> arbitrary

instance Arbitrary Value where
    arbitrary = frequency
        [ (10, D <$> arbitrary)
        , (10, N <$> arbitrary)
        , (15, L <$> arbitrary)
        ]
