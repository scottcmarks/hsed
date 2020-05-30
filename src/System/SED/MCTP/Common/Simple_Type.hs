{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : System.SED.MCTP.Common.Simple_Type
Description : Core data type formats
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Formats.

-}

module System.SED.MCTP.Common.Simple_Type
where

import           System.SED.MCTP.Common.Base_Type (Core_bytes, Core_integer,
                                                   Core_max_bytes,
                                                   Core_uinteger)

type Core_bytes_4       = Core_bytes  4
type Core_bytes_12      = Core_bytes 12
type Core_bytes_16      = Core_bytes 16
type Core_bytes_20      = Core_bytes 20
type Core_bytes_32      = Core_bytes 32
type Core_bytes_48      = Core_bytes 48
type Core_bytes_64      = Core_bytes 64

type Core_feedback_size = Core_uinteger 2

type Core_integer_1     = Core_integer 1
type Core_integer_2     = Core_integer 2

type Core_max_bytes_32  = Core_max_bytes 32
type Core_name          = Core_max_bytes 32
type Core_password      = Core_max_bytes 32
type Core_type_def_max_size = 128
type Core_type_def      = Core_max_bytes Core_type_def_max_size

type Core_uid           = Core_bytes 8

type Core_uinteger_1    = Core_uinteger 1
type Core_uinteger_128  = Core_uinteger 128
type Core_uinteger_2    = Core_uinteger 2
type Core_uinteger_20   = Core_uinteger 20
type Core_uinteger_21   = Core_uinteger 21
type Core_uinteger_24   = Core_uinteger 24
type Core_uinteger_256  = Core_uinteger 256
type Core_uinteger_28   = Core_uinteger 28
type Core_uinteger_30   = Core_uinteger 30
type Core_uinteger_36   = Core_uinteger 36
type Core_uinteger_4    = Core_uinteger 4
type Core_uinteger_48   = Core_uinteger 48
type Core_uinteger_64   = Core_uinteger 64
type Core_uinteger_66   = Core_uinteger 66
type Core_uinteger_8    = Core_uinteger 8
