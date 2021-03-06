{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : System.SED.MCTP.Common.TablesUIDs
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Table UIDs as specified in Table 240.

The specification for each table is translated to three constant
declarations.  For instance, the information on the @AccessControl@
table becomes

@
    --| The HalfUID that identifies the AccessControl table
    hAccessControl :: HalfUID
    hAccessControl = halfUID 0x00 0x00 0x00 0x07

    --| The UID that identifies the AccessControl table itself
    uAccessControlTable :: UID
    uAccessControlTable = uid 0x00 0x00 0x00 0x07 0x00 0x00 0x00 0x00

    --| The UID that identifies the AccessControl table entry in the Table Table
    uAccessControlTableObject :: UID
    uAccessControlTableObject = uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x07
@

Unfortunately, Template Haskell can neither see manual comments nor create
comments for Haddock to see; hence the names themselves must suffice as documentation.


-}

module System.SED.MCTP.Common.TableUIDs
where

import           Data.Functor                           ((<$>))
import qualified Data.Map                               as Map (lookup)
import           Data.Refined                           (plain)
import           GHC.Base                               (String, (.), (<>))
import           GHC.Maybe                              (Maybe)
import           System.SED.MCTP.Common.Reference_Types (Byte_Table_HalfUID,
                                                         IsTable_HalfUID (..),
                                                         Null_Table_HalfUID,
                                                         Object_Table_HalfUID)
import           System.SED.MCTP.Common.TableUIDs.TH    (t240)
import           System.SED.MCTP.Common.UID             (HalfUID (..), UID (..))


-- * Table HalfUID, UIDs, and Object UIDs
-- **** For each row of the table (after the header row), define the indicated constants.
-- For example, from the second row, we define
--   hSPInfo :: HalfUID
--   othSPInfo :: Object_Table_HalfUID
--   uSPInfoTable :: UID
--   uSPInfoTableObject :: UID
-- together with informtion to map between the above and name strings
--   hSPInfo -> "SPInfo"
--   uSPInfoTable -> "SPInfo Table"
--   uSPInfoTableObject -> "SPInfo Table Object"


[t240|


                              Table 240   Table UIDs
    +------------------------+------------------------+--------------+--------+
    |UID of Table Descriptor |UID of Table            |Table Name    |Template|
    |Object                  |                        |              |        |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 01 |00 00 00 01 00 00 00 00 |Table         |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 02 |00 00 00 02 00 00 00 00 |SPInfo        |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 03 |00 00 00 03 00 00 00 00 |SPTemplates   |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 04 |00 00 00 04 00 00 00 00 |Column        |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 05 |00 00 00 05 00 00 00 00 |Type          |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 06 |00 00 00 06 00 00 00 00 |MethodID      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 07 |00 00 00 07 00 00 00 00 |AccessControl |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 08 |00 00 00 08 00 00 00 00 |ACE           |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 09 |00 00 00 09 00 00 00 00 |Authority     |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 0A |00 00 00 0A 00 00 00 00 |Certificates  |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 0B |00 00 00 0B 00 00 00 00 |C_PIN         |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 0C |00 00 00 0C 00 00 00 00 |C_RSA_1024    |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 0D |00 00 00 0D 00 00 00 00 |C_RSA_2048    |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 0E |00 00 00 0E 00 00 00 00 |C_AES_128     |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 0F |00 00 00 0F 00 00 00 00 |C_AES_256     |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 10 |00 00 00 10 00 00 00 00 |C_EC_160      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 11 |00 00 00 11 00 00 00 00 |C_EC_192      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 12 |00 00 00 12 00 00 00 00 |C_EC_224      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 13 |00 00 00 13 00 00 00 00 |C_EC_256      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 14 |00 00 00 14 00 00 00 00 |C_EC_384      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 15 |00 00 00 15 00 00 00 00 |C_EC_521      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 16 |00 00 00 16 00 00 00 00 |C_EC_163      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 17 |00 00 00 17 00 00 00 00 |C_EC_233      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 18 |00 00 00 18 00 00 00 00 |C_EC_283      |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 19 |00 00 00 19 00 00 00 00 |C_HMAC_160    |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 1A |00 00 00 1A 00 00 00 00 |C_HMAC_256    |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 1B |00 00 00 1B 00 00 00 00 |C_HMAC_384    |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 1C |00 00 00 1C 00 00 00 00 |C_HMAC_512    |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 00 1D |00 00 00 1D 00 00 00 00 |SecretProtect |Base    |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 02 01 |00 00 02 01 00 00 00 00 |TPerInfo      |Admin   |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 02 03 |00 00 02 03 00 00 00 00 |CryptoSuite   |Admin   |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 02 04 |00 00 02 04 00 00 00 00 |Template      |Admin   |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 02 05 |00 00 02 05 00 00 00 00 |SP            |Admin   |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 04 01 |00 00 04 01 00 00 00 00 |ClockTime     |Clock   |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 06 01 |00 00 06 01 00 00 00 00 |H_SHA_1       |Crypto  |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 06 02 |00 00 06 02 00 00 00 00 |H_SHA_256     |Crypto  |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 06 03 |00 00 06 03 00 00 00 00 |H_SHA_384     |Crypto  |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 06 04 |00 00 06 04 00 00 00 00 |H_SHA_512     |Crypto  |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 0A 01 |00 00 0A 01 00 00 00 00 |Log           |Log     |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 0A 02 |00 00 0A 02 00 00 00 00 |LogList       |Log     |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 08 01 |00 00 08 01 00 00 00 00 |LockingInfo   |Locking |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 08 02 |00 00 08 02 00 00 00 00 |Locking       |Locking |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 08 03 |00 00 08 03 00 00 00 00 |MBRControl    |Locking |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 08 04 |00 00 08 04 00 00 00 00 |MBR           |Locking |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 08 05 |00 00 08 05 00 00 00 00 |K_AES_128     |Locking |
    +------------------------+------------------------+--------------+--------+
    |00 00 00 01 00 00 08 06 |00 00 08 06 00 00 00 00 |K_AES_256     |Locking |
    +------------------------+------------------------+--------------+--------+

|]

-- * Safe accessor functions for the generated `Map's
--
-- The definitions of @nameHalfUID@ and @nameUID@ are generated
-- by the splice of @t240@ above.


class TableID a where
    lookup :: a -> Maybe String
instance TableID(HalfUID) where
    lookup = (`Map.lookup` nameHalfUID)
instance TableID(UID) where
    lookup = (`Map.lookup` nameUID)
instance {-# OVERLAPPABLE #-} IsTable_HalfUID a => TableID(a) where
    lookup = lookup . plain . toTable_HalfUID
instance {-# OVERLAPPING #-} TableID(Null_Table_HalfUID) where
    lookup = ((<> " Null Table HalfUID") <$>) . lookup . plain . toTable_HalfUID
instance {-# OVERLAPPING #-} TableID(Byte_Table_HalfUID) where
    lookup = ((<> " Byte Table HalfUID") <$>) . lookup . plain . toTable_HalfUID
instance {-# OVERLAPPING #-} TableID(Object_Table_HalfUID) where
    lookup = ((<> " Object Table HalfUID") <$>) . lookup . plain . toTable_HalfUID
 -- TODO -- finish these off for O_T_UID etc?, elim OVERLAP
