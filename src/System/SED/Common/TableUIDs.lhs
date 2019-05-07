\documentstyle{article}
\begin{document}
\chapter{TableUIDs}

Tables


\begin{code}
{-|
Module      : System.SED.Common.Tables
Description : SED table UIDs
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Table UIDs.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module System.SED.Common.TableUIDs (
                                     -- * Table UIDs
                                     uTableTable
                                   , uSPInfoTable
                                   , uSPTemplatesTable
                                   , uColumnTable
                                   , uTypeTable
                                   , uMethodIDTable
                                   , uAccessControlTable
                                   , uACETable
                                   , uAuthorityTable
                                   , uCertificatesTable
                                   , uC_PINTable
                                   , uC_RSA_1024Table
                                   , uC_RSA_2048Table
                                   , uC_AES_128Table
                                   , uC_AES_256Table
                                   , uC_EC_160Table
                                   , uC_EC_192Table
                                   , uC_EC_224Table
                                   , uC_EC_256Table
                                   , uC_EC_384Table
                                   , uC_EC_521Table
                                   , uC_EC_163Table
                                   , uC_EC_233Table
                                   , uC_EC_283Table
                                   , uC_HMAC_160Table
                                   , uC_HMAC_256Table
                                   , uC_HMAC_384Table
                                   , uC_HMAC_512Table
                                   , uSecretProtectTable
                                   , uTPerInfoTable
                                   , uCryptoSuiteTable
                                   , uTemplateTable
                                   , uSPTable
                                   , uClockTimeTable
                                   , uH_SHA_1Table
                                   , uH_SHA_256Table
                                   , uH_SHA_384Table
                                   , uH_SHA_512Table
                                   , uLogTable
                                   , uLogListTable
                                   , uLockingInfoTable
                                   , uLockingTable
                                   , uMBRControlTable
                                   , uMBRTable
                                   , uK_AES_128Table
                                   , uK_AES_256Table

                                   -- * Table HalfUIDs
                                   , hNull
                                   , hTable

                                   -- * Utilty functions
                                   , tableHalfUIDFromName             -- | HalfUID for table
                                   , tableNameFromHalfUID             -- | Name for table by HalfUID
                                   , tableUIDFromName                 -- | Table UID for table
                                   , tableDescriptorObjectUIDFromName -- | Table Table Row UID for table
                                   )
where

import           Data.List                    (find)
import           RIO
import           Test.QuickCheck              hiding (generate)

import           System.SED.Common.UID
-- import           System.SED.Common.Table

\end{code}
3.2.5 Tables

Tables SHALL be stored in SP-specific parts of the secure storage area of the TPer. The SP-related
secure storage area(s) of a TPer SHALL only be accessible via the host interface-specific IF-SEND and
IF-RECV commands. Table content SHALL NOT, unless otherwise stated, be part of the User
Addressable Logical Block Address space on the Storage Device and therefore is not affected by the
partitioning or formatting of the Storage Device by the host operating system.

Begin Informative Content

All persistent data for SPs are stored in tables Ð the only data for an SP that persists past the end of a
session is the data that is stored in tables. Tables survive operations on user-areas, such as
reformatting.

A table is defined as a grid with columns and addressable rows. At each column and row intersection
there is a cell. All the cells in a column have the same type. The column types for a host-created table
are specified at table creation.

For some SSCs, the number of rows in a table whose size is not specified is completely determined
when it is created (additional rows are not able to be allocated), but other SSCs define tables whose
size is not specified with a dynamically allocable number of rows. If an SSC permits additional rows to
be added to a table, then the number of rows specified at table creation is the initial number of rows
allocated for that table.

End Informative Content

A table name or table column name MAY be up to 32 bytes in length. From convention, the names
assigned in this document consist of ASCII characters, the first of which is a letter and others are
letters, digits or underscores. Adjacent underscores do not occur. All names are case sensitive.

Within an SP, tables MAY be created and deleted. For each table, rows MAY be created and deleted
(except within a Fromte table Ð see 3.2.5.1), but columns are created only when the table is created.
Tables MAY contain zero or more rows. A specific Security Subsystem Class MAY disallow the
creation of any of these.

Each SP has a set of metadata tables (such as the Table table, Column table, etc.) that describes all
the tables of the SP including the metadata tables themselves.

Access control provides a means to limit the methods that MAY be successfully invoked on tables, or
particular rows or cells of tables.


Some table columns represent control points for functionality provided by an SP, either based on the
templates incorporated into the SP, or on the underlying TPer implementation. If the functionality
represented by a particular column or set of columns as defined in this Specification is not provided by
an SP, then access to the table columns that represent that functionality MAY be restricted.

3.2.5.1 Kinds of Tables

There are two kinds of tables:

a. Fromte table. Fromte tables provide raw data storage. A byte table has one unnamed column
of type bytes_1. The address of the first row in a byte table is 0. Upon creation, the value
of all cells in a byte table SHALL be 0x00. The rows of a byte table SHALL NOT be
allocated or freed (i.e. via CreateRow or DeleteRow). Fromte table rows are addressed by
row number.
b. Object table. Object tables provide storage for data that binds a set of methods and
access controls to that data. When a table is created it SHALL be allocated a fixed number
of fixed-size columns. Zero or more columns are designated as the unique set of values
(see 3.2.5.4).


For Object tables:

a. A newly created table is initially empty and rows SHALL be created using the CreateRow
method, before they are usable.
b. There is always a UID column of type UID. In object tables, rows are addressed by UID.


3.2.5.2 Objects

Begin Informative Content

An object is any row of an object table. The particular object type is defined by the object table in which
the object occurs. The columns of the object table define the contents of each object in it.

For a specific SP, there are methods on the SP itself, methods that act on the tables and have the
whole table as their possible scope, and methods for each of the objects within the SP. Object-specific
ACLs are applied to the methods capable of manipulating that objectÕs data (see 3.4.2).

End Informative Content

3.2.5.3 Unique HalfUIDentifiers (UIDs)

Each object table has a column named UID. This column contains an 8-byte unique identifier for that
row. Each row has an SP-wide unique value in this column. This value is never shared with another
row, and is never reused by that SP. The TPer SHALL guarantee that UIDs are unique across the
entire SP anytime that a UID is generated, and that UIDs SHALL NOT be re-used even if an object is
deleted and the UID is no longer in use.

The UID column is present to provide anti-spoofing capability, and to provide a means to address these
rows. New UIDs are assigned when rows are created and old values are discarded when rows are
deleted. If all UIDs have been used, no more rows are able to be created.

Each table is also represented by a UID. A tableÕs UID is derived from the UID of that table in the
Table table. The Table table is an object table in which each row is a table descriptor object that
stores metadata about the associated table.

The bytes in a UID SHALL be utilized as follows:

a. The first four bytes of a table rowÕs UID SHALL be the Òcontaining tableÓ portion of the UID
and the last four bytes SHALL be assigned in a TPer-specific manner.
b. UIDs of tables SHALL be assigned as follows:
i. The UIDs of table descriptor objects (the table's row in the Table table) SHALL be
0x00 0x00 0x00 0x01 XX XX XX XX, where XX XX XX XX represents the values
assigned by the TPer to that objectÕs UID, or assigned by this specification or an








SSC for pre-defined tables. For example, The Table table's UID SHALL be 0x00
0x00 0x00 0x01 0x00 0x00 0x00 0x01
ii. The UID used to reference the actual table (rather than that tableÕs row in the
Table table) SHALL be XX XX XX XX 0x00 0x00 0x00 0x00, where XX XX XX XX
are the last four bytes of the UID from that table's row in the Table table. Four
0x00Õs as the last four bytes of a UID that does not have four 0x00Õs at the
beginning are references to a table.
iii. All object UIDs SHALL have their high four bytes be the high four bytes of the
containing tableÕs UID. So, references to rows in a table are assigned UIDs based
on the UID of the containing table. For instance, references to the rows in table XX
XX XX XX 0x00 0x00 0x00 0x00 are assigned UIDs XX XX XX XX yy yy yy yy
where the first four bytes of the containing table UID and of the row are the same.







All UIDs with their first four bytes equal to 0x00 0x00 0x00 0x00 are reserved for use by the TCG and
SHALL NOT be assigned by the TPer.

When necessary to refer to the SP with a UID, as when an SP method is invoked, a UID of 0x00 0x00
0x00 0x00 0x00 0x00 0x00 0x01 is reserved to signify "this SP".

For each table defined in this specification, UIDs with last four bytes between 0x00 0x00 0x00 0x01
and 0x00 0x01 0x00 0x00 SHALL be reserved for use by the TCG.

A NULL UID reference is all zeroes (0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00). This is used to
indicate that no object is being referenced.

3.2.5.4 Unique Column Value Combinations

In addition to the UID column, an object table MAY also have one or more columns designated by the
host (for host-created tables) or by the specification (for tables specified in this document) as required
to be unique.

If a table has a column or set of columns defined as unique, then each row of the table SHALL have a
value or combination of values in the indicated column(s) that is unique within the table for those
column values. When more than one column is marked as participating in this uniqueness requirement,
each of these columns participate in the unique value ("multi-column unique value").

The TPer is not required to keep rows of the table sorted by these unique values.




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


\begin{code}

newtype TableName = TableName ByteString
    deriving (Eq, Show)
instance IsString TableName where
    fromString = TableName . fromString

newtype TemplateName = TemplateName ByteString
    deriving (Eq, Show)
instance IsString TemplateName where
    fromString = TemplateName . fromString


data    TableDef     = TableDef HalfUID TableName TemplateName
    deriving (Eq, Show)

tdef ::
    Word8 -> Word8 -> Word8 -> Word8
 -> TableName
 -> TemplateName
 -> TableDef
tdef b3 b2 b1 b0 ta te = TableDef (HalfUID b3 b2 b1 b0) ta te

-- | From Table 240
tableDefs :: [TableDef]
tableDefs = --    HalfUID        TableName        TemplateName
    [ tdef  0x00 0x00 0x00 0x01  "Table"          "Base"
    , tdef  0x00 0x00 0x00 0x02  "SPInfo"         "Base"
    , tdef  0x00 0x00 0x00 0x03  "SPTemplates"    "Base"
    , tdef  0x00 0x00 0x00 0x04  "Column"         "Base"
    , tdef  0x00 0x00 0x00 0x05  "Type"           "Base"
    , tdef  0x00 0x00 0x00 0x06  "MethodID"       "Base"
    , tdef  0x00 0x00 0x00 0x07  "AccessControl"  "Base"
    , tdef  0x00 0x00 0x00 0x08  "ACE"            "Base"
    , tdef  0x00 0x00 0x00 0x09  "Authority"      "Base"
    , tdef  0x00 0x00 0x00 0x0A  "Certificates"   "Base"
    , tdef  0x00 0x00 0x00 0x0B  "C_PIN"          "Base"
    , tdef  0x00 0x00 0x00 0x0C  "C_RSA_1024"     "Base"
    , tdef  0x00 0x00 0x00 0x0D  "C_RSA_2048"     "Base"
    , tdef  0x00 0x00 0x00 0x0E  "C_AES_128"      "Base"
    , tdef  0x00 0x00 0x00 0x0F  "C_AES_256"      "Base"
    , tdef  0x00 0x00 0x00 0x10  "C_EC_160"       "Base"
    , tdef  0x00 0x00 0x00 0x11  "C_EC_192"       "Base"
    , tdef  0x00 0x00 0x00 0x12  "C_EC_224"       "Base"
    , tdef  0x00 0x00 0x00 0x13  "C_EC_256"       "Base"
    , tdef  0x00 0x00 0x00 0x14  "C_EC_384"       "Base"
    , tdef  0x00 0x00 0x00 0x15  "C_EC_521"       "Base"
    , tdef  0x00 0x00 0x00 0x16  "C_EC_163"       "Base"
    , tdef  0x00 0x00 0x00 0x17  "C_EC_233"       "Base"
    , tdef  0x00 0x00 0x00 0x18  "C_EC_283"       "Base"
    , tdef  0x00 0x00 0x00 0x19  "C_HMAC_160"     "Base"
    , tdef  0x00 0x00 0x00 0x1A  "C_HMAC_256"     "Base"
    , tdef  0x00 0x00 0x00 0x1B  "C_HMAC_384"     "Base"
    , tdef  0x00 0x00 0x00 0x1C  "C_HMAC_512"     "Base"
    , tdef  0x00 0x00 0x00 0x1D  "SecretProtect"  "Base"
    , tdef  0x00 0x00 0x02 0x01  "TPerInfo"       "Admin"
    , tdef  0x00 0x00 0x02 0x03  "CryptoSuite"    "Admin"
    , tdef  0x00 0x00 0x02 0x04  "Template"       "Admin"
    , tdef  0x00 0x00 0x02 0x05  "SP"             "Admin"
    , tdef  0x00 0x00 0x04 0x01  "ClockTime"      "Clock"
    , tdef  0x00 0x00 0x06 0x01  "H_SHA_1"        "Crypto"
    , tdef  0x00 0x00 0x06 0x02  "H_SHA_256"      "Crypto"
    , tdef  0x00 0x00 0x06 0x03  "H_SHA_384"      "Crypto"
    , tdef  0x00 0x00 0x06 0x04  "H_SHA_512"      "Crypto"
    , tdef  0x00 0x00 0x0A 0x01  "Log"            "Log"
    , tdef  0x00 0x00 0x0A 0x02  "LogList"        "Log"
    , tdef  0x00 0x00 0x08 0x01  "LockingInfo"    "Locking"
    , tdef  0x00 0x00 0x08 0x02  "Locking"        "Locking"
    , tdef  0x00 0x00 0x08 0x03  "MBRControl"     "Locking"
    , tdef  0x00 0x00 0x08 0x04  "MBR"            "Locking"
    , tdef  0x00 0x00 0x08 0x05  "K_AES_128"      "Locking"
    , tdef  0x00 0x00 0x08 0x06  "K_AES_256"      "Locking"
    ]

instance Arbitrary TableDef where
    arbitrary = elements tableDefs


errorNoTableDef :: (Show s) => s -> e
errorNoTableDef = error . ("Could not find table def for " <>) . show

tableHalfUIDFromName :: TableName -> HalfUID
tableHalfUIDFromName n = case find (\(TableDef _ n' _) -> n == n' ) tableDefs of
  Just (TableDef h _ _) ->h
  Nothing -> errorNoTableDef n

tableNameFromHalfUID :: HalfUID -> TableName
tableNameFromHalfUID h = case find (\(TableDef h' _ _) -> h == h') tableDefs of
  Just (TableDef _ n _) -> n
  Nothing -> errorNoTableDef h

tableDescriptorObjectUIDFromHalfUID :: HalfUID -> UID
tableDescriptorObjectUIDFromHalfUID h = UID hTable h

tableUIDFromHalfUID :: HalfUID -> UID
tableUIDFromHalfUID h = UID h hNull

-- | Table UID for table
tableUIDFromName :: TableName -> UID
tableUIDFromName = tableUIDFromHalfUID . tableHalfUIDFromName

-- | Row UID for the description of a table as a row in the Table Table
tableDescriptorObjectUIDFromName :: TableName -> UID
tableDescriptorObjectUIDFromName = tableDescriptorObjectUIDFromHalfUID . tableHalfUIDFromName


-- * HalfUIDs used to identify tables in Table UIDs and
-- Table Table Descriptor Object UIDs (Table Table row UIDs)

hTable :: HalfUID
hTable = tableHalfUIDFromName "Table"


-- * Table UIDs
--
uTableTable :: UID
uTableTable = tableUIDFromName "Table"

uSPInfoTable :: UID
uSPInfoTable = tableUIDFromName "SPInfo"

uSPTemplatesTable :: UID
uSPTemplatesTable = tableUIDFromName "SPTemplates"

uColumnTable :: UID
uColumnTable = tableUIDFromName "Column"

uTypeTable :: UID
uTypeTable = tableUIDFromName "Type"

uMethodIDTable :: UID
uMethodIDTable = tableUIDFromName "MethodID"

uAccessControlTable :: UID
uAccessControlTable = tableUIDFromName "AccessControl"

uACETable :: UID
uACETable = tableUIDFromName "ACE"

uAuthorityTable :: UID
uAuthorityTable = tableUIDFromName "Authority"

uCertificatesTable :: UID
uCertificatesTable = tableUIDFromName "Certificates"

uC_PINTable :: UID
uC_PINTable = tableUIDFromName "C_PIN"

uC_RSA_1024Table :: UID
uC_RSA_1024Table = tableUIDFromName "C_RSA_1024"

uC_RSA_2048Table :: UID
uC_RSA_2048Table = tableUIDFromName "C_RSA_2048"

uC_AES_128Table :: UID
uC_AES_128Table = tableUIDFromName "C_AES_128"

uC_AES_256Table :: UID
uC_AES_256Table = tableUIDFromName "C_AES_256"

uC_EC_160Table :: UID
uC_EC_160Table = tableUIDFromName "C_EC_160"

uC_EC_192Table :: UID
uC_EC_192Table = tableUIDFromName "C_EC_192"

uC_EC_224Table :: UID
uC_EC_224Table = tableUIDFromName "C_EC_224"

uC_EC_256Table :: UID
uC_EC_256Table = tableUIDFromName "C_EC_256"

uC_EC_384Table :: UID
uC_EC_384Table = tableUIDFromName "C_EC_384"

uC_EC_521Table :: UID
uC_EC_521Table = tableUIDFromName "C_EC_521"

uC_EC_163Table :: UID
uC_EC_163Table = tableUIDFromName "C_EC_163"

uC_EC_233Table :: UID
uC_EC_233Table = tableUIDFromName "C_EC_233"

uC_EC_283Table :: UID
uC_EC_283Table = tableUIDFromName "C_EC_283"

uC_HMAC_160Table :: UID
uC_HMAC_160Table = tableUIDFromName "C_HMAC_160"

uC_HMAC_256Table :: UID
uC_HMAC_256Table = tableUIDFromName "C_HMAC_256"

uC_HMAC_384Table :: UID
uC_HMAC_384Table = tableUIDFromName "C_HMAC_384"

uC_HMAC_512Table :: UID
uC_HMAC_512Table = tableUIDFromName "C_HMAC_512"

uSecretProtectTable :: UID
uSecretProtectTable = tableUIDFromName "SecretProtect"

uTPerInfoTable :: UID
uTPerInfoTable = tableUIDFromName "TPerInfo"

uCryptoSuiteTable :: UID
uCryptoSuiteTable = tableUIDFromName "CryptoSuite"

uTemplateTable :: UID
uTemplateTable = tableUIDFromName "Template"

uSPTable :: UID
uSPTable = tableUIDFromName "SP"

uClockTimeTable :: UID
uClockTimeTable = tableUIDFromName "ClockTime"

uH_SHA_1Table :: UID
uH_SHA_1Table = tableUIDFromName "H_SHA_1"

uH_SHA_256Table :: UID
uH_SHA_256Table = tableUIDFromName "H_SHA_256"

uH_SHA_384Table :: UID
uH_SHA_384Table = tableUIDFromName "H_SHA_384"

uH_SHA_512Table :: UID
uH_SHA_512Table = tableUIDFromName "H_SHA_512"

uLogTable :: UID
uLogTable = tableUIDFromName "Log"

uLogListTable :: UID
uLogListTable = tableUIDFromName "LogList"

uLockingInfoTable :: UID
uLockingInfoTable = tableUIDFromName "LockingInfo"

uLockingTable :: UID
uLockingTable = tableUIDFromName "Locking"

uMBRControlTable :: UID
uMBRControlTable = tableUIDFromName "MBRControl"

uMBRTable :: UID
uMBRTable = tableUIDFromName "MBR"

uK_AES_128Table :: UID
uK_AES_128Table = tableUIDFromName "K_AES_128"

uK_AES_256Table :: UID
uK_AES_256Table = tableUIDFromName "K_AES_256"


\end{code}
\end{document}
