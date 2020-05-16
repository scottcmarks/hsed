\documentstyle{article}
\begin{document}
\chapter{Table}

Table


\begin{code}
{-|
Module      : System.SED.MCTP.Common.Table
Description : SED table datatype
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Table

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.SED.MCTP.Common.Table where

import           RIO

import           System.SED.MCTP.Common.UID

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

A table name or table column name MAY be up to 32 bytes in length. By convention, the names
assigned in this document consist of ASCII characters, the first of which is a letter and others are
letters, digits or underscores. Adjacent underscores do not occur. All names are case sensitive.

Within an SP, tables MAY be created and deleted. For each table, rows MAY be created and deleted
(except within a Byte table Ð see 3.2.5.1), but columns are created only when the table is created.
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

a. Byte table. Byte tables provide raw data storage. A byte table has one unnamed column
of type bytes_1. The address of the first row in a byte table is 0. Upon creation, the value
of all cells in a byte table SHALL be 0x00. The rows of a byte table SHALL NOT be
allocated or freed (i.e. via CreateRow or DeleteRow). Byte table rows are addressed by
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

3.2.5.3 Unique Identifiers (UIDs)

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

\begin{code}

newtype TableName    = TableName    ByteString deriving (Eq, IsString)
instance Show TableName where show (TableName bs) = show bs

newtype TemplateName = TemplateName ByteString deriving (Eq, IsString)
instance Show TemplateName where show (TemplateName bs) = show bs

data Table = Table { _tableName    :: TableName
                   , _tableHalfUID :: HalfUID
                   , _tableColumns :: [TableColumn]
                   , _tableRows    :: [TableRow]
                   }
    deriving (Eq, Show)

newtype TableRow = TableRow ()
    deriving (Eq, Show)

\end{code}












5.3.2.3.1 UID

This is the unique identifier of this row of the Table table.

This column SHALL NOT be modifiable by the host.

5.3.2.3.2 Name

This is the name of the table.

This column SHALL NOT be modifiable by the host for tables that are created during issuance.

5.3.2.3.3 CommonName

This is a name that MAY be shared among multiple table descriptor objects.

This column SHALL NOT be modifiable by the host for tables that are created during issuance.

5.3.2.3.4 TemplateID

In the Admin SP, this column is used to identify the template to which this table belongs and indicates a
table that is not present in the Admin SP. In SPs other than the Admin SP, the value of this column
SHALL be zeroes. See 5.4.4.1 for details.

This column SHALL NOT be modifiable by the host.

5.3.2.3.5 Kind

This value indicates the type of table Ð either object or byte.

This column SHALL NOT be modifiable by the host.

5.3.2.3.6 Column

This is a reference to the Column table row of this tableÕs first column. For byte tables this value SHALL
be the null uid.

This column SHALL NOT be modifiable by the host.

5.3.2.3.7 NumColumns

This value indicates the number of columns in the table. For byte tables this SHALL be 0x01.

This column SHALL NOT be modifiable by the host.

5.3.2.3.8 Rows

This value indicates the actual number of rows that have been created for the table.

This column SHALL NOT be modifiable by the host.


5.3.2.3.9 RowsFree

This value indicates the number of unused rows in the table out of those allocated for use.

This column SHALL NOT be modifiable by the host.

5.3.2.3.10 RowBytes

This value is the number of bytes in each row of the table. This is the total number of bytes utilized by
each table row, and SHALL include bytes devoted to overhead for system columns, type identification,
etc.

This column SHALL NOT be modifiable by the host.

5.3.2.3.11 LastID

For object tables, this value is the most recent uid assigned to an object in that table. For byte tables,
this value SHALL be the null uid.

This column SHALL NOT be modifiable by the host.

5.3.2.3.12 MinSize

This is the number of rows initially requested for this table. The table is able to contain at least this
many rows. This column is user-settable (access control permitting). For more information see
5.3.4.2.1.

5.3.2.3.13 MaxSize

This is a host-defined maximum number of rows that MAY exist in this table. The table SHALL never
have more than this many rows, although the TPer is not required to guarantee that the table can grow
to MaxSize rows.

This column is user-settable (access control permitting), but the TPer MAY prevent the value in this
column from being changed. A value of 0 indicates no host-defined limit of rows that MAY be created
in this table.





                 Table 171 Table Table Description
    +--------------+------------+---------+--------------------+
    |Column Number |Column Name |IsUnique |Column Type         |
    +--------------+------------+---------+--------------------+
    |0x00          |UID         |         |uid                 |
    +--------------+------------+---------+--------------------+
    |0x01          |Name        |Yes      |name                |
    +--------------+------------+---------+--------------------+
    |0x02          |CommonName  |Yes      |name                |
    +--------------+------------+---------+--------------------+
    |0x03          |TemplateID  |Yes      |Template_object_ref |
    +--------------+------------+---------+--------------------+
    |0x04          |Kind        |         |table_kind          |
    +--------------+------------+---------+--------------------+
    |0x05          |Column      |         |Column_object_ref   |
    +--------------+------------+---------+--------------------+
    |0x06          |NumColumns  |         |uinteger_4          |
    +--------------+------------+---------+--------------------+
    |0x07          |Rows        |         |uinteger_4          |
    +--------------+------------+---------+--------------------+
    |0x08          |RowsFree    |         |uinteger_4          |
    +--------------+------------+---------+--------------------+
    |0x09          |RowBytes    |         |uinteger_4          |
    +--------------+------------+---------+--------------------+
    |0x0A          |LastID      |         |uid                 |
    +--------------+------------+---------+--------------------+
    |0x0B          |MinSize     |         |uinteger_4          |
    +--------------+------------+---------+--------------------+
    |0x0C          |MaxSize     |         |uinteger_4          |
    +--------------+------------+---------+--------------------+


\begin{code}


data TableColumn = TableColumn { _columnNumber   :: Int
                               , _columnName     :: ByteString -- FIXME
                               , _columnIsUnique :: Bool
                               , _columnType     :: ColumnType
                               }
    deriving (Eq, Show)

newtype ColumnType = ColumnType ()
    deriving (Eq, Show)

-- | This is the object descriptor for the Table Table in the Columns Table,
--   which is to say it is that row of the Columns Table that describes
--   the columns of the Table Table.


\end{code}
\end{document}
