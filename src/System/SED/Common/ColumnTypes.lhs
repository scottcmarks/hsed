\documentstyle{article}
\begin{document}
\chapter{ColumnTypes}

Table column types.


\begin{code}
{-|
Module      : System.SED.Common.ColumnTypes
Description : SED tokens
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Table column types.

-}

{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}


module System.SED.Common.ColumnTypes where

import           Control.Monad.Trans.Reader       (Reader)
import           Control.Monad.Reader.Class       (asks)

import           Data.Foldable                    (elem)
import           Data.Functor                     ((<$>))
import           Data.Maybe                       (maybe)
import           Data.Set                         (Set)
import           Data.String                      (IsString(..))

import           GHC.Base                         (id, otherwise, pure,
                                                   ($), (.), (>>=))
import           GHC.Classes                      (Eq(..), Ord(..))
import           GHC.Enum                         (Enum(..))
import           GHC.List                         (map)
import           GHC.Maybe                        (Maybe(..))
import           GHC.Natural                      (Natural(..))
import           GHC.Read                         (Read(..))
import           GHC.Show                         (Show(..))
import           GHC.Types                        (Bool(..),Int(..))
import           GHC.TypeNats                     (KnownNat)

import           Test.QuickCheck                  () -- FIXME


import           Extras.Bytes                     (Fixed_bytes)

import           System.SED.Common.ColumnTypes.TH (ttype)
import           System.SED.Common.TableUIDs      () -- FIXME
import           System.SED.Common.UID            (UID, uidUpper, uidLower)

\end{code}


5.1.1 Column Types Overview

The following are the primitive data types used for column types as defined by the specification. How
these primitive values are stored in a table cell is implementation dependent.

a. integer. Signed integer. To differentiate among the type sizes, a size identifier is specified
with the type, i.e., a one-byte integer is denoted as integer_1, etc.
b. uinteger. Unsigned integer. To differentiate among the type sizes, a size identifier is
specified with the type, i.e. a one-byte integer is denoted as uinteger_1, etc.
c. bytes. A fixed size sequence of bytes that is used to represent any type of data such as
strings, blobs, bit vectors, time/dates, etc. To differentiate among the type sizes, a size
identifier is specified with the type, i.e. a one-byte bytes type is denoted as bytes_1, etc.
d. bytes{max=n}. A variable size sequence of bytes. To differentiate among the type sizes, a
size identifier is specified with the type, i.e. a one-byte max bytes type is denoted as
max_bytes_1, etc. Invocation of the Get method on a table cell with this type of value
SHALL return the exact sequence of bytes, with the same token length, as was originally
set.


The value of a Type object's Format column SHALL indicate the structure and required values of that
type. The parsing of the value of this column is defined in a general manner using the following rules in
ABNF (see [9]). Additional specific information is provided after the notation. In the Format column, the
Format code and the table_kind value SHALL be encoded as a uinteger_2. All other values are
encoded as indicated.

Type = Base_Type / Simple_Type / Enumeration_Type / Alternative_Type / List_Type /
Restricted_Reference_Type / General_Reference_Type / Named_Value_Type / Struct_Type
/ Set_Type



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



a. Base_Type. The Base_Type format describes the most basic types. Other types are
created using the Base Types as building blocks. The Base Types SHALL NOT be used
directly. Base Types SHALL always have a Size column value of 0 in the Type table.



    a. 0 Ð this is the Format code indicating that this is a Base_Type.
\begin{code}

{-
Type
    Base_Type                    =  0
    Simple_Type                  =  1 bytes_8 uinteger_2
    Enumeration_Type             =  2 1*(uinteger_2 uinteger_2)
    Alternative_Type             =  3 2*bytes_8
    List_Type                    =  4 uinteger_2 bytes_8
    Restricted_Reference_Type{5} =  5 1*bytes_8
    Restricted_Reference_Type{6} =  6 1*bytes_8
    General_Reference_Type{7}    =  7
    General_Reference_Type{8}    =  8
    General_Reference_Type{9}    =  9
    General_Reference_Table_Type = 10 table_kind
    Named_Value_Name_Type        = 11 1*32bytes bytes_8
    Name_Value_Integer_Type      = 12 integer_2 bytes_8
    Name_Value_Uinteger_Type     = 13 uinteger_2 bytes_8
    Struct_Type                  = 14 1*bytes_8
    Set_Type                     = 15 1*(uinteger_2 uinteger_2)
-}

\end{code}

b. Simple_Type. The Simple_Type format defines an instance of one of the Base_Type
types. The Simple_Type always includes a uinteger in the format column, which defines
the size for that instance of that Simple_Type.
a. 1 Ð this is the Format code indicating that this is a Simple_Type.
b. bytes_8 Ð this SHALL be a uidref to a Type object that is a Base_Type.
c. uinteger_2 Ð this is the size of this instantiation of the Base_Type.



c. Enumeration_Type. This is a n unsigned integer in a specific range.
a. 2 Ð this is the Format code indicating this is an Enumeration_Type.
b. 1*(uinteger_2 uinteger_2) Ð this is a number of pairs of values of uinteger_2 that
represent the supported ranges of values in the enumeration.





If a non-contiguous range of values is supported, the Format column SHALL contain a
number of uinteger_2 pairs to identify all of the supported values.

a. An invocation of the CreateRow method SHALL contain only a single pair of uinteger_2
values.
b. Pseudo-code example: enum {0..2} represents a range of 0 to 2 inclusive.
d. Alternative_Type. This is a value that SHALL be an element of one of the specified types.
The Alternative_Type format defines a union with the uinteger specifying the number of
member types and followed by that many uidref{TypeObjectUID} references to the member
types.
a. 3 Ð this is the Format code indicating that this is an Alternative_Type
b. 2*bytes_8 Ð this is a number of 2 or more uidrefs to different Type objects, other than
Base_Types, that identify the options available for this type.





Pseudo-code example: typeOr{boolean,uinteger_4,bytes_7}

e. List_Type. This is a sequence of values of the same type. The maximum number of
elements is specified. The elements of the list are not required to be provided in any
specified order. The elements of the list SHALL be returned to the host (with the Get
method, for example) in the order in which they were received by the TPer.
a. 4 Ð this is the format code indicating that this is a List_Type.
b. uinteger_2 Ð this is the maximum number of elements that make up the list.
c. bytes_8 Ð this SHALL be a uidref to a Type object, other than a Base_Type, that
indicates the type of the elements of the list.





Pseudo-code example: list[10]{boolean} is a list of boolean values, with a maximum of 10
elements.

f. Restricted_Reference_Type. A reference to a row SHALL be contained in a specific table
or group of tables. The reference is to a physical row number (5) or a UID (6) within the
table. The value of a ref is the uinteger row number for a byte table. The value of a uidref is
a UID from the UID column of an object table. A uidref value of 0x00 0x00 0x00 0x00
0x00 0x00 0x00 0x00, called the NULL UID, serves as a Ònull pointerÓ.
a. 5/6 Ð these are the format codes indicating that this is a Restricted_Reference_Type
i. 5 Ð this format code indicates that this type SHALL be the row number
contained in one of the indicated byte tables.









ii. 6 Ð this format code indicates that this type SHALL be the UID of an object
contained in one of the indicated object tables.



b. 1*bytes_8 Ð this is 1 or more UIDs that SHALL be to different Table UIDs that identify
the tables within which the row number or uidref SHALL exist.





In this example, TableName is the name of the referenced table:

a. Pseudo-code example: uidref{ <TableName>ObjectUID }
b. Pseudo-code example: ref{ <TableName>ObjectUID }
g. General_Reference_Type. This is a reference to a row of some byte table, to the UID of
some object, or to the UID of some table. The General_Reference_Type format defines a
physical row number of a byte table (7), a uid of some object (8), or a uid of some table (9).
The UID reserved to represent Òthis SPÓ is encompassed by a General_Reference_Type
of 8. The value of a ref is the uinteger row number for a byte table. The value of a uidref is
a UID from the UID column of an object table. A uidref value of 0x00 0x00 0x00 0x00
0x00 0x00 0x00 0x00, called the NULL UID, serves as a Ònull pointerÓ.
a. 7/8/9 Ð these are the format codes indicating that this is a General_Reference_Type.
i. 7 Ð this format code indicates that this type SHALL be the physical row number
of a byte table.
ii. 8 Ð this format code indicates that this type SHALL be the UID of some object.
iii. 9 Ð this format code indicates that this type SHALL be the UID of some table.








Pseudo-code example: uidref{*}

Pseudo-code example: ref{*}

h. General_Reference_Table_Type. This is a reference to a a specific kind of table, either
byte or object.
a. 10 Ð this format code indicates that this is a General_Reference_Table_Type.
b. table_kind (1/2) Ð this identifies whether the type value is the UID of an object table or
the UID of a byte table.
i. 1 Ð this table_kind value indicates that the type value SHALL be the UID of an
object table.
ii. 2 Ð this table_kind value indicates that type the SHALL be the UID of a byte
table.






i. Named_Value_Name_Type. This is a Named value pair where the Name in the pair is a
max_bytes_32, and the value is a uidref to the required type of the value.
a. 11 Ð this format code indicates that this is a Named_Value_Name_Type.
b. max_bytes_32 Ð this is a string with a maximum length of 32 characters that SHALL be
the name submitted with the value.
c. bytes_8 Ð this SHALL be a uidref to a Type object, other than a Base_Type, that
indicates the type of the value to be submitted.



j. Named_Value_Integer_Type. This is a Named value pair where the Name in the pair is
an integer_2, and the value is a uidref to the required type of the value.
a. 12 Ð this format code indicates that this is a Named_Value_Name_Type.
b. integer_2 Ð this is a signed integer that SHALL be the name submitted with the value.
c. bytes_8 Ð this SHALL be a uidref to a Type object, other than a Base_Type, that
indicates the type of the value to be submitted.






k. Named_Value_Uinteger_Type. This is a Named value pair where the Name in the pair is
a uinteger_2, and the value is a uidref to the required type of the value.
a. 13 Ð this format code indicates that this is a Named_Value_Name_Type.
b. uinteger_2 Ð this is an unsigned integer that SHALL be the name submitted with the
value.
c. bytes_8 Ð this SHALL be a uidref to a Type object, other than a Base_Type, that
indicates the type of the value to be submitted.



l. Struct_Type. This is a combination of different Named value types. The Struct_Type
format indicator is followed by the number of elements and then uidrefs to the rows in the
Type table that represent each of those elements. Name-value pairs in structs represent
optional components. These MAY be excluded when passing that struct as a method
parameter. When used as a column type, the size SHALL account for inclusion of all of a
struct's components.
a. 14 Ð this format code indicates that this is a Struct_Type
b. 1*bytes_8 Ð this is a number of uidrefs to different Type objects, other than
Base_Types, that identify the components of this type.





Named value types in a struct SHALL all be different uidrefs and SHALL all be defined to
utilize different names.

If an element of a Struct is supplied when the Struct is referenced (for instance, in a method
parameter), then that element SHALL appear in the order identified for that Struct in the
Type table.

For a Struct made up of Named value parameters A, B, C, and D, if the Struct is
referenced, as in a method parameter, if element A is supplied then it SHALL be supplied
first within the Struct. Other correct element orderings include:

a. ExampleStruct [ A, C, D ]
b. ExampleStruct [ B, D ]
c. ExampleStruct [ A, D ]


Invalid element orderings include:

a. ExampleStruct [ D, C ]
b. ExampleStruct [ B, C, A ]
c. ExampleStruct [ B, A, D, C ]
m. Set_Type. A set of unsigned integers in a specific range. The Set_Type format defines the
range of the valid elements of the set, where the first integer is the start value of the valid
elements of the set and the second integer is the end value. The type itself is not limited to
only a single selection from among the choices defined, as in the Enumeration_Type. The
Set_Type provides the host the ability to select more than one of the options. Each SHALL
appear only once in the Set. The Set MAY hold any amount of selections, from zero to the
number of selections.
a. 15 Ð this format code indicates that this is a Set_Type
b. 1*(uinteger_2 uinteger_2) Ð this is a number of pairs of values of uinteger_2 that
represent the supported ranges of values in the set.
i. If a non-contiguous range of values is supported, the Format column SHALL
contain a number of uinteger_2 pairs to identify all of the supported values.









ii. An invocation of the CreateRow method SHALL contain only a single pair of
uinteger_2 values.








Pseudo-code example: Set{0..2} Ð Valid values for this set are made up of the following =
{}, {0}, {1}, {2}, {0,1}, {0,2}, {1,2}, {0,1,2}.

5.1.2 Types Encoding

Certain column types used in messaging as method parameters (particularly in the Set method) utilize
the interface grouping mechanisms (Named and List values) to provide clarity regarding the scope of
the transmitted values.

a. Simple types Ð values of this type require no special handling in the messaging stream.
b. Enumeration types Ð values of this type require no special handling in the messaging
stream.
c. Alternative types Ð values of this type are encoded in Get and Set methods as follows:
a. The Alternative column type is handled similarly to a Named value in a parameter list.
The Named value grouping tokens are used (SN and EN tokens, which represent
"StartName" and "EndName" respectively). The Name for the pair is the last four bytes
of the UID ("half_uid") of the value's Type object. The value in the Named value is the
value of the option being set to or retrieved from the column.





Example: When setting a 16-byte key value to the Key column of the K_AES_128 table, the
value would be encoded as:

F2 A400000202 D010000102030405060708090A0B0C0D0E0F F3

d. List type Ð values of this type are encoded as follows:
a. The List column type is handled in the same way a parameter list is handled, by using
the interface List value grouping tokens (F0 and F1 tokens, which represent "[" and "]"
respectively) to enclose the values in the list.





Example: F0 tokenized_value tokenized _value tokenized _value F1

e. Restricted Reference types Ð values of this type require no special handling in the
messaging stream.
f. General Reference types Ð values of this type require no special handling in the messaging
stream.
g. Named value types Ð values of this type are encoded as follows:
a. Values of this type are handled in the same way a Named value in a parameter list is
handled, by using the Named value grouping tokens (SN and EN tokens, which
represent "StartName" and "EndName" respectively) to enclose the name-value pair.





Example: F2 tokenized_name tokenized_value F3

h. Struct value types Ð Structs allow the creation of composite types by combining Named
value types and other types. Values of the struct type are made up of either optional
Named value types, or other types that are required to be supplied. The optional types
MAY NOT be included when sending values for a struct. Values of this type are encoded
as follows:
a. The struct itself is delimited using the List value grouping tokens (F0 and F1 tokens,
which represent "[" and "]" respectively) to enclose the values in the struct. The Named
values that make up the values stored in the struct are each grouped using the
interface Named value-grouping tokens (SN and EN tokens, which represent
"StartName" and "EndName" respectively) to enclose each name-value pair.






Example: F0 F2 tokenized_name tokenized_value F3 F2 tokenized_name tokenized_value
F3 F1

i. Set value types Ð values of this type are encoded as follows:
a. The Set column type is handled in the same way that the List type is handled, by using
the interface List value grouping tokens (F0 and F1 tokens, which represent "[" and "]"
respectively) to enclose the values in the Set.





Example: F0 tokenized_value tokenized_value F1


--------------------------------------------snip--------------------------------------------
--------------------------------------------------------------------------------
text

                Table foo
    +-----------------+-------------------+
    |Enumeration Value|Associated Value   |
    +-----------------+-------------------+
    |0                |                   |
    +-----------------+-------------------+
    |1                |                   |
    +-----------------+-------------------+
    |2                |                   |
    +-----------------+-------------------+
    |3                |                   |
    +-----------------+-------------------+

--------------------------------------------------------------------------------
section

text

\begin{code}

[ttype|

                     Table etc.
    +-----------------------+----------+--------------+
    |UID                    |Name      |Format        |
    +-----------------------+----------+--------------+
    |00 00 00 00 00 00 00 00|example'  |Core_Type,    |
    |                       |          |count,        |
    |                       |          |options       |
    +-----------------------+----------+--------------+

|]

\end{code}
--------------------------------------------------------------------------------
5.1.3.80 Type_object _ref

The Type_object _ref type describes a uidref to an object in the Type table.

Table 144 Type_object _ref


UID

Name

Format

00 00 00 05 00 00 0C 02

Type_object_ref

Restricted_Reference_Type{6},
uidref {TypeTableUID}
--------------------------------------------------------------------------------
5.1.3.81 uid

This is the type used for the UID column of object tables.

Table 145 uid


UID

Name

Format

00 00 00 05 00 00 02 09

uid

Simple_Type,
bytes,
8
--------------------------------------------------------------------------------
5.1.3.82 uinteger

This is the base type that is used to represent an unsigned integer.

Table 146 uinteger


UID

Name

Format

00 00 00 05 00 00 00 05

uinteger

Base_Type
--------------------------------------------------------------------------------
5.1.3.83 uinteger_1

This is a uinteger type with a size restriction of 1 byte.

Table 147 uinteger_1


UID

Name

Format

00 00 00 05 00 00 02 11

uinteger_1

Simple_Type,

uinteger,
1
--------------------------------------------------------------------------------
5.1.3.84 uinteger_128

This is a uinteger type with a size restriction of 128 bytes.

Table 148 uinteger_128


UID

Name

Format

00 00 00 05 00 00 02 12

uinteger_128

Simple_Type,

uinteger,
128
--------------------------------------------------------------------------------
5.1.3.85 uinteger_2

This is a uinteger type with a size restriction of 2 bytes.

Table 149 uinteger_2


UID

Name

Format

00 00 00 05 00 00 02 15

uinteger_2

Simple_Type,

uinteger,
2
--------------------------------------------------------------------------------
5.1.3.86 uinteger_20

This is a uinteger type with a size restriction of 20 bytes.

Table 150 uinteger_20


UID

Name

Format

00 00 00 05 00 00 02 16

uinteger_20

Simple_Type,

uinteger,
20
--------------------------------------------------------------------------------
5.1.3.87 uinteger_21

This is a uinteger type with a size restriction of 21 bytes.

Table 151 uinteger_21


UID

Name

Format

00 00 00 05 00 00 02 17

uinteger_21

Simple_Type,

uinteger,
21
--------------------------------------------------------------------------------
5.1.3.88 uinteger_24

This is a uinteger type with a size restriction of 24 bytes.

Table 152 uinteger_24


UID

Name

Format

00 00 00 05 00 00 02 18

uinteger_24

Simple_Type,

uinteger,
24
--------------------------------------------------------------------------------
5.1.3.89 uinteger_256

This is a uinteger type with a size restriction of 256 bytes.

Table 153 uinteger_256


UID

Name

Format

00 00 00 05 00 00 02 19

uinteger_256

Simple_Type,

uinteger,
256
--------------------------------------------------------------------------------
5.1.3.90 uinteger_28

This is a uinteger type with a size restriction of 28 bytes.

Table 154 uinteger_28


UID

Name

Format

00 00 00 05 00 00 02 1A

uinteger_28

Simple_Type,

uinteger,
28
--------------------------------------------------------------------------------
5.1.3.91 uinteger_30

This is a uinteger type with a size restriction of 30 bytes.

Table 155 uinteger_30


UID

Name

Format

00 00 00 05 00 00 02 1B

uinteger_30

Simple_Type,

uinteger,
30
--------------------------------------------------------------------------------
5.1.3.92 uinteger_36

This is a uinteger type with a size restriction of 36 bytes.

Table 156 uinteger_36


UID

Name

Format

00 00 00 05 00 00 02 1F

uinteger_36

Simple_Type,

uinteger,
36
--------------------------------------------------------------------------------
5.1.3.93 uinteger_4

This is a uinteger type with a size restriction of 4 bytes.

Table 157 uinteger_4


UID

Name

Format

00 00 00 05 00 00 02 20

uinteger_4

Simple_Type,

uinteger,
4
--------------------------------------------------------------------------------
5.1.3.94 uinteger_48

This is a uinteger type with a size restriction of 48 bytes.

Table 158 uinteger_48


UID

Name

Format

00 00 00 05 00 00 02 23

uinteger_48

Simple_Type,

uinteger,
48
--------------------------------------------------------------------------------
5.1.3.95 uinteger_64

This is a uinteger type with a size restriction of 64 bytes.

Table 159 uinteger_64


UID

Name

Format

00 00 00 05 00 00 02 24

uinteger_64

Simple_Type,

uinteger,
64
--------------------------------------------------------------------------------
5.1.3.96 uinteger_66

This is a uinteger type with a size restriction of 66 bytes.

Table 160 uinteger_66


UID

Name

Format

00 00 00 05 00 00 02 27

uinteger_66

Simple_Type,

uinteger,
66
--------------------------------------------------------------------------------
5.1.3.97 uinteger_8

This is a uinteger type with a size restriction of 8 bytes.

Table 161 uinteger_8


UID

Name

Format

00 00 00 05 00 00 02 25

uinteger_8

Simple_Type,

uinteger,
8
--------------------------------------------------------------------------------

5.1.3.98 verify_mode

This enumeration type defines the verification operation the TPer SHALL perform during the re-
encryption process after a sector has been written with the new encryption key.

Table 162 verify_mode


UID

Name

Format

00 00 00 05 00 00 04 12

verify_mode

Enumeration_Type,
0,
7





The enumeration values are associated as defined in Table 163.

Table 163 verify_mode Enumeration Values


Enumeration Value

Associated Value

0

No verify

1

Verify enabled

2-7

Reserved
--------------------------------------------------------------------------------
5.1.3.99 Year

Name-value pair that has a Name of "0" and takes year_enum as the value.

Table 164 Year


UID

Name

Format

00 00 00 05 00 00 14 01

Year

Name_Value_Uinteger_Type,
0,
year_enum







5.1.3.100 year_enum

Used in association with the Year name-value pair.

Table 165 year_enum


UID

Name

Format

00 00 00 05 00 00 04 16

year_enum

Enumeration_Type,
1970,
9999
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- data Type = Base_Type
--           | Simple_Type
--               Fixed_bytes_8  -- actually uidref for Base_Type
--               Unsigned_Integer_2 -- size of the instantiation of the Base_Type
--           | Enumeration_Type
--               [(Unsigned_Integer_2, Unsigned_Integer_2)]
--           | Alternative_Type
--               [Fixed_bytes_8] -- actually uidrefs for non-Base_Type Type
--           | List_Type
--               Unsigned_Integer_2
--               Fixed_bytes_8  -- actually uidref for non-Base_Type Type of elements
--           | Restricted_Reference_Type_Byte_Table
--               [Fixed_bytes_8] -- actually Byte Table uidrefs
--           | Restricted_Reference_Type_Object_Table
--               [Fixed_bytes_8] -- actually Object Table uidrefs
--           | General_Reference_Type_Byte_Table_Row_Number
--           | General_Reference_Type_Object_UID
--           | General_Reference_Type_Table_UID
--           | General_Reference_Table_Type
--               Table_Kind -- uinteger*2 = 1 -> Object Table UID
--                                          2 -> Byte Table UID
--           | Named_Value_Name_Type
--               Max_Bytes_32 -- name
--               Fixed_bytes_8 -- actually uidref for non-Base_Type Type
--           | Named_Value_Integer_Type
--               Integer_2 -- name
--               Fixed_bytes_8 -- actually uidref for non-Base_Type Type
--           | Named_Value_Uinteger_Type
--               Uinteger_2 -- name
--               Fixed_bytes_8 -- actually uidref for non-Base_Type Type
--           | Struct_Type
--               [Fixed_bytes_8] -- actually uidrefs for non-Base_Type Type of elements
--           | Set_Type
--               [(Uinteger_2, Uinteger_2)] -- inclusive ranges of values


-- data Base_Type = Signed_Integer
--                | Unsigned_Integer
--                | Fixed_bytes Int
--                | Max_Bytes Int



So the thing is, this is where the entanglement of the Table Table, the Column Table, and the Type Table shows up.
The Table Table is described by a row in the Table Table, as are the other two.  The columns that define what that
row denotes are described by the linked list of Column entries, with the head of that list pointed to by the Column
Column in that row, whose type, Column_object_ref, is described by the Type Table entry pointed to by the Type Column
value of the Column row ...

Those Type Table entries start with the four Base_Type entries for integer, uinteger, bytes, and max_bytes.

There are fifteen combinators which are used to build up the remaining 96 Type Table entries.
The first, Simple_Type, is the only one which references a Base_Type, combining it with a two-byte integer to specify
a size (or in the case of max_bytes, max size).  The reference used in Simple_Type is called a byte_8 in the Core
Spec, whereas it is actually a Restricted_Reference_Type of the uidref (tag 6) variety.  This same or similar type
error is made in multiple combinator descriptions.

In addition to an expression built up by combining other types with the combinators, a type MAY have an attached predicate.
For example, the very first Column Type is
AC_element
  UID: 00 00 00 05 00 00 08 01
  Format: List_Type, *, ACE_expression
with the notes -- "The size of the AC_element list is implementation-dependant."  -- explains the "*"
               -- "A minimum size restriction MAY be defined by an SSC." -- indicates the need for a predicate hook.

The issues of implementation-dependant information and SSC-defined information would seem to call out for a Reader
monad with getters.  Even so, each such "*" needs to be translated as e.g. "AC_elementSize" to name the getter.



\begin{code}

type TPer a = Reader Env a

data Env = Env { core           :: Core
               , implementation :: Implementation
               , ssc            :: SSC
               }
    deriving (Show, Eq, Read)

data Core = Core
    deriving (Show, Eq, Read)

data Implementation = Implementation { __AC_elementSize :: Int
                                     , __max_columns    :: Int
                                     }
    deriving (Show, Eq, Read)

data SSC = SSC { __AC_elementMinSize :: Maybe Int
               , __min_max_columns   :: Maybe Int
               }
    deriving (Show, Eq, Read)

\end{code}

5.1.3 Column Types

This section describes each of the column types in the Template Reference sections of the Core
Specification. The UID, Name, and Format columns identify the column values of the Type table. These
values SHALL comprise the Type table for every SP, prior to any personalization. These types SHALL
NOT be able to be changed or deleted by the host.

Included in this section are descriptions of the column types for each column of each table defined in
this specification, as well as descriptions of each of the component types of the column types.
Component types are types that have entries in the Type table, but are not referenced directly as
column types. They are used to make up other types that do represent column types.

The UID column in the description table in each section SHALL be the UID for that type.

The Name column specifies the name for that type.

The Format column identifies the structure of the associated type. The first value in the Format column
is the name of that type's Format code. Additional values listed in the column are determined by the
type's format code. For readability, the names of Type objects are used in place of their UID, and
commas are used to separate values.

An asterisk (*) in any of the descriptive tables indicates SSC-specific or implementation-specific values.
\begin{code}

\end{code}
5.1.3.1 AC_element

An AC_element is a list type made up of ACE_expressions. The size of the AC_element list is
implementation-dependant. A minimum size restriction MAY be defined by an SSC.



+---------------------------------------------------------------+
|                      Table 46 AC_element                      |
+------------------------+----------+---------------------------+
|UID                     |Name      |Format                     |
+------------------------+----------+---------------------------+
|00 00 00 05 00 00 08 01 |AC_element|List_Type,                 |
|                        |          |*,                         |
|                        |          |ACE_expression             |
+------------------------+----------+---------------------------+



\begin{code}

-- newtype Core_AC_element = Core_AC_element [Core_ACE_expression]

-- uCore_AC_element' :: UID
-- uCore_AC_element' = uid 0x00 0x00 0x00 0x05 0x00 0x00 0x08 0x01


-- columnTypeNames :: [String]
-- columnTypeNames =
--     [
--         "AC_element"
--     ]

-- columnTypeUIDs :: [UID]
-- columnTypeUIDs =
--     [
--         uCore_AC_element'
--     ]

-- columnTypeName' :: Map UID String
-- columnTypeName' = fromList $ zip columnTypeUIDs columnTypeNames

-- columnTypeUID'  :: Map String UID
-- columnTypeUID'  = fromList $ zip columnTypeNames columnTypeUIDs

-- _AC_elementSize :: TPer Int
-- _AC_elementSize = __AC_elementSize <$> asks implementation

-- _AC_elementMinSize :: TPer (Maybe Int)
-- _AC_elementMinSize = __AC_elementMinSize <$> asks ssc

\end{code}
5.1.3.2 ACE_columns

This Set type identifies the columns to which an ACE applies. The values are: 0=Column0,
1=Column1, 2=Column2, etc. Each value in the set maps to a "Column Number". The size of the set is
SSC/implementation dependant based on the maximum number of columns allowed in a table. For
tables created from templates, the table descriptions in this specification indicate the ordering of the
columns, such that the first column listed in a table description is "Column0", the second is "Column1",
etc. For object tables created using the CreateTable method, the UID column SHALL be Column
Number 0, the first column defined in the Columns parameter of CreateTable SHALL be Column
Number 1, etc.

                   Table 47 ACE_columns
    +-----------------------+-----------+------------+
    |UID                    |Name       |Format      |
    +-----------------------+-----------+------------+
    |00 00 00 05 00 00 1A 03|ACE_columns|Set_Type,   |
    |                       |           |0,          |
    |                       |           |*           |
    +-----------------------+-----------+------------+

\begin{code}


newtype Core_ACE_columns = Core_ACE_columns (Set Int)

_ACE_columnsSetSize :: TPer Int
_ACE_columnsSetSize = max_columns

\end{code}
5.1.3.3 ACE_expression

This is an alternative type where the options are either a uidref to an Authority object or one of the
boolean_ACE (AND = 0 and OR = 1) options. This type is used within the AC_element list to form a
postfix Boolean expression of Authorities.

                       Table 48 ACE_expression
    +-----------------------+--------------+---------------------+
    |UID                    |Name          |Format               |
    +-----------------------+--------------+---------------------+
    |00 00 00 05 00 00 06 01|ACE_expression|Alternative_Type,    |
    |                       |              |Authority_object_ref,|
    |                       |              |boolean_ACE          |
    +-----------------------+--------------+---------------------+

\begin{code}

-- data Core_ACE_expression = Core_ACE_expression_Authority_object_ref Core_Authority_object_ref
--                          | Core_ACE_expression_boolean_ACE          Core_boolean_ACE

-- _ACE_expressionSetSize :: TPer Int
-- _ACE_expressionSetSize = max_columns

\end{code}
5.1.3.4 ACE_object_ref

This type describes a uidref to an object contained in the ACE table.

                           Table 49 ACE_object_ref
    +-----------------------+--------------+-----------------------------+
    |UID                    |Name          |Format                       |
    +-----------------------+--------------+-----------------------------+
    |00 00 00 05 00 00 0C 04|ACE_object_ref|Restricted_Reference_Type{6},|
    |                       |              |uidref{ACETableUID}          |
    +-----------------------+--------------+-----------------------------+

\begin{code}


-- FIXME -- type level prgramming


-- newtype Core_ACE_object_ref = Core_ACE_object_ref (Core_uidref ->
--                                                    Maybe Core_Restricted_Reference_Type_Object)
-- mkCore_ACE_object_ref ::
--     Core_uidref
--  -> Maybe Core_Restricted_Reference_Type_Object
-- mkCore_ACE_object_ref = mkCore_Restricted_object_ref_To uACETable

\end{code}
5.1.3.5 ACL

The ACL type is a list of uidrefs to ACE objects. The length of the list, and therefore the number of
ACEs that MAY be included in a single Access Control List, is SSC/implementation dependant.

\begin{code}


[ttype|

                     Table 50 ACL
    +-----------------------+-----+--------------+
    |UID                    |Name |Format        |
    +-----------------------+-----+--------------+
    |00 00 00 05 00 00 08 02|ACL  |List_Type,    |
    |                       |     |*,            |
    |                       |     |ACE_object_ref|
    +-----------------------+-----+--------------+

|]


-- data Core_ACL = Core_ACL [Core_ACE_object_ref]


-- FIXME -- length check?

\end{code}
5.1.3.6 adv_key_mode

This enumeration type defines the behavior of the NextKey column.

\begin{code}

[ttype|
                     Table 51 adv_key_mode
    +-----------------------+------------+-----------------+
    |UID                    |Name        |Format           |
    +-----------------------+------------+-----------------+
    |00 00 00 05 00 00 04 0F|adv_key_mode|Enumeration_Type,|
    |                       |            |0,               |
    |                       |            |7                |
    +-----------------------+------------+-----------------+

|]

\end{code}
The enumeration values are associated with key behaviors as defined in Table 52.


    +----------------------------------------+
    |Table 52 adv_key_mode Enumeration Values|
    +------------------+---------------------+
    |Enumeration Value |Behavior             |
    +------------------+---------------------+
    |0                 |Wait for AdvKey_Req  |
    +------------------+---------------------+
    |1                 |Auto-advance keys    |
    +------------------+---------------------+
    |2-7               |Reserved             |
    +------------------+---------------------+


\begin{code}

data Core_adv_key_mode = Core_adv_key_mode_Wait_for_AdvKey_Req
                       | Core_adv_key_mode_Auto_advance_keys
                       | Core_adv_key_mode_Reserved_2
                       | Core_adv_key_mode_Reserved_3
                       | Core_adv_key_mode_Reserved_4
                       | Core_adv_key_mode_Reserved_5
                       | Core_adv_key_mode_Reserved_6
                       | Core_adv_key_mode_Reserved_7
    deriving(Enum,Eq,Ord,Show)


\end{code}
5.1.3.7 attr_flags

This set type describes the types of attributes available for the AttributeFlags column of the Column
table.

\begin{code}

[ttype|
                 Table 53 attr_flags
    +-----------------------+----------+---------+
    |UID                    |Name      |Format   |
    +-----------------------+----------+---------+
    |00 00 00 05 00 00 1A 04|attr_flags|Set_Type,|
    |                       |          |0,       |
    |                       |          |31       |
    +-----------------------+----------+---------+

|]
\end{code}

The set values are associated with column behaviors as defined in Table 54.

    +----------------------------------------+
    |     Table 54 attr_flags Set Values     |
    +------------------+---------------------+
    |Set Value         |Behavior             |
    +------------------+---------------------+
    |0                 |Get Not Permitted    |
    +------------------+---------------------+
    |1                 |Set Not Permitted    |
    +------------------+---------------------+
    |2-31              |Reserved             |
    +------------------+---------------------+


\begin{code}

data Core_attr_flags = Core_attr_flags (Set Core_attr_flags_Set_Values)
    deriving(Eq,Ord,Show)
data Core_attr_flags_Set_Values = Core_attr_flags_Set_Values_Get_Not_Permitted
                                | Core_attr_flags_Set_Values_Set_Not_Permitted
                                | Core_attr_flags_Set_Values_Reserved_02
                                | Core_attr_flags_Set_Values_Reserved_03
                                | Core_attr_flags_Set_Values_Reserved_04
                                | Core_attr_flags_Set_Values_Reserved_05
                                | Core_attr_flags_Set_Values_Reserved_06
                                | Core_attr_flags_Set_Values_Reserved_07
                                | Core_attr_flags_Set_Values_Reserved_08
                                | Core_attr_flags_Set_Values_Reserved_09
                                | Core_attr_flags_Set_Values_Reserved_10
                                | Core_attr_flags_Set_Values_Reserved_11
                                | Core_attr_flags_Set_Values_Reserved_12
                                | Core_attr_flags_Set_Values_Reserved_13
                                | Core_attr_flags_Set_Values_Reserved_14
                                | Core_attr_flags_Set_Values_Reserved_15
                                | Core_attr_flags_Set_Values_Reserved_16
                                | Core_attr_flags_Set_Values_Reserved_17
                                | Core_attr_flags_Set_Values_Reserved_18
                                | Core_attr_flags_Set_Values_Reserved_19
                                | Core_attr_flags_Set_Values_Reserved_20
                                | Core_attr_flags_Set_Values_Reserved_21
                                | Core_attr_flags_Set_Values_Reserved_22
                                | Core_attr_flags_Set_Values_Reserved_23
                                | Core_attr_flags_Set_Values_Reserved_24
                                | Core_attr_flags_Set_Values_Reserved_25
                                | Core_attr_flags_Set_Values_Reserved_26
                                | Core_attr_flags_Set_Values_Reserved_27
                                | Core_attr_flags_Set_Values_Reserved_28
                                | Core_attr_flags_Set_Values_Reserved_29
                                | Core_attr_flags_Set_Values_Reserved_30
                                | Core_attr_flags_Set_Values_Reserved_31
    deriving(Enum,Eq,Ord,Show)


\end{code}
5.1.3.8 auth_method

This enumeration type is used to represent the authentication methods that MAY be used to
authenticate authorities (see 5.3.4.1.3).

\begin{code}

[ttype|

                      Table 55 auth_method
    +-----------------------+------------+-----------------+
    |UID                    |Name        |Format           |
    +-----------------------+------------+-----------------+
    |00 00 00 05 00 00 04 08|auth_method |Enumeration_Type,|
    |                       |            |0,               |
    |                       |            |23               |
    +-----------------------+------------+-----------------+
|]

\end{code}
The enumeration values are associated with authentication methods as defined in Table 56.



    +----------------------------------------+
    |Table 56 auth_method Enumeration Values |
    +------------------+---------------------+
    |Enumeration Value |Authentication Method|
    +------------------+---------------------+
    |0                 |None                 |
    +------------------+---------------------+
    |1                 |Password             |
    +------------------+---------------------+
    |2                 |Exchange             |
    +------------------+---------------------+
    |3                 |Sign                 |
    +------------------+---------------------+
    |4                 |SymK                 |
    +------------------+---------------------+
    |5                 |HMAC                 |
    +------------------+---------------------+
    |6                 |TPerSign             |
    +------------------+---------------------+
    |7                 |TPerExchange         |
    +------------------+---------------------+
    |8-23              |Reserved             |
    +------------------+---------------------+


\begin{code}

data Core_auth_method = Core_auth_method_None
                      | Core_auth_method_Password
                      | Core_auth_method_Exchange
                      | Core_auth_method_Sign
                      | Core_auth_method_SymK
                      | Core_auth_method_HMAC
                      | Core_auth_method_TperSign
                      | Core_auth_method_TPerExchange
                      | Core_auth_method_Reserved_8
                      | Core_auth_method_Reserved_9
                      | Core_auth_method_Reserved_10
                      | Core_auth_method_Reserved_11
                      | Core_auth_method_Reserved_12
                      | Core_auth_method_Reserved_13
                      | Core_auth_method_Reserved_14
                      | Core_auth_method_Reserved_15
                      | Core_auth_method_Reserved_16
                      | Core_auth_method_Reserved_17
                      | Core_auth_method_Reserved_18
                      | Core_auth_method_Reserved_19
                      | Core_auth_method_Reserved_20
                      | Core_auth_method_Reserved_21
                      | Core_auth_method_Reserved_22
                      | Core_auth_method_Reserved_23
    deriving(Enum,Eq,Ord,Show)


\end{code}

5.1.3.9 Authority_object_ref

The Authority_object_ref type describes a uidref to an object in the Authority table.
\begin{code}

[ttype|


                        Table 57 Authority_object_ref
    +-----------------------+--------------------+-----------------------------+
    |UID                    |Name                |Format                       |
    +-----------------------+--------------------+-----------------------------+
    |00 00 00 05 00 00 0C 05|Authority_object_ref|Restricted_Reference_Type{6},|
    |                       |                    |uidref{AuthorityTableUID}    |
    +-----------------------+--------------------+-----------------------------+
|]

\end{code}

\begin{code}


-- FIXME -- type level prgramming


-- newtype Core_Authority_object_ref =
--     Core_Authority_object_ref (Core_uidref
--                               -> Maybe Core_Restricted_Reference_Type_Object)

-- mkCore_Authority_object_ref ::
--     Core_uidref
--  -> Maybe Core_Restricted_Reference_Type_Object
-- mkCore_Authority_object_ref = mkCore_Restricted_object_ref_To uAuthorityTable

\end{code}



5.1.3.10 boolean
The boolean column type is an enumeration used to represent True or False.

\begin{code}

[ttype|


                        Table 58 boolean
    +-----------------------+------------+-----------------+
    |UID                    |Name        |Format           |
    +-----------------------+------------+-----------------+
    |00 00 00 05 00 00 04 01|boolean     |Enumeration_Type,|
    |                       |            |0,               |
    |                       |            |1                |
    +-----------------------+------------+-----------------+

|]

\end{code}
The enumeration values are associated as defined in Table 59.



    +------------------------------------+
    |Table 59 boolean Enumeration Values |
    +------------------+-----------------+
    |Enumeration Value |Associated Value |
    +------------------+-----------------+
    |0                 |False            |
    +------------------+-----------------+
    |1                 |True             |
    +------------------+-----------------+



\begin{code}

type Core_boolean = Bool

\end{code}


5.1.3.11 boolean_ACE

This enumeration is used to identify the Boolean operators "And", "Or", and "Not".

\begin{code}

[ttype|


                      Table 60 boolean_ACE
    +-----------------------+------------+-----------------+
    |UID                    |Name        |Format           |
    +-----------------------+------------+-----------------+
    |00 00 00 05 00 00 04 0E|boolean_ACE |Enumeration_Type,|
    |                       |            |0,               |
    |                       |            |2                |
    +-----------------------+------------+-----------------+

|]

\end{code}
The enumeration values are associated with Boolean operators as defined in Table 61.


    Table 61 boolean_ACE Enumeration Values
    +------------------+---------------------+
    |Enumeration Value |Associated Value     |
    +------------------+---------------------+
    |0                 |And                  |
    +------------------+---------------------+
    |1                 |Or                   |
    +------------------+---------------------+
    |2                 |Not                  |
    +------------------+---------------------+



\begin{code}

data Core_boolean_ACE = And | Or | Not
    deriving (Show, Enum, Eq, Read)

\end{code}


5.1.3.12 byte_row_ref

Type used for referencing a row in a byte table.
\begin{code}

[ttype|


                           Table 62 byte_row_ref
    +-----------------------+-------------+---------------------------+
    |UID                    |Name         |Format                     |
    +-----------------------+-------------+---------------------------+
    |00 00 00 05 00 00 0F 01|byte_row_ref |General_Reference_Type {7} |
    +-----------------------+-------------+---------------------------+
|]

\end{code}


\begin{code}

type Core_byte_row_ref = Core_General_Reference_Type_Byte_Row

\end{code}
5.1.3.13 byte_table_ref

This is a reference type that SHALL be used specifically for uidrefs to byte tables. When performing
type checking, as part of that type checking the TPer SHALL validate that this uidref is to a table that is
a byte table.
\begin{code}

[ttype|


                          Table 63 byte_table_ref
    +-----------------------+--------------+----------------------------+
    |UID                    |Name          |Format                      |
    +-----------------------+--------------+----------------------------+
    |00 00 00 05 00 00 10 01|byte_table_ref|General_Reference_Table_Type|
    |                       |              |2                           |
    +-----------------------+--------------+----------------------------+

|]

\end{code}

\begin{code}

-- type Core_byte_table_ref = 'General_Reference_Table_Type_Byte

\end{code}


5.1.3.14 bytes

This type represents the bytes base type, and is used to represent a value made up of a fixed-size
sequence of bytes.
\begin{code}

[ttype|


                  Table 64 bytes
    +-----------------------+------+----------+
    |UID                    |Name  |Format    |
    +-----------------------+------+----------+
    |00 00 00 05 00 00 00 02|bytes |Base_Type |
    +-----------------------+------+----------+
|]

\end{code}


\begin{code}

newtype Core_bytes n = Core_bytes (Fixed_bytes n)
    deriving (Eq,Ord,IsString)
instance (KnownNat n) => Show (Core_bytes n) where
    show (Core_bytes bs) = show bs

\end{code}


5.1.3.15 bytes_4

This is a bytes type with a size requirement of 4.
\begin{code}

[ttype|


                  Table 65 bytes_4
    +-----------------------+-------+---------------------+
    |UID                    |Name   |Format               |
    +-----------------------+-------+---------------------+
    |00 00 00 05 00 00 02 38|bytes_4|Simple_Type,         |
    |                       |       |bytes,               |
    |                       |       |4                    |
    +-----------------------+-------+---------------------+

|]

\end{code}
\begin{code}

type Core_bytes_4 = Core_bytes 4

\end{code}


5.1.3.16 bytes_12

This is a bytes type with a size requirement of 12.
\begin{code}

[ttype|


                       Table 66 bytes_12
    +-----------------------+--------+---------------------+
    |UID                    |Name    |Format               |
    +-----------------------+--------+---------------------+
    |00 00 00 05 00 00 02 01|bytes_12|Simple_Type,         |
    |                       |        |bytes,               |
    |                       |        |12                   |
    +-----------------------+--------+---------------------+
|]

\end{code}

\begin{code}

type Core_bytes_12 = Core_bytes 12

\end{code}


5.1.3.17 bytes_16

This is a bytes type with a size requirement of 16.
\begin{code}

[ttype|


                       Table 67 bytes_16
    +-----------------------+--------+---------------------+
    |UID                    |Name    |Format               |
    +-----------------------+--------+---------------------+
    |00 00 00 05 00 00 02 02|bytes_16|Simple_Type,         |
    |                       |        |bytes,               |
    |                       |        |16                   |
    +-----------------------+--------+---------------------+
|]

\end{code}

\begin{code}

type Core_bytes_16 = Core_bytes 16

\end{code}


5.1.3.18 bytes_20

This is a bytes type with a size requirement of 20.
\begin{code}

[ttype|


                       Table 68 bytes_20
    +-----------------------+--------+---------------------+
    |UID                    |Name    |Format               |
    +-----------------------+--------+---------------------+
    |00 00 00 05 00 00 02 36|bytes_20|Simple_Type,         |
    |                       |        |bytes,               |
    |                       |        |20                   |
    +-----------------------+--------+---------------------+
|]

\end{code}

\begin{code}

type Core_bytes_20 = Core_bytes 20

\end{code}



5.1.3.19 bytes_32

This is a bytes type with a size requirement of 32.
\begin{code}

[ttype|


                       Table 69 bytes_32
    +-----------------------+--------+---------------------+
    |UID                    |Name    |Format               |
    +-----------------------+--------+---------------------+
    |00 00 00 05 00 00 02 05|bytes_32|Simple_Type,         |
    |                       |        |bytes,               |
    |                       |        |32                   |
    +-----------------------+--------+---------------------+
|]

\end{code}

\begin{code}

type Core_bytes_32 = Core_bytes 32

\end{code}


5.1.3.20 bytes_48

This is a bytes type with a size requirement of 48.
\begin{code}

[ttype|


                       Table 70 bytes_48
    +-----------------------+--------+---------------------+
    |UID                    |Name    |Format               |
    +-----------------------+--------+---------------------+
    |00 00 00 05 00 00 02 37|bytes_48|Simple_Type,         |
    |                       |        |bytes,               |
    |                       |        |48                   |
    +-----------------------+--------+---------------------+
|]

\end{code}

\begin{code}

type Core_bytes_48 = Core_bytes 48

\end{code}




5.1.3.21 bytes_64

This is a bytes type with a size requirement of 64.
\begin{code}

[ttype|


                       Table 71 bytes_64
    +-----------------------+--------+---------------------+
    |UID                    |Name    |Format               |
    +-----------------------+--------+---------------------+
    |00 00 00 05 00 00 02 06|bytes_64|Simple_Type,         |
    |                       |        |bytes,               |
    |                       |        |64                   |
    +-----------------------+--------+---------------------+
|]

\end{code}

\begin{code}

type Core_bytes_64 = Core_bytes 64

\end{code}

5.1.3.22 Certificates_object_ref

The Certificates_object_ref type describes a uidref to an object in the Certificates table.
\begin{code}

[ttype|

                           Table 72 Certificates_object_ref
    +-----------------------+-----------------------+-----------------------------+
    |UID                    |Name                   |Format                       |
    +-----------------------+-----------------------+-----------------------------+
    |00 00 00 05 00 00 0C 06|Certificates_object_ref|Restricted_Reference_Type{6},|
    |                       |                       |uidref{CertificatesTableUID} |
    +-----------------------+-----------------------+-----------------------------+
|]

\end{code}

\begin{code}


-- FIXME -- type level prgramming


newtype Core_Certificates_object_ref =
    Core_Certificates_object_ref (Core_uidref
                              -> Maybe Core_Restricted_Reference_Type_Object)

-- mkCore_Certificates_object_ref ::
--     Core_uidref
--  -> Maybe Core_Restricted_Reference_Type_Object
-- mkCore_Certificates_object_ref = mkCore_Restricted_object_ref_To uCertificatesTable

\end{code}



5.1.3.23 clock_kind

This enumeration type is used to define the type of clock currently active.
\begin{code}

[ttype|

                     Table 73 clock_kind
    +-----------------------+-----------+-----------------+
    |UID                    |Name       |Format           |
    +-----------------------+-----------+-----------------+
    |00 00 00 05 00 00 04 0B|clock_kind |Enumeration_Type,|
    |                       |           |0,               |
    |                       |           |3                |
    +-----------------------+-----------+-----------------+
|]

\end{code}

\begin{code}


-- FIXME -- type level prgramming


-- newtype Core_Certificates_object_ref =
--     Core_Certificates_object_ref (Core_uidref
--                               -> Maybe Core_Restricted_Reference_Type_Object)

-- mkCore_Certificates_object_ref ::
--     Core_uidref
--  -> Maybe Core_Restricted_Reference_Type_Object
-- mkCore_Certificates_object_ref = mkCore_Restricted_object_ref_To uCertificatesTable

\end{code}





The enumeration values are associated as defined in Table 74.



    +---------------------------------------+
    |Table 74 clock_kind Enumeration Values |
    +------------------+--------------------+
    |Enumeration Value |Associated Value    |
    +------------------+--------------------+
    |0                 |Timer               |
    +------------------+--------------------+
    |1                 |Low                 |
    +------------------+--------------------+
    |2                 |High                |
    +------------------+--------------------+
    |3                 |LowAndHigh          |
    +------------------+--------------------+






5.1.3.24 clock_time

This is a struct type made up of name-value pairs, and is used to represent time. Any value not
supplied is treated as 0.

If the host has supplied a trusted time since powerup, that time is used; otherwise a monotonic counter
is used.

The clock_time type represents times in either Generalized Time or UTC Time. Using this type to
represent UTC Time requires 0's (zeroes) in fields where Generalized time requires a value but UTC
Time does not (i.e. 2006 in UTC Time would be represented as 0006). Per the definition for the
component types, the names for these name-value types are 0x00 (for the Year), 0x01 (for the Month),
0x02 (for the Day), 0x03 (for the Hour), 0x04 (for the Minute), 0x05 (for the Seconds), and 0x06 (for the
Fraction).

\begin{code}

[ttype|

                     Table 75 clock_time
    +-----------------------+-----------+-----------------+
    |UID                    |Name       |Format           |
    +-----------------------+-----------+-----------------+
    |00 00 00 05 00 00 18 05|clock_time |Struct_Type,     |
    |                       |           |Year,            |
    |                       |           |Month,           |
    |                       |           |Day,             |
    |                       |           |Hour,            |
    |                       |           |Minute,          |
    |                       |           |Seconds,         |
    |                       |           |Fraction         |
    +-----------------------+-----------+-----------------+

|]

\end{code}


5.1.3.25 Column_object_ref

The Column_object_ref type describes a uidref to an object in the Column table.

\begin{code}

[ttype|

                         Table 76 Column_object_ref
    +-----------------------+-----------------+-----------------------------+
    |UID                    |Name             |Format                       |
    +-----------------------+-----------------+-----------------------------+
    |00 00 00 05 00 00 0C 07|Column_object_ref|Restricted_Reference_Type{6},|
    |                       |                 |uidref {ColumnTable_UID}     |
    +-----------------------+-----------------+-----------------------------+

|]

\end{code}


5.1.3.26 cred_object_uidref

The cred_object_uidref type is a restricted reference type that SHALL be used specifically for uidrefs to
credential objects. When performing type checking, as part of that type checking the TPer SHALL
validate that this uidref is to an object in a credential (C_*) table.

In the Format column of Table 77, the * is used to indicate the entire range of that particular type of
credential table.



\begin{code}

[ttype|

                          Table 77 cred_object_uidref
    +-----------------------+-------------------+-----------------------------+
    |UID                    |Name               |Format                       |
    +-----------------------+-------------------+-----------------------------+
    |00 00 00 05 00 00 0C 0B|cred_object_uidref |Restricted_Reference_Type{6},|
    |                       |                   |uidref {C_PINTableUID},      |
    |                       |                   |uidref {C_AES_*TableUID},    |
    |                       |                   |uidref {C_RSA_*TableUID},    |
    |                       |                   |uidref{C_EC_*TableUID},      |
    |                       |                   |uidref{C_HMAC_*TableUID}     |
    +-----------------------+-------------------+-----------------------------+

|]

\end{code}


5.1.3.27 date

The date type represents the date portion of the time from the system clock. This is a set of name-value pairs, with the names 0x00 (for the Year), 0x01 (for the Month), and 0x02 (for the Day).

\begin{code}

[ttype|

                     Table 78 date
    +-----------------------+----+-----------------+
    |UID                    |Name|Format           |
    +-----------------------+----+-----------------+
    |00 00 00 05 00 00 18 04|date|Struct_Type,     |
    |                       |    |Year,            |
    |                       |    |Month,           |
    |                       |    |Day              |
    +-----------------------+----+-----------------+

|]

\end{code}


5.1.3.28 Day

Name-value pair that has a Name of "2" and takes day_enum as the value.

\begin{code}

[ttype|

                         Table 79 Day
    +-----------------------+----+-------------------------+
    |UID                    |Name|Format                   |
    +-----------------------+----+-------------------------+
    |00 00 00 05 00 00 14 03|Day |Name_Value_Uinteger_Type,|
    |                       |    |2,                       |
    |                       |    |day_enum                 |
    +-----------------------+----+-------------------------+

|]

\end{code}

5.1.3.29 day_enum

Used in association with the Day name-value pair.

\begin{code}

[ttype|

                         Table 80 day_enum
    +-----------------------+--------+-----------------+
    |UID                    |Name    |Format           |
    +-----------------------+--------+-----------------+
    |00 00 00 05 00 00 04 18|day_enum|Enumeration_Type,|
    |                       |        |1,               |
    |                       |        |31               |
    +-----------------------+--------+-----------------+

|]

\end{code}


5.1.3.30 enc_supported

This enumeration type is used to define the types of user data encryption
supported by the TPer.

\begin{code}

[ttype|

                         Table 81 enc_supported
    +-----------------------+-------------+-----------------+
    |UID                    |Name         |Format           |
    +-----------------------+-------------+-----------------+
    |00 00 00 05 00 00 04 1D|enc_supported|Enumeration_Type,|
    |                       |             |0,               |
    |                       |             |15               |
    +-----------------------+-------------+-----------------+

|]

\end{code}

The enumeration values are associated as defined in Table 82.

        Table 82 enc_supported Enumes
    +-----------------+----------------+
    |Enumeration Value|Associated Value|
    +-----------------+----------------+
    |0                |None            |
    +-----------------+----------------+
    |1                |Media Encryption|
    +-----------------+----------------+
    |2-15             |Reserved        |
    +-----------------+----------------+




5.1.3.31 feedback_size

This uinteger type represents the feedback sizes for AES used in CFB mode. If AES Mode is CFB, this
SHALL be between 1 and the block length.

\begin{code}

[ttype|

                     Table 83 feedback_size
    +-----------------------+-------------+-----------------+
    |UID                    |Name         |Format           |
    +-----------------------+-------------+-----------------+
    |00 00 00 05 00 00 18 04|feedback_size|Simple_Type,     |
    |                       |             |uinteger,        |
    |                       |             |2                |
    +-----------------------+-------------+-----------------+

|]

\end{code}

5.1.3.32 Fraction

Name-value pair that has a Name of "6" and takes fraction enum as the value.

\begin{code}

[ttype|

                     Table 84 Fraction
    +-----------------------+---------+-------------------------+
    |UID                    |Name     |Format                   |
    +-----------------------+---------+-------------------------+
    |00 00 00 05 00 00 14 07|Fraction |Name_Value_Uinteger_Type |
    |                       |         |6,                       |
    |                       |         |fraction_enum            |
    +-----------------------+---------+-------------------------+

|]

\end{code}

5.1.3.33 fraction_enum

Used in association with the Fraction name-value pair.

\begin{code}

[ttype|

                     Table 85 fraction_enum
    +-----------------------+-------------+-----------------+
    |UID                    |Name         |Format           |
    +-----------------------+-------------+-----------------+
    |00 00 00 05 00 00 18 04|fraction_enum|Enumeration_Type,|
    |                       |             |0,               |
    |                       |             |999              |
    +-----------------------+-------------+-----------------+

|]

\end{code}

5.1.3.34 gen_status

This set type is used to identify the general status of the re-encryption process.

\begin{code}

[ttype|

                    Table 86 gen_status
    +-----------------------+----------+---------+
    |UID                    |Name      |Format   |
    +-----------------------+----------+---------+
    |00 00 00 05 00 00 18 04|gen_status|Set_Type,|
    |                       |          |0,       |
    |                       |          |63       |
    +-----------------------+----------+---------+

|]
\end{code}



The enumeration values are associated as defined in table Table 87. Values 0-31 are valid for the
PAUSED state, value 32-63 are valid for the PENDING state (see 5.7.3.3).

    +-----------------------------------------------------------------------------------------------------+
    |                               Table 87 gen_status Enumeration Values                                |
    +-------+----------------------------------------+----------------------------------------------------+
    |Column |Associated Value                        |Meaning                                             |
    |Value  |                                        |                                                    |
    +-------+----------------------------------------+----------------------------------------------------+
    |0      |None                                    |                                                    |
    +-------+----------------------------------------+----------------------------------------------------+
    |1      |pending_tper_error                      |Last ReEncryptState value was PENDING AND a         |
    |       |                                        |TPer_Error_Detect condition was detected            |
    +-------+----------------------------------------+----------------------------------------------------+
    |2      |active_tper_error                       |Last ReEncryptState value was ACTIVE AND a          |
    |       |                                        |TPer_Error_Detect condition was detected            |
    +-------+----------------------------------------+----------------------------------------------------+
    |3      |active_pause_requested                  |Last ReEncryptState value was ACTIVE AND PAUSE_req  |
    |       |                                        |was detected                                        |
    +-------+----------------------------------------+----------------------------------------------------+
    |4      |pend_pause_requested                    |Last ReEncryptState value was PENDING AND a         |
    |       |                                        |PAUSE_req value was detected                        |
    +-------+----------------------------------------+----------------------------------------------------+
    |5      |pend_reset_stop_detect                  |A reset condition AND its associated ContOnReset    |
    |       |                                        |configuration does not allow re-encryption to       |
    |       |                                        |continue AND last state was PENDING                 |
    +-------+----------------------------------------+----------------------------------------------------+
    |6      |key_error                               |ReEncryptState value was PENDING AND valid keys were|
    |       |                                        |not found in any C_* table OR insufficient access   |
    |       |                                        |control granted for reading C_* table.              |
    +-------+----------------------------------------+----------------------------------------------------+
    |7 to 31|reserved                                |                                                    |
    |       |                                        |                                                    |
    +-------+----------------------------------------+----------------------------------------------------+
    |32     |wait_AvailableKeys                      |keys are not available                              |
    +-------+----------------------------------------+----------------------------------------------------+
    |33     |wait_for_TPer_resources                 |TPer_Ready condition is not True                    |
    |       |                                        |                                                    |
    +-------+----------------------------------------+----------------------------------------------------+
    |34     |active_reset_stop_detect                |A reset condition AND its associated ContOnReset    |
    |       |                                        |configuration does not allow re-encryption to       |
    |       |                                        |continue AND last ReEncryptState value was ACTIVE   |
    +-------+----------------------------------------+----------------------------------------------------+
    |34-63  |reserved                                |                                                    |
    +-------+----------------------------------------+----------------------------------------------------+






5.1.3.35 hash_protocol

This enumeration type determines the hash algorithm to be used when creating a digital signature.

\begin{code}

[ttype|

                     Table 88 hash_protocol
    +-----------------------+-------------+-----------------+
    |UID                    |Name         |Format           |
    +-----------------------+-------------+-----------------+
    |00 00 00 05 00 00 04 0D|hash_protocol|Enumeration_Type,|
    |                       |             |0,               |
    |                       |             |15               |
    |                       |             |                 |
    +-----------------------+-------------+-----------------+

|]

\end{code}



The enumeration values are associated as defined in Table 89.

    +-----------------------------------------+
    |Table 89 hash_protocol Enumeration Values|
    +------------------+----------------------+
    |Enumeration Value |Associated Value      |
    +------------------+----------------------+
    |0                 |None                  |
    +------------------+----------------------+
    |1                 |SHA 1                 |
    +------------------+----------------------+
    |2                 |SHA 256               |
    +------------------+----------------------+
    |3                 |SHA 384               |
    +------------------+----------------------+
    |4                 |SHA 512               |
    +------------------+----------------------+
    |5-15              |Reserved              |
    +------------------+----------------------+


5.1.3.36 Hour

Name-value pair that has a Name of "3" and takes hour_enum as the value.

\begin{code}

[ttype|

                         Table 90 Hour
    +-----------------------+----+-------------------------+
    |UID                    |Name|Format                   |
    +-----------------------+----+-------------------------+
    |00 00 00 05 00 00 14 04|Hour|Name_Value_Uinteger_Type,|
    |                       |    |3,                       |
    |                       |    |hour_enum                |
    +-----------------------+----+-------------------------+

|]

\end{code}


5.1.3.37 hour_enum

Used in association with the Hour name-value pair.

\begin{code}

[ttype|

                     Table 91 hour_enum
    +-----------------------+-------------+-----------------+
    |UID                    |Name         |Format           |
    +-----------------------+-------------+-----------------+
    |00 00 00 05 00 00 04 19|hour_enum    |Enumeration_Type,|
    |                       |             |0,               |
    |                       |             |23               |
    +-----------------------+-------------+-----------------+

|]
\end{code}



5.1.3.38 integer

This is the base type used to represent a signed integer.

\begin{code}

[ttype|

                  Table 92 integer
    +-----------------------+-------+----------+
    |UID                    |Name   |Format    |
    +-----------------------+-------+----------+
    |00 00 00 05 00 00 00 04|integer|Base_Type |
    +-----------------------+-------+----------+

|]

\end{code}


5.1.3.39 integer_1

This is an integer type with a size limit of 1 byte.

\begin{code}

[ttype|

                   Table 93 integer_1
    +-----------------------+---------+------------+
    |UID                    |Name     |Format      |
    +-----------------------+---------+------------+
    |00 00 00 05 00 00 02 10|integer_1|Simple_Type,|
    |                       |         |integer,    |
    |                       |         |1           |
    +-----------------------+---------+------------+

|]

\end{code}


5.1.3.40 integer_2

This is an integer type with a size limit of 2 bytes.

\begin{code}

[ttype|

                   Table 94 integer_2
    +-----------------------+---------+------------+
    |UID                    |Name     |Format      |
    +-----------------------+---------+------------+
    |00 00 00 05 00 00 02 15|integer_2|Simple_Type,|
    |                       |         |integer,    |
    |                       |         |2           |
    +-----------------------+---------+------------+

|]

\end{code}


5.1.3.41 key_128

This is an alternative type, with options for various key sizes.section

text

\begin{code}

[ttype|

                     Table 95 key_128
    +-----------------------+-------+-----------------+
    |UID                    |Name   |Format           |
    +-----------------------+-------+-----------------+
    |00 00 00 05 00 00 06 02|key_128|Alternative_Type,|
    |                       |       |bytes_16,        |
    |                       |       |bytes_32         |
    +-----------------------+-------+-----------------+

|]

\end{code}


5.1.3.42 key_256

This is an alternative type, with options for various key sizes.


\begin{code}

[ttype|

                     Table 96 key_256
    +-----------------------+-------+-----------------+
    |UID                    |Name   |Format           |
    +-----------------------+-------+-----------------+
    |00 00 00 05 00 00 06 03|key_256|Alternative_Type,|
    |                       |       |bytes_32,        |
    |                       |       |bytes_64         |
    +-----------------------+-------+-----------------+

|]

\end{code}




5.1.3.43 keys_avail_conds

This enumeration describes the conditions required to assert KeysAvailable in the Locking table.

\begin{code}

[ttype|

                      Table 97 keys_avail_conds
    +-----------------------+----------------+-----------------+
    |UID                    |Name            |Format           |
    +-----------------------+----------------+-----------------+
    |00 00 00 05 00 00 04 10|keys_avail_conds|Enumeration_Type,|
    |                       |                |0,               |
    |                       |                |7                |
    +-----------------------+----------------+-----------------+

|]

\end{code}


The enumeration values are associated as defined in Table 98.


                          Table 98 keys_avail_conds Enumeration Values
    +-----------+---------------------------------------------------------------------------+
    |Enumeration|Associated Value                                                           |
    |Value      |                                                                           |
    +-----------+---------------------------------------------------------------------------+
    |0          |None                                                                       |
    +-----------+---------------------------------------------------------------------------+
    |1          |Authentication of an authority with Set access to any of the ReadLocked,   |
    |           |WriteLocked, ReadLockEnabled or WriteLockEnabled columns for that LBA range|
    +-----------+---------------------------------------------------------------------------+
    |2-7        |Reserved                                                                   |
    +-----------+---------------------------------------------------------------------------+



5.1.3.44 lag

A struct made up of 2 uinteger_2 name-value types, used to define the lag when setting time. The two
types represent seconds and fraction of seconds. The names required, as defined by the component
types, are 0x05 ("Seconds") for the first value and 0x06 ("Fraction") for the second. The "Fraction" value
is a number of milliseconds.

\begin{code}

[ttype|

                   Table 99 lag
    +-----------------------+----+------------+
    |UID                    |Name|Format      |
    +-----------------------+----+------------+
    |00 00 00 05 00 00 18 02|lag |Struct_Type,|
    |                       |    |Seconds,    |
    |                       |    |Fraction    |
    +-----------------------+----+------------+

|]

\end{code}

5.1.3.45 last_reenc_stat

This enumeration identifies the last attempted re-encryption step.

\begin{code}

[ttype|

                     Table 100 last_reenc_stat
    +-----------------------+---------------+-----------------+
    |UID                    |Name           |Format           |
    +-----------------------+---------------+-----------------+
    |00 00 00 05 00 00 04 11|last_reenc_stat|Enumeration_Type,|
    |                       |               |0,               |
    |                       |               |7                |
    +-----------------------+---------------+-----------------+

|]

\end{code}

The enumeration values are associated as defined in Table 101.

     Table 101 last_reenc_stat Enumeration Values
    +-----------------+--------------------------+
    |Enumeration Value|Associated Value          |
    +-----------------+--------------------------+
    |0                |Success                   |
    +-----------------+--------------------------+
    |1                |Read Error                |
    +-----------------+--------------------------+
    |2                |Write Error               |
    +-----------------+--------------------------+
    |3                |Verify Error              |
    +-----------------+--------------------------+
    |4-7              |Reserved                  |
    +-----------------+--------------------------+


5.1.3.46 life_cycle_state

This enumeration is used to represent the current life cycle state of the SP.

\begin{code}

[ttype|

                     Table 102 life_cycle_state
    +-----------------------+----------------+-----------------+
    |UID                    |Name            |Format           |
    +-----------------------+----------------+-----------------+
    |00 00 00 05 00 00 04 05|life_cycle_state|Enumeration_Type,|
    |                       |                |0,               |
    |                       |                |15               |
    +-----------------------+----------------+-----------------+

|]

\end{code}

The enumeration values are associated as defined in Table 103.

    Table 103 life_cycle_state Enumeration Values
    +-----------------+--------------------------+
    |Enumeration Value|Associated Value          |
    +-----------------+--------------------------+
    |0                |Issued                    |
    +-----------------+--------------------------+
    |1                |Issued-Disabled           |
    +-----------------+--------------------------+
    |2                |Issued-Frozen             |
    +-----------------+--------------------------+
    |3                |Issued-Disabled-Frozen    |
    +-----------------+--------------------------+
    |4                |Issued-Failed             |
    +-----------------+--------------------------+
    |5-7              |Unassigned                |
    +-----------------+--------------------------+
    |8-13             |Reserved for SSC Usage    |
    +-----------------+--------------------------+
    |14-15            |Unassigned                |
    +-----------------+--------------------------+

5.1.3.47 LogList_object_ref

The LogList_object_ref type describes a uidref to an object in the LogList table.

\begin{code}

[ttype|

                              Table 104 LogList_object_ref
    +-----------------------+-------------------------+-----------------------------+
    |UID                    |Name                     |Format                       |
    +-----------------------+-------------------------+-----------------------------+
    |00 00 00 05 00 00 0C 0D|LogList_object_ref       |Restricted_Reference_Type{6},|
    |                       |                         |uidref {LogListTableUID}     |
    +-----------------------+-------------------------+-----------------------------+

|]

\end{code}

5.1.3.48 log_row_ref

This type SHALL be used specifically for rows in Log tables. When performing type checking, as part of
that type checking the TPer SHALL validate that this is the uid of a row in a Log table.

The * in the Format column of Table 105 indicates that other Log tables besides the default log MAY
exist in a particular SP, and that the Format column value for this type also includes those.

\begin{code}

[ttype|

                        Table 105 log_row_ref
    +-----------------------+-----------+-------------------------+
    |UID                    |Name       |Format                   |
    +-----------------------+-----------+-------------------------+
    |00 00 00 05 00 00 0C 0A|log_row_ref|Restricted_Reference_Type|
    |                       |           |{6},                     |
    |                       |           |uidref{LogTableUID},     |
    |                       |           |*                        |
    +-----------------------+-----------+-------------------------+

|]

\end{code}


5.1.3.49 log_select

This enumeration is used to identify the scope of the logging for an access control association or
authority authentication.

\begin{code}

[ttype|

                     Table 106 log_select
    +-----------------------+----------+-----------------+
    |UID                    |Name      |Format           |
    +-----------------------+----------+-----------------+
    |00 00 00 05 00 00 04 0C|log_select|Enumeration_Type,|
    |                       |          |0,               |
    |                       |          |3                |
    +-----------------------+----------+-----------------+

|]

\end{code}


The enumeration values are associated as defined in Table 107.

    Table 107 log_select Enumeration Values
    +-----------------+-------------------+
    |Enumeration Value|Associated Value   |
    +-----------------+-------------------+
    |0                |None               |
    +-----------------+-------------------+
    |1                |LogSuccess         |
    +-----------------+-------------------+
    |2                |LogFail            |
    +-----------------+-------------------+
    |3                |LogAlways          |
    +-----------------+-------------------+


5.1.3.50 max_bytes

This is the base type that is used to represent a bytes value that is equal to or less than the size
specified for the type instance.

\begin{code}

[ttype|

                 Table 108 max_bytes
    +-----------------------+---------+---------+
    |UID                    |Name     |Format   |
    +-----------------------+---------+---------+
    |00 00 00 05 00 00 00 03|max_bytes|Base_Type|
    +-----------------------+---------+---------+

|]

\end{code}

5.1.3.51 max_bytes_32

This is a max bytes type that provides a maximum size of 32.

\begin{code}

[ttype|

                  Table 109 max_bytes_32
    +-----------------------+------------+------------+
    |UID                    |Name        |Format      |
    +-----------------------+------------+------------+
    |00 00 00 05 00 00 02 0D|max_bytes_32|Simple_Type,|
    |                       |            |max_bytes,  |
    |                       |            |32          |
    +-----------------------+------------+------------+

|]

\end{code}

5.1.3.52 max_bytes_64

This is a max bytes type that provides a maximum size of 64.

\begin{code}

[ttype|

                  Table 110 max_bytes_64
    +-----------------------+------------+------------+
    |UID                    |Name        |Format      |
    +-----------------------+------------+------------+
    |00 00 00 05 00 00 02 0E|max_bytes_64|Simple_Type,|
    |                       |            |max_bytes,  |
    |                       |            |64          |
    +-----------------------+------------+------------+

|]

\end{code}

5.1.3.53 mediakey_obj_uidref

This is a restricted reference type that SHALL be used specifically for uidrefs to media encryption key
objects (in the K_* tables). When performing type checking, as part of that type checking the TPer
SHALL validate that this uidref is to an object in a media encryption key table.

\begin{code}

[ttype|

                            Table 111 mediakey_obj_uidref
    +-----------------------+----------------------+-----------------------------+
    |UID                    |Name                  |Format                       |
    +-----------------------+----------------------+-----------------------------+
    |00 00 00 05 00 00 0C 0C|mediakey_object_uidref|Restricted_Reference_Type{6},|
    |                       |                      |uidref {K_AES_128TableUID},  |
    |                       |                      |uidref {K_AES_256TableUID}   |
    +-----------------------+----------------------+-----------------------------+

|]

\end{code}

5.1.3.54 MethodID_object _ref

The MethodID_object _ref type describes a uidref to an object in the MethodID table.

\begin{code}

[ttype|

    Table 112 MethodID_object _ref
    +-----------------------+-------------------+-----------------------------+
    |UID                    |Name               |Format                       |
    +-----------------------+-------------------+-----------------------------+
    |00 00 00 05 00 00 0C 03|MethodID_object_ref|Restricted_Reference_Type{6},|
    |                       |                   |uidref {MethodIDTableUID}    |
    +-----------------------+-------------------+-----------------------------+

|]

\end{code}

5.1.3.55 messaging_type

This enumeration is used to describe the options for selecting secure messaging.

\begin{code}

[ttype|

    Table 113 messaging_type
    +-----------------------+--------------+-----------------+
    |UID                    |Name          |Format           |
    +-----------------------+--------------+-----------------+
    |00 00 00 05 00 00 04 04|messaging_type|Enumeration_Type,|
    |                       |              |0,               |
    |                       |              |255              |
    +-----------------------+--------------+-----------------+

|]

\end{code}

The enumeration values and their associations defined in Table 179.
--------------------------------------------------------------------------------
5.1.3.56 Minute

Name-value pair that has a Name of "4" and takes minute_enum as the value.

\begin{code}

[ttype|

                         Table 114 Minute
    +-----------------------+------+-------------------------+
    |UID                    |Name  |Format                   |
    +-----------------------+------+-------------------------+
    |00 00 00 05 00 00 14 05|Minute|Name_Value_Uinteger_Type,|
    |                       |      |4,                       |
    |                       |      |minute_enum              |
    +-----------------------+------+-------------------------+

|]

\end{code}


5.1.3.57 minute_enum

Used in association with the Minute name-value pair.

\begin{code}

[ttype|

                     Table 115 minute_enum
    +-----------------------+-----------+-----------------+
    |UID                    |Name       |Format           |
    +-----------------------+-----------+-----------------+
    |00 00 00 05 00 00 04 1A|minute_enum|Enumeration_Type,|
    |                       |           |0,               |
    |                       |           |59               |
    +-----------------------+-----------+-----------------+

|]

\end{code}

5.1.3.58 Month

Name-value pair that has a Name of "1" and takes month_enum as the value.

\begin{code}

[ttype|

                     Table 116 Month
    +-----------------------+-----+-------------------------+
    |UID                    |Name |Format                   |
    +-----------------------+-----+-------------------------+
    |00 00 00 05 00 00 14 02|Month|Name_Value_Uinteger_Type,|
    |                       |     |1,                       |
    |                       |     |month_enum               |
    +-----------------------+-----+-------------------------+

|]

\end{code}

5.1.3.59 month_enum

Used in association with the Month name-value pair.

\begin{code}

[ttype|

                     Table 117 month_enum
    +-----------------------+------------------+-----------------+
    |UID                    |Name              |Format           |
    +-----------------------+------------------+-----------------+
    |00 00 00 05 00 00 04 17|month_enum        |Enumeration_Type,|
    |                       |                  |1,               |
    |                       |                  |12               |
    +-----------------------+------------------+-----------------+

|]

\end{code}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
5.1.3.60 name

This max bytes type, with a size limitation of 32, is used to represent names.

\begin{code}

[ttype|

                  Table 118 name
    +-----------------------+----+------------+
    |UID                    |Name|Format      |
    +-----------------------+----+------------+
    |00 00 00 05 00 00 02 0B|name|Simple_Type,|
    |                       |    |max_bytes,  |
    |                       |    |32          |
    +-----------------------+----+------------+

|]

\end{code}

5.1.3.61 object_ref

Type used for referencing an object in an object table.

\begin{code}

[ttype|

                     Table etc.
    +-----------------------+------------------+----------------------+
    |UID                    |Name              |Format                |
    +-----------------------+------------------+----------------------+
    |00 00 00 05 00 00 0F 02|object_ref        |General_Reference_Type|
    |                       |                  |{8}                   |
    |                       |                  |                      |
    +-----------------------+------------------+----------------------+

|]

\end{code}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
5.1.3.62 padding_type

This enumeration is used to identify the type of padding used with RSA encryption. RSAES-PKCS1-
v1_5 or RSAES-OAEP (see [18]) SHALL be used for RSA encryption. RSASSA-PKCS1-v1_5 or
RSASSA-PSS (see [18]) SHALL be used for RSA signing.

\begin{code}

[ttype|

                     Table 120 padding_type
    +-----------------------+------------+-----------------+
    |UID                    |Name        |Format           |
    +-----------------------+------------+-----------------+
    |00 00 00 05 00 00 04 06|padding_type|Enumeration_Type,|
    |                       |            |0,               |
    |                       |            |15               |
    +-----------------------+------------+-----------------+

|]

\end{code}

The enumeration values are associated as defined in Table 121.

    Table 121 padding_type Enumeration Values
    +-----------------+-------------------+
    |Enumeration Value|Associated Value   |
    +-----------------+-------------------+
    |0                |None               |
    +-----------------+-------------------+
    |1                |None               |
    +-----------------+-------------------+
    |2                |RSAES-PKCS1-v1_5   |
    +-----------------+-------------------+
    |3                |RSAES-OAEP         |
    +-----------------+-------------------+
    |4                |RSASSA-PKCS1-v1_5  |
    +-----------------+-------------------+
    |5-15             |Reserved           |
    +-----------------+-------------------+

5.1.3.63 password

This max bytes type, with a size limitation of 32, is used in the C_PIN table.

\begin{code}

[ttype|

                   Table 122 password
    +-----------------------+--------+------------+
    |UID                    |Name    |Format      |
    +-----------------------+--------+------------+
    |00 00 00 05 00 00 02 0C|password|Simple_Type,|
    |                       |        |max_bytes,  |
    |                       |        |32          |
    +-----------------------+--------+------------+

|]

\end{code}
--------------------------------------------------------------------------------
5.1.3.64 protect_types

This set is used to identify the protection mechanisms in operation when a column is identified as
hidden.

\begin{code}

[ttype|

                   Table 123 protect_types
    +-----------------------+-------------+--------------+
    |UID                    |Name         |Format        |
    +-----------------------+-------------+--------------+
    |00 00 00 05 00 00 1A 05|protect_types|Set_Type,     |
    |                       |             |0,            |
    |                       |             |255           |
    +-----------------------+-------------+--------------+

|]

\end{code}

The empty set indicates that keys are not hidden. The values of the set are all applied to the protected
value. The set values are assigned in [3].
--------------------------------------------------------------------------------
5.1.3.65 reencrypt_request

This enumeration is used to identify the host re-encryption request value.





\begin{code}

[ttype|

                     Table 124 reencrypt_request
    +-----------------------+-----------------+-----------------+
    |UID                    |Name             |Format           |
    +-----------------------+-----------------+-----------------+
    |00 00 00 05 00 00 04 13|reencrypt_request|Enumeration_Type,|
    |                       |                 |1,               |
    |                       |                 |16               |
    +-----------------------+-----------------+-----------------+

|]

\end{code}

The enumeration values are associated as defined in 5.7.2.2.14.

5.1.3.66 reencrypt_state

This enumeration type identifies the present re-encryption state for an LBA range.

\begin{code}

[ttype|

                     Table 125 reencrypt_state
    +-----------------------+---------------+-----------------+
    |UID                    |Name           |Format           |
    +-----------------------+---------------+-----------------+
    |00 00 00 05 00 00 04 14|reencrypt_state|Enumeration_Type,|
    |                       |               |1,               |
    |                       |               |16               |
    +-----------------------+---------------+-----------------+

|]

\end{code}

The enumeration values are associated as defined in Table 126.

   Table 126 reencrypt_state Enumeration Values
    +-----------------+-------------------+
    |Enumeration Value|Associated Value   |
    +-----------------+-------------------+
    |1                |Idle               |
    +-----------------+-------------------+
    |2                |Pending            |
    +-----------------+-------------------+
    |3                |Active             |
    +-----------------+-------------------+
    |4                |Completed          |
    +-----------------+-------------------+
    |5                |Paused             |
    +-----------------+-------------------+
    |6-16             |Reserved           |
    +-----------------+-------------------+

5.1.3.67 reset_types

This Set type identifies the various TCG reset options available.

\begin{code}

[ttype|

                     Table 127 reset_types
    +-----------------------+-----------+--------------+
    |UID                    |Name       |Format        |
    +-----------------------+-----------+--------------+
    |00 00 00 05 00 00 1A 01|reset_types|Set_Type,     |
    |                       |           |0,            |
    |                       |           |31            |
    +-----------------------+-----------+--------------+

|]

\end{code}

The Set values are associated as defined in Table 128.


    Table 128 reset_types Set Values
    +-----------------+----------------+
    |Enumeration Value|Associated Value|
    +-----------------+----------------+
    |0                |Power Cycle     |
    +-----------------+----------------+
    |1                |Hardware        |
    +-----------------+----------------+
    |2                |HotPlug         |
    +-----------------+----------------+
    |3-15             |Reserved        |
    +-----------------+----------------+
    |16-31            |Vendor Unique   |
    +-----------------+----------------+

5.1.3.68 Seconds

Name-value pair that has a Name of "5" and takes seconds_enum as the value.


\begin{code}

[ttype|

                       Table 129 Seconds
    +-----------------------+-------+-------------------------+
    |UID                    |Name   |Format                   |
    +-----------------------+-------+-------------------------+
    |00 00 00 00 00 00 00 00|Seconds|Name_Value_Uinteger_Type,|
    |                       |       |5,                       |
    |                       |       |seconds_enum             |
    +-----------------------+-------+-------------------------+

|]

\end{code}

5.1.3.69 seconds_enum

Used in association with the Seconds name-value pair.

\begin{code}

[ttype|

                     Table 130 seconds_enum
    +-----------------------+------------+-----------------+
    |UID                    |Name        |Format           |
    +-----------------------+------------+-----------------+
    |00 00 00 05 00 00 04 1B|seconds_enum|Enumeration_Type,|
    |                       |            |0,               |
    |                       |            |59               |
    +-----------------------+------------+-----------------+

|]

\end{code}

5.1.3.70 SPTemplates_object_ref

The SPTemplates_object_ref type describes a uidref to an object in the SPTemplates table.

\begin{code}

[ttype|

                     Table 131 SPTemplates_object_ref
    +-----------------------+----------------------+-----------------------------+
    |UID                    |Name                  |Format                       |
    +-----------------------+----------------------+-----------------------------+
    |00 00 00 05 00 00 0C 01|SPTemplates_object_ref|Restricted_Reference_Type{6},|
    |                       |                      |uidref{SPTemplatesTableUID}  |
    +-----------------------+----------------------+-----------------------------+

|]

\end{code}

5.1.3.71 SSC

This is a list of names used to represent the SSCs that a TPer supports.

\begin{code}

[ttype|

                     Table 132 SSC
    +-----------------------+----+--------------+
    |UID                    |Name|Format        |
    +-----------------------+----+--------------+
    |00 00 00 05 00 00 08 03|SSC |List_Type,    |
    |                       |    |*,            |
    |                       |    |name          |
    +-----------------------+----+--------------+

|]

\end{code}

5.1.3.72 symmetric_mode

Defines the mode to be used with an AES credential.

\begin{code}

[ttype|

                     Table 133 symmetric_mode
    +-----------------------+--------------+-----------------+
    |UID                    |Name          |Format           |
    +-----------------------+--------------+-----------------+
    |00 00 00 05 00 00 04 0A|symmetric_mode|Enumeration_Type,|
    |                       |              |0,               |
    |                       |              |23               |
    +-----------------------+--------------+-----------------+

|]

\end{code}


The enumeration values are associated as defined in Table 134.

 Table 134 symmetric_mode Enumeration Values
    +-----------------+----------------+
    |Enumeration Value|Associated Value|
    +-----------------+----------------+
    |0                |ECB             |
    +-----------------+----------------+
    |1                |CBC             |
    +-----------------+----------------+
    |2                |CFB             |
    +-----------------+----------------+
    |3                |OFB             |
    +-----------------+----------------+
    |4                |GCM             |
    +-----------------+----------------+
    |5                |CTR             |
    +-----------------+----------------+
    |6                |CCM             |
    +-----------------+----------------+
    |7                |XTS             |
    +-----------------+----------------+
    |8                |LRW             |
    +-----------------+----------------+
    |9                |EME             |
    +-----------------+----------------+
    |10               |CMC             |
    +-----------------+----------------+
    |11               |XEX             |
    +-----------------+----------------+
    |12-23            |Reserved        |
    +-----------------+----------------+

5.1.3.73 symmetric_mode_media

Defines the modes availableto be used with AES for user data encryption.

\begin{code}

[ttype|

                     Table 135 symmetric_mode_media
    +-----------------------+--------------------+-----------------+
    |UID                    |Name                |Format           |
    +-----------------------+--------------------+-----------------+
    |00 00 00 05 00 00 04 03|symmetric_mode_media|Enumeration_Type,|
    |                       |                    |0,               |
    |                       |                    |23               |
    +-----------------------+--------------------+-----------------+

|]

\end{code}


The enumeration values are associated as defined in Table 134.

Table 136 symmetric_mode_media Enumeration Values
    +-----------------+-----------------+
    |Enumeration Value|Associated Value |
    +-----------------+-----------------+
    |0                |ECB              |
    +-----------------+-----------------+
    |1                |CBC              |
    +-----------------+-----------------+
    |2                |CFB              |
    +-----------------+-----------------+
    |3                |OFB              |
    +-----------------+-----------------+
    |4                |GCM              |
    +-----------------+-----------------+
    |5                |CTR              |
    +-----------------+-----------------+
    |6                |CCM              |
    +-----------------+-----------------+
    |7                |XTS              |
    +-----------------+-----------------+
    |8                |LRW              |
    +-----------------+-----------------+
    |9                |EME              |
    +-----------------+-----------------+
    |10               |CMC              |
    +-----------------+-----------------+
    |11               |XEX              |
    +-----------------+-----------------+
    |12-22            |Reserved         |
    +-----------------+-----------------+
    |23               |Media Encryption |
    +-----------------+-----------------+

5.1.3.74 table_kind

Defines the kinds of tables.

\begin{code}

[ttype|

                     Table 137 table_kind
    +-----------------------+----------+-----------------+
    |UID                    |Name      |Format           |
    +-----------------------+----------+-----------------+
    |00 00 00 05 00 00 04 15|table_kind|Enumeration_Type,|
    |                       |          |1,               |
    |                       |          |8                |
    +-----------------------+----------+-----------------+

|]

\end{code}

The enumeration values are associated as defined in Table 138.

    Table 138 table_kind Enumeration Values
    +-----------------+-------------------+
    |Enumeration Value|Table Type         |
    +-----------------+-------------------+
    |1                |Object             |
    +-----------------+-------------------+
    |2                |Byte               |
    +-----------------+-------------------+
    |3-8              |Reserved           |
    +-----------------+-------------------+

5.1.3.75 table_or_object_ref

This alternative type defines a reference to either the uid of a table or the
uid of some object, or the UID of "ThisSP".

\begin{code}

[ttype|

                     Table 139 table_or_object_ref
    +-----------------------+-------------------+-----------------+
    |UID                    |Name               |Format           |
    +-----------------------+-------------------+-----------------+
    |00 00 00 05 00 00 06 06|table_or_object_ref|Alternative_Type,|
    |                       |                   |object_ref,      |
    |                       |                   |table_ref        |
    +-----------------------+-------------------+-----------------+

|]

\end{code}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
5.1.3.76 Table_object_ref

The Table_object_ref type describes a uidref to an object in the Table table.



\begin{code}

[ttype|

                        Table 140 Table_object_ref
    +-----------------------+----------------+-----------------------------+
    |UID                    |Name            |Format                       |
    +-----------------------+----------------+-----------------------------+
    |00 00 00 05 00 00 0C 09|Table_object_ref|Restricted_Reference_Type{6},|
    |                       |                |uidref {TableTableUID}       |
    +-----------------------+----------------+-----------------------------+

|]

\end{code}

5.1.3.77 table_ref

Type used for referencing a table.

\begin{code}

[ttype|

                        Table 141 table_ref
    +-----------------------+---------+-------------------------+
    |UID                    |Name     |Format                   |
    +-----------------------+---------+-------------------------+
    |00 00 00 05 00 00 0F 03|table_ref|General_Reference_Type{9}|
    +-----------------------+---------+-------------------------+

|]

\end{code}

5.1.3.78 Template_object_ref

The Template_object_ref type describes a uidref to an object in the Admin SP's Template table.

\begin{code}

[ttype|

                 Table 142 Template_object_ref
    +-----------------------+-------------------+-----------------------------+
    |UID                    |Name               |Format                       |
    +-----------------------+-------------------+-----------------------------+
    |00 00 00 05 00 00 0C 08|Template_object_ref|Restricted_Reference_Type{6},|
    |                       |                   |uidref{TemplateTableUID}     |
    |                       |                   |                             |
    +-----------------------+-------------------+-----------------------------+

|]

\end{code}
--------------------------------------------------------------------------------
5.1.3.79 type_def

The type_def type describes the format of the Type table's Format column. The
value in the Format column of this type SHALL be encoded and parseable based
on the notation description of the type formats (see 5.1.1).

\begin{code}

[ttype|

                   Table 143 type_def
    +-----------------------+--------+------------+
    |UID                    |Name    |Format      |
    +-----------------------+--------+------------+
    |00 00 00 05 00 00 02 03|type_def|Simple_Type,|
    |                       |        |max_bytes,  |
    |                       |        |*           |
    +-----------------------+--------+------------+

|]

\end{code}
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
\begin{code}



type Core_uidref               = UID



implementationDependant_AC_elementSize :: Int
implementationDependant_AC_elementSize = 32

implementationDependant_max_columns :: Int
implementationDependant_max_columns = 32

imp' :: Implementation
imp' = Implementation { __AC_elementSize =
                         implementationDependant_AC_elementSize
                      , __max_columns =
                         implementationDependant_max_columns
                      }

ssc_AC_elementMinSize :: Maybe Int
ssc_AC_elementMinSize = Just 23 -- Opal.  Or, Nothing would work.

ssc_min_max_columns :: Maybe Int
ssc_min_max_columns = Nothing -- Opal doesn't have CreateTable, so ...

_max_columns :: TPer Int
_max_columns = __max_columns <$> asks implementation

_min_max_columns :: TPer (Maybe Int)
_min_max_columns = __min_max_columns <$> asks ssc



ssc' :: SSC
ssc' = SSC { __AC_elementMinSize = ssc_AC_elementMinSize
           , __min_max_columns   = ssc_min_max_columns
           }




core' :: Core
core' = Core




newtype Core_General_Reference_Type_Byte_Row =
        Core_General_Reference_Type_Byte_Row Natural

newtype Core_General_Reference_Type_Object =
        Core_General_Reference_Type_Object Core_uidref

mkCore_General_object_ref :: Core_uidref -> Maybe Core_General_Reference_Type_Object
mkCore_General_object_ref objectUID =
    Just $ Core_General_Reference_Type_Object objectUID

data Core_table_kind = ByteTable | ObjectTable

-- GADT for references to tables
data General_Reference_Table_Type (a::Core_table_kind) c where
  General_Reference_Table_Type_Byte   :: Core_uidref -> General_Reference_Table_Type 'ByteTable   Core_uidref
  General_Reference_Table_Type_Object :: Core_uidref -> General_Reference_Table_Type 'ObjectTable Core_uidref


---    deriving (Eq, Show, Ord)


type Core_Reference_UID_List = [UID]

data Core_Restricted_Reference_Type_Object =
     Core_Restricted_Reference_Type_Object Core_Reference_UID_List Core_uidref

mkCore_Restricted_object_ref ::
    Core_Reference_UID_List
 -> Core_uidref
 -> Maybe Core_Restricted_Reference_Type_Object
mkCore_Restricted_object_ref tableUIDs objectUID
    | uidUpper objectUID `elem` map uidLower tableUIDs =
          Just $ Core_Restricted_Reference_Type_Object tableUIDs objectUID
    | otherwise =
          Nothing

mkCore_Restricted_object_ref_To ::
    Core_uidref
 -> Core_uidref
 -> Maybe Core_Restricted_Reference_Type_Object
mkCore_Restricted_object_ref_To tableUID = mkCore_Restricted_object_ref [tableUID]



env :: Env
env = Env core' imp' ssc'



-- valid :: TPer Bool
-- valid = andM validityChecks

-- validityChecks :: [TPer Bool]
-- validityChecks =
--     [ _AC_elementSizeValid
--     , pure True -- FIXME: more checks
--     ]

-- _AC_elementSizeValid :: TPer Bool
-- _AC_elementSizeValid = do
--     maybeMinSize <-_AC_elementMinSize
--     case maybeMinSize of
--       Just minSize -> do
--           size <- _AC_elementSize
--           pure $ minSize <= size
--       Nothing -> pure False

max_columns :: TPer Int
max_columns = do
    maxColumns <- _max_columns
    _min_max_columns >>= (pure . (min maxColumns) . (maybe 0 id))



\end{code}
\end{document}
