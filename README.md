# hsed
Goals drift.

In the commercial, common, nearly-"static" case, almost nothing is writable.
One cannot create new tables or types; probably not even add or delete rows.
Mostly just set passwords and fill out ranges in the Locking table.
In that configuration, all the types are known, and hence can be erased,
and the purpose of the types is to prove the internal consistency of the
configuration.
In the general case, almost everything is writable.
Tables and types and rows can come and go, subject to consistency,
so the refinement of values to dependent types must be done by runtime validation.
The proof of the general case should lift to the static case.


Short-term TODO list

Finish ColumnTypes
- Parse Format field of Type Table row tables
- add rows to monad
- generate Type Table at the end
- Template Haskell for Enumeration tables (one special case)

Type checking on Set methods

Generating the TPer and SSC overlapping specs

Further thoughts on Formats...

Most immediately, can parse the field and generate the byte encoding of the type which is the reification of the described Core_Type.
Current thought is that the parsing could produce something like a Proxy (<Core_Type>) (e.g. Proxy Enumeration_Type).
Since we want to subsequently generate the byte encoding, looks like the parsing result should be a StreamItem (i.e. have parse and generate methods).

format column





Long-term notes:
command parsing and generation all functional
device itself must be in IO(SelfEncryptingDrive), because successive reads will return different answers
- other processes can affect answers
- the device itself can be power cycled (mere power down could just cause an Exception)

does a SelfEncryptingDrive instance map ComPacket -> Either ComPacket IOError
definitely have an acquire-use-release pattern
- acquire device as in acquire "disk3"
- release device


data Final = Uint | Int | Bytes

data Datum = Final | ContinuedBytes

data Sequence = StartName | EndName | StartList | EndList

data Control = Call | EndOfData | EndOfSession | StartTransaction | EndTransaction

data Empty = EmptyAtom

data Token = Datum | Sequence | Control | Empty

---- parse(Token) ---

data NamedValue = NamedValue Name Value -- StartName *> Name Value <* EndName

newtype Name = Final

data List = [Value] -- StartList *> many Value <* EndList

data Value = Final | NamedValue | List

--- parse(Value) ---



The type of $(mx "foo") should be (HasSize a, IsString a, KnownNat l, 3 <= l) => MaxSize l a

coercion should work from BoundedSize l1 u1 a to BoundedSize l2 u2 a so long as
easy case) l2 <= l1 and u1 <= u2, so that nothing needs to be done; or
easy case) (l1..u1) and (l2..u2) don't intersect, so there are no values; or
hard case) runtime check required to make sure size is between l2 and u2.
