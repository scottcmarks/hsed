# hsed
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
