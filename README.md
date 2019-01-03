# hsed
command parsing and generation all functional
device itself must be in IO(SelfEncryptingDrive), because successive reads will return different answers
- other processes can affect answers
- the device itself can be power cycled (mere power down could just cause an Exception)

does a SelfEncryptingDrive instance map ComPacket -> Either ComPacket IOError
definitely have an acquire-use-release pattern
- acquire device as in acquire "disk3"
- release device
