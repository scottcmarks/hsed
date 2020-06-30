{-# LANGUAGE NoImplicitPrelude #-}


{-|
Module      : System.SED.MCTP.Common.Call
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Parse and generate method invocations and responses.

-}

module System.SED.MCTP.Common.Call where

import           RIO
import           Test.QuickCheck                   hiding (generate)

import           System.SED.MCTP.Common.StreamItem
import           System.SED.MCTP.Common.Token
import           System.SED.MCTP.Common.UID
import           System.SED.MCTP.Common.Value

{-
3.2.4.1 Method Syntax

A method invocation is made up of the following parts:

1. Method Header Ð The method header is made up of the InvokingID and the MethodID, and
identifies what method is being called and on what the method is operating.
1. InvokingID Ð This is the 8-byte UID of the table, object, or SP upon which the method is
being invoked.
a. For SP methods invoked within a session, the InvokingID SHALL be 0x00 0x00 0x00
0x00 0x00 0x00 0x00 0x01, which is used to signify Òthis SPÓ.
b. For methods invoked at the Session Manager Layer, the InvokingID SHALL be 0x00
0x00 0x00 0x00 0x00 0x00 0x00 0xFF, known as the "SMUID".
c. For other methods, this is the 8-byte UID of the table or object upon which the method
is being invoked.



2. MethodID Ð This is the 8-byte UID of the method being invoked.
a. For methods invoked within a session, this SHALL be the UID column value of the
object that represents the methed as assigned in the MethodID table.
b. For Session Manager Layer methods, this SHALL be the UID as assigned in Table
241. There SHALL NOT be rows in the MethodID table that represent these methods.
2. Method Parameters Ð This is a list of the parameters submitted to the method. These
parameters MAY be one of two types.
1. Required parameters Ð These parameters are required to be submitted to the method
invocation. These parameters SHALL appear first in the method invocation, ahead of any
optional parameters, and SHALL be submitted in the order in which they are listed in a
method's signature as defined in this specification.
2. Optional parameters Ð These parameters SHALL NOT be required to be submitted to the
method invocation. Optional parameters that are submitted to a method invocation SHALL
be submitted after all required parameters, and SHALL appear in the order defined in this
specification.
a. Optional parameters are submitted to the method invocation as Named value pairs.
The Name portion of the Named value pair SHALL be a uinteger. Starting at zero,
these uinteger values are assigned based on the ordering of the optional parameters
as defined in this document.
b. The first optional parameter in a method signature SHALL be represented by the
"name" zero (0x00) in the Named value pair when that method is invoked, and SHALL
thus have the format "0x00 = value" when that method is invoked.
c. Each optional parameter in a method signature after the first SHALL be represented by
the uinteger of the previous optional parameter indicated in the method's signature
incremented by one. Thus, the second optional parameter in an invocation of a
particular method SHALL have the format "0x01 = value".





3.2.4.2 Method Encoding

A method invocation is made up of a sequence of tokens that are sent from the application to the TPer,
and from the TPer to the host for Session Manager method responses, as follows:

1. Call token Ð A Call token is transmitted to indicate that a method invocation is to follow.
2. Method Header Ð This is the encoding of the InvokingID and the MethodID. This value is:
1. InvokingID Ð The InvokingID is a bytes token representing the 8-byte value that is the
first part of the Method Header being transmitted.
2. MethodID Ð The MethodID is a bytes token representing the 8-byte value that is the
second part of the Method Header being transmitted.
3. Parameters Ð The parameters are submitted to a method invocation as a list. The parameter
list follows this format:
1. Start List token Ð This identifies the beginning of the list of parameters.
2. Required parameters Ð This is the set of parameters that are required to be sent to a
method. The encoding of required parameters is dependent on the type associated
with that parameter, as defined by the method signature and the context in which the
method is being invoked.
3. Optional parameters Ð this is the set of zero or more Named value pairs that MAY be
sent to the method to represent the method's optional parameters. Each optional
parameter SHALL be made up of the following parts:
1. The Start Name token, which indicates the start of this optional parameter.



2. The encoded name, which in the case of optional parameters is a uinteger.
3. The encoded value. The encoding of the parameter value is dependent on the
type associated with that parameter, as defined by the method signature and the
context in which the method is being invoked.
4. The End Name token, which indicates the end of this optional parameter
4. End List token Ð this identifies the end of the list of parameters.
4. End Of Data token Ð The End of Data token is transmitted to indicate that the method
invocation is ended.
5. Status Code List Ð This is the status list, a list of values of type uinteger, which contains the
status codes expected from the host's invocation of the method. These status values are
encoded using List tokens.
1. The first value in the list SHALL be 0x00 for a method that the host expects to
complete properly. For a method that the host wishes to abort, the host SHALL NOT
include a value that is 0x00 as the first value in the status list, which SHALL cause the
TPer to abort processing on that method and return that non-0x00 value as the first
value in the status list.
2. The second and third values in the status list are reserved, and are defined in this
specification to be 0x00 and 0x00 and SHOULD be ignored by the TPer.









Except for the Session Manager methods, each method call SHALL have a response that is a
sequence of tokens that are sent from the TPer to the host as follows.

1. Start List token Ð This identifies the beginning of the list of results.
2. Output Results Ð This is zero or more token sequences that represent the response to the
method, as defined in the method signature.
3. End List token Ð This identifies the end of the list of results.
4. End Of Data Ð The End of Data token is transmitted to indicate that the result list has ended.
5. Status List Ð This is the status list, a list of values of type uinteger, which contains the status
codes expected from the host's invocation of the method. These status values are encoded
using List tokens.
i. If the host invoked the method with a status list whose first uinteger was 0x00, then the
first value in the status list SHALL always be the status of the method, as described in
5.1.5. If the host invoked the method with a status list whose first byte was not 0x00,
then the first value in the status list SHALL contain the same value that was sent by the
host in the first uinteger of the host's status list.
ii. The second and third values in the list are uintegers reserved for use by the TCG, and
are defined in this specification to be 0x00 and 0x00 and SHOULD be ignored by the
host.
iii. Additional values MAY be returned in the status list, as long as the first three values in
the status list are returned as required by this specification.







Method responses SHALL be returned for all method invocations or method invocation attempts within
a session. Responses for method invocation attempts of methods not recognized by the TPer or that
result in some other failure condition MAY return an empty method result (the output result is an empty
list) and an error code. Unrecognized method invocation attempts outside of Regular sessions SHALL
be ignored by the TPer Ð in these cases, no response is sent.

Session Manager protocol layer method invocations that are recognized but fail SHALL result in the
normal response format for that method, accompanied by an error status code. Session startup
methods that fail in this way SHALL have returned the expected method response, but that method


SHALL have only the identifying parameters (Host, SP) and an error status code. If the identifying
parameters (particularly the Host parameter) are invalid (i.e. of the incorrect type), the TPer MAY ignore
the method.

The TPer MAY begin sending the response as soon as enough parameters have been received to
prepare a response.

3.2.4.3 Method Result Retrieval Protocol

A method is invoked by tokenizing the method call and its parameters as described in previous
sections, using the token encoding format and Subpacket-Packet-ComPacket format. The host sends
the ComPacket to the TPer in an IF-SEND command. Multiple IF-SEND commands MAY be required
to encompass the entirety of a method invocation or series of method invocations and their related
data.

The host then polls the TPer by transmitting IF-RECV commands. When the TPer has packaged its
response, it transmits the tokenized results to the host in the payload of an IF-RECV command.
Multiple IF-RECV commands MAY be required to retrieve all of the results of a particular method
invocation or series of method invocations.

For additional information on the operation of the IF-SEND and IF-RECV commands, see the
descriptions for those commands as detailed in the appropriate interface specifications.
-}


data MethodHeader = MethodHeader{ _invokingID :: UID
                                , _methodID   :: UID
                                }
    deriving(Show,Eq)

data Parameters = Parameters { _required::[Value]
                             , _optional::[NamedValue]  -- FIXME: OptionalParameter, because the Name must be (Unsigned i), i <- 0..
                             }
    deriving(Show,Eq)
data MethodCall = MethodCall { _header     :: MethodHeader
                             , _parameters :: Parameters
                             , _statusCode :: Natural
                             }
    deriving(Show,Eq)


instance StreamItem MethodHeader where
    parser = MethodHeader <$> parser <*> parser
    generate (MethodHeader i m) = generate i <> generate m

instance StreamItem Parameters where
    parser = Parameters <$> many parser <*> many parser
    generate (Parameters r o) = mconcat $ map generate r <> map generate o

d :: Natural -> Value
d = D . Datum . Unsigned

instance StreamItem MethodCall where
    parser = do
        Call      <- parser
        mh        <- parser
        p         <- parser
        EndOfData <- parser
        status    <- parser
        case status of
          (List [ D (Datum (Unsigned sc))
                , D (Datum (Unsigned  0))
                , D (Datum (Unsigned  0))]) ->
              pure $ MethodCall mh p sc
          (List l) ->
              fail $ "Status code list should be 3 uintegers, \
                     \with the second and third being zero, but is " <> show l

    generate (MethodCall mh p sc) =
          generate Call
       <> generate mh
       <> generate p
       <> generate EndOfData
       <> generate ( List [ D (Datum (Unsigned sc))
                          , D (Datum (Unsigned  0))
                          , D (Datum (Unsigned  0))
                          ] )



instance Arbitrary MethodHeader where
    arbitrary = MethodHeader <$> arbitrary <*> arbitrary

instance Arbitrary Parameters where
    arbitrary = Parameters <$> arbitrary <*> arbitrary

instance Arbitrary MethodCall where
    arbitrary = MethodCall <$> arbitrary <*> arbitrary <*> arbitrary
