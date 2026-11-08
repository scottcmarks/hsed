# The LAN leg's wire: the simulator over mh_transport frames

The reduced client contract design §7 item 4 promises: what a client of `hsed-sim` sends and receives, so that a
client other than hsed's own (`Hsed.Transport.Mhtx.Client`) can drive the simulated TPer. The frames are
mh_transport's (its `SPEC.md`, v1, unchanged) with hsed's application layer over them (design §6.3); the hub is
`Hsed.Transport.Mhtx.Hub`, served by `hsed-sim` on 127.0.0.1 alone (Scott's answer Q1, design §11 M5c).

Every rule names its source. **S** is `~/mh_transport/SPEC.md` (v1, 129 lines), cited by line; the `.dart` files
are `~/mh_transport/packages/mh_transport/lib/src/`'s; **design** is `docs/spec-as-source.md`, cited by section
and line; the Haskell modules are hsed's (`Frame` is `Hsed.Transport.Mhtx.Frame`, `Conn` is
`Hsed.Transport.Mhtx.Conn`, `Hub` and `Client` likewise). Where design §6.3 is silent, M5c decides, and says so
("M5c"); the hub's and the client's rules, first written from M5c's plan (its protocol rules), are held here to
`Hub`, `Client` and `hsed-sim` as built.

## Connecting

- **One URL**: `ws://127.0.0.1:PORT/ws`. `hsed-sim` listens on 127.0.0.1 alone, a `--listen` naming any other
  host refused with exit 2 (Scott's answer Q1, until M6 brings TLS and a required token); `--listen 127.0.0.1:0`,
  the default, takes an ephemeral port, and `hsed-sim` prints one line, `PORT=<n>`, on stdout (design §7 item 5 as
  corrected). Agents and tests bind port 0 alone, never 8765 (D6). There is no `wss://` at M5c (no wuss).
- **A token**, where the hub was started with `--token-file`: the request's `token` query parameter, given once,
  `ws://127.0.0.1:PORT/ws?token=T` (S L92-95, MHTX's form). The hub compares it by digest in constant time and never
  logs it; see [What the hub allows](#what-the-hub-allows).
- **No browser**: a request carrying an `Origin` header (a web page's websocket or cross-origin POST; hsed's client,
  the Dart conformance client and the Python fixture's tests send none), or whose `Host` names a host other than
  `127.0.0.1` or `localhost` (a DNS name rebound to 127.0.0.1), is answered 403 before anything else, on `/ws` and
  the hooks alike (the M5c review; mh_transport's hubs check neither, so this is hardening, not the protocol).
- **Size**: a frame and a message are each at most 2 MiB, both ways (`Conn.messageLimit`; websockets' own
  defaults bound nothing, its `Connection/Options.hs:62-69`). A message over it is closed with 1009, see
  [Close codes](#close-codes). No compression is negotiated; a text message's bytes are not held to UTF-8 by the
  transport but by the frame's reader (below).
- **The heartbeat is JSON** (`ping` and `pong` frames, below), not WebSocket control frames: a WebSocket ping is
  answered by the websockets library and counts for nothing else (S L57-66; the Python fixture turns uvicorn's own
  off, `conformance_fixture.py:69`).

## Frames

- Every frame is one WebSocket text message holding one JSON object with a string `type`; hsed sends nothing else:
  no binary message, no WebSocket-level ping (S L18-19).
- A receiver ignores, with no reply, a frame it cannot decode (not UTF-8, not JSON, a JSON array or scalar:
  `protocol.dart:29-37`), a frame without a string `type`, and a type it does not know (S L19-21; design §6.3
  :512; `Frame.fromClient`, `Frame.fromServer`).
- Binary messages are ignored, but count as liveness: the hub's silence timer is reset by any message, binary and
  junk included (S L21 and L58-59, "has sent nothing (ping or otherwise)"; `hub.dart:84-86`).
- `hello`, `ping` and `pong` belong to the transport and never reach the application; the `/emit` hook refuses them
  (S L23-29).
- `sed.req`, `sed.rsp` and `sed.evt` are hsed's application layer, its names and payload shapes (S L31-33; design
  §6.3 :499-514).
- SPEC's model has clients send nothing the server depends on but the heartbeat (S L11-14). hsed's `sed.req`
  extends that model as an application layer on the same frames; no conformance check is affected.
- JSON as hsed reads and writes it (M5c): members a frame does not name are ignored; a member written twice is read
  as aeson reads it (the first; Dart and Python keep the last), and hsed writes none twice; numbers are read as
  aeson reads them, which takes `2.0` and `1e3` for the integers 2 and 1000, each held to its range, and none read
  past 64 bits (`Frame`'s readers). hsed writes integers as JSON integers; its own frames (`hello`, `pong`,
  `sed.rsp`, `sed.evt`, and the view's) with `type` first and their members in the order this page gives them, the
  objects inside a frame (`app`, a query, a view) in their keys' order; a broadcast `/emit` was given is the
  emitted object with `seq` inserted, every member in key order (`Frame.broadcastText`), `type` wherever it falls.
  Member order means nothing to a reader (S L18-19).

## hello, ping and pong

- **hello is the first frame on every connection**, before anything else (S L37). The hub reads `seq`, registers
  the connection and queues hello in one STM transaction, so that no broadcast falls between them
  (`hub.dart:66-77` does the same with no `await` between; the Python hub awaits, `mh_transport.py:101-105`).
- **hello's members** (S L45-53): `v` 1; `server_id`, chosen once per process, 12 characters of `[a-z0-9]`
  (`hub.dart:170-173`); `seq`, the broadcasts so far, 0 at start; `heartbeat.interval_ms` and
  `heartbeat.timeout_ms`, JSON integers (`protocol.dart:94-98` rejects a hello whose numbers are not ints: `200.0`
  will not do); `app`, a free object, which hsed's hub fills with `app.sed`:

  ```json
  {"type":"hello","v":1,"server_id":"q7d2m0s8x1kz","seq":0,"heartbeat":{"interval_ms":20000,"timeout_ms":60000},
   "app":{"sed":{"allows":["peek","release","reset"],"base_comid":4100,"errata":"scott",
   "impl":"hsed-sim Version 0.1.0.1 [claude/m5c 612411b]","n_comids":1,"role":"tper","setup":{"hold_ifrecvs":0,
   "inactive_comid_discard":false,"locking_ofs":"manufactured","msid":"32 bytes, sha256 a6c4ca3911e4",
   "params":"<64 hex digits>","units":"bytes"},"ssc":"opal-2.30","units":"bytes","v":1}}}
  ```

- **`app.sed`** (design §6.3 :503; `Frame.SedApp`, `Frame.sedAppOf`, `Frame.sedAppValue`), what TPer answers:

  | member | value |
  |---|---|
  | `v` | 1, the layer's version; another is refused |
  | `role` | `tper` (the in-process simulator's `Describe`) |
  | `impl` | what answers: `hsed-sim` and its version, as `hsed-sim --version` prints it (`Version 0.1.0.1 [<branch> <commit>]`) |
  | `units` | `bytes` or `blocks512`: what IF-SEND's and IF-RECV's lengths are rounded to in the TPer (design §6.2); `len` itself is always bytes |
  | `ssc`, `errata` | `opal-2.30`, `scott` |
  | `base_comid`, `n_comids` | the static ComIDs: the base and how many |
  | `allows` | the ops beyond commands and `describe` the hub allows: `peek` and `release` with `--allow-remote-control`, `reset` with `--allow-remote-reset`; each word once |
  | `setup` | how the simulator was started (`Hsed.Sim.View.SimSetup`, `setupOf`), named after `hsed-sim`'s flags: `units`, `hold_ifrecvs`, `locking_ofs`, `inactive_comid_discard`; `sim_sid_pin` and `msid`, each by its length and the first 12 hex digits of its SHA-256 alone, published only with `--allow-remote-control`; `profile`, the Level 0 profile's digest; `params`, the digest of every other parameter (the secrets and the profile's path left out) |

  A member with nothing to say (`ssc`, `errata`, `base_comid`, `n_comids`, `setup`; `allows` when empty) is left
  out; a reader takes it absent or `null`. Design §6.3's own hello, which has neither `allows` nor `setup`, reads
  and writes back unchanged.
- **The client pings**: `{"type":"ping"}` every `interval_ms` after hello (S L57; `client.dart:412-415`).
- **The hub answers each ping** with `{"type":"pong"}`, whatever the connection's age and whatever request of the
  connection is in progress: the connection's reader queues the pong itself (S L58 and L64).
- **The hub closes a silent connection**: one that has sent nothing for `timeout_ms` is closed with code 4000 and
  reason `heartbeat timeout` (S L58-60; `protocol.dart:26`). The hub sweeps every `interval_ms`, as `hub.dart:149-168`
  does, so a silent connection is closed between `timeout_ms` and `timeout_ms + interval_ms` after its last message.
- **The HTTP server's own timeout never comes first**: `hsed-sim` sets warp's connection timeout to
  max 30 (ceil((`timeout_ms` + `interval_ms`) / 1000) + 1) seconds (`Hub.warpTimeout`, `Hub.hs:448`), 81 s at the
  default heartbeat, 30 s at 200/600 ms. warp tickles its timeout at every byte a websocket sends or receives, which
  undoes the pause it offers (`Warp.pauseTimeout`), and kills a connection idle through two of its periods: at its
  own 30 s it would cut a client silent for 30 to 60 s, which the default heartbeat keeps until 60 s, with no
  close code, before the hub's 4000 (`SoakSpec` shows it).
- **The client's proof of life**: any inbound frame; a client that hears nothing for `interval_ms` plus a grace
  (10 s) drops the connection and reconnects (S L60-62; `client.dart:420-429`, which counts decoded frames alone;
  hsed's client counts any message).
- **A client that receives a ping answers pong** (S L65).
- **Broadcasts carry `seq`**: every frame the hub broadcasts carries `seq`, the hub's running count, incremented once
  per broadcast and shared by every recipient (S L70-71; `hub.dart:103-111`), the hub bumping it and queueing the
  frame to every connection in one STM transaction; the counter advances even when no client is connected (S L77).
  hsed's broadcasts are `sed.evt` and, with the hooks, what `/emit` is given. hello's `seq` is the last one sent, so
  that a client can judge what it missed (below).

## Requests, responses and events

### sed.req

`{"type":"sed.req","id":N,"op":OP, ...}`, the op's members after it (design §6.3 :504-506; `Frame.SedReq`,
`Frame.reqText`, read by `Frame.fromClient`):

| `op` | members | what it asks |
|---|---|---|
| `if_send` | `proto` (0-255), `comid` (0-65535), `len`, `payload` | IF-SEND of `len` bytes |
| `if_recv` | `proto`, `comid`, `len` | IF-RECV of `len` bytes |
| `reset` | `event`: `power_cycle`, `hardware_reset` or `hot_plug` | a reset (Opal 2.30 Table 16's types 0-2, PDF 31; the words of `Hsed.Wire.Internal`'s events); TPER_RESET is an IF-SEND |
| `describe` | none | the TPer described |
| `peek` | `query` (below) | a query of the simulator's state |
| `release` | `comid` | the ComID's prepared response released now (S2 to S3); answered `good`, `len` 0 |

```json
{"type":"sed.req","id":2,"op":"if_send","proto":1,"comid":4100,"len":2048,"payload":"AAAAABAEAAAAAAAAAAAAAIQ..."}
{"type":"sed.req","id":3,"op":"if_recv","proto":1,"comid":4100,"len":2048}
{"type":"sed.req","id":5,"op":"reset","event":"power_cycle"}
{"type":"sed.req","id":6,"op":"peek","query":{"column":"Enabled","name":"cell","row":"0000000900030001","sp":"0000020500000002","table":"0000000900000000"}}
```

- **Ids** are integers from 1 to 2^53 - 1 (`Frame.mostId`, the largest a JSON number carries exactly to a reader
  of doubles), strictly increasing per socket; a client registers a request before it sends it and never reuses an
  id (design §6.3 :513). hsed's client keeps one counter for its life, across sockets.
- **`len` is always bytes**, whatever `app.sed.units` says, and at most 1 MiB, 1048576 (M5c; design §6.3 :512;
  `Hsed.Wire.mostCarried`).
- **Payloads** are standard padded canonical base64 (RFC 4648 §4: the `+/` alphabet, `=` padding, no line break,
  the unused bits of the last character zero, so that `AB==` is refused) of exactly `len` bytes (M5c; design §6.2
  :495). Design §6.3's own example decodes so: `AAAAABAE` is `00 00 00 00 10 04`, a ComPacket's reserved bytes and
  ComID 0x1004, the frame's `comid` 4100. 1 MiB of payload is 1,398,104 characters, a frame of about 1.4 MB, within
  the 2 MiB limit. An IF-SEND's payload is the transfer whole: the host pads it to `len` itself.

### sed.rsp

On the requesting socket alone, with no `seq` (S L87-88: frames to one client need not carry it; and a SPEC client
takes any integer `seq` for the broadcasts' count, `client.dart:390-393`, so hsed's client ignores a `sed.rsp` that
carries one). `id` is the request's; one of four forms follows it (design §6.3 :505-508; `Frame.SedRsp`,
`Frame.rspText`, read by `Frame.fromServer`):

```json
{"type":"sed.rsp","id":2,"status":"good","len":0}
{"type":"sed.rsp","id":3,"status":"good","len":2048,"payload":"AAAAABAE..."}
{"type":"sed.rsp","id":4,"status":"unsupported","detail":"the reset op is not allowed: hsed-sim allows it with --allow-remote-reset","len":0}
{"type":"sed.rsp","id":7,"describe":{"allows":["peek","release","reset"],"base_comid":4100,...,"v":1}}
{"type":"sed.rsp","id":6,"view":{"cell":"enum","label":"FALSE","value":0}}
{"type":"sed.rsp","id":null,"status":"protocol_error","detail":"id: an integer from 1 to 9007199254740991, not \"7\""}
```

- **An interface status**: `status`, the snake case of `Hsed.Wire.IfStatus` (`ifStatusName`,
  `Hsed/Wire/Internal.hs:105-114`): `good`, `invalid_security_protocol_id`, `invalid_transfer_length_on_if_send`,
  `other_invalid_command_parameter`, `synchronous_protocol_violation`, `data_protection_error`,
  `transport_failure` and `unsupported`, the last two with their `detail` (`ifStatusDetail`, `:117-121`); then
  `len`, and `payload` where `len` is not 0. An IF-RECV answered `good` carries exactly the `len` asked, zero-padded
  (design §6.2); a status other than `good` has `len` 0 (M5c; the in-process transport so answers,
  `Hsed/Transport/InProc.hs:101-103`, `newInProc`'s exchange). Interface status is never carried in a payload (design §6.2).
- **`describe`**: the `app.sed` object, as in hello; always answered.
- **`view`**: a peek's answer (below).
- **`protocol_error`** with a `detail`, sent only in reply to a frame whose `type` is `sed.req` that does not read:
  a field missing, of the wrong type or out of its range; an unknown `op`; `len` over 1 MiB; a payload that is not
  standard padded canonical base64, or whose length is not `len`; and an id not greater than the last the socket's
  requests were accepted with. Its `id` is the request's where that was an integer (within 64 bits), else `null`;
  a refused request does not advance the socket's last id (design §6.3 :512, the id's handling M5c's). Every other
  frame, a request's wrong `type` included, is ignored, as conformance check 3 requires (S L19-21;
  `conformance.dart:141-151`).
- hsed's client refuses a length past what its unit carries (`mostCarried`: 255 blocks in `blocks512`, 1 MiB in
  `bytes`) itself, with `other_invalid_command_parameter` and nothing sent, as the in-process transport does
  (`InProc.hs:101-103`, `newInProc`'s exchange), so that its transcripts equal `--target sim`'s; the hub's
  `protocol_error` is for clients that do not.
- **Order**: the hub works through each socket's requests in arrival order and replies on that socket. The TPer is
  one, its per-ComID S1/S2/S3 machine shared by every socket, so that two hosts on one ComID see
  `synchronous_protocol_violation` as two hosts on one drive would (design §6.3 :514).
- **Backpressure**: the hub takes a socket's next request only while the bytes queued to that socket and not yet
  written are under its backlog, 8 MiB (`HubOpts.hoBacklog`, the M5c review), so that a client that sends and does
  not read is held back by its own socket rather than held in the hub's memory: a socket holds about the backlog and
  one message of the hub's memory, whatever it asks.

### sed.evt

```json
{"type":"sed.evt","seq":7,"event":"power_cycle","comids_reset":[4100],"sessions_aborted":[[4100,4096]]}
```

A broadcast, stamped with the next `seq` like any other (design §6.3 :509 and :514): for each reset a client's
`reset` op causes and the TPer answers `good` (there is no operator's reset at M5c; it is M6's), sent to every
connection, the requester's included, before the requester's `sed.rsp`, the event and its broadcast one step
under the hub's lock. `comids_reset` is every ComID; `sessions_aborted` the
`[comid, tsn]` pairs of the sessions open before the reset (`Frame.SedEvt`, `Frame.evtText`). A client takes another
client's `sed.evt` as a reset it did not ask for. A `sed.evt` names no requester: while a client's own reset waits,
another client's reset that the hub runs first is indistinguishable from its own, so that hsed's client, which takes
the first `sed.evt` to arrive while one of its resets waits as that reset's, counts concurrent resets from several
clients rightly but may attribute them wrongly (the one noticed as another's being its own; the M5c review; M5c runs
one client per hub). A client that needs the attribution needs a requester id in `sed.evt`, which hsed's own layer
may add.

### The view: peek's queries and answers

A `peek`'s `query` is an object whose `name` says what it asks (`Hsed.Sim.View.SimQuery`; `Frame.queryValue`,
`Frame.parseQuery`); UIDs are 16 lower-case hex digits, bytes standard padded base64, a missing thing `null`
(`Frame.answerValue`, `Frame.parseAnswer`):

| `name` | members | `view` |
|---|---|---|
| `comids` | none | every ComID, ascending: `{"comid":4100,"life":"issued","sync":{"state":"s1"}}`, `life` one of `inactive`, `issued`, `associated` or `null`, `sync`'s `state` `s1`, or `s2` or `s3` with the `packets` it holds |
| `sessions` | none | the sessions open: `[[comid, tsn], ...]`, ascending |
| `life_cycle` | `sp` | the SP's LifeCycleState value (Opal 2.30 Table 49, PDF 98: 8 Manufactured-Inactive, 9 Manufactured), or `null` |
| `credential` | `sp`, `authority` | `{"pin":B64,"try_limit":N,"tries":N}`, the authority's C_PIN object's, or `null` |
| `cell` | `sp`, `table`, `row`, `column` (its name) | the committed cell, or `null`: `{"cell":KIND, ...}`, KIND one of `uid` (`uid`), `text` (`text`), `enum` (`label`, `value`), `ref` (`name`, `uid`), `uint` (`value`), `int` (`value`), `bytes` (`bytes`), `list` (`cells`) |
| `byte_table` | `sp`, `table` | `{"size":N,"fill":N,"written":[[offset,B64], ...]}`, every extent written, offsets ascending, or `null` |
| `msid` | none | the MSID the simulator was given, B64 |
| `locking_ranges` | none | the Locking table's rows' UIDs, the global range first |

**A view answer must fit the 2 MiB message** (the lead's L5): the hub sends an answer whole, and a client holding
to the limit (hsed's) closes the connection with 1009 when one is larger. Base64 makes 2 MiB hold about 1.5 MiB of
bytes, so a `byte_table` whose written extents pass about 1.5 MiB (a DataStore written past that) cannot be peeked;
no case of M5c comes near it (SPF-02:2 writes 1 KiB).

### Nothing sent is sent again

No request is ever re-sent, an IF-RECV included: an IF-RECV in S3 delivers the staged packets and moves the ComID to
S1 (`Hsed/Sim/Sync.hs:358-366`), so a retry after a lost reply would read "all returned, no data" and the response
would be lost silently. A dropped socket fails every request sent on it with `transport_failure`; requests issued
while disconnected wait for the reconnect, and fail after a new `server_id` or missed broadcasts (M5c, correcting
design §6.3 :513's "IF-RECV may be retried").

## What the hub allows

- **Always**: IF-SEND, IF-RECV and `describe`.
- **The control ops**, `peek` and `release`, only with `--allow-remote-control`, and **`reset`** only with
  `--allow-remote-reset`; otherwise each is answered `status` `unsupported` with a `detail` naming the flag, the
  socket kept (design §7 item 5 :524, §11 M5c, Option A): the detail reads `the peek op is not allowed: hsed-sim
  allows it with --allow-remote-control`, and likewise for `release` and, with `--allow-remote-reset`, `reset`
  (`Hub.opReply`). `app.sed.allows` says which are on. A `peek` answers
  C_PIN values and the MSID in clear base64 over the loopback socket: inherent to Option A, the jig authenticating
  with PINs read so; any local user can reach a loopback port, which `--token-file` narrows.
- **The hooks**, only with `--conformance-hooks`, on the same listener as `/ws` (the Dart suite's `--emit` and
  `--count`, `bin/conformance.dart:57-70`):
  - `POST /emit`: the body, a JSON object, broadcast to every connection with the next `seq`, which replaces any
    `seq` the body carried; answered 200 `{"seq":n}`. The counter advances even with no connection (S L77). A
    body that is not JSON, or not a JSON object, is answered 400 (the lead's L3: neither SPEC.md nor the Python
    fixture gives a status, `conformance_fixture.py:34-36` broadcasting whatever it decodes, its own failure an
    exception's 500), and so are an object with no string `type` (S L18-19) and a `type` that is the transport's
    (`hello`, `ping`, `pong`) or hsed's (`sed.*`), so that the hook can forge no `sed.evt` or `sed.rsp`; a body
    past 2 MiB, the frames' own limit (`Conn.messageLimit`), is answered 413, and so is one whose broadcast, the
    object as written with its `seq`, is past it, which no peer of hsed's would read (the M5c review). Each
    refusal's body is `{"error":<why>}`; nothing is broadcast and no `seq` is spent.
  - `GET /clients`: 200 `{"count":n}`, the connections registered, `n` a JSON integer.
  - With `--token-file`, each hook asks for the token as `/ws` does (`?token=T`), answering 401 `{"error":...}`
    where it is missing or wrong (the M5c review).
  - A hook asked by another method, a hook without `--conformance-hooks`, and any other path: 404. `/ws` without
    a WebSocket upgrade: 426. A request from a web page or for another host: 403, before any of these
    ([Connecting](#connecting)).
- **A token**, only with `--token-file`: the `token` query parameter, compared by its SHA-256 digest in constant
  time (so that the comparison tells nothing of the token's length either); missing, wrong, or given more than once
  (taken as none, whichever comes first), the connection is accepted and closed with 4001 before hello (S L92-95),
  the token never logged; the hooks ask for it too (401). The file is read as bytes, one trailing LF or CRLF
  stripped (the lead's L3; the Python hub reads no token, its caller checking it, `mh_transport.py:94-98`). The jig
  sends no query at M5c (its `--target` refuses one); the jig's token side is M6's.

## Close codes

| code | reason | sent by |
|---|---|---|
| 1000 | normal closure (RFC 6455 §7.4.1) | a client closing (hsed's on close) |
| 1001 | going away (RFC 6455 §7.4.1) | the hub closing its peers (`hubClose`): `hsed-sim` on SIGINT or SIGTERM, then exiting 0 |
| 1002 | protocol error (RFC 6455 §7.4.1) | either end, `Conn`: a frame the websockets library cannot read |
| 1008 | policy violation (RFC 6455 §7.4.1) | the hub, naming the policy in its reason: `send queue full` or `too many requests waiting`, a connection's queues bounded (1024 frames each, `HubOpts.hoQueue`; the send queue also in bytes, its backlog and a message more, which only broadcasts and pongs, not held back with the requests, can reach); hsed's client, refusing a hello that does not come within its wait or does not read, or an `app.sed` changed across a reconnect, after which it is latched, every request failing |
| 1009 | message too big (RFC 6455 §7.4.1) | either end, `Conn`: a message or frame over 2 MiB |
| 4000 | `heartbeat timeout` | the hub: a connection silent for `timeout_ms` (S L58-60; `protocol.dart:26`) |
| 4001 | a wrong token | the hub, before hello (S L92-93) |

- **1009 is Conn's own**: over its limit, websockets throws and sends no close frame, under wai-websockets the
  stream simply closing, which a peer sees as 1006, a closure with no code (M5c's reading of websockets 0.12.7.3,
  design §13.34 (28)(b): `Hybi13.hs:205-206`, `Demultiplex.hs:119-120`, `Connection.hs:266-300`). `Conn` catches it,
  sends 1009 itself, and does not read the stream again; the peer reads the close if it reads before the socket
  goes.
- **The closing handshake** (RFC 6455 §5.5.1, §7.1.1; `Conn`): a close is sent once; nothing is sent after it; data
  that arrives after one's own close is dropped, so that no request crossing a close is answered; the end that
  receives a close echoes it (the websockets library does so as it reads it, `Connection.hs:291-294`); the end that
  sent the first waits at most 1 s for the echo, then drops the connection. A connection dropped without a close is
  a closure with no code.

## A client's rules

hsed's client (`Hsed.Transport.Mhtx.Client`, design §7 item 4) keeps these, and any client of `hsed-sim` should:

- **Require hello**: connect within 5 s, hello within 3 s of it, or the attempt fails (S L99-104, `requireHello`;
  `client.dart:114-115` and `:315-317`). hsed's client then requires `v` 1, an `app.sed` that reads, `role` `tper`
  and known `units`; the jig requires `peek` in `allows` and holds its own simulator flags to `app.sed.setup`
  (`Hsed.Sim.View.setupDifferences`), refusing a hub that differs, each difference named.
- **Ping** every `interval_ms`; take any inbound frame as proof of life; drop the connection after `interval_ms`
  plus 10 s of silence (S L57-62); answer a ping with pong (S L65).
- **Requests**: ids from 1, increasing, registered before they are sent, never reused; replies correlated by id, a
  reply to an id not pending ignored, a `sed.rsp` carrying `seq` ignored; each request timed out (hsed's client:
  15 s, past the host's own 10 s per command, `Hsed.Wire.defaultExchangeOpts`, so that a slow hub is cut by the
  host as the in-process simulator would be); a request issued while disconnected waiting for the reconnect
  (hsed's client: 3 s, then `transport_failure`, "not connected to ... within 3 s"); nothing re-sent, see
  [Nothing sent is sent again](#nothing-sent-is-sent-again). hsed's client's waits are `Client.clientOpts`':
  connect 5 s, hello 3 s, a request 15 s, the heartbeat's grace 10 s, a reconnect 3 s, and `defaultBackoff`.
- **Reconnect** after any drop: 1 s doubling to a 30 s cap, equal jitter (half the delay fixed, half uniform), the
  counter reset after each successful connection; the newest attempt wins, older ones discarding their result
  (S L108-111; `backoff.dart:5-35`). A refused handshake, or a close before hello (4001 among them), is a failed
  connection, backed off (S L94-95).
- **Judge each reconnect** by its hello against the `server_id` and the last `seq` seen (S L78-85;
  `client.dart:397-406`): the same `server_id` and an equal `seq`, none missed; a greater `seq`, some missed;
  another `server_id`, a first connection or no hello, unknown. To hsed's client a new `server_id` is a hub
  restarted, a new TPer at its Original Factory State, which the run takes as a power cycle (its session and its
  host properties forgotten); broadcasts missed mean a reset may have happened; each is a notice, as is another
  client's `sed.evt`.

## Conformance

mh_transport's eight checks (S L113-129; `conformance.dart:112-275`) are re-expressed in Haskell, with a
binary-frame check, at a heartbeat of 200 ms and a timeout of 600 ms, against hubs on 127.0.0.1 and ephemeral ports
(`hsed-lan-test`'s ConformanceSpec, which CI runs on both runners, Scott's answer Q3): checks 1-7 with
`conformance.dart`'s waits, and check 8 with hsed's own client, whose reconnects the hub forces on one
`server_id` (`hubClose`), each held at a gate the check opens once the close is done, a broadcast made while it is
away (unknown at the first hello, then some, then none), so that no scheduling decides the verdict (the M5c
review).
The Dart suite itself is Scott's step, against `hsed-sim --conformance-hooks --listen 127.0.0.1:0 --heartbeat-interval-ms 200
--dead-timeout-ms 600`, run from a throwaway copy of `~/mh_transport` as `tools/mhtx-conformance.sh` prints it; its
printed last line, `CONFORMANT: 8 passed, 0 failed, 0 skipped`, is read, not its exit status, which is 0 with skips
too (Scott's answer Q2; `bin/conformance.dart:85`).
