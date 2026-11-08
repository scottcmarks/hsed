# The LAN leg's wire: the simulator over mh_transport frames

The reduced client contract design §7 item 4 promises: what a client of `hsed-sim` sends and receives, so that a
client other than hsed's own (`Hsed.Transport.Mhtx.Client`) can drive the simulated TPer. The frames are
mh_transport's (its `SPEC.md`, v1, unchanged) with hsed's application layer over them (design §6.3); the hub is
`Hsed.Transport.Mhtx.Hub`, served by `hsed-sim` on 127.0.0.1 alone (Scott's answer Q1, design §11 M5c).

*Not yet written: group G1 writes this contract, every rule with its source (M5c's plan, its protocol rules). Until
then this page is its outline.*

## Frames

Every frame one WebSocket text message holding a JSON object with a string `type`; what a receiver cannot decode,
or does not know, ignored with no reply. *Not yet written: group G1.*

## hello, ping and pong

mh_transport's own: `hello` first on every connection, its `app.sed` saying what TPer answers; the heartbeat.
*Not yet written: group G1.*

## Requests, responses and events

`sed.req` and its ops (`if_send`, `if_recv`, `reset`, `describe`, and the control ops `peek` and `release`),
`sed.rsp` on the requesting socket with no `seq`, `protocol_error`, and the seq-stamped `sed.evt`; ids, payloads
and lengths. *Not yet written: group G1.*

## What the hub allows

The control ops behind `--allow-remote-control`, the reset op behind `--allow-remote-reset`, the conformance hooks
behind `--conformance-hooks`, a token behind `--token-file`. *Not yet written: group G1.*

## Close codes

1000, 1001, 1008, 1009, 4000 and 4001. *Not yet written: group G1.*

## A client's rules

Hello required, ids registered before they are sent, nothing sent ever sent again, reconnects judged for missed
broadcasts. *Not yet written: group G1.*
