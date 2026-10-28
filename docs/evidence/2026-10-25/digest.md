# Facts digest for the hsed spec-as-source design round (2026-10-25)

Full reader maps with file:line / PDF-page evidence are in the same directory under `maps/`
(hsed-codec, hsed-tables, mh-transport, drive-io, pdf-structure, tcg-protocol, spec-tables,
critique). Read them for detail; this digest is the agreed baseline.

## The program (Scott's words, paraphrased)
Annotate the published PDFs of Core Spec 2.01 and Opal SSC 2.30 with PDF "comments" so they read
unchanged in an ordinary viewer, and compile the annotated PDFs into a running host (test jig) /
simulated-drive (reference TPer) pair. Host and drive exchange a completely serialized
command/response byte stream over pluggable transports: in-process; a real SED on a USB adapter via
low-level drive I/O; or two machines on a LAN via ~/mh_transport. HARD CONSTRAINT: the comments must
be minimal and lean on Haskell definitions so they "say a lot with a little". Haskell is the target
(minimalism, possibility of proofs). Opal Family Test Cases v1.02 (in tcg/) is a candidate third
target (the jig's test catalogue).

## PDFs (pdf-structure, spec-tables)
- Both are PDF 1.5 from Acrobat PDFMaker for Word, unencrypted, all permissions, linearized, full
  text layer with word positions, bookmarks, only /Link annots today. Core: 306 pages, 248 numbered
  tables (LoT lists 247; Table 185 missing from LoT), PDF page = printed + 17. Opal 2.30: 101 pages,
  50 numbered tables, PDF page = printed + 1. Every "Table N  Title" (Core) / "Table N - Title"
  (Opal) caption is unique on its page.
- BOTH ARE TAGGED PDFs: StructTreeRoot with /Table -> /TR -> /TH|/TD -> MCID -> marked content in
  the page stream. Walking it recovers cells exactly (Core Table 46: UID/Name/Format rows; Opal
  Table 20 preconfiguration rows incl. the rotated headers that gs txtwrite scrambles) and already
  stitches page-spanning tables (Core 40 tables span 2-3 pages; Opal 16 span up to 23 pages, e.g.
  Locking SP AccessControl Table 38 = 22 pages). Caption is the immediately preceding struct
  sibling (Core: /L->/LI; Opal: /P). /Table /A /BBox attributes are unreliable; derive Rects from
  word geometry (pymupdf find_tables strategy "lines_strict" gives true grids; "lines" over-segments;
  Table.extract() mangles underscores; get_text("words", clip=...) is clean).
- Opal Table 1 legend gives normative-adjacent meaning to cell SHADING and BOLD-UNDERLINE (read-only
  vs personalizable access control, VU, (N)); no text layer carries it.
- Hidden /Text annotation with /F 2 or /F 6 (Hidden|Print) anchored to a table bbox renders
  pixel-identically under Ghostscript 10.08, MuPDF and Apple CoreGraphics (what Preview uses); /F 4
  draws a note icon. Payload recoverable via pikepdf, pypdf, pymupdf; words inside the Rect
  recoverable.
- Writers: pypdf PdfWriter(incremental=True).add_annotation -> +1,010 bytes, ORIGINAL IS AN EXACT
  BYTE PREFIX, cleanest. pymupdf incremental -> +1,198 bytes, prefix kept, adds Popup/NM/C noise.
  pikepdf (qpdf) cannot do incremental: rewrites whole file (+39 KB, de-linearized).
  Haskell: pdf-toolbox-core 0.1.3 reads xref streams/object streams and its Pdf.Core.Writer
  supports incremental update (writeXRefStream); NOT in lts-17.7, present in LTS 24.x.
- A Python venv exists at ~/hsed/.venv (Homebrew Python 3.15; pymupdf 1.28.2, pikepdf 10.17.0,
  pypdf 6.21.2), self-ignored by git. Ghostscript at /opt/local/bin/gs.
- Core table classes: 235 directly compilable (A), 6 need interpretation (B: 3, 45, 167, 185,
  230, 231), 7 prose (C). Opal: 48 A, 1 B, 1 C. hsed covers 137 Core tables by number (4-16 in
  Token.hs; 46-165 in TypeUIDs.hs; 171; 239-241); NOT covered: 17-45 (ComPacket/Packet/Subpacket,
  ComID mgmt, Level 0), 166 (status codes), 167-170, 172-238 (table descriptions, preconfig,
  crypto), 242-248 (UIDs). No Opal table is in hsed. Unnumbered code-shaped content: ~51 Core method
  signatures `X.M[ a : t, b = t ] => [ ... ]` and 18 abstract types with "Format:" pseudo-code; Opal
  has 3 signatures (Activate, Revert, RevertSP).
- Core errata already known: 'Table 179' dangling reference (PDF 116), Table 185 missing from LoT,
  typos Fractoin/Eror/'object _ref', illegal ABNF in 5.1.1 (Scott's tcg/'Core Spec errata.txt').

## hsed today (hsed-codec, hsed-tables)
- Working, property-tested token codec: StreamItem class (attoparsec parser + generate), full
  Table 04 token set, Value/NamedValue/List, MethodCall (Call hdr params EOD status). Defects:
  Parameters omit the StartList/EndList around params (Call.hs:210-212); removeEmpty truncates at
  the first Empty atom (Token.hs:359); long-atom bound too large; optional params absorbed by
  required; no MethodResponse type. NOTHING above method level: no ComPacket/Packet/Subpacket, no
  session manager, no IF-SEND/IF-RECV, no transport, no device model. TPer/Run and Host/Run are
  one-line stubs.
- Spec tables are embedded as hand-transcribed ASCII-art inside quasiquoters: 100 [ttype| (Core
  Tables 46-165 UID/Name/Format rows -> uXType :: UID and fXType :: String; the Format column is
  NOT parsed), 19 [tenum| (2-column enum tables -> data Core_X with value-ordered ctors), one
  bespoke [t240| (Table 240 -> table UIDs/HalfUIDs). Grammar is positional and brittle (exact '|'
  offsets). Transcription errors found: Seconds UID all zeros; feedback_size, fraction_enum,
  gen_status carry date's UID; two captions mis-numbered (157 as 147, 161 as 160); plus deliberate
  typo corrections. The generated UIDs are consumed nowhere at runtime. Lesson: PDF -> code, never
  code -> PDF; regenerate TypeUIDs from the PDF and diff.
- A type-level Data.BoundedSize/Refined library (DerivingVia, TH splices for sized literals) and
  an unimplemented Format parser (Core_Format GADT with `undefined` parsers) exist.
- BUILD IS BLOCKED: stack 2.11.1 cannot fetch the Hackage index (root-key rotation 2025-07-24;
  needs stack >= 3.9.1, ghcup has 3.11.1); resolver lts-17.7 pins GHC 8.10.4 which has NO
  aarch64-darwin bindist. Installed: ghcup GHC 9.4.7 (+HLS), Homebrew GHC 9.14.1, cabal 3.6.2.
  Porting to 9.4: three `PlainTV` sites need the TH 2.17 flag, three `Nat` imports move to
  GHC.TypeNats, Assert-based (<=) constraints are the risk. ~1/3 of package.yaml deps are unused
  (ghc, megaparsec, mtl, vector, abnf git dep, ...). websockets 0.12.7.2 IS in lts-17.7; gdp,
  static-text, heredoc, singletons 3.x in current Stackage.

## Protocol contract (tcg-protocol, critique)
- Core Table 24: every interface command is IF-SEND or IF-RECV {Protocol ID u8 (0x01 ComPackets,
  0x02 ComID mgmt incl. STACK_RESET/TPER_RESET, 0x00 SPC inventory), Transfer Length, ComID u16}
  + payload. FIVE INTERFACE STATUS CODES are returned OUT OF BAND (sense / ATA status / NVMe SC):
  Invalid Security Protocol ID, Invalid Transfer Length on IF-SEND, Other Invalid Command
  Parameter, Synchronous Protocol Violation, Data Protection Error. Units: SCSI/ATA 512-byte
  blocks (INC_512=1), NVMe bytes. Resets (Power Cycle, Hardware Reset) are transport EVENTS, not
  payload, and drive ComID expiry, session abort, Properties reset, LockOnReset, MBRDoneOnReset.
- ComPacket 20-byte hdr (ComID, ext, OutstandingData, MinTransfer, Length) > Packet 24-byte hdr
  (TSN|HSN, Seq, AckType, Ack, Length) > Subpacket 12-byte hdr (Kind, Length) + 4-byte pad. One
  ComPacket per command. Method = Call, InvokingID, MethodID, SL params EL, EOD, SL status 0 0 EL.
  Session Manager: InvokingID SMUID 00..00FF, methods FF01 Properties .. FF06 CloseSession, on
  the control session (Session 0). Synchronous state machine per ComID S1 Awaiting IF-SEND -> S2
  Processing -> S3 Awaiting IF-RECV, Table 45 fixes (Length, OutstandingData, MinTransfer).
- Level 0 Discovery: IF-RECV proto 0x01 ComID 0x0001, 48-byte header + feature descriptors
  (0x0001 TPer, 0x0002 Locking, 0x0003 Geometry, 0x0203 Opal V2 incl. Base ComID, 0x0005 SIIS,
  0x0404 Data Removal, ...). Bit layouts confirmed by sedutil DtaStructures.h.
- OPAL 2.30 OVERLAY (supersedes Core defaults for an Opal simulator): Properties minimums
  MaxComPacketSize 2048 / MaxPacketSize 2028 / MaxIndTokenSize 1992, one ComPacket/Packet/Subpacket,
  no continued atoms, synchronous only; static ComIDs (ext 0x0000) from the 0x0203 descriptor;
  TPER_RESET = IF-SEND proto 0x02 ComID 0x0004; STACK_RESET (M). SP table: Admin SP
  00 00 02 05 00 00 00 01 'Manufactured', Locking SP 00 00 02 05 00 00 00 02 'Manufactured-Inactive'
  (StartSession must fail until Activate 00 00 00 06 00 00 02 03). C_PIN_SID 00 00 00 0B 00 00 00 01,
  C_PIN_MSID 00 00 00 0B 00 00 84 02, Admin1 Locking C_PIN 00 00 00 0B 00 01 00 01, User1
  00 00 00 0B 00 03 00 01. Preconfiguration = Opal Tables 18-29 (Admin SP), 33-48 (Locking SP);
  schema patches Tables 30-32, 49 (life_cycle_state replaces Core 102/103), 50 (Table table +2
  columns); feature-descriptor pins Tables 2-12; comms narrowings 13-17. Compile Opal as an ORDERED
  PATCH SET over Core. Opal 2.30 references Core 2.01 and SIIS 1.11; SSC minor version 0x3.
- Tier-1 host flow: Level 0 -> Properties -> StartSession(Admin SP, Anybody) -> Get C_PIN_MSID.PIN
  (cell_block [3=3,4=3]) -> EndOfSession. Tier-2: Authenticate SID with MSID, Set SID PIN, Activate
  Locking SP, ranges, MBR. Opal Family Test Cases v1.02: UCT-01..16, SPF-01..20, ETC-01..24.
- SIIS 1.20 (also in tcg/) is normative for the CDB layer (proto 0x06 byte-count mode, NVMe SPSP0/1,
  status mappings, reset mappings, status codes 0x13/0x14) and is not one of the two PDFs to annotate.

## Real-drive path (drive-io; verified live on this Mac)
- sedutil (C++, ~/preboot/DTA/sedutil, v1.49.1 develop) boundary: pure virtual
  `DtaDrive::sendCmd(ATACOMMAND TRUSTED_SEND|TRUSTED_RECEIVE, u8 protocol, u16 comID, void*, len)`,
  lengths multiples of 512; exec = one TRUSTED_SEND then 25 ms-polled TRUSTED_RECEIVE (2048 B)
  while outstandingData!=0 && minTransfer==0. Static ComID = base ComID from Level 0.
- macOS: ONLY path is the BrightPlazaTPer.kext (loaded now; SIP disabled; matches
  IOSCSIPeripheralDeviceNub; whitelists INQUIRY, REQUEST SENSE, SECURITY PROTOCOL IN A2h/OUT B5h,
  ATA PASS-THROUGH(12) A1h wrapping 5Ch/5Eh/ECh). User space: SEDKernelInterface.framework
  (/Library/Frameworks) C ABI: OpenUserClient, DriverPerformSCSICommand(cdb12, page-aligned buf,
  len, ...), TPerUpdate, CloseUserClient, findBSDName/findParent. Requires root + exclusive open;
  sense data NOT returned (all errors collapse to 0xff). NVMe Security Send/Receive and native ATA
  are UNIMPLEMENTED on macOS -> only USB-bridged SEDs reachable (SAT or UAS/SCSI). Installed
  framework/kext build 625; sources are at 626; last build 2024-05-07; rebuild on macOS 27 unverified.
- Attached right now: /dev/disk6 Samsung 970 EVO Plus 250GB behind a Realtek RTL9210 USB-NVMe bridge
  ('TPer Interface Type' = SAT, admitted via a quirk-table entry), Opal feature 0x0203, base ComID
  0x1004, LockingEnabled=N, Locked=N, MBR not enabled, FAT32 volume 'SED NVME' (may hold data: NO
  write operations without Scott). `sudo sedutil-cli --scan/--query` work. sedutil-cli has NO raw
  IF-SEND/IF-RECV passthrough; sedutil's TODO.txt itself proposes a C 'libsed' shim with void*
  handles "for Python or Haskell".
- Linux (errol/x86): SG_IO and NVMe ioctl paths exist in sedutil. MacVFN/xnvme (DriverKit, PCIe
  NVMe only, SIP off, beta, 2024 forks) is a parked option, not on the critical path.

## mh_transport (mh-transport)
- Protocol v1: one WebSocket per client; every frame a JSON text object with string "type";
  reserved hello/ping/pong; everything else is an application message; server pushes broadcasts
  with seq; client.send() is fire-and-forget (no ids, no timeouts); unicast app frames are allowed
  (SPEC L87-88). Binary frames: Dart ignores them; the PYTHON HUB CRASHES THE CONNECTION on one
  (SPEC violation; that file is symlinked into the live MHTX trading server -> changes ship to
  production). Hubs exist in Dart and Python only; the single client is Dart; NO Haskell code.
  Conformance CLI `dart run mh_transport:conformance ws://...` certifies any server in any
  language. CCC and MHTX were cut over the same day; churn has two live consumers.
- Fit: lifecycle/heartbeat/reconnect/server_id restart detection fit; request/response and
  binary do not. Minimal extension = application types sed.req/sed.rsp with id, op
  (if_send|if_recv|discovery|reset|...), proto, comid, len, base64 payload, status; reply on the
  originating socket; request helper (correlate by id, timeout, fail pending on disconnect) in the
  SED layer first.

## Home-repo constraints (critique)
- wapex everyday re-sync = reset --hard origin/work + submodule update --force: submodule commits
  not pushed to GitHub AND pointer-bumped on wapex are snapped away on other machines. Any new repo
  must be added via .gitmodules. Never git clean. The .claude memory slice IS tracked (CLAUDE.md
  note is stale). Explicit paths only when staging; no AI attribution trailers (global rule).
