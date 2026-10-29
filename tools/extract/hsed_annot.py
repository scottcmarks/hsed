"""The ONE place that knows the hsed annotation format (docs/ir-v1.md; design docs/spec-as-source.md
sections 3.1-3.3).  annotate.py, apply.py, dump.py and pdf_checks.py import everything from here;
renaming a key, a kind or the sidecar's column order is a change to this file only.

The PDF side -- one hidden /Text annotation per anchor:

    << /Type /Annot /Subtype /Text /F 6 /Rect [x0 y0 x1 y1]
       /Contents (erratum scott ["Fractoin" ~> "Fraction"])        the payload text, verbatim
       /HSED << /V 1 /Kind /Bind /Spec (core-2.01) /Id (T75) /Struct (0/1429) >>
       /P <page ref> >>                                              (added by pypdf)

/F 6 is Hidden | Print: never drawn, never printed.  /HSED is identity only: format version, kind,
spec, anchor id and the /K index path of the anchored structure element ("" for the `doc` anchor,
i.e. /StructTreeRoot itself).  Everything else about the anchor (page, label, ordinal, header,
hashes, fragment rects) is derivable from the pristine PDF and lives in the manifest.

The sidecar side -- `spec/annot/<spec>.hsa`, one anchor per line, `#` comments and blank lines
ignored:

    core-2.01 | T75 | erratum scott ["Fractoin" ~> "Fraction"]

Kind follows from the id: `doc` -> Doc, a section number -> Rule, `T<n>` -> Bind,
`<section>#<item>` -> Law; a section or an item whose payload starts with `behaviour` or `fails` is
Behaviour (design section 3.1: a method heading or a Fails list), as hsedc's annotKind has it.
What a payload may say on each kind of anchor is VOCABULARY below, by its first word; a sidecar line
whose first word does not belong on its anchor is refused here, before hsedc (whose grammar and
anchor-kind check, Hsed.Spec.Rules.anchorKindMismatch, are the authority) would refuse it.
The canonical order of anchors (in the PDF's incremental section, in the dump, in the manifest) is
kind order Doc, Rule, Bind, Law, Behaviour, then the natural order of the id (T9 before T10).

Library-neutral: the records are plain dataclasses; adapters convert to pypdf objects (the writer)
and from pikepdf / pypdf / pymupdf objects (the readers).
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any, Callable, Iterable

# ----------------------------------------------------------------------------- format constants
PRIVATE_KEY = "/HSED"                # the private dictionary (PDF 32000 Annex E second-class name)
VERSION = 1                          # /HSED /V
FLAGS = 6                            # /F: Hidden (bit 2) | Print (bit 3)
KINDS = ("Doc", "Rule", "Bind", "Law", "Behaviour")      # /HSED /Kind, in canonical order
DOC_ID = "doc"                       # the one document-level anchor
DOC_PAGE = 0                         # it sits on the first page ...
DOC_RECT = (36.0, 720.0, 108.0, 756.0)                   # ... in the top-left inch
DOC_STRUCT = ""                      # ... anchored at /StructTreeRoot (the empty /K path)
BEHAVIOUR_WORDS = ("behaviour", "fails")                 # payload heads that make an item anchor a Behaviour
HSA_SEP = " | "

SECTION_RE = re.compile(r"^\d+(?:\.\d+)*$")                       # 5.1.3
TABLE_RE = re.compile(r"^T\d+$")                                  # T75
ITEM_RE = re.compile(r"^\d+(?:\.\d+)*#[A-Za-z0-9]+$")             # 3.3.10.2.1#3a, 5.2.2#p1
NATURAL_RE = re.compile(r"(\d+)")
FIRST_WORD_RE = re.compile(r"[\s(]*([A-Za-z_][A-Za-z0-9_']*)")

# The first word of a payload and the classes of anchor it may sit on: "doc" (the doc anchor),
# "section" (a heading), "table" (T<n>), "item" (<section>#<item>).  The words are hsedc's
# Hsed.Spec.Grammar.vocabulary; hsed-spec's VocabularySpec reads this table (one entry per line,
# between the braces) and keeps it equal to what hsedc accepts.  A table binding or a modifier
# belongs on a table (a modifier chain binds only a table); `sig` belongs nowhere, signature blocks
# being rule-bound.
VOCABULARY = {
    "doc": ("doc",),
    "rule": ("section",),
    "behaviour": ("section", "item"),
    "law": ("item",),
    "fails": ("item",),
    "sig": (),
    "typeRow": ("table",),
    "enumValues": ("table",),
    "tableDesc": ("table",),
    "columnsAddition": ("table",),
    "tableUIDs": ("table",),
    "uidTable": ("table",),
    "statusCodes": ("table",),
    "codes": ("table",),
    "fieldValue": ("table",),
    "fieldType": ("table",),
    "byteFieldValue": ("table",),
    "byteGrid": ("table",),
    "layout": ("table",),
    "feature": ("table",),
    "featureNamed": ("table",),
    "bits": ("table",),
    "constValues": ("table",),
    "ranges": ("table",),
    "preconfig": ("table",),
    "tokens": ("table",),
    "stateTable": ("table",),
    "propsNames": ("table",),
    "propsInitial": ("table",),
    "propsReq": ("table",),
    "legend": ("table",),
    "decision": ("table",),
    "prose": ("table",),
    "refines": ("table",),
    "narrows": ("table",),
    "replaces": ("table",),
    "extends": ("table",),
    "erratum": ("table",),
    "cols": ("table",),
    "rangeWords": ("table",),
    "enumInDescription": ("table",),
    "expand": ("table",),
    "as": ("table",),
    "waive": ("table",),
}


# ----------------------------------------------------------------------------- kinds and order
def anchor_class(anchor_id: str) -> str:
    """"doc", "section", "table" or "item"; ValueError for a malformed id."""
    for name, matches in (("doc", lambda i: i == DOC_ID), ("section", SECTION_RE.match), ("table", TABLE_RE.match),
                          ("item", ITEM_RE.match)):
        if matches(anchor_id):
            return name
    raise ValueError(f"malformed anchor id {anchor_id!r} (expected doc, <section>, T<n> or <section>#<item>)")


def first_word(payload: str) -> str | None:
    """The name a payload starts with (past any opening parentheses), None when it starts with none."""
    m = FIRST_WORD_RE.match(payload)
    return m.group(1) if m else None


def kind_of(anchor_id: str, payload: str) -> str:
    """The /Kind an anchor id (and, for a section or an item, its payload) implies; ValueError for a
    malformed id."""
    cls = anchor_class(anchor_id)
    if cls in ("section", "item") and first_word(payload) in BEHAVIOUR_WORDS:
        return "Behaviour"
    return {"doc": "Doc", "section": "Rule", "table": "Bind", "item": "Law"}[cls]


def payload_problem(anchor_id: str, payload: str) -> str | None:
    """Why a payload cannot sit on an anchor, judged by its first word (VOCABULARY); None when it may."""
    cls, word = anchor_class(anchor_id), first_word(payload)
    if word is None:
        return "the payload does not start with a name"
    if word not in VOCABULARY:
        return f"unknown name {word!r}: not in the annotation vocabulary (hsedc's Hsed.Spec.Grammar)"
    if cls not in VOCABULARY[word]:
        where = " or ".join("the doc anchor" if c == "doc" else f"a {c} anchor" for c in VOCABULARY[word]) or "no anchor"
        here = "the doc anchor" if cls == "doc" else f"a {cls} anchor"
        return f"{word!r} belongs on {where}, not on {here}"
    return None


def natural_key(s: str) -> tuple:
    """'T10' after 'T9', '5.10' after '5.9': digit runs compare as integers, the rest as text.
    re.split with a capturing group alternates text and digit runs, so positions always share a type."""
    return tuple(int(t) if i % 2 else t for i, t in enumerate(NATURAL_RE.split(s)))


def sort_key(kind: str, anchor_id: str) -> tuple:
    return (KINDS.index(kind), natural_key(anchor_id))


# ----------------------------------------------------------------------------- the sidecar (.hsa)
@dataclass(frozen=True)
class Anchor:
    """One `.hsa` line: spec, anchor id and the payload text (design section 3.3 grammar, not
    parsed here -- Haskell is the only parser of payloads)."""
    spec: str
    id: str
    payload: str

    @property
    def kind(self) -> str:
        return kind_of(self.id, self.payload)

    @property
    def sort_key(self) -> tuple:
        return sort_key(self.kind, self.id)

    @property
    def line(self) -> str:
        return HSA_SEP.join((self.spec, self.id, self.payload))


def parse_hsa_line(line: str, where: str = "") -> Anchor | None:
    """The Anchor on one line, None for a comment or blank line; ValueError names `where`."""
    body = line.strip()
    if not body or body.startswith("#"):
        return None
    parts = [p.strip() for p in body.split("|", 2)]
    if len(parts) != 3 or not all(parts):
        raise ValueError(f"{where}: expected 'spec | id | payload', got {line.rstrip()!r}")
    spec, anchor_id, payload = parts
    try:
        problem = payload_problem(anchor_id, payload)   # a malformed id is an error here, not later
    except ValueError as e:
        raise ValueError(f"{where}: {line.rstrip()!r}: {e}") from None
    if problem:
        raise ValueError(f"{where}: {line.rstrip()!r}: {problem}")
    return Anchor(spec, anchor_id, payload)


def parse_hsa(text: str, name: str = "<hsa>") -> list[Anchor]:
    """Every anchor in a sidecar, in file order (not sorted); duplicates are an error."""
    anchors = [a for a in (parse_hsa_line(l, f"{name}:{n}") for n, l in enumerate(text.splitlines(), 1)) if a]
    keys = [(a.spec, a.id) for a in anchors]
    dups = sorted({k for k in keys if keys.count(k) > 1})
    if dups:
        raise ValueError(f"{name}: anchor listed more than once: {', '.join(f'{s} | {i}' for s, i in dups)}")
    return anchors


def format_hsa(anchors: Iterable[Anchor]) -> str:
    """The canonical sidecar text: one line per anchor in canonical order, newline-terminated."""
    return "".join(a.line + "\n" for a in sorted(anchors, key=lambda a: a.sort_key))


# ----------------------------------------------------------------------------- the annotation (PDF side)
@dataclass(frozen=True)
class HsedAnnot:
    """One hsed annotation as stored in a PDF."""
    spec: str
    id: str
    payload: str                     # /Contents
    kind: str                        # /HSED /Kind as stored
    struct: str                      # /HSED /Struct ("" = the structure tree root)
    page: int                        # 0-based index of the page whose /Annots lists it
    rect: tuple[float, float, float, float]
    flags: int = FLAGS
    version: int = VERSION

    @property
    def anchor(self) -> Anchor:
        return Anchor(self.spec, self.id, self.payload)

    @property
    def sort_key(self) -> tuple:
        return sort_key(self.kind, self.id)


def make_annot(anchor: Anchor, struct: str, page: int, rect: Iterable[float]) -> HsedAnnot:
    return HsedAnnot(spec=anchor.spec, id=anchor.id, payload=anchor.payload, kind=anchor.kind, struct=struct,
                     page=page, rect=tuple(round(float(v), 2) for v in rect))


def problems(a: HsedAnnot) -> list[str]:
    """What is inconsistent about an annotation as read back (empty when it is exactly what the
    writer emits)."""
    return ([] if a.version == VERSION else [f"/V is {a.version}, expected {VERSION}"]) \
        + ([] if a.flags == FLAGS else [f"/F is {a.flags}, expected {FLAGS}"]) \
        + ([] if a.kind in KINDS else [f"unknown /Kind {a.kind!r}"]) \
        + ([] if a.kind == kind_of(a.id, a.payload) else [f"/Kind {a.kind} but the id implies {kind_of(a.id, a.payload)}"]) \
        + ([] if (a.id == DOC_ID) == (a.struct == DOC_STRUCT) else ["only the doc anchor has the empty struct path"])


# ----------------------------------------------------------------------------- writer adapter (pypdf)
def _literal_bytes(s: str) -> bytes:
    """A PDF literal string body: printable ASCII as is (backslash and parentheses escaped), anything
    else in PDFDocEncoding as octal -- so the tail reads `(0/1429)` rather than pypdf's `(0\\0571429)`."""
    from pypdf.generic import TextStringObject
    raw = TextStringObject(s).get_encoded_bytes()
    return b"".join(b"\\" + bytes([c]) if c in b"\\()" else bytes([c]) if 32 <= c < 127 else b"\\%03o" % c
                    for c in raw)


def to_pypdf(a: HsedAnnot):
    """The annotation as a pypdf DictionaryObject (pypdf adds /P in add_annotation)."""
    from pypdf.generic import (ArrayObject, DictionaryObject, FloatObject, NameObject, NumberObject,
                               TextStringObject)

    class Literal(TextStringObject):                 # same value, compact serialisation
        def write_to_stream(self, stream, encryption_key=None):
            stream.write(b"(" + _literal_bytes(self) + b")")

    private = DictionaryObject({
        NameObject("/V"): NumberObject(a.version),
        NameObject("/Kind"): NameObject("/" + a.kind),
        NameObject("/Spec"): Literal(a.spec),
        NameObject("/Id"): Literal(a.id),
        NameObject("/Struct"): Literal(a.struct),
    })
    return DictionaryObject({
        NameObject("/Type"): NameObject("/Annot"),
        NameObject("/Subtype"): NameObject("/Text"),
        NameObject("/F"): NumberObject(a.flags),
        NameObject("/Rect"): ArrayObject([FloatObject(v) for v in a.rect]),
        NameObject("/Contents"): Literal(a.payload),
        NameObject(PRIVATE_KEY): private,
    })


# ----------------------------------------------------------------------------- reader adapters
def _from_fields(page: int, contents: str, rect, flags, private: dict[str, Any]) -> HsedAnnot:
    return HsedAnnot(spec=str(private.get("Spec", "")), id=str(private.get("Id", "")), payload=contents,
                     kind=str(private.get("Kind", "")), struct=str(private.get("Struct", "")), page=page,
                     rect=tuple(round(float(x), 2) for x in rect), flags=int(flags), version=int(private.get("V", 0)))


def _name(v) -> str:
    s = str(v)
    return s[1:] if s.startswith("/") else s


def from_pikepdf(obj, page: int) -> HsedAnnot | None:
    """pikepdf annotation dictionary -> HsedAnnot, None when it is not an hsed annotation."""
    import pikepdf
    if not isinstance(obj, pikepdf.Dictionary) or str(obj.get("/Subtype", "")) != "/Text":
        return None
    h = obj.get(PRIVATE_KEY)
    if not isinstance(h, pikepdf.Dictionary):
        return None
    private = {"V": int(h.get("/V", 0)), "Kind": _name(h.get("/Kind", "")), "Spec": str(h.get("/Spec", "")),
               "Id": str(h.get("/Id", "")), "Struct": str(h.get("/Struct", ""))}
    return _from_fields(page, str(obj.get("/Contents", "")), obj.get("/Rect", [0, 0, 0, 0]), obj.get("/F", 0), private)


def from_pypdf(obj, page: int) -> HsedAnnot | None:
    obj = obj.get_object()
    if obj.get("/Subtype") != "/Text":
        return None
    h = obj.get(PRIVATE_KEY)
    if h is None:
        return None
    h = h.get_object()
    private = {"V": int(h.get("/V", 0)), "Kind": _name(h.get("/Kind", "")), "Spec": str(h.get("/Spec", "")),
               "Id": str(h.get("/Id", "")), "Struct": str(h.get("/Struct", ""))}
    return _from_fields(page, str(obj.get("/Contents", "")), obj.get("/Rect", [0, 0, 0, 0]), obj.get("/F", 0), private)


def from_pymupdf(doc, xref: int, page: int) -> HsedAnnot | None:
    """pymupdf gives a /Text annotation a fixed icon rect and extra flags through its Annot object, so
    the raw dictionary values are read through xref_get_key (whose key paths reach into /HSED)."""
    def key(k):
        kind, val = doc.xref_get_key(xref, k)
        return None if kind == "null" else val

    if key("Subtype") != "/Text" or key(PRIVATE_KEY[1:]) is None:
        return None
    rect = [float(x) for x in key("Rect").strip("[]").split()]
    private = {"V": int(key("HSED/V") or 0), "Kind": _name(key("HSED/Kind") or ""), "Spec": key("HSED/Spec") or "",
               "Id": key("HSED/Id") or "", "Struct": key("HSED/Struct") or ""}
    return _from_fields(page, key("Contents") or "", rect, key("F") or 0, private)


def read_pikepdf(path: str) -> list[HsedAnnot]:
    """Every hsed annotation in document order (page order, then /Annots order)."""
    import pikepdf
    with pikepdf.open(path) as pdf:
        found = [from_pikepdf(a, i) for i, pg in enumerate(pdf.pages) for a in (pg.obj.get("/Annots") or [])]
    return [a for a in found if a is not None]


def read_pypdf(path: str) -> list[HsedAnnot]:
    import pypdf
    r = pypdf.PdfReader(path)
    found = [from_pypdf(a, i) for i, pg in enumerate(r.pages) for a in (pg.get("/Annots") or [])]
    return [a for a in found if a is not None]


def read_pymupdf(path: str) -> list[HsedAnnot]:
    import pymupdf
    doc = pymupdf.open(path)
    try:
        found = [from_pymupdf(doc, xref, i) for i, pg in enumerate(doc) for xref, _, _ in pg.annot_xrefs()]
    finally:
        doc.close()
    return [a for a in found if a is not None]


READERS: dict[str, Callable[[str], list[HsedAnnot]]] = {
    "pikepdf": read_pikepdf, "pypdf": read_pypdf, "pymupdf": read_pymupdf}
