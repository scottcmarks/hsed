"""The ONE place that knows the hsed annotation payload format (PROVISIONAL grammar).

An hsed annotation is a hidden PDF /Text annotation, one per spec table:

    /Type /Annot  /Subtype /Text
    /F 6                                  Hidden | Print  (never drawn, never printed)
    /Rect [x0 y0 x1 y1]                   the table's bbox on its first page (PDF user space, y up)
    /Contents (hsed: <directive>)         the whole visible payload: a name in the hsed library
    /HSED << /V 1                         private dictionary, machine anchor
             /Path (0/1288)               /StructTreeRoot /K index path of the /Table element
             /Caption (Table 46 AC_element)
             /Pages [99] >>               0-based page indices the table touches (first = anchor page)

<directive> is one of DIRECTIVES below.  Renaming a directive, the "hsed:" prefix, the private
key or the dictionary field names is a change to this file only; annotate.py, compile.py and the
probes import everything from here.

Library-neutral: the format is a plain dataclass; adapters convert to pypdf objects (writer) and
from pikepdf / pypdf / pymupdf objects (readers).
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any

# ----------------------------------------------------------------------------- grammar constants
PREFIX = "hsed:"                     # /Contents marker; the compiler ignores every annotation without it
PRIVATE_KEY = "/HSED"                # private dictionary key (second-class name, PDF 32000 Annex E)
VERSION = 1                          # /HSED /V
FLAGS = 6                            # /F: Hidden (bit 2) | Print (bit 3)

TYPE_ROW = "typeRow"                 # a UID / Name / Format Type-table row (Core 5.1.3, Tables 46-165)
ENUM_VALUES = "enumValues"           # an "<name> Enumeration Values" / "<name> Set Values" table
TABLE_UIDS = "tableUIDs"             # Table 240 Table UIDs
DIRECTIVES = {
    TYPE_ROW: "Type table row -> u<Name>Type :: UID, f<Name>Type :: String",
    ENUM_VALUES: "enumeration table -> data Core_<name> = Core_<name>_<Label> | ...",
    TABLE_UIDS: "Table 240 -> u<T>Table, u<T>TableObject :: UID, h<T> :: HalfUID",
}

CONTENTS_RE = re.compile(r"^\s*" + re.escape(PREFIX) + r"\s*(?P<directive>[A-Za-z_][A-Za-z0-9_']*)\s*$")
PATH_SEP = "/"


# ----------------------------------------------------------------------------- the payload
@dataclass(frozen=True)
class HsedAnnot:
    directive: str
    path: tuple[int, ...]            # struct index path (/K indices from /StructTreeRoot)
    caption: str
    pages: tuple[int, ...]           # 0-based page indices, first is the anchor page
    rect: tuple[float, float, float, float]

    @property
    def page(self) -> int:
        return self.pages[0]


def format_contents(directive: str) -> str:
    """The /Contents string, byte for byte (ASCII): 'hsed: typeRow'."""
    if directive not in DIRECTIVES:
        raise ValueError(f"unknown directive {directive!r}; known: {sorted(DIRECTIVES)}")
    return f"{PREFIX} {directive}"


def parse_contents(s: str) -> str | None:
    """Directive named by a /Contents string, or None when this is not an hsed annotation."""
    m = CONTENTS_RE.match(s or "")
    return m.group("directive") if m else None


def format_path(path) -> str:
    return PATH_SEP.join(str(int(i)) for i in path)


def parse_path(s: str) -> tuple[int, ...]:
    return tuple(int(t) for t in s.strip().split(PATH_SEP) if t != "")


# ----------------------------------------------------------------------------- writer adapter (pypdf)
def to_pypdf(a: HsedAnnot):
    """pypdf DictionaryObject for the annotation (pypdf adds /P itself in add_annotation)."""
    from pypdf.generic import (ArrayObject, DictionaryObject, FloatObject, NameObject,
                               NumberObject, TextStringObject)
    private = DictionaryObject({
        NameObject("/V"): NumberObject(VERSION),
        NameObject("/Path"): TextStringObject(format_path(a.path)),
        NameObject("/Caption"): TextStringObject(a.caption),
        NameObject("/Pages"): ArrayObject([NumberObject(p) for p in a.pages]),
    })
    return DictionaryObject({
        NameObject("/Type"): NameObject("/Annot"),
        NameObject("/Subtype"): NameObject("/Text"),
        NameObject("/Rect"): ArrayObject([FloatObject(round(v, 2)) for v in a.rect]),
        NameObject("/Contents"): TextStringObject(format_contents(a.directive)),
        NameObject("/F"): NumberObject(FLAGS),
        NameObject(PRIVATE_KEY): private,
    })


# ----------------------------------------------------------------------------- reader adapters
def _from_fields(contents: str, rect, flags, private: dict | None) -> tuple[HsedAnnot | None, list[str]]:
    """Common decoding; returns (annot or None, problems). Problems are advisory (the directive
    alone is enough to compile; /HSED is the robust anchor)."""
    directive = parse_contents(contents)
    if directive is None:
        return None, []
    problems = ([] if directive in DIRECTIVES else [f"unknown directive {directive!r}"]) \
        + ([] if int(flags or 0) == FLAGS else [f"/F is {flags}, expected {FLAGS}"]) \
        + ([] if private else [f"missing {PRIVATE_KEY} dictionary"])
    p = private or {}
    return HsedAnnot(directive=directive,
                     path=parse_path(str(p.get("Path", ""))),
                     caption=str(p.get("Caption", "")),
                     pages=tuple(int(x) for x in p.get("Pages", [])),
                     rect=tuple(float(x) for x in rect)), problems


def from_pikepdf(obj) -> tuple[HsedAnnot | None, list[str]]:
    import pikepdf
    if str(obj.get("/Subtype", "")) != "/Text":
        return None, []
    priv = obj.get(PRIVATE_KEY)
    private = None if priv is None else {
        "Path": str(priv.get("/Path", "")),
        "Caption": str(priv.get("/Caption", "")),
        "Pages": [int(x) for x in (priv.get("/Pages") or [])],
        "V": int(priv.get("/V", 0)),
    }
    return _from_fields(str(obj.get("/Contents", "")), obj.get("/Rect", [0, 0, 0, 0]), obj.get("/F", 0), private)


def from_pypdf(obj) -> tuple[HsedAnnot | None, list[str]]:
    obj = obj.get_object()
    if obj.get("/Subtype") != "/Text":
        return None, []
    priv = obj.get(PRIVATE_KEY)
    private = None if priv is None else {
        "Path": str(priv.get("/Path", "")), "Caption": str(priv.get("/Caption", "")),
        "Pages": [int(x) for x in priv.get("/Pages", [])], "V": int(priv.get("/V", 0))}
    return _from_fields(str(obj.get("/Contents", "")), obj.get("/Rect", [0, 0, 0, 0]), obj.get("/F", 0), private)


def find_pikepdf(pdf) -> list[tuple[int, HsedAnnot, list[str]]]:
    """[(page_index, annot, problems)] for every hsed annotation in an open pikepdf.Pdf."""
    found = ((i, *from_pikepdf(a)) for i, pg in enumerate(pdf.pages) for a in (pg.obj.get("/Annots") or []))
    return [(i, a, probs) for i, a, probs in found if a is not None]


def smallest_payload(a: HsedAnnot) -> dict[str, Any]:
    """What the payload would shrink to under the alternatives discussed in the report."""
    contents = format_contents(a.directive)
    return {"contents_bytes": len(contents.encode()),
            "directive_only_bytes": len(a.directive.encode()),
            "private_dict_text": f"<</V {VERSION}/Path({format_path(a.path)})/Caption({a.caption})/Pages[{' '.join(map(str, a.pages))}]>>",
            }
