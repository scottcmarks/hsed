#!/usr/bin/env python -I
"""Regenerate what hsed's TypeUIDs.hs hand-transcribes from spec/ir/core-2.01.json and diff the two.

Core Spec 2.01 section 5.1.3 (Tables 46-165) holds, per column type, one 'Type table' row
(UID / Name / Format) and, for enumerations and sets, a companion 'Enumeration Values' /
'Set Values' table (value-or-range / label).  hsed embeds them as [ttype| ... |] and
[tenum| ... |] ASCII-art quasiquotes.  This script:

  1. reads the extracted tables and derives the (UID bytes, Name, [Format tokens]) triple of
     every Type table and the [(lo, hi, label)] pairs of every enumeration/set table;
  2. parses the quasiquote blocks of TypeUIDs.hs with the same grammar the TH parsers use
     (fields are fixed-width, consecutive lines of a row are concatenated per column);
  3. pairs them in document order (100 ttype blocks <-> 100 Type tables, 19 tenum blocks <->
     19 enum/set tables; Table 87 gen_status is a commented-out FIXME in hsed) and reports
     every difference, classified.

Usage:  python -I regen_typeuids.py spec/ir/core-2.01.json TypeUIDs.hs regen_report.txt regen_typeuids.json
"""
from __future__ import annotations

import json
import re
import sys
from collections import Counter
from itertools import zip_longest

TYPE_HEADER = ["UID", "Name", "Format"]
ENUM_CAPTION_RE = re.compile(r"Enumeration Values|Set Values|Enumes", re.I)
CAPTION_RE = re.compile(r"^\s*Table\s+(\d+)\s*(.*?)\s*$")
RANGE_RE = re.compile(r"^(\d+)\s*(?:(?:-|–|to)\s*(\d+))?$")
UID_RE = re.compile(r"^(?:[0-9A-Fa-f]{2}\s*){8}$")
WS = re.compile(r"\s+")

# Known, deliberate hand edits in hsed (PDF text -> hsed text), from tcg/'Core Spec errata.txt'
# and the hsed-tables map: these are typo corrections the PDF-anchored source should carry explicitly.
DELIBERATE = [
    ("Fractoin", "Fraction"),            # Table 75 clock_time Format
    ("Eror", "Error"),                   # Table 101 Enumeration label 'Verify Eror'
    ("_object _ref", "_object_ref"),     # captions of Tables 72, 76, 131, 140, 142, 144 (112 kept the typo)
]
# Known places where the PDF is correct and hsed's hand transcription differs (PDF text -> hsed
# text): not typos, so the PDF-anchored source keeps the text as printed and carries no erratum.
HAND_TRANSCRIPTION = [
    ("Set Value", "Enumeration Value"),  # Table 128 reset_types, a Set Values table: 'Set Value | Associated Value'
    ("Operator", "Associated Value"),    # Table 61 boolean_ACE: 'Enumeration Value | Operator' (And, Or)
]
# Known accidental corruptions in hsed (by PDF table number), from the hsed-tables map.
EXPECTED_CAPTION_QUIRKS = {
    82: "caption 'enc_supported Enumes' is a garbled/truncated copy of 'enc_supported Enumeration Values' (harmless: tenum reads only the number and identifier)",
}
EXPECTED_HSED_ERRORS = {
    129: "UID of Seconds transcribed as all zeros",
    83: "feedback_size carries date's UID 00 00 00 05 00 00 18 04",
    85: "fraction_enum carries date's UID",
    86: "gen_status carries date's UID",
    157: "caption numbered 147 (and named uinteger_1) instead of 157 uinteger_4",
    161: "caption numbered 160 instead of 161",
}


def squash(s: str) -> str:
    return WS.sub("", s)


def norm(s: str) -> str:
    return WS.sub(" ", s).strip()


# ----------------------------------------------------------------------------- PDF side
def pdf_type_row(t: dict) -> dict:
    rows = t["rows"]
    uid_txt, name, fmt = (c["text"] for c in rows[0][:3])
    uid = [int(b, 16) for b in uid_txt.split()] if UID_RE.match(uid_txt) else None
    tokens = [tok.strip() for tok in fmt.split(",")]
    tokens = [tok for tok in tokens if tok]
    problems = ([f"expected 1 data row, got {len(rows)}"] if len(rows) != 1 else []) \
        + ([f"bad UID cell {uid_txt!r}"] if uid is None else []) \
        + ([f"header {[c['text'] for c in t['header_rows'][0]]}"] if [c["text"] for c in t["header_rows"][0]] != TYPE_HEADER else [])
    return {"table": t["table_number"], "caption": t["caption"], "uid": uid, "uid_text": uid_txt,
            "name": name, "format_tokens": tokens, "format_text": fmt, "pages": [p + 1 for p in t["pages"]], "problems": problems}


def pdf_enum_rows(t: dict) -> dict:
    header = [c["text"] for c in t["header_rows"][0]] if t["header_rows"] else []

    def parse(r):
        m = RANGE_RE.match(r[0]["text"])
        lo, hi = (int(m.group(1)), int(m.group(2) or m.group(1))) if m else (None, None)
        return {"value": r[0]["text"], "lo": lo, "hi": hi, "label": r[1]["text"] if len(r) > 1 else "",
                "extra": [c["text"] for c in r[2:]]}

    rows = [parse(r) for r in t["rows"]]
    problems = [f"unparsed value cell {r['value']!r}" for r in rows if r["lo"] is None] \
        + ([f"{t['n_cols']} columns"] if t["n_cols"] != 2 else [])
    return {"table": t["table_number"], "caption": t["caption"], "header": header, "rows": rows,
            "pages": [p + 1 for p in t["pages"]], "problems": problems}


def classify_pdf(tables: list[dict]) -> tuple[list[dict], list[dict], list[dict]]:
    sel = sorted((t for t in tables if t["table_number"] is not None and 46 <= t["table_number"] <= 165 and not t.get("part")),
                 key=lambda t: t["table_number"])
    is_type = lambda t: t["header_rows"] and [c["text"] for c in t["header_rows"][0]] == TYPE_HEADER
    types = [pdf_type_row(t) for t in sel if is_type(t)]
    enums = [pdf_enum_rows(t) for t in sel if not is_type(t) and ENUM_CAPTION_RE.search(t["caption"] or "")]
    other = [t for t in sel if not is_type(t) and not ENUM_CAPTION_RE.search(t["caption"] or "")]
    return types, enums, other


# ----------------------------------------------------------------------------- hsed side
def split_cells(line: str, cuts: list[int]) -> list[str]:
    """Fixed-width fields exactly as TH.hs takeField does: `take len` then trim trailing whitespace;
    the leading indentation 'field' is dropped (tail)."""
    fields = [line[a + 1:b].rstrip() for a, b in zip(cuts, cuts[1:])]
    return fields


def parse_block(body: str) -> dict:
    lines = body.splitlines()
    cap = next((CAPTION_RE.match(l) for l in lines if CAPTION_RE.match(l)), None)
    seps = [i for i, l in enumerate(lines) if l.lstrip().startswith("+")]
    cuts = [i for i, ch in enumerate(lines[seps[0]]) if ch == "+"]
    groups = [[split_cells(l, cuts) for l in lines[a + 1:b]] for a, b in zip(seps, seps[1:])]
    # a group's columns are concatenated line by line with no separator (map mconcat . transpose)
    merged = [["".join(col) for col in zip(*g)] for g in groups if g]
    return {"num": int(cap.group(1)) if cap else None, "caption_rest": cap.group(2) if cap else None,
            "caption_line": norm(cap.group(0)) if cap else None, "header": merged[0], "rows": merged[1:],
            "groups": groups}


def hsed_blocks(src: str, kind: str) -> list[dict]:
    return [parse_block(m.group(1)) for m in re.finditer(r"^\[" + kind + r"\|(.*?)^\|\]", src, re.S | re.M)]


def hsed_type(b: dict) -> dict:
    uid_txt, name, fmt = b["rows"][0][:3]
    fmt = fmt[:-1] if fmt.endswith(",") else fmt         # trimComma
    tokens = [tok.strip() for tok in fmt.split(",") if tok.strip()]
    return {"num": b["num"], "caption_line": b["caption_line"], "uid_text": uid_txt,
            "uid": [int(x, 16) for x in uid_txt.split()] if UID_RE.match(uid_txt) else None,
            "name": name.strip(), "format_string": fmt, "format_tokens": tokens, "header": b["header"]}


def hsed_enum(b: dict) -> dict:
    def row(r):
        m = RANGE_RE.match(r[0].strip())
        return {"value": r[0].strip(), "lo": int(m.group(1)) if m else None,
                "hi": int(m.group(2) or m.group(1)) if m else None, "label": r[1]}
    return {"num": b["num"], "caption_line": b["caption_line"], "header": b["header"], "rows": [row(r) for r in b["rows"]]}


# ----------------------------------------------------------------------------- diffing
def known_edit(edits: list[tuple[str, str]], pdf_s: str, hs_s: str) -> str | None:
    hits = [f"{a!r} -> {b!r}" for a, b in edits if a in pdf_s and squash(pdf_s.replace(a, b)) == squash(hs_s)]
    return hits[0] if hits else None


def compare_text(field: str, pdf_s: str, hs_s: str, table: int) -> list[dict]:
    if pdf_s == hs_s:
        return []
    known = [(cls, note) for cls, edits in (("deliberate typo fix in hsed", DELIBERATE),
                                            ("hsed hand-transcription difference (PDF correct)", HAND_TRANSCRIPTION))
             if (note := known_edit(edits, pdf_s, hs_s))]
    if known:
        cls, note = known[0]
        return [{"table": table, "field": field, "pdf": pdf_s, "hsed": hs_s, "class": cls, "note": note}]
    if squash(pdf_s) == squash(hs_s):
        return [{"table": table, "field": field, "pdf": pdf_s, "hsed": hs_s, "class": "whitespace-only"}]
    if field == "caption" and (squash(pdf_s).startswith(squash(hs_s)) or table in EXPECTED_CAPTION_QUIRKS):
        return [{"table": table, "field": field, "pdf": pdf_s, "hsed": hs_s, "class": "hsed transcription error",
                 "note": EXPECTED_CAPTION_QUIRKS.get(table, "caption truncated in hsed (harmless: tenum/ttype only read the table number and identifier)")}]
    return [{"table": table, "field": field, "pdf": pdf_s, "hsed": hs_s, "class": "UNEXPLAINED"}]


def diff_types(pdf_types: list[dict], hs_types: list[dict]) -> list[dict]:
    out = []
    for p, h in zip(pdf_types, hs_types):
        n = p["table"]
        if h["num"] != n:
            out.append({"table": n, "field": "caption number", "pdf": n, "hsed": h["num"],
                        "class": "hsed transcription error" if n in EXPECTED_HSED_ERRORS else "UNEXPLAINED",
                        "note": EXPECTED_HSED_ERRORS.get(n)})
        out += compare_text("caption", p["caption"], h["caption_line"], n) if h["num"] == n else []
        if p["uid"] != h["uid"]:
            out.append({"table": n, "field": "UID", "pdf": p["uid_text"], "hsed": h["uid_text"],
                        "class": "hsed transcription error" if n in EXPECTED_HSED_ERRORS else "UNEXPLAINED",
                        "note": EXPECTED_HSED_ERRORS.get(n)})
        out += compare_text("Name", p["name"], h["name"], n)
        if p["format_tokens"] != h["format_tokens"]:
            out += compare_text("Format", ", ".join(p["format_tokens"]), ", ".join(h["format_tokens"]), n)
        out += [{"table": n, "field": "extraction", "pdf": pr, "hsed": None, "class": "extraction problem"} for pr in p["problems"]]
    return out


def diff_enums(pdf_enums: list[dict], hs_enums: list[dict]) -> list[dict]:
    out = []
    for p, h in zip(pdf_enums, hs_enums):
        n = p["table"]
        if h["num"] != n:
            out.append({"table": n, "field": "caption number", "pdf": n, "hsed": h["num"], "class": "UNEXPLAINED"})
            continue
        out += compare_text("caption", p["caption"], h["caption_line"], n)
        out += compare_text("header", " | ".join(p["header"]), " | ".join(h["header"]), n)
        for i, (pr, hr) in enumerate(zip_longest(p["rows"], h["rows"])):
            if pr is None or hr is None:
                out.append({"table": n, "field": f"row {i}", "pdf": pr, "hsed": hr, "class": "UNEXPLAINED", "note": "row count differs"})
                continue
            if (pr["lo"], pr["hi"]) != (hr["lo"], hr["hi"]):
                out.append({"table": n, "field": f"row {i} value", "pdf": pr["value"], "hsed": hr["value"], "class": "UNEXPLAINED"})
            out += compare_text(f"row {i} label", pr["label"], hr["label"], n)
        out += [{"table": n, "field": "extraction", "pdf": pr, "hsed": None, "class": "extraction problem"} for pr in p["problems"]]
    return out


# ----------------------------------------------------------------------------- report
def fmt_diff(d: dict) -> str:
    note = f"   [{d['note']}]" if d.get("note") else ""
    return f"  Table {d['table']:>3} {d['field']:<16} pdf={d['pdf']!r}  hsed={d['hsed']!r}{note}"


def main(argv):
    tables_json, hs_path, report_path, out_json = argv[1:5]
    tables = json.load(open(tables_json))["tables"]
    src = open(hs_path, encoding="utf-8").read()
    pdf_types, pdf_enums, pdf_other = classify_pdf(tables)
    hs_types = [hsed_type(b) for b in hsed_blocks(src, "ttype")]
    hs_enums = [hsed_enum(b) for b in hsed_blocks(src, "tenum")]
    hs_enum_nums = {h["num"] for h in hs_enums}
    pdf_enums_embedded = [p for p in pdf_enums if p["table"] in hs_enum_nums]
    pdf_enums_missing = [p for p in pdf_enums if p["table"] not in hs_enum_nums]

    diffs = diff_types(pdf_types, hs_types) + diff_enums(pdf_enums_embedded, hs_enums)
    classes = Counter(d["class"] for d in diffs)
    by_class = {c: [d for d in diffs if d["class"] == c] for c in classes}

    summary = [
        "REGEN REPORT: Core Spec 2.01 Tables 46-165 (section 5.1.3) extracted from the PDF vs hsed TypeUIDs.hs",
        "=" * 100,
        f"PDF Type tables (UID/Name/Format): {len(pdf_types)}   hsed [ttype| blocks: {len(hs_types)}",
        f"PDF enum/set tables:               {len(pdf_enums)}   hsed [tenum| blocks: {len(hs_enums)}"
        f"   (PDF enum tables not embedded in hsed: {[p['table'] for p in pdf_enums_missing]})",
        f"Other tables in 46-165:            {[t['table_number'] for t in pdf_other]}",
        f"Table numbers paired in order:     types {[p['table'] for p in pdf_types] == sorted(p['table'] for p in pdf_types)}, "
        f"enums {[p['table'] for p in pdf_enums_embedded]}",
        "",
        "Differences by class:",
        *(f"  {c:<50} {n}" for c, n in sorted(classes.items())),
        f"  {'TOTAL':<50} {len(diffs)}",
        f"  UNEXPLAINED differences: {classes.get('UNEXPLAINED', 0)}   extraction problems: {classes.get('extraction problem', 0)}",
        "",
        "Regenerated data: Type triples = {} (all UIDs 8 bytes: {}), enum tables = {} with {} rows".format(
            len(pdf_types), all(p["uid"] and len(p["uid"]) == 8 for p in pdf_types), len(pdf_enums), sum(len(p["rows"]) for p in pdf_enums)),
    ]
    details = ["", "DETAILS BY CLASS", "-" * 100]
    for c, ds in sorted(by_class.items()):
        details += [f"[{c}] ({len(ds)})"] + [fmt_diff(d) for d in ds] + [""]
    regen = ["REGENERATED TYPE TRIPLES (table, UID, Name, Format tokens)", "-" * 100] + \
        [f"  T{p['table']:>3} {' '.join(f'{b:02X}' for b in p['uid'])}  {p['name']:<28} {p['format_tokens']}" for p in pdf_types] + \
        ["", "REGENERATED ENUM/SET TABLES", "-" * 100] + \
        [line for p in pdf_enums for line in
         [f"  T{p['table']:>3} {p['caption']}   header={p['header']}"] +
         [f"        {r['lo']:>4}-{r['hi']:<4} {r['label']}" + (f"   {r['extra']}" if r['extra'] else "") for r in p["rows"]]]
    text = "\n".join(summary + details + regen) + "\n"
    open(report_path, "w").write(text)
    json.dump({"types": pdf_types, "enums": pdf_enums, "diffs": diffs}, open(out_json, "w"), indent=1)
    print("\n".join(summary))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
