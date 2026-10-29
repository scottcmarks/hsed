#!/usr/bin/env python -I
"""Extract a tagged TCG PDF (Core 2.01 / Opal SSC 2.30) into IR v1 (docs/ir-v1.md): every /Table,
every /H1../H6 heading, every /P and /LI outside tables, and every hsed annotation in the input.

Two independent views of the same document are joined by the structure-tree path:

  * pikepdf walks /StructTreeRoot and gives the *logical* model: every /Table (top level
    and nested), its /TR rows, its /TH|/TD|/Artifact cells, the /K-array index path of
    every element, and -- through the /Pg of each marked-content leaf -- the pages a table
    touches.  This view already stitches page-spanning tables.
  * pymupdf (MuPDF 1.28, TEXT_COLLECT_STRUCTURE) gives the *physical* model per page: the
    XML text page wraps characters in <struct idx raw std> nodes whose idx chain is exactly
    the same /K index path, so every character (glyph quad, font, size, colour) can be
    attributed to a cell without interpreting the content stream ourselves.  Filled
    rectangles from page.get_drawings() supply cell shading and underline strokes.

Output: one sorted-key JSON document (irVersion 1; see `main` for the top level, `table_record`,
`prose_records` and `annot_records` for the entries) plus a one-line-per-table index.  The run ends
with the sanity invariants of design section 5.1 step 9, printed PASS/FAIL to stderr; any FAIL makes
the exit status 1.  The output is deterministic (no timestamps, no absolute paths).

Usage:
  python -I extract_tables.py <pdf> <out.json> <index.txt> --spec core-2.01 --page-offset 17
                              [--pristine <original.pdf>] [--check-against <core.json>] [--pages a-b]
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import sys
import time
import unicodedata
import xml.etree.ElementTree as ET
from collections import Counter, defaultdict
from itertools import takewhile
from typing import Iterable

import pikepdf
import pymupdf
from pikepdf import Array, Dictionary

# ----------------------------------------------------------------------------- constants
IR_VERSION = 1
NOTICE = "tcg/TCG-NOTICE.txt"          # the TCG licence the extracted data travels under
CELL_TAGS = {"/TH", "/TD", "/Artifact"}
ROW_GROUP_TAGS = {"/THead", "/TBody", "/TFoot"}
HEADING_TAGS = {f"/H{i}" for i in range(1, 7)}
PARA_TAGS = {"/P", "/LI"}
THIN = 1.6            # pt: a filled rect thinner than this is a rule / underline, not shading
UNDER_LO, UNDER_HI = -0.10, 0.22   # underline rect centre must lie this far below the baseline (em); Word draws
                                   # underlines at ~0.15 em, a tight cell's bottom border sits at >= 0.25 em
CAPTION_RE = re.compile(r"^\s*Table\s+(\d+)\b")
WS_RE = re.compile(r"\s+")


def norm(s: str) -> str:
    return WS_RE.sub(" ", s).strip()


# ----------------------------------------------------------------------------- pikepdf: logical tree
def kids(node) -> list:
    k = node.get("/K") if isinstance(node, Dictionary) else None
    return [] if k is None else (list(k) if isinstance(k, Array) else [k])


def is_elem(x) -> bool:
    return isinstance(x, Dictionary) and "/S" in x and not ("/Type" in x and str(x.Type) in ("/MCR", "/OBJR"))


def tag(x) -> str | None:
    return str(x.S) if is_elem(x) else None


def leaf_pages(node, page_of: dict, inherited=None) -> set[int]:
    """0-based page indices of every marked-content leaf under `node` (handles /Pg inheritance)."""
    if isinstance(node, Array):
        return set().union(*(leaf_pages(k, page_of, inherited) for k in node)) if len(node) else set()
    if isinstance(node, Dictionary):
        pg = node.get("/Pg")
        here = page_of.get(pg.objgen) if pg is not None and hasattr(pg, "objgen") else inherited
        if "/Type" in node and str(node.Type) == "/MCR":
            return {here} - {None}
        if "/Type" in node and str(node.Type) == "/OBJR":
            return set()
        k = node.get("/K")
        if k is None:
            return set()
        if not isinstance(k, (Dictionary, Array)):          # bare MCID integer
            return {here} - {None}
        return leaf_pages(k, page_of, here)
    return {inherited} - {None}                            # bare integer inside a K array


def attr_dict(elem) -> dict:
    """Flatten /A (dict or array of dicts) into one plain dict of the table attributes we care about."""
    a = elem.get("/A") if isinstance(elem, Dictionary) else None
    dicts = [a] if isinstance(a, Dictionary) else [d for d in (a or []) if isinstance(d, Dictionary)] if isinstance(a, Array) else []
    wanted = ("/ColSpan", "/RowSpan", "/Scope", "/Headers")
    return {k[1:]: (int(d[k]) if k in ("/ColSpan", "/RowSpan") else str(d[k])) for d in dicts for k in wanted if k in d}


def find_tables(root, page_of) -> list[dict]:
    """Every /Table element in the tree: path, element, nesting parent path, sibling info."""
    out = []

    def walk(node, path, table_ancestors):
        ks = kids(node)
        for i, k in enumerate(ks):
            if not is_elem(k):
                continue
            p = path + [i]
            if tag(k) == "/Table":
                out.append({"path": p, "elem": k, "parent": node, "siblings": ks,
                            "nested_in": table_ancestors[-1] if table_ancestors else None})
                walk(k, p, table_ancestors + [p])
            else:
                walk(k, p, table_ancestors)

    walk(root, [], [])
    return out


def table_rows(telem) -> list[tuple[list[int], object]]:
    """[(row_path_suffix, TR element)] flattening any THead/TBody/TFoot wrappers (none in these PDFs)."""
    rows = []
    for i, r in enumerate(kids(telem)):
        if tag(r) == "/TR":
            rows.append(([i], r))
        elif tag(r) in ROW_GROUP_TAGS:
            rows.extend(([i, j], rr) for j, rr in enumerate(kids(r)) if tag(rr) == "/TR")
    return rows


# ----------------------------------------------------------------------------- pymupdf: physical pages
def parse_quad(q: str) -> list[tuple[float, float]]:
    v = [float(t) for t in q.split()]
    return [(v[0], v[1]), (v[2], v[3]), (v[4], v[5]), (v[6], v[7])]


def page_chars(page) -> tuple[dict, dict]:
    """Return (chars_by_path, types_by_path): all characters on the page keyed by the full struct
    path (tuple of /K indices) of the innermost struct element that contains them."""
    xml = page.get_text("xml", flags=pymupdf.TEXT_COLLECT_STRUCTURE | pymupdf.TEXT_PRESERVE_WHITESPACE)
    root = ET.fromstring(xml)
    by_path, types = defaultdict(list), {}
    seq = [0]                      # content order on the page; nested elements interleave with their parent

    def char_rec(ch, fname, fsize, d):
        seq[0] += 1
        return {"c": ch.get("c"), "quad": parse_quad(ch.get("quad")),
                "origin": (float(ch.get("x")), float(ch.get("y"))),
                "font": fname, "size": fsize, "flags": int(ch.get("flags") or 0),
                "color": ch.get("color"), "dir": d, "seq": seq[0]}

    def walk(el, chain, raws):
        for c in el:
            if c.tag == "struct":
                ch = chain + (int(c.get("idx")),)
                types[ch] = raws + (c.get("raw"),)
                walk(c, ch, raws + (c.get("raw"),))
            elif c.tag == "block":
                for line in c.iter("line"):
                    d = tuple(float(t) for t in line.get("dir").split())
                    for font in line.iter("font"):
                        by_path[chain].extend(char_rec(ch, font.get("name"), float(font.get("size")), d) for ch in font.iter("char"))

    walk(root, (), ())
    return by_path, types


def page_fills(page) -> list[dict]:
    """Filled drawings (MuPDF coords), in drawing order, with RGB 0-255."""
    return [{"rect": d["rect"], "rgb": tuple(round(c * 255) for c in d["fill"]), "seq": i}
            for i, d in enumerate(page.get_drawings()) if d.get("fill") is not None and d["rect"].is_valid and not d["rect"].is_empty]


def split_fills(fills):
    thin = [f for f in fills if min(f["rect"].width, f["rect"].height) < THIN]
    fat = [f for f in fills if min(f["rect"].width, f["rect"].height) >= THIN]
    return thin, fat


def frame(ch):
    """Projection into the glyph's writing frame: (along baseline, below baseline), in pt.
    MuPDF y grows downwards, so for direction (dx,dy) the 'down' unit vector is (-dy,dx)."""
    dx, dy = ch["dir"]
    ox, oy = ch["origin"]
    return lambda x, y: ((x - ox) * dx + (y - oy) * dy, (x - ox) * (-dy) + (y - oy) * dx)


def line_underline_rects(line: list[dict], thin_rects) -> list:
    """Thin filled rects that underline (part of) this text line: centred just below the baseline
    and not extending beyond the line's ends by more than 0.3 em -- a cell border is thin too, but
    it spans the whole cell (padding on both sides) and so fails the length test; for rotated
    cells the right-hand border would otherwise sit exactly where an underline does."""
    proj = frame(line[0])
    size = line[0]["size"]
    ink = [ch for ch in line if ch["c"].strip()] or line        # synthetic trailing spaces must not widen the line
    alongs = [proj(x, y)[0] for ch in ink for x, y in ch["quad"]]
    lo, hi = min(alongs) - 0.3 * size, max(alongs) + 0.3 * size

    def is_underline(r):
        ps = [proj(x, y) for x, y in ((r.x0, r.y0), (r.x1, r.y0), (r.x0, r.y1), (r.x1, r.y1))]
        below = sum(b for _, b in ps) / 4
        return (UNDER_LO * size <= below <= UNDER_HI * size
                and lo <= min(a for a, _ in ps) and max(a for a, _ in ps) <= hi)

    return [(min(a for a, _ in ps), max(a for a, _ in ps))
            for r in (f["rect"] for f in thin_rects) if is_underline(r)
            for ps in [[proj(x, y) for x, y in ((r.x0, r.y0), (r.x1, r.y0), (r.x0, r.y1), (r.x1, r.y1))]]]


def underlined_chars(line: list[dict], thin_rects) -> list[bool]:
    proj = frame(line[0])
    spans = line_underline_rects(line, thin_rects)
    mids = [sum(proj(x, y)[0] for x, y in ch["quad"]) / 4 for ch in line]
    return [any(a <= m <= b for a, b in spans) for m in mids]


def is_bold(ch) -> bool:
    return bool(ch["flags"] & 8) or "Bold" in ch["font"] or "bold" in ch["font"].lower()


def union_rect(points: Iterable[tuple[float, float]]) -> pymupdf.Rect | None:
    pts = list(points)
    if not pts:
        return None
    xs, ys = [p[0] for p in pts], [p[1] for p in pts]
    return pymupdf.Rect(min(xs), min(ys), max(xs), max(ys))


def to_pdf_space(rect: pymupdf.Rect, inv: pymupdf.Matrix) -> list[float]:
    """MuPDF (top-left origin) rect -> PDF user-space [x0,y0,x1,y1] with y up."""
    r = rect * inv
    r.normalize()
    return [round(r.x0, 2), round(r.y0, 2), round(r.x1, 2), round(r.y1, 2)]


def fill_at(point: pymupdf.Point, fat_rects) -> dict | None:
    """Last-drawn (top-most) non-thin filled rect containing the point."""
    hits = [f for f in fat_rects if f["rect"].contains(point)]
    return hits[-1] if hits else None


# ----------------------------------------------------------------------------- building cells
class PageCache:
    """Lazy per-page physical data (chars by struct path, fills) so each page is parsed once."""

    def __init__(self, doc):
        self.doc, self._chars, self._fills, self._grid = doc, {}, {}, {}

    def chars(self, pno):
        if pno not in self._chars:
            self._chars[pno] = page_chars(self.doc[pno])
        return self._chars[pno]

    def fills(self, pno):
        if pno not in self._fills:
            self._fills[pno] = split_fills(page_fills(self.doc[pno]))
        return self._fills[pno]

    def grid(self, pno) -> list[dict]:
        """The drawn cell grid: every table pymupdf's lines_strict strategy sees on the page, as
        {'bbox': Rect, 'rows': [[Rect|None, ...], ...]} in MuPDF coords."""
        if pno not in self._grid:
            self._grid[pno] = [{"bbox": pymupdf.Rect(t.bbox), "rows": [[pymupdf.Rect(c) if c else None for c in r.cells] for r in t.rows]}
                               for t in self.doc[pno].find_tables(strategy="lines_strict").tables]
        return self._grid[pno]

    def grid_cells(self, pno) -> list[pymupdf.Rect]:
        return [c for t in self.grid(pno) for r in t["rows"] for c in r if c]

    def inv(self, pno):
        return ~self.doc[pno].transformation_matrix


def chars_under(cache: PageCache, path: tuple, pages: Iterable[int], stop_tags: Iterable[str] = ("Table",)) -> list[tuple[int, dict]]:
    """All (page, char) records under struct path `path` on the given pages.  Characters that belong
    to an element tagged in `stop_tags` nested *below* `path` are excluded: by default a nested /Table
    (its own cells), for a list item also a nested /L (its own items)."""
    n, stop = len(path), set(stop_tags)

    def keep(p, raws):
        return p[:n] == path and not (stop & set(raws[n:]))

    return sorted(((pno, ch)
                   for pno in sorted(pages)
                   for p, recs in cache.chars(pno)[0].items() if keep(p, cache.chars(pno)[1].get(p, ()))
                   for ch in recs), key=lambda pc: (pc[0], pc[1]["seq"]))


def group_lines(recs: list[tuple[int, dict]]) -> list[list[dict]]:
    """Characters (content order) -> visual lines: a char continues the current line when it is on
    the same page, written in the same direction and sits on the same baseline.  This re-interleaves
    nested elements (/Reference, /Span, /Link) that MuPDF emits as separate blocks."""
    def same_line(prev, ch):
        _, below = frame(prev[1])(*ch["origin"])
        return prev[0] == ch["__page"] and prev[1]["dir"] == ch["dir"] and abs(below) < 0.5 * prev[1]["size"]

    lines = []
    for pno, ch in recs:
        ch["__page"] = pno
        (lines[-1] if lines and same_line((lines[-1][0]["__page"], lines[-1][-1]), ch) else lines.append([]) or lines[-1]).append(ch)
    # reading order: lines top-to-bottom along the writing frame, glyphs left-to-right within a line
    # (MuPDF's XML follows structure-tree order, which differs from position for re-attributed glyphs)
    if lines:
        proj = frame(lines[0][0])
        for ln in lines:
            along = frame(ln[0])
            ln.sort(key=lambda ch: along(*ch["origin"])[0])
        lines.sort(key=lambda ln: (ln[0]["__page"], round(proj(*ln[0]["origin"])[1], 1)))
    return lines


def line_text(line: list[dict]) -> str:
    """Join chars, inserting a space where consecutive glyphs leave a gap wider than 0.25 em."""
    proj = frame(line[0])
    ends = [max(proj(x, y)[0] for x, y in ch["quad"]) for ch in line]
    starts = [min(proj(x, y)[0] for x, y in ch["quad"]) for ch in line]
    gaps = [False] + [s - e > 0.25 * ch["size"] and not (ch["c"].isspace() or prev["c"].isspace())
                      for prev, ch, e, s in zip(line, line[1:], ends, starts[1:])]
    return "".join((" " if g else "") + ch["c"] for g, ch in zip(gaps, line))


def text_lines(recs: list[tuple[int, dict]]) -> list[str]:
    return [norm(t) for t in map(line_text, group_lines(recs)) if norm(t)]


DERIVED_KEYS = {"text", "lines", "page", "bbox", "fragments", "bold", "underline", "fonts", "color",
                "fill_rgb", "fill_bbox", "est_bbox", "_recs"}


def record_from_chars(cache: PageCache, kind: str, recs: list[tuple[int, dict]], extra: dict | None = None) -> dict:
    """Build a cell record from its (page, char) records.  The chars are kept under the private key
    '_recs' until the reconciliation passes have run; strip_private() removes them before output."""
    recs = sorted(recs, key=lambda pc: (pc[0], pc[1]["seq"]))
    ink = [(p, ch) for p, ch in recs if ch["c"].strip()]
    vlines = group_lines(recs)
    lines = [norm(t) for t in map(line_text, vlines) if norm(t)]
    by_page = {p: [ch for pp, ch in ink if pp == p] for p in sorted({p for p, _ in ink})}
    frags = [(p, union_rect(pt for ch in chs for pt in ch["quad"])) for p, chs in by_page.items()]
    first_page = frags[0][0] if frags else None
    rec = {"kind": kind, "text": " ".join(lines), "lines": lines, "page": first_page,
           "bbox": to_pdf_space(frags[0][1], cache.inv(first_page)) if frags else None}
    if len(frags) > 1:
        rec["fragments"] = [{"page": p, "bbox": to_pdf_space(r, cache.inv(p))} for p, r in frags]
    if ink:
        thin = {p: cache.fills(p)[0] for p in by_page}
        under = [u for vl in vlines for ch, u in zip(vl, underlined_chars(vl, thin[vl[0]["__page"]])) if ch["c"].strip()]
        rec["bold"] = all(is_bold(ch) for _, ch in ink)
        rec["underline"] = all(under)
        rec["fonts"] = sorted({ch["font"] for _, ch in ink})
        colors = {ch["color"] for _, ch in ink}
        if colors != {"#000000"}:
            rec["color"] = sorted(colors)
        centre = pymupdf.Point((frags[0][1].x0 + frags[0][1].x1) / 2, (frags[0][1].y0 + frags[0][1].y1) / 2)
        f = fill_at(centre, cache.fills(first_page)[1])
        rec["fill_rgb"] = list(f["rgb"]) if f else None
        rec["fill_bbox"] = to_pdf_space(f["rect"], cache.inv(first_page)) if f else None
    else:
        rec.update({"bold": None, "underline": None, "fill_rgb": None})
    rec.update(extra or {})
    rec["_recs"] = recs
    return rec


def rebuild(cache: PageCache, cell: dict, recs: list[tuple[int, dict]]) -> None:
    """Re-derive a cell in place from a new set of chars, keeping its non-derived keys."""
    keep = {k: v for k, v in cell.items() if k not in DERIVED_KEYS}
    cell.clear()
    cell.update(record_from_chars(cache, keep.pop("kind"), recs, keep))


def cell_record(cache: PageCache, cell_elem, cell_path: tuple, pages: set[int], nested_paths: list) -> dict:
    """`pages` are the pages this cell's own marked-content leaves live on (empty -> no glyphs)."""
    recs = chars_under(cache, cell_path, pages) if pages else []
    extra = {**attr_dict(cell_elem), **({"nested_tables": nested_paths} if nested_paths else {})}
    return record_from_chars(cache, tag(cell_elem)[1:], recs, extra)


def strip_private(records: list[dict]) -> list[dict]:
    for r in records:
        for row in r["header_rows"] + r["rows"] + [x["cells"] for x in r["repeated_header_rows"]]:
            for c in row:
                c.pop("_recs", None)
    return records


# ----------------------------------------------------------------------------- struct-vs-geometry reconciliation
# Word's PDFMaker occasionally mis-tags a table: two adjacent cells become one /TD (Core Tables 71,
# 72, 73, 81: Name and Format share a cell) and a cell's continuation lines on the next page are hung
# under the previous table's cell (Core Table 70 owns Table 71's 'bytes, 64').  Both are repaired from
# the drawn geometry: header column bands + vertical rules, and the lines_strict cell grid.

def pdf_x_extent(cache: PageCache, pno: int, ch: dict) -> tuple[float, float]:
    pts = [pymupdf.Point(x, y) * cache.inv(pno) for x, y in ch["quad"]]
    return min(p.x for p in pts), max(p.x for p in pts)


def vertical_rule_between(cache: PageCache, pno: int, x_gap: tuple[float, float], y_range: tuple[float, float]) -> bool:
    """Is there a drawn vertical rule (thin filled rect) inside x_gap spanning most of y_range (PDF space)?"""
    y0, y1 = y_range
    for f in cache.fills(pno)[0]:
        r = to_pdf_space(f["rect"], cache.inv(pno))
        if r[2] - r[0] < THIN and x_gap[0] <= (r[0] + r[2]) / 2 <= x_gap[1]:
            overlap = min(r[3], y1) - max(r[1], y0)
            if overlap >= 0.5 * (y1 - y0):
                return True
    return False


def split_merged_cells(rows: list[list[dict]], cache: PageCache) -> list[list[dict]]:
    """A data row narrower than the header may contain a cell whose glyphs partition cleanly into
    several header column bands with a vertical rule between the parts: that is two (or more) cells
    mis-tagged as one, so split it.  A glyph crossing a band boundary, or no rule, means a genuinely
    merged cell (byte-layout tables) and the cell is left alone."""
    if len(rows) < 2 or not rows[0] or not all(c.get("bbox") for c in rows[0]):
        return rows
    lefts = [c["bbox"][0] for c in rows[0]]

    def band_of(x):
        return max((j for j, l in enumerate(lefts) if x >= l - 2.0), default=0)

    def parts_of(c):
        recs = c.get("_recs") or []
        ink = [(p, ch) for p, ch in recs if ch["c"].strip()]
        if not ink or len({p for p, _ in ink}) != 1 or not c.get("bbox"):
            return [c]
        pno = ink[0][0]
        placed = [(band_of(x0 + 0.1), band_of(x1 - 0.1), p, ch) for p, ch in ink for x0, x1 in [pdf_x_extent(cache, pno, ch)]]
        if any(b0 != b1 for b0, b1, _, _ in placed):
            return [c]
        groups = {}
        for b0, _, p, ch in placed:
            groups.setdefault(b0, []).append((p, ch))
        if len(groups) < 2:
            return [c]
        bands = sorted(groups)
        y_range = (c["bbox"][1], c["bbox"][3])
        gaps = [(max(pdf_x_extent(cache, pno, ch)[1] for _, ch in groups[a]), min(pdf_x_extent(cache, pno, ch)[0] for _, ch in groups[b]))
                for a, b in zip(bands, bands[1:])]
        if not all(g[0] < g[1] and vertical_rule_between(cache, pno, g, y_range) for g in gaps):
            return [c]
        spaces = [(p, ch) for p, ch in recs if not ch["c"].strip()]
        extra = {k: v for k, v in c.items() if k not in DERIVED_KEYS and k != "kind"}
        return [record_from_chars(cache, c["kind"], groups[b] + [(p, ch) for p, ch in spaces if band_of(sum(pdf_x_extent(cache, p, ch)) / 2) == b],
                                  {**extra, "split_from_merged": True}) for b in bands]

    return [rows[0]] + [[part for c in r for part in (parts_of(c) if len(r) < len(rows[0]) else [c])] for r in rows[1:]]


def adopt_stray_fragments(records: list[dict], cache: PageCache) -> None:
    """A cell whose glyphs continue on a page where none of its row-mates live has a stray fragment.
    If the drawn cell grid (find_tables, lines_strict) on that page places the fragment inside a cell
    of another table, move the glyphs there."""
    top = [r for r in records if not r["nested_in"]]
    all_cells = lambda r: [c for row in r["header_rows"] + r["rows"] + [x["cells"] for x in r["repeated_header_rows"]] for c in row]

    def centre_mupdf(pno, chars):
        r = union_rect(pt for ch in chars for pt in ch["quad"])
        return pymupdf.Point((r.x0 + r.x1) / 2, (r.y0 + r.y1) / 2)

    def host_cell(pno, gcell, src):
        for t in top:
            if t is src or pno not in t["pages"]:
                continue
            for c in all_cells(t):
                b = c.get("bbox") if c.get("page") == pno else None
                if b and gcell.contains(pymupdf.Point((b[0] + b[2]) / 2, (b[1] + b[3]) / 2) * ~cache.inv(pno)):
                    return t, c
        return None, None

    for src in top:
        for row in src["header_rows"] + src["rows"]:
            pages = Counter(c["page"] for c in row if c.get("page") is not None)
            if not pages:
                continue
            main = pages.most_common(1)[0][0]
            for c in row:
                if not c.get("fragments"):
                    continue
                for sp in sorted({p for p, ch in c["_recs"] if p != main}):
                    strays = [(p, ch) for p, ch in c["_recs"] if p == sp]
                    gcell = next((g for g in cache.grid_cells(sp) if g.contains(centre_mupdf(sp, [ch for _, ch in strays]))), None)
                    if gcell is None:
                        continue
                    tgt_table, tgt = host_cell(sp, gcell, src)
                    if tgt is None:
                        continue
                    moved = norm(" ".join(text_lines(strays)))
                    rebuild(cache, tgt, tgt["_recs"] + strays)
                    rebuild(cache, c, [(p, ch) for p, ch in c["_recs"] if p != sp])
                    tgt["adopted_from"] = {"struct_path": src["struct_path"], "text": moved}
                    c["moved_out"] = {"to_struct_path": tgt_table["struct_path"], "text": moved}
                    src["warnings"].append(f"moved stray fragment {moved!r} (page {sp + 1}) to table at {tgt_table['struct_path']}")
                    tgt_table["warnings"].append(f"adopted {moved!r} from table at {src['struct_path']} (tagging defect)")


def cell_centre_mupdf(cache: PageCache, c: dict) -> pymupdf.Point | None:
    b = c.get("bbox") or c.get("est_bbox")
    return pymupdf.Point((b[0] + b[2]) / 2, (b[1] + b[3]) / 2) * ~cache.inv(c["page"]) if b and c.get("page") is not None else None


def adopt_orphans(records: list[dict], cache: PageCache) -> None:
    """Glyphs whose struct chain MuPDF could not place (idx -1: the element's /P parent does not list
    it in its /K, so it is unreachable from /StructTreeRoot -- Core Table 189's twelve body rows hang
    from such a dangling /TR) are attributed by the drawn grid: the geometric table that contains them
    is matched to the struct table owning most cells inside it; glyphs landing in a grid cell that
    already hosts a struct cell join it, and grid rows with no struct row at all become new rows."""
    top = [r for r in records if not r["nested_in"]]

    def cells_of(t):
        return [c for row in t["header_rows"] + t["rows"] + [x["cells"] for x in t["repeated_header_rows"]] for c in row]

    def row_key(row):
        boxes = [(c["page"], c["bbox"]) for c in row if c.get("bbox")]
        return (min(p for p, _ in boxes), -max(b[3] for _, b in boxes)) if boxes else None

    for pno in sorted({p for t in top for p in t["pages"]}):
        by_path, _ = cache.chars(pno)
        orphans = [ch for p, recs in by_path.items() if -1 in p for ch in recs]
        if not any(ch["c"].strip() for ch in orphans):
            continue
        for gt in cache.grid(pno):
            inside = [ch for ch in orphans if gt["bbox"].contains(pymupdf.Point(*ch["origin"]))]
            if not any(ch["c"].strip() for ch in inside):
                continue
            counts = {id(t): sum(1 for c in cells_of(t) if c.get("page") == pno and (pt := cell_centre_mupdf(cache, c)) and gt["bbox"].contains(pt)) for t in top}
            host = max((t for t in top if counts[id(t)]), key=lambda t: counts[id(t)], default=None)
            if host is None:
                continue
            for grow in gt["rows"]:
                per_cell = [[ch for ch in inside if cell and cell.contains(pymupdf.Point(*ch["origin"]))] for cell in grow]
                if not any(ch["c"].strip() for chs in per_cell for ch in chs):
                    continue
                hosted = [(cell, next((c for c in cells_of(host) if c.get("page") == pno and (pt := cell_centre_mupdf(cache, c)) and cell.contains(pt)), None))
                          for cell in grow if cell]
                if any(c for _, c in hosted):
                    for (cell, c), chs in zip(((cell, c) for cell, c in hosted), (chs for cell, chs in zip(grow, per_cell) if cell)):
                        if c is not None and any(ch["c"].strip() for ch in chs):
                            rebuild(cache, c, c["_recs"] + [(pno, ch) for ch in chs])
                            c["adopted_orphan"] = norm(" ".join(text_lines([(pno, ch) for ch in chs])))
                            host["warnings"].append(f"adopted orphan glyphs {c['adopted_orphan']!r} on page {pno + 1} into an existing cell (dangling struct element)")
                else:
                    new_row = [record_from_chars(cache, "TD", [(pno, ch) for ch in chs], {"recovered_by_geometry": True}) for chs in per_cell]
                    key = row_key(new_row)
                    after = [k for k, r in enumerate(host["rows"]) if row_key(r) and row_key(r) > key]
                    host["rows"].insert(after[0] if after else len(host["rows"]), new_row)
                    host["warnings"].append(f"row recovered from the drawn grid on page {pno + 1}: {[c['text'][:20] for c in new_row]} (dangling struct element)")
            fill_empty_cells(host["header_rows"] + host["rows"], cache)
            host["n_rows"], host["n_cols"] = len(host["rows"]), max((len(r) for r in host["header_rows"] + host["rows"]), default=0)


def fill_empty_cells(rows: list[list[dict]], cache: PageCache) -> None:
    """Empty cells have no glyphs to locate them: estimate the cell rectangle as the intersection
    of its column's x-extent and its row's y-extent (same page), then look up the shading there."""
    def extents(cells):
        return [(c["page"], c["bbox"]) for c in cells if c.get("bbox")]
    ncols = max((len(r) for r in rows), default=0)
    col_boxes = [[(p, b) for r in rows for j, c in enumerate(r) if j == col for p, b in extents([c])] for col in range(ncols)]
    for row in rows:
        row_boxes = extents(row)
        row_pages = {p for p, _ in row_boxes}
        for j, c in enumerate(row):
            if c.get("bbox") or len(row_pages) != 1:
                continue
            (pno,) = row_pages
            ys = [b for p, b in row_boxes if p == pno]
            xs = [b for p, b in col_boxes[j] if p == pno] if j < ncols else []
            if not ys or not xs:
                continue
            y0, y1 = min(b[1] for b in ys), max(b[3] for b in ys)
            x0, x1 = min(b[0] for b in xs), max(b[2] for b in xs)
            # back to MuPDF space for the drawing lookup
            inv = cache.inv(pno)
            centre_pdf = pymupdf.Point((x0 + x1) / 2, (y0 + y1) / 2)
            centre = centre_pdf * ~inv
            f = fill_at(centre, cache.fills(pno)[1])
            c.update({"page": pno, "est_bbox": [round(x0, 2), round(y0, 2), round(x1, 2), round(y1, 2)],
                      "fill_rgb": list(f["rgb"]) if f else None,
                      "fill_bbox": to_pdf_space(f["rect"], inv) if f else None})


# ----------------------------------------------------------------------------- ragged-row join (design 5.1 step 5)
# A cell that continues past a page break (or a paragraph break Word turned into a row) comes out
# as a short extra row holding only the continued columns: Opal T17 row 7 'Initial Assumption: 1
# Minimum' belongs to MaxSubpackets' third cell, Core T26 'these are "non-reserved" ComIDs.' to the
# Description above it.  Such a row is appended to its predecessor.  The merged byte-layout rows
# ('3 | Length' under a nine-column Bit header, Opal T2-T8, Core T38-T44) are short too, but they
# are full-width rows: they start in the first column, and are never joined.

def cell_box(c: dict) -> list[float] | None:
    return c.get("bbox") or c.get("est_bbox")


def column_bands(header: list[dict]) -> list[tuple[float, float]] | None:
    """Column x-bands from the header cells' left edges (2 pt tolerance, as split_merged_cells):
    band j runs from column j's left edge to column j+1's; None when the header has no usable boxes."""
    lefts = [b[0] for c in header for b in [cell_box(c)] if b]
    if len(lefts) != len(header) or lefts != sorted(lefts):
        return None
    return [(l - 2.0, r - 2.0) for l, r in zip(lefts, lefts[1:] + [float("inf")])]


def x_overlap(a: list[float], b: list[float]) -> float:
    return min(a[2], b[2]) - max(a[0], b[0])


def ragged_join_targets(header: list[dict], prev: list[dict] | None, row: list[dict]) -> list[int] | None:
    """The predecessor cell index each cell of `row` continues, or None when `row` is not a
    continuation: it is the first data row, has as many cells as the header, has an empty cell, lacks
    geometry, or is full-width (starts in the first column).  Each cell goes to the predecessor cell
    whose x-extent it overlaps most."""
    bands = column_bands(header) if header else None
    boxes, pboxes = [cell_box(c) for c in row], [cell_box(c) for c in (prev or [])]
    if (bands is None or prev is None or not row or len(row) >= len(header)
            or not all(c["text"] for c in row) or not all(boxes) or not any(pboxes)):
        return None
    if len(bands) < 2 or min(b[0] for b in boxes) < bands[1][0]:
        return None                                      # full-width: the row begins in the first column
    candidates = [j for j, pb in enumerate(pboxes) if pb]
    targets = [max(candidates, key=lambda j: x_overlap(b, pboxes[j])) for b in boxes]
    return targets if all(x_overlap(b, pboxes[j]) > 0 for b, j in zip(boxes, targets)) else None


def ragged_warning(rows: list[list[dict]]) -> list[str]:
    return [f"ragged rows: widths {sorted(Counter(len(r) for r in rows).items())}"] if len({len(r) for r in rows}) > 1 else []


def join_ragged_rows(records: list[dict], cache: PageCache) -> None:
    """Fold every continuation row into the row above it (flag `ragged_join` on the cells that grew)."""
    for r in records:
        header = r["header_rows"][0] if r["header_rows"] else []
        kept, joined = [], []
        for row in r["rows"]:
            targets = ragged_join_targets(header, kept[-1] if kept else None, row)
            if targets is None:
                kept.append(row)
                continue
            for j, c in zip(targets, row):
                tgt = kept[-1][j]
                rebuild(cache, tgt, tgt["_recs"] + c["_recs"])
                tgt["ragged_join"] = True
            joined.append(f"ragged row {[c['text'][:30] for c in row]} joined into the row above (continuation)")
        if joined:
            r["rows"], r["n_rows"] = kept, len(kept)
            r["warnings"] = [w for w in r["warnings"] if not w.startswith("ragged rows:")] + ragged_warning(r["header_rows"] + kept) + joined


# ----------------------------------------------------------------------------- captions and headers
def element_text(cache: PageCache, path: tuple, pages: Iterable[int]) -> str:
    return norm(" ".join(text_lines(chars_under(cache, path, pages, stop_tags=()))))


def find_caption(cache: PageCache, t: dict, pages: set[int], page_of) -> tuple[int | None, str | None, list | None]:
    """Caption = nearest sibling (3 before, 1 after) whose text starts 'Table N'.  Searched on the
    table's pages plus the page before (captions precede the table, maybe on the previous page)."""
    sibs, i = t["siblings"], t["path"][-1]
    search_pages = pages | {min(pages) - 1} - {-1} if pages else set()
    candidates = [j for j in (i - 1, i - 2, i - 3, i + 1) if 0 <= j < len(sibs) and is_elem(sibs[j]) and tag(sibs[j]) != "/Table"]

    def units(j):
        """The sibling itself, or -- for a Word list (/L) -- its /LI items last-first, since a
        caption styled as a list item is merged into a preceding list (Core Table 239)."""
        e, base = sibs[j], tuple(t["path"][:-1] + [j])
        items = [(base + (k,), li) for k, li in enumerate(kids(e)) if tag(li) == "/LI"] if tag(e) == "/L" else []
        return list(reversed(items)) + [(base, e)]

    for cpath, e in (u for j in candidates for u in units(j)):
        cpages = leaf_pages(e, page_of) or search_pages
        txt = element_text(cache, cpath, cpages)
        m = CAPTION_RE.match(txt)
        if m:
            return int(m.group(1)), txt, list(cpath)
    return None, None, None


def row_texts(row: list[dict]) -> list[str]:
    return [c["text"] for c in row]


def split_header(rows: list[list[dict]], row_pages: list[int | None]) -> tuple[list, list, list, str]:
    """Return (header_rows, data_rows, repeated_header_rows, header_source); the cascade of design
    section 5.1 step 3, `header_source` named as in docs/ir-v1.md."""
    all_th = [bool(r) and all(c["kind"] == "TH" for c in r) for r in rows]
    n_th = len(list(takewhile(bool, all_th)))
    texts = [row_texts(r) for r in rows]
    first_repeats = [k for k in range(1, len(rows)) if texts[k] == texts[0] and row_pages[k] is not None
                     and row_pages[k - 1] is not None and row_pages[k] != row_pages[k - 1]]
    inked = [c for c in rows[0] if c["text"]] if rows else []
    if n_th == len(rows) and len(rows) - len(first_repeats) >= 3:
        n_hdr, source = 1, "first-of-all-th"            # Word tagged every row /TH (Core T233, T243, T244, T245);
        #                                                 a two-row all-/TH table is a two-level header (Core T7 part 1)
    elif n_th:
        n_hdr, source = n_th, "leading-th"
    elif len(rows) >= 2 and first_repeats:
        n_hdr, source = 1, "repeat"                      # Opal: header row re-stated on every page
    elif len(rows) >= 2 and len(inked) >= 2 and sum(bool(c.get("bold")) for c in inked) * 2 >= len(inked):
        n_hdr, source = 1, "bold-first-row"             # Opal: Arial Narrow Bold header cells, no /TH
    else:
        n_hdr, source = 0, "none"
    first = texts[0] if n_hdr else None

    def is_repeat(k):
        """Row k re-states the first header row and starts on a new page -> Word's repeated header."""
        return (n_hdr and k > 0 and row_texts(rows[k]) == first and row_pages[k] is not None
                and row_pages[k - 1] is not None and row_pages[k] != row_pages[k - 1])

    repeated = [{"row_index": k, "page": row_pages[k], "cells": rows[k]} for k in range(len(rows)) if is_repeat(k)]
    rep_idx = {d["row_index"] for d in repeated}
    header = [r for k, r in enumerate(rows[:n_hdr]) if k not in rep_idx]
    data = [r for k, r in enumerate(rows) if k >= n_hdr and k not in rep_idx]
    return header, data, repeated, source


# ----------------------------------------------------------------------------- main assembly
def table_record(cache: PageCache, t: dict, page_of: dict, page_offset: int, nested_by_parent: dict) -> dict:
    telem, path = t["elem"], tuple(t["path"])
    pages = leaf_pages(telem, page_of)
    rows_raw = table_rows(telem)
    rows, dropped_artifacts = [], 0
    for rsuffix, r in rows_raw:
        cells = []
        for ci, c in enumerate(kids(r)):
            if not is_elem(c):
                continue
            cpath = path + tuple(rsuffix) + (ci,)
            if tag(c) == "/Artifact":
                dropped_artifacts += 1
                continue
            cells.append(cell_record(cache, c, cpath, leaf_pages(c, page_of), nested_by_parent.get(cpath, [])))
        rows.append(cells)
    rows = split_merged_cells(rows, cache)
    row_pages = [min((c["page"] for c in cells if c["page"] is not None), default=None) for cells in rows]
    fill_empty_cells(rows, cache)
    number, caption, cpath = (None, None, None) if t["nested_in"] else find_caption(cache, t, pages, page_of)
    header, data, repeated, source = split_header(rows, row_pages)
    ncols = max((len(r) for r in rows), default=0)
    warnings = ragged_warning(rows) \
        + ([f"{dropped_artifacts} /Artifact cells dropped"] if dropped_artifacts else []) \
        + (["no text in any cell"] if not any(c["text"] for r in rows for c in r) else []) \
        + ([f"cells split across pages: {sum(1 for r in rows for c in r if c.get('fragments'))}"] if any(c.get("fragments") for r in rows for c in r) else []) \
        + ([f"mis-tagged merged cells split by geometry: {sum(1 for r in rows for c in r if c.get('split_from_merged'))}"] if any(c.get("split_from_merged") for r in rows for c in r) else [])
    return {"table_number": number, "caption": caption, "caption_path": cpath,
            "struct_path": path_str(path), "nested_in": t["nested_in"],
            "pages": sorted(pages), "printed_pages": [p - page_offset for p in sorted(pages)],
            "n_rows": len(data), "n_cols": ncols, "header_source": source,
            "header_rows": header, "repeated_header_rows": repeated, "rows": data, "warnings": warnings}


def mark_parts(records: list[dict]) -> list[dict]:
    """Several struct tables sharing one caption number (continuation tables with repeated captions)
    get part = 1, 2, ... in document order."""
    counts = Counter(r["table_number"] for r in records if r["table_number"] is not None)
    seen = Counter()
    for r in records:
        n = r["table_number"]
        if n is not None and counts[n] > 1:
            seen[n] += 1
            r["part"] = seen[n]
            r["parts"] = counts[n]
    return records


def mark_ids(records: list[dict]) -> list[dict]:
    """IR v1 `id` (T<n> for a numbered table, both parts of a split one) and `ordinal` (1-based among
    top-level /Table structs in document order; nested tables have none)."""
    top = [r for r in records if not r["nested_in"]]
    for k, r in enumerate(top, 1):
        r["ordinal"] = k
    for r in records:
        r.setdefault("ordinal", None)
        r["id"] = f"T{r['table_number']}" if r["table_number"] is not None else None
    return records


def table_cells(t: dict) -> list[dict]:
    return [c for row in t["header_rows"] + t["rows"] + [x["cells"] for x in t["repeated_header_rows"]] for c in row]


def table_rects(t: dict) -> list[list]:
    """[[page, [x0, y0, x1, y1]], ...]: per page, the union of every box attributed to a cell there
    (glyph bbox, estimated box of an empty cell, drawn cell rect, per-page fragments), PDF user space."""
    boxes = [(c["page"], b) for c in table_cells(t) if c.get("page") is not None
             for b in (c.get("bbox"), c.get("est_bbox"), c.get("fill_bbox")) if b] \
        + [(f["page"], f["bbox"]) for c in table_cells(t) for f in c.get("fragments", []) if f.get("bbox")]
    return [[p, [round(f(b[i] for q, b in boxes if q == p), 2) for i, f in ((0, min), (1, min), (2, max), (3, max))]]
            for p in sorted({p for p, _ in boxes})]


# ----------------------------------------------------------------------------- prose: headings, paragraphs, list items
SECTION_RE = re.compile(r"^(\d+(?:\.\d+)*)\s+(.*\S)\s*$")
TOC_LINE_RE = re.compile(r"\.{3,}\s*\d*\s*$")                 # dot leaders then a page number: a contents line
LABEL_RE = re.compile(r"^(?:(?P<num>\d{1,3})[.)]?|\[(?P<ref>\d{1,3})\]\.?|\(?(?P<alpha>[a-z]{1,4})[.)])$")
BULLET = "•"
BULLETS = {BULLET, "o", "", "", "-", "–", "▪", "◦", "·", "*"}


def parse_heading(text: str) -> tuple[str | None, str]:
    """('5.1.3', 'Column Types') from a heading's text; (None, text) for an unnumbered heading."""
    m = SECTION_RE.match(text)
    return (m.group(1), m.group(2)) if m else (None, text)


def is_toc_line(text: str) -> bool:
    """Word styles the top-level contents lines as /H1 ('1 INTRODUCTION ..... 1'); they are not headings."""
    return TOC_LINE_RE.search(text) is not None


def parse_label(text: str) -> tuple[str | None, str]:
    """(label, body) of a list item.  PDFMaker writes the label as the leading text of the /LBody
    (no /Lbl element in these PDFs): '3.' / '3)' / '3' -> '3', 'a.' / '(a)' -> 'a', 'iii.' -> 'iii',
    '[2]' -> '[2]', a bullet glyph -> BULLET; anything else (a caption tagged as a list item, a
    continuation fragment) has no label and the whole text is the body."""
    head, _, rest = text.partition(" ")
    if head in BULLETS:
        return BULLET, rest
    m = LABEL_RE.match(head)
    if not m:
        return None, text
    return m.group("num") or m.group("alpha") or f"[{m.group('ref')}]", rest


def item_label(labels: Iterable[str | None]) -> str | None:
    """Labels of the /LI chain outermost first, concatenated: '3' then 'a' is '3a'; None when none is known."""
    known = [l for l in labels if l is not None]
    return "".join(known) if known else None


def norm_words(text: str) -> list[str]:
    """Lower-cased words with punctuation and symbols stripped (Unicode categories P*, S*; C* too for
    soft hyphens and private-use bullets), split on whitespace.  The underscore stays: it is an
    identifier character in these specs (C_PIN, AC_element, K_AES_128), not punctuation."""
    return [w for w in ("".join(ch for ch in tok if ch == "_" or unicodedata.category(ch)[0] not in "PSC")
                        for tok in text.lower().split()) if w]


def words_hash(words: Iterable[str]) -> str:
    return hashlib.sha256(" ".join(words).encode()).hexdigest()


def glyph_rect(cache: PageCache, recs: list[tuple[int, dict]]) -> tuple[int | None, list[float] | None]:
    """(first page, union of the ink glyphs on it in PDF user space) of some (page, char) records."""
    ink = [(p, ch) for p, ch in recs if ch["c"].strip()]
    if not ink:
        return None, None
    page = min(p for p, _ in ink)
    return page, to_pdf_space(union_rect(pt for p, ch in ink if p == page for pt in ch["quad"]), cache.inv(page))


def path_str(path: Iterable[int]) -> str:
    return "/".join(str(i) for i in path)


def prose_records(cache: PageCache, root, page_of: dict) -> tuple[list[dict], list[dict], dict]:
    """(headings, paras, counts) in document order: HeadingIR from every /H1../H6 (contents lines
    skipped), ParaIR from every /P and /LI outside tables that has ink (empty spacer paragraphs
    skipped).  `section` is the nearest preceding numbered heading; an /LI's `item` concatenates the
    labels of its /LI ancestors and its own; its words exclude nested lists."""
    headings, paras, counts = [], [], Counter()
    state = {"section": None}

    def recs_of(path, elem, stop):
        pages = leaf_pages(elem, page_of)
        return chars_under(cache, path, pages, stop_tags=stop) if pages else []

    def walk(node, path, labels, in_table):
        n_li = 0
        for i, k in enumerate(kids(node)):
            if not is_elem(k):
                continue
            p, t = path + (i,), tag(k)
            own = None
            if t in HEADING_TAGS:
                recs = recs_of(p, k, ())
                text = norm(" ".join(text_lines(recs)))
                page, rect = glyph_rect(cache, recs)
                if is_toc_line(text):
                    counts["toc_lines_skipped"] += 1
                else:
                    section, title = parse_heading(text)
                    headings.append({"section": section, "title": title, "level": int(t[2]), "page": page,
                                     "struct_path": path_str(p), "rect": rect})
                    state["section"] = section or state["section"]
            elif t in PARA_TAGS and not in_table:
                n_li += t == "/LI"
                recs = recs_of(p, k, ("Table", "L"))
                text = norm(" ".join(text_lines(recs)))
                page, rect = glyph_rect(cache, recs)
                label, body = parse_label(text) if t == "/LI" else (None, text)
                own = f"{BULLET}{n_li}" if label == BULLET else label
                if page is None:
                    counts["empty_paras_skipped"] += 1
                else:
                    paras.append({"section": state["section"], "item": item_label(labels + [own]) if t == "/LI" else None,
                                  "kind": t[1:], "page": page, "struct_path": path_str(p), "rect": rect,
                                  "hash": words_hash(norm_words(body)), "words": None})
            walk(k, p, labels + [own] if t == "/LI" else labels, in_table or t == "/Table")

    walk(root, (), [], False)
    return headings, paras, dict(counts)


# ----------------------------------------------------------------------------- hsed annotations in the input
def pdf_text(v) -> str | None:
    return None if v is None else str(v)


def pdf_name(v) -> str | None:
    s = pdf_text(v)
    return s[1:] if s and s.startswith("/") else s


def annot_records(pdf) -> list[dict]:
    """AnnotIR for every /Text annotation whose dictionary carries /HSED (docs/ir-v1.md): the anchor
    fields from /HSED (/Kind /Spec /Id /Struct /V), the page, /Rect, /F and the /Contents payload."""
    def rec(page, a):
        h = a.get("/HSED")
        return {"kind": pdf_name(h.get("/Kind")), "spec": pdf_text(h.get("/Spec")), "id": pdf_text(h.get("/Id")),
                "struct": pdf_text(h.get("/Struct")), "v": int(h.get("/V", 0)), "page": page,
                "rect": [round(float(x), 2) for x in a.get("/Rect", [])], "flags": int(a.get("/F", 0)),
                "contents": pdf_text(a.get("/Contents"))}

    return [rec(i, a) for i, pg in enumerate(pdf.pages) for a in (pg.obj.get("/Annots") or [])
            if isinstance(a, Dictionary) and str(a.get("/Subtype", "")) == "/Text" and isinstance(a.get("/HSED"), Dictionary)]


# ----------------------------------------------------------------------------- invariants (design 5.1 step 9)
def table_by_id(ir: dict, tid: str) -> dict | None:
    return next((t for t in ir["tables"] if t["id"] == tid and not t.get("part")), None)


def invariants(ir: dict, against: dict | None) -> list[tuple[str, bool | None, str]]:
    """[(name, ok, detail)] for the spec in `ir`; ok None means skipped (needs --check-against)."""
    tables, headings = ir["tables"], ir["headings"]
    numbered = {t["table_number"] for t in tables if t["table_number"] is not None}

    def rows_check(tid, want):
        t = table_by_id(ir, tid)
        got = t["n_rows"] if t else None
        return (f"{tid} has {want} data rows", got == want, f"got {got}")

    def cells_check(tid, want):
        t = table_by_id(ir, tid)
        got = sorted({len(r) for r in t["rows"]}) if t else None
        return (f"{tid} has {want} cells per data row", got == [want], f"got widths {got}")

    def header_vs_column(tid, other_id, col):
        t, o = table_by_id(ir, tid), table_by_id(against, other_id) if against else None
        if o is None:
            return (f"{tid} header equals {other_id} column {col}", None, "needs --check-against with that table")
        mine = [norm(c["text"]) for c in t["header_rows"][0]] if t and t["header_rows"] else None
        theirs = [norm(r[col]["text"]) for r in o["rows"] if len(r) > col]
        return (f"{tid} header equals {other_id} column {col}", mine == theirs, f"mine {mine} theirs {theirs}")

    core = [
        ("heading 5.1.3 on page index 99, printed label 83",
         any(h["section"] == "5.1.3" and h["page"] == 99 and h["page"] + 1 - ir["labelOffset"] == 83 for h in headings),
         f"got {[(h['page'], h['page'] + 1 - ir['labelOffset']) for h in headings if h['section'] == '5.1.3']}"),
        ("248 distinct numbered captions", len(numbered) == 248, f"got {len(numbered)}"),
        ("Table 185 present", 185 in numbered, f"{'present' if 185 in numbered else 'absent'}"),
        ("T46 ordinal 48", (table_by_id(ir, "T46") or {}).get("ordinal") == 48, f"got {(table_by_id(ir, 'T46') or {}).get('ordinal')}"),
        cells_check("T71", 3), cells_check("T81", 3),
        rows_check("T189", 14), rows_check("T233", 14), rows_check("T243", 17), rows_check("T244", 4), rows_check("T245", 5),
    ]
    opal = [
        ("50 distinct numbered captions", len(numbered) == 50, f"got {len(numbered)}"),
        header_vs_column("T24", "T178", 1),
        rows_check("T17", 11), rows_check("T38", 96), rows_check("T22", 45),
    ]
    return {"core-2.01": core, "opal-2.30": opal}.get(ir["spec"], [])


def index_line(r: dict) -> str:
    num = f"T{r['table_number']:03d}" if r["table_number"] is not None else "T---"
    part = f".{r['part']}/{r['parts']}" if r.get("part") else ""
    pg = f"pdf {min(r['pages'])+1}-{max(r['pages'])+1}" if r["pages"] else "pdf ?"
    pp = f"(p{min(r['printed_pages'])}-{max(r['printed_pages'])})" if r["printed_pages"] else ""
    hdr = f"{len(r['header_rows'])}h+" if r["header_rows"] else ""
    shape = f"{hdr}{r['n_rows']}x{r['n_cols']}"
    nest = " NESTED" if r["nested_in"] else ""
    cap = r["caption"] or ("(unnumbered: " + " | ".join(c["text"] for c in (r["header_rows"] or r["rows"] or [[]])[0])[:70] + ")")
    return f"{num}{part:<5} {pg:<12} {pp:<10} {shape:<9}{nest} {cap}"


def source_md5(pdf_path: str, pristine: str | None) -> tuple[str, str]:
    """(source basename, md5): of the pristine PDF when given -- the input must then start with its
    bytes (an annotated file is pristine + incremental tail) -- else of the whole input."""
    data = open(pdf_path, "rb").read()
    if pristine is None:
        return os.path.basename(pdf_path), hashlib.md5(data).hexdigest()
    head = open(pristine, "rb").read()
    if not data.startswith(head):
        sys.exit(f"{pdf_path} does not start with the bytes of {pristine}")
    return os.path.basename(pristine), hashlib.md5(head).hexdigest()


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("pdf")
    ap.add_argument("out_json")
    ap.add_argument("out_index")
    ap.add_argument("--spec", required=True, help="SpecId of the document, e.g. core-2.01 or opal-2.30")
    ap.add_argument("--page-offset", type=int, default=0, help="printed page = PDF page index + 1 - offset ... i.e. PDF(1-based) = printed + offset")
    ap.add_argument("--pristine", help="the original PDF; the input must start with its bytes and pdfMd5 is its md5")
    ap.add_argument("--check-against", help="another spec's IR JSON for the cross-document invariants (Opal T24 vs Core T178)")
    ap.add_argument("--pages", help="restrict to tables touching these 1-based PDF pages, e.g. 100-101 (debugging; invariants skipped)")
    args = ap.parse_args(argv)

    t0 = time.time()
    source, md5 = source_md5(args.pdf, args.pristine)
    pdf = pikepdf.open(args.pdf)
    doc = pymupdf.open(args.pdf)
    page_of = {pg.obj.objgen: i for i, pg in enumerate(pdf.pages)}
    cache = PageCache(doc)
    tables = find_tables(pdf.Root.StructTreeRoot, page_of)
    if args.pages:
        a, b = (int(x) for x in args.pages.split("-")) if "-" in args.pages else (int(args.pages),) * 2
        want = set(range(a - 1, b))
        tables = [t for t in tables if leaf_pages(t["elem"], page_of) & want]
    nested_by_parent = defaultdict(list)
    for t in tables:
        if t["nested_in"]:
            nested_by_parent[tuple(t["path"][:-1])].append(t["path"])   # parent cell path -> nested table paths
    # printed page = 1-based PDF page - offset  ->  0-based index + 1 - offset
    records = mark_parts([table_record(cache, t, page_of, args.page_offset - 1, nested_by_parent) for t in tables])
    adopt_stray_fragments(records, cache)
    adopt_orphans(records, cache)
    join_ragged_rows(records, cache)
    strip_private(mark_ids(records))
    for r in records:
        r["rects"] = table_rects(r)
    headings, paras, prose_counts = prose_records(cache, pdf.Root.StructTreeRoot, page_of)
    annots = annot_records(pdf)
    elapsed = time.time() - t0

    # everything below is deterministic (no wall-clock time, no absolute path) so that regenerating a
    # committed JSON file diffs clean
    counts = {"n_tables": len(records), "n_numbered": sum(r["table_number"] is not None for r in records),
              "distinct_numbers": len({r["table_number"] for r in records if r["table_number"] is not None}),
              "n_nested": sum(bool(r["nested_in"]) for r in records), "n_headings": len(headings),
              "n_paras": len(paras), **prose_counts}
    # the counts travel under "extractor"; n_annots stays out so that an annotated input's IR differs
    # from the pristine one's in "annots" alone
    ir = {"irVersion": IR_VERSION, "spec": args.spec, "source": source, "pdfMd5": md5, "pages": len(pdf.pages),
          "labelOffset": args.page_offset, "notice": NOTICE,
          "extractor": {"script": os.path.basename(__file__), "version": IR_VERSION,
                        "pymupdf": pymupdf.__version__, "pikepdf": pikepdf.__version__, **counts},
          "wordComplete": [], "tables": records, "headings": headings, "paras": paras, "sigs": [], "annots": annots}
    with open(args.out_json, "w") as f:
        json.dump(ir, f, indent=1, ensure_ascii=False, sort_keys=True)
    with open(args.out_index, "w") as f:
        f.write("\n".join(index_line(r) for r in records) + "\n")

    against = json.load(open(args.check_against)) if args.check_against else None
    results = [] if args.pages else invariants(ir, against)
    for name, ok, detail in results:
        print(f"{'SKIP' if ok is None else 'PASS' if ok else 'FAIL'} {args.spec}: {name} ({detail})", file=sys.stderr)
    verdict = Counter("skip" if ok is None else "pass" if ok else "fail" for _, ok, _ in results)
    print(json.dumps({"spec": args.spec, "source": source, "pdfMd5": md5, **counts, "n_annots": len(annots),
                      "invariants": dict(verdict), "seconds": round(elapsed, 1)}), file=sys.stderr)
    return 1 if verdict["fail"] else 0


if __name__ == "__main__":
    sys.exit(main())
