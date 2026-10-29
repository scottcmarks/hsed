"""compile.py: read the ANNOTATED Core spec, find the hsed annotations, re-anchor each one to its
table by /HSED /Path in a fresh run of the extractor over the same annotated file, check that the
table's shape fits the directive, and generate ONE Haskell module in hsed's own shapes.

Usage: python -I compile.py <annotated.pdf> <out_dir> [--extractor <extract_tables.py>]
                            [--compare-json <spec/ir/core-2.01.json>] [--discover-only]
Writes <out_dir>/Hsed/Generated/CoreM1.hs, <out_dir>/Equiv.hs, <out_dir>/annotated_tables.json,
       <out_dir>/compile_report.json
"""
from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
from collections import defaultdict
from itertools import chain

import pikepdf

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import hsed_annot as H  # noqa: E402

PYTHON = sys.executable
DEFAULT_EXTRACTOR = os.path.join(HERE, "extract_tables.py")
SPEC = "core-2.01"                     # the extractor's --spec (IR v1 `spec`)
PAGE_OFFSET = 17                       # Core 2.01: PDF page (1-based) = printed page + 17
MODULE = "Hsed.Generated.CoreM1"
HEX_UID_RE = re.compile(r"^([0-9A-Fa-f]{2})( [0-9A-Fa-f]{2}){7}$")
HEX_HALF_RE = re.compile(r"^([0-9A-Fa-f]{2})( [0-9A-Fa-f]{2}){3}$")
ID_RE = re.compile(r"[A-Za-z0-9_]+")


# ----------------------------------------------------------------------------- 1. discover
def discover(pdf_path: str) -> list[dict]:
    with pikepdf.open(pdf_path) as pdf:
        return [{"page": i, "annot": a, "problems": probs} for i, a, probs in H.find_pikepdf(pdf)]


# ----------------------------------------------------------------------------- 2. extract + locate
def run_extractor(extractor: str, pdf_path: str, out_dir: str) -> dict:
    out_json, out_idx = f"{out_dir}/annotated_tables.json", f"{out_dir}/annotated_index.txt"
    r = subprocess.run([PYTHON, "-I", extractor, pdf_path, out_json, out_idx, "--spec", SPEC, "--page-offset", str(PAGE_OFFSET)],
                       capture_output=True, text=True, check=True)
    return {"json": out_json, "index": out_idx, "meta": json.loads(r.stderr.strip().splitlines()[-1])}


def locate(a: H.HsedAnnot, tables: list[dict]) -> tuple[dict | None, list[str]]:
    hits = [t for t in tables if tuple(t["struct_path"]) == a.path]
    if len(hits) != 1:
        return None, [f"struct path {H.format_path(a.path)} matched {len(hits)} tables"]
    t = hits[0]
    problems = ([] if t["caption"] == a.caption else [f"caption differs: pdf {t['caption']!r} vs annot {a.caption!r}"]) \
        + ([] if tuple(t["pages"]) == a.pages else [f"pages differ: pdf {t['pages']} vs annot {list(a.pages)}"])
    return t, problems


# ----------------------------------------------------------------------------- 3. shape checks
def header_texts(t: dict) -> list[str]:
    return [c["text"] for c in t["header_rows"][0]] if t["header_rows"] else []


def is_range(s: str) -> bool:
    return re.fullmatch(r"\d+(-\d+)?", s.strip()) is not None


def check_shape(directive: str, t: dict) -> list[str]:
    hdr, rows = header_texts(t), t["rows"]
    if directive == H.TYPE_ROW:
        return ([] if hdr == ["UID", "Name", "Format"] else [f"typeRow needs header [UID, Name, Format], got {hdr}"]) \
            + ([] if len(rows) == 1 else [f"typeRow needs exactly 1 row, got {len(rows)}"]) \
            + [f"UID cell {r[0]['text']!r} is not 8 hex bytes" for r in rows if not HEX_UID_RE.match(r[0]["text"])]
    if directive == H.ENUM_VALUES:
        return ([] if len(hdr) == 2 and "Value" in hdr[0] else [f"enumValues needs [<x> Value, <label>] header, got {hdr}"]) \
            + ([] if rows else ["enumValues needs rows"]) \
            + [f"value cell {r[0]['text']!r} is not an int or int-int range" for r in rows if not is_range(r[0]["text"])] \
            + [f"ragged row {i}: {len(r)} cells" for i, r in enumerate(rows) if len(r) != 2]
    if directive == H.TABLE_UIDS:
        want = ["UID of Table Descriptor Object", "UID of Table", "Table Name", "Template"]
        return ([] if hdr == want else [f"tableUIDs needs header {want}, got {hdr}"]) \
            + [f"row {i}: {len(r)} cells" for i, r in enumerate(rows) if len(r) != 4] \
            + [f"row {i}: bad UID {r[k]['text']!r}" for i, r in enumerate(rows) for k in (0, 1) if not HEX_UID_RE.match(r[k]["text"])] \
            + [f"row {i} ({r[2]['text']}): lower half of object UID != upper half of table UID"
               for i, r in enumerate(rows) if hex_bytes(r[0]["text"])[4:] != hex_bytes(r[1]["text"])[:4]]
    return [f"unknown directive {directive}"]


# ----------------------------------------------------------------------------- 4. generate (hsed shapes)
def hex_bytes(s: str) -> list[int]:
    return [int(b, 16) for b in s.split()]


def hs_uid(bs: list[int]) -> str:
    return "uid " + " ".join(f"0x{b:02X}" for b in bs)


def hs_half(bs: list[int]) -> str:
    return "halfUID " + " ".join(f"0x{b:02X}" for b in bs)


def format_string(cell: dict) -> str:
    """hsed's ttype concatenates the trimmed physical lines of the Format cell: 'List_Type,*,ACE_expression'."""
    return "".join(line.strip() for line in cell["lines"])


def enum_name(caption: str) -> str:
    """hsed's enumTableTitle: 'Table <n> <idChars>...' -> idChars."""
    m = re.match(r"Table \d+ ([A-Za-z0-9_]+)", caption)
    return m.group(1)


def scrub(label: str) -> str:
    """hsed's scrub: drop commas, split on space or dash, join with '_'."""
    return "_".join(re.split(r"[ -]", label.replace(",", "")))


def parse_range(s: str) -> list[int]:
    lo, _, hi = s.strip().partition("-")
    return list(range(int(lo), int(hi) + 1)) if hi else [int(lo)]


def enum_constructors(core_name: str, rows: list[tuple[str, list[int]]]) -> list[tuple[str, int]]:
    """Port of hsed's dEnum: value-ordered constructors, _NA gap fillers, _<v> suffixes for a label's
    second and later values, so that fromEnum == spec value."""
    given = [(f"{core_name}_{scrub(label)}", vals) for label, vals in rows]
    given_values = [v for _, vs in given for v in vs]
    missing = [v for v in range(0, max(given_values) + 1) if v not in given_values]
    full = ([(f"{core_name}_NA", missing)] if missing else []) + given
    merged = defaultdict(list)                                       # fromListWith (flip (++)): insertion order
    for n, vs in full:
        merged[n].extend(vs)
    consolidated = sorted(merged.items())                            # Map.toList: key order
    gen = lambda n, vs: [(n, vs[0])] + [(f"{n}_{v}", v) for v in vs[1:]]
    return sorted(chain.from_iterable(gen(n, vs) for n, vs in consolidated), key=lambda p: p[1])


def provenance(pdf_name: str, t: dict, page0: int, extra: str = "") -> str:
    return (f"-- | {t['caption']}; {pdf_name} PDF page {page0 + 1} (printed {page0 + 1 - PAGE_OFFSET}); "
            f"struct path {H.format_path(t['struct_path'])}{extra}")


def gen_type_row(pdf_name: str, t: dict) -> list[str]:
    [[u, n, f]] = t["rows"]
    tag = f"{n['text']}Type"
    prov = provenance(pdf_name, t, u["page"], "; columns UID, Name, Format")
    return [prov, f"u{tag} :: UID", f"u{tag} = {hs_uid(hex_bytes(u['text']))}", "",
            prov, f"f{tag} :: String", f"f{tag} = {json.dumps(format_string(f))}", ""]


def gen_enum(pdf_name: str, t: dict) -> list[str]:
    core = f"Core_{enum_name(t['caption'])}"
    ctors = enum_constructors(core, [(r[1]["text"], parse_range(r[0]["text"])) for r in t["rows"]])
    pages = sorted({c["page"] for r in t["rows"] for c in r})
    prov = provenance(pdf_name, t, pages[0], f"; {len(t['rows'])} rows, values 0..{ctors[-1][1]}")
    body = [f"    {'=' if i == 0 else '|'} {name:<40} -- {v}" for i, (name, v) in enumerate(ctors)]
    return [prov, f"data {core}"] + body + ["    deriving (Bounded, Enum, Eq, Ord, Show)", ""]


def gen_table_uids(pdf_name: str, t: dict) -> list[str]:
    rows = [(hex_bytes(r[0]["text"]), hex_bytes(r[1]["text"]), r[2]["text"], r[3]["text"], r[2]["page"]) for r in t["rows"]]

    def defs(i, obj, tab, name, template, page0):
        prov = provenance(pdf_name, t, page0, f"; row {i + 1}/{len(rows)} {name} ({template})")
        return [prov, f"u{name}TableObject :: UID", f"u{name}TableObject = {hs_uid(obj)}",
                prov, f"u{name}Table :: UID", f"u{name}Table = {hs_uid(tab)}",
                prov, f"h{name} :: HalfUID", f"h{name} = {hs_half(obj[4:])}", ""]

    prov = provenance(pdf_name, t, rows[0][4], f"; all {len(rows)} rows")
    maps = [prov, "nameHalfUID :: Map HalfUID String", "nameHalfUID = fromList"] \
        + [f"    {'[' if i == 0 else ','} (h{n}, {json.dumps(n)})" for i, (_, _, n, _, _) in enumerate(rows)] + ["    ]", "",
           prov, "nameUID :: Map UID String", "nameUID = fromList"] \
        + [f"    {'[' if i == 0 and k == 0 else ','} (u{n}Table{'Object' if k else ''}, {json.dumps(n + ' Table' + (' Object' if k else ''))})"
           for i, (_, _, n, _, _) in enumerate(rows) for k in (0, 1)] + ["    ]", ""]
    return list(chain.from_iterable(defs(i, *r) for i, r in enumerate(rows))) + maps


GENERATORS = {H.TYPE_ROW: gen_type_row, H.ENUM_VALUES: gen_enum, H.TABLE_UIDS: gen_table_uids}


def module_text(pdf_path: str, pdf_md5: str, items: list[tuple[H.HsedAnnot, dict]]) -> str:
    name = os.path.basename(pdf_path)
    head = [f"-- Generated by compile.py from the hsed annotations in {name} (md5 {pdf_md5}).",
            "-- Do not edit: the PDF is the source. Each definition carries its provenance:",
            "-- caption; PDF page (1-based, printed page); /StructTreeRoot /K index path of the /Table.",
            f"module {MODULE} where", "",
            "import Data.Map (Map, fromList)",
            "import System.SED.MCTP.Common.UID (HalfUID, UID, halfUID, uid)", ""]
    sections = [[f"-- * {a.caption} (hsed: {a.directive})", ""] + GENERATORS[a.directive](name, t) for a, t in items]
    return "\n".join(head + list(chain.from_iterable(sections))) + "\n"


def equiv_text(items: list[tuple[H.HsedAnnot, dict]]) -> str:
    """A runtime check: every generated value equals what hsed's own quasiquoters produce."""
    t240 = next(t for a, t in items if a.directive == H.TABLE_UIDS)
    names = [r[2]["text"] for r in t240["rows"]]
    uid_pairs = [f'    , ("{p}{n}{s}", G.{p}{n}{s} == T.{p}{n}{s})' for n in names for p, s in (("u", "TableObject"), ("u", "Table"))]
    half_pairs = [f'    , ("h{n}", G.h{n} == T.h{n})' for n in names]
    return "\n".join([
        "module Main (main) where", "",
        "import qualified Hsed.Generated.CoreM1 as G",
        "import qualified System.SED.MCTP.Common.TypeUIDs as H",
        "import qualified System.SED.MCTP.Common.TableUIDs as T",
        "import System.Exit (exitFailure, exitSuccess)", "",
        "checks :: [(String, Bool)]",
        "checks =",
        '    [ ("uAC_elementType", G.uAC_elementType == H.uAC_elementType)',
        '    , ("fAC_elementType", G.fAC_elementType == H.fAC_elementType)',
        '    , ("Core_adv_key_mode ctor names", map show [minBound .. maxBound :: G.Core_adv_key_mode] == map show [minBound .. maxBound :: H.Core_adv_key_mode])',
        '    , ("Core_adv_key_mode fromEnum", map fromEnum [minBound .. maxBound :: G.Core_adv_key_mode] == map fromEnum [minBound .. maxBound :: H.Core_adv_key_mode])',
        '    , ("nameUID map", G.nameUID == T.nameUID)',
        '    , ("nameHalfUID map", G.nameHalfUID == T.nameHalfUID)',
    ] + uid_pairs + half_pairs + [
        "    ]", "",
        "main :: IO ()",
        "main = do",
        "    let bad = [n | (n, ok) <- checks, not ok]",
        '    putStrLn (show (length checks) ++ " checks, " ++ show (length bad) ++ " failures" ++ concatMap ("\\n  FAIL " ++) bad)',
        '    putStrLn ("uAC_elementType = " ++ show G.uAC_elementType ++ "; fAC_elementType = " ++ show G.fAC_elementType)',
        '    putStrLn ("Core_adv_key_mode = " ++ show [minBound .. maxBound :: G.Core_adv_key_mode])',
        "    if null bad then exitSuccess else exitFailure", ""])


# ----------------------------------------------------------------------------- 5. compare extractor runs
def compare_json(a_path: str, b_path: str) -> dict:
    a, b = json.load(open(a_path))["tables"], json.load(open(b_path))["tables"]
    diffs = [i for i, (x, y) in enumerate(zip(a, b)) if x != y]
    return {"n_tables": [len(a), len(b)], "equal": len(a) == len(b) and not diffs, "first_differing_indices": diffs[:10]}


# ----------------------------------------------------------------------------- main
def main(argv=None) -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("pdf")
    ap.add_argument("out_dir")
    ap.add_argument("--extractor", default=DEFAULT_EXTRACTOR)
    ap.add_argument("--compare-json")
    ap.add_argument("--discover-only", action="store_true")
    args = ap.parse_args(argv)
    os.makedirs(f"{args.out_dir}/Hsed/Generated", exist_ok=True)
    import hashlib
    pdf_md5 = hashlib.md5(open(args.pdf, "rb").read()).hexdigest()

    found = discover(args.pdf)
    report = {"pdf": args.pdf, "pdf_md5": pdf_md5,
              "discovered": [{"page": f["page"], "directive": f["annot"].directive, "path": H.format_path(f["annot"].path),
                              "caption": f["annot"].caption, "pages": list(f["annot"].pages), "rect": list(f["annot"].rect),
                              "problems": f["problems"]} for f in found]}
    if args.discover_only:
        print(json.dumps(report, indent=1))
        return 0 if len(found) == 3 and not any(f["problems"] for f in found) else 1

    ext = run_extractor(args.extractor, args.pdf, args.out_dir)
    tables = json.load(open(ext["json"]))["tables"]
    report["extractor"] = ext["meta"]
    if args.compare_json:
        report["extractor_output_vs_original_run"] = compare_json(ext["json"], args.compare_json)

    located = [(f, *locate(f["annot"], tables)) for f in found]
    report["located"] = [{"directive": f["annot"].directive, "path": H.format_path(f["annot"].path), "found": t is not None,
                          "caption": t and t["caption"], "pages": t and t["pages"], "header": t and header_texts(t),
                          "n_rows": t and len(t["rows"]), "problems": probs + (check_shape(f["annot"].directive, t) if t else [])}
                         for f, t, probs in located]
    ok = [(f["annot"], t) for (f, t, _), rep in zip(located, report["located"]) if t is not None and not rep["problems"]]
    report["n_compiled"] = len(ok)
    if len(ok) == len(found) == 3:
        hs = module_text(args.pdf, pdf_md5, ok)
        open(f"{args.out_dir}/Hsed/Generated/CoreM1.hs", "w").write(hs)
        open(f"{args.out_dir}/Equiv.hs", "w").write(equiv_text(ok))
        report["module"] = {"path": f"{args.out_dir}/Hsed/Generated/CoreM1.hs", "lines": hs.count("\n"),
                            "definitions": len(re.findall(r"^[a-z]\w* ::", hs, re.M)), "data_decls": len(re.findall(r"^data ", hs, re.M))}
    json.dump(report, open(f"{args.out_dir}/compile_report.json", "w"), indent=1, default=str)
    print(json.dumps({k: v for k, v in report.items() if k != "discovered"}, indent=1, default=str))
    return 0 if len(ok) == 3 else 1


if __name__ == "__main__":
    sys.exit(main())
