"""equiv_check.py: textual equivalence of the generated module against hsed's own -ddump-splices
transcripts (dev/Column_Types.txt for Tables 46 and 52, dev/TableUIDs.txt for Table 240).
Both sides are normalised to (name -> semantic value): module qualifications dropped, uid/halfUID
byte lists evaluated (decimal in the dumps, hex in the module), declaration order and comments
ignored. Usage: python -I equiv_check.py <CoreM1.hs> <Column_Types.txt> <TableUIDs.txt>
"""
from __future__ import annotations

import json
import re
import sys
from itertools import groupby

QUAL_RE = re.compile(r"\b(?:[A-Z][A-Za-z0-9_']*\.)+(?=[A-Za-z_])")     # GHC.Base.String -> String
COMMENT_RE = re.compile(r"--.*$", re.M)
SIG_RE = re.compile(r"^(\w+) :: (.+)$")
ID_RE = re.compile(r"^(\w+) = \(*(uid|halfUID)\b(.*)$")
STR_RE = re.compile(r'^(\w+) = ("(?:[^"\\]|\\.)*")$')
MAP_RE = re.compile(r"^(\w+) = fromList \[(.*)\]$")
DATA_RE = re.compile(r"^data (\w+) = (.*?) deriving \((.*)\)$")
PAIR_RE = re.compile(r'\((\w+), "([^"]*)"\)')


def splice_blocks(dump: str) -> list[tuple[str, str]]:
    """[(quoted_table_text, declarations_text)] for every 'Splicing declarations' block."""
    parts = re.split(r"^\S.*Splicing declarations\n", dump, flags=re.M)[1:]
    return [tuple(p.split("  ======>\n", 1)) for p in parts if "  ======>\n" in p]


def decls_from_dump(text: str) -> str:
    """Drop the ghci epilogue ('Ok, N modules loaded.' and the prompt)."""
    return "\n".join(l for l in text.splitlines() if not l.startswith(("Ok,", "λ>", "/Users/")))


def group_decls(text: str, indent: int) -> list[str]:
    """Join continuation lines (indented deeper than `indent`) onto their declaration."""
    lines = [l for l in COMMENT_RE.sub("", text).splitlines() if l.strip()]
    starts = [len(l) - len(l.lstrip()) <= indent for l in lines]
    groups, cur = [], []
    for l, s in zip(lines, starts):
        if s and cur:
            groups.append(cur)
            cur = []
        cur.append(l.strip())
    return [" ".join(g) for g in groups + ([cur] if cur else [])]


def normalise(decl: str) -> tuple[str, object] | None:
    d = re.sub(r"\s+", " ", QUAL_RE.sub("", decl)).strip()
    if m := DATA_RE.match(d):
        return ("data " + m.group(1), {"ctors": [c.strip() for c in m.group(2).split("|")], "deriving": sorted(x.strip() for x in m.group(3).split(","))})
    if m := ID_RE.match(d):
        return (m.group(1), [m.group(2)] + [int(x, 0) for x in re.findall(r"0x[0-9A-Fa-f]+|\d+", m.group(3))])
    if m := STR_RE.match(d):
        return (m.group(1), json.loads(m.group(2)))
    if m := MAP_RE.match(d):
        return (m.group(1), sorted(PAIR_RE.findall(m.group(2))))
    if m := SIG_RE.match(d):
        return (m.group(1) + " ::", m.group(2))
    return None


def table(decls: list[str]) -> dict:
    return dict(filter(None, map(normalise, decls)))


def main(argv) -> int:
    module, col_types, table_uids = argv[1:4]
    mine = table(group_decls(open(module).read(), 0))
    blocks = splice_blocks(open(col_types).read())
    want = ("Table 46 AC_element", "Table 52 adv_key_mode Enumeration Values")
    picked = [decls for quoted, decls in blocks if any(w in quoted for w in want)]
    t240 = decls_from_dump(open(table_uids).read().split("  ======>\n", 1)[1])
    theirs = table([d for text in picked + [t240] for d in group_decls(text, 4)])
    only_mine = sorted(set(mine) - set(theirs))
    only_theirs = sorted(set(theirs) - set(mine))
    differ = sorted(k for k in set(mine) & set(theirs) if mine[k] != theirs[k])
    print(json.dumps({"splice_blocks_in_Column_Types": len(blocks), "blocks_picked": len(picked),
                      "n_mine": len(mine), "n_theirs": len(theirs), "n_common_equal": len(set(mine) & set(theirs)) - len(differ),
                      "only_in_generated": only_mine, "only_in_hsed_splices": only_theirs,
                      "differing": {k: {"generated": mine[k], "hsed": theirs[k]} for k in differ}}, indent=1))
    return 0 if not differ and not only_theirs and not only_mine else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
