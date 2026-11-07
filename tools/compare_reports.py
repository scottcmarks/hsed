"""tools/compare_reports.py: two hsed-jig runs compared by their reports, the JSON, the Markdown and the JUnit XML,
after normalising what any two runs differ in (the hsed version, the start and the report directory); every other
difference is reported unless an allowance named on the command line allows it.

  .venv/bin/python -I tools/compare_reports.py [--allow-keys KEY] [--allow-texts OLD NEW]
      [--allow-md-sections TITLE] [--show-allowed] [--limit N] A B

A and B are each one of:
  - a run's JSON report (jig-<target>-<stamp>.json), its Markdown and JUnit reports beside it (.md, .xml), which
    may be named instead;
  - a run's report directory (hsed-jig --report-dir), holding exactly one JSON report;
  - a directory tools/jig-runs.sh wrote, whose runs.tsv names each run's JSON report: each run of either compared
    with the run of the same name in the other, a run one alone has a difference.

Normalised in each run's three reports before they are compared, each by the values its own JSON gives:
  - the hsed version, the JSON's "hsed" (Version 0.1.0.1 [branch commit]), wherever it stands (the targets' impl,
    the Markdown's header and targets table, the JUnit property), as <hsed>;
  - the start, the JSON's "started", and the forms the Markdown's header, the JUnit timestamp and the reports'
    names give it, as <started>;
  - the report directory, options.report_dir, and a path under it, as <report_dir>.

Compared: the JSON as values, an object's members by key and a list's elements aligned as difflib aligns them, so
that an element one side alone has is one difference, not a shift of every element after it (the elements of a
replaced block are paired in order, those left over one side's alone); the Markdown and the JUnit line by line,
aligned the same way. A line is the text between two LFs as the file holds it, read with no newline translation,
the last the text after the final LF (empty where the file ends in one), so that a difference in line endings (a
CR or a CRLF where the other side has an LF) or in the final newline is reported like any other, and no other
character (a form feed, a Unicode line separator) ends a line.

Allowances, each option repeatable:
  --allow-keys KEY           a JSON difference at KEY or under it: a dotted path from the report's root, list
                             positions left out (options.tests, cases.observations), or a bare key name, matched at
                             any depth (locking_ofs)
  --allow-texts OLD NEW      a text, a JSON string or a Markdown or JUnit line, that is A's with each OLD of the
                             allowances replaced by its NEW, in the order given; with OLD empty (the empty-old
                             allowance), something B alone has with NEW in its text: an object member, a list
                             element or a line that B adds, or B's value where A's is empty ("", null, [] or {}),
                             its text its strings and its keys; never something A alone has
  --allow-md-sections TITLE  a Markdown line under a heading that begins with TITLE (its #s left out; a heading
                             heads its own section, and fenced code holds none), on each side that has the line

Exit status: 0 equal under the allowances; 1 a difference none allows, or a run one set alone has; 2 a usage error,
or a report missing or not readable.

Unit tests: tools/test_compare_reports.py (make compare-test).
"""
import argparse
import difflib
import functools
import itertools
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path

KINDS = ("json", "md", "xml")


class _Absent:
    """What a side has where the other alone has a member, an element or a line (JSON's null is None)."""

    def __repr__(self):
        return "ABSENT"


ABSENT = _Absent()


class Unreadable(Exception):
    """A report missing or not readable, or an argument naming none."""


@dataclass(frozen=True)
class Allowances:
    keys: tuple = ()          # --allow-keys
    texts: tuple = ()         # --allow-texts, (old, new) pairs
    md_sections: tuple = ()   # --allow-md-sections


@dataclass(frozen=True)
class Change:
    """One difference: in which report, where (a JSON path, or the lines, A's then B's), the object keys on a JSON
    path (list positions left out), A's and B's values or lines (ABSENT on the side that lacks it); in the Markdown,
    the headings over each side's line, outermost first; and the key of an object member one side alone has."""
    kind: str
    where: str
    keys: tuple
    a: object
    b: object
    a_heads: tuple = ()
    b_heads: tuple = ()
    member: object = None


# * Normalising

STAMP = re.compile(r"(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})")


def started_forms(started):
    """The forms a run's start takes in its reports, the longest first: the JSON's own (fractions of a second and
    all), the Markdown header's (%Y-%m-%dT%H:%M:%SZ), the JUnit timestamp's (no Z) and the reports' names'
    (%Y%m%dT%H%M%SZ).

    >>> started_forms("2026-11-06T13:45:12.5Z")
    ['2026-11-06T13:45:12.5Z', '2026-11-06T13:45:12Z', '2026-11-06T13:45:12', '20261106T134512Z']
    """
    m = STAMP.match(started)
    if not m:
        return [started] if started else []
    year, month, day, hour, minute, second = m.groups()
    return unique([started, f"{year}-{month}-{day}T{hour}:{minute}:{second}Z", f"{year}-{month}-{day}T{hour}:{minute}:{second}",
                   f"{year}{month}{day}T{hour}{minute}{second}Z"])


def version_forms(version):
    """A version as the reports print it: as it is, as a JUnit attribute escapes it, and as a Markdown table cell."""
    xml = functools.reduce(lambda t, e: t.replace(*e), (("&", "&amp;"), ("<", "&lt;"), (">", "&gt;"), ('"', "&quot;"),
                                                         ("'", "&apos;")), version)
    return unique([version, xml, version.replace("|", "\\|")]) if version else []


def unique(xs):
    """The elements in order, each once."""
    return list(dict.fromkeys(xs))


def text_field(obj, key):
    """An object's member if it is a string, else the empty string."""
    value = obj.get(key) if isinstance(obj, dict) else None
    return value if isinstance(value, str) else ""


def normaliser(report):
    """What normalises a run's texts, from its JSON report: its version as <hsed>, its start as <started>, its report
    directory, the whole text or a path under it, as <report_dir> (under it only where it names a directory, a /
    in it, so that a short word is left alone)."""
    swaps = ([(form, "<hsed>") for form in version_forms(text_field(report, "hsed"))]
             + [(form, "<started>") for form in started_forms(text_field(report, "started"))])
    report_dir = text_field(report.get("options") if isinstance(report, dict) else None, "report_dir")
    root = report_dir.rstrip("/")
    under = re.compile(r"(?<![\w.-])" + re.escape(root) + r"(?![\w.-])") if "/" in report_dir and root else None

    def normalise(text):
        swapped = functools.reduce(lambda t, s: t.replace(*s), swaps, text)
        if report_dir and swapped == report_dir:
            return "<report_dir>"
        return under.sub("<report_dir>", swapped) if under else swapped
    return normalise


def normalised(value, normalise):
    """A JSON value with each string in it normalised (an object's keys left as they are)."""
    if isinstance(value, str):
        return normalise(value)
    if isinstance(value, list):
        return [normalised(v, normalise) for v in value]
    if isinstance(value, dict):
        return {k: normalised(v, normalise) for k, v in value.items()}
    return value


# * Comparing

def aligned(a, b, key, paired, a_only, b_only):
    """The changes between two sequences, aligned as difflib aligns their elements' keys: the elements of a replaced
    block paired in order (paired(i, j)), those left over one side's alone (a_only(i), b_only(j))."""
    matcher = difflib.SequenceMatcher(None, [key(x) for x in a], [key(x) for x in b], autojunk=False)

    def block(i1, i2, j1, j2):
        n = min(i2 - i1, j2 - j1)
        return ([c for k in range(n) for c in paired(i1 + k, j1 + k)]
                + [c for i in range(i1 + n, i2) for c in a_only(i)]
                + [c for j in range(j1 + n, j2) for c in b_only(j)])
    return [c for tag, i1, i2, j1, j2 in matcher.get_opcodes() if tag != "equal" for c in block(i1, i2, j1, j2)]


def canonical(value):
    """A JSON value as one text, its objects' keys sorted: equal texts, equal values."""
    return json.dumps(value, sort_keys=True, ensure_ascii=False, separators=(",", ":"))


def rendered(path):
    """A JSON path as the changes give it: options.tests, cases[2].verdict, transcript[5/6] (A's position, then B's
    where they differ)."""
    def part(p):
        if isinstance(p, str):
            return "." + p
        i, j = p
        return f"[{j if i is None else i}]" if i is None or j is None or i == j else f"[{i}/{j}]"
    return "".join(map(part, path)).lstrip(".") or "(root)"


def json_change(path, a, b):
    member = path[-1] if path and isinstance(path[-1], str) and (a is ABSENT or b is ABSENT) else None
    return Change("json", rendered(path), tuple(p for p in path if isinstance(p, str)), a, b, member=member)


def json_changes(a, b, path=()):
    """The changes between two JSON values: an object's members by key (A's order, then B's own), a list's elements
    aligned; a value of another type, an empty list or object against a full one, or a different scalar, one change."""
    if type(a) is not type(b) or (isinstance(a, (list, dict)) and (not a) != (not b)):
        return [json_change(path, a, b)]
    if isinstance(a, dict):
        return [c for k in list(a) + [k for k in b if k not in a] for c in member_changes(a, b, k, path)]
    if isinstance(a, list):
        return aligned(a, b, canonical, lambda i, j: json_changes(a[i], b[j], path + ((i, j),)),
                       lambda i: [json_change(path + ((i, None),), a[i], ABSENT)],
                       lambda j: [json_change(path + ((None, j),), ABSENT, b[j])])
    return [] if a == b else [json_change(path, a, b)]


def member_changes(a, b, key, path):
    if key not in b:
        return [json_change(path + (key,), a[key], ABSENT)]
    if key not in a:
        return [json_change(path + (key,), ABSENT, b[key])]
    return json_changes(a[key], b[key], path + (key,))


HEADING = re.compile(r"(#{1,6})\s+(.*\S)\s*$")


def headings(lines):
    """Each Markdown line's headings, outermost first, a heading line's own among them; a line in fenced code (```)
    is never a heading."""
    def step(state, line):
        heads, fenced = state
        if line.startswith("```"):
            return heads, not fenced
        m = None if fenced else HEADING.match(line)
        if not m:
            return heads, fenced
        level = len(m.group(1))
        return tuple(h for h in heads if h[0] < level) + ((level, m.group(2)),), fenced
    states = itertools.islice(itertools.accumulate(lines, step, initial=((), False)), 1, None)
    return [tuple(text for _, text in heads) for heads, _ in states]


def line_changes(kind, a, b, a_heads=None, b_heads=None):
    """The changes between two reports' lines, aligned; the Markdown's with each side's headings."""
    a_heads = a_heads if a_heads is not None else [()] * len(a)
    b_heads = b_heads if b_heads is not None else [()] * len(b)

    def change(i, j):
        heads = (a_heads[i] if i is not None else ()) or (b_heads[j] if j is not None else ())
        numbers = unique(str(n + 1) for n in (i, j) if n is not None)
        where = f"line {'/'.join(numbers)}" + (f" ({heads[-1]})" if heads else "")
        return Change(kind, where, (), a[i] if i is not None else ABSENT, b[j] if j is not None else ABSENT,
                      a_heads[i] if i is not None else (), b_heads[j] if j is not None else ())
    return aligned(a, b, lambda line: line, lambda i, j: [] if a[i] == b[j] else [change(i, j)],
                   lambda i: [change(i, None)], lambda j: [change(None, j)])


# * Allowing

def key_allows(allowance, keys):
    """Whether --allow-keys ALLOWANCE allows a JSON change whose path has these object keys: the allowance a dotted
    path its keys begin with, or a bare name one of them is.

    >>> key_allows("options.tests", ("options", "tests")), key_allows("locking_ofs", ("options", "locking_ofs"))
    (True, True)
    >>> key_allows("options.test", ("options", "tests")), key_allows("cases.verdict", ("verdict",))
    (False, False)
    """
    parts = tuple(allowance.split("."))
    return keys[:len(parts)] == parts or (len(parts) == 1 and parts[0] in keys)


def is_empty(value):
    """Whether a JSON value is empty: "", null, [] or {}."""
    return value is None or (isinstance(value, (str, list, dict)) and not value)


def texts_of(value):
    """The texts in a JSON value, which an empty-old allowance searches: a string itself, an object's keys and the
    texts of its members, a list's elements' texts, any other value as JSON writes it."""
    if isinstance(value, str):
        return [value]
    if isinstance(value, dict):
        return [t for k, v in value.items() for t in [k, *texts_of(v)]]
    if isinstance(value, list):
        return [t for v in value for t in texts_of(v)]
    return [json.dumps(value)]


def text_allows(a, b, texts, member=None):
    """The --allow-texts allowance that allows a change from A's text to B's, if one does: B's text A's with each
    non-empty OLD replaced by its NEW; or, where B alone has something (A's side absent or empty), an empty OLD's
    NEW in B's texts, a member's key among them."""
    swaps = [(old, new) for old, new in texts if old]
    if isinstance(a, str) and isinstance(b, str) and swaps and functools.reduce(lambda t, s: t.replace(*s), swaps, a) == b:
        return "--allow-texts"
    if b is ABSENT or not (a is ABSENT or is_empty(a)):
        return None
    searched = texts_of(b) + ([member] if member else [])
    return next((f"--allow-texts '' {json.dumps(new, ensure_ascii=False)}"
                 for old, new in texts if not old and any(new in t for t in searched)), None)


def section_allows(change, title):
    """Whether --allow-md-sections TITLE allows a Markdown change: on each side that has the line, a heading over it
    begins with TITLE."""
    sides = [heads for heads, line in ((change.a_heads, change.a), (change.b_heads, change.b)) if line is not ABSENT]
    return all(any(h.startswith(title) for h in heads) for heads in sides)


def allowance(change, allow):
    """The allowance that allows a change, named as on the command line, or None."""
    by_key = (f"--allow-keys {k}" for k in allow.keys if change.kind == "json" and key_allows(k, change.keys))
    by_section = (f"--allow-md-sections {json.dumps(t, ensure_ascii=False)}"
                  for t in allow.md_sections if change.kind == "md" and section_allows(change, t))
    return next(itertools.chain(by_key, by_section), None) or text_allows(change.a, change.b, allow.texts, change.member)


# * Reading

def report_paths(json_path):
    """A run's three reports by its JSON report's name (its stem may hold dots: a replay file's base name)."""
    base = str(json_path)[:-len(".json")]
    return {kind: Path(f"{base}.{kind}") for kind in KINDS}


def lines_of(text):
    """A report's lines: the text between two LFs, the last the text after the final LF, empty where the text ends
    in one; a CR stays in its line, and nothing but an LF ends one (not str.splitlines(), which also ends a line at a
    CR, a form feed or a Unicode line separator and drops the final newline).

    >>> lines_of("a\\nb\\n"), lines_of("a\\nb"), lines_of("a\\r\\nb\\fc\\n")
    (['a', 'b', ''], ['a', 'b'], ['a\\r', 'b\\x0cc', ''])
    """
    return text.split("\n")


def load_run(json_path):
    """A run's three reports, normalised: the JSON as values, the Markdown and the JUnit as lines, each read as its
    bytes decode, with no newline translation."""
    paths = report_paths(json_path)
    missing = [str(p) for p in paths.values() if not p.is_file()]
    if missing:
        raise Unreadable("missing: " + ", ".join(missing))
    try:
        texts = {kind: p.read_bytes().decode("utf-8") for kind, p in paths.items()}
        report = json.loads(texts["json"])
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as e:
        raise Unreadable(f"{json_path}: {e}") from e
    normalise = normaliser(report)
    return {"json": normalised(report, normalise),
            **{kind: [normalise(line) for line in lines_of(texts[kind])] for kind in ("md", "xml")}}


def run_changes(a_json, b_json):
    """Every change between two runs' reports, the JSON's, then the Markdown's, then the JUnit's."""
    a, b = load_run(a_json), load_run(b_json)
    return (json_changes(a["json"], b["json"])
            + line_changes("md", a["md"], b["md"], headings(a["md"]), headings(b["md"]))
            + line_changes("xml", a["xml"], b["xml"]))


def run_set(directory):
    """The runs tools/jig-runs.sh wrote into a directory, by name, from its runs.tsv (run and report columns; a
    report of - is a run that wrote none, None here)."""
    try:
        rows = [line.split("\t") for line in (directory / "runs.tsv").read_text(encoding="utf-8").splitlines() if line]
    except (OSError, UnicodeDecodeError) as e:
        raise Unreadable(f"{directory / 'runs.tsv'}: {e}") from e
    columns = {name: i for i, name in enumerate(rows[0])} if rows else {}
    if "run" not in columns or "report" not in columns:
        raise Unreadable(f"{directory / 'runs.tsv'}: no run and report columns")
    for i, row in enumerate(rows[1:], start=2):
        if len(row) < len(rows[0]):
            raise Unreadable(f"{directory / 'runs.tsv'}: row {i} has {len(row)} columns, fewer than the header's {len(rows[0])}")
    return {row[columns["run"]]: (None if row[columns["report"]] == "-" else directory / row[columns["report"]])
            for row in rows[1:]}


def runs_of(path):
    """What an argument names: {None: its JSON report} for a run, {name: report} for a set of runs."""
    if path.is_file():
        if path.suffix not in (".json", ".md", ".xml"):
            raise Unreadable(f"{path}: not a report (.json, .md or .xml)")
        return {None: path.with_name(path.name[:-len(path.suffix)] + ".json")}
    if (path / "runs.tsv").is_file():
        return run_set(path)
    if path.is_dir():
        reports = sorted(path.glob("jig-*.json"))
        if len(reports) != 1:
            raise Unreadable(f"{path}: {len(reports)} JSON reports (jig-*.json), not one: name the report")
        return {None: reports[0]}
    raise Unreadable(f"{path}: no such report or directory")


# * Printing

def shown(value, width=100):
    """A value as one short text."""
    text = json.dumps(value, ensure_ascii=False, sort_keys=True)
    return text if len(text) <= width else text[:width - 3] + "..."


def common_prefix_length(a, b):
    """How many characters two texts share before they first differ (not os.path.commonprefix, which Python 3.15,
    the venv's, deprecates).

    >>> common_prefix_length("0011", "0021")
    2
    >>> common_prefix_length("00", "001")
    2
    """
    return next((i for i, (x, y) in enumerate(zip(a, b)) if x != y), min(len(a), len(b)))


def contrast(a, b, width=100):
    """A's and B's values side by side; two texts from a little before where they first differ."""
    if not (isinstance(a, str) and isinstance(b, str)):
        return f"{shown(a, width)} vs {shown(b, width)}"
    start = max(0, common_prefix_length(a, b) - 20)

    def cut(t):
        return ("..." if start else "") + t[start:start + width] + ("..." if len(t) > start + width else "")
    return f"{json.dumps(cut(a), ensure_ascii=False)} vs {json.dumps(cut(b), ensure_ascii=False)}"


def described(change):
    what = (f"only in B: {shown(change.b)}" if change.a is ABSENT else f"only in A: {shown(change.a)}"
            if change.b is ABSENT else contrast(change.a, change.b))
    return f"{change.kind} {change.where}: {what}"


def counted(judged):
    """How many changes of each report, and how many allowed: json 2 (1 allowed), md 0, xml 0."""
    def count(kind):
        whys = [why for c, why in judged if c.kind == kind]
        allowed = sum(1 for why in whys if why)
        return f"{kind} {len(whys)}" + (f" ({allowed} allowed)" if allowed else "")
    return ", ".join(map(count, KINDS))


def outcome_lines(name, judged, limit, show_allowed):
    """A comparison's lines: its counts and verdict, then each change not allowed (at most limit), and with
    --show-allowed each allowed one and its allowance."""
    refused = [c for c, why in judged if not why]
    verdict = "equal under the allowances" if not refused else f"{len(refused)} not allowed"
    listed = [f"  {described(c)}" for c in refused[:limit]] + (
        [f"  ... and {len(refused) - limit} more"] if len(refused) > limit else [])
    allowed = [f"  allowed by {why}: {described(c)}" for c, why in judged if why] if show_allowed else []
    return [f"{name + ': ' if name else ''}{counted(judged)}: {verdict}"] + listed + allowed


def compared(name, a_json, b_json, allow, limit, show_allowed):
    """One pair of runs compared: (its lines, 0 equal, 1 a change not allowed, 2 a report missing or unreadable)."""
    label = f"{name}: " if name else ""
    if a_json is None or b_json is None:
        side = "A" if a_json is None else "B"
        return [f"{label}{side} has no report (runs.tsv gives -)"], 2
    try:
        judged = [(c, allowance(c, allow)) for c in run_changes(a_json, b_json)]
    except Unreadable as e:
        return [f"{label}{e}"], 2
    return outcome_lines(name, judged, limit, show_allowed), (1 if any(not why for _, why in judged) else 0)


def parser():
    p = argparse.ArgumentParser(prog="compare_reports.py", description=__doc__.split("\n\n")[0],
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("a", metavar="A", help="a run's report (.json, .md or .xml), its report directory, or a "
                                          "tools/jig-runs.sh directory")
    p.add_argument("b", metavar="B", help="another, of the same kind")
    p.add_argument("--allow-keys", metavar="KEY", action="append", default=[],
                   help="allow a JSON difference at or under KEY (a dotted path, list positions left out, or a bare "
                        "key name at any depth)")
    p.add_argument("--allow-texts", metavar=("OLD", "NEW"), nargs=2, action="append", default=[],
                   help="allow a text that is A's with OLD replaced by NEW; with OLD empty, something B alone has "
                        "with NEW in it")
    p.add_argument("--allow-md-sections", metavar="TITLE", action="append", default=[],
                   help="allow a Markdown difference under a heading that begins with TITLE")
    p.add_argument("--show-allowed", action="store_true", help="list the allowed differences too")
    p.add_argument("--limit", metavar="N", type=int, default=50, help="list at most N differences a run (default 50)")
    return p


def main(argv=None):
    args = parser().parse_args(argv)
    allow = Allowances(tuple(args.allow_keys), tuple(map(tuple, args.allow_texts)), tuple(args.allow_md_sections))
    if any(not old and not new for old, new in allow.texts):
        print("compare_reports.py: --allow-texts '' '' allows nothing it can name: give OLD or NEW", file=sys.stderr)
        return 2
    if any(not title for title in allow.md_sections):
        print("compare_reports.py: --allow-md-sections '' would allow every Markdown change under any heading: give a section title", file=sys.stderr)
        return 2
    if any(not key for key in allow.keys):
        print("compare_reports.py: --allow-keys '' allows nothing it can name: give a key", file=sys.stderr)
        return 2
    try:
        a_runs, b_runs = runs_of(Path(args.a)), runs_of(Path(args.b))
    except Unreadable as e:
        print(f"compare_reports.py: {e}", file=sys.stderr)
        return 2
    if (None in a_runs) != (None in b_runs):
        print("compare_reports.py: one of A and B is a run and the other a set of runs (runs.tsv)", file=sys.stderr)
        return 2
    names = list(a_runs) + [n for n in b_runs if n not in a_runs]
    outcomes = [compared(n, a_runs[n], b_runs[n], allow, args.limit, args.show_allowed) if n in a_runs and n in b_runs
                else ([f"{n}: only in {'A' if n in a_runs else 'B'}"], 1) for n in names]
    header = [f"A: {args.a}", f"B: {args.b}"]
    status = max((code for _, code in outcomes), default=0)
    sets = [] if None in a_runs else [
        f"{len(names)} runs: " + ("equal under the allowances" if status == 0 else
                                  f"{sum(1 for _, code in outcomes if code)} differ or could not be compared")]
    print("\n".join(header + [line for lines, _ in outcomes for line in lines] + sets))
    return status


if __name__ == "__main__":
    sys.exit(main())
