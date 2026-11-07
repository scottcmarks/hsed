"""Unit tests for tools/compare_reports.py over small hand-made reports (no jig run needed): the normalising of the
version, the start and the report directory; the JSON compared as values, its lists aligned; the Markdown and the
JUnit compared as lines; each allowance, --allow-keys by path and by bare name, --allow-texts by replacement and with
an empty OLD for what B alone has, --allow-md-sections by heading; the runs of two tools/jig-runs.sh directories
paired by name; and the exit status. The module's doctests run with them.

Run:  .venv/bin/python -I -m unittest discover -s tools -p 'test_compare_reports.py'   (make compare-test)
"""
import contextlib
import copy
import doctest
import io
import json
import tempfile
import unittest
import warnings
from pathlib import Path

import compare_reports as C

VERSION = "Version 0.1.0.1 [claude/m5b ee1b00c]"
STARTED = "2026-11-06T13:45:12.345678Z"


def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(C))
    return tests


def report(version=VERSION, started=STARTED, report_dir="/scratch/a/run1"):
    """A small JSON report: the keys the jig writes that the comparison reads, and a few it compares."""
    return {
        "hsed": version,
        "started": started,
        "options": {"target": "sim", "tests": [], "report_dir": report_dir, "locking_ofs": "manufactured-inactive"},
        "targets": [{"role": "sim", "impl": "hsed-sim " + version}],
        "cases": [{"key": "UCT-01", "verdict": "pass", "observations": [{"name": "step 1", "value": "read"}]},
                  {"key": "ETC-02", "verdict": "pass", "observations": []}],
        "transcript": [{"op": "IF-RECV", "len": 2048}, {"op": "IF-SEND", "len": 512}, {"op": "IF-RECV", "len": 512}],
        "interrupted": None,
        "exit": 0,
    }


def markdown(version=VERSION, started="2026-11-06T13:45:12Z", cases=("- UCT-01: pass", "- ETC-02: pass"), extra=()):
    return "\n".join([
        "# hsed-jig run: sim", "",
        f"- hsed: {version}", f"- started: {started}", "- IF-SENDs passed to a drive's transport: 0", "- exit: 0", "",
        "## Targets", "", "| role | impl |", "|---|---|", f"| sim | hsed-sim {version} |", "",
        "## Cases", "", *cases, "",
        *extra,
        "## Transcript", "", "```", "# not a heading: fenced", "```", ""])


def junit(version=VERSION, started="2026-11-06T13:45:12"):
    return "\n".join([
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<testsuites name="hsed-jig" tests="3" failures="0" skipped="0">',
        f'  <testsuite name="hsed-jig sim" tests="3" failures="0" skipped="0" timestamp="{started}">',
        "    <properties>",
        f'      <property name="hsed" value="{version}"/>',
        '      <property name="if_send_issued" value="0"/>',
        "    </properties>",
        '    <testcase classname="hsed-jig.sim" name="UCT-01"/>',
        '    <testcase classname="hsed-jig.sim" name="ETC-02"/>',
        '    <testcase classname="hsed-jig.sim" name="exit code"/>',
        "  </testsuite>", "</testsuites>", ""])


class Reports(unittest.TestCase):
    """Writes report triples into a scratch directory and compares them through main."""

    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.root = Path(self.dir.name)

    def tearDown(self):
        self.dir.cleanup()

    def write(self, where, stem="jig-sim-20261106T134512Z", json_report=None, md=None, xml=None):
        """A run's three reports under the scratch directory; its JSON's path."""
        directory = self.root / where
        directory.mkdir(parents=True, exist_ok=True)
        path = directory / f"{stem}.json"
        path.write_text(json.dumps(json_report if json_report is not None else report()), encoding="utf-8")
        (directory / f"{stem}.md").write_text(md if md is not None else markdown(), encoding="utf-8")
        (directory / f"{stem}.xml").write_text(xml if xml is not None else junit(), encoding="utf-8")
        return path

    def compare(self, a, b, *options):
        """main's exit status and what it printed."""
        out = io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(out):
            status = C.main([*options, str(a), str(b)])
        return status, out.getvalue()


class Normalising(Reports):
    def test_two_runs_differing_in_version_start_and_report_directory_are_equal(self):
        other = "Version 0.1.0.1 [claude/m5b 1234567 (dirty)]"
        a = self.write("a/run1")
        b = self.write("b/run1", stem="jig-sim-20261107T090001Z",
                       json_report=report(version=other, started="2026-11-07T09:00:01Z", report_dir="/scratch/b/run1"),
                       md=markdown(version=other, started="2026-11-07T09:00:01Z"),
                       xml=junit(version=other, started="2026-11-07T09:00:01"))
        status, out = self.compare(a, b)
        self.assertEqual(status, 0, out)
        self.assertIn("json 0, md 0, xml 0: equal under the allowances", out)

    def test_the_version_is_normalised_wherever_it_stands(self):
        normalise = C.normaliser(report())
        self.assertEqual(normalise("hsed-sim " + VERSION), "hsed-sim <hsed>")
        self.assertEqual(normalise("- started: 2026-11-06T13:45:12Z"), "- started: <started>")
        self.assertEqual(normalise('timestamp="2026-11-06T13:45:12"'), 'timestamp="<started>"')
        self.assertEqual(normalise(STARTED), "<started>")

    def test_the_report_directory_is_normalised_as_a_path_alone(self):
        normalise = C.normaliser(report(report_dir="/scratch/a/run1"))
        self.assertEqual(normalise("/scratch/a/run1"), "<report_dir>")
        self.assertEqual(normalise("written to /scratch/a/run1/jig-sim.json"), "written to <report_dir>/jig-sim.json")
        self.assertEqual(normalise("/scratch/a/run10/x"), "/scratch/a/run10/x")
        short = C.normaliser(report(report_dir="out"))
        self.assertEqual(short("out"), "<report_dir>")
        self.assertEqual(short("time out"), "time out")

    def test_a_version_escaped_in_a_junit_attribute_is_normalised(self):
        version = 'Version 1 [a&b "c"]'
        normalise = C.normaliser(report(version=version))
        self.assertEqual(normalise('value="Version 1 [a&amp;b &quot;c&quot;]"'), 'value="<hsed>"')


class Json(Reports):
    def test_a_changed_option_is_one_difference_at_its_path(self):
        b_report = report()
        b_report["options"]["tests"] = ["UCT-01", "ETC-02"]
        status, out = self.compare(self.write("a"), self.write("b", json_report=b_report))
        self.assertEqual(status, 1, out)
        self.assertIn('json options.tests: [] vs ["UCT-01", "ETC-02"]', out)
        self.assertIn("json 1, md 0, xml 0: 1 not allowed", out)

    def test_an_element_one_side_alone_has_is_one_difference_not_a_shift(self):
        b_report = report()
        b_report["transcript"].insert(1, {"op": "IF-SEND", "len": 1024})
        changes = C.json_changes(report(), b_report)
        self.assertEqual([(c.where, c.a, c.b) for c in changes], [("transcript[1]", C.ABSENT, {"op": "IF-SEND", "len": 1024})])

    def test_a_changed_element_is_paired_and_compared_within(self):
        b_report = report()
        b_report["transcript"][2]["len"] = 1024
        changes = C.json_changes(report(), b_report)
        self.assertEqual([(c.where, c.keys, c.a, c.b) for c in changes], [("transcript[2].len", ("transcript", "len"), 512, 1024)])

    def test_a_member_one_side_alone_has(self):
        b_report = report()
        b_report["left_out"] = ["ETC-20"]
        del b_report["interrupted"]
        changes = C.json_changes(report(), b_report)
        self.assertEqual([(c.where, c.a, c.b) for c in changes], [("interrupted", None, C.ABSENT), ("left_out", C.ABSENT, ["ETC-20"])])

    def test_values_of_different_types_differ(self):
        self.assertEqual(len(C.json_changes({"x": 1}, {"x": "1"})), 1)
        self.assertEqual(len(C.json_changes({"x": True}, {"x": 1})), 1)
        self.assertEqual(len(C.json_changes({"x": 1}, {"x": 1.0})), 1)
        self.assertEqual(C.json_changes({"x": [1, {"y": None}]}, {"x": [1, {"y": None}]}), [])

    def test_positions_paired_across_a_shift_name_both(self):
        changes = C.json_changes(["a", "b", {"k": 1}], ["z", "a", "b", {"k": 2}])
        self.assertEqual([c.where for c in changes], ["[0]", "[2/3].k"])

    def test_two_long_texts_are_shown_from_a_little_before_where_they_first_differ(self):
        a, b = "0" * 200, "0" * 150 + "1" + "0" * 49
        with warnings.catch_warnings():
            warnings.simplefilter("error")
            text = C.contrast(a, b)
        self.assertEqual(text, json.dumps("..." + a[130:]) + " vs " + json.dumps("..." + b[130:]))
        self.assertEqual(C.contrast("abc", "abd"), '"abc" vs "abd"')

    def test_the_differences_listed_are_limited_and_the_rest_counted(self):
        b_report = report()
        b_report["transcript"] += [{"op": "IF-SEND", "len": n} for n in range(5)]
        status, out = self.compare(self.write("a"), self.write("b", json_report=b_report), "--limit", "2")
        self.assertEqual(status, 1, out)
        self.assertIn("json 5, md 0, xml 0: 5 not allowed", out)
        self.assertIn("  ... and 3 more", out)
        self.assertEqual(sum(1 for line in out.splitlines() if line.startswith("  json transcript")), 2)


class AllowKeys(Reports):
    def setUp(self):
        super().setUp()
        b_report = report()
        b_report["options"]["tests"] = ["UCT-01", "ETC-02"]
        self.a, self.b = self.write("a"), self.write("b", json_report=b_report)

    def test_a_dotted_path_allows_the_difference_under_it(self):
        status, out = self.compare(self.a, self.b, "--allow-keys", "options.tests")
        self.assertEqual(status, 0, out)
        self.assertIn("json 1 (1 allowed), md 0, xml 0: equal under the allowances", out)

    def test_a_bare_name_allows_it_at_any_depth_and_a_parent_path_too(self):
        self.assertEqual(self.compare(self.a, self.b, "--allow-keys", "tests")[0], 0)
        self.assertEqual(self.compare(self.a, self.b, "--allow-keys", "options")[0], 0)

    def test_a_path_that_is_not_a_prefix_by_keys_does_not(self):
        self.assertEqual(self.compare(self.a, self.b, "--allow-keys", "options.test")[0], 1)
        self.assertEqual(self.compare(self.a, self.b, "--allow-keys", "tests.options")[0], 1)

    def test_list_positions_are_left_out_of_a_path(self):
        b_report = report()
        b_report["cases"][0]["observations"][0]["value"] = "read again"
        b = self.write("c", json_report=b_report)
        self.assertEqual(self.compare(self.a, b, "--allow-keys", "cases.observations.value")[0], 0)
        self.assertEqual(self.compare(self.a, b, "--allow-keys", "cases.verdict")[0], 1)

    def test_the_allowed_differences_are_listed_on_request(self):
        status, out = self.compare(self.a, self.b, "--allow-keys", "options.tests", "--show-allowed")
        self.assertEqual(status, 0, out)
        self.assertIn('allowed by --allow-keys options.tests: json options.tests: [] vs ["UCT-01", "ETC-02"]', out)

    def test_an_empty_key_is_a_usage_error(self):
        status, out = self.compare(self.a, self.b, "--allow-keys", "")
        self.assertEqual(status, 2, out)
        self.assertIn("--allow-keys ''", out)


class AllowTexts(Reports):
    OLD_LINE = "- IF-SENDs issued on a drive: 0"
    NEW_LINE = "- IF-SENDs passed to a drive's transport: 0"

    def test_a_replacement_allows_a_changed_string_and_line(self):
        b_report = report()
        b_report["cases"][0]["observations"][0]["value"] = "read (design 9.3 item 6 and 13.8, Scott's)"
        a_report = report()
        a_report["cases"][0]["observations"][0]["value"] = "read (design 9.3 item 6; M4c)"
        a = self.write("a", json_report=a_report, md=markdown().replace(self.NEW_LINE, self.OLD_LINE))
        b = self.write("b", json_report=b_report)
        status, out = self.compare(a, b)
        self.assertEqual(status, 1, out)
        self.assertIn("json 1, md 1, xml 0: 2 not allowed", out)
        status, out = self.compare(a, b, "--allow-texts", "issued on a drive", "passed to a drive's transport",
                                   "--allow-texts", "; M4c)", " and 13.8, Scott's)")
        self.assertEqual(status, 0, out)
        self.assertIn("json 1 (1 allowed), md 1 (1 allowed), xml 0", out)

    def test_a_replacement_that_explains_part_of_a_change_does_not_allow_it(self):
        a = self.write("a", md=markdown().replace(self.NEW_LINE, self.OLD_LINE + " (since M4c)"))
        b = self.write("b")
        status, out = self.compare(a, b, "--allow-texts", "issued on a drive", "passed to a drive's transport")
        self.assertEqual(status, 1, out)
        self.assertIn("md line 5 (hsed-jig run: sim): ", out)

    def test_an_empty_old_allows_what_b_alone_has_with_new_in_it(self):
        b_report = report()
        b_report["cases"][1]["observations"].append({"name": "left out", "value": "ETC-20: the Locking SP is Manufactured"})
        b_report["left_out"] = ["ETC-20"]
        b = self.write("b", json_report=b_report,
                       md=markdown(cases=("- UCT-01: pass", "- ETC-02: pass", "- left out: ETC-20")))
        a = self.write("a")
        status, out = self.compare(a, b, "--allow-texts", "", "ETC-20")
        self.assertEqual(status, 0, out)
        self.assertIn("json 2 (2 allowed), md 1 (1 allowed), xml 0", out)

    def test_an_empty_old_finds_new_in_the_key_of_a_member_b_adds(self):
        b_report = report()
        b_report["left_out"] = []
        b = self.write("b", json_report=b_report)
        self.assertEqual(self.compare(self.write("a"), b, "--allow-texts", "", "ETC-20")[0], 1)
        self.assertEqual(self.compare(self.write("c"), b, "--allow-texts", "", "left_out")[0], 0)

    def test_an_empty_old_whose_new_is_not_in_what_b_adds_does_not_allow_it(self):
        b = self.write("b", md=markdown(cases=("- UCT-01: pass", "- ETC-02: pass", "- ETC-20: skipped")))
        status, out = self.compare(self.write("a"), b, "--allow-texts", "", "left out")
        self.assertEqual(status, 1, out)
        self.assertIn('md line 18 (Cases): only in B: "- ETC-20: skipped"', out)

    def test_an_empty_old_never_allows_what_a_alone_has(self):
        a = self.write("a", md=markdown(cases=("- UCT-01: pass", "- ETC-02: pass", "- left out: ETC-20")))
        status, out = self.compare(a, self.write("b"), "--allow-texts", "", "left out")
        self.assertEqual(status, 1, out)
        self.assertIn('only in A: "- left out: ETC-20"', out)

    def test_an_empty_old_allows_a_value_where_a_has_an_empty_one(self):
        self.assertIsNotNone(C.text_allows(None, "stopped: SIGINT", (("", "SIGINT"),)))
        self.assertIsNotNone(C.text_allows("", "stopped: SIGINT", (("", "SIGINT"),)))
        self.assertIsNotNone(C.text_allows([], [{"name": "SIGINT"}], (("", "SIGINT"),)))
        self.assertIsNone(C.text_allows("stopped", "stopped: SIGINT", (("", "SIGINT"),)))
        self.assertIsNone(C.text_allows(0, 5, (("", "5"),)))

    def test_an_empty_old_searches_keys_and_strings_inside_what_b_adds(self):
        self.assertIsNotNone(C.text_allows(C.ABSENT, {"left_out": [1, 2]}, (("", "left_out"),)))
        self.assertIsNotNone(C.text_allows(C.ABSENT, [{"k": "ETC-20"}], (("", "ETC-20"),)))
        self.assertIsNone(C.text_allows(C.ABSENT, [{"k": "ETC-20"}], (("", "ETC-21"),)))

    def test_an_allowance_empty_on_both_sides_is_a_usage_error(self):
        status, out = self.compare(self.write("a"), self.write("b"), "--allow-texts", "", "")
        self.assertEqual(status, 2, out)


class AllowMdSections(Reports):
    SECTION = ("## Left out", "", "- ETC-20: the Locking SP is Manufactured", "")

    def test_a_section_b_alone_has_is_allowed_by_its_title(self):
        b = self.write("b", md=markdown(extra=self.SECTION))
        status, out = self.compare(self.write("a"), b)
        self.assertEqual(status, 1, out)
        self.assertIn("json 0, md 4, xml 0: 4 not allowed", out)
        status, out = self.compare(self.write("a"), b, "--allow-md-sections", "Left")
        self.assertEqual(status, 0, out)
        self.assertIn("md 4 (4 allowed)", out)

    def test_a_section_title_allows_nothing_outside_it(self):
        b = self.write("b", md=markdown(cases=("- UCT-01: fail", "- ETC-02: pass")))
        status, out = self.compare(self.write("a"), b, "--allow-md-sections", "Left out", "--allow-md-sections", "Targets")
        self.assertEqual(status, 1, out)
        self.assertIn('md line 16 (Cases): "- UCT-01: pass" vs "- UCT-01: fail"', out)
        self.assertEqual(self.compare(self.write("c"), b, "--allow-md-sections", "Cases")[0], 0)

    def test_headings_nest_by_level_and_fenced_code_holds_none(self):
        lines = ["# run", "- a", "## Cases", "### UCT-01", "- pass", "## Transcript", "```", "## not one", "```", "x"]
        self.assertEqual(C.headings(lines), [
            ("run",), ("run",), ("run", "Cases"), ("run", "Cases", "UCT-01"), ("run", "Cases", "UCT-01"),
            ("run", "Transcript"), ("run", "Transcript"), ("run", "Transcript"), ("run", "Transcript"), ("run", "Transcript")])

    def test_a_changed_line_is_allowed_only_where_both_sides_lie_under_the_section(self):
        change = C.Change("md", "line 3", (), "x", "y", ("run", "Cases"), ("run", "Transcript"))
        self.assertFalse(C.section_allows(change, "Cases"))
        self.assertTrue(C.section_allows(change, "run"))

    def test_an_empty_section_title_is_a_usage_error(self):
        b = self.write("b", md=markdown(extra=self.SECTION))
        status, out = self.compare(self.write("a"), b, "--allow-md-sections", "")
        self.assertEqual(status, 2, out)
        self.assertIn("--allow-md-sections ''", out)


class JUnit(Reports):
    def test_a_testcase_b_adds_is_a_difference_a_text_allowance_may_allow(self):
        xml = junit().replace('    <testcase classname="hsed-jig.sim" name="exit code"/>',
                              '    <testcase classname="hsed-jig.sim" name="ETC-20"/>\n'
                              '    <testcase classname="hsed-jig.sim" name="exit code"/>')
        b = self.write("b", xml=xml)
        status, out = self.compare(self.write("a"), b)
        self.assertEqual(status, 1, out)
        self.assertIn('xml line 10: only in B: "    <testcase classname=\\"hsed-jig.sim\\" name=\\"ETC-20\\"/>"', out)
        self.assertEqual(self.compare(self.write("c"), b, "--allow-texts", "", 'name="ETC-20"')[0], 0)


class Locating(Reports):
    def test_a_report_directory_and_a_report_of_any_of_the_three_kinds(self):
        a = self.write("a")
        b = self.write("b")
        for left, right in ((a.parent, b.parent), (a.with_suffix(".md"), b), (a, b.with_suffix(".xml"))):
            status, out = self.compare(left, right)
            self.assertEqual(status, 0, out)

    def test_a_directory_with_two_reports_names_neither(self):
        a = self.write("a")
        self.write("a", stem="jig-sim-20261106T134513Z")
        status, out = self.compare(a.parent, self.write("b").parent)
        self.assertEqual(status, 2, out)
        self.assertIn("2 JSON reports (jig-*.json), not one", out)

    def test_a_report_missing_its_markdown_is_unreadable(self):
        a = self.write("a")
        a.with_suffix(".md").unlink()
        status, out = self.compare(a, self.write("b"))
        self.assertEqual(status, 2, out)
        self.assertIn("missing: ", out)

    def test_a_stem_with_dots_finds_its_own_markdown_and_junit(self):
        a = self.write("a", stem="jig-replay-a.b-20261106T134512Z")
        self.assertEqual(C.report_paths(a)["md"].name, "jig-replay-a.b-20261106T134512Z.md")
        self.assertEqual(self.compare(a, self.write("b"))[0], 0)


class RunSets(Reports):
    def write_set(self, where, runs):
        """A tools/jig-runs.sh directory: each run's reports and runs.tsv."""
        rows = ["run\texit\tentries\treport\targs"]
        for name, json_report in runs.items():
            path = self.write(f"{where}/run{name}", json_report=json_report)
            rows.append(f"{name}\t{json_report['exit']}\t{len(json_report['transcript'])}\t{path.relative_to(self.root / where)}\t--target sim")
        (self.root / where / "runs.tsv").write_text("\n".join(rows) + "\n", encoding="utf-8")
        return self.root / where

    def test_runs_are_paired_by_name_and_each_compared(self):
        changed = report()
        changed["exit"] = 1
        a = self.write_set("a", {"1": report(), "1m": report()})
        b = self.write_set("b", {"1": report(), "1m": changed})
        status, out = self.compare(a, b)
        self.assertEqual(status, 1, out)
        self.assertIn("1: json 0, md 0, xml 0: equal under the allowances", out)
        self.assertIn("1m: json 1, md 0, xml 0: 1 not allowed", out)
        self.assertIn("json exit: 0 vs 1", out)
        self.assertIn("2 runs: 1 differ or could not be compared", out)

    def test_a_run_one_set_alone_has_is_a_difference(self):
        a = self.write_set("a", {"1": report(), "2": report()})
        b = self.write_set("b", {"1": report()})
        status, out = self.compare(a, b)
        self.assertEqual(status, 1, out)
        self.assertIn("2: only in A", out)

    def test_equal_sets_under_an_allowance(self):
        b_report = report()
        b_report["options"]["tests"] = ["UCT-01"]
        a = self.write_set("a", {"1": report(), "2": report()})
        b = self.write_set("b", {"1": b_report, "2": copy.deepcopy(b_report)})
        status, out = self.compare(a, b, "--allow-keys", "options.tests")
        self.assertEqual(status, 0, out)
        self.assertIn("2 runs: equal under the allowances", out)

    def test_a_run_and_a_set_are_not_compared(self):
        a = self.write_set("a", {"1": report()})
        status, out = self.compare(a, self.write("b"))
        self.assertEqual(status, 2, out)

    def test_a_run_that_wrote_no_report_is_unreadable(self):
        a = self.write_set("a", {"1": report()})
        b = self.write_set("b", {"1": report()})
        (b / "runs.tsv").write_text("run\texit\tentries\treport\targs\n1\t2\t-\t-\t--target sim\n", encoding="utf-8")
        status, out = self.compare(a, b)
        self.assertEqual(status, 2, out)
        self.assertIn("1: B has no report", out)

    def test_a_runs_tsv_row_shorter_than_the_header_is_unreadable(self):
        a = self.write_set("a", {"1": report()})
        b = self.write_set("b", {"1": report()})
        (b / "runs.tsv").write_text("run\texit\tentries\treport\targs\n1\t0\n", encoding="utf-8")
        status, out = self.compare(a, b)
        self.assertEqual(status, 2, out)
        self.assertIn("row 2 has 2 columns", out)


if __name__ == "__main__":
    unittest.main()
