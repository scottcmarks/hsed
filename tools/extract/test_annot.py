"""Unit tests for the annotation side (hsed_annot.py, annotate.py) and the extractor's md5 pins and
invariants (no TCG PDF needed):  the sidecar refusing a payload of the wrong kind for its anchor by
its first word, the kinds and the canonical order, the incremental tail's xref stream listing
itself, the pins of design section 5.1 step 1, and Core Table 7's two parts.

Run:  .venv/bin/python -I -m unittest discover -s tools/extract -p 'test_*.py'   (make extract-test)
"""
import io
import os
import tempfile
import unittest
from pathlib import Path

import pikepdf
import pypdf

import annotate as A
import extract_tables as X
import hsed_annot as H


class Vocabulary(unittest.TestCase):
    def test_first_word(self):
        self.assertEqual(H.first_word('erratum scott ["a" ~> "b"]'), "erratum")
        self.assertEqual(H.first_word("  (rule typeRow)"), "rule")
        self.assertIsNone(H.first_word('"typeRow"'))

    def test_every_word_has_its_anchors(self):
        self.assertEqual(H.VOCABULARY["rule"], ("section",))
        self.assertEqual(H.VOCABULARY["doc"], ("doc",))
        self.assertEqual(H.VOCABULARY["behaviour"], ("section", "item"))
        self.assertEqual(H.VOCABULARY["sig"], ())
        self.assertTrue(all(H.VOCABULARY[w] == ("table",) for w in ("typeRow", "erratum", "waive", "as", "refines", "prose")))

    def test_the_committed_kind_of_payload_passes(self):
        for anchor_id, payload in (("doc", "doc core201"), ("5.1.3", "rule typeRow & rule enumValues"),
                                   ("T75", 'erratum scott ["Fractoin" ~> "Fraction"]'),
                                   ("T149", 'waive "C-UID-UNIQUE" "00 00 00 05 00 00 02 15" "no erratum says which is wrong"'),
                                   ("3.3.10.2.1#3a", 'law "Sync.ifSendWhileBusy"'), ("5.3.3.6", 'behaviour "Base.get"')):
            self.assertIsNone(H.payload_problem(anchor_id, payload), (anchor_id, payload))

    def test_a_payload_on_the_wrong_kind_of_anchor_is_refused(self):
        self.assertEqual(H.payload_problem("T46", "rule typeRow"), "'rule' belongs on a section anchor, not on a table anchor")
        self.assertEqual(H.payload_problem("5.1.5", "refines (core 46)"), "'refines' belongs on a table anchor, not on a section anchor")
        self.assertEqual(H.payload_problem("5.1.3#p1", "rule typeRow"), "'rule' belongs on a section anchor, not on a item anchor")
        self.assertEqual(H.payload_problem("doc", "rule typeRow"), "'rule' belongs on a section anchor, not on the doc anchor")
        self.assertEqual(H.payload_problem("T46", "doc core201"), "'doc' belongs on the doc anchor, not on a table anchor")
        self.assertEqual(H.payload_problem("T46", 'law "Foo.bar"'), "'law' belongs on a item anchor, not on a table anchor")
        self.assertEqual(H.payload_problem("T46", "sig"), "'sig' belongs on no anchor, not on a table anchor")

    def test_an_unknown_first_word_is_refused(self):
        self.assertIn("unknown name 'frobnicate'", H.payload_problem("T46", "frobnicate"))

    def test_parse_hsa_refuses_the_line(self):
        with self.assertRaisesRegex(ValueError, r"x\.hsa:2: .*'rule' belongs on a section anchor, not on a table anchor"):
            H.parse_hsa("core-2.01 | doc | doc core201\ncore-2.01 | T46 | rule typeRow\n", "x.hsa")

    def test_kind_follows_the_id_and_a_behaviour_payload(self):
        self.assertEqual(H.kind_of("5.3.3.6", 'behaviour "Base.get"'), "Behaviour")
        self.assertEqual(H.kind_of("5.1.3", "rule typeRow"), "Rule")
        self.assertEqual(H.kind_of("3.3.10.2.1#3a", 'law "x"'), "Law")
        self.assertEqual(H.kind_of("T46", 'behaviour "x"'), "Bind")

    def test_canonical_order_is_natural(self):
        ids = ["T149", "T75", "doc", "T101", "5.1.3"]
        self.assertEqual(sorted(ids, key=lambda i: H.sort_key(H.kind_of(i, "x"), i)), ["doc", "5.1.3", "T75", "T101", "T149"])


def tiny_pdf() -> bytes:
    """A one-page PDF written in full by pypdf (a classic xref table)."""
    w = pypdf.PdfWriter()
    w.add_blank_page(width=200, height=200)
    buf = io.BytesIO()
    w.write(buf)
    return buf.getvalue()


class XrefStream(unittest.TestCase):
    def test_the_increment_lists_its_own_xref_stream(self):
        with tempfile.TemporaryDirectory() as d:
            pristine, out = os.path.join(d, "p.pdf"), os.path.join(d, "a.pdf")
            Path(pristine).write_bytes(tiny_pdf())
            anchor = H.Anchor("core-2.01", "T1", "prose")
            A.write_annotated(pristine, out, [H.make_annot(anchor, "0/1", 0, (10, 10, 50, 50))])
            data, head = Path(out).read_bytes(), Path(pristine).read_bytes()
            self.assertTrue(data.startswith(head))
            with pikepdf.open(out) as pdf:
                self.assertEqual(pdf.check_pdf_syntax(), [])
            self.assertEqual([a.id for a in H.read_pikepdf(out)], ["T1"])
            self.assertEqual(A.self_indexed_xref(data, len(head)), data)   # already lists itself: unchanged

    def test_pypdf_alone_leaves_it_out(self):
        """The defect self_indexed_xref repairs: without it qpdf warns."""
        with tempfile.TemporaryDirectory() as d:
            pristine = os.path.join(d, "p.pdf")
            Path(pristine).write_bytes(tiny_pdf())
            w = pypdf.PdfWriter(pristine, incremental=True)
            w.add_annotation(page_number=0, annotation=H.to_pypdf(H.make_annot(H.Anchor("core-2.01", "T1", "prose"), "0/1", 0, (1, 1, 2, 2))))
            buf = io.BytesIO()
            w.write(buf)
            raw = os.path.join(d, "raw.pdf")
            Path(raw).write_bytes(buf.getvalue())
            with pikepdf.open(raw) as pdf:
                self.assertTrue(any("xref stream itself" in m for m in pdf.check_pdf_syntax()))


class Pins(unittest.TestCase):
    def test_a_pinned_spec_takes_only_its_pdf(self):
        self.assertIsNone(X.pin_problem("core-2.01", "5ceb93ab118a578125e3755eb885b668"))
        self.assertIsNone(X.pin_problem("opal-2.30", "054e505db3895b25c38dd9dfbcc36259"))
        self.assertIn("pinned", X.pin_problem("core-2.01", "054e505db3895b25c38dd9dfbcc36259"))

    def test_an_unpinned_spec_takes_any(self):
        self.assertIsNone(X.pin_problem("core-2.02", "0" * 32))


class Table7(unittest.TestCase):
    def ir(self, parts):
        t = {"table_number": 7, "id": "T7", "ordinal": 8}
        return {"spec": "core-2.01", "labelOffset": 17, "headings": [],
                "tables": [dict(t, part=p, parts=n, n_rows=r, rows=[], header_rows=[]) for p, n, r in parts]}

    def check(self, parts):
        return [ok for name, ok, _ in X.invariants(self.ir(parts), None) if name.startswith("T7 ")]

    def test_two_parts_one_data_row(self):
        self.assertEqual(self.check([(1, 2, 0), (2, 2, 1)]), [True])
        self.assertEqual(self.check([(None, None, 1)]), [False])
        self.assertEqual(self.check([(1, 2, 0), (2, 2, 2)]), [False])

    def test_a_table_7_word_did_not_split_has_no_part(self):
        """Opal's Table 7 records no part: the invariants must not trip over it."""
        ir = {"spec": "opal-2.30", "labelOffset": 1, "headings": [],
              "tables": [{"table_number": 7, "id": "T7", "n_rows": 3, "rows": [], "header_rows": []}]}
        self.assertTrue(all(name for name, _, _ in X.invariants(ir, None)))


if __name__ == "__main__":
    unittest.main()
