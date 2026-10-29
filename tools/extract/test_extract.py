"""Unit tests for the pure rules in extract_tables.py (no PDF needed):
ragged-row join, section-number parsing, list-item labels, word normalisation and hashing.

Run:  .venv/bin/python -I -m unittest discover -s tools/extract -p 'test_*.py'   (make extract-test)
"""
import hashlib
import unittest

import extract_tables as X


def cell(text, x0, x1, est=False):
    box = [x0, 100.0, x1, 110.0]
    return {"text": text, "kind": "TD", **({"est_bbox": box} if est else {"bbox": box})}


# Opal T17: header 'Property Name | TPer ... | Host ...' at 58.5 / 227.1 / 395.7
HEADER = [cell("Property Name", 58.5, 133.2), cell("TPer Property Requirements", 227.1, 369.4), cell("Host Property Requirements", 395.7, 538.0)]
PREV = [cell("MaxSubpackets", 58.5, 133.2), cell("(M) 1 minimum", 227.1, 278.4), cell("(M)", 395.7, 411.4)]
# a nine-column byte layout: Byte | Bit 7 | ... | Bit 0
BYTE_HEADER = [cell("Byte", 89.0, 124.3)] + [cell(f"Bit {7 - i}", 151.8 + 54.7 * i, 157.3 + 54.7 * i) for i in range(8)]
BYTE_PREV = [cell("2", 97.1, 102.6), cell("Version", 219.7, 253.6), cell("Reserved", 434.8, 477.5)]


class RaggedJoin(unittest.TestCase):
    def test_continuation_row_joins_the_overlapping_column(self):
        row = [cell("Initial Assumption: 1 Minimum", 395.7, 500.1)]
        self.assertEqual(X.ragged_join_targets(HEADER, PREV, row), [2])

    def test_two_continued_columns(self):
        row = [cell("more", 227.1, 300.0), cell("and more", 400.0, 500.0)]
        self.assertEqual(X.ragged_join_targets(HEADER, PREV, row), [1, 2])

    def test_full_width_row_starting_in_the_first_column_is_never_joined(self):
        self.assertIsNone(X.ragged_join_targets(BYTE_HEADER, BYTE_PREV, [cell("3", 97.1, 102.6), cell("Length", 331.6, 362.1)]))

    def test_row_as_wide_as_the_header_is_not_joined(self):
        row = [cell("", 58.5, 60.0, est=True), cell("x", 227.1, 240.0), cell("y", 395.7, 400.0)]
        self.assertIsNone(X.ragged_join_targets(HEADER, PREV, row))

    def test_row_with_an_empty_cell_is_not_joined(self):
        self.assertIsNone(X.ragged_join_targets(HEADER, PREV, [cell("", 227.1, 240.0, est=True), cell("y", 395.7, 400.0)]))

    def test_first_data_row_has_no_predecessor(self):
        self.assertIsNone(X.ragged_join_targets(HEADER, None, [cell("y", 395.7, 400.0)]))

    def test_no_header_geometry_means_no_join(self):
        self.assertIsNone(X.ragged_join_targets([{"text": "a", "kind": "TH"}], PREV, [cell("y", 395.7, 400.0)]))
        self.assertIsNone(X.ragged_join_targets([], PREV, [cell("y", 395.7, 400.0)]))

    def test_cell_goes_to_the_predecessor_cell_it_overlaps_most(self):
        row = [cell("spans two", 260.0, 420.0)]            # 18.4 pt over column 1, 15.7 pt over column 2
        self.assertEqual(X.ragged_join_targets(HEADER, PREV, row), [1])

    def test_column_bands(self):
        self.assertEqual(X.column_bands(HEADER), [(56.5, 225.1), (225.1, 393.7), (393.7, float("inf"))])
        self.assertIsNone(X.column_bands([cell("b", 227.1, 240.0), cell("a", 58.5, 60.0)]))   # not left-to-right


class SectionNumbers(unittest.TestCase):
    def test_numbered_heading(self):
        self.assertEqual(X.parse_heading("5.1.3 Column Types"), ("5.1.3", "Column Types"))
        self.assertEqual(X.parse_heading("1 Introduction"), ("1", "Introduction"))
        self.assertEqual(X.parse_heading("3.3.10.2.1 Restrictions "), ("3.3.10.2.1", "Restrictions"))

    def test_unnumbered_heading(self):
        self.assertEqual(X.parse_heading("Revision History"), (None, "Revision History"))
        self.assertEqual(X.parse_heading("Table 03 Foo Table Description"), (None, "Table 03 Foo Table Description"))

    def test_contents_lines_are_not_headings(self):
        self.assertTrue(X.is_toc_line("1 INTRODUCTION ........................ 1"))
        self.assertTrue(X.is_toc_line("6 APPENDIX 1 – REQUIRED UID ASSIGNMENTS ....... 283"))
        self.assertFalse(X.is_toc_line("5.1.3 Column Types"))
        self.assertFalse(X.is_toc_line("Tables"))


class ItemLabels(unittest.TestCase):
    def test_label_forms(self):
        self.assertEqual(X.parse_label("3. After an IF-SEND command"), ("3", "After an IF-SEND command"))
        self.assertEqual(X.parse_label("1) If an authority"), ("1", "If an authority"))
        self.assertEqual(X.parse_label("1 The host tells the SP"), ("1", "The host tells the SP"))
        self.assertEqual(X.parse_label("a. While awaiting receipt"), ("a", "While awaiting receipt"))
        self.assertEqual(X.parse_label("b) the thousands separator"), ("b", "the thousands separator"))
        self.assertEqual(X.parse_label("(c) parenthesised"), ("c", "parenthesised"))
        self.assertEqual(X.parse_label("iii. If no additional responses"), ("iii", "If no additional responses"))
        self.assertEqual(X.parse_label("[2] Trusted Computing Group"), ("[2]", "Trusted Computing Group"))
        self.assertEqual(X.parse_label("[1]. IETF RFC 2119"), ("[1]", "IETF RFC 2119"))

    def test_bullets(self):
        self.assertEqual(X.parse_label("• Mandatory (M): a feature"), (X.BULLET, "Mandatory (M): a feature"))
        self.assertEqual(X.parse_label("o MBR Shadowing feature SHALL"), (X.BULLET, "MBR Shadowing feature SHALL"))

    def test_no_label(self):
        self.assertEqual(X.parse_label("Table 46 AC_element"), (None, "Table 46 AC_element"))
        self.assertEqual(X.parse_label("of the following conditions are met:"), (None, "of the following conditions are met:"))
        self.assertEqual(X.parse_label("3.2.3.4.1.1.1 Reserved"), (None, "3.2.3.4.1.1.1 Reserved"))
        self.assertEqual(X.parse_label("2048 bytes"), (None, "2048 bytes"))

    def test_nested_labels_concatenate(self):
        self.assertEqual(X.item_label(["3", "a"]), "3a")
        self.assertEqual(X.item_label(["3", "d", "ii"]), "3dii")
        self.assertEqual(X.item_label(["2", None, "a"]), "2a")
        self.assertEqual(X.item_label(["•5", "•1"]), "•5•1")
        self.assertIsNone(X.item_label([None, None]))
        self.assertIsNone(X.item_label([]))


class Hashing(unittest.TestCase):
    def test_normalisation(self):
        self.assertEqual(X.norm_words('IF-SEND, "quoted" Words.  C_PIN\tx'), ["ifsend", "quoted", "words", "c_pin", "x"])
        self.assertEqual(X.norm_words("0x00 – 0x0FFF (M) a­b "), ["0x00", "0x0fff", "m", "ab"])
        self.assertEqual(X.norm_words(""), [])

    def test_hash_is_sha256_of_the_space_joined_words(self):
        words = X.norm_words("The TPer SHALL return a 2048-byte ComPacket.")
        self.assertEqual(X.words_hash(words), hashlib.sha256(b"the tper shall return a 2048byte compacket").hexdigest())

    def test_hash_ignores_case_and_punctuation(self):
        self.assertEqual(X.words_hash(X.norm_words("Hello, World!")), X.words_hash(X.norm_words("hello world")))


if __name__ == "__main__":
    unittest.main()
