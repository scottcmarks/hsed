"""Unit tests for the pure rules in extract_tables.py (no PDF needed):
ragged-row join, section-number parsing, list-item labels, word normalisation and hashing,
paragraph text located glyph by glyph, signature blocks, table references and List of Tables entries,
byte grids and their cells' drawn extents, the sections whose paragraphs carry their words.

Run:  .venv/bin/python -I -m unittest discover -s tools/extract -p 'test_*.py'   (make extract-test)
"""
import hashlib
import unittest

import pymupdf

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


def glyphs(text, x=100.0, y=100.0, page=0, gap_after=()):
    """Synthetic glyph records for a line of text, 5 pt per character on a 10 pt font; a character
    whose index is in gap_after is followed by a 5 pt gap (wider than 0.25 em: a space)."""
    out, pos = [], x
    for i, c in enumerate(text):
        out.append((page, {"c": c, "quad": [(pos, y - 8), (pos + 5, y - 8), (pos, y + 2), (pos + 5, y + 2)], "origin": (pos, y),
                           "size": 10.0, "dir": (1.0, 0.0), "seq": len(out)}))
        pos += 5 + (5 if i in gap_after else 0)
    return out


class ParaChars(unittest.TestCase):
    def test_gaps_become_spaces_without_glyphs(self):
        chars = X.para_chars(glyphs("TableN", gap_after={4}))
        self.assertEqual("".join(c for c, _ in chars), "Table N")
        self.assertIsNone(chars[5][1])
        self.assertEqual(chars[6][1]["c"], "N")

    def test_the_text_is_what_text_lines_gives(self):
        recs = glyphs("  Two  words ", gap_after={6}) + glyphs("next line", y=120.0)
        self.assertEqual("".join(c for c, _ in X.para_chars(recs)), X.norm(" ".join(X.text_lines(recs))))

    def test_collapse(self):
        self.assertEqual(X.collapse_chars([(" ", None), ("a", 1), (" ", None), ("\t", None), ("b", 2), (" ", None)]),
                         [("a", 1), (" ", None), ("b", 2)])

    def test_collapse_is_norm_glyph_by_glyph(self):
        for text in ["", " ", "a", "  a  b\t\tc ", "a \n b", " x ", "ab  "]:
            pairs = [(c, i) for i, c in enumerate(text)]
            out = X.collapse_chars(pairs)
            self.assertEqual("".join(c for c, _ in out), X.norm(text), text)
            self.assertTrue(all(g is None if c == " " else text[g] == c for c, g in out), text)

    def test_item_body_drops_the_label_and_its_space(self):
        chars = [(c, i) for i, c in enumerate("a) if the bit is set, see Table 11")]
        label, body = X.parse_label("".join(c for c, _ in chars))
        self.assertEqual(label, "a")
        self.assertEqual("".join(c for c, _ in X.item_body_chars(chars, label)), body)
        self.assertEqual(X.item_body_chars(chars, label)[0], ("i", 3))
        self.assertEqual(X.item_body_chars(chars, None), chars)


def item(k, text, kind="P", section="5.3.3.6", page=200, parent=(0,)):
    return {"path": parent + (k,), "kind": kind, "text": text, "section": section, "page": page, "rect": [90.0, 700.0 - 10 * k, 300.0, 710.0 - 10 * k]}


def items(*texts, start=1):
    return [item(start + i, t) for i, t in enumerate(texts)]


class SignatureBlocks(unittest.TestCase):
    def test_a_table_and_object_method_has_two_heads(self):
        (sig,) = X.sig_blocks([item(0, "This method is used to fetch the values.")]
                              + items("TableUID.Get [", "ObjectUID.Get [", "Cellblock : cell_block ]", "=>", "[ Result : bytes ]"))
        self.assertEqual(sig["heads"], ["TableUID.Get", "ObjectUID.Get"])
        self.assertEqual(sig["text"], "TableUID.Get [\nObjectUID.Get [\nCellblock : cell_block ]\n=>\n[ Result : bytes ]")
        self.assertEqual((sig["section"], sig["page"], sig["struct_path"]), ("5.3.3.6", 200, "0/1"))
        self.assertEqual(sig["rect"], [90.0, 650.0, 300.0, 700.0])

    def test_the_block_ends_where_the_result_closes(self):
        (sig,) = X.sig_blocks(items("SMUID.StartSession [ see StartSession definition in 5.2.3.1]", "=>", "SMUID.SyncSession [",
                                    "HostSessionID : uinteger,", "SignedHash = bytes ]", "The SyncSession is returned by the TPer."))
        self.assertEqual(sig["heads"], ["SMUID.StartSession"])
        self.assertTrue(sig["text"].endswith("SignedHash = bytes ]"))

    def test_a_method_with_no_response_ends_where_its_parameters_close(self):
        (sig,) = X.sig_blocks(items("SMUID.CloseSession [", "RemoteSessionNumber : uinteger,", "LocalSessionNumber : uinteger ]",
                                    "This is the portion of the session number."))
        self.assertEqual(sig["text"], "SMUID.CloseSession [\nRemoteSessionNumber : uinteger,\nLocalSessionNumber : uinteger ]")

    def test_a_head_printed_without_its_bracket(self):
        (sig,) = X.sig_blocks(items("CredentialObjectUID.Sign", "HashObjectUID.Sign[", "BufferOut = cell_block ]", "=>", "[ Result : bytes ]"))
        self.assertEqual(sig["heads"], ["CredentialObjectUID.Sign", "HashObjectUID.Sign"])

    def test_an_unclosed_parameter_list_runs_to_the_arrow(self):
        (sig,) = X.sig_blocks(items("CredentialObjectUID.GetPackage [", "Log = bytes", "=>", "[ Result : package ]", "This parameter defines it."))
        self.assertEqual(sig["text"].splitlines()[-1], "[ Result : package ]")

    def test_one_paragraph_heads_and_two_blocks_in_a_row(self):
        sigs = X.sig_blocks(items("ThisSP.DeleteSP[]", "=>", "[ ]") + items("ThisSP.GetFreeSpace [ ]", "=>", "[ FreeSpace : uinteger ]", start=5))
        self.assertEqual([s["heads"] for s in sigs], [["ThisSP.DeleteSP"], ["ThisSP.GetFreeSpace"]])

    def test_the_block_stops_at_a_paragraph_that_is_not_the_next_sibling(self):
        (sig,) = X.sig_blocks(items("ObjectUID.Delete[ ]", "=>") + [item(9, "[ ]"), item(10, "If invoked on an SP object.")])
        self.assertEqual(sig["text"], "ObjectUID.Delete[ ]\n=>")

    def test_no_block_without_a_bracketed_head(self):
        self.assertEqual(X.sig_blocks(items("CredentialObjectUID.Sign", "Invoking SMUID.Properties[ ] is allowed.")), [])
        self.assertEqual(X.sig_blocks([item(1, "TableUID.Get [", kind="LI")]), [])

    def test_every_spec_fails_a_block_in_no_numbered_section(self):
        ir = {"spec": "test-1.0", "labelOffset": 0, "tables": [], "headings": [], "lot": [], "xrefs": [],
              "sigs": [{"section": None, "page": 4, "heads": ["A.b"], "text": "A.b [ ]"}]}
        (name, ok, detail), = [r for r in X.invariants(ir, None) if "signature" in r[0]]
        self.assertEqual((name, ok, detail), ("every signature block lies in a numbered section", False, "none on PDF [5]"))

    def test_bracket_depth(self):
        self.assertEqual(X.bracket_depth("typeOr { Bytes : bytes, RowValues : list [ x ] } ]"), -1)


class FakeCache:
    """The two things xref_records asks of a PageCache: links and the page matrix."""
    def __init__(self, links):
        self._links = links

    def links(self, pno):
        return self._links.get(pno, [])

    def inv(self, pno):
        return pymupdf.Identity


class TableReferences(unittest.TestCase):
    def test_a_reference_with_its_sentence_rect_and_link(self):
        recs = glyphs("Seeabove.Valuesareset", gap_after={2, 8, 14, 17}) + glyphs("inTable179.", y=120.0, gap_after={1, 6})
        link = (pymupdf.Rect(140.0, 110.0, 160.0, 125.0), 166)
        (x,) = X.xref_records(FakeCache({0: [link]}), X.para_chars(recs), "5.1.3.55")
        self.assertEqual((x["number"], x["page"], x["section"], x["target"]), (179, 0, "5.1.3.55", 166))
        self.assertEqual(x["sentence"], "Values are set in Table 179.")
        self.assertEqual(x["rect"], [115.0, 112.0, 160.0, 122.0])

    def test_no_link_over_the_number_is_no_target(self):
        (x,) = X.xref_records(FakeCache({}), X.para_chars(glyphs("Table03", gap_after={4})), None)
        self.assertEqual((x["number"], x["target"]), (3, None))

    def test_sentences(self):
        text = "Hi there. The values are in Table 22 and Table 23. (See 3.2.) Done"
        self.assertEqual(X.sentence_at(text, text.index("Table 23")), "The values are in Table 22 and Table 23.")
        self.assertEqual(X.sentence_at(text, 0), "Hi there.")
        self.assertEqual(X.sentence_at("e.g. a value in Table 5.", 16), "e.g. a value in Table 5.")


class ListOfTables(unittest.TestCase):
    def test_entries(self):
        self.assertEqual(X.parse_lot_entry("Table 01 Global Terminology ................... 2"), (1, "Global Terminology", 2))
        self.assertEqual(X.parse_lot_entry("Table 09 0-Length Byte Encoding .......... 17"), (9, "0-Length Byte Encoding", 17))
        self.assertEqual(X.parse_lot_entry("Table 161 uinteger_8 ................ 109"), (161, "uinteger_8", 109))
        self.assertEqual(X.parse_lot_entry("Table 184 C_AES_128 Table Description..........158"), (184, "C_AES_128 Table Description", 158))
        self.assertEqual(X.parse_lot_entry("Table 26 - Admin SP – TPerInfo Columns ........ 48"), (26, "Admin SP – TPerInfo Columns", 48))

    def test_a_wrapped_title(self):
        self.assertEqual(X.parse_lot_entry("Table 185 C_AES_128/C_AES_256 ResidualData Column Values After "
                                           "Encrypt/Decrypt/EncryptFinalize/DecryptFinalize ......... 159"),
                         (185, "C_AES_128/C_AES_256 ResidualData Column Values After Encrypt/Decrypt/EncryptFinalize/DecryptFinalize", 159))

    def test_not_entries(self):
        self.assertIsNone(X.parse_lot_entry("Figure 1 Diagram of the Core Architecture ...... 5"))
        self.assertIsNone(X.parse_lot_entry("Tables"))
        self.assertIsNone(X.parse_lot_entry("Table 03"))

def grid_table(path, first_row):
    return {"struct_path": path, "header_rows": [[{"text": t} for t in first_row]], "rows": []}


GRID_ROW = ["Bit Byte"] + [str(b) for b in range(7, -1, -1)]


class ByteGrids(unittest.TestCase):
    def test_a_first_row_of_a_corner_then_seven_to_zero(self):
        self.assertTrue(X.is_byte_grid(grid_table("0/1", GRID_ROW)))
        self.assertTrue(X.is_byte_grid({"header_rows": [], "rows": [[{"text": t} for t in ["Bit"] + GRID_ROW[1:]]]}))   # Core T17: no header
        self.assertFalse(X.is_byte_grid(grid_table("0/1", ["Byte", "Field", "Value"])))
        self.assertFalse(X.is_byte_grid(grid_table("0/1", GRID_ROW[:-1] + ["1"])))
        self.assertFalse(X.is_byte_grid({"header_rows": [], "rows": []}))

    def test_cells_zipped_with_the_drawn_cells_of_their_row(self):
        # Core T42 byte 0: '0' | '(MSB)' | 'Feature Code' | '' under four drawn cells; the empty one gets its own
        drawn = {"r": [[73.0, 127.0], [127.0, 182.0], [182.0, 510.0], [510.0, 566.0]]}
        self.assertEqual(X.row_extents([("r", 0), ("r", 1), ("r", 2), None], drawn), drawn["r"])

    def test_cells_in_one_drawn_cell_each_take_their_own(self):
        # Core T17 byte 0: '(MSB)' and 'Reserved' both lie in the one merged drawn cell of the row
        drawn = {"r": [[144.0, 209.0], [209.0, 495.0]]}
        self.assertEqual(X.row_extents([("r", 0), ("r", 1), ("r", 1), None], drawn),
                         [[144.0, 209.0], [209.0, 495.0], [209.0, 495.0], None])

    def test_cells_over_two_drawn_rows_take_their_own(self):
        drawn = {"a": [[0.0, 10.0], [10.0, 20.0]], "b": [[0.0, 10.0], [10.0, 20.0]]}
        self.assertEqual(X.row_extents([("a", 0), ("b", 1)], drawn), [[0.0, 10.0], [10.0, 20.0]])
        self.assertEqual(X.row_extents([None, None], {}), [None, None])


class WordComplete(unittest.TestCase):
    HEADINGS = [{"section": "3.3.6", "struct_path": "0/10"}, {"section": "3.3.6.4", "struct_path": "0/20"},
                {"section": None, "struct_path": "0/25"}, {"section": "3.3.6.4.1", "struct_path": "0/30"},
                {"section": "3.3.6.5", "struct_path": "0/40"}, {"section": "3.3.7", "struct_path": "0/50"}]

    def test_the_section_of_a_table_is_that_of_the_numbered_heading_before_it(self):
        tables = [grid_table("0/5", GRID_ROW), grid_table("0/21", GRID_ROW), grid_table("0/26", GRID_ROW), grid_table("0/100", GRID_ROW)]
        self.assertEqual(X.table_sections(self.HEADINGS, tables), {"0/5": None, "0/21": "3.3.6.4", "0/26": "3.3.6.4", "0/100": "3.3.7"})

    def test_the_section_of_a_byte_grid_and_its_subsections(self):
        tables = [grid_table("0/21", GRID_ROW), grid_table("0/45", ["Byte", "Field", "Value"])]
        self.assertEqual(X.word_complete_sections(self.HEADINGS, tables), ["3.3.6.4", "3.3.6.4.1"])

    def test_words_on_the_paragraphs_of_those_sections_only(self):
        paras = [{"section": "3.3.6.4", "_words": ["The", "Length", "field"]}, {"section": "3.3.7", "_words": ["x"]}]
        X.set_words(paras, ["3.3.6.4"])
        self.assertEqual(paras, [{"section": "3.3.6.4", "words": ["The", "Length", "field"]}, {"section": "3.3.7", "words": None}])


if __name__ == "__main__":
    unittest.main()
