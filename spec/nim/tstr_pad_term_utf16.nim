# Autogenerated from KST: please remove this line if doing any edits by hand!

import os, streams, options, sequtils
import ../../compiled/nim/str_pad_term_utf16
import auxiliary/test_utils

let r = StrPadTermUtf16.fromFile("../../src/str_pad_term_utf16.bin")

assert r.strTerm == "a\u0200b"
assert r.strTermInclude == "c\u0200d\000"
assert r.strTermAndPad == "e\u0200f"
