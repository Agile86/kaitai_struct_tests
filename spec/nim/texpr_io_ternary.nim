# Autogenerated from KST: please remove this line if doing any edits by hand!

import os, streams, options, sequtils
import ../../compiled/nim/expr_io_ternary
import auxiliary/test_utils

let r = ExprIoTernary.fromFile("../../src/term_strz.bin")

assert r.oneOrTwoIoSize1 == 8
assert r.oneOrTwoIoSize2 == 8
assert r.oneOrTwoIoSizeAdd3 == 11
