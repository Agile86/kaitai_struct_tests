# Autogenerated from KST: please remove this line if doing any edits by hand!

import os, streams, options, sequtils
import ../../compiled/nim/switch_else_only
import auxiliary/test_utils

let r = SwitchElseOnly.fromFile("../../src/switch_opcodes.bin")

assert r.opcode == 83
assert r.primByte == 102
assert r.ut.value == @[114'u8, 0'u8, 73'u8, 66'u8]