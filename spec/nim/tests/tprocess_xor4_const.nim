# Autogenerated from KST: please remove this line if doing any edits by hand!

let r = ProcessXor4Const.fromFile("src" / "process_xor_4.bin")

check(r.key == @[-20, -69, -93, 20])
check(r.buf == @[102, 111, 111, 32, 98, 97, 114])