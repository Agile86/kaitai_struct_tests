# Autogenerated from KST: please remove this line if doing any edits by hand!

let r = NavRoot.fromFile("src" / "nav.bin")

check(r.header.qtyEntries == 2)
check(r.header.filenameLen == 8)
check(len(r.index.entries) == 2)
check(r.index.entries[0].filename == "FIRST___")
check(r.index.entries[1].filename == "SECOND__")