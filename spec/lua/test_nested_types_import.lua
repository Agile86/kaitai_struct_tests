-- Autogenerated from KST: please remove this line if doing any edits by hand!

local luaunit = require("luaunit")

require("nested_types_import")

TestNestedTypesImport = {}

function TestNestedTypesImport:test_nested_types_import()
    local r = NestedTypesImport:from_file("src/fixed_struct.bin")

    luaunit.assertEquals(r.a_cc.value_cc, 80)
    luaunit.assertEquals(r.a_c_d.value_d, 65)
    luaunit.assertEquals(r.b.value_b, 67)
    luaunit.assertEquals(r.b.a_cc.value_cc, 75)
    luaunit.assertEquals(r.b.a_c_d.value_d, 45)
    luaunit.assertNil(r.a_cc._parent)
    luaunit.assertNil(r.a_cc._root)
    luaunit.assertNil(r.a_c_d._parent)
    luaunit.assertNil(r.a_c_d._root)
    luaunit.assertNil(r.b._parent)
    luaunit.assertNil(r.b._root)
end