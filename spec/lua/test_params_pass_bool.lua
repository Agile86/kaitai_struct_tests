-- Autogenerated from KST: please remove this line if doing any edits by hand!

local luaunit = require("luaunit")

require("params_pass_bool")

TestParamsPassBool = {}

function TestParamsPassBool:test_params_pass_bool()
    local r = ParamsPassBool:from_file("src/term_strz.bin")

    luaunit.assertEquals(r.s_false, 0)
    luaunit.assertEquals(r.s_true, 1)
    luaunit.assertEquals(r.seq_b1.arg, 1)
    luaunit.assertEquals(#r.seq_b1.foo, 1)
    luaunit.assertEquals(r.seq_bool.arg, false)
    luaunit.assertEquals(#r.seq_bool.foo, 2)
    luaunit.assertEquals(r.literal_b1.arg, 0)
    luaunit.assertEquals(#r.literal_b1.foo, 2)
    luaunit.assertEquals(r.literal_bool.arg, true)
    luaunit.assertEquals(#r.literal_bool.foo, 1)
    luaunit.assertEquals(r.inst_b1.arg, 1)
    luaunit.assertEquals(#r.inst_b1.foo, 1)
    luaunit.assertEquals(r.inst_bool.arg, false)
    luaunit.assertEquals(#r.inst_bool.foo, 2)
end
