-- Autogenerated from KST: please remove this line if doing any edits by hand!

local luaunit = require("luaunit")

require("params_pass_usertype")

TestParamsPassUsertype = {}

function TestParamsPassUsertype:test_params_pass_usertype()
    local r = ParamsPassUsertype:from_file("src/position_in_seq.bin")

    luaunit.assertEquals(r.first.foo, 1)
    luaunit.assertEquals(r.one.buf, "\002")
end