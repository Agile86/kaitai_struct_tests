# Autogenerated from KST: please remove this line if doing any edits by hand!

using Test
import CastNestedModule

@testset "CastNested test" begin
    r = CastNestedModule.from_file("src/switch_opcodes.bin")


    @test r.opcodes_0_str.value == "foobar"
    @test r.opcodes_0_str_value == "foobar"
    @test r.opcodes_1_int.value == 66
    @test r.opcodes_1_int_value == 66
end
