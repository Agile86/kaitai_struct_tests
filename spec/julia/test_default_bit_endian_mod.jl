# Autogenerated from KST: please remove this line if doing any edits by hand!

using Test
import DefaultBitEndianModModule

@testset "DefaultBitEndianMod test" begin
    r = DefaultBitEndianModModule.from_file("src/fixed_struct.bin")


    @test r.main.one == 336
    @test r.main.two == 8608
    @test r.main.nest.two == 11595
    @test r.main.nest_be.two == 12799
end