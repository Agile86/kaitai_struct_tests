# Autogenerated from KST: please remove this line if doing any edits by hand!

using Test
import ParamsPassStructModule

@testset "ParamsPassStruct test" begin
    r = ParamsPassStructModule.from_file("src/enum_negative.bin")


    @test r.first.foo == 255
    @test r.one.bar.qux == 1
    @test r.one.foo.foo == 255
    @test r.one.bar.foo.foo == 255
end
