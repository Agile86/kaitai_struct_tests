# Autogenerated from KST: please remove this line if doing any edits by hand!

using Test
import Enum0Module

@testset "Enum0 test" begin
    r = Enum0Module.from_file("src/enum_0.bin")


    @test r.pet_1 == Enum0Module.enum_0_animal__cat
    @test r.pet_2 == Enum0Module.enum_0_animal__chicken
end
