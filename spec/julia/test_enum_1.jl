# Autogenerated from KST: please remove this line if doing any edits by hand!

using Test
import Enum1Module

@testset "Enum1 test" begin
    r = Enum1Module.from_file("src/enum_0.bin")


    @test r.main.submain.pet_1 == Enum1Module.enum_1_main_obj_animal__cat
    @test r.main.submain.pet_2 == Enum1Module.enum_1_main_obj_animal__chicken
end
