# Autogenerated from KST: please remove this line if doing any edits by hand!

using Test
import IfInstancesModule

@testset "IfInstances test" begin
    r = IfInstancesModule.from_file("src/fixed_struct.bin")


    @test r.never_happens === nothing
end
