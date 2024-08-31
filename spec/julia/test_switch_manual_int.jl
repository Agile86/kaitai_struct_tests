# Autogenerated from KST: please remove this line if doing any edits by hand!

using Test
import SwitchManualIntModule

@testset "SwitchManualInt test" begin
    r = SwitchManualIntModule.from_file("src/switch_opcodes.bin")


    @test Base.size(r.opcodes, 1) == 4
    @test r.opcodes[1].code == 83
    @test r.opcodes[1].body.value == "foobar"
    @test r.opcodes[2].code == 73
    @test r.opcodes[2].body.value == 66
    @test r.opcodes[3].code == 73
    @test r.opcodes[3].body.value == 55
    @test r.opcodes[4].code == 83
    @test r.opcodes[4].body.value == ""
end
