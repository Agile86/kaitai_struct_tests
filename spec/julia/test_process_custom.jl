# Autogenerated from KST: please remove this line if doing any edits by hand!

using Test
import ProcessCustomModule

@testset "ProcessCustom test" begin
    r = ProcessCustomModule.from_file("src/process_rotate.bin")


    @test r.buf1 == Vector{UInt8}([16, 179, 148, 148, 244])
    @test r.buf2 == Vector{UInt8}([95, 186, 123, 147, 99, 35, 95])
    @test r.buf3 == Vector{UInt8}([41, 51, 177, 56, 177])
end
