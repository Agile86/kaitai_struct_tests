# Autogenerated from KST: please remove this line if doing any edits by hand!

using Test
import StrEncodingsDefaultModule

@testset "StrEncodingsDefault test" begin
    r = StrEncodingsDefaultModule.from_file("src/str_encodings.bin")


    @test r.str1 == "Some ASCII"
    @test r.rest.str2 == "\u3053\u3093\u306b\u3061\u306f"
    @test r.rest.str3 == "\u3053\u3093\u306b\u3061\u306f"
    @test r.rest.str4 == "\u2591\u2592\u2593"
end
