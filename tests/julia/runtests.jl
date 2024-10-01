using Test
using YggInterface

external_testlib = Vector{String}()
dynamic_testlib_dir = ""
for x in ARGS
  println(x)
  if (startswith(x, "--dynamic-testlib-dir="))
    global dynamic_testlib_dir = split("--dynamic-testlib-dir=")[1]
  elseif (startswith(x, "--external-testlib="))
    push!(external_testlib, split(x, "--external-testlib=")[1])
  end
end

@testset "Document Tests $x" for x in ["hello", 1, 3.0, Core.UInt64(2),
                                       Core.Int64(5), Int64(5),
                                       ["hello", 1, 3.0, Core.UInt64(2)]]
  doc = Document(x)
  @test extract(doc) == x
end

@testset "Function Tests" begin
end
