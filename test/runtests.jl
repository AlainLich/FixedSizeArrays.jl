module FirstTest

using GeometryTypes
using FixedSizeArrays

const a = ["1.909", "1.909", "1.909"]
@show Vector3{Float64}(a)

end # module FirstTest

for f in [ "core.jl"  "ops.jl"   "test_matrix.jl"  "vcat.jl"]
   include(f)
end
