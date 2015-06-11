
module DebugRunTest

using FixedSizeArrays
using Base.Test

#= Test with matrices
=#
fm = FixedMatrix{Float64, 4,4}(rand(4,4))
ma = fm*fm
mb = fm+fm
mc = mb * mb - 4 * ma

@test sum(abs(mc)) == 0
@test sum(mc * mc') == 0

#= original test
=#
r = rand(4,4)

@show("Welcome (1)")

a= FixedArray{Float64,2,Tuple{4,4}}(r)
b = nvec((4,4),r...)



end # module DebugRunTest
