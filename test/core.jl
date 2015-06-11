module TestCore

using FixedSizeArrays

r = rand(4,4)
b = nvec((4,4), r...)
println(b)

end # module TestCore
