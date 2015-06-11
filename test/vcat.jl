module TestVcat

using FixedSizeArrays

immutable Vec3{T}
	x::T
	y::T
	z::T
end


Vec3{Float32}(1f0, 2f0, 3f0)
# Missing ctor  Vec3{Float32}[1f0, 2f0, 3f0]
warn("Likely a missing  ctor  Vec3{Float32}[1f0, 2f0, 3f0]")

warn("Removed 2 tests, do not know what is intended")
# What is intended here?
#=
Vec3{Float32}[
	1f0 2f0 3f0;
	1f0 2f0 3f0
]

Vec3{Float32}([
	1f0 2f0 3f0;
	1f0 2f0 3f0
])
=#

end #module TestVcat
