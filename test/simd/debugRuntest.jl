module MeasureMulAdd

using FixedSizeArrays
using FixedSizeArrays.MacrosCodeTest

push!(LOAD_PATH,joinpath(Pkg.dir(),"FixedSizeArrays","test","simd"))
using BenchmarkLib

using Base.Test

#= Test with matrices
=#
fm = FixedMatrix{Float64, 4,4}(rand(4,4))
ma = fm*fm
mb = fm+fm
mc = mb * mb - 4 * ma

@test sum(abs(mc)) == 0
@test sum(mc * mc') == 0

# kept from original test:
r = rand(4,4)
a= FixedArray{Float64,2,Tuple{4,4}}(r)
b = nvec((4,4),r...)


function simdMul!{T<:Real}(
                 c::FixedSizeArrays.Matrix4x4{T},
                 a::FixedSizeArrays.Matrix4x4{T},
                 b::FixedSizeArrays.Matrix4x4{T})
    @assert size(a,2) == size(b,1)
    nlig = size(a,1)
    ncol = size(b,2)
    nmidl= size(a,2)

    tmp = zeros(T,nlig,ncol)
    for j in 1:ncol
       for k in 1:nmidl
           @simd for i in 1:nlig
              @inbounds tmp[i,j] = tmp[i,j] + a[i,k] * b[k,j]
           end  
        end
    end
    c = tmp
    return nothing
end

function simdAdd!{T<:Real}(
                          c::FixedSizeArrays.Matrix4x4{T},
                          a::FixedSizeArrays.Matrix4x4{T},
                          b::FixedSizeArrays.Matrix4x4{T})
    @assert size(a) == size(b)
    nlig = size(a,1)
    ncol = size(a,2)

    tmp = zeros(T,nlig,ncol)
    for j in 1:ncol
           @simd for i in 1:nlig
              @inbounds tmp[i,j] = a[i,j] + b[i,j]
           end  
    end
    c = tmp
    return nothing
end


tmp = FixedMatrix{Float64, 4,4}(zeros(4,4))

simdMul!(tmp,fm,fm)
@show tmp
@show tmp - fm * fm
## is a FSA mutable?
## @assert tmp == fm * fm


simdAdd!(tmp,fm,fm)
@show tmp
@show tmp - (fm + fm)
## is a FSA mutable?
## @assert tmp == ( fm + fm )

#=

@file_native("/tmp/FixedASimd_Mult3.asm",
             simdMul!,
             ( FixedSizeArrays.Matrix4x4{Float64},
               FixedSizeArrays.Matrix4x4{Float64},
               FixedSizeArrays.Matrix4x4{Float64}))


@file_native("/tmp/FixedASimd_Add3.asm",
             simdAdd!,
             ( FixedSizeArrays.Matrix4x4{Float64},
               FixedSizeArrays.Matrix4x4{Float64},
               FixedSizeArrays.Matrix4x4{Float64}))

=#


end # module MeasureMulAdd
