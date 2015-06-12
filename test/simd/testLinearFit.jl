#
#  Test the linear fit function and gain a better understanding of models
#  and precision.
#
push!(LOAD_PATH,joinpath(Pkg.dir(),"FixedSizeArrays","test","simd"))
using BenchmarkLib

# parameters
nbsamples = 50
kt = 200.0
st = 1000.0

# model
#      we measure n iterations of a kernel
#      1) observe totaltime = st + kt * n + error
#         data  :(n,totaltime)+
#         deduce  kt   :time per iteration
#                 st   : startup
#                 possibly characteristics of error
#
#      2) observe totaltime/n deduce same
#         data  :(n,totaltime/n)+
#

function bldSynthetic(nbsamples,kt,st)
    data      = Array{Float64,2}(nbsamples,5)
    data[:,1] = 1:nbsamples
    data[:,2] = st + data[:,1] * kt
    data[:,3] = data[:,2] ./ data[:,1]
    data[:,4] = data[:,2] + 30 * randn(nbsamples) 
    data[:,5] = data[:,4] ./ data[:,1]  
    labels    = ["niter", "totaltime", "itertime",
                 "totaltime+err", "itertime w.err" ]
    Analysis( data, Array{Float64,2}(0,0),  labels)
end

println("Building synthetic data for $nbsamples events")
println("\tModel kt=$kt, st=$st")
antest = bldSynthetic(nbsamples,kt,st)
println(fmtMat(antest))

fitModels = [ :total,
              :total, :periter, :total, :periter ]
linearFit(antest, fitModels)
println("Result of linear fit")
showMat(antest.linFitData)

