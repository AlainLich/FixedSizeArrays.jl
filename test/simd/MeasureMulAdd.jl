module MeasureMulAdd

using FixedSizeArrays
using FixedSizeArrays.MacrosCodeTest

push!(LOAD_PATH,joinpath(Pkg.dir(),"FixedSizeArrays","test","simd"))
using BenchmarkLib

using Base.Test

const FLPType     = Float64          # Float32 or Float64  (what happens with int?)
const doCodeDumps = false            # output code llvm and asm
const showfit     = true             # in graphics show fitted line
const outputDir   = joinpath(Pkg.dir(),"FixedSizeArrays","test","output")
const focusCalib  = true             # when false try all sorts of repetition counts
                              # when true, focus on usefull ranges (based on declarations)
#= Test with matrices
=#
fm = FixedMatrix{FLPType, 4,4}(rand(FLPType,4,4))
ma = fm*fm
mb = fm+fm
mc = mb * mb - 4 * ma

@test sum(abs(mc)) == 0
@test sum(mc * mc') == 0

# kept from original test:
r = rand(FLPType,4,4)
a= FixedArray{FLPType,2,Tuple{4,4}}(r)
b = nvec((4,4),r...)

# try a simd version of array operation
function simdMul{T<:Real}( a::Array{T,2}, b::Array{T,2})
    @assert size(a,2) == size(b,1)
    nlig = size(a,1)
    ncol = size(b,2)
    nmidl= size(a,2)
    c = zeros(T,nlig,ncol)
    
    for j in 1:ncol
       for k in 1:nmidl
           @simd for i in 1:nlig
              @inbounds c[i,j] = c[i,j] + a[i,k] * b[k,j]
           end  
        end
    end
    return c
end

function simdAdd{T<:Real}( a::Array{T,2}, b::Array{T,2})
    @assert size(a) == size(b)
    nlig = size(a,1)
    ncol = size(a,2)
    c = Array{T,2}(nlig,ncol)
    for j in 1:ncol
           @simd for i in 1:nlig
              @inbounds c[i,j] = a[i,j] + b[i,j]
           end  
    end
    return c
end

# go fixed  (not generic)
function simdMul{T<:Real}(
                 a::FixedSizeArrays.Matrix4x4{T},
                 b::FixedSizeArrays.Matrix4x4{T})
    @assert size(a,2) == size(b,1)
    nlig = size(a,1)
    ncol = size(b,2)
    nmidl= size(a,2)
    c = zeros(T,nlig,ncol)

    for j in 1:ncol
       for k in 1:nmidl
           @simd for i in 1:nlig
              @inbounds c[i,j] = c[i,j] + a[i,k] * b[k,j]
           end  
        end
    end
    return FixedMatrix{T, 4,4}(c)
end

function simdAdd{T<:Real}(
                          a::FixedSizeArrays.Matrix4x4{T},
                          b::FixedSizeArrays.Matrix4x4{T})
    @assert size(a) == size(b)
    nlig = size(a,1)
    ncol = size(a,2)
    c = Array{T,2}(nlig,ncol)
    for j in 1:ncol
           @simd for i in 1:nlig
              @inbounds c[i,j] = a[i,j] + b[i,j]
           end  
    end
    return  FixedMatrix{T, 4,4}(c)
end


# basic check against the existing libraries
@assert  r*r == simdMul(r,r)
@assert  r+r == simdAdd(r,r)
@assert simdMul(fm,fm) == fm * fm
@assert simdAdd(fm,fm) == fm + fm

# hide all expressions in functions
FSAMul(a::FixedSizeArrays.Matrix4x4{FLPType}, b::FixedSizeArrays.Matrix4x4{FLPType}) = a * b
FSAAdd(a::FixedSizeArrays.Matrix4x4{FLPType}, b::FixedSizeArrays.Matrix4x4{FLPType}) = a + b
ArrMul(a::Array{FLPType,2}, b::Array{FLPType,2}) = a * b 
ArrAdd(a::Array{FLPType,2}, b::Array{FLPType,2}) = a + b 

prec= FLPType == Float32 ? "32" : "64"

if doCodeDumps
  
  @file_native(outputDir * "/FSA_Mult$(prec).asm",
              FSAMul,
             ( FixedSizeArrays.Matrix4x4{FLPType},
              FixedSizeArrays.Matrix4x4{FLPType} ))


  @file_llvm(outputDir * "/FSA_Mult$(prec).llvm",
             FSAMul,
             ( FixedSizeArrays.Matrix4x4{FLPType},
              FixedSizeArrays.Matrix4x4{FLPType} ))

  @file_native(outputDir * "/FSA_Add$(prec).asm",
             FSAAdd,
             ( FixedSizeArrays.Matrix4x4{FLPType},
              FixedSizeArrays.Matrix4x4{FLPType} ))

  @file_native(outputDir * "/Array_Mult$(prec).asm",
            ArrAdd,
             ( Array{FLPType,2}, Array{FLPType,2} ))


  @file_native(outputDir * "/ArraySimd_Mult$(prec).asm",
             simdMul,
             ( Array{FLPType,2}, Array{FLPType,2} ))


  @file_native(outputDir * "/ArraySimd_Add$(prec).asm",
             simdAdd,
             ( Array{FLPType,2}, Array{FLPType,2} ))


  @file_native(outputDir * "/FixedASimd_Mult$(prec).asm",
             simdMul,
             ( FixedSizeArrays.Matrix4x4{FLPType},
               FixedSizeArrays.Matrix4x4{FLPType}))


  @file_native(outputDir * "/FixedASimd_Add$(prec).asm",
             simdAdd,
             ( FixedSizeArrays.Matrix4x4{FLPType},
               FixedSizeArrays.Matrix4x4{FLPType}))


  @file_llvm(outputDir * "/ArraySimd_Mult$(prec).llvm",
             simdMul,
             ( Array{FLPType,2}, Array{FLPType,2} ))


  @file_llvm(outputDir * "/ArraySimd_Add$(prec).llvm",
             simdAdd,
             ( Array{FLPType,2}, Array{FLPType,2} ))



  @file_llvm(outputDir * "/FixedASimd_Mult$(prec).llvm",
             simdMul,
             ( FixedSizeArrays.Matrix4x4{FLPType},
               FixedSizeArrays.Matrix4x4{FLPType}))


  @file_llvm(outputDir * "/FixedASimd_Add$(prec).llvm",
             simdAdd,
             ( FixedSizeArrays.Matrix4x4{FLPType},
               FixedSizeArrays.Matrix4x4{FLPType}))

end


#=
       BENCHMARK CALIBRATION
=#

(anCalibr,rawCalTable) = calibrate()

println("End calibration measurements++++++++++  ****\n")

# Now we generate the report(s) and figures

cal        = focusCalib ? "-FC" :""
redirIOS   = open("output/calib$(prec)$(cal).data","w")
oldSTDOUT  = STDOUT
redirect_stdout(redirIOS)


println("FLPType=$FLPType, doCodeDumps=$doCodeDumps, showfit=$showfit," *
         "focusCalib=$focusCalib")
versioninfo()
datePrint(STDOUT,"computed on:")
println("\n")

println("Raw calibration data")
println(fmtMat(anCalibr.excldata))
println(fmtMat(anCalibr))

lfCalibr = linearFit(anCalibr)
println("** Timer calibration data (Full set)**")
showMat(lfCalibr)


println("WARNING:the criteria are for investigating the reliable domain\n",
        "        (NOT FOR REAL USE)")



## Check for near equality of per iteration measures obtained by both methods
println("\nNear equality (+/- 5%) of elapsed/cputime")
linesF1 = filter( i-> abs((anCalibr.data[i,2] - anCalibr.data[i,3]) /
                          anCalibr.data[i,2]) <1/20,
                 1:size(anCalibr.data,1))
println(linesF1)
println(fmtMat( anCalibr, linesF1))
println("Linear fit of lines selected (case 1)")
lF1 = linearFit(anCalibr; lines=linesF1 )
showMat(lF1)

## Use as criterion sufficient elapsed time (seems weird in view of possible
## task preemption, kept here because elapse time resolution supposed to be ns)
println("\nSufficient total elapsed (?)")
linesF2 = filter( i-> anCalibr.data[i,7] > 1.0e6,
                 1:size(anCalibr.data,1))
println(linesF2)
println(fmtMat( anCalibr, linesF2))

println("Linear fit of lines selected (case 2)")
lF2 = linearFit(anCalibr; lines=linesF2 )
showMat(lF2)

## Use as criterion sufficient CPU time (recall that CPU time resol is order ms)
println("\nSufficient cputime (?)")
linesF3 = filter( i-> anCalibr.data[i,8] > 1.0e6,
                 1:size(anCalibr.data,1))
println(linesF3)
println(fmtMat( anCalibr, linesF3))
println("Linear fit of lines selected (case 3)")
lF3 = linearFit(anCalibr; lines=linesF3 )
showMat(lF3)


## Output LLVM + NATIVE CODE OF CALIBRATION RELATED FUNCTIONS

calibFn() = 1.0 * 1.0
function calibMacro()
    calib =  @timeIter( calibFn() , [1 2 3])
end

if doCodeDumps
   @file_native(outputDir * "/CalibrateTimeIter.asm",
             calibMacro,
             ( ))
   @file_llvm(outputDir * "/CalibrateTimeIter.llvm",
             calibMacro,
             ( ))

   @file_native(outputDir * "/CalibrationInnerFn.asm",
             calibFn,
             ( ))
   @file_llvm(outputDir * "/CalibrationInnerFn.llvm",
             calibFn,
             ( ))
end
    


println("**  END calibration **\n")

#=
       BENCHMARK PROPER
=#

# set repetitions in an adhoc manner some of the kernels are very costly
function repeatVect(_::Void)
     repeatVect =[2^i for i in 0:24]
     repeatVect = sort(vcat(repeatVect,repeatVect,repeatVect,rand(1:256,10),rand(2^10:2^24,10)))
end

function repeatVect(rng::UnitRange{Int})
     from  =  round( Int64, log2(start(rng)))
     to    =  round( Int64, log2(rng[end])) + 1
     rptv =[2^i for i in from:to]
     rptv = sort(vcat(rptv,rand(rng,length(rptv))))
end



################################### END New Repetition setup

## This macro performs the benchmark, and also generates the Analysis
## object. Recall that we use macro because expr is to be evaluated within
## the timed loop (@timeIter).
macro benchAna ( expr , rptv)
 quote
   local nbSamples = length($(esc(rptv)))
   local bench = $(esc(Array{TimeIter,1}))(nbSamples)
   for i in 1:nbSamples
       bench[i] = @timeIter( $(esc(expr)) , $(esc(rptv))[i])
   end
   local ab= $(esc(analyze))(bench)
   (bench,ab)
 end
end

bench1, ab1 = @benchAna(  FSAMul ( mb ,mb ),  repeatVect(10^3:10^4))
bench2, ab2 = @benchAna(  ArrMul ( r , r ),  repeatVect(10^3:10^6))
bench3, ab3 = @benchAna(  FSAAdd ( mb ,mb ),  repeatVect(10^3:10^5))
bench4, ab4 = @benchAna(  simdMul( r,  r  ) , repeatVect(10^3:10^6))
bench5, ab5 = @benchAna(  simdAdd( r,  r  ) , repeatVect(10^3:10^6))
bench6, ab6 = @benchAna(  simdMul( fm, fm ) , repeatVect(10^3:10^4))
bench7, ab7 = @benchAna(  simdAdd( fm, fm ) , repeatVect(10^3:10^4))

b1=  @timeIter(  FSAMul ( mb ,mb ) , 10^4)
println("Check with b1=", fmt(b1))
b2=  @timeIter( ArrMul ( r , r ) , 10^6)
println("Check with b2=", fmt(b2))
b3=  @timeIter( FSAAdd ( mb ,mb ), 10^5)
println("Check with b3=", fmt(b3))
b4=  @timeIter( simdMul( r,  r  ) , 10^6)
println("Check with b4=", fmt(b4))
b5=  @timeIter( simdAdd( r,  r  ) , 10^6)
println("Check with b5=", fmt(b5))
b6=  @timeIter( simdMul( fm,  fm  ) , 10^4)
println("Check with b6=", fmt(b6))
b7=  @timeIter( simdAdd( fm,  fm  ) , 10^4)
println("Check with b7=", fmt(b7))

println("\nFSA *")
lf1= linearFit(ab1)
showMat(lf1)

println("\nFSA +")
lf3= linearFit(ab3)
showMat(lf3)

println("\nArray *")
lf2= linearFit(ab2)
showMat(lf2)

println("\nArray Simd*")
lf4= linearFit(ab4)
showMat(lf4)

println("\nArray Simd+")
lf5= linearFit(ab5)
showMat(lf5)

println("\nFSA Simd*")
lf6= linearFit(ab6)
showMat(lf6)

println("\nFSA Simd+")
lf7= linearFit(ab7)
showMat(lf7)

redirect_stdout(oldSTDOUT)

## Now do the plots
using Gadfly
prec= FLPType == Float32 ? "32" : "64"
fit = showfit ? "F" :""

for total in [false,true]
   fmt  = total ? "T" : ""
   tot  = total ? "(Total)" : ""
   offt = total ? 5 : 0
   plotFn = total ? doPlotNorm : doPlot 
   doPlot2Cols(anCalibr,2+offt,3,outputDir * "/CalibrEvsC$(fit)$(fmt).svg")
   plotFn(anCalibr,2+offt,outputDir * "/CalibrETime$(fit)$(fmt).svg",
          title = "Calibration kernel $(tot) Elapsed time FLP$(prec)",
          showfit=showfit)
   plotFn(anCalibr,3+offt,outputDir * "/CalibrCPUTime$(fit)$(fmt).svg",
          title = "Calibration kernel  $(tot) CPU time FLP$(prec)",
          showfit=showfit)
   plotFn(anCalibr,4+offt,outputDir * "/CalibrGCTime$(fit)$(fmt).svg",
          title = "Calibration kernel  $(tot) GC time FLP$(prec)",
          showfit=showfit)
   plotFn(anCalibr,5+offt,outputDir * "/CalibrAllocs$(fit)$(fmt).svg",
          title = "Calibration kernel  $(tot) number of allocs FLP$(prec)",
          showfit=showfit)

   plotFn(ab1,3+offt,outputDir * "/FSAMultCpuTimeFlp$(prec)$(fit)$(fmt).svg",
          title = "FSA Multiply 4x4 CPU  $(tot) time FLP$(prec)",
          showfit=showfit)
   plotFn(ab2,3+offt,outputDir * "/ArrayMultCPUTimeFlp$(prec)$(fit)$(fmt).svg",
          title = "Array Multiply 4x4 CPU  $(tot) time FLP$(prec)",
          showfit=showfit)
   plotFn(ab3,3+offt,outputDir * "/FSAAddCPUTimeFlp$(prec)$(fit)$(fmt).svg",
          title = "FSA Add 4x4 CPU  $(tot) time FLP$(prec)",
          showfit=showfit)
   plotFn(ab4,3+offt,outputDir * "/ArraySimdMultCPUTimeFlp$(prec)$(fit)$(fmt).svg",
          title = "Array Mult (SIMD)  4x4 CPU  $(tot) time FLP$(prec)",
          showfit=showfit)
   plotFn(ab5,3+offt,outputDir * "/ArraySimdAddTCPUimeFlp$(prec)$(fit)$(fmt).svg",
          title = "Array Add (SIMD) 4x4 CPU  $(tot) time FLP$(prec)",
          showfit=showfit)

   plotFn(ab1,2+offt,outputDir * "/FSAMultETimeFlp$(prec)$(fit)$(fmt).svg",
          title = "FSA Multiply 4x4 Elapsed  $(tot) time FLP$(prec)",
          showfit=showfit, attribs=())
   plotFn(ab2,2+offt,outputDir * "/ArrayMultETimeFlp$(prec)$(fit)$(fmt).svg",
          title = "Array Multiply 4x4 Elapsed  $(tot) time FLP$(prec)",
          showfit=showfit, attribs=())
   plotFn(ab3,2+offt,outputDir * "/FSAAddETimeFlp$(prec)$(fit)$(fmt).svg",
          title = "FSA Add 4x4 Elapsed  $(tot) time FLP$(prec)",
          showfit=showfit, attribs=())
   plotFn(ab4,2+offt,outputDir * "/ArraySimdMultETimeFlp$(prec)$(fit)$(fmt).svg",
          title = "Array Mult (SIMD)  4x4 Elapsed  $(tot) time FLP$(prec)",
          showfit=showfit, attribs=())
   plotFn(ab5,2+offt,outputDir * "/ArraySimdAddETimeFlp$(prec)$(fit)$(fmt).svg",
          title = "Array Add (SIMD) 4x4 Elapsed  $(tot) time FLP$(prec)",
          showfit=showfit, attribs=())
end


# Plots corresponding to calibration data
doPlot2Cols(anCalibr.data[linesF1,:],2,3,
            anCalibr.colLabels[2],anCalibr.colLabels[3],
            outputDir * "/CalibrEQCritRR$(fit)$(fmt).svg",
        title="Elapsed vs CPUTime (per iteration, select= elapse +-5% cputime )",
            attribs=())

doPlot2Cols(anCalibr.data[linesF2,:],2,3,
            anCalibr.colLabels[2],anCalibr.colLabels[3],
            outputDir * "/CalibrELAPSCritRR$(fit)$(fmt).svg",
        title="Elapsed vs CPUTime (per iteration, select=total elapse >1.e6 ns)",
            attribs=())

doPlot2Cols(anCalibr.data[linesF3,:],2,3,
            anCalibr.colLabels[2],anCalibr.colLabels[3],
            outputDir * "/CalibrCPUCritRR$(fit)$(fmt).svg",
        title="Elapsed vs CPUTime (per iteration, select= total CPU time > 1.e6 ns)",
            attribs=())

doPlot2Cols(anCalibr.data[linesF2,:],2,7,
            anCalibr.colLabels[2],anCalibr.colLabels[7],
            outputDir * "/CalibrELAPSCrit2.svg",
            title="Elapsed (per iter) vs total Elapse , select= total elapse >1.e6 ns)",
            attribs=(Scale.y_log10,))    #use default for Scale.x_

doPlot2Cols(anCalibr.data[linesF2,:],3,7,
            anCalibr.colLabels[3],anCalibr.colLabels[7],
            outputDir * "/CalibrELAPSCrit3.svg",
        title="CPU time(per iter) vs total Elapse , select= totatl elapse > 1.e6 ns)",
            attribs=(Scale.y_log10,))    #use default for Scale.x_
    
end # module MeasureMulAdd
