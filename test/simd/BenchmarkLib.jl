#==
#    This file is 
#    Copyright (C) Alain Lichnewsky, 2014, 2015 
# 
#    Licensed under the MIT License  ( http://opensource.org/licenses/MIT ).
==#

module BenchmarkLib

using CPUTime

export    TimeIter,
          Analysis,
          Model,
          @timeIter,
          calibrate, analyze,   linearFit,
          showMat, fmtMat, fmt,
          doPlot, doPlotNorm, doPlot2Cols
immutable TimeIter
   niter      :: Int64  #iterations of measured kernel
   elapsed    :: Int64  #elapsed time in ns as  returned by Base.time_ns() =>jl_hrtime
   cputime    :: Int64  #cpu time  in microseconds       as returned by CPUTime.CPUtime_us()
   allocBytes :: Int64  #allocated bytes,                with (GCD)(.total_allocd + .allocd) 
   gcTime     :: Int64  #time used for GC,               with(GCD)(.total_time)
   allocs     :: Int64  #numberof allocations            with (GCD)(.malloc + .realloc + .poolalloc)

   ## in comments above GCD refers to Base.GC_Diff fields

   function TimeIter(ti::TimeIter)
       new(ti.niter, ni. elapsed, ni.cputime, ni.allocBytes, ni.gcTime,ni.allocs )
   end
   

   function TimeIter(niter, elapsed, cputime, allocBytes, gcTime, allocs )
       new(niter, elapsed, cputime,allocBytes, gcTime, allocs )
   end

end


type  Analysis
  data       ::Array{Float64,2}
  excldata   ::Array{Float64,2} # data eliminated in outlier detect
  colLabels  ::Array{String,1}
  linFitData ::Array{Float64,2}
  linFitModels::Array{Symbol,1}

  function Analysis{S<:String}( data     :: Array{Float64,2},
                     excldata :: Array{Float64,2}, 
                     labs     :: Array{S,1})
      new( data, excldata, labs, Array{Float64,2}(0,0), Array{Symbol,1}(0))
  end

  function Analysis{S<:String}( data     :: Array{Float64,2},
                     labs     :: Array{S,1})
      new( data, Array{Float64,2}(0,0), labs, Array{Float64,2}(0,0),
                 Array{Symbol,1}(0))
  end
end


# This is derived from the macro time in Base.Utils
# The expression ex must not have side effects, it will be
# run multiple times. No value is returned (hopefully the compiler
# will not eliminate the expression (dead code))
macro timeIter(ex,iter)
    quote
        local v0   = $(esc(ex))
        v0 = nothing 

        local stats = $(esc(Base.gc_num))()
        local cputime = $(esc(CPUTime.CPUtime_us))()
        local elapsedtime = $(esc(Base.time_ns))()
        local i::Int
        for i in 1:$(esc(iter))
            $(esc(ex))
        end
        elapsedtime   = $(esc(Base.time_ns))() - elapsedtime
        cputime       =  $(esc(CPUTime.CPUtime_us))() - cputime
        local diff    = $(esc(Base.GC_Diff))(Base.gc_num(), stats)
        local bytes   = diff.total_allocd + diff.allocd
        local allocs  = diff.malloc + diff.realloc + diff.poolalloc
        TimeIter($(esc(iter)), Int(elapsedtime), Int(cputime), bytes, diff.total_time, allocs)
    end
end
function analyze(b :: Array{TimeIter,1})
     data = hcat ( map( x -> x.niter, b),
            map( x -> x.elapsed/x.niter, b),
            map( x -> x.cputime/x.niter*1000, b), # time unit is ns
            map( x -> x.allocBytes/x.niter, b),
            map( x -> x.gcTime/x.niter, b),
            map( x -> x.allocs/x.niter, b),
            map( x -> x.elapsed, b),
            map( x -> x.cputime*1000, b), # time unit is ns
            map( x -> x.allocBytes, b),
            map( x -> x.gcTime, b),
            map( x -> x.allocs, b),
)
     #
     # To avoid issues with the initialization, we suppress the first 
     # 2 measurements (better outlier detection ** TBD ** )
     ret = Analysis( data[1:end,:], Array{Float64,2}(0,0),
               ["niter", "elapsed", "cputime", "allocBytes", "gctime", "allocs",
                "TotElapsed", "TotCputime", "TotAllocBytes", "TotGctime", "TotAllocs"])
     ret.linFitModels = [:total,
                         :periter,:periter,:periter,:periter,:periter,:periter,
                         :total,  :total,  :total,  :total,  :total]
     ret
end

# Helper functions (move eventually to lib)

showMat{T}(mat::Array{T}) = println(map(x -> @sprintf("%+7.3e",x), mat))

function fmtMat{T}(mat::Array{T})
           join ( [  join( map(x -> @sprintf("%+7.3e",x), mat[i,:]), "  ")
                     for i in 1:size(mat,1) ],
                  "\n")
       end
function fmtMat(mat::Analysis)
           str1= join( map(x -> (x*"          ")[1:10], mat.colLabels[:]), "  ")
           str2= fmtMat(mat.data)
           str1 * "\n" * str2
end

function fmtMat(mat::Analysis,lines::Array{Int,1})
           str1= join( map(x -> (x*"          ")[1:10], mat.colLabels[:]), "  ")
           str2= fmtMat(mat.data[lines,:])
           str1 * "\n" * str2
end
function fmt(ti::TimeIter)
   ar = [ ti.niter,
     ti.elapsed/ti.niter,
     ti.cputime/ti.niter*1000,  # time unit is ns
     ti.allocBytes/ti.niter,
     ti.gcTime/ti.niter,
     ti.allocs/ti.niter,
     ti.elapsed,
     ti.cputime*1000,	     # time unit is ns
     ti.allocBytes,
     ti.gcTime, 
     ti.allocs,
   ]
   join(ar,"  ")
end
#=
       BENCHMARK CALIBRATION
=#

function calibRepeat(_::Void)
     repeatVect =[2^i for i in 0:24]
     repeatVect = sort(vcat(repeatVect,repeatVect,repeatVect,rand(1:256,10),rand(2^10:2^24,10)))
end

function calibRepeat(rng::UnitRange{Int})
     from  =  round( Int64, log2(start(rng)))
     to    =  round( Int64, log2(rng[end]))+1
     repeatVect =[2^i for i in from:to]
     repeatVect = sort(vcat(repeatVect,repeatVect,repeatVect,rand(rng,length(repeatVect))))
end


function calibFn()
     1.0 * 1.0
end

#  Note: we should find a way to eliminate constants in repRange
#
function calibrate(fn::Function=calibFn; repRange::Union{UnitRange{Int},Void}=10^6:5*10^7)
    local repeatVect = calibRepeat(repRange)
    local nbSamples  = length(repeatVect)

    local calTb =  Array{TimeIter,1}(nbSamples)
    for i in 1:nbSamples
       calTb[i] = @timeIter( fn() , repeatVect[i])
    end
    local anCalibr = analyze(calTb)
    (anCalibr,calTb)
end

# Here we try to validate criteria for validation


#
#  We use these classes to select according to a variety of models
#
abstract Models
immutable Model{T} <: Models ; end
Model(s::Symbol) = Model{s}()
#
#   Build matrix corresponding to model

       #   We use a model of the dependency of the measured data
       #   wrt the number of iterations. Basically:
       #   data =  perIter + startup / nb_iters
function modelMatrix( x::Array{Float64,1},  _::Model{:periter})
       mat  = hcat( ones( x ),  map( x->1/x, x) )
end

modelReorder( x::Array{Float64,1},  _::Model{:periter}) =  x

       #   We use a model of the dependency of the measured data
       #   wrt the number of iterations. Basically:
       #   data =  perIter * nb_iters + startup 
function modelMatrix( x::Array{Float64,1},  _::Model{:total})
       mat  = hcat( ones( x ), x )
end

modelReorder( x::Array{Float64,1},  _::Model{:total}) =  x[[2,1]]

#
#   Linear fit based on a model, we will probably have multiple
#   such models, depending on the modeling situation
#
function linearFit( ana::Analysis, models::Array{Symbol,1};
                    lines = Array{Int,1}[])
   @assert length(ana.colLabels) == size(ana.data,2)
   retval = Array{Float64,2}(2,length(ana.colLabels))
   linesel = length(lines)   
   retval[:,1] = [ 1 0]
   for icol = 2:length(ana.colLabels)
       vals =  linesel == 0 ? ana.data[ : , icol] : ana.data[ lines , icol]
       iters = linesel == 0 ? ana.data[ : , 1]    : ana.data[ lines , 1]

       #   We use a model of the dependency of the measured data
       #   wrt the number of iterations. 

       mat  = modelMatrix(iters, Model(models[icol]))

       #   so that using matrix, we get data = mat * column(perIter, startup)
       #   construct a pseudo inverse of mat based on SVD
       s    = svd(mat)
       invdiag = map(x->1/x, s[2])
       pinv = (s[3]')  * diagm(invdiag) * (s[1]') 

       #   solve the least square problem using the pseudo inverse
       ab   =  pinv * vals
       #   if needed reorder the output
       ab   =  modelReorder( ab, Model(models[icol]))
       perIter  =  ab[1]
       startup  =  ab[2]
       # output
       colnm=ana.colLabels[icol]
       println("($colnm)\tstartup = ", @sprintf("%+7.3e",startup),
               "\t per Iter = ",       @sprintf("%+7.3e",perIter))
       retval[:, icol] = ab
   end
   ana.linFitData = retval
   retval
end


linearFit( ana::Analysis; lines = Array{Int,1}[]) = linearFit( 
           ana, ana.linFitModels, lines=lines)

# basic:returns a list of line segments corresponding to the fitted function
lineFit(ana::Analysis,col::Int) = lineFit( ana, col, Model(ana.linFitModels[col]) )

function lineFit(ana::Analysis,col::Int,_::Model{:total})
     println("In lineFit TOTAL")
      minx::Float64 = reduce( min, ana.data[:,1])
      maxx::Float64 = reduce( max, ana.data[:,1])
      a::Float64    = ana.linFitData[1,col]
      b::Float64    = ana.linFitData[2,col]

      return ([minx,maxx],[a*minx+b,a*maxx+b])
end
function lineFit(ana::Analysis,col::Int,_::Model{:periter})
     println("In lineFit PERITER")
      minx::Float64 = reduce( min, ana.data[:,1])
      maxx::Float64 = reduce( max, ana.data[:,1])
      miny::Float64 = reduce( min, ana.data[:,col])
      maxy::Float64 = reduce( max, ana.data[:,col])
      a::Float64    = ana.linFitData[1,col]
      b::Float64    = ana.linFitData[2,col]
      nseg = 20
      
      return ( [ minx + i*(maxx-minx)/nseg            for i in 1:nseg],
               [ a  + b/(minx + i*(maxx-minx)/nseg)   for i in 1:nseg])
end
# basic:returns a list of line segments corresponding to the fitted function
lineFit(ana::Analysis,col::Int) = lineFit( ana, col, Model(ana.linFitModels[col]) )

function lineFit(ana::Analysis,col::Int,_::Model{:totalnorm})
     println("In lineFit TOTALNORM")
      minx::Float64 = reduce( min, ana.data[:,1])
      maxx::Float64 = reduce( max, ana.data[:,1])
      a::Float64    = ana.linFitData[1,col]
      b::Float64    = ana.linFitData[2,col]
      nseg = 20

      return ( [ minx + i*(maxx-minx)/nseg            for i in 1:nseg],
               [ a  + b/(minx + i*(maxx-minx)/nseg)   for i in 1:nseg])
end
lineFit(ana::Analysis,col::Int,_::Model{:periternorm}) =  lineFit(ana,col,Model(:periter))

# ------------ VISUALIZE ------------
using Gadfly

function doPlot(ana::Analysis,col::Int,fname::String;
                     title::String = "",
                     attribs =  (Scale.x_log10, Scale.y_log10),
                     showfit::Bool=false)
    plt::Plot
    if showfit
        xline,yline = lineFit(ana,col)
        layer1 = layer( x = ana.data[:,1] ,  y = ana.data[:,col],Geom.point)
        layer2  = layer( x = xline, y = yline, Geom.line)
        legends = [Guide.XLabel(ana.colLabels[1]), 
               Guide.YLabel(ana.colLabels[col]),
               Guide.Title(title)]
        plt = plot([layer1,layer2], legends...)
    else
        plt = plot(x=ana.data[:,1] ,  y = ana.data[:,col],
               Guide.XLabel(ana.colLabels[1]), 
               Guide.YLabel(ana.colLabels[col]),
               Guide.title(title), attribs...   )
    end
    draw(SVGJS(fname,20cm,16cm),plt)
end

function doPlotNorm(ana::Analysis,col::Int,fname::String;
                     title::String = "",
                     attribs =  (Scale.x_log10, Scale.y_log10),
                     showfit::Bool=false)
    plt::Plot
    if showfit
        # we fix the model fitting function to take into account normalization to single iter
        # for the display ( irrespective of model used for fitting !!).
        modelfit = ana.linFitModels[col] == :periter ? :periternorm : :totalnorm
        xline,yline = lineFit(ana,col,Model(modelfit))

        plt=plot(  layer( x = ana.data[:,1] ,  y = ana.data[:,col] ./ ana.data[:,1],Geom.point),
                   layer( x = xline, y = yline, Geom.line),
                   Guide.XLabel(ana.colLabels[1]), 
                   Guide.YLabel(ana.colLabels[col]),
                   Guide.title(title), attribs...)
    else
        plt = plot(x=ana.data[:,1] ,  y = ana.data[:,col] ./ ana.data[:,1],
               Guide.XLabel(ana.colLabels[1]), 
               Guide.YLabel(ana.colLabels[col]),
               Guide.title(title), attribs...   )
    end
    draw(SVGJS(fname,20cm,16cm),plt)
end

function doPlot2Cols(ana::Analysis,
                     col1::Int, col2::Int,fname::String;
                     title::String = "",
                     attribs =  (Scale.x_log10, Scale.y_log10))
    plt = plot(x=ana.data[:,col1] ,  y = ana.data[:,col2],
               Guide.XLabel(ana.colLabels[col1]), 
               Guide.YLabel(ana.colLabels[col2]),
               Guide.title(title),   
               attribs...) 
    draw(SVGJS(fname,20cm,16cm),plt)
end

function doPlot2Cols{T}(tbl::Array{T,2},
                     col1::Int, col2::Int,
                     lab1::String, lab2::String,
                     fname::String;
                     title::String = "",
                     attribs =  (Scale.x_log10, Scale.y_log10))
    plt = plot( x=tbl[:,col1] ,  y = tbl[:,col2],
                Guide.XLabel(lab1), 
		Guide.YLabel(lab2),
		Guide.title(title), 
           	attribs...) 
    draw(SVGJS(fname,20cm,16cm),plt)
end


end # module BenchmarkLib

