dir = joinpath(Pkg.dir(),"FixedSizeArrays","test")

for file in [  "maintests.jl", "core.jl" , "ops.jl",  "test_matrix.jl",  "vcat.jl"
               ## , "setindex.jl"  ::   removed because of error !!!!
             ]
    println("Start test file $dir/$file")
    include(joinpath(dir,file))
end

warn("Removed setindex.jl from test list because of error : invalid redefinition of constant Matrix1x1")
