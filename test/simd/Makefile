# This makefile is essentially made for the development of the simd branch
# Do not expect it to work on your machine

DEVELORIGIN=/home/alain/test/BigData
DEVTRY=$(DEVELORIGIN)/JULIA-TRY
JULIAHOME=/home/alain/julia.d/v0.4/


SIMDSRC=$(JULIAHOME)/FixedSizeArrays/src/simd
SIMDTEST=$(JULIAHOME)/FixedSizeArrays/test/simd

.PHONY: import

import:
	cat $(DEVTRY)/MacrosCodeTest.jl |cpif  $(SIMDSRC)/MacrosCodeTest.jl
	cat $(DEVTRY)/BenchmarkLib.jl   |cpif  $(SIMDTEST)/BenchmarkLib.jl
