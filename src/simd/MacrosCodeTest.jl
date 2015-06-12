#==
#    This file is 
#    Copyright (C) Alain Lichnewsky, 2014, 2015 
# 
#    Licensed under the MIT License  ( http://opensource.org/licenses/MIT ).
==#

module MacrosCodeTest

export 
       @file_native, @file_llvm, 
       datePrint


import Base.Dates: now, DateFormat, format

function argPrint(ios,args)
   i = 0
   for a in args
      i+=1
      println(ios,"\t\t\t[$i]=  ",a)
   end
end

function datePrint(ios, msg)
   println(ios,"\msg", Dates.format(now(),Dates.DateFormat("dd/mm/yyyy HH:MM")))
end


@doc """  This function assumes that the function fn passed in first arg
         will accept an iostream as its first arg, followed by args...
         It returns an array containing the lines output to a string IOBuffer
""" ->
function eval_str(fn,args)
        iob = IOBuffer(true,true)
        fn(iob,args...)
        split(ASCIIString(iob.data),"\n")
end
macro file_native(fname,fn,args)
     return  quote
          ios = $(esc(open))($(esc(fname)),"w")
          $(esc(println))(ios,"**code_native for function=\"",$(esc(fn)),
                      "\"\n\t\twith args:")
          argPrint(ios,$(esc(args)))
          datePrint(ios,"\t\tprinted ")
          $(esc(code_native))(ios, $(esc(fn)), $(esc(args)))
          $(esc(println))(ios,"++++\t++++\n")
          for l in  eval_str($(esc(versioninfo)),())
	      $(esc(println))(ios, "\t** $l")
          end
          $(esc(close))(ios)
     end
end
macro file_llvm(fname,fn,args)
     return  quote
          ios = $(esc(open))($(esc(fname)),"w")
          $(esc(println))(ios,"**code_llvm for function=\"",$(esc(fn)),
                      "\"\n\t\twith args:")
          argPrint(ios,$(esc(args)))
          datePrint(ios,"\t\tprinted ")
          
          $(esc(code_llvm))(ios, $(esc(fn)), $(esc(args)))
          $(esc(println))(ios,"++++\t++++\n")
          for l in  eval_str($(esc(versioninfo)),())
	      $(esc(println))(ios, "\t** $l")
          end
          $(esc(close))(ios)
     end
end

end # module MacrosCodeTest
