#!/bin/csh

 if (${#argv} != 3) then
   echo '' 
   echo 'usage:  sum_all_basins.csh scenario year1 year2 '
   echo ' '
   exit
 endif

 set scen = $argv[1]
 set year1 = $argv[2]
 set year2 = $argv[3]
 set basins = (S watershed RIMP )


 foreach basin ($basins)
   sbatch summarize_output_aveann.csh $scen $basin $year1 $year2
 end
