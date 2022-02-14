#!/bin/csh

set SCENARIO    = $argv[1]

source ../../../config/seglists/allBay.land

foreach SEG ($segments)
   #sbatch -pdebug run.csh $seg
   sbatch -pdegub run_lugx.csh $SCENARIO $SEG
end
