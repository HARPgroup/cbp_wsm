#!/bin/csh

set SCENARIO    = $argv[1]

#set iPWD = `pwd`
#cd  ../../../run/standard/
#csh make_land_directories.csh $SCENARIO
#cd  $iPWD

source ../../../config/seglists/allBay.land

foreach SEG ($segments)
   #sbatch -pdebug run.csh $seg
   sbatch run_lugx.csh $SCENARIO $SEG
end
