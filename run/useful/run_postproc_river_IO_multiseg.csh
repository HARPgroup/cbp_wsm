#!/bin/csh

set SCENARIO     = $argv[1]
set BASIN        = $argv[2]
set TIMEINTERVAL = $argv[3]
set IOFlag       = $argv[4]

source ../../config/seglists/$BASIN.riv

@ i = 1
foreach SEGMENT ( $segments ) 

     #srun --nodes=1 --ntasks=1 --exclusive --job-name=RIV-IO-$i run_postproc_river_IO_oneseg.csh $SCENARIO $SEGMENT $TIMEINTERVAL $IOFlag $user-$SCENARIO-$i
     csh run_postproc_river_IO_oneseg.csh $SCENARIO $SEGMENT $TIMEINTERVAL $IOFlag $user-$SCENARIO-$i

     @ i = $i + 1
end

wait
