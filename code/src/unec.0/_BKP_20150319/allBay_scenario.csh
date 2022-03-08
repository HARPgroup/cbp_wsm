#!/bin/csh

set SCENARIO    = $argv[1]
set CALIBRATION = $argv[2]

source ../../../config/seglists/allBay.land

foreach SEG ($segments)
   #sbatch -pdebug run.csh $seg
   sbatch run_scenario.csh $SCENARIO $CALIBRATION $SEG
end
