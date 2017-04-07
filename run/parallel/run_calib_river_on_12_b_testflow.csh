#!/bin/csh

 set scenario = $argv[1]

 sbatch ../standard/run_river_ICPRB.csh $scenario SLbig
 sbatch ../standard/run_river_ICPRB.csh $scenario lowP 
 sbatch ../standard/run_river_ICPRB.csh $scenario PL 
 sbatch ../standard/run_river_ICPRB.csh $scenario Y 
 sbatch ../standard/run_river_ICPRB.csh $scenario E 
 sbatch ../standard/run_river_ICPRB.csh $scenario R 
 sbatch ../standard/run_river_ICPRB.csh $scenario J_BFL
 sbatch ../standard/run_river_ICPRB.csh $scenario W 
 sbatch ../standard/run_river_ICPRB.csh $scenario X 
 sbatch ../standard/run_river_ICPRB.csh $scenario SM 
