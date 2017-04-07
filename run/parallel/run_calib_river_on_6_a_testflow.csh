#!/bin/csh

 set scenario = $argv[1]

 sbatch ../standard/run_river_ICPRB.csh $scenario SU
 sbatch ../standard/run_river_ICPRB.csh $scenario SW
 sbatch ../standard/run_river_ICPRB.csh $scenario SJ
 sbatch ../standard/run_river_ICPRB.csh $scenario SLsmall
 sbatch ../standard/run_river_ICPRB.csh $scenario PS
 sbatch ../standard/run_river_ICPRB.csh $scenario PU
 sbatch ../standard/run_river_ICPRB.csh $scenario J_AFL
