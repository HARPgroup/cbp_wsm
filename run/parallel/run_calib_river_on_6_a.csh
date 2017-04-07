#!/bin/csh

 set scenario = $argv[1]

 sbatch  ../standard/run_river.csh $scenario SU
 sbatch  ../standard/run_river.csh $scenario SW
 sbatch  ../standard/run_river.csh $scenario SJ
 sbatch  ../standard/run_river.csh $scenario SLsmall
 sbatch  ../standard/run_river.csh $scenario PS
 sbatch  ../standard/run_river.csh $scenario PU
 sbatch  ../standard/run_river.csh $scenario J_AFL
