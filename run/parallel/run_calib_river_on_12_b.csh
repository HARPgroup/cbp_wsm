#!/bin/csh

 set scenario = $argv[1]

 sbatch ../standard/run_river.csh $scenario SLbig
 sbatch ../standard/run_river.csh $scenario lowP 
 sbatch ../standard/run_river.csh $scenario PL 
 sbatch ../standard/run_river.csh $scenario Y 
 sbatch ../standard/run_river.csh $scenario E 
 sbatch ../standard/run_river.csh $scenario R 
 sbatch ../standard/run_river.csh $scenario J_BFL
 sbatch ../standard/run_river.csh $scenario W 
 sbatch ../standard/run_river.csh $scenario X 
 sbatch ../standard/run_river.csh $scenario SM 
