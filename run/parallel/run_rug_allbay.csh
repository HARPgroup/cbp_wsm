#!/bin/csh

 set scenario = $argv[1]

 sbatch ../standard/run_rug.csh $scenario SU
 sbatch ../standard/run_rug.csh $scenario SW
 sbatch ../standard/run_rug.csh $scenario SJ
 sbatch ../standard/run_rug.csh $scenario PS
 sbatch ../standard/run_rug.csh $scenario PU
 sbatch ../standard/run_rug.csh $scenario J_AFL
 sbatch ../standard/run_rug.csh $scenario SL
 sbatch ../standard/run_rug.csh $scenario lowP 
 sbatch ../standard/run_rug.csh $scenario PL 
 sbatch ../standard/run_rug.csh $scenario Y 
 sbatch ../standard/run_rug.csh $scenario E 
 sbatch ../standard/run_rug.csh $scenario R 
 sbatch ../standard/run_rug.csh $scenario J_BFL
 sbatch ../standard/run_rug.csh $scenario W 
 sbatch ../standard/run_rug.csh $scenario X 
 sbatch ../standard/run_rug.csh $scenario D 
 sbatch ../standard/run_rug.csh $scenario G 
 sbatch ../standard/run_rug.csh $scenario SM 
