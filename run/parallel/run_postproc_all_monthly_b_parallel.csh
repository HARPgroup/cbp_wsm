#!/bin/csh

 set scenario = $argv[1]

 sbatch ../useful/run_postproc_all_monthly.csh $scenario SLbig
 sbatch ../useful/run_postproc_all_monthly.csh $scenario lowP 
 sbatch ../useful/run_postproc_all_monthly.csh $scenario PL 
 sbatch ../useful/run_postproc_all_monthly.csh $scenario Y 
 sbatch ../useful/run_postproc_all_monthly.csh $scenario E 
 sbatch ../useful/run_postproc_all_monthly.csh $scenario R 
 sbatch ../useful/run_postproc_all_monthly.csh $scenario J_BFL
 sbatch ../useful/run_postproc_all_monthly.csh $scenario W 
 sbatch ../useful/run_postproc_all_monthly.csh $scenario X 
 sbatch ../useful/run_postproc_all_monthly.csh $scenario SM 
