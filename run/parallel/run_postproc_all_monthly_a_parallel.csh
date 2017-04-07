#!/bin/csh

 set scenario = $argv[1]

 sbatch ../useful/run_postproc_all_monthly.csh $scenario SU
 sbatch ../useful/run_postproc_all_monthly.csh $scenario SW
 sbatch ../useful/run_postproc_all_monthly.csh $scenario SJ
 sbatch ../useful/run_postproc_all_monthly.csh $scenario SLsmall
 sbatch ../useful/run_postproc_all_monthly.csh $scenario PS
 sbatch ../useful/run_postproc_all_monthly.csh $scenario PU
 sbatch ../useful/run_postproc_all_monthly.csh $scenario J_AFL
