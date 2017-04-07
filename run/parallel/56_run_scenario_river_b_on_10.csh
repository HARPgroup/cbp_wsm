#!/bin/csh

 set scenario = $argv[1]

 sbatch -pdebug ../standard/run_scenario_river.csh $scenario SLbig
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario lowP 
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario PL 
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario Y 
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario E 
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario R 
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario J_BFL
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario W 
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario X 
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario SM 
