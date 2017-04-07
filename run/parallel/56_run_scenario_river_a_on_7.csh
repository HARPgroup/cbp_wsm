#!/bin/csh

 set scenario = $argv[1]

 sbatch -pdebug ../standard/run_scenario_river.csh $scenario SU
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario SW
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario SJ
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario SLsmall
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario PS
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario PU
 sbatch -pdebug ../standard/run_scenario_river.csh $scenario J_AFL
