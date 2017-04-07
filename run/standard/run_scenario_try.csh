#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

#  if (${#argv} != 3) then
#    if (${#argv} != 2) then
#      echo ' '
#      echo 'usage:  run_scenario.csh scenario basin'
#      echo ' or     run_scenario.csh scenario basin tree'
#      echo ' '
#      exit
#    endif
#  endif

#  set scenario = $argv[1]
#  set basin = $argv[2]
  set scenario = p532cal_062211  #$argv[1]
  set basin = stmary
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  if (-e problem) then
    rm problem
  endif

  $tree/run/try_scripts/make_land_directories.csh $scenario
  $tree/run/try_scripts/make_river_directories.csh $scenario

  $tree/run/try_scripts/run_lug.csh $scenario $basin $tree

    if (-e problem) then
      echo 'problem in run_scenario.csh'
      cat problem
      exit
    endif

  $tree/run/try_scripts/run_land.csh $scenario $basin $tree

    if (-e problem) then
      echo 'problem in run_scenario.csh'
      cat problem
      exit
    endif

  $tree/run/try_scripts/run_etm.csh $scenario $basin $tree

    if (-e problem) then
      echo 'problem in run_scenario.csh'
      cat problem
      exit
    endif

  $tree/run/try_scripts/run_scenario_river.csh $scenario $basin $tree

    if (-e problem) then
      echo 'problem in run_scenario.csh'
      cat problem
      exit
    endif

  $tree/run/try_scripts/run_scenario_postproc.csh $scenario $basin $tree

    if (-e problem) then
      echo 'problem in run_scenario.csh'
      cat problem
      exit
    endif

     
  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif
 
