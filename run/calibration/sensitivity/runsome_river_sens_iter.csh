#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 5) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  run_river_sens_iter.csh scenario basin year1 year2'
      echo ' or     run_river_sens_iter.csh scenario basin year1 year2 tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]
  if (${#argv} == 5) then
    set tree = $argv[5]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  $tree/run/run_river.csh $scenario temp1 $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in river '
    cat problem
    if (${#argv} == 4) then
      rm problem
    endif
    exit
  endif

  $tree/run/run_postproc2.csh $scenario $basin $year1 $year2 $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in postproc '
    cat problem
    if (${#argv} == 4) then
      rm problem
    endif
    exit
  endif

  if (${#argv} == 4) then
    cd ../
    rm -r temp$$
  endif

