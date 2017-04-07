#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 5) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  run_PSTEMP_river_iter.csh scenario basin num'
      echo ' or     run_PSTEMP_river_iter.csh scenario basin num tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  set num = $argv[4]

  if (${#argv} == 5) then
    set tree = $argv[5]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  $tree/run/standard/run_rug.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in rug '
    cat problem
    exit
  endif

  $tree/run/standard/run_river.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in river '
    cat problem
    exit
  endif

  $tree/run/calibration/PSTEMP/run_postproc_PSTEMP_calib.csh $scenario $calscen $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in postproc '
    cat problem
    exit
  endif

  if (${#argv} == 4) then
    cd ../
    rm -r temp$$
  endif

