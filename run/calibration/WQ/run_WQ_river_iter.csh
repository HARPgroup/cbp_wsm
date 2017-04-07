#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 6) then
    if (${#argv} != 7) then
      echo ' '
      echo 'usage:  run_WQ_river_iter.csh scenario calscen basin year1 year2 num'
      echo ' or     run_WQ_river_iter.csh scenario calscen basin year1 year2 num tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  set year1 = $argv[4]
  set year2 = $argv[5]
  set num = $argv[6]
  if (${#argv} == 7) then
    set tree = $argv[7]
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

  $tree/run/calibration/WQ/run_postproc_WQ_cal.csh $scenario $calscen WQ $basin $year1 $year2 $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in postproc '
    cat problem
    exit
  endif

  if (${#argv} == 6) then
    cd ../
    rm -r temp$$
  endif

