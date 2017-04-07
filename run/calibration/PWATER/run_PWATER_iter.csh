#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_PWATER_iter.csh scenario calscen basin'
      echo ' or     run_PWATER_iter.csh scenario calscen basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  $tree/run/standard/run_lug.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in lug '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif

  $tree/run/standard/run_land.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in land '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif

  $tree/run/standard/run_etm.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in etm '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif

  $tree/run/standard/run_river.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in river '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif

  $tree/run/calibration/PWATER/run_postproc_flow_calib.csh $scenario $calscen $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in postproc '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif

  $tree/run/calibration/PWATER/sumall.csh $scenario $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in sumall '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif


  if (${#argv} == 3) then
    cd ../
    rm -r temp$$
  endif

