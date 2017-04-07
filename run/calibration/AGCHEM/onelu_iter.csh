#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  runiter.csh scenario basin'
      echo ' or     runiter.csh scenario basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  
  $tree/run/calibration/AGCHEM/onelu_lug.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in lug '
    cat problem
    if (${#argv} == 2) then
      rm problem
    endif
    exit
  endif

 
  $tree/run/calibration/AGCHEM/onelu_land.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in land '
    cat problem
    if (${#argv} == 2) then
      rm problem
    endif
    exit
  endif


  $tree/run/calibration/AGCHEM/onelu_pltgen.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in etm '
    cat problem
    if (${#argv} == 2) then
      rm problem
    endif
    exit
  endif


  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

