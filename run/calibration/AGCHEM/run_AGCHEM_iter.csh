#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  runiter.csh scenario basin'
      echo ' or     runiter.csh scenario basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set lu = $argv[3]

  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  
  $tree/run/calibration/AGCHEM/onelu_lug.csh $scenario $basin $lu $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in lug '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif

 
  $tree/run/calibration/AGCHEM/onelu_land.csh $scenario $basin $lu $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in land '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif


  $tree/run/calibration/AGCHEM/onelu_pltgen.csh $scenario $basin $lu $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in etm '
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

