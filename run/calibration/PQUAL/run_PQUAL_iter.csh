#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_PQUAL_iter.csh scenario basin landuse'
      echo ' or     run_PQUAL_iter.csh scenario basin landuse tree'
      echo ' '
      exit
    endif
  endif

  set year1 = 1985
  set year2 = 2005

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

  $tree/run/useful/run_1lug.csh $scenario $basin $lu $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in lug '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif

  $tree/run/useful/run_1land.csh $scenario $basin $lu $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in land '
    cat problem
    if (${#argv} == 3) then
      rm problem
    endif
    exit
  endif

  echo $scenario $basin $lu $year1 $year2 | $tree/code/bin/pltgen_annave_summary.exe 

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in pltgen program'
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

