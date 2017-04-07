#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  summarize_input_aveann.csh scenario basin year1 year2'
      echo ' or     summarize_input_aveann.csh scenario basin year1 year2 tree'
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
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  if (-e problem) then
    rm problem
  endif

  echo $scenario $basin $year1 $year2 | $tree/code/bin/sumin_aveann.exe
  if (-e problem) then
    echo ' '
    cat problem
    exit
  endif

  if (${#argv} == 4) then
    cd ../
    rm -r temp$$/
  endif

