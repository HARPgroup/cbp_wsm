#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage: basingen.csh river_scenario basin'
      echo ' '
      echo '  BASIN can be a single unique ID, like 2720'
      echo '     or a BASIN name, like P, YM, or EM1'
      echo '     or the word ALL'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
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

   echo $basin $scenario | $tree/code/bin/basingen.exe

  if (-e problem) then
    echo ' '
    cat problem
    rm problem
    exit
  endif

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

