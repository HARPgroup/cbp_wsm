#!/bin/csh
#   GET SCENARIO AND BASIN

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage: riverinfo.csh river_scenario uniqid'
      echo ' or '
      echo '       riverinfo.csh river_scenario uniqid tree'
      echo ' '
      exit
    endif
  endif

  set scen = $argv[1]
  set uniqid = $argv[2]
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

  echo $uniqid $scen | $tree/code/bin/riverinfo.exe

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

