#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_rug.csh scenario basin'
      echo ' or     run_rug.csh scenario basin tree'
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

  source $tree/config/seglists/${basin}.riv

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)

    echo making River UCI for segment $seg   River scenario $scenario

    echo $seg, $scenario | $tree/code/bin/rug.exe

    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

  end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

