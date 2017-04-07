#!/bin/csh
#   GET SCENARIO AND BASIN
  set basin = $argv[1]
  set scen = $argv[2]

  source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  source $tree/config/seglists/${basin}.riv

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)
    echo $seg $scen | $tree/code/bin/stand_alone_get_downstream.exe

    if (-e problem) then
      echo ' '
      cat problem
      rm problem
      exit
    endif

  end

