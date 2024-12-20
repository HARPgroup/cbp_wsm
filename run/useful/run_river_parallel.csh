#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_land_parallel.csh scenario basin'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set basin = $argv[2]

  source ../../config/seglists/${basin}.riv

  foreach seg ($segments)

    sbatch run_river_oneseg.csh $scenario $seg 

  end



      
