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

  source ../../config/seglists/${basin}.land

  foreach seg ($segments)

    sbatch -pdebug ../useful/run_land_seg.csh $scenario $seg 

  end



      
