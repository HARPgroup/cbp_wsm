#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  write_input_monthly_parallel.csh scenario basin'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set basin = $argv[2]

  source ../../config/seglists/${basin}.land

  foreach seg ($segments)

    sbatch write_input_monthly.csh $scenario $seg 

  end



      
