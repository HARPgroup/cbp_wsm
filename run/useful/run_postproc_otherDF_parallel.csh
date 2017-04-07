#!/bin/csh
#   GET SCENARIO, DFSCEN, BASIN, and TREE

    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  run_postproc_otherDF_parallel.csh scenario DFscen basin year1 year2'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set DFscen = $argv[2]
  set basin = $argv[3]
  set year1 = $argv[4]
  set year2 = $argv[5]

  source ../../config/seglists/${basin}.riv

  foreach seg ($segments)

    sbatch run_postproc_otherDF_oneseg.csh $scenario $DFscen $seg $year1 $year2

  end



      
