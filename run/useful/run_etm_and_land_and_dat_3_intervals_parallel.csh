#!/bin/csh
#   GET SCENARIO, BASIN, and TREE
###########  The etm and land postprocessors do primarily the same thing, so 
########## half the time can be saved by combining them for scenarios and 
##########  river calibration processing
########### the date range in the etm is the date range specified in the river control file
###############  the input years are only used for average annual output

    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_etm_and_land_and_dat_3_intervals_parallel.csh scenario basin '
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set basin = $argv[2]

  source ../../config/seglists/${basin}.riv

  foreach seg ($segments)

    sbatch run_etm_and_land_and_dat_3_intervals_oneseg.csh $scenario $seg

  end



      
