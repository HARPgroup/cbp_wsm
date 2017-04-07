#!/bin/csh
#   GET SCENARIO, BASIN, and TREE
###########  The etm and land postprocessors do primarily the same thing, so 
########## half the time can be saved by combining them for scenarios and 
##########  river calibration processing
########### the date range in the etm is the date range specified in the river control file
###############  the input years are only used for average annual output

    if (${#argv} != 6) then
      echo ' '
      echo 'usage:  run_postproc_river_stats_parallel.csh scenario calscen basin module year1 year2'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  set module = $argv[4]
  set year1 = $argv[5]
  set year2 = $argv[6]

  source ../../config/seglists/${basin}.riv

  foreach seg ($segments)

    sbatch run_postproc_river_stats_oneseg.csh $scenario $calscen $seg $module $year1 $year2

  end



      
