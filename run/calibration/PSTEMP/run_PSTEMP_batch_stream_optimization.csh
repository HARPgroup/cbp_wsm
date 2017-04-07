#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 3) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  run_PSTEMP_batch_stream_optimization.csh scenario calscen seglist_name'
      echo ' or     run_PSTEMP_batch_stream_optimization.csh scenario calscen seglist_name tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]

  source ../../../config/seglists/$basin.calib

  foreach rseg ($segments)  # loop over calibration stations

     sbatch run_PSTEMP_one_station_river_optimization.csh $scenario $calscen $rseg 

  end


