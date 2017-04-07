#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_PSTEMP_stream_optimization.csh scenario calscen '
      echo ' or     run_PSTEMP_stream_optimization.csh scenario calscen tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]

  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif


  source $tree/config/seglists/temp.calib

  foreach rseg ($segments)  # loop over calibration stations

     $tree/run/calibration/PSTEMP/run_PSTEMP_one_station_river_optimization.csh $scenario $calscen $rseg $tree
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

