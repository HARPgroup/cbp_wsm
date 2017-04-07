#!/bin/csh

#  this script runs the WQ calibration step between updates of the 
#    regional factors.  It runs in sequence
# run_etm_and_land_and_dat_simultaneously.csh
# run_WQ_stream_optimization.csh
# run_postproc_river_aveann.csh
# run_subgrid_big_basins_only.csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 5) then
    if (${#argv} != 6) then
      echo ' '
      echo 'usage:  run_whole_WQ_iteration.csh scenario calscen basin year1 year2'
      echo ' or     run_whole_WQ_iteration.csh scenario calscen basin year1 year2 tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  set year1 = $argv[4]
  set year2 = $argv[5]
  if (${#argv} == 6) then
    set tree = $argv[6]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif
#################### FINISHED WITH INPUT

  if (-e problem) then
    rm problem
  endif

  $tree/run/useful/run_etm_and_land_and_dat_simultaneously.csh $scenario $basin $year1 $year2 $tree

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in tree/run/useful/run_etm_and_land_and_dat_simultaneously.csh'
       cat problem
       exit
     endif

  $tree/run/calibration/WQ/run_WQ_stream_optimization.csh $scenario $calscen $basin $year1 $year2 $tree

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in tree/run/calibration/WQ/run_WQ_stream_optimization.csh'
       cat problem
       exit
     endif

  $tree/run/useful/run_postproc_river_aveann.csh $scenario $basin $year1 $year2 $tree

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in tree/run/useful/run_postproc_river_aveann.csh'
       cat problem
       exit
     endif

  if ($basin == "SL") then
    set basin = "SLsgtf"
  endif

  $tree/run/calibration/transport/run_subgrid_big_basins_only.csh $scenario $calscen $basin $year1 $year2 $tree

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in tree/run/calibration/transport/run_subgrid_big_basins_only.csh'
       cat problem
       exit
     endif


  if (${#argv} == 5) then
    cd ../
    rm -r temp$$
  endif
