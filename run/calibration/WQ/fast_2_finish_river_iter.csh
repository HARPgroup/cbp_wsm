#!/bin/csh

#  this script runs a calibration of the river parameters for water quality
# in the specified basin.  Calibration sensitivities are calculated at all 
# stations in the basin simultaneously for the best overall calibration rather
# than a strict nesting strategy

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 7) then
    if (${#argv} != 6) then
      echo ' '
      echo 'usage:  run_WQ_stream_optimization.csh scenario calscen basin year1 year2 num'
      echo ' or     run_WQ_stream_optimization.csh scenario calscen basin year1 year2 num tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  set year1 = $argv[4]
  set year2 = $argv[5]
  set num = $argv[6]
  if (${#argv} == 7) then
    set tree = $argv[7]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif
#################### FINISHED WITH INPUT

     $tree/run/useful/read_dot_out.csh $scenario $basin $year1 $year2 $tree

     if (!(-e $tree/output/river/summary/$scenario/sumout_${basin}.csv)) then
       echo 'Problem with read_dot_out'
       echo 'file' $tree/output/river/summary/$scenario/sumout_${basin}.csv 'not created'
       exit
     endif

     grep 'total' $tree/output/river/summary/$scenario/sumout_${basin}.csv >> $tree/output/river/summary/$scenario/sumout_iter_${basin}.csv

     echo $calscen, $scenario, $basin, $num, $year1, $year2 | $tree/code/bin/calib_iter_WQ_river_${calscen}.exe

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in calib_iter_WQ_river_'${calscen}'.exe'
       cat problem
       exit
     endif

     if (-e $tree/run/calibration/WQ/${calscen}_${scenario}.converge) then
       $tree/run/calibration/WQ/run_postproc_daily.csh $scenario $basin $year1 $year2 $tree
       if (-e problem) then
         echo ' '
         echo ' PROBLEM in run_postproc_daily.csh '
         cat problem
         exit
       endif

       rm $tree/run/calibration/WQ/${calscen}_${scenario}.converge
       echo 'calibration complete, convergence criterion met'
       exit

     else
       echo 'finished with iteration '$num' proceed with next iteration <30'
     endif

  if (${#argv} == 6) then
    cd ../
    rm -r temp$$
  endif
